/** $Id: ngrendering_getbot_impl.c,v 1.7 1999/08/03 01:58:09 janssen Exp $
 BeginILUCopyright

 Copyright (c) 1991-1999 Xerox Corporation.  All Rights Reserved.

 Unlimited use, reproduction, modification, and distribution of this
 software and modified versions thereof is permitted.  Permission is
 granted to make derivative works from this software or a modified
 version thereof.  Any copy of this software, a modified version
 thereof, or a derivative work must include both the above copyright
 notice of Xerox Corporation and this paragraph.  Any distribution of
 this software, a modified version thereof, or a derivative work must
 comply with all applicable United States export control laws.  This
 software is made available AS IS, and XEROX CORPORATION DISCLAIMS ALL
 WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
 LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
 EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
 NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
 OF THE POSSIBILITY OF SUCH DAMAGES.

 EndILUCopyright
*/
/* Last edited by Mike Spreitzer October 9, 1998 2:01 pm PDT */

#include "nggetbot.h"
#include "ngrendering_getbot_impl.h"

/* xxx We have a problem here - if the sink is exported over a 
concurrent protocol (e.g. w3ng) then we end up getting a thread
per request - this makes it nastier to assemble rendering / 
rendering chunks into the proper order (even though they are received
in order due to the webserver's use of serialization), and makes knowing the
last byte received quite problematic.  It would be extremely helpfun if
ILU provided some sort of control over thread-per-request (thread per request
on a connection/object/server/objectclass/whatever).

  For now, this code will simple receive the rendering / chunks, and
  call it finished when it receives a Done call.

*/


static ilu_boolean log_and_clear_if_exception(CORBA_Environment* ilu_env, char* pc_message) {
	if (ilu_env->_major != CORBA_NO_EXCEPTION) {
		if (DO_LOG_BASIC()) {
			LOG_ENTER();
			nglib_show_and_clear_if_exception(ilu_env, pc_message, g_p_logfile);
			LOG_EXIT();
		}
		else nglib_show_and_clear_if_exception(ilu_env, NULL, NULL);
		return ilu_TRUE;
	}
	return ilu_FALSE;
}


/* ************************************************************** */
/* RenderingData Sink */

static void FreeRenderingSinkObjectData(RenderingSinkObjectData* p_object_data) {
	
	CORBA_Environment ilu_env;
	ilu_Error an_error;

	if (p_object_data->m_done_condition)
		ilu_DestroyCondition(p_object_data->m_done_condition);

	if (p_object_data->m_access_mutex) {
		ilu_DestroyMutex(p_object_data->m_access_mutex, &an_error);
		if (ILU_ERRNOK(an_error)) {
			ILU_HANDLED(an_error);
			ILU_CLER(an_error);
		}
	}

	if (p_object_data->m_datasource) {
		CORBA_Object_release(p_object_data->m_datasource, &ilu_env);
		log_and_clear_if_exception(&ilu_env, "CORBA_Object_release problem");
	}
	
	if (p_object_data->m_pc_object_id)
		ilu_free(p_object_data->m_pc_object_id);

	ilu_free(p_object_data);
}


/* allocates instance data for RenderingSinks 
   pc_object_id is the object id of the object whose rendering we're going to receive */
RenderingSinkObjectData* MallocRenderingSinkObjectData(char* pc_object_id, ilu_FineTime time_begin) {

	RenderingSinkObjectData* p_object_data;
	ilu_Error an_error;

	p_object_data = (RenderingSinkObjectData*) ilu_malloc(sizeof (RenderingSinkObjectData));
	if (!p_object_data) 
		return NULL;

	p_object_data->m_done_condition = NULL;
	p_object_data->m_access_mutex = NULL;

	p_object_data->m_datasource = NULL;
	p_object_data->m_card_last_byte_received_index = 0;
	p_object_data->m_pc_object_id = pc_object_id;

	p_object_data->m_done_condition = 
		ilu_CreateCondition("RenderingSinkObjectData", pc_object_id, &an_error);
	if (ILU_ERRNOK(an_error)) {
		ILU_HANDLED(an_error);
		ILU_CLER(an_error);
		goto problem;
	}
	
	p_object_data->m_access_mutex = ilu_CreateMutex("RenderingSinkObjectData", pc_object_id);
	if (!p_object_data->m_access_mutex)
		goto problem;

	p_object_data->m_time_begin = time_begin;

	return p_object_data;

problem:
	FreeRenderingSinkObjectData(p_object_data);
	return NULL;
}





NgBasic_String 
server_NgRendering_RenderingSink_GetInterfaceDefinitionSource 
(NgRendering_RenderingSink _handle,
 ILU_C_ENVIRONMENT *_status) {
	
	NGLIB_NYI("server_NgRendering_RenderingSink_GetInterfaceDefinitionSource");
	return NULL;
}



void 
server_NgRendering_RenderingSink_RegisterSourceControl 
(NgRendering_RenderingSink render_sink, 
 NgStream_DataSource thesource, 
 ILU_C_ENVIRONMENT *_status) {

	CORBA_Environment ilu_env;

	RenderingSinkObjectData* p_object_data = render_sink->instanceData;

	ilu_AcquireMutex(p_object_data->m_access_mutex);

	p_object_data->m_datasource = CORBA_Object_duplicate(thesource, &ilu_env);
	log_and_clear_if_exception(&ilu_env, "CORBA_Object_duplicate problem");

	ilu_ReleaseMutex(p_object_data->m_access_mutex);
}



void 
server_NgRendering_RenderingSink_Done
(NgRendering_RenderingSink render_sink,
 ILU_C_ENVIRONMENT *_status) {
	
	ilu_Error an_error;
	RenderingSinkObjectData* p_object_data = render_sink->instanceData;
	
	ilu_AcquireMutex(p_object_data->m_access_mutex);
	
	ilu_CondNotify(p_object_data->m_done_condition, &an_error);
	if (ILU_ERRNOK(an_error)) {
		fprintf(stderr, "Error calling ilu_CondNotify on g_get_thread_count_condition - proceeding anyway\n");
		ILU_HANDLED(an_error);
		ILU_CLER(an_error);
	}
	
	ilu_ReleaseMutex(p_object_data->m_access_mutex);
}



void
server_NgRendering_RenderingSink_RegisterResponseCacheControl 
(NgRendering_RenderingSink _handle,
 NgCache_ResponseCacheControl* responseCacheInfo,
 ILU_C_ENVIRONMENT *_status) {
	
	/* xxx we don't do any caching at the moment */

	NGLIB_NYI("server_NgRendering_RenderingSink_RegisterResponseCacheControl");
	return ;
}



void
server_NgRendering_RenderingSink_RenderingProblem 
(NgRendering_RenderingSink render_sink,
 NgRendering_RenderingProblemReport* report, 
 ILU_C_ENVIRONMENT *_status) {
	
	RenderingSinkObjectData* p_object_data = render_sink->instanceData;
	
	ilu_AcquireMutex(p_object_data->m_access_mutex);
	
	if (DO_LOG_BASIC()) {
		LOG_ENTER();
		nglib_print_rendering_problem_report(report, g_p_logfile);
		LOG_EXIT();
	}
	
	ilu_ReleaseMutex(p_object_data->m_access_mutex);

	/* say we're done */
	server_NgRendering_RenderingSink_Done(render_sink, _status);
	
}



void
server_NgRendering_RenderingSink_ReceiveRendering 
(NgRendering_RenderingSink render_sink, 
 NgRendering_Rendering* therendering, 
 ILU_C_ENVIRONMENT *_status) {
	
	ilu_FineTime timeinterval;
	RenderingSinkObjectData* p_object_data = render_sink->instanceData;
	
	ilu_AcquireMutex(p_object_data->m_access_mutex);
	
	/* determine how log it took from the SendRendering call to receipt of 
	   the first byte of the rendering, and add it to the stats*/	
	timeinterval = ilu_FineTime_Sub(ilu_FineTime_Now(), p_object_data->m_time_begin);		
	SEND_TIME_ENTER();
	AddStat(&g_send_to_first_byte_timesums, timeinterval);
	SEND_TIME_EXIT();
	
	if (DO_LOG_BASIC()) {
		LOG_ENTER();
		fprintf(g_p_logfile, "server_NgRendering_RenderingSink_ReceiveRendering on %s\n",
			p_object_data->m_pc_object_id);
		if (DO_LOG_FULL()) {
			nglib_print_ngrendering_rendering(therendering, 1, g_p_logfile);
		}
		else if (DO_LOG_BASIC()) {
			nglib_print_ngrendering_rendering(therendering, 0, g_p_logfile);
		}
		LOG_EXIT();
	}
	
	/* xxx this is messed up somewhat because server_NgRendering_RenderingSink_ReceiveRenderingChunk
	calls could be happening too, mixing up what the actual value is */
	if (therendering->contentRange) 
		p_object_data->m_card_last_byte_received_index = 
		nglib_max(therendering->contentRange->endValue, p_object_data->m_card_last_byte_received_index);
	
	ilu_ReleaseMutex(p_object_data->m_access_mutex);
	
}



void 
server_NgRendering_RenderingSink_ReceiveRenderingChunk
(NgRendering_RenderingSink render_sink,
 NgRendering_RenderingChunk* thechunk,
 ILU_C_ENVIRONMENT *_status) {

	RenderingSinkObjectData* p_object_data = render_sink->instanceData;
	
	ilu_AcquireMutex(p_object_data->m_access_mutex);
	
	if (DO_LOG_BASIC()) {
		LOG_ENTER();
		fprintf(g_p_logfile, "server_NgRendering_RenderingSink_ReceiveRenderingChunk on %s\n",
			p_object_data->m_pc_object_id);
		if (DO_LOG_FULL()) {
			nglib_print_ngrendering_rendering_chunk(thechunk, 1, g_p_logfile);
		}
		else if (DO_LOG_BASIC()) {
			nglib_print_ngrendering_rendering_chunk(thechunk, 0, g_p_logfile);
		}
		LOG_EXIT();
	}
	
	/* xxx this is messed up somewhat because other server_NgRendering_RenderingSink_ReceiveRenderingChunk
	calls could be happening too, mixing up what the actual value is */
	if (thechunk->contentRange) 
		p_object_data->m_card_last_byte_received_index = 
			nglib_max(thechunk->contentRange->endValue, p_object_data->m_card_last_byte_received_index);

	ilu_ReleaseMutex(p_object_data->m_access_mutex);

}



NgBasic_OptionalCardinal 
server_NgRendering_RenderingSink_Resynchronize 
(NgRendering_RenderingSink _handle, 
 ILU_C_ENVIRONMENT *_status) {
	
	NGLIB_NYI("server_NgRendering_RenderingSink_Resynchronize");
	return NULL;
}


/* dummy definitions to satisfy linker */

NgBasic_String 
server_NgRendering_PutableRenderable_GetInterfaceDefinitionSource 
(NgRendering_PutableRenderable _handle,
 ILU_C_ENVIRONMENT *_status) {
	
	NGLIB_NYI("server_NgRendering_PutableRenderable_GetInterfaceDefinitionSource");
	return NULL;
}



NgRendering_RenderingPreferences* 
server_NgRendering_PutableRenderable_GetAvailableRenderings
(NgRendering_PutableRenderable _handle, 
 ILU_C_ENVIRONMENT *_status) {
	
	NGLIB_NYI("server_NgRendering_PutableRenderable_GetAvailableRenderings");
	return NULL;
}



NgRendering_Rendering*
server_NgRendering_PutableRenderable_GetRendering 
(NgRendering_PutableRenderable _handle, 
 NgRendering_RenderingPreferences* renderingPreferences, 
 NgCache_RequestCacheControl* requestCacheInfo, 
 NgCache_ResponseCacheControl** responseCacheInfo, 
 ILU_C_ENVIRONMENT *_status) {
	
	NGLIB_NYI("server_NgRendering_PutableRenderable_GetRendering");
	return NULL;
}



void 
server_NgRendering_PutableRenderable_SendRendering 
(NgRendering_PutableRenderable _handle, 
 NgRendering_RenderingPreferences* renderingPreferences,
 NgCache_RequestCacheControl* requestCacheInfo, 
 NgRendering_RenderingSink renderSink, 
 NgBasic_OptionalCardinal suggestedChunkSize, 
 ILU_C_ENVIRONMENT *_status) {
	
	NGLIB_NYI("server_NgRendering_PutableRenderable_SendRendering");
	return ;
}



void 
server_NgRendering_PutableRenderable_SendRenderingSynched
(NgRendering_PutableRenderable _handle, 
 NgRendering_RenderingPreferences* renderingPreferences,
 NgCache_RequestCacheControl* requestCacheInfo,
 NgRendering_RenderingSink renderSink, 
 NgBasic_OptionalCardinal suggestedChunkSize, 
 ILU_C_ENVIRONMENT *_status) {
	
	NGLIB_NYI("server_NgRendering_PutableRenderable_SendRenderingSynched");
	return ;
}



void 
server_NgRendering_PutableRenderable_PutRendering 
(NgRendering_PutableRenderable _handle, 
 NgRendering_Rendering* renderingInput, 
 ILU_C_ENVIRONMENT *_status) {
	
	NGLIB_NYI("server_NgRendering_PutableRenderable_PutRendering");
	return ;
}



NgBasic_String 
server_NgRendering_Renderable_GetInterfaceDefinitionSource 
(NgRendering_Renderable _handle, 
 ILU_C_ENVIRONMENT *_status) {
	
	NGLIB_NYI("server_NgRendering_Renderable_GetInterfaceDefinitionSource");
	return NULL;
}



NgRendering_RenderingPreferences* 
server_NgRendering_Renderable_GetAvailableRenderings
(NgRendering_Renderable _handle,
 ILU_C_ENVIRONMENT *_status) {
	
	NGLIB_NYI("server_NgRendering_Renderable_GetAvailableRenderings");
	return NULL;
}



NgRendering_Rendering* 
server_NgRendering_Renderable_GetRendering
(NgRendering_Renderable _handle, 
 NgRendering_RenderingPreferences* renderingPreferences, 
 NgCache_RequestCacheControl* requestCacheInfo,
 NgCache_ResponseCacheControl** responseCacheInfo, 
 ILU_C_ENVIRONMENT *_status) {
	
	NGLIB_NYI("server_NgRendering_Renderable_GetRendering");
	return NULL;
}



void 
server_NgRendering_Renderable_SendRendering 
(NgRendering_Renderable _handle, 
 NgRendering_RenderingPreferences* renderingPreferences,
 NgCache_RequestCacheControl* requestCacheInfo, 
 NgRendering_RenderingSink renderSink, 
 NgBasic_OptionalCardinal suggestedChunkSize, 
 ILU_C_ENVIRONMENT *_status) {
	
	NGLIB_NYI("server_NgRendering_Renderable_SendRendering");
	return ;
}



void 
server_NgRendering_Renderable_SendRenderingSynched 
(NgRendering_Renderable _handle,
 NgRendering_RenderingPreferences* renderingPreferences,
 NgCache_RequestCacheControl* requestCacheInfo, 
 NgRendering_RenderingSink renderSink,
 NgBasic_OptionalCardinal suggestedChunkSize, 
 ILU_C_ENVIRONMENT *_status) {
	
	NGLIB_NYI("server_NgRendering_Renderable_SendRenderingSynched");
	return ;
}


