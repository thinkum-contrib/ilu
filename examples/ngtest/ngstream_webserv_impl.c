/** $Id: ngstream_webserv_impl.c,v 1.6 1999/08/03 01:58:07 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 2:02 pm PDT */

#include "nglib.h"
#include "ngwebserver.h"
#include "ngstream_webserv_impl.h"


/* XXX note, this code doesn't deal with barrier exceptions that
could cause it to need to resync with a sink. */



/* ILU server */
ILU_C_Server g_stream_server     = NULL;


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



/* ********************************************************* */
/* object table and setup functions                          */
/* ********************************************************* */

static CORBA_Object object_of_stream_handle (ilu_string str_object_id, 
											 ilu_private p_iluserver) {
	
 /*  Stream Sources are usually meant to be ephemeral.  As such,
	 the object table function for this server will return NULL (i.e.
	 sources and sinks cannot be created from the outside, e.g. a client
	 turns an sbh into a surrogate, and passes it to the server).
*/
	return NULL;
}


static void free_stream_object_table_storage (ilu_private p_iluserver) {
  return; /* nothing to do yet */
}


void ngstream_impl_setup_server() {

	static ilu_boolean b_already_setup = ilu_FALSE;
	ILU_C_ObjectTable object_table;
	char pc_serverid[1024];						/* holds a server id */

	if (b_already_setup)
		return ;
	
	/* create object table and ILU server*/

	/* StreamServer */
	object_table = ILU_C_CreateObjectTable (object_of_stream_handle, 
			free_stream_object_table_storage, (ilu_private) &g_stream_server);

	sprintf (pc_serverid, "StreamServer.%s%s", g_pc_hostname, g_pc_ngwebserver_suffix);

	g_stream_server = ILU_C_InitializeServer ( pc_serverid, object_table, 
		g_pc_nonhttp_pinfo, get_ngtinfo(), (ilu_Passport) ILU_NIL, ilu_TRUE );

	b_already_setup = ilu_TRUE;

}




NgBasic_String 
server_NgStream_DataSink_GetInterfaceDefinitionSource 
(NgStream_DataSink _handle, 
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgStream_DataSink_GetInterfaceDefinitionSource");
    return NULL;
}


void 
server_NgStream_DataSink_RegisterSourceControl 
(NgStream_DataSink _handle, NgStream_DataSource thesource, 
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgStream_DataSink_RegisterSourceControl");
    return ;
}


void 
server_NgStream_DataSink_Done
(NgStream_DataSink _handle,
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgStream_DataSink_Done");
    return ;
}

/* ************************************************************** */
/* Data Source */

static void FreeDataSourceObjectData(DataSourceObjectData* p_object_data) {
	
	ilu_Error an_error;
	CORBA_Environment ilu_env;
	
	if (p_object_data->m_state_change_condition)
		ilu_DestroyCondition(p_object_data->m_state_change_condition);

	if (p_object_data->m_access_mutex) {
		ilu_DestroyMutex(p_object_data->m_access_mutex, &an_error);
		if (ILU_ERRNOK(an_error)) {
			ILU_HANDLED(an_error);
			ILU_CLER(an_error);
		}
	}
	if (p_object_data->m_renderSink) {
		CORBA_Object_release(p_object_data->m_renderSink, &ilu_env);
		log_and_clear_if_exception(&ilu_env, "CORBA_Object_release problem");
	}

	if (p_object_data->m_pipeline) {
		ILU_C_ReleasePipeline(p_object_data->m_pipeline, &ilu_env);
		log_and_clear_if_exception(&ilu_env, "Calling ILU_C_ReleasePipeline");
		ILU_C_SetPipelineContext(NULL); 
	}
	
	CLOSE_FUNCTION(p_object_data->m_h_native_content_file);
	ilu_free(p_object_data->m_pc_object_id);	
	ilu_free(p_object_data);
}

DataSourceObjectData* MallocDataSourceObjectData(int h_native_content_file,
												 ilu_cardinal start, 
												 ilu_cardinal end,
												 ilu_cardinal suggestedChunkSize,
												 char* pc_content_type,
												 ilu_boolean b_like_head,
												 char* pc_object_id,
												 NgStream_DataSink renderSink) {
	DataSourceObjectData* p_object_data;
	ilu_Error an_error;
	CORBA_Environment ilu_env;
	
	p_object_data = (DataSourceObjectData*) ilu_malloc(sizeof (DataSourceObjectData));
	if (!p_object_data) 
		return NULL;
	

	p_object_data->m_state_change_condition = NULL;
	p_object_data->m_access_mutex = NULL;
	p_object_data->m_renderSink = NULL;
	p_object_data->m_pipeline = NULL;
	p_object_data->m_serializer = NULL;
	p_object_data->m_h_native_content_file = h_native_content_file;
	if (suggestedChunkSize != 0)
		p_object_data->m_card_chunking_size = suggestedChunkSize;
	else 
		p_object_data->m_card_chunking_size = NGWEBSEVER_DEFAULT_CHUNK_SIZE;
	p_object_data->m_card_next_unsent_byte_index = start;
	p_object_data->m_card_last_byte_to_send_index = end;
	p_object_data->m_i_state = 0; /* 0 = run, 1 = paused, 2 = abort */
	p_object_data->m_pc_object_id = pc_object_id;
	p_object_data->m_pc_content_type = pc_content_type;
	p_object_data->m_b_like_head = b_like_head;

	p_object_data->m_state_change_condition = 
		ilu_CreateCondition("DataSourceObjectData", pc_object_id, &an_error);
	if (ILU_ERRNOK(an_error)) {
		ILU_HANDLED(an_error);
		ILU_CLER(an_error);
		goto problem;
	}
	
	p_object_data->m_access_mutex = ilu_CreateMutex("DataSourceObjectData", pc_object_id);
	if (!p_object_data->m_access_mutex)
		goto problem;
	
	p_object_data->m_renderSink = CORBA_Object_duplicate(renderSink, &ilu_env);
	if (log_and_clear_if_exception(&ilu_env, "CORBA_Object_duplicate problem"))
		goto problem;

	p_object_data->m_pipeline = ILU_C_CreatePipeline(&ilu_env);
	if (log_and_clear_if_exception(&ilu_env, "Calling ILU_C_CreatePipeline"))
		goto problem;

	/* note serializer gets created elsewhere because this function gets called before
	   we create the object */

	return p_object_data;

problem:
	FreeDataSourceObjectData(p_object_data);
	return NULL;
}




static ilu_boolean data_source_send_rendering(DataSourceObjectData* p_object_data) {
	
	NgRendering_Rendering the_rendering;
	NgBasic_UnsignedInclusiveInterval range;
	CORBA_Environment ilu_env;

	/* set up a Rendering to send */
	nglib_NgRendering_Rendering_clean_assign(&the_rendering);
	
	the_rendering.contentRange = &range; 

	/* set the content type */
	the_rendering.contentType = p_object_data->m_pc_content_type;
	
	if (p_object_data->m_b_like_head) { 
		/* don't actually send the bytes */
		the_rendering.contentRange->startValue = p_object_data->m_card_next_unsent_byte_index;
		the_rendering.contentRange->endValue = p_object_data->m_card_last_byte_to_send_index;
		
	}
	else { /* send the first bunch of bytes in the rendering */
		/* determine how much to send */
		the_rendering.contentRange->startValue = p_object_data->m_card_next_unsent_byte_index;
		if (p_object_data->m_card_last_byte_to_send_index - p_object_data->m_card_next_unsent_byte_index + 1 <=
			p_object_data->m_card_chunking_size ) {
			/* what's left is less than or equal to the chunk size, send it all */
			the_rendering.contentRange->endValue = p_object_data->m_card_last_byte_to_send_index;
			/* advance next unsent byte pointer past the last byte */
			p_object_data->m_card_next_unsent_byte_index = p_object_data->m_card_last_byte_to_send_index + 1;
		}
		else {
			/* what's left is more than the chunk size, send a chunk */
			the_rendering.contentRange->endValue = 
				p_object_data->m_card_next_unsent_byte_index + p_object_data->m_card_chunking_size - 1;
			/* advance next unsent byte pointer past the last byte */
			p_object_data->m_card_next_unsent_byte_index = 
				p_object_data->m_card_next_unsent_byte_index + p_object_data->m_card_chunking_size;
		}
		
		/* seek to start of range */
		if (SEEK_FUNCTION(p_object_data->m_h_native_content_file, the_rendering.contentRange->startValue, SEEK_CUR) == -1) 
			return ilu_FALSE;
		
		/* get and fill a buffer for the rendering bytes */
		the_rendering.renderingBytes._maximum = the_rendering.contentRange->endValue - the_rendering.contentRange->startValue + 1;
		the_rendering.renderingBytes._length = the_rendering.renderingBytes._maximum;
		the_rendering.renderingBytes._buffer = (CORBA_octet*) 
			ilu_malloc(the_rendering.renderingBytes._maximum);
		if (READ_FUNCTION(p_object_data->m_h_native_content_file, the_rendering.renderingBytes._buffer,
			the_rendering.renderingBytes._maximum) == -1) 
			return ilu_FALSE;
	}
	
	/* send the rendering to the Sink */
	NgRendering_RenderingSink_ReceiveRendering(p_object_data->m_renderSink, &the_rendering, &ilu_env);
	if (log_and_clear_if_exception(&ilu_env, "NgStream_DataSink_ReceiveRendering problem")) {
		if (the_rendering.renderingBytes._buffer)
			ilu_free(the_rendering.renderingBytes._buffer);
		return ilu_FALSE;
	}

	if (the_rendering.renderingBytes._buffer)
		ilu_free(the_rendering.renderingBytes._buffer);

	if (DO_LOG_BASIC()) {
		LOG_ENTER();
		fprintf(g_p_logfile, "data_source_send_rendering of %s, range %u to %u\n",
			p_object_data->m_pc_object_id, the_rendering.contentRange->startValue, 
			the_rendering.contentRange->endValue);
		LOG_EXIT();
	}

	return ilu_TRUE;
}



static ilu_boolean data_source_send_chunk(DataSourceObjectData* p_object_data, 
										  ilu_cardinal card_start, ilu_cardinal card_end) {
	NgRendering_RenderingChunk chunk;
	NgBasic_UnsignedInclusiveInterval range;
	CORBA_Environment ilu_env;

	chunk.contentRange = &range;
	
	/* determine how much to send */
	chunk.contentRange->startValue = card_start;
	chunk.contentRange->endValue = card_end;
	
	/* seek to start of range */
	if (SEEK_FUNCTION(p_object_data->m_h_native_content_file, chunk.contentRange->startValue, SEEK_CUR) == -1) 
		/* xxx should really put out some sort of message here */
		return ilu_FALSE;
	
	/* get and fill a buffer for the rendering bytes */
	chunk.renderingBytes._maximum = chunk.contentRange->endValue - chunk.contentRange->startValue + 1;
	chunk.renderingBytes._length = chunk.renderingBytes._maximum;
	chunk.renderingBytes._buffer = (CORBA_octet*) 
		ilu_malloc(chunk.renderingBytes._maximum);
	if (READ_FUNCTION(p_object_data->m_h_native_content_file, chunk.renderingBytes._buffer,
		chunk.renderingBytes._maximum) == -1) 
		/* xxx should really put out some sort of message here */
		return ilu_FALSE;
	
	
	/* send the rendering to the Sink */
	NgRendering_RenderingSink_ReceiveRenderingChunk(p_object_data->m_renderSink, &chunk, &ilu_env);
	if (log_and_clear_if_exception(&ilu_env, "NgStream_DataSink_ReceiveRendering problem")) {
		if (chunk.renderingBytes._buffer)
			ilu_free(chunk.renderingBytes._buffer);
		/* xxx should really put out some sort of message here */
		return ilu_FALSE;
	}
	
	if (chunk.renderingBytes._buffer)
		ilu_free(chunk.renderingBytes._buffer);

	if (DO_LOG_BASIC()) {
		LOG_ENTER();
		fprintf(g_p_logfile, "data_source_send_chunk of %s, range %u to %u\n",
			p_object_data->m_pc_object_id, chunk.contentRange->startValue, chunk.contentRange->endValue);
		LOG_EXIT();
	}

	if (chunk.contentRange->startValue > chunk.contentRange->endValue)
		return ilu_FALSE;
	
	return ilu_TRUE;
}

static ilu_boolean data_source_send_next_chunk(DataSourceObjectData* p_object_data) {

	ilu_cardinal card_start;
	ilu_cardinal card_end;

	/* determine how much to send */
	card_start = p_object_data->m_card_next_unsent_byte_index;
	if (p_object_data->m_card_last_byte_to_send_index - p_object_data->m_card_next_unsent_byte_index + 1 <=
		p_object_data->m_card_chunking_size ) {
		/* what's left is less than or equal to the chunk size, send it all */
		card_end = p_object_data->m_card_last_byte_to_send_index;
		/* advance next unsent byte pointer past the last byte */
		p_object_data->m_card_next_unsent_byte_index = p_object_data->m_card_last_byte_to_send_index + 1;
	}
	else {
		/* what's left is more than the chunk size, send a chunk */
		card_end = 
			p_object_data->m_card_next_unsent_byte_index + p_object_data->m_card_chunking_size - 1;
		/* advance next unsent byte pointer past the last byte */
		p_object_data->m_card_next_unsent_byte_index = 
			p_object_data->m_card_next_unsent_byte_index + p_object_data->m_card_chunking_size;
	}

	return data_source_send_chunk(p_object_data, card_start, card_end);
}


/* animates the data source */
void do_data_source_work (NgStream_DataSource data_source) {


	DataSourceObjectData* p_object_data = data_source->instanceData;
	CORBA_Environment ilu_env;
	ilu_Error an_error;

	/* set up serializer and pipeline */
	p_object_data->m_serializer = nglib_get_object_serializer(p_object_data->m_renderSink);
	ILU_C_SetSerializationContext(p_object_data->m_serializer);
	ILU_C_SetPipelineContext(p_object_data->m_pipeline);

	if (g_batcher) /* ensure batching if appropriate */
		ILU_C_SetBatcherContext(g_batcher); 

	/* register this DataSource with the Sink */
	NgStream_DataSink_RegisterSourceControl(p_object_data->m_renderSink, data_source, &ilu_env);
	if (log_and_clear_if_exception(&ilu_env, "NgStream_DataSink_RegisterSourceControl problem"))
		/* xxx perhaps too brutal?, but if we can't do this, what hope do we have of sending renderings! */
		goto finished; 

	/* register response cache info - xxx note ignoring caching for now */

	/* send a rendering with the first chunk */
	if (!data_source_send_rendering(p_object_data))
		goto done; /* had a problem sending that first rendering */

	if (p_object_data->m_b_like_head) /* no actual chunks to send */
		goto done;


	/* send the rest of the chunks */
	while (1) {
		ilu_AcquireMutex(p_object_data->m_access_mutex);
		
		switch (p_object_data->m_i_state) {
		case 0: /* run */
				if (p_object_data->m_card_next_unsent_byte_index >= p_object_data->m_card_last_byte_to_send_index)
					goto done;
				if (!data_source_send_next_chunk(p_object_data)) /* problem sending chunk */
					goto done;
				break;

		case 1: /* paused */
			ilu_CMWait1(p_object_data->m_state_change_condition, p_object_data->m_access_mutex, &an_error);
			if (ILU_ERRNOK(an_error)) {
				ILU_HANDLED(an_error);
				ILU_CLER(an_error);
				ilu_ReleaseMutex(p_object_data->m_access_mutex);
				/* xxx should really put out some sort of message here */
				goto done;
			}
			break;
			
		case 2: /* abort */
		default:
			ilu_ReleaseMutex(p_object_data->m_access_mutex);
			goto finished;
		}
		
		ilu_ReleaseMutex(p_object_data->m_access_mutex);
	}

done:

	/* send the done message */
	NgStream_DataSink_Done(p_object_data->m_renderSink,  &ilu_env);
	log_and_clear_if_exception(&ilu_env, "NgStream_DataSink_Done problem");

finished:

	/* release our serializer, destroy our object and free its data */
	nglib_release_object_serializer(p_object_data->m_renderSink);
	ILU_C_SetSerializationContext(NULL);
	CORBA_Object_release(data_source, &ilu_env);
	log_and_clear_if_exception(&ilu_env, "CORBA_Object_release problem");
	FreeDataSourceObjectData(p_object_data);

}



NgBasic_String 
server_NgStream_DataSource_GetInterfaceDefinitionSource 
(NgStream_DataSource _handle,
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgStream_DataSource_GetInterfaceDefinitionSource");
    return NULL;
}


/* called to change the state of a DataSource */
static void NgStream_DataSource_change_state(NgStream_DataSource data_source, int i_state) {
	ilu_Error an_error;
	DataSourceObjectData* p_object_data = data_source->instanceData;
	
	ilu_AcquireMutex(p_object_data->m_access_mutex);
	
	if (p_object_data->m_i_state != i_state) {
		/* a change from current state */
		
		if (DO_LOG_BASIC()) {
			LOG_ENTER();
			fprintf(g_p_logfile, "NgStream_DataSource_change_state from %i to %i\n",
				p_object_data->m_i_state, i_state);
			LOG_EXIT();
		}
		
		p_object_data->m_i_state = i_state;
		ilu_CondNotify(p_object_data->m_state_change_condition, &an_error);
		
		if (ILU_ERRNOK(an_error)) {
			if (DO_LOG_BASIC()) {
				LOG_ENTER();
				fprintf(g_p_logfile, "Error calling ilu_CondNotify on DataSource - proceeding anyway\n");
				LOG_EXIT();
			}
			ILU_HANDLED(an_error);
			ILU_CLER(an_error);
		}
	}
	
	ilu_ReleaseMutex(p_object_data->m_access_mutex);
    return ;
}

void 
server_NgStream_DataSource_Abort 
(NgStream_DataSource data_source, 
 ILU_C_ENVIRONMENT *_status) {

	NgStream_DataSource_change_state(data_source, 2);
}


void 
server_NgStream_DataSource_Pause 
(NgStream_DataSource data_source,
 ILU_C_ENVIRONMENT *_status) {
    
	NgStream_DataSource_change_state(data_source, 1);
}


void 
server_NgStream_DataSource_Resume
(NgStream_DataSource data_source, 
 ILU_C_ENVIRONMENT *_status) {
    
	NgStream_DataSource_change_state(data_source, 0);
}



void 
server_NgStream_DataSource_Resend 
(NgStream_DataSource data_source, 
 NgBasic_UnsignedInclusiveInterval* repeatRange,
 ILU_C_ENVIRONMENT *_status) {
   	DataSourceObjectData* p_object_data = data_source->instanceData;

	ilu_AcquireMutex(p_object_data->m_access_mutex);

	data_source_send_chunk(p_object_data, repeatRange->startValue, repeatRange->endValue);

	ilu_ReleaseMutex(p_object_data->m_access_mutex);
}



void
server_NgStream_DataSource_SuggestChunkSize 
(NgStream_DataSource data_source, 
 CORBA_unsigned_long suggestedSize, 
 ILU_C_ENVIRONMENT *_status) {
    
	DataSourceObjectData* p_object_data = data_source->instanceData;

	ilu_AcquireMutex(p_object_data->m_access_mutex);

	p_object_data->m_card_chunking_size = suggestedSize;

	ilu_ReleaseMutex(p_object_data->m_access_mutex);
}

