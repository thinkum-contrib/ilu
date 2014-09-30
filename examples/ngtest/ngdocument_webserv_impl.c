/** $Id: ngdocument_webserv_impl.c,v 1.10 1999/08/03 01:58:14 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 2:00 pm PDT */

#include "nglib.h"
#include "ngwebserver.h"
#include "ngstream_webserv_impl.h"

/* ILU server */
ILU_C_Server g_document_server     = NULL;


/* ********************************************************* */
/* object table and setup functions                          */
/* ********************************************************* */

static CORBA_Object object_of_documentwebdocobj (ilu_string str_object_id, 
										   ilu_private p_iluserver) {
	
	/* object ids in this server normally begin with their typename :
	   e.g. w3ng:DocumentServer.pundit.parc.xerox.com/WebDocument:/foo/bar.html
	   if not, they are assumed to be of type WebDocument
	*/

	if (strncmp("PutableWebDocument:", str_object_id, 19) == 0)
		return (NgDocument_PutableWebDocument__OTCreateTrue (str_object_id,
		*((ILU_C_Server *) p_iluserver), NULL)); 

	if (strncmp("HTTPCompatibleWebDocument:", str_object_id, 26) == 0)
		return (NgDocument_HTTPCompatibleWebDocument__OTCreateTrue (str_object_id,
		*((ILU_C_Server *) p_iluserver), NULL));
	
	/* otherwise just assume that it's a WebDocument */
	return (NgDocument_WebDocument__OTCreateTrue (str_object_id,
		*((ILU_C_Server *) p_iluserver), NULL));
}


static void free_document_object_table_storage (ilu_private p_iluserver) {
  return; /* nothing to do yet */
}




void ngdocument_impl_setup_server() {

	static ilu_boolean b_already_setup = ilu_FALSE;
	ILU_C_ObjectTable object_table;
	char pc_serverid[1024];						/* holds a server id */

	if (b_already_setup)
		return ;
	
	/* create object table and ILU server*/

	/* DocumentServer */
	object_table = ILU_C_CreateObjectTable (object_of_documentwebdocobj, 
			free_document_object_table_storage, (ilu_private) &g_document_server);

	sprintf (pc_serverid, "DocumentServer.%s%s", g_pc_hostname, g_pc_ngwebserver_suffix);

	g_document_server = ILU_C_InitializeServer ( pc_serverid, object_table, 
		g_pc_nonhttp_pinfo, get_ngtinfo(), (ilu_Passport) ILU_NIL, ilu_TRUE );

	b_already_setup = ilu_TRUE;

}

/* ************************************************************************ */
/* HTTPCompatibleWebDocument methods */

NgRendering_RenderingPreferences* 
server_NgDocument_HTTPCompatibleWebDocument_GetAvailableRenderings 
(NgDocument_HTTPCompatibleWebDocument webdocobj, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_HTTPCompatibleWebDocument_GetAvailableRenderings");
    return NULL;
}



NgRendering_Rendering* 
server_NgDocument_HTTPCompatibleWebDocument_GetRendering 
(NgDocument_HTTPCompatibleWebDocument webdocobj, 
 NgRendering_RenderingPreferences* renderingPreferences, 
 NgCache_OptionalRequestCacheControl requestCacheInfo, 
 NgCache_OptionalResponseCacheControl* responseCacheInfo,
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_HTTPCompatibleWebDocument_GetRendering");
    return NULL;
}



void 
server_NgDocument_HTTPCompatibleWebDocument_SendRendering 
(NgDocument_HTTPCompatibleWebDocument webdocobj, 
 NgRendering_RenderingPreferences* renderingPreferences, 
 NgCache_RequestCacheControl* requestCacheInfo,
 NgRendering_RenderingSink renderSink, 
 NgBasic_OptionalCardinal suggestedChunkSize, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_HTTPCompatibleWebDocument_SendRendering");
    return ;
}



void 
server_NgDocument_HTTPCompatibleWebDocument_SendRenderingSynched 
(NgDocument_HTTPCompatibleWebDocument webdocobj, 
 NgRendering_RenderingPreferences* renderingPreferences, 
 NgCache_RequestCacheControl* requestCacheInfo,
 NgRendering_RenderingSink renderSink, 
 NgBasic_OptionalCardinal suggestedChunkSize, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_HTTPCompatibleWebDocument_SendRenderingSynched");
    return ;
}



NgBasic_String 
server_NgDocument_HTTPCompatibleWebDocument_GetInterfaceDefinitionSource 
(NgDocument_HTTPCompatibleWebDocument webdocobj, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_HTTPCompatibleWebDocument_GetInterfaceDefinitionSource");
    return NULL;
}



NgProperty_PropertySequence* 
server_NgDocument_HTTPCompatibleWebDocument_GetProperties
(NgDocument_HTTPCompatibleWebDocument webdocobj, 
 NgProperty_PropertyNames* propertiesToGet, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_HTTPCompatibleWebDocument_GetProperties");
    return NULL;
}



NgRendering_Rendering* 
server_NgDocument_HTTPCompatibleWebDocument_GetRenderingAndProperties 
(NgDocument_HTTPCompatibleWebDocument webdocobj, 
 NgRendering_RenderingPreferences* renderingPreferences, 
 NgCache_OptionalRequestCacheControl requestCacheInfo, 
 NgCache_OptionalResponseCacheControl* responseCacheInfo,
 NgProperty_PropertyNames* propertiesToGet, 
 NgProperty_PropertySequence** theproperties, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_HTTPCompatibleWebDocument_GetRenderingAndProperties");
    return NULL;
}



void 
server_NgDocument_HTTPCompatibleWebDocument_SendRenderingAndProperties 
(NgDocument_HTTPCompatibleWebDocument webdocobj, 
 NgRendering_RenderingPreferences* renderingPreferences, 
 NgCache_RequestCacheControl* requestCacheInfo, 
 NgProperty_PropertyNames* propertiesToGet, 
 NgDocument_RenderingAndPropertiesSink renderPropSink, 
 NgBasic_OptionalCardinal suggestedChunkSize, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_HTTPCompatibleWebDocument_SendRenderingAndProperties");
    return ;
}



void 
server_NgDocument_HTTPCompatibleWebDocument_SendRenderingAndPropertiesSynched 
(NgDocument_HTTPCompatibleWebDocument webdocobj, 
 NgRendering_RenderingPreferences* renderingPreferences, 
 NgCache_RequestCacheControl* requestCacheInfo, 
 NgProperty_PropertyNames* propertiesToGet,
 NgDocument_RenderingAndPropertiesSink renderPropSink, 
 NgBasic_OptionalCardinal suggestedChunkSize, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_HTTPCompatibleWebDocument_SendRenderingAndPropertiesSynched");
    return ;
}



iluhttp_Response* 
server_NgDocument_HTTPCompatibleWebDocument_GET 
(NgDocument_HTTPCompatibleWebDocument webdocobj, 
 iluhttp_Request* request, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_HTTPCompatibleWebDocument_GET");
    return NULL;
}



iluhttp_Response* 
server_NgDocument_HTTPCompatibleWebDocument_HEAD 
(NgDocument_HTTPCompatibleWebDocument webdocobj, 
 iluhttp_Request* request, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_HTTPCompatibleWebDocument_HEAD");
    return NULL;
}



iluhttp_Response* 
server_NgDocument_HTTPCompatibleWebDocument_POST
(NgDocument_HTTPCompatibleWebDocument webdocobj, 
 iluhttp_Request* request, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_HTTPCompatibleWebDocument_POST");
    return NULL;
}


/* ************************************************************************ */
/* PutableWebDocument methods */


NgRendering_RenderingPreferences* 
server_NgDocument_PutableWebDocument_GetAvailableRenderings 
(NgDocument_PutableWebDocument webdocobj, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_PutableWebDocument_GetAvailableRenderings");
    return NULL;
}



NgRendering_Rendering* 
server_NgDocument_PutableWebDocument_GetRendering 
(NgDocument_PutableWebDocument webdocobj, 
 NgRendering_RenderingPreferences* renderingPreferences, 
 NgCache_OptionalRequestCacheControl requestCacheInfo, 
 NgCache_OptionalResponseCacheControl* responseCacheInfo,
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_PutableWebDocument_GetRendering");
    return NULL;
}



void 
server_NgDocument_PutableWebDocument_SendRendering 
(NgDocument_PutableWebDocument webdocobj, 
 NgRendering_RenderingPreferences* renderingPreferences, 
 NgCache_RequestCacheControl* requestCacheInfo, 
 NgRendering_RenderingSink renderSink, 
 NgBasic_OptionalCardinal suggestedChunkSize, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_PutableWebDocument_SendRendering");
    return ;
}





NgProperty_PropertySequence* 
server_NgDocument_PutableWebDocument_GetProperties 
(NgDocument_PutableWebDocument webdocobj, 
 NgProperty_PropertyNames* propertiesToGet, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_PutableWebDocument_GetProperties");
    return NULL;
}



NgRendering_Rendering* 
server_NgDocument_PutableWebDocument_GetRenderingAndProperties 
(NgDocument_PutableWebDocument webdocobj, 
 NgRendering_RenderingPreferences* renderingPreferences, 
 NgCache_OptionalRequestCacheControl requestCacheInfo, 
 NgCache_OptionalResponseCacheControl* responseCacheInfo,
 NgProperty_PropertyNames* propertiesToGet, 
 NgProperty_PropertySequence** theproperties, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_PutableWebDocument_GetRenderingAndProperties");
    return NULL;
}



void 
server_NgDocument_PutableWebDocument_SendRenderingAndProperties 
(NgDocument_PutableWebDocument webdocobj, 
 NgRendering_RenderingPreferences* renderingPreferences, 
 NgCache_RequestCacheControl* requestCacheInfo, 
 NgProperty_PropertyNames* propertiesToGet, 
 NgDocument_RenderingAndPropertiesSink renderPropSink, 
 NgBasic_OptionalCardinal suggestedChunkSize, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_PutableWebDocument_SendRenderingAndProperties");
    return ;
}



void 
server_NgDocument_PutableWebDocument_SendRenderingAndPropertiesSynched 
(NgDocument_PutableWebDocument webdocobj, 
 NgRendering_RenderingPreferences* renderingPreferences,
 NgCache_RequestCacheControl* requestCacheInfo, 
 NgProperty_PropertyNames* propertiesToGet, 
 NgDocument_RenderingAndPropertiesSink renderPropSink,
 NgBasic_OptionalCardinal suggestedChunkSize, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_PutableWebDocument_SendRenderingAndPropertiesSynched");
    return ;
}



NgBasic_String 
server_NgDocument_PutableWebDocument_GetInterfaceDefinitionSource
(NgDocument_PutableWebDocument webdocobj,
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_PutableWebDocument_GetInterfaceDefinitionSource");
    return NULL;
}




void
server_NgDocument_PutableWebDocument_SendRenderingSynched
(NgDocument_PutableWebDocument webdocobj, 
 NgRendering_RenderingPreferences* renderingPreferences, 
 NgCache_RequestCacheControl* requestCacheInfo,
 NgRendering_RenderingSink renderSink, 
 NgBasic_OptionalCardinal suggestedChunkSize, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_PutableWebDocument_SendRenderingSynched");
    return ;
}



void 
server_NgDocument_PutableWebDocument_PutRendering
(NgDocument_PutableWebDocument webdocobj, 
 NgRendering_Rendering* renderingInput, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_PutableWebDocument_PutRendering");
    return ;
}




void 
server_NgDocument_PutableWebDocument_PutProperties
(NgDocument_PutableWebDocument webdocobj, 
 NgProperty_PropertyModificationSequence* propertiesToSet, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_PutableWebDocument_PutProperties");
    return ;
}



void 
server_NgDocument_PutableWebDocument_PutRenderingAndProperties 
(NgDocument_PutableWebDocument webdocobj, 
 NgRendering_Rendering* renderingInput,
 NgProperty_PropertyModificationSequence* propertiesToSet,
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_PutableWebDocument_PutRenderingAndProperties");
    return ;
}



/* ************************************************************************ */
/*  WebDocument method utilities */


/* create a NgRendering_RenderingPreferences structure suitable
for return via GetAvailableRenderings, etc., basic things filled in */

NgRendering_RenderingPreferences* 
make_NgRendering_RenderingPreferences() {
	NgRendering_RenderingPreferences* p_preferences;

	p_preferences = nglib_NgRendering_RenderingPreferences_clean_allocate();

		/* we assume everything we deal with US_ASCII, and en locale */
	IANA_Charsets_Registry_CharsetMIBEnumValueSequence_Push(&(p_preferences->acceptCharsets), 
		 IANA_Charsets_Registry_US_ASCII); 

	NgBasic_StringSequence_Push(&(p_preferences->acceptLocales), nglib_duplicate_c_string("en"));

	p_preferences->range = (NgBasic_OptionalUnsignedInclusiveInterval)
		ilu_malloc(sizeof(NgBasic_UnsignedInclusiveInterval));
	p_preferences->range->startValue = 0;
	p_preferences->range->endValue = 0;

	return p_preferences;

}


/* create a NgRendering_Rendering structure suitable
for return via GetRendering, etc., basic things filled in */

NgRendering_Rendering* 
make_NgRendering_Rendering() {
	NgRendering_Rendering* p_rendering;
	
	p_rendering = nglib_NgRendering_Rendering_clean_allocate();
	
	p_rendering->contentRange = (NgBasic_OptionalUnsignedInclusiveInterval)
		ilu_malloc(sizeof(NgBasic_UnsignedInclusiveInterval));
	
	p_rendering->contentRange->startValue = 0;
	
	p_rendering->contentRange->endValue = 0;
	
	return p_rendering;
}


/* Given a NgDocument_WebDocument, this function returns a fd for a file open in mode
i_file_mode to the native content of the document, sets ppc_content_type 
to point to a content type string (allocated in static storage), and fills
in *p_stat_struct with the result of an fstat call.  -1 may be
returned if the file is outside of g_pc_file_base, or if it can't be
opened in i_file_mode. If ppc_object_id non null, object id is put there,
and owned by the caller. */
int get_NgDocument_WebDocument_NativeContentFile(NgDocument_WebDocument webdocobj,
												   char** ppc_content_type, 
												   int i_file_mode,
												   STAT_STRUCT* p_stat_struct,
												   char** ppc_object_id) {

	char* pc_this_server_id;
	char* pc_this_object_id;
	char c_filename[1024];
	int i_length;
	int h_file;
	
	/* get a hold of the instance ID */
	if (!ILU_C_IDOfObject( webdocobj, &pc_this_server_id, &pc_this_object_id))
		return -1;

	/* log appropriately */
	if (DO_LOG_BASIC()) {
		LOG_ENTER();
		fprintf(g_p_logfile, "\n------------------------------------------\n");
		fprintf(g_p_logfile, "NativeContentFile checking %s\n", pc_this_object_id);
		LOG_EXIT();
	}
	
    /* In this server, the WebDocument keeps its native content in a file,
		The pathname to this file is g_pc_file_base/, followed by the
	   part of the instance id as outlined in the following section's comments */

	if ((strcmp(pc_this_object_id, "WebDocument:") == 0) ||
		(strcmp(pc_this_object_id, "/") == 0)) 
		/* instance handle is 'just' WebDocument: or just / -- indicates the 'default' document */
		i_length = sprintf(c_filename, "%s%s", g_pc_file_base, DEFAULT_SERVER_FILE_NAME );
	
	else if (strncmp("WebDocument:", pc_this_object_id, 12) == 0) 
		/* instance handle begins with WebDocument:, use what follows that as path from base */
		i_length = sprintf(c_filename, "%s/%s", g_pc_file_base, pc_this_object_id + 12 );
		else /* instance handle doesn't start with WebDocument: and isn't just / -- treat instance handle
			as path from base*/
			i_length = sprintf(c_filename, "%s/%s", g_pc_file_base, pc_this_object_id);		

	ilu_free(pc_this_server_id);  /* don't need these anymore */
	if (ppc_object_id)
		*ppc_object_id = pc_this_object_id;
	else
		ilu_free(pc_this_object_id); 
	
	if (nglib_validate_path(c_filename, g_pc_file_base) == ilu_FALSE)
		/* return NULL if we've gone outside our root */
		return -1;
	
	/* open up the file */
	h_file = OPEN_FUNCTION(c_filename, i_file_mode); 
	if ((h_file == -1) || (FSTAT_FUNCTION(h_file, p_stat_struct) == -1)) {
		if (ppc_object_id){
			ilu_free(*ppc_object_id); 
			*ppc_object_id = NULL;;
		}
		/* return not exist if there's a problem opening the file */
		return -1;
	}
	
	/* get the content type from the file name extension */
	*ppc_content_type = nglib_content_type_from_extension(
		nglib_extension_in_pathname(c_filename, i_length));

	if (DO_LOG_BASIC()) {
		ilu_Passport callers_passport;
		LOG_ENTER();
		fprintf(g_p_logfile, "get_NgDocument_WebDocument_NativeContentFile, %s, size %u",
			c_filename, p_stat_struct->st_size);
		callers_passport = ILU_C_CallerIdentity();
		if (callers_passport) {
			ilu_IdentityInfo caller_id;
			caller_id = ILU_C_FindIdentity(callers_passport, ilu_ConnectionIdentity);
			if (caller_id) {
				char c_string_form[1024];
				ilu_Error ilu_error;
				ilu_DisplayIdentity(caller_id, c_string_form, 1024, &ilu_error);
				if (ILU_ERRNOK(ilu_error))
					ILU_HANDLED(ilu_error);
				else
					fprintf(g_p_logfile, ", Caller Identity = %s", c_string_form);
			}
			
		}
		fprintf(g_p_logfile, "\n");
		LOG_EXIT();
	}

    return h_file;
}




/* ************************************************************************ */
/*  WebDocument methods */


NgRendering_RenderingPreferences* 
server_NgDocument_WebDocument_GetAvailableRenderings 
(NgDocument_WebDocument webdocobj, 
 ILU_C_ENVIRONMENT* ilu_env) {
	
	NgRendering_RenderingPreferences* p_preferences;
	int h_native_content_file;
	char* pc_content_type;
	STAT_STRUCT stat_struct;
	
	if (g_batcher) /* ensure batching if appropriate */
		ILU_C_SetBatcherContext(g_batcher); 

	/* get ahold of our native content */
	h_native_content_file = get_NgDocument_WebDocument_NativeContentFile(webdocobj,
		&pc_content_type, 
		RDONLY_MODE | BINARY_MODE,
		&stat_struct,
		NULL);
	
	CLOSE_FUNCTION(h_native_content_file); /* don't really need the file open */
	
	if (h_native_content_file == -1) {
		nglib_assign_ObjectNotExist(ilu_env, "Native content not found", NULL);
		return NULL;
	}
	
	if (!pc_content_type) {
		nglib_assign_ObjectNotExist(ilu_env, "Unknown content type", NULL);
		return NULL;
	}
	
	/* fill in and return the preferences */
    p_preferences = make_NgRendering_RenderingPreferences();
	NgRendering_RenderingTypeSequence_Push(&(p_preferences->allowContentTypes), 
		nglib_duplicate_c_string(pc_content_type));
	
	p_preferences->range->endValue = stat_struct.st_size;
	
    return p_preferences;
}



NgRendering_Rendering* 
server_NgDocument_WebDocument_GetRendering 
(NgDocument_WebDocument webdocobj,
 NgRendering_RenderingPreferences* renderingPreferences,
 NgCache_OptionalRequestCacheControl requestCacheInfo, 
 NgCache_OptionalResponseCacheControl* responseCacheInfo,
 ILU_C_ENVIRONMENT* ilu_env) {
	
	int h_native_content_file;
	char* pc_content_type;
	STAT_STRUCT stat_struct;
	NgRendering_Rendering* p_rendering;
	unsigned long ul_startbyte;
	unsigned long ul_endbyte;

	ilu_boolean b_like_head = ilu_FALSE;	

	if (g_batcher) /* ensure batching if appropriate */
		ILU_C_SetBatcherContext(g_batcher); 

	/* get ahold of our native content */
	h_native_content_file = get_NgDocument_WebDocument_NativeContentFile(webdocobj,
		&pc_content_type, 
		RDONLY_MODE | BINARY_MODE,
		&stat_struct,
		NULL);
	
	
	if (h_native_content_file == -1) {
		nglib_assign_ObjectNotExist(ilu_env, "Native content not found", NULL);
		return NULL;
	}
	
	if (!pc_content_type) {
		nglib_assign_ObjectNotExist(ilu_env, "Unknown content type", NULL);
		CLOSE_FUNCTION(h_native_content_file);
		return NULL;
	}
	
	/* check content type, language, locale */
	if ( /* not acceptible */
		((renderingPreferences->allowContentTypes._length != 0) &&
		!nglib_is_rendering_type_member(&(renderingPreferences->allowContentTypes), pc_content_type)) ||
		
		/* on the disallowed list */
		((renderingPreferences->disallowContentTypes._length != 0) &&
		nglib_is_rendering_type_member(&(renderingPreferences->disallowContentTypes), pc_content_type)) ||
		
		/* acceptable character set */
		((renderingPreferences->acceptCharsets._length != 0) &&
		!nglib_is_IANA_Charsets_Registry_CharsetMIBEnumValueSequence_member (
		&(renderingPreferences->acceptCharsets), IANA_Charsets_Registry_US_ASCII)) ||
		
		/* acceptable locale */
		((renderingPreferences->acceptLocales._length != 0) &&
		!nglib_is_NgBasic_StringSequence_member (
		&(renderingPreferences->acceptLocales), "en"))
		) {
		NgRendering_RenderingPreferences* p_preferences;
		
		/* fill in and return the preferences in an exception */
		p_preferences = make_NgRendering_RenderingPreferences();
		NgRendering_RenderingTypeSequence_Push(&(p_preferences->allowContentTypes), 
			nglib_duplicate_c_string(pc_content_type));
		
		p_preferences->range->endValue = stat_struct.st_size - 1;		
		
		nglib_assign_NoRenderingMatch(ilu_env, p_preferences);
		CLOSE_FUNCTION(h_native_content_file);
		return NULL;
	}
	
	/* xxx note we ignore encoding preferences completely */

	/* xxx we also ignore CacheControl completely */
	responseCacheInfo = NULL;

	/* OK, we're looking good, lets set up a Rendering to return */
	
	p_rendering = make_NgRendering_Rendering();
	
	/* set the content type */
	p_rendering->contentType = nglib_duplicate_c_string(pc_content_type);
	
	/* find out how many bytes are wanted */
	if (renderingPreferences->range == NULL) {
		/* entire rendering requested */
		ul_startbyte = 0;
		ul_endbyte = stat_struct.st_size - 1; 
	}
	else if (renderingPreferences->range->startValue == renderingPreferences->range->endValue) {
		/* like http HEAD method */
		b_like_head = ilu_TRUE;
		ul_startbyte = 0;
		ul_endbyte = stat_struct.st_size - 1; 
	}
	else {
		ul_startbyte = renderingPreferences->range->startValue;
		ul_endbyte = nglib_min((unsigned long)(stat_struct.st_size - 1), 
			renderingPreferences->range->endValue);
	}
	
	if (b_like_head) {
		/* doesn't really want the bytes themselves */
		CLOSE_FUNCTION(h_native_content_file);
		
		return p_rendering;
	}
	
	/* seek to start of range */
	if (ul_startbyte != 0 &&
		SEEK_FUNCTION(h_native_content_file, ul_startbyte, SEEK_CUR) == -1) {
		nglib_assign_ObjectNotExist(ilu_env, "Couldn't seek in native content", NULL);
		CLOSE_FUNCTION(h_native_content_file);
		NgRendering_Rendering__Free(p_rendering);
		ilu_free(p_rendering);		
		return NULL;
	}
	
	
	/* get a buffer for the rendering bytes */
	p_rendering->contentRange->startValue = ul_startbyte;
	p_rendering->contentRange->endValue = ul_endbyte;
	p_rendering->renderingBytes._maximum = ul_endbyte - ul_startbyte + 1;
	p_rendering->renderingBytes._length = p_rendering->renderingBytes._maximum;
	p_rendering->renderingBytes._buffer = (CORBA_octet*) 
		ilu_malloc(p_rendering->renderingBytes._maximum);
	if (READ_FUNCTION(h_native_content_file, p_rendering->renderingBytes._buffer, 
		p_rendering->renderingBytes._maximum) == -1) {
		nglib_assign_ObjectNotExist(ilu_env, "Couldn't read native content", NULL);
		CLOSE_FUNCTION(h_native_content_file);
		NgRendering_Rendering__Free(p_rendering);
		ilu_free(p_rendering);		
		return NULL;
	}
	
	CLOSE_FUNCTION(h_native_content_file);


	if (DO_LOG_SUMMARY()) {
		LOG_ENTER();
		g_i_request_count++;
		LOG_EXIT();
	}
	
	return p_rendering;
}


void send_sink_objectnotexist_problem_report(NgRendering_RenderingSink renderSink, char* pc_reason) {
	
	ILU_C_ENVIRONMENT local_env;
	/* send back a problem report to the sink */
	NgRendering_RenderingProblemReport problem;
	
	problem._d = 2;
	problem._u.objectNotExist.reasonPhrase = pc_reason;
	problem._u.objectNotExist.specificsData = NULL;
	NgRendering_RenderingSink_RenderingProblem(renderSink, &problem, &local_env);
	if (local_env._major != CORBA_NO_EXCEPTION) {
		if (DO_LOG_BASIC()) {
			LOG_ENTER();
			nglib_show_and_clear_if_exception(&local_env, "Calling NgRendering_RenderingSink_RenderingProblem", g_p_logfile);
			LOG_EXIT();
		}
		else nglib_show_and_clear_if_exception(&local_env, NULL, NULL);
	}
}



void 
server_NgDocument_WebDocument_SendRendering 
(NgDocument_WebDocument webdocobj, 
 NgRendering_RenderingPreferences* renderingPreferences, 
 NgCache_RequestCacheControl* requestCacheInfo, 
 NgRendering_RenderingSink renderSink, 
 NgBasic_OptionalCardinal suggestedChunkSize, 
 ILU_C_ENVIRONMENT* ilu_env) {
    	
	int h_native_content_file;
	char* pc_content_type;
	STAT_STRUCT stat_struct;
	unsigned long ul_startbyte;
	unsigned long ul_endbyte;
	ilu_boolean b_like_head = ilu_FALSE;
	ILU_C_ENVIRONMENT local_env;
	DataSourceObjectData* p_data_source_data;
	NgStream_DataSource data_source;
	ilu_Error an_error;
	char* pc_object_id;

	/* get ahold of our native content */
	h_native_content_file = get_NgDocument_WebDocument_NativeContentFile(webdocobj,
		&pc_content_type, 
		RDONLY_MODE | BINARY_MODE,
		&stat_struct,
		&pc_object_id);
	
	
	if (h_native_content_file == -1) {
		ilu_free(pc_object_id); 
		send_sink_objectnotexist_problem_report(renderSink, "Native content not found");
		return;
	}
	
	if (!pc_content_type) {
		ilu_free(pc_object_id); 
		send_sink_objectnotexist_problem_report(renderSink, "Unknown content type");
		CLOSE_FUNCTION(h_native_content_file);
		return;
	}
	
	/* check content type, language, locale */
	if ( /* not acceptable */
		((renderingPreferences->allowContentTypes._length != 0) &&
		!nglib_is_rendering_type_member(&(renderingPreferences->allowContentTypes), pc_content_type)) ||
		
		/* on the disallowed list */
		((renderingPreferences->disallowContentTypes._length != 0) &&
		nglib_is_rendering_type_member(&(renderingPreferences->disallowContentTypes), pc_content_type)) ||
		
		/* acceptable character set */
		((renderingPreferences->acceptCharsets._length != 0) &&
		!nglib_is_IANA_Charsets_Registry_CharsetMIBEnumValueSequence_member (
		&(renderingPreferences->acceptCharsets), IANA_Charsets_Registry_US_ASCII)) ||
		
		/* acceptable locale */
		((renderingPreferences->acceptLocales._length != 0) &&
		!nglib_is_NgBasic_StringSequence_member (
		&(renderingPreferences->acceptLocales), "en"))) {
		/* fill in and return the preferences in a report */
		NgRendering_RenderingProblemReport problem;
		NgBasic_UnsignedInclusiveInterval range;
		
		problem._d = 0;
		
		nglib_NgRendering_RenderingPreferences_clean_assign(&(problem._u.noMatch));
		
		NgRendering_RenderingTypeSequence_Push(&(problem._u.noMatch.allowContentTypes), 
			pc_content_type);
		problem._u.noMatch.range = &range;
		range.startValue = 0;
		problem._u.noMatch.range->endValue = stat_struct.st_size - 1;		
		
		NgRendering_RenderingSink_RenderingProblem(renderSink, &problem, &local_env);
		if (local_env._major != CORBA_NO_EXCEPTION) {
			if (DO_LOG_BASIC()) {
				LOG_ENTER();
				nglib_show_and_clear_if_exception(&local_env, "Calling NgRendering_RenderingSink_RenderingProblem", g_p_logfile);
				LOG_EXIT();
			}
			else nglib_show_and_clear_if_exception(&local_env, NULL, NULL);
		}

		ilu_free(pc_object_id); 
		CLOSE_FUNCTION(h_native_content_file);
		return ;
	}
	
	/* find out how many bytes are wanted */
	if (renderingPreferences->range == NULL) {
		/* entire rendering requested */
		ul_startbyte = 0;
		ul_endbyte = stat_struct.st_size - 1; 
	}
	else if (renderingPreferences->range->startValue == renderingPreferences->range->endValue) {
		/* like http HEAD method */
		b_like_head = ilu_TRUE;
		ul_startbyte = 0;
		ul_endbyte = stat_struct.st_size - 1; 
	}
	else {
		ul_startbyte = renderingPreferences->range->startValue;
		ul_endbyte = nglib_min((unsigned long)(stat_struct.st_size - 1), 
			renderingPreferences->range->endValue);
	}

	/* create a data source object */
	
	p_data_source_data = MallocDataSourceObjectData(h_native_content_file,
		ul_startbyte, 
		ul_endbyte,
		suggestedChunkSize ? *suggestedChunkSize : 0,
		nglib_duplicate_c_string(pc_content_type),
		b_like_head,
		pc_object_id,
		renderSink
	);

	if (!p_data_source_data) {
		/* xxx should return an out of memory exception of some sort here,
		but we can't since we're a asynchronous call! */
	}


	data_source = NgStream_DataSource__CreateTrue (NULL, g_stream_server, p_data_source_data);

	/* fork a thread to do the rest */ 
	/* xxx probably want a pool of worker threads instead of forking threads relentlessly */
	if (!ilu_OSForkNewThread ((nglib_fork_procedure)do_data_source_work, data_source, &an_error)) {
		if (DO_LOG_BASIC()) {
			LOG_ENTER();
			fprintf(g_p_logfile, "do_data_source_work - Couldn't fork thread\n");
			LOG_EXIT();
		}
		ILU_HANDLED(an_error);
		ILU_CLER(an_error);
	}

	if (DO_LOG_SUMMARY()) {
		LOG_ENTER();
		g_i_request_count++;
		LOG_EXIT();
	}

	return ;
}



void 
server_NgDocument_WebDocument_SendRenderingSynched 
(NgDocument_WebDocument webdocobj, 
 NgRendering_RenderingPreferences* renderingPreferences, 
 NgCache_RequestCacheControl* requestCacheInfo, 
 NgRendering_RenderingSink renderSink, 
 NgBasic_OptionalCardinal suggestedChunkSize, 
 ILU_C_ENVIRONMENT* ilu_env) {

  if (g_batcher) /* ensure batching if appropriate */
		ILU_C_SetBatcherContext(g_batcher); 

server_NgDocument_WebDocument_SendRendering( webdocobj, renderingPreferences, 
	requestCacheInfo, renderSink, suggestedChunkSize, ilu_env);
 return ;
}



NgBasic_String 
server_NgDocument_WebDocument_GetInterfaceDefinitionSource
(NgDocument_WebDocument webdocobj, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_WebDocument_GetInterfaceDefinitionSource");
    return NULL;
}



NgProperty_PropertySequence* 
server_NgDocument_WebDocument_GetProperties
(NgDocument_WebDocument webdocobj,
 NgProperty_PropertyNames* propertiesToGet, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_WebDocument_GetProperties");
    return NULL;
}



NgRendering_Rendering* 
server_NgDocument_WebDocument_GetRenderingAndProperties 
(NgDocument_WebDocument webdocobj, 
 NgRendering_RenderingPreferences* renderingPreferences, 
 NgCache_OptionalRequestCacheControl requestCacheInfo, 
 NgCache_OptionalResponseCacheControl* responseCacheInfo,
 NgProperty_PropertyNames* propertiesToGet, 
 NgProperty_PropertySequence** theproperties, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_WebDocument_GetRenderingAndProperties");
    return NULL;
}



void 
server_NgDocument_WebDocument_SendRenderingAndProperties
(NgDocument_WebDocument webdocobj, 
 NgRendering_RenderingPreferences* renderingPreferences, 
 NgCache_RequestCacheControl* requestCacheInfo, 
 NgProperty_PropertyNames* propertiesToGet, 
 NgDocument_RenderingAndPropertiesSink renderPropSink, 
 NgBasic_OptionalCardinal suggestedChunkSize, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_WebDocument_SendRenderingAndProperties");
    return ;
}



void 
server_NgDocument_WebDocument_SendRenderingAndPropertiesSynched 
(NgDocument_WebDocument webdocobj,
 NgRendering_RenderingPreferences* renderingPreferences, 
 NgCache_RequestCacheControl* requestCacheInfo, 
 NgProperty_PropertyNames* propertiesToGet, 
 NgDocument_RenderingAndPropertiesSink renderPropSink, 
 NgBasic_OptionalCardinal suggestedChunkSize, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_WebDocument_SendRenderingAndPropertiesSynched");
    return ;
}


/* ************************************************************************ */
/* RenderingAndPropertiesSink methods */


NgBasic_String 
server_NgDocument_RenderingAndPropertiesSink_GetInterfaceDefinitionSource 
(NgDocument_RenderingAndPropertiesSink webdocobj, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_RenderingAndPropertiesSink_GetInterfaceDefinitionSource");
    return NULL;
}



void 
server_NgDocument_RenderingAndPropertiesSink_RegisterSourceControl 
(NgDocument_RenderingAndPropertiesSink webdocobj, 
 NgStream_DataSource thesource, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_RenderingAndPropertiesSink_RegisterSourceControl");
    return ;
}



void
server_NgDocument_RenderingAndPropertiesSink_Done
(NgDocument_RenderingAndPropertiesSink webdocobj,
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_RenderingAndPropertiesSink_Done");
    return ;
}



void
server_NgDocument_RenderingAndPropertiesSink_RegisterResponseCacheControl 
(NgDocument_RenderingAndPropertiesSink webdocobj, 
 NgCache_ResponseCacheControl* responseCacheInfo, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_RenderingAndPropertiesSink_RegisterResponseCacheControl");
    return ;
}



void 
server_NgDocument_RenderingAndPropertiesSink_RenderingProblem 
(NgDocument_RenderingAndPropertiesSink webdocobj, 
 NgRendering_RenderingProblemReport* report,
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_RenderingAndPropertiesSink_RenderingProblem");
    return ;
}



void
server_NgDocument_RenderingAndPropertiesSink_ReceiveRendering 
(NgDocument_RenderingAndPropertiesSink webdocobj, 
 NgRendering_Rendering* therendering, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_RenderingAndPropertiesSink_ReceiveRendering");
    return ;
}



void
server_NgDocument_RenderingAndPropertiesSink_ReceiveRenderingChunk 
(NgDocument_RenderingAndPropertiesSink webdocobj, 
 NgRendering_RenderingChunk* thechunk, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_RenderingAndPropertiesSink_ReceiveRenderingChunk");
    return ;
}



NgBasic_OptionalCardinal 
server_NgDocument_RenderingAndPropertiesSink_Resynchronize 
(NgDocument_RenderingAndPropertiesSink webdocobj, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_RenderingAndPropertiesSink_Resynchronize");
    return NULL;
}



void 
server_NgDocument_RenderingAndPropertiesSink_PropertiesProblem 
(NgDocument_RenderingAndPropertiesSink webdocobj, 
 NgDocument_PropertiesProblemReport* report, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_RenderingAndPropertiesSink_PropertiesProblem");
    return ;
}



void 
server_NgDocument_RenderingAndPropertiesSink_ReceiveProperties 
(NgDocument_RenderingAndPropertiesSink webdocobj, 
 NgProperty_PropertySequence* theproperties, 
 ILU_C_ENVIRONMENT* ilu_env) {
    
    NGLIB_NYI("server_NgDocument_RenderingAndPropertiesSink_ReceiveProperties");
    return ;
}



