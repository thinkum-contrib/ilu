/** $Id: ngformprocessor_webserv_impl.c,v 1.6 1999/08/03 01:58:12 janssen Exp $
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

/* ILU server */
ILU_C_Server g_form_server     = NULL;


/* ********************************************************* */
/* object table and setup functions                          */
/* ********************************************************* */

static CORBA_Object object_of_form_handle (ilu_string str_object_id, 
										   ilu_private p_iluserver) {
	
	/* object ids in this server must begin with their typename :
	   e.g. w3ng:DocumentServer.pundit.parc.xerox.com/FormProcessor:/foo/bar.html
	
	*/
	if (strncmp("FormProcessor:", str_object_id, 14) == 0)
		return (NgFormProcessor_FormProcessor__OTCreateTrue (str_object_id,
		*((ILU_C_Server *) p_iluserver), NULL)); 

	return NULL;
}


static void free_form_object_table_storage (ilu_private p_iluserver) {
  return; /* nothing to do yet */
}


void ngform_impl_setup_server() {

	static ilu_boolean b_already_setup = ilu_FALSE;
	ILU_C_ObjectTable object_table;
	char pc_serverid[1024];						/* holds a server id */

	if (b_already_setup)
		return ;
	
	/* create object table and ILU server*/

	/* FormServer */
	object_table = ILU_C_CreateObjectTable (object_of_form_handle, 
			free_form_object_table_storage, (ilu_private) &g_form_server);

	sprintf (pc_serverid, "FormServer.%s%s", g_pc_hostname, g_pc_ngwebserver_suffix);

	g_form_server = ILU_C_InitializeServer ( pc_serverid, object_table, 
		g_pc_nonhttp_pinfo, get_ngtinfo(), (ilu_Passport) ILU_NIL, ilu_TRUE );

	b_already_setup = ilu_TRUE;

}


NgBasic_String 
server_NgFormProcessor_FormProcessor_GetInterfaceDefinitionSource 
(NgFormProcessor_FormProcessor _handle, 
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgFormProcessor_FormProcessor_GetInterfaceDefinitionSource");
    return NULL;
}



/* ProcessForm -- simply send back a page echoing the input */

static char* sg_process_form_rendering_start = "\
<HTML>\
<HEAD>\
<TITLE>NgFormProcessor_FormProcessor: Process Form Page</TITLE>\
</HEAD>\
<BODY>\
<H1>NgFormProcessor_FormProcessor: Process Form Page as Echo</H1>\
<HR>";

static char* sg_process_form_rendering_end = "</BODY></HTML>";

NgRendering_Rendering* 
server_NgFormProcessor_FormProcessor_ProcessForm 
(NgFormProcessor_FormProcessor _handle, 
 NgFormProcessor_FormInputElementSequence* formEntries,
 NgCache_ResponseCacheControl** responseCacheInfo, 
 ILU_C_ENVIRONMENT * ilu_env) {
    
	/* XXX hmmm, we have no rendering preferences as input! */
	extern NgRendering_Rendering* make_NgRendering_Rendering();
	NgRendering_Rendering* p_rendering;	
	char pc_bodybytes[4096]; /* XXX boldly assume rendering is never larger than this XXX */
	unsigned int ui_index;
	char* pc_walker = pc_bodybytes;
	NgFormProcessor_FormInputElement* p_pair;
	void* pv_temp;
	CORBA_TypeCode the_typecode;
	
	/* XXX need much more care in error checking, etc. */
	
	/* create an html pave that echos what was sent */
	pc_walker = pc_walker + sprintf(pc_walker, "%s", sg_process_form_rendering_start);
	
	for (ui_index = 0; ui_index < formEntries->_length; ui_index++) {
		p_pair = NgFormProcessor_FormInputElementSequence_Nth(formEntries, ui_index);
		pv_temp = NULL;
		
		the_typecode = ILU_C_Any_TypeCode (&(p_pair->value), ilu_env);
		
		if (the_typecode == TC_CORBA_string) {
			pc_walker = pc_walker + sprintf(pc_walker, "String : %s = ", p_pair->name);
			pv_temp = ILU_C_Any_Value (&(p_pair->value), ilu_env);
			pc_walker = pc_walker + sprintf(pc_walker, "%s",  *(char**)pv_temp);
			goto checked_label;
		}
		if (the_typecode == TC_CORBA_long) {
			pc_walker = pc_walker + sprintf(pc_walker, "Integer : %s = ", p_pair->name);
			pv_temp = ILU_C_Any_Value (&(p_pair->value), ilu_env);
			pc_walker = pc_walker + sprintf(pc_walker, "%d",  (*((int*)pv_temp)));
			goto checked_label;
		}
		if (the_typecode == TC_CORBA_boolean) {
			pc_walker = pc_walker + sprintf(pc_walker, "Boolean : %s = ", p_pair->name);
			pv_temp = ILU_C_Any_Value (&(p_pair->value), ilu_env);
			pc_walker = pc_walker + sprintf(pc_walker, "%s",  (pv_temp ? "True" : "False"));
			goto checked_label;
		}
		
		pc_walker = pc_walker + sprintf(pc_walker, "Unknown Type ");
		
checked_label:
		ilu_free(pv_temp);
		pc_walker = pc_walker + sprintf(pc_walker, "<HR>");
		
	}
	
	pc_walker = pc_walker + sprintf(pc_walker, "%s", sg_process_form_rendering_end);
	
	/* now make up a rendering and return it*/
	p_rendering = make_NgRendering_Rendering();
	
	/* set the content type */
	p_rendering->contentType = nglib_duplicate_c_string("text/html");
	
	/* set the range */
	p_rendering->contentRange->endValue = strlen(pc_bodybytes);
	
	/* set the rendering bytes */
	p_rendering->renderingBytes._length = p_rendering->contentRange->endValue;
	p_rendering->renderingBytes._maximum = p_rendering->renderingBytes._length + 1;
	p_rendering->renderingBytes._buffer = (unsigned char*)nglib_duplicate_c_string(pc_bodybytes);
	
    return p_rendering;
}



void 
server_NgFormProcessor_FormProcessor_SendFormReply 
(NgFormProcessor_FormProcessor _handle, 
 NgFormProcessor_FormInputElementSequence* formEntries, 
 NgFormProcessor_ProcessedFormSink formSink, 
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgFormProcessor_FormProcessor_SendFormReply");
    return ;
}



void 
server_NgFormProcessor_FormProcessor_SendFormReplySynched 
(NgFormProcessor_FormProcessor _handle, 
 NgFormProcessor_FormInputElementSequence* formEntries, 
 NgFormProcessor_ProcessedFormSink formSink, 
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgFormProcessor_FormProcessor_SendFormReplySynched");
    return ;
}



NgBasic_String
server_NgFormProcessor_ProcessedFormSink_GetInterfaceDefinitionSource 
(NgFormProcessor_ProcessedFormSink _handle, 
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgFormProcessor_ProcessedFormSink_GetInterfaceDefinitionSource");
    return NULL;
}



void 
server_NgFormProcessor_ProcessedFormSink_RegisterSourceControl
(NgFormProcessor_ProcessedFormSink _handle, 
 NgStream_DataSource thesource, 
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgFormProcessor_ProcessedFormSink_RegisterSourceControl");
    return ;
}



void
server_NgFormProcessor_ProcessedFormSink_Done 
(NgFormProcessor_ProcessedFormSink _handle,
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgFormProcessor_ProcessedFormSink_Done");
    return ;
}



void 
server_NgFormProcessor_ProcessedFormSink_RegisterResponseCacheControl 
(NgFormProcessor_ProcessedFormSink _handle, 
 NgCache_ResponseCacheControl* responseCacheInfo, 
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgFormProcessor_ProcessedFormSink_RegisterResponseCacheControl");
    return ;
}



void 
server_NgFormProcessor_ProcessedFormSink_RenderingProblem 
(NgFormProcessor_ProcessedFormSink _handle, 
 NgRendering_RenderingProblemReport* report, 
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgFormProcessor_ProcessedFormSink_RenderingProblem");
    return ;
}



void 
server_NgFormProcessor_ProcessedFormSink_ReceiveRendering 
(NgFormProcessor_ProcessedFormSink _handle, 
 NgRendering_Rendering* therendering,
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgFormProcessor_ProcessedFormSink_ReceiveRendering");
    return ;
}



void 
server_NgFormProcessor_ProcessedFormSink_ReceiveRenderingChunk
(NgFormProcessor_ProcessedFormSink _handle, 
 NgRendering_RenderingChunk* thechunk,
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgFormProcessor_ProcessedFormSink_ReceiveRenderingChunk");
    return ;
}



NgBasic_OptionalCardinal
server_NgFormProcessor_ProcessedFormSink_Resynchronize 
(NgFormProcessor_ProcessedFormSink _handle,
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgFormProcessor_ProcessedFormSink_Resynchronize");
    return NULL;
}



void server_NgFormProcessor_ProcessedFormSink_FormProblem
(NgFormProcessor_ProcessedFormSink _handle, 
 NgFormProcessor_FormProblemReport* report, 
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgFormProcessor_ProcessedFormSink_FormProblem");
    return ;
}



