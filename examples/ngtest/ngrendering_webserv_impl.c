/** $Id: ngrendering_webserv_impl.c,v 1.5 1999/08/03 01:58:07 janssen Exp $
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
#include "NgRendering.h"


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
 NgCache_OptionalRequestCacheControl requestCacheInfo, 
 NgCache_OptionalResponseCacheControl* responseCacheInfo,
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgRendering_PutableRenderable_GetRendering");
    return NULL;
}



void 
server_NgRendering_PutableRenderable_SendRendering 
(NgRendering_PutableRenderable _handle, 
 NgRendering_RenderingPreferences* renderingPreferences,
 NgCache_OptionalRequestCacheControl requestCacheInfo, 
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
 NgCache_OptionalRequestCacheControl requestCacheInfo, 
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



NgBasic_String 
server_NgRendering_RenderingSink_GetInterfaceDefinitionSource 
(NgRendering_RenderingSink _handle,
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgRendering_RenderingSink_GetInterfaceDefinitionSource");
    return NULL;
}



void 
server_NgRendering_RenderingSink_RegisterSourceControl 
(NgRendering_RenderingSink _handle, 
 NgStream_DataSource thesource, 
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgRendering_RenderingSink_RegisterSourceControl");
    return ;
}



void 
server_NgRendering_RenderingSink_Done
(NgRendering_RenderingSink _handle,
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgRendering_RenderingSink_Done");
    return ;
}



void
server_NgRendering_RenderingSink_RegisterResponseCacheControl 
(NgRendering_RenderingSink _handle,
 NgCache_ResponseCacheControl* responseCacheInfo,
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgRendering_RenderingSink_RegisterResponseCacheControl");
    return ;
}



void
server_NgRendering_RenderingSink_RenderingProblem 
(NgRendering_RenderingSink _handle,
 NgRendering_RenderingProblemReport* report, 
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgRendering_RenderingSink_RenderingProblem");
    return ;
}



void
server_NgRendering_RenderingSink_ReceiveRendering 
(NgRendering_RenderingSink _handle, 
 NgRendering_Rendering* therendering, 
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgRendering_RenderingSink_ReceiveRendering");
    return ;
}



void 
server_NgRendering_RenderingSink_ReceiveRenderingChunk
(NgRendering_RenderingSink _handle,
 NgRendering_RenderingChunk* thechunk,
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgRendering_RenderingSink_ReceiveRenderingChunk");
    return ;
}



NgBasic_OptionalCardinal 
server_NgRendering_RenderingSink_Resynchronize 
(NgRendering_RenderingSink _handle, 
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgRendering_RenderingSink_Resynchronize");
    return NULL;
}



