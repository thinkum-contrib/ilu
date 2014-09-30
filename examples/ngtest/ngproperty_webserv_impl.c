/** $Id: ngproperty_webserv_impl.c,v 1.5 1999/08/03 01:58:11 janssen Exp $
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

#include "nglib.h"
#include "ngwebserver.h"

/* ILU server */
ILU_C_Server g_property_server     = NULL;

/* ********************************************************* */
/* object table and setup functions                          */
/* ********************************************************* */

static CORBA_Object object_of_property_handle (ilu_string str_object_id, 
										   ilu_private p_iluserver) {
	
	/* object ids in this server must begin with their typename :
	   e.g. w3ng:DocumentServer.pundit.parc.xerox.com/PropertySet:/foo/bar.html
	
	*/
	if (strncmp("PropertySet:", str_object_id, 14) == 0)
		return (NgProperty_PropertySet__OTCreateTrue (str_object_id,
		*((ILU_C_Server *) p_iluserver), NULL)); 

	if (strncmp("PutablePropertySet:", str_object_id, 14) == 0)
		return (NgProperty_PutablePropertySet__OTCreateTrue (str_object_id,
		*((ILU_C_Server *) p_iluserver), NULL)); 

	return NULL;
}


static void free_property_object_table_storage (ilu_private p_iluserver) {
  return; /* nothing to do yet */
}


void ngproperty_impl_setup_server() {

	static ilu_boolean b_already_setup = ilu_FALSE;
	ILU_C_ObjectTable object_table;
	char pc_serverid[1024];						/* holds a server id */

	if (b_already_setup)
		return ;
	
	/* create object table and ILU server*/

	/* PropertyServer */
	object_table = ILU_C_CreateObjectTable (object_of_property_handle, 
			free_property_object_table_storage, (ilu_private) &g_property_server);

	sprintf (pc_serverid, "PropertyServer.%s%s", g_pc_hostname, g_pc_ngwebserver_suffix);

	g_property_server = ILU_C_InitializeServer ( pc_serverid, object_table, 
		g_pc_nonhttp_pinfo, get_ngtinfo(), (ilu_Passport) ILU_NIL, ilu_TRUE );

	b_already_setup = ilu_TRUE;

}


NgBasic_String 
server_NgProperty_PutablePropertySet_GetInterfaceDefinitionSource 
(NgProperty_PutablePropertySet _handle, 
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgProperty_PutablePropertySet_GetInterfaceDefinitionSource");
    return NULL;
}



NgProperty_PropertySequence* 
server_NgProperty_PutablePropertySet_GetProperties 
(NgProperty_PutablePropertySet _handle, 
 NgProperty_PropertyNames* propertiesToGet, 
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgProperty_PutablePropertySet_GetProperties");
    return NULL;
}



void 
server_NgProperty_PutablePropertySet_PutProperties 
(NgProperty_PutablePropertySet _handle, 
 NgProperty_PropertyModificationSequence* propertiesToSet,
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgProperty_PutablePropertySet_PutProperties");
    return ;
}



NgBasic_String
server_NgProperty_PropertySet_GetInterfaceDefinitionSource 
(NgProperty_PropertySet _handle, 
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgProperty_PropertySet_GetInterfaceDefinitionSource");
    return NULL;
}



NgProperty_PropertySequence* 
server_NgProperty_PropertySet_GetProperties 
(NgProperty_PropertySet _handle, 
 NgProperty_PropertyNames* propertiesToGet, 
 ILU_C_ENVIRONMENT *_status) {
    
    NGLIB_NYI("server_NgProperty_PropertySet_GetProperties");
    return NULL;
}



