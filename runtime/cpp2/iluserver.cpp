/*
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
/* $Id: iluserver.cpp,v 1.25 1999/08/18 02:37:08 janssen Exp $ */


// include ILU C++ header file
#include "ilu.hpp"

// for sprintf
#include <stdio.h>

// for OS_SLEEP
#include <oscalls.h>


//////////////////////////////////////////////////////////////////
// iluServer - provides a the C++ view of a kernel server object

//////////////////////////////////////////////////////////////////
// globals and statics

// protocol and transport to use by default - initialized to whatever is set to be the default
// in the kernel (found in iluconf.h or iluwin.h) - set from iluCppRuntime::iluInitialize()
char* iluServer::sm_pc_default_protocol = NULL;
iluTransportInfo iluServer::sm_ppc_default_transport_info = NULL;

// for in memory transport
char* iluServer::sm_pc_in_memory_protocol_info = NULL;
iluTransportInfo iluServer::sm_ppc_in_memory_transport_info = NULL;

// Iff a default server is needed, then this member points to it.
iluServer* iluServer::sm_p_default_server;

// sm_card_servercount (initially 0)
// are used to automatically generate unique server id's when needed.
iluCardinal iluServer::sm_card_server_counter = 0;

//////////////////////////////////////////////////////////////////
// constructor and destructor 

// Creates a server.  If no id is specified, one is automatically created based on
// based on time, hostname, and process id.  If p_object_table is null, a default
// object table implementation is used.  if b_addport is TRUE, a port is created
// and added to the server using the specified protocol and transport, and becomes the default
// port of the server.  pc_protocol_type and transport_info default to whattever 
// the default protocol and transport are currently set to. Caller owns pc_server_id
// p_object_table, pc_protocol_type, transport_info, and p_passport. p_passport points 
// to an iluPassport, defaulted to null -  this passport containing an ILU GSS identity, 
// which is used as the identity of the principal offering the service, and put into the 
// connection information in the string binding handle of objects on that server.

iluServer::iluServer(char* pc_server_id, 
		     iluObjectTable* p_object_table /* ILUowned */, 
		     iluProtocolInfo  pc_protocol_type,
		     iluTransportInfo transport_info, 
		     iluPassport* p_passport,
		     ILUCPP_BOOL b_addport
		     ) {
	
	ILU_ERRS((internal, no_memory)) an_error;
	char* pc_server_id_to_use;
	
	ILUCPP_DEBUG1("iluServer::iluServer entered\n");
	
	// ensure we have a server id to use, inventing one if need be
	if (pc_server_id == NULL)
		pc_server_id_to_use = ilu_InventID();
	else {
		pc_server_id_to_use = ilu_StrdupE(pc_server_id, &an_error);
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	}
	
	// create the kernel server itself 
	m_kernel_server = ilu_CreateTrueServer(pc_server_id_to_use, 
		(p_object_table ? p_object_table->iluGetKernelObjectTable() : NULL), 
		iluCppInternal::iluGetCppLanguageIndex(), &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);

	// set the language specific server of the kernel server to this iluServer
	ilu_SetLSS(m_kernel_server, this, iluCppInternal::iluGetCppLanguageIndex(), &an_error);
	if (ILU_ERRNOK(an_error))	
		iluExitServerMutex(ilu_rootClass);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);

	iluExitServerMutex(ilu_rootClass);

	// add an in memory transport port
	iluAddPort(sm_pc_in_memory_protocol_info, sm_ppc_in_memory_transport_info, ILUCPP_FALSE, p_passport, ILUCPP_FALSE);
	
	// add a port as specified in the arguments - defaulting appropriately
	if (b_addport)
	  iluAddPort((pc_protocol_type ? pc_protocol_type : iluGetDefaultProtocol()),
		     (transport_info ? transport_info : iluGetDefaultTransport()), ILUCPP_TRUE,
		     p_passport, ILUCPP_TRUE); 
	
	// set any object table to know that it's for this server
	if (p_object_table) 
		p_object_table->iluSetServer(this);

	ILUCPP_DEBUG2("iluServer::iluServer True server %s created and ports added\n", pc_server_id_to_use);

	return;
}


//////////////////////////////////////////////////////////////////////
// used to get around some compilers complaining about passing a c++
// function where a c function is expected.

extern "C" {typedef int (*C_iluUnlinkKernelObject) (iluKernelObject, ilu_refany );}




//////////////////////////////////////////////////////////////////
// destructor - basically whacks the kernel server and breaks
// all associations between kernel objects in this server and 
// and their language specific objects. Indirectly also deletes any
// iluObjectTable used with this iluServer.

iluServer::~iluServer() {

	// close down the kernel server - the callback disassociates all the
	// server's kernel objects from the language specific objects
	ilu_BankAndScanServer(m_kernel_server, 
			      REINTERPRET_CAST(C_iluUnlinkKernelObject, iluObject::iluUnlinkKernelObject), 
			      this, (iluCardinal*)0);

	// unlink this iluServer from the kernel server
	iluError an_error;
	iluEnterServerMutex(ilu_rootClass);
	ilu_SetLSS(m_kernel_server, NULL, iluCppInternal::iluGetCppLanguageIndex(), &an_error);
	if (ILU_ERRNOK(an_error))	
		iluExitServerMutex(ilu_rootClass);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);

	ILUCPP_DEBUG1("iluServer::~iluServer() True server destructed\n");
}


//////////////////////////////////////////////////////////////////
// Adding ports	


//////////////////////////////////////////////////////////////////////
// used to get around some compilers complaining about passing a c++
// function where a c function is expected.

extern "C" {typedef void (*C_iluNonThreadedReadConnectionRequests)(void*); 
}


// Adds another port to an existing server If b_become_default_port is 
// ilu_TRUE the new port will become the default port for this server.
// p_passport is an iluPassport, defaulted to nil. Caller owns the arguments.
void iluServer::iluAddPort (iluProtocolInfo pc_protocol_type,
			    iluTransportInfo transport_info,
			    ILUCPP_BOOL b_become_default_port, 
			    iluPassport* p_passport,
			    ILUCPP_BOOL b_public) {	
	iluError an_error;
	iluPort new_port;
	
	ILUCPP_DEBUG1("iluServer::iluAddPort()\n");

	// make up a new port
	new_port = ilu_FullCreatePort(m_kernel_server, pc_protocol_type, transport_info, 
				      (p_passport ? p_passport->iluGetIluPassport() : NULL),
				      (b_public ? iluTRUE : iluFALSE), &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	// set it as the default if we're supposed to 
	if (b_become_default_port) 
		ilu_SetServerDefaultPort(m_kernel_server, new_port);
	
	if (iluCppInternal::iluGetRunningThreaded() && 
		transport_info != sm_ppc_in_memory_transport_info) {
		// fork off a thread to monitor for connection requests
		iluCppInternal::iluThrowingFork(iluCppInternal::iluThreadedReadConnectionRequests, new_port);
	} 
	else { // tell ilu the routine to call when something comes in 
		ilu_SetConnectionRequestHandler(new_port, 
						REINTERPRET_CAST(C_iluNonThreadedReadConnectionRequests, 
								 iluCppInternal::iluNonThreadedReadConnectionRequests),
						new_port, &an_error);
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	}
}


// Get the Cinfo of a server.
// If b_public, get the cinfo of the first public port.  Otherwise, the first
// private port.
ILUCPP_BOOL iluServer::iluGetCInfo (iluProtocolInfo *pp_pinfo,
				    iluTransportInfo *pp_tinfo,
				    ILUCPP_BOOL b_public)
{
  iluError an_error;
  iluProtocolInfo p_local_pinfo;
  iluTransportInfo  p_local_tinfo;
  iluBoolean b_status;

  ILUCPP_DEBUG1("iluServer::iluGetCInfo()\n");

  // get the server mutex
  ilu_EnterServerMutex(m_kernel_server, iluFALSE, &an_error);
  ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);

  // get the info
  b_status = ilu_ServerCInfo (m_kernel_server, (b_public ? iluTRUE : iluFALSE),
			      &p_local_pinfo, &p_local_tinfo, &an_error);
  ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);

  *pp_pinfo = ilu_StrdupE(p_local_pinfo, &an_error);
  ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);

  *pp_tinfo = ilu_CopyTinfo (p_local_tinfo, &an_error);
  ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);

  ilu_ExitServerMutex(m_kernel_server, ilu_FALSE, &an_error);
  ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);

  return (b_status ? ILUCPP_TRUE : ILUCPP_FALSE);
}


// Add the specified cinfo to the server.
void iluServer::iluAddCInfo (const iluProtocolInfo p_pinfo,
			     const iluTransportInfo p_tinfo)
{
  iluError an_error;

  ILUCPP_DEBUG1("iluServer::iluAddCInfo()\n");
  ilu_AddCInfo (m_kernel_server, p_pinfo, p_tinfo, &an_error);
  ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
}


//////////////////////////////////////////////////////////////////
// Server relocation

extern "C" {
    static void s_f_server_relocate_shim (ilu_Server server,
					  void *rock,
					  iluError *err) {
	iluServerRelocator *p_relocate_obj = (iluServerRelocator *) rock;
	iluProtocolInfo p_pinfo = NULL;
	iluTransportInfo p_tinfo = NULL;
	if (!(p_relocate_obj->doRelocation(&p_pinfo, &p_tinfo))) {
	    ILU_CLER(*err);
	} else {
	    ILU_ERR_CONS3(relocate,err,rel_scope,ilu_relocate_conn,rel_pinfo,p_pinfo,rel_tinfo,p_tinfo,0);
	}
    }
}

iluServerRelocator* iluServer::iluSetRelocator (iluServerRelocator *p_relocator_obj)
{
    iluError an_error, another_error;
    iluServerRelocator *p_old_relocator;

    ILUCPP_DEBUG1("iluServer::iluGetCInfo()\n");

    // get the server mutex
    ilu_EnterServerMutex(m_kernel_server, iluTRUE, &an_error);
    ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
    
    // set the new relocator obj, and get the old one
    p_old_relocator = (iluServerRelocator *) ilu_SetServerRelocateProc(m_kernel_server,
								       &s_f_server_relocate_shim,
								       (void *) p_relocator_obj,
								       &an_error);
    // exit the server mutex
    ilu_ExitServerMutex(m_kernel_server, iluTRUE, &another_error);
    ILU_HANDLED(another_error);

    // now check any error from ilu_SetServerRelocateProc
    ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);

    // finally, return the old locator obj, if any
    return p_old_relocator;
}


//////////////////////////////////////////////////////////////////
// Running

// This runs the main, outer loop of an iluServer. It never returns
// if p_i_stop_on_non_zero isn't supplied, else it returns when 
// *p_i_stop_on_non_zero is non zero.  If you're running threaded
// this routine simply goes into a sleep loop.

void iluServer::iluRun(int *p_i_stop_on_non_zero) {

	if (iluCppInternal::iluGetRunningThreaded()) {
		while (1) OS_SLEEP(30);
	}

	if (p_i_stop_on_non_zero == NULL) {
		int i_stop_on_non_zero = 0;
		ilu_RunMainLoop(&i_stop_on_non_zero);
	}
	else ilu_RunMainLoop(p_i_stop_on_non_zero);
}


//////////////////////////////////////////////////////////////////
// Defaults

// Get and set the default protocol used when adding a port on a 
// server- initialized to whatever is set to be the default
// in the kernel (found in iluconf.h or iluwin.h)

iluProtocolInfo iluServer::iluGetDefaultProtocol() {
	return sm_pc_default_protocol;
}


void iluServer::iluSetDefaultProtocol(iluProtocolInfo pc_new_default_protocol){

	sm_pc_default_protocol = pc_new_default_protocol;
}


// Get and set the default transports used when adding a port on a 
// server - initialized to whatever is set to be the default
// in the kernel (found in iluconf.h or iluwin.h)
// Callee owns pc_new_default_transport_info.

const iluTransportInfo iluServer::iluGetDefaultTransport() {
	return sm_ppc_default_transport_info;
}

void iluServer::iluSetDefaultTransport(iluTransportInfo ppc_new_default_transport_info) {

	sm_ppc_default_transport_info = ppc_new_default_transport_info;
}


// Returns the default iluServer, creating one if need be.
iluServer& iluServer::iluGetDefaultServer() {

	if (sm_p_default_server)
		return *sm_p_default_server;

	char pc_serverid[1024];

	/* create a new server id */

	char * newid = ilu_InventID();
	sprintf(pc_serverid, "default_server%lu_id_%s", ((unsigned long)sm_card_server_counter++), newid);
	ilu_free(newid);

	sm_p_default_server = new iluServer(pc_serverid);
	
	return *sm_p_default_server;
}


// Sets the default iluServer, returns old default, which is NULL if no default currently is set
iluServer* iluServer::iluSetDefaultServer(iluServer& new_default_server) {
	iluServer* p_old_default_server = sm_p_default_server;
	sm_p_default_server = &new_default_server;
	return p_old_default_server;
}


//////////////////////////////////////////////////////////////////
// Stub and internal use only

// grab and release the mutex that's used to ensure exclusive access
// to the kernel server

void iluServer::iluEnterServerMutex(iluClass the_class) {
	    ilu_EnterServer(m_kernel_server, the_class);
	  ILUCPP_DEBUG2("iluServer::iluEnterServerMutex for server %s\n", ilu_IDOfServer(m_kernel_server)); 
}


void iluServer::iluExitServerMutex(iluClass the_class) {
	    ilu_ExitServer(m_kernel_server, the_class);
	  ILUCPP_DEBUG2("iluServer::iluExitServerMutex for server %s\n", ilu_IDOfServer(m_kernel_server)); 
}


// accessor for the iluServer's kernel server
iluKernelServer iluServer::iluGetKernelServer() {
	return m_kernel_server;
}

// accessor for the iluServer's kernel server id
const char* iluServer::iluGetKernelServerId() {
	return ilu_IDOfServer(m_kernel_server);
}


//////////////////////////////////////////////////////////////////
// End of File
//////////////////////////////////////////////////////////////////




