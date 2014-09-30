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
/* $Id: ilugarbagecollect.cpp,v 1.13 1999/08/03 01:55:50 janssen Exp $ */


// include ILU C++ header file
#include "ilu.hpp"

//////////////////////////////////////////////////////////////////////
// globals & static members

// holds the ilu class record for iluGarbageCollectorCallback objects
iluClass iluGarbageCollectorCallback::m_ILUClassRecord;

// points to the one and only garbage collector object
iluGarbageCollectorCallback* iluGarbageCollectorCallback::sm_p_the_garbage_collector_object;


//////////////////////////////////////////////////////////////////////
// initialize garbage collection

void iluGarbageCollectorCallback::iluInitialize() {		
	
	ILUCPP_DEBUG1("iluGarbageCollectorCallback::iluInitialize() entered\n");
	
	// assume that the cpp runtime has initialized

	if (ilu_IsGcClientSet())
		// some other call or runtime has already set things up for this process
		return;

	if ( sm_p_the_garbage_collector_object == 0) {  
		// don't yet have a iluGarbageCollectorCallback instance		
		
		// create and set the class record member to be the iluClass for this object type
		m_ILUClassRecord = ilu_GetGcCallbackClass();
		
		// create a server that the callback object will live in
		iluServer* p_server_for_gc_callback = new iluServer(NULL, NULL, NULL, NULL);
		
		ILUCPP_DEBUG1("iluGarbageCollectorCallback::iluInitialize() gc server created\n");
		
		// create the callback object itself
		sm_p_the_garbage_collector_object = new iluGarbageCollectorCallback(CONST_CAST(char*, "GarbageCollectorCallback"), 
			*p_server_for_gc_callback);
		
		// tell the kernel what the callback object is
		ilu_SetGcClient(sm_p_the_garbage_collector_object->iluGetKernelObject());
		
		ILUCPP_DEBUG1("iluGarbageCollectorCallback::iluInitialize() gc callback set\n");
	}
}


//////////////////////////////////////////////////////////////////////
// for use in narrowing

void* iluGarbageCollectorCallback::iluDowncast (iluClass class_to_cast_down_to) {
	if (class_to_cast_down_to == NULL)
		return((void *)((iluObject*) this));
	else if (class_to_cast_down_to == m_ILUClassRecord)
		return ((void*) this);
	else return (NULL);
}


//////////////////////////////////////////////////////////////////////
// Constructs a new iluGarbageCollectorCallback using pc_instance_handle as the instance
// identifier, and puts the object under the specified iluServer.

iluGarbageCollectorCallback::iluGarbageCollectorCallback(char* pc_instance_handle, iluServer& r_an_ilu_server) : 
iluObject(iluGarbageCollectorCallback::m_ILUClassRecord, pc_instance_handle, r_an_ilu_server) {
	
	if (sm_p_the_garbage_collector_object) { // we already have one - uh oh
		iluError an_error;
		
		// Note: is this really the right error to use here?
		ILU_ERR_CONS1(bad_operation, &an_error, minor, ilu_bom_noSuchOperationOnType, ilu_FALSE);
		
		ILU_ERRPRINTF("Error Warning: %s, file %s, line %i, Error name: %s\n",
			"iluGarbageCollectorCallback constructor called more than once" , 
			__FILE__, __LINE__, ILU_ERR_NAME(an_error)); 
		
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	}
	ILUCPP_DEBUG1("iluGarbageCollectorCallback::iluGarbageCollectorCallback()\n");
}


//////////////////////////////////////////////////////////////////////
// default constructor - should never get caled

iluGarbageCollectorCallback::iluGarbageCollectorCallback() {
	
	iluError an_error;
	
	// Note: is this really the right error to use here?
	ILU_ERR_CONS1(bad_operation, &an_error, minor, ilu_bom_noSuchOperationOnType, ilu_FALSE);
	
	ILU_ERRPRINTF("Error Warning: %s, file %s, line %i, Error name: %s\n",
		"iluGarbageCollectorCallback default constructor called" , 
		__FILE__, __LINE__, ILU_ERR_NAME(an_error)); 
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
}


//////////////////////////////////////////////////////////////////////
// destructor - should only be happening on program termination

iluGarbageCollectorCallback::~iluGarbageCollectorCallback() {
	
	// delete our server since we know that we live in our own private server
	delete (iluGetServer());
}



//////////////////////////////////////////////////////////////////
// End of File
//////////////////////////////////////////////////////////////////




