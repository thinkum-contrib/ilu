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
/* $Id: iluobjecttable.cpp,v 1.12 1999/08/03 01:55:48 janssen Exp $ */


// include ILU C++ header file
#include "ilu.hpp"


//////////////////////////////////////////////////////////////////
// iluObjectTable - An C++ class for developers to derive from to
// provide Object Tables - never directly instantiate this class.



//////////////////////////////////////////////////////////////////
// constructor

// used to get around some compilers complaining about passing a c++
// function where a c function is expected.
extern "C" {
  typedef iluKernelObject (*C_iluDispatchObjectOfInstanceHandle)(iluKernelObjectTable,
		iluCString);

 typedef void (*C_iluDispatchDestructor)(iluKernelObjectTable);
}



iluObjectTable::iluObjectTable() {

	ILU_ERRS((no_memory)) an_error;

	// create a new kernel object table
	m_kernel_object_table_dispatcher = (iluKernelObjectTable) ilu_MallocE(sizeof(ilu_ObjectTable_s), &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);

	// set up the kernel table to call the static member functions which will 
	// invoke virtually the instance's methods
	m_kernel_object_table_dispatcher->ot_object_of_ih = 
	  REINTERPRET_CAST(C_iluDispatchObjectOfInstanceHandle, iluDispatchObjectOfInstanceHandle);
	m_kernel_object_table_dispatcher->ot_free_self = 
	  REINTERPRET_CAST(C_iluDispatchDestructor, iluDispatchDestructor);
	m_kernel_object_table_dispatcher->ot_rock = this;

	// when an iluObjectTable is passed to the constructor for an iluServer, the
	// iluServer constructor sets this member to itself so that iluObjectOfInstanceHandle
	// can know what server to create the object in.
	m_p_server = NULL;
}


//////////////////////////////////////////////////////////////////
// destructor
// Do whatever destroying the Object Table needs to do to free up resources, etc.
// It gets called when the ilu server it's associated with it is shut down.

iluObjectTable::~iluObjectTable() {
	// free the kernel object table storage
	ilu_free(m_kernel_object_table_dispatcher);
}



// functions to dispatch the opertion to the actual iluObjectTable instance
iluKernelObject iluObjectTable::iluDispatchObjectOfInstanceHandle(iluKernelObjectTable pv_iluKernelObjectTable_instance,
																  iluCString pc_instance_handle) {
	// get a hold of the C++ object
	iluObject* p_the_object = ((iluObjectTable*)(pv_iluKernelObjectTable_instance->ot_rock))->iluObjectOfInstanceHandle(pc_instance_handle);

	// return the iluObject's kernel object
	return (p_the_object ? p_the_object->iluGetKernelObject() : NULL);
}


void iluObjectTable::iluDispatchDestructor(iluKernelObjectTable pv_iluKernelObjectTable_instance) {
	// call delete on the iluObjectTable
	delete ((iluObjectTable*)(pv_iluKernelObjectTable_instance->ot_rock));
}


//////////////////////////////////////////////////////////////////
// End of File
//////////////////////////////////////////////////////////////////




