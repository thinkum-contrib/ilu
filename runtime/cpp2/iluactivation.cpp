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
/* $Id: iluactivation.cpp,v 1.18 1999/08/03 01:55:53 janssen Exp $ */


// include ILU C++ header file
#include "ilu.hpp"
#include "iluhash.h"


// Note that none of this activation code is used when we are not running threaded,
// so all of the implementations in here operate under the assumption that we're
// threaded.


//////////////////////////////////////////////////////////////////////
// globals & static members

// the one and only object activation table
// Note: erhaps it would be better to have one per iluServer ?
iluObjectActivationTable* iluObjectActivationTable::sm_p_object_activation_table;


//////////////////////////////////////////////////////////////////////
// Extraction operators for object wrappers from ILUCPP_TRUECalls 
// should call IncrementInvocationCount for every object coming in.  
// Insertion operators for object wrappers from iluSurrogateCalls 
// should call IncrementInvocationCount for every object going out.  
// Object wrapper destructors call DecrementInvocationCount if 
// IncrementInvocationCount had been performed on them - this ensures
// that the decrement takes place automaticaly when control leaves the stub.



//////////////////////////////////////////////////////////////////////
// iluActivationInfo is the information associated with kernel objects
// in a iluObjectActivationTable


//////////////////////////////////////////////////////////////////////
// constructor

iluActivationInfo::iluActivationInfo () {
	
	iluError an_error;
	
	// create our condition
	m_activation_condition = ilu_CreateCondition(NULL, NULL, &an_error);
	
	// don't bother to check an_error since the only one that can be returned is
	// if we don't have conditions, which we do since we are running threaded
	
	// whether or not the object is currently active - initially ILUCPP_TRUE
	m_b_active = ILUCPP_TRUE;
	
	// how many ilu method calls are currently operating on this object
	// this is either an incoming ilu methods on a true object, or outgoing
	// ilu method calls on a surrogate - initially 1
	m_card_number_ongoing_invocations = 1;
	
	ILUCPP_DEBUG1("iluActivationInfo::iluActivationInfo() constructed\n");
}


//////////////////////////////////////////////////////////////////////
// destructor

iluActivationInfo::~iluActivationInfo (){
	
	ilu_DestroyCondition(m_activation_condition);
	
	ILUCPP_DEBUG1("iluActivationInfo::~iluActivationInfo() destructed\n");
	
}


//////////////////////////////////////////////////////////////////////
// decrement the number of invocations and signal the condition if zero

void iluActivationInfo::iluDecrementInvocations() { 
	
	if (m_card_number_ongoing_invocations > 0) {  
		m_card_number_ongoing_invocations-- ;
		
		if (m_card_number_ongoing_invocations == 0) {
			ILUCPP_DEBUG2("iluActivationInfo::iluDecrementInvocations() notifying condition %p\n", m_activation_condition);
			
			ilu_NotifyCondition(m_activation_condition);
		}
	}
}


//////////////////////////////////////////////////////////////////////
// used to get around some compilers complaining about passing a c++
// function where a c function is expected.

extern "C" {typedef void(*C_Deleter_proc)(ilu_refany);}



//////////////////////////////////////////////////////////////////////
// returns when the number of invocations is zero

void iluActivationInfo::iluWaitForZeroInvocations(iluMutex a_mutex) { 

	iluError an_error;
	while (m_card_number_ongoing_invocations > 0) {
		ILUCPP_DEBUG2("iluActivationInfo::iluWaitForZeroInvocations() waiting on condition %p\n", m_activation_condition);
		ilu_CMWait1(m_activation_condition, a_mutex, &an_error);
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	}
}


//////////////////////////////////////////////////////////////////////
// iluObjectActivationTable associates kernel objects with iluActivationInfo
// objects.  It's consulted to determine whether an object is active,
// inactive, or how many calls it's currently involved in (in this language runtime)



//////////////////////////////////////////////////////////////////
// constructor

iluObjectActivationTable::iluObjectActivationTable() {
	
	if (sm_p_object_activation_table) {
		ILU_ERRPRINTF("Warning: %s, file %s, line %i", 
			"iluObjectActivationTable::iluObjectActivationTable() - overwriting existing table!", 
			__FILE__, __LINE__);
		delete sm_p_object_activation_table;
	}
	
	sm_p_object_activation_table = this;
	
	// make up our hash table Note: 1021 is Prime, but is it realy the best choice here?
	m_activation_hash_table = 
		ilu_hash_MakeNewTable(1021, ilu_hash_HashPointer, ilu_hash_PointerCompare);
	
	// make the mutex used to control access to the table
	m_activation_table_mutex = ilu_CreateMutex(CONST_CAST(char*, "ilucpp"), CONST_CAST(char*, "activation_table"));
	
	ILUCPP_DEBUG1("iluObjectActivationTable::iluObjectActivationTable() constructed\n");
	
}



//////////////////////////////////////////////////////////////////
// destructor

iluObjectActivationTable::~iluObjectActivationTable() {
	
	// free our has table storage
  ilu_hash_FreeHashTable (m_activation_hash_table, 0,  
			  REINTERPRET_CAST(C_Deleter_proc, iluActivationInfo::iluDeleteActivationInfo));
	
	// if we were the global activation table, NULL it out
	if (sm_p_object_activation_table == this)
		sm_p_object_activation_table = NULL;	
	
	// Note: this call doesn't exist yet! - but it's not a big deal since
	// we only create one activation table
	// ilu_DestroyMutex(m_activation_table_mutex);
	
	ILUCPP_DEBUG1("iluObjectActivationTable::~iluObjectActivationTable() destructed\n");
	
}


//////////////////////////////////////////////////////////////////
// If the object is not in the table, it make a new active entry for it.
// Marks the object active 

void iluObjectActivationTable::iluActivate(iluKernelObject a_kernel_object) {
	
	if (a_kernel_object == NULL)	// ignore null objects
		return;

	// grab the mutex - released on function exit when destructor runs
	iluMutexer table_mutex(m_activation_table_mutex);
	iluActivationInfo* p_activation_info;
	
	// get the activation information related to the kernel object
	p_activation_info = (iluActivationInfo*) ilu_hash_FindInTable(m_activation_hash_table, a_kernel_object);
	
	if (!p_activation_info) {
		// not found so add an entry
		p_activation_info = new iluActivationInfo();
		ilu_hash_AddToTable(m_activation_hash_table, a_kernel_object, p_activation_info);
		ILUCPP_DEBUG2("iluObjectActivationTable::iluActivate() added activation info for kernel object %p\n", a_kernel_object);
		return ;
	}
	
	// force the activity indicator true
	p_activation_info->iluSetActive(ILUCPP_TRUE);
	
	ILUCPP_DEBUG2("iluObjectActivationTable::iluActivate() activated kernel object %p\n", a_kernel_object);
	
}


//////////////////////////////////////////////////////////////////
// waits till the invocations count on the object reaches zero and then
// marks the object inactive - returns ILUCPP_FALSE if had to be added to 
//  in the table, else ILUCPP_TRUE

ILUCPP_BOOL iluObjectActivationTable::iluDeactivate(iluKernelObject a_kernel_object) {

	if (a_kernel_object == NULL)	// ignore null objects
		return ILUCPP_TRUE;

	iluError an_error;
	ILUCPP_BOOL b_in_table = ILUCPP_TRUE;
	ILU_CLER(an_error);
	
	// get exclusive access
	ilu_EnterMutex(m_activation_table_mutex, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);	
	iluActivationInfo* p_activation_info;
	
	// get the activation information related to the kernel object
	p_activation_info = (iluActivationInfo*) ilu_hash_FindInTable(m_activation_hash_table, a_kernel_object);
	
	if (!p_activation_info) {
		// not found so add an entry
		p_activation_info = new iluActivationInfo();
		ilu_hash_AddToTable(m_activation_hash_table, a_kernel_object, p_activation_info);
		ILUCPP_DEBUG2("iluObjectActivationTable::iluDectivate() added activation info for kernel object %p\n", a_kernel_object);
		b_in_table = ILUCPP_FALSE;
	}
	
	// force the activity indicator false
	p_activation_info->iluSetActive(ILUCPP_FALSE);
	
	// wait till there's zero invocations
	p_activation_info->iluWaitForZeroInvocations(m_activation_table_mutex); 
	
	// release exclusive access
	ilu_ExitMutex(m_activation_table_mutex, iluFALSE, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);		
	
	ILUCPP_DEBUG2("iluObjectActivationTable::iluDeactivate() deactivated kernel object %p\n", a_kernel_object);
	
	return b_in_table;
}



//////////////////////////////////////////////////////////////////////
// If the object is not in the table, it make a new active entry for it.
// If the object is active, increments the invocations count of the object and
// returns ILUCPP_TRUE, else returns ILUCPP_FALSE;

ILUCPP_BOOL iluObjectActivationTable::iluIncrementInvocations(iluKernelObject a_kernel_object) {
	
	if (a_kernel_object == NULL)	// ignore null objects
		return ILUCPP_TRUE;

	// grab the mutex - released on function exit when destructor runs
	iluMutexer table_mutex(m_activation_table_mutex);
	iluActivationInfo* p_activation_info;
	
	// get the activation information related to the kernel object
	p_activation_info = (iluActivationInfo*) ilu_hash_FindInTable(m_activation_hash_table, a_kernel_object);
	
	ILUCPP_DEBUG2("iluObjectActivationTable::iluIncrementInvocations() on kernel object %p\n", a_kernel_object);
	
	if (!p_activation_info) {
		// not found so add an entry
		p_activation_info = new iluActivationInfo();
		ilu_hash_AddToTable(m_activation_hash_table, a_kernel_object, p_activation_info);
		return ILUCPP_TRUE;
	}
	
	// if it's active, bump its count up
	if (p_activation_info->iluGetActive()) {
		p_activation_info->iluIncrementInvocations();
		return ILUCPP_TRUE;
	}
	
	return ILUCPP_FALSE;
}




//////////////////////////////////////////////////////////////////////
// If the object is in the table, decrements the invocations count 
// of the object and returns ILUCPP_TRUE, else returns ILUCPP_FALSE.  If the count reaches,
// zero any threads waiting on the condition in the iluActivationInfo associated 
// with a_kernel_object (i.e. a call to iluDeactivate) are awakened.

ILUCPP_BOOL iluObjectActivationTable::iluDecrementInvocations(iluKernelObject a_kernel_object) {

	if (a_kernel_object == NULL)	// ignore null objects
		return ILUCPP_TRUE;

	// grab the mutex - released on function exit when destructor runs
	iluMutexer table_mutex(m_activation_table_mutex);
	iluActivationInfo* p_activation_info;
	
	// get the activation information related to the kernel object
	p_activation_info = (iluActivationInfo*) ilu_hash_FindInTable(m_activation_hash_table, a_kernel_object);
	
	if (!p_activation_info)		// not found so return
		return ILUCPP_FALSE;
	
	// bump its count down
	p_activation_info->iluDecrementInvocations();
	
	ILUCPP_DEBUG2("iluObjectActivationTable::iluDecrementInvocations() on kernel object %p\n", a_kernel_object);
	
	return ILUCPP_TRUE;
}




//////////////////////////////////////////////////////////////////////
// removes the entry for a_kernel_object from the table - returns
// ILUCPP_FALSE if a_kernel_object wasn't in the table, else ILUCPP_TRUE;

ILUCPP_BOOL iluObjectActivationTable::iluRemove(iluKernelObject a_kernel_object) {
	
	if (a_kernel_object == NULL)	// ignore null objects
		return ILUCPP_TRUE;

	// grab the mutex - released on function exit when destructor runs
	iluMutexer table_mutex(m_activation_table_mutex);
	iluActivationInfo* p_activation_info;
	
	// get the activation information related to the kernel object
	p_activation_info = (iluActivationInfo*) ilu_hash_RemoveFromTable(m_activation_hash_table, a_kernel_object);
	
	if (!p_activation_info) 
		return ILUCPP_FALSE;
	
	ILUCPP_DEBUG2("iluObjectActivationTable::iluRemove() on kernel object %p\n", a_kernel_object);
	
	// free it
	delete p_activation_info;
	
	return ILUCPP_TRUE;
}





//////////////////////////////////////////////////////////////////
// End of File
//////////////////////////////////////////////////////////////////




