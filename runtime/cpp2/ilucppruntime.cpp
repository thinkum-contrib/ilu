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
/* $Id: ilucppruntime.cpp,v 1.28 1999/08/03 01:55:51 janssen Exp $ */


// include ILU C++ header file
#include "corba.hpp"

// for isdigit, etc.
#include <ctype.h>


//////////////////////////////////////////////////////////////////
// iluCppRuntime member functions - Abstract class that provides 
// various static member functions that the application can use to 
// control the runtime's behavior.  Not meant to ever be subclassed.


//////////////////////////////////////////////////////////////////
// globals and statics

// ILUCPP_TRUE iff already initialized
ILUCPP_BOOL iluCppRuntime::sm_b_initialized;


//////////////////////////////////////////////////////////////////
// Initialize the runtime for use - Also calls all the functions 
// (typically interface initialization functions) that are on the 
// iluCppInternal::sm_p_initialization_function_list.
// iluCppRuntime::iluInitialize's use depends on your use of threading:
//
// case1: No threading at all - the stubber generated code will call iluInitialize for you.
//
// case2: Using ILU's native operating system (OS) thread support - explicitly call 
//	   iluInitialize with ILUCPP_TRUE before any other ilu related initializations.
//
// case3: Your own thread package - call iluCppRuntime::iluSetForkProcedure, 
//		iluCppRuntime::iluSetNonNativeThreadIDFunction
//      then call the ILU kernel functions ilu_SetWaitTech, and ilu_SetLockTech
//	    appropriately, call iluMainLoop::iluSetMainLoop, 
//	    then call  iluCppRuntime::iluInitialize()

void iluCppRuntime::iluInitialize(ILUCPP_BOOL b_use_native_threads) {
	
	iluError an_error;
	
	if (sm_b_initialized) {
		
		if (iluCppInternal::sm_p_initialization_function_list) {
			// we already initialized, but there some more init functions to 
			// run (perhaps because of a dynamic code load
			// call all the initialization functions
			iluCppInternal::iluCallInitializationFunctions(&iluCppInternal::sm_p_initialization_function_list);
		}
		return;
	}
	
	ILUCPP_DEBUG1("iluCppRuntime::iluInitialize called\n");
	
	// get our language registration index
	iluCppInternal::sm_card_cpp_language_index = ilu_RegisterLanguage(CONST_CAST(char*, "C++"));
	iluCppInternal::iluEnsureGCNoter();

	// take care of threading	
	if (b_use_native_threads) {
		
#ifdef ILU_OS_THREADED	
		if (iluMainLoop::iluGetMainLoop()) 
			// if someone already set the main loop
			_ilu_Assert(ILUCPP_TRUE, "iluCppRuntime::iluInitialize - you have set the MainLoop - \
			you cannot supply your own iluMainLoop when using native operating system threads");
		
		else { // use the default threaded main loop provided internally by ilu
			
			// initialize the kernel to OS threaded operation
			if (!ilu_InitializeOSThreading(&an_error)) {
				ILUCPP_DEBUG1("iluCppRuntime::iluInitialize, ilu_InitializeOSThreading failed\n");
				ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
			}
			ILUCPP_DEBUG1("iluCppRuntime::iluInitialize ilu_InitializeOSThreading succeeded\n");
			
			// set whether we're going to be running threaded or not
			iluCppInternal::sm_b_running_threaded = b_use_native_threads;
		}
#else
		_ilu_Assert(ILUCPP_TRUE, "iluCppRuntime::iluInitialize - OS threading not enabled in this build");
#endif
		
	}
	else {
		// see if we might be in a non-native OS, threaded mode
		// set whether we're going to be running threaded or not - if we have
		// conditions then we assume there's a thread package in place
	  if (ilu_KernelThreaded()) {
	    // someone else (maybe Java or Common Lisp) has already registered a threads package
	    iluCppInternal::sm_b_running_threaded = ILUCPP_TRUE;
	    iluCppInternal::sm_pfunction_forking_procedure = REINTERPRET_CAST(iluForkProc, ilu_Fork);
	  } else {
	    iluCppInternal::sm_b_running_threaded = ILUCPP_FALSE;
	  }
	}
	
	if (iluCppInternal::sm_b_running_threaded) {
		
		// create the object activation table
		iluObjectActivationTable::sm_p_object_activation_table = new iluObjectActivationTable();
		
		// fork off a thread that creates threads to monitor new connections
		iluCppInternal::iluThrowingFork(iluCppInternal::iluCreateMonitorsForNewOutgoingConnections, NULL);
		ILUCPP_DEBUG1("iluCppRuntime::iluInitialize iluCreateMonitorsForNewOutgoingConnections forked\n");
		
		// tell the kernel we did it
		ilu_NewConnectionGetterForked(&an_error);
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	}		
	
	// get a pointer to ilu's object type mutex
	iluCppInternal::sm_object_type_mutex = ilu_GetOTMutex();
	
	// create the mutex for exclusive access to the surrogate creator list
	iluCppInternal::sm_surrogate_creator_list_mutex = ilu_CreateMutex(CONST_CAST(char*, "ilucpp"), CONST_CAST(char*, "surrogate_creator"));

	// create the mutex for exclusive access object reference count operations
	iluObject::sm_reference_count_mutex = ilu_CreateMutex(CONST_CAST(char*, "ilucpp"), CONST_CAST(char*, "reference_count"));

	// set up the passport hash table
	iluPassport::iluInitialize();

	// set the default protocols and transports to use
	iluServer::sm_pc_default_protocol = ilu_DefaultProtocolInfo();
	iluServer::sm_ppc_default_transport_info = ilu_DefaultTransportInfo();
	iluServer::sm_pc_in_memory_protocol_info = ilu_DefaultProtocolInfo();
	iluServer::sm_ppc_in_memory_transport_info = ilu_LocalTransportInfo();

	// call the initialization function for CORBA::Object
	NAME_OUTSIDE_SCOPE(CORBA, Object)::iluInitialize();

	// mark that we're initialized already
	sm_b_initialized = ILUCPP_TRUE;	

	// call any and all initialization functions
	iluCppInternal::iluCallInitializationFunctions(&iluCppInternal::sm_p_initialization_function_list);


}


//////////////////////////////////////////////////////////////////
// adds an init function onto the runtime's list of (typically interface initialization)
// functions to call when iluCppRuntime::iluInitialize is called
void iluCppRuntime::iluAddInitializationFunction(iluPFunctionInitializer pf_initialize) {
	iluCppInternal::iluAddInitializationFunction(
		&(iluCppInternal::sm_p_initialization_function_list),
		pf_initialize);
}


//////////////////////////////////////////////////////////////////
// when running non-native threaded, this should be called (before initialization) set to 
// the function that will return a thread unique iluCardinal id of the current thread

void iluCppRuntime::iluSetNonNativeThreadIDFunction(iluNonNativeThreadIDFunction p_thread_id_function) {
	iluCppInternal::sm_card_thread_id_function = p_thread_id_function;
}

//////////////////////////////////////////////////////////////////
// Tells the runtime what function to call when a surrogate for an object of
// the specified class is needed.  This allows an implementation to subclass
// off a surrogate class, and write a new surrogate creation function that
// creates an instance of this new subclass.  This more specialized surrogate 
// might do message filtering, caching, monitoring, etc. Call this function after you've
// performed initialization, but before you do any operations which might
// create a surrogate of the specified class.  It basically overwrites the
// default surrogate creation function set up by the surrogate stubs. It
// returns the old surrogate creator function, or NULL if was previously no
// surrogate creator for that class (note: NULL return should not really happen 
// unless a mistake or something clever is being done)
iluPFunctionSurrogateCreator iluCppRuntime::iluSetSurrogateCreator(iluClass surrogate_class,
																   iluPFunctionSurrogateCreator pfunction_surrogate_creator ) {

	return iluCppInternal::iluSetSurrogateCreator(surrogate_class, pfunction_surrogate_creator );
}
																 


//////////////////////////////////////////////////////////////////
// If your using your own threads package call this before calling the 
// the ILU kernel functions ilu_SetWaitTech, etc. and pass a pointer to your 
// function that forks a thread.

void iluCppRuntime::iluSetForkProcedure(iluForkProc pfunction_fork_procedure) {
	iluCppInternal::sm_pfunction_forking_procedure = pfunction_fork_procedure;
}


//////////////////////////////////////////////////////////////////
// utility functions for dealing with iluCharacter strings

// returns the length of the iluCharacter string
iluCardinal iluCppRuntime::iluCharacterStringLength(const iluCharacter* p_chars) {
	
	iluCardinal index = 0;
	const iluCharacter* p_walker = p_chars;
	
	while (*p_walker != 0) {
		index++;
		p_walker++;
	}
	return index;
}


// copys the source iluCharacter string to the destination, returns the destination
iluCharacter* iluCppRuntime::iluCharacterStringCopy(iluCharacter* p_chars_destination, const iluCharacter* p_chars_source) {
	
	iluCharacter* p_walker_destination = p_chars_destination;
	const iluCharacter* p_walker_source = p_chars_source;
	
	while (*p_walker_source != 0) {
		*p_walker_destination = *p_walker_source;
		p_walker_source++;
		p_walker_destination++;
	}
	
	*p_walker_destination = 0;
	
	return p_chars_destination;
}


// returns a duplicate of the source iluCharacter string
iluCharacter* iluCppRuntime::iluCharacterStringDuplicate(const iluCharacter* p_chars_source) {
	if (!p_chars_source) 
		return ((iluCharacter*)NULL);
	iluCharacter* p_duplicate = new iluCharacter[iluCharacterStringLength(p_chars_source) + 1];
	iluCharacterStringCopy(p_duplicate, p_chars_source);
	return p_duplicate;
}


// returns true if strings are the same, else false
ILUCPP_BOOL iluCppRuntime::iluCharacterStringEqual(const iluCharacter* p_chars_one, const iluCharacter* p_chars_two) {
	
	const iluCharacter* p_walker_one = p_chars_one;
	const iluCharacter* p_walker_two = p_chars_two;
	
	while (1) {
		if (*p_walker_one != *p_walker_two) return ILUCPP_FALSE;
		if (*p_walker_one == 0) return ILUCPP_TRUE;
		p_walker_one++;
		p_walker_two++;
	}
	
}

// returns a new iluCharacter string filled in from the iluShortCharacter string
iluCharacter* iluCppRuntime::iluCharStringFromShortCharString(const iluShortCharacter* pc_shortchars) {
	iluCharacter* pc_chars = (iluCharacter*)(iluCppRuntime::iluMalloc ((strlen(pc_shortchars) + 1) * sizeof(iluCharacter)));
	iluCharacter* pc_chars_walker = pc_chars;
	while (*pc_shortchars != 0) {
		*pc_chars_walker = *pc_shortchars;
		pc_chars_walker++;
		pc_shortchars++;
	}
	*pc_chars_walker = 0;
	return pc_chars;
}

// returns true if the iluCharacter string matches the iluShortCharacter string
ILUCPP_BOOL iluCppRuntime::iluCharStringShortCharStringEqual(const iluCharacter* pc_chars, const iluShortCharacter* pc_shortchars) {
	while (*pc_shortchars != 0) {
		if (*pc_chars != *pc_shortchars)
			return ILUCPP_FALSE;
		pc_chars++;
		pc_shortchars++;
	}
	if (*pc_chars != 0) 
		return ILUCPP_FALSE;
	return ILUCPP_TRUE;
}




//////////////////////////////////////////////////////////////////
// Use these to form a string binding handle from relevant parts,
// if protocol and/or transport info are NULL, current defaults are used.
// For iluFormSBHUsingContactInfo, p_str_encodedContactInfo is as would
// be obtained from iluParseSBH.


char* iluCppRuntime::iluFormSBH(const char* pc_serverid, const char* pc_instance_handle,
								iluClass the_ilu_class, iluProtocolInfo pc_protocol_type,
								iluTransportInfo transport_info) {
	
	iluError an_error;
	char* pc_return;
	
	pc_return = ilu_FormSBH (CONST_CAST(char*, pc_serverid), CONST_CAST(char*, pc_instance_handle),
		the_ilu_class->cl_unique_id, pc_protocol_type, transport_info, &an_error);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return pc_return;
}



char*  iluCppRuntime::iluFormSBHUsingContactInfo(const char* pc_serverid, const char* pc_instance_handle,
										iluClass the_ilu_class, char* p_str_encodedContactInfo) {

	iluError an_error;
	char* pc_partial;
	char* pc_return;
	char* ppc_empty_transport_info[1] = {NULL};

	// form a partial sbh using null protocol and transport info
	pc_partial = ilu_FormSBH (CONST_CAST(char*, pc_serverid), CONST_CAST(char*, pc_instance_handle),
		the_ilu_class->cl_unique_id, CONST_CAST(char*, ""), ppc_empty_transport_info, &an_error);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);

	int i_partial_length = strlen(pc_partial);

	// overwrite @ since it should already be in p_str_encodedContactInfo
	pc_partial[i_partial_length - 1] = '\0'; 

	// note no + 1 since we overwrite last @ sign
	pc_return = new char [i_partial_length + strlen (p_str_encodedContactInfo)];

	// put in the partial sbh into the value to be returned
	strcpy (pc_return, pc_partial);

	iluFree(pc_partial); // clean up

	// append the supplied contact info
	strcat (pc_return, p_str_encodedContactInfo);

	return pc_return;
}



//////////////////////////////////////////////////////////////////
// Parse a string binding handle, returning whichever elements are
// specified by passing in non-NIL pointers.  Caller retains ownership of URL 
// argument.  If p_str_plainInstanceHandle != NIL, ownership of
// *p_str_plainInstanceHandle is passed to caller iff successful.  Similarly for
// p_str_plainServerID and p_str_plainMstid. *p_str_encodedContactInfo is set to point
// into the given URL (The whole sequence of contact info is returned in 
// *p_str_encodedContactInfo) , and *p_card_encodedContactInfoLen is set to the
// length of the contact info substring; the next character is left unmolested.
// If the p_b_malloced_contact_info out parameter is true, then caller must
// arrange to free it

ILUCPP_BOOL iluCppRuntime::iluParseSBH(iluCString  str_encodedSBH, 
									  iluCString*   p_str_plainInstanceHandle, 
									  iluCString*   p_str_plainServerID,
									  iluCString*   p_str_plainMstid, 
									  iluCString*   p_str_encodedContactInfo, 
									  iluCardinal*  p_card_encodedContactInfoLen,
									  ILUCPP_BOOL*  p_b_malloced_contact_info) {
	iluError an_error;
	ILUCPP_BOOL b_return;
	iluBoolean b_malloced_contact_info;

	b_return = (ilu_ParseSBH(str_encodedSBH, p_str_plainInstanceHandle, p_str_plainServerID,
		p_str_plainMstid, p_str_encodedContactInfo, p_card_encodedContactInfoLen, &b_malloced_contact_info,
		&an_error) ? ILUCPP_TRUE : ILUCPP_FALSE);

	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	if (p_b_malloced_contact_info)
		*p_b_malloced_contact_info = (b_malloced_contact_info ?  ILUCPP_TRUE : ILUCPP_FALSE);
		
	return b_return;
}


//////////////////////////////////////////////////////////////////
// iluWString_var Class

//////////////////////////////////////////////////////////////////
// Constructors and destructor

iluWString_var::iluWString_var(){
	m_pc_wstring = NULL;
	m_b_release = ILUCPP_TRUE;
}


iluWString_var::iluWString_var(iluCharacter* pc_wstring){
	// non const arg is consumed per corba spec
	m_pc_wstring = pc_wstring;
	m_b_release = ILUCPP_TRUE;
}


iluWString_var::iluWString_var(const iluCharacter* pc_wstring) {
	// const arg is copied per corba spec 
	m_pc_wstring = iluCppRuntime::iluCharacterStringDuplicate(pc_wstring);
	m_b_release = ILUCPP_TRUE;
}


iluWString_var::iluWString_var(const iluWString_var& r_wstring_to_copy){
	// String_var arg is copied per corba spec 
	m_pc_wstring = iluCppRuntime::iluCharacterStringDuplicate(r_wstring_to_copy.m_pc_wstring);
	m_b_release = ILUCPP_TRUE;
}


iluWString_var::~iluWString_var() {
	if (m_b_release)
		delete [] (m_pc_wstring);
}


//////////////////////////////////////////////////////////////////
// assignment operations

iluWString_var& iluWString_var::operator=(iluCharacter* pc_wstring){
	if (m_b_release)
		delete [] (m_pc_wstring);
	// arg is consumed per corba spec
	m_pc_wstring = pc_wstring;
	m_b_release = ILUCPP_TRUE;
	return *this;
}

iluWString_var& iluWString_var::operator=(const iluCharacter* pc_wstring){
	if (m_b_release)
		delete [] (m_pc_wstring);
	// const arg is copied per corba spec 
	m_pc_wstring = iluCppRuntime::iluCharacterStringDuplicate(pc_wstring);
	m_b_release = ILUCPP_TRUE;
	return *this;
}

iluWString_var& iluWString_var::operator=(const iluWString_var& r_wstring_to_copy) {
	if (&r_wstring_to_copy == this)
		return *this; // assigning to self
	if (m_b_release)
		delete [] (m_pc_wstring);
	// const arg is copied per corba spec 
	m_pc_wstring = iluCppRuntime::iluCharacterStringDuplicate(r_wstring_to_copy.m_pc_wstring);
	m_b_release = ILUCPP_TRUE;
	return *this;
}


iluWString_var::operator iluCharacter*(){
	return m_pc_wstring;
}

iluWString_var::operator const iluCharacter*() const{
	return m_pc_wstring;
}

iluCharacter& iluWString_var::operator[](iluCardinal card_index){
	return m_pc_wstring[card_index];
}

iluCharacter iluWString_var::operator[](iluCardinal card_index) const{
	return m_pc_wstring[card_index];
}

// following is for use by ILU stubs 
iluCharacter*& iluWString_var::iluStringVarReference() const {
	return ((iluCharacter *&)m_pc_wstring);
}


// accessors on the m_b_release member
void iluWString_var::iluSetRelease(ILUCPP_BOOL b_release_on_destruct) const {
	// cast around constness
	ILUCPP_BOOL* p_release = CONST_CAST(ILUCPP_BOOL*, &m_b_release);
	*p_release = b_release_on_destruct;
}


ILUCPP_BOOL iluWString_var::iluGetRelease() const {
	return m_b_release;
}


//////////////////////////////////////////////////////////////////
// End of File
//////////////////////////////////////////////////////////////////




