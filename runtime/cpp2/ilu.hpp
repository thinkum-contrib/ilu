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
/* $Id: ilu.hpp,v 1.83 1999/09/14 19:21:26 janssen Exp $ */


// Any function or type which is not part of the CORBA 2.0 specification has
// the prefix 'ilu'.  It should be understood that use of 'ilu' prefixed
// functionality is not portable to other (non-ILU) CORBA implementations

// Some arguments or return values of functions (e.g. char*) have storage
// management requirements.  Basically this revolves around whether the caller
// retains or gets ownership of the parameter and is therefore responsible for
// eventually releasing it, or if ILU takes or retains ownership, where it
// will be released at ILU's discretion.  Any function parameter that becomes
// owned by ILU is marked with the comment /* ILUowned */ Any return value (or
// 'out' parameter) that remains under the ownership of ILU is similarly
// marked.  Anything not so marked is not ILU's responsibility.


//////////////////////////////////////////////////////////////////
// Preprocessing checks and inclusions

// prevent multiple inclusions 
#ifndef __ilucpp_H_
#define __ilucpp_H_

// error out if we're not being processed by a c++ compiler
#ifndef __cplusplus
#error "ilu.hpp is a C++ header file"
#endif

#ifdef _MSC_VER
// For Visual C++ turn off warning about C++ Exception Specification ignored
#pragma warning( disable : 4290 )
#endif

#include <stdlib.h>
#include <string.h>

#ifdef WIN32
#include <strstrea.h>
#else
#include <strstream.h>
#endif

extern "C" {
#include <iluxport.h>
#include <iludebug.h>
}

#include <cppportability.hpp>

//////////////////////////////////////////////////////////////////
// define appropriate import/export macros depending on whether
// we're building the kernel (export) or an app (import)

#if defined(WIN32)

#if defined(ILU_BUILDING_RUNTIME)
// we're actually building the runtime, so declare things as exported

#if (!(defined(CPLUSPLUSMAPPING_NESTEDCLASSES) || defined(CPLUSPLUSMAPPING_NAMESPACES)))
// must be using underscores
#define ILU_RUNTIME_PUBLIC        __declspec(dllexport) extern
#else
#define ILU_RUNTIME_PUBLIC        __declspec(dllexport) 
#endif

#define ILU_RUNTIME_PUBLIC_CLASS  class __declspec(dllexport)

#else

// we're must be building an app, so declare things as imported
#if (!(defined(CPLUSPLUSMAPPING_NESTEDCLASSES) || defined(CPLUSPLUSMAPPING_NAMESPACES)))
// must be using underscores
#define ILU_RUNTIME_PUBLIC       __declspec(dllimport) extern
#else
#define ILU_RUNTIME_PUBLIC       __declspec(dllimport) 
#endif

#define ILU_RUNTIME_PUBLIC_CLASS class __declspec(dllimport)
#endif /* defined(ILU_BUILDING_RUNTIME) */

#else
// we're not on win32
#define ILU_RUNTIME_PUBLIC extern
#define ILU_RUNTIME_PUBLIC_CLASS class

#endif /* defined(WIN32) */


//////////////////////////////////////////////////////////////////
// some development - debugging support

#define ILUCPP_FORMAT_STRING_SIZE 2048

ILU_RUNTIME_PUBLIC const char* iluCppMinorErrorDescription (ilu_ErrorType et, unsigned long minor);

#ifdef ENABLE_DEBUGGING

ILU_RUNTIME_PUBLIC char* iluCppTagFormatString (char* pc_destination, const char* pc_source);

#define ILUCPP_DEBUG1(formatstring) if (ilu_DebugLevel & LSR_DEBUG) { \
	char c_new_format_string[ILUCPP_FORMAT_STRING_SIZE]; \
	ilu_DebugPrintf(iluCppTagFormatString(c_new_format_string, formatstring));}

#define ILUCPP_DEBUG2(formatstring, arg1) if (ilu_DebugLevel & LSR_DEBUG) { \
	char c_new_format_string[ILUCPP_FORMAT_STRING_SIZE]; \
	ilu_DebugPrintf(iluCppTagFormatString(c_new_format_string, formatstring), arg1);}

#define ILUCPP_DEBUG3(formatstring, arg1, arg2) if (ilu_DebugLevel & LSR_DEBUG) { \
	char c_new_format_string[ILUCPP_FORMAT_STRING_SIZE]; \
	ilu_DebugPrintf(iluCppTagFormatString(c_new_format_string, formatstring), arg1, arg2);}

#define ILUCPP_DEBUG4(formatstring, arg1, arg2, arg3) if (ilu_DebugLevel & LSR_DEBUG) { \
	char c_new_format_string[ILUCPP_FORMAT_STRING_SIZE]; \
	ilu_DebugPrintf(iluCppTagFormatString(c_new_format_string, formatstring), arg1, arg2, arg3);}

#define ILUCPP_DEBUG5(formatstring, arg1, arg2, arg3, arg4) if (ilu_DebugLevel & LSR_DEBUG) { \
	char c_new_format_string[ILUCPP_FORMAT_STRING_SIZE]; \
	ilu_DebugPrintf(iluCppTagFormatString(c_new_format_string, formatstring), arg1, arg2, arg3, arg4);}

#else
#define ILUCPP_DEBUG1()
#define ILUCPP_DEBUG2()
#define ILUCPP_DEBUG3()
#define ILUCPP_DEBUG4()
#define ILUCPP_DEBUG5()

#endif


#define ILUCPP_NYI() { \
	ilu_DebugPrintf("Not Yet Implemented in %s, line %i\n", __FILE__,  __LINE__); \
*((int*)0) = 0;}


//////////////////////////////////////////////////////////////////
// typedefs to provice C++ view of basic types, etc. - i.e. shield app
// from all kernel concepts

typedef ilu_integer			iluInteger;
typedef ilu_shortinteger	iluShortInteger;
typedef ilu_longinteger		iluLongInteger;

typedef ilu_cardinal		iluCardinal;
typedef ilu_shortcardinal	iluShortCardinal;
typedef ilu_longcardinal	iluLongCardinal;

typedef	ilu_byte			iluByte;

typedef ilu_boolean			iluBoolean;
#define iluTRUE				ilu_TRUE
#define iluFALSE			ilu_FALSE

typedef ilu_real			iluReal;
typedef ilu_shortreal		iluShortReal;
typedef ilu_longreal		iluLongReal;

typedef ilu_character		iluCharacter;
typedef ilu_shortcharacter	iluShortCharacter;

typedef ilu_string			iluCString;
typedef ilu_wstring			iluWString;


typedef ilu_Call				iluCall;
typedef ilu_Class				iluClass;
typedef void*					iluAlarm;
typedef ilu_TransportInfo		iluTransportInfo;
typedef ilu_ProtocolInfo		iluProtocolInfo;
typedef ilu_Exception			iluException;
typedef ilu_Call_s				iluCallStruct;
typedef ilu_FineTime			iluFineTime;
typedef ilu_Mutex				iluMutex;
typedef ilu_Condition			iluCondition;
typedef ilu_Error				iluError;
typedef ilu_Port				iluPort;
typedef ilu_Connection			iluConnection;
typedef ilu_Method				iluMethod;
typedef ilu_ProtocolException	iluProtocolException;
typedef ilu_ErrorType			iluErrorType;
typedef ilu_Type				iluType;
typedef ilu_ConstantValue_s		iluConstantValue_s;
typedef ilu_UnionArm			iluUnionArm;
typedef ilu_ArgDirection		iluArgDirection;
typedef ilu_IdentityInfo		iluIdentityInfo;
typedef ilu_IdentityType		iluIdentityType;
typedef ilu_TypeUID				iluTypeUID;
typedef ilu_ConstantValue		iluConstantValue;
typedef ilu_TypeKind			iluTypeKind;
typedef ilu_HashTable			iluHashTable;

#ifdef ADD_VARIANT_SUPPORT
typedef ilu_Pickle				iluPickle;
#endif

typedef iluBoolean (*iluForkProc)(void (*pfunction_procedure) (void* pv_argument),
							   void* pv_argument,
							   ILU_ERRS((no_memory, no_resources, internal)) * p_error);


// when running non-native threaded, this describes a function  that will return a thread unique iluCardinal
// id of the current thread
typedef iluCardinal (*iluNonNativeThreadIDFunction)();


//////////////////////////////////////////////////////////////////
// typedefs to distinguish similarly named concepts in the kernel
// from their usage in the C++ runtime

typedef ilu_Object iluKernelObject;
typedef ilu_Server iluKernelServer;
typedef ilu_ObjectTable iluKernelObjectTable;


//////////////////////////////////////////////////////////////////
// Exception related defines

// used to throw a CORBA exception if the ilu error an_error is not ok
#define ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error) \
if ILU_ERRNOK(an_error) iluCppInternal::iluThrowExceptionFromError (&an_error, __FILE__, __LINE__);


// warns of an error and marks it handled if b_clean is set
#define ILUCPP_ERRWARN(an_error, message, b_clean)  \
if (ILU_ERRNOK(an_error)) { \
	char c_new_format_string[ILUCPP_FORMAT_STRING_SIZE]; \
	iluInteger i_major; unsigned long l_minor; \
	const char *pc_filename; \
	int i_linenum; \
	l_minor = ilu_FullCORBAizeSystemErr(&an_error, &i_major, &pc_filename, &i_linenum); \
	ILU_ERRPRINTF(iluCppTagFormatString(c_new_format_string, \
					    "Error Warning: %s, file %s, line %i, Error name: %s (%s) at %s:%i\n"), \
		      message, __FILE__, __LINE__, ILU_ERR_NAME(an_error), \
		      ilu_DescribeCorbaMinor(i_major, l_minor), pc_filename, i_linenum); \
	if (b_clean) { ILU_HANDLED(an_error); \
				   ILU_CLER(an_error); }\
}

// warns of a problem
#define ILUCPP_WARN(message) { \
	char c_new_format_string[ILUCPP_FORMAT_STRING_SIZE]; \
	ILU_ERRPRINTF(iluCppTagFormatString(c_new_format_string, "Error Warning: %s, file %s, line %i\n"), \
	message, __FILE__, __LINE__); \
}

// warns of an unexpected exception problem
#define ILUCPP_WARN_UNEXPECTED_EXCEPTION(function_name) { \
	char c_new_format_string[ILUCPP_FORMAT_STRING_SIZE]; \
	ILU_ERRPRINTF(iluCppTagFormatString(c_new_format_string, "Warning Caught Unexpected Exception in: %s, file %s, line %i\n"), \
	function_name, __FILE__, __LINE__); \
}

// cons up and throw an error indicating that you shouldn't be doing this
// - note : is this this appropriate error? 
#define ILUCPP_BAD_OPERATION() { \
	iluError an_error; \
	ILU_ERR_CONS1(bad_operation, &an_error, minor, ilu_bom_noSuchOperationOnType, ilu_FALSE); \
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error); \
}

#define ILUCPP_BAD_DISC_SET() { \
	iluError an_error; \
	ILU_ERR_CONS1(bad_param, &an_error, minor, ilu_bpm_unionDiscValue, ilu_FALSE); \
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error); \
}

#define ILUCPP_BAD_DISC() { \
	iluError an_error; \
	ILU_ERR_CONS1(marshal, &an_error, minor,ilu_mm_bad_union_disc, ilu_FALSE); \
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error); \
}


#define ILUCPP_BAD_ASSIGNMENT() ILUCPP_BAD_OPERATION(); return *this;

#define ILUCPP_ASSIGN_ERROR(error_type, an_error) { \
		if ((error_type) != ILU_ERRTYP(success)) \
		  _ilu_NoteRaise((error_type), __FILE__, __LINE__);\
		an_error.ilu_type = (error_type); \
}



//////////////////////////////////////////////////////////////////
// Classes
//
// iluInitializationFunctionNode - Node in a list of initialization functions
//
// iluCppRuntime - Abstract class that provides various static member functions
// that the application can use to control the runtime's behavior
// 
// iluObjectTable - An abstract C++ class for developers to derive from to
// provide Object Tables
//
// iluMainLoop - An abstract base class for developers to derive from to
// create their own main loop
//
// iluServer - provides a the C++ view of a kernel server object
// 
// iluObject - The most base class for all ILU C++ objects.  CORBA::Object
// inherits from this class.   All non-static member functions are virtual
// to allow creative overrides (at your own risk of course). 
//
// iluPassport - encapsulates ilu_Passport functionality
// 
// iluGSS - encapsulates GSS functionality
//
// iluWString_var Class - analog to CORBA(String_var) only for
// ILU Characters (16 bit)
//
//
// Classes NOT meant for use by the application developer
//
// The iluGarbageCollectorCallback class is meant to have nor more than
// a single instance of itself created.  This instance is what server's
// ping on in a client to determine whether or not the client is still
// alive.
//	
// iluMutexer is a helper class used to ensure that mutexes are released when
// functions are exited.  The constructor grabs the mutex - the destructor 
// releases it
//
// iluCppInternal - Abstract class that provides various calls that are specificly 
// meant only for use by the C++ runtime or stubs.
//
// iluBaseCall iluTrueCall and iluSurrogateCall - classes meant for use 
// by true stubs and surrogates only, that encapsulate some of the standard 
// control operations for performing ILU calls
//
// iluPickleCall is meant for use by Corba any insertion and extraction
// only
//
//////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////
// forward decls of classes, and typedefs

ILU_RUNTIME_PUBLIC_CLASS ilu_var;
ILU_RUNTIME_PUBLIC_CLASS iluInitializationFunctionNode;
ILU_RUNTIME_PUBLIC_CLASS iluSurrogateCreatorNode;
ILU_RUNTIME_PUBLIC_CLASS iluCppRuntime;
ILU_RUNTIME_PUBLIC_CLASS iluCppInternal;
ILU_RUNTIME_PUBLIC_CLASS iluObjectTable;
ILU_RUNTIME_PUBLIC_CLASS iluMainLoop;
ILU_RUNTIME_PUBLIC_CLASS iluServer;
ILU_RUNTIME_PUBLIC_CLASS iluObject;
ILU_RUNTIME_PUBLIC_CLASS iluPassport;
class iluGarbageCollectorCallback;
ILU_RUNTIME_PUBLIC_CLASS iluMutexer;
ILU_RUNTIME_PUBLIC_CLASS iluBaseCall;
ILU_RUNTIME_PUBLIC_CLASS iluTrueCall;
ILU_RUNTIME_PUBLIC_CLASS iluSurrogateCall;
ILU_RUNTIME_PUBLIC_CLASS iluPickleCall;

// an iluObject_ptr is simply a pointer to a iluObject
typedef iluObject*    iluObject_ptr;

// a pointer to an initializer function
typedef void (*iluPFunctionInitializer)();


// A pointer to an surrogate creator function  A surrogate creator function
// should at the minimum create an instance of a surrogate, call the instances member 
// function iluAssociateKernelObject passing the iluKernelObject, and then 
// return a pointer to the new instance. 
typedef iluObject* (*iluPFunctionSurrogateCreator)(iluKernelObject);


// used to ensure safe passage of enumerations
enum iluDummyEnum {iluDummyEnumValue};


//////////////////////////////////////////////////////////////////
// Just a future placeholder for now, iluTemplatableObject_var
// inherits from ilu_var

ILU_RUNTIME_PUBLIC_CLASS ilu_var {
};


//////////////////////////////////////////////////////////////////
// iluInitializationFunctionNode - Node in a list of initialization functions

ILU_RUNTIME_PUBLIC_CLASS iluInitializationFunctionNode {
	
public:
	
	// constructor
	iluInitializationFunctionNode(iluPFunctionInitializer pf_initializer, 
		iluInitializationFunctionNode* p_next_node) :
	m_pf_initializer(pf_initializer), m_p_next_node(p_next_node) {}
	
	// run the function member
	void iluRunInitializer() {(m_pf_initializer)();}
				
	// members
	iluPFunctionInitializer m_pf_initializer;
	iluInitializationFunctionNode* m_p_next_node;
				
				
}; // end class iluInitializationFunctionNode


//////////////////////////////////////////////////////////////////
// iluSurrogateCreatorNode - Node in a list of surrogate creator functions

ILU_RUNTIME_PUBLIC_CLASS iluSurrogateCreatorNode {
	
public:
	
	// constructor
	iluSurrogateCreatorNode(iluClass the_class, iluPFunctionSurrogateCreator pf_surrogate_creator, 
		iluSurrogateCreatorNode* p_next_node) : 
		m_class(the_class),
		m_pf_surrogate_creator(pf_surrogate_creator), 
		m_p_next_node(p_next_node) {}
	
	// run the function member
	iluObject* iluRunSurrogateCreatorIfClass(iluClass the_class, iluKernelObject kernel_object) {
	  ILUCPP_DEBUG2("iluSurrogateCreatorNode::iluRunSurrogateCreatorIfClass checking %s\n", the_class->cl_name); 
	  return ((the_class == m_class) ? ((m_pf_surrogate_creator)(kernel_object)) : ((iluObject*) NULL));
	}
				
	// members
	iluClass m_class;		// class of ilu object this creator function creates
	iluPFunctionSurrogateCreator m_pf_surrogate_creator;
	iluSurrogateCreatorNode* m_p_next_node;
				
				
}; // end class iluSurrogateCreatorNode



//////////////////////////////////////////////////////////////////
// iluCppRuntime - Abstract class that provides various static member functions
// that the application can use to control the runtime's behavior.  Not
// meant to ever be subclassed.

ILU_RUNTIME_PUBLIC_CLASS iluCppRuntime {

public:

	//////////////////////////////////////////////////////////////////
	// Initialize the runtime for use - Also calls all the functions 
	// (typically interface initialization functions) that are on the 
	// iluCppInternal::sm_p_initialization_function_list.
	// iluCppRuntime::iluInitialize's use depends on your use of threading:
	//
	// case1: No threading at all - just call iluCppRuntime::iluInitialize();
	//
	// case2: Using ILU's native operating system (OS) thread support - call 
	//	   iluCppRuntime::iluInitialize(ILUCPP_TRUE)
	//
	// case3: Your own thread package - call iluCppRuntime::iluSetForkProcedure, 
	//		iluCppRuntime::iluSetNonNativeThreadIDFunction
	//      then call the ILU kernel functions ilu_SetWaitTech, and ilu_SetLockTech
	//	    appropriately, call iluMainLoop::iluSetMainLoop, 
	//	    then call  iluCppRuntime::iluInitialize()
	static void iluInitialize(ILUCPP_BOOL b_use_native_threads = ILUCPP_FALSE);

	// adds an init function onto the runtime's list of (typically interface initialization)
	// functions to call when iluCppRuntime::iluInitialize is called
	static void iluAddInitializationFunction(iluPFunctionInitializer pf_initialize);

	// when running non-native threaded, this should be called (before initialization) set to 
	// the function that will return a thread unique iluCardinal id of the current thread
	static void iluSetNonNativeThreadIDFunction(iluNonNativeThreadIDFunction p_thread_id_function);

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
	// unless a mistake or something clever is being done - this means you've
	// added a new node to the surrogate creator function list)
	static iluPFunctionSurrogateCreator iluSetSurrogateCreator(iluClass surrogate_class,
		iluPFunctionSurrogateCreator pfunction_surrogate_creator );

	// If your using your own threads package call this before calling the 
	// the ILU kernel functions ilu_SetWaitTech, etc. and pass a pointer to your 
	// function that forks a thread.
	static void iluSetForkProcedure(iluForkProc pfunction_fork_procedure);


	//////////////////////////////////////////////////////////////////
	// iluCharacter functions
	
	// returns the length of the iluCharacter string
	static iluCardinal iluCharacterStringLength(const iluCharacter* p_chars);
	
	// copys the source iluCharacter string to the destination, returns the destination
	static iluCharacter* iluCharacterStringCopy(iluCharacter* p_chars_destination, const iluCharacter* p_chars_source);

	// returns a duplicate of the source iluCharacter string
	static iluCharacter* iluCharacterStringDuplicate(const iluCharacter* p_chars_source);

	// returns true if strings are the same, else false
	static ILUCPP_BOOL iluCharacterStringEqual(const iluCharacter* p_chars_one, const iluCharacter* p_chars_two);
	
	// returns a new iluCharacter string filled in from the iluShortCharacter string
	static iluCharacter* iluCharStringFromShortCharString(const iluShortCharacter* pc_shortchars);

	// returns true if the iluCharacter string matches the iluShortCharacter string
	static ILUCPP_BOOL iluCharStringShortCharStringEqual(const iluCharacter* pc_chars, const iluShortCharacter* pc_shortchars);


	//////////////////////////////////////////////////////////////////
	// SBH processing

	// Use these to form a string binding handle from relevant parts,
	// if protocol and/or transport info are NULL, current defaults are used.
	// For iluFormSBHUsingContactInfo, p_str_encodedContactInfo is as would
	// be obtained from iluParseSBH.
	static char* iluFormSBH(const char* pc_serverid, const char* pc_instance_handle,
		iluClass the_ilu_class, iluProtocolInfo pc_protocol_type = ((iluProtocolInfo) NULL),
		iluTransportInfo transport_info = ((iluTransportInfo) NULL));

	static char* iluFormSBHUsingContactInfo(const char* pc_serverid, const char* pc_instance_handle,
		iluClass the_ilu_class, char* p_str_encodedContactInfo = NULL);


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
	static ILUCPP_BOOL iluParseSBH(iluCString  str_encodedSBH, 
									  iluCString*   p_str_plainInstanceHandle = NULL, 
									  iluCString*   p_str_plainServerID = NULL,
									  iluCString*   p_str_plainMstid = NULL, 
									  iluCString*   p_str_encodedContactInfo = NULL, 
									  iluCardinal*  p_card_encodedContactInfoLen = NULL,
									  ILUCPP_BOOL*  p_b_malloced_contact_info = NULL);									  


	//////////////////////////////////////////////////////////////////
	// File descriptor budget functions

	// return the current file descriptor budget
	static iluCardinal iluGetFDBudget(void) {return ilu_GetFDBudget();}

	// set the current file descriptor budget, returns budget
	static iluCardinal iluSetFDBudget(iluCardinal card_new_budget) {return ilu_SetFDBudget(card_new_budget);}


	//////////////////////////////////////////////////////////////////
	// mamory functions

	// use this to free things returned by ilu
	static void iluFree(void* pv /* ILUowned */ ) { ilu_free(pv); }

	// use this to malloc things from ilu
	static void* iluMalloc(iluCardinal card_size) { return ilu_malloc(card_size); }

	virtual void iluSimplyEnsureThisClassCannotBeInstantiated() = 0;

private:

	// ILUCPP_TRUE iff already initialized
	static ILUCPP_BOOL sm_b_initialized;

}; // end class iluCppRuntime



//////////////////////////////////////////////////////////////////
// iluCppInternal - Abstract class that provides various calls that are specificly 
// meant only for use by the C++ runtime or stubs.  Not meant to ever be subclassed.

ILU_RUNTIME_PUBLIC_CLASS iluCppInternal {
	
	// friends to allow initialization of sm_object_type_mutex and sm_pfunction_forking_procedure
	friend void iluCppRuntime::iluInitialize(ILUCPP_BOOL);
	friend void iluCppRuntime::iluAddInitializationFunction(iluPFunctionInitializer);
	friend void iluCppRuntime::iluSetForkProcedure(iluForkProc);
	friend void iluCppRuntime::iluSetNonNativeThreadIDFunction(iluNonNativeThreadIDFunction);

	virtual void iluSimplyEnsureThisClassCannotBeInstantiated() = 0;
	
public :

	//////////////////////////////////////////////////////////////////
	// Garbage Collection

	// Called by (surrogate) stubs produced for Collectible objects to ensure that a callback
	// object is created to be the target of pings from true objects.  It is 
	// created in its own server.
	static void iluEnsureGCCallback();

	// Called by (true) stubs produced for Collectible objects to ensure that a 
	// a client's interest in a collectible object can effect whether the true object is
	// kept in memory (has a ref count on it)
	static void iluEnsureGCNoter();
	
	// the function that is called by the kernel when a Collectible object has
	// or ceases to have clients
	static iluBoolean iluGCNoter(iluKernelObject a_kernel_object, int i_interested);

	//////////////////////////////////////////////////////////////////
	// Type definition related

	// grab and release the mutex that's used to ensure exclusive access for 
	// calls involving object type definitions
	static void iluEnterObjectTypeMutex();
	static void iluExitObjectTypeMutex();

	// adds the named exception to the (internal) table of exceptions for the named interface
	static iluException iluDefineException(iluCString pc_interface, iluCString pc_exception,
		iluCString pc_type_id_of_exception_value);

	// adds the named object type to the system
	static iluClass	iluDefineObjectType(
		iluCString pc_classname,
		iluCString pc_brand,
		iluCString pc_type_id,
		iluCString pc_singleton, // protocol info if class is a singleton, else NULL
		ILUCPP_BOOL b_optional,
		ILUCPP_BOOL b_collectible,
		iluCString pc_docstring,
		iluCardinal card_number_of_methods,
		iluCardinal card_number_of_superclasses,	
		iluCString* ppc_array_of_superclass_ids);

	// Defines the i'th method of the specified class 
	static iluMethod iluDefineMethod(iluClass class_method_is_part_of,
		  iluCardinal card_ith_method,
		  iluCString pc_method_name,
		  iluCardinal card_method_id,
		  ILUCPP_BOOL b_cacheable,
		  ILUCPP_BOOL b_asynchronous,
		  iluCardinal card_number_of_exceptions_it_can_raise,
		  iluException* ppc_array_of_exception_names,
		  iluCardinal card_number_of_arguments,
		  iluCString pc_return_type_id);

	// Defines the index'th arg of method m.
	static ILUCPP_BOOL iluDefineMethodArg(iluMethod method,
		    iluCardinal card_index,
		    iluCString pc_argname,
		    ILUCPP_BOOL	b_sibling,
		    iluArgDirection e_direction,
		    iluCString pc_typeid);

	// register a new sequence type 
	static iluType
		iluRegisterSequenceType(
		iluCString pc_name,				// name 
		iluCString pc_interface_name,	// interface-name 
		iluCString pc_interface_brand,	// interface-brand, OPTIONAL 
		iluCString pc_uid,				// UID 
		iluTypeUID pc_type_uid,			// base type 
		iluCardinal card_limit,			// limit (0 if none) 
		ILUCPP_BOOL * p_b_new			// OUT:  true if new registration 	
		) ;
	
	// Register a new array type 
	static iluType
		iluRegisterArrayType (
		iluCString pc_name,					// name 
		iluCString pc_interface_name,		// interface-name 
		iluCString pc_interface_brand,		// interface-brand, OPTIONAL 
		iluCString pc_uid,					// UID 
		iluTypeUID pc_type_uid,				// base type 
		iluCardinal card_num_dimensions,	// number of dimensions 
		iluCardinal * p_card_dim_seq,		// pointer to sequence of dimensions 
		ILUCPP_BOOL * p_b_new				// OUT:  true if new registration 	
		);
	
	// call this function to begin registering a new record type 
	static iluType
		iluRegisterRecordType (
		iluCString pc_name,				// name 
		iluCString pc_interface_name,	// interface-name 
		iluCString pc_interface_brand,	// interface-brand, OPTIONAL 
		iluCString pc_uid,				// UID 
		iluCardinal card_num_fields,	// number of fields 
		ILUCPP_BOOL * p_b_new			// OUT:  true if new registration 	
		);
	
	// call this function repeatedly to fill in the fields of a newly registered record type 
	static ILUCPP_BOOL
		iluRegisterRecordField (
		iluType the_type,				// record type 
		iluCardinal card_field_index,	// field index 
		iluCString pc_field_name,		// field name 
		iluTypeUID pc_type_uid			// field type 
		);
	
	// call this function to begin registering a new enumeration type 
	static iluType
		iluRegisterEnumerationType (
		iluCString pc_name,				// name 
		iluCString pc_interface_name,	// interface-name 
		iluCString pc_interface_brand,	// interface-brand, OPTIONAL 
		iluCString pc_uid,				// UID 
		iluCardinal card_num_elements,	// number of elements 
		ILUCPP_BOOL * p_b_new			// OUT:  true if new registration 	
		);
	
	// call this function repeatedly to fill in the fields of a newly registered enum type 
	static ILUCPP_BOOL
		iluRegisterEnumerationElement (
		iluType the_type,				// enumeration type 
		iluCardinal card_element_num,	// which element 
		iluCString pc_element_name,		// element name 
		iluCardinal card_element_value	// element value 
		);
	
	// call this function to begin registering a new union type 
	static iluType
		iluRegisterUnionType(
		iluCString pc_name,				// name 
		iluCString pc_interface_name,	// interface-name 
		iluCString pc_interface_brand,	// interface-brand, OPTIONAL 
		iluCString pc_uid,				// UID 
		iluTypeUID pc_type_uid,			// discriminant type 
		iluCardinal card_num_arms,		// number of arms 
		iluCardinal card_default_arm,	// default arm (0 for none { } 1-n otherwise) 
		ILUCPP_BOOL b_others_allowed,	// others allowed 
		ILUCPP_BOOL * p_b_new			// OUT:  true if new registration 	
		);
	
	// call this function repeatedly to fill in the fields of a newly registered union type 
	static iluUnionArm
		iluRegisterUnionArm  (
		ilu_Type the_type,				// union type 
		iluCardinal card_arm_number,	// which arm? 
		iluCString pc_arm_name,			// arm name 
		iluTypeUID pc_type_uid,			// arm type 
		iluCardinal card_num_values		// number of values 
		);
	
	// if there was more than one possible value for a union arm, fill them in here 
	static ILUCPP_BOOL
		iluRegisterUnionArmValue (
		iluUnionArm arm,					// arm to fill in 
		iluCardinal card_value_index,		// value index 
		iluConstantValue	constant_value	// value 
		);
	
	// register an optional type 
	static iluType
		iluRegisterOptionalType (
		iluCString pc_name,				// name 
		iluCString pc_interface_name,	// interface-name 
		iluCString pc_interface_brand,	// interface-brand, OPTIONAL 
		iluCString pc_uid,				// UID 
		iluTypeUID pc_type_uid,			// base type for optional 
		ILUCPP_BOOL * p_b_new			// OUT:  true if new registration 	
		);
	
	// register an alias for a type 
	static iluType
		iluRegisterAliasType (
		iluCString pc_name,				// name 
		iluCString pc_interface_name,	// interface-name 
		iluCString pc_interface_brand,	// interface-brand, OPTIONAL 
		iluCString pc_uid,				// UID 
		iluTypeUID pc_type_uid,			// base type 
		ILUCPP_BOOL * p_b_new			// OUT:  true if new registration 	
		);
	
	// register an object type 
	static iluType
		iluRegisterObjectType(
		iluCString pc_name,				// name 
		iluCString pc_interface_name,	// interface-name 
		iluCString pc_interface_brand,	// interface-brand, OPTIONAL 
		iluCString pc_uid,				// UID 
		iluClass the_class,				// class ptr from iluDefineObjectType 
		ILUCPP_BOOL * p_b_new			// OUT:  true if new registration 	
		) ;
	
	// add simple type 
	static iluType
		iluRegisterPrimitiveType (
		iluCString pc_name,				// name 
		iluCString pc_interface_name,	// interface-name 
		iluCString pc_interface_brand,	// interface-brand, OPTIONAL 
		iluCString pc_uid,				// UID 
		iluTypeKind type_kind,			// which primitive type 
		ILUCPP_BOOL *	p_b_new			// OUT:  true if new registration 	
		);
	
	
	// called when the specified class is completely defined (i.e. after all methods 
	// for the class have been defined
	static void iluObjectTypeDefined(iluClass class_completely_defined);


	//////////////////////////////////////////////////////////////////
	// thread and connection related

	// calls ilu_OSForkNewThread and throws an exception if it returns an error
	static void iluThrowingFork(void (*pfunction_procedure) (void* pv_argument), 
		void* pv_argument);

	// When OS threading is used, forks threads to monitor any new outgoing connections
	static void iluCreateMonitorsForNewOutgoingConnections(void* pv_ignored);

	// calls ilu's procedure to monitor an outgoing connection when OS threading is used
	static void iluMonitorOutgoingConnection(void* pv_connection);

	// when a connection request arrives at the port when OS threading is used, fork
	// a thread to handle input on it
	static void iluThreadedReadConnectionRequests (void* pv_port);

	// gets called when when running single threaded and a connection request arrives 
	// at the port
	static void iluNonThreadedReadConnectionRequests (void* pv_port);

	// handles input on a connection when OS threading is used
	static void iluRunConnection (void *arg);

	// when a connection handles concurrent requests, this function gets
	// run in a thread to run the stub procedure and finish up
	static void iluPerformThreadedRequest(void* pv_invocation_arg);

	// service a request - called either from the thread handling the connection
	// (iluRunConnection) in the OS threaded case, or in the single threaded case
	// from ilu who has been told (ilu_SetConnectionRequestHandler) that this is the
	// function to call when a input comes in on a connection
	static ILUCPP_BOOL iluServiceRequest (void* pv_connection);
	
	// just calls the iluServiceRequest and cleans up if we're done with the connection
	static void iluNonThreadedServiceRequest (void* pv_arg);

	// disables requests on a connection
	static ILUCPP_BOOL iluDisableRequests(iluCall call, iluConnection connection);
	
	// enables requests on a connection
	static ILUCPP_BOOL iluEnableRequests(iluCall call, iluConnection connection);

	// returns ILUCPP_TRUE iff we're running in a mode where we have condition variables
	static ILUCPP_BOOL iluHaveConditions();

	// calls the iluDecrementInvocations function of the activation table - here
	// so we don't have to export the activation table to the stubs
	static ILUCPP_BOOL iluDecrementInvocations(iluKernelObject a_kernel_object);

	//////////////////////////////////////////////////////////////////
	// initialization

	// Walks down each node in the list, calls the nodes function, and 
	// removes that node from the list.
	static void iluCallInitializationFunctions(iluInitializationFunctionNode** pp_initialization_function_list);

	// Creates a iluInitializationFunctionNode for the list
	static void iluAddInitializationFunction(iluInitializationFunctionNode** pp_initialization_function_list, 
		iluPFunctionInitializer pf_initialize);


	// Tells the runtime what function to call when a surrogate for an object of
	// the specified class is needed.
	static void iluRegisterSurrogateCreator(iluClass surrogate_class,
		iluPFunctionSurrogateCreator pfunction_surrogate_creator );

	// called by the iluCppRuntime::iluSetSurrogateCreator
	static iluPFunctionSurrogateCreator iluSetSurrogateCreator(iluClass surrogate_class,
		iluPFunctionSurrogateCreator pfunction_surrogate_creator );

	// calls the surrogate creator function for the specified class, and returns 
	// an iluObject* to the surrogate, or NULL if the class isn't found.
	static iluObject* iluCreateSurrogate(iluClass surrogate_class, 
		iluKernelObject a_kernelobject);


	//////////////////////////////////////////////////////////////////
	// exception related

	// Thows the appropriate corba system exception for the iluError
	static void iluThrowExceptionFromError(iluError* p_error, const char* pc_filename, int i_line_number);


	//////////////////////////////////////////////////////////////////
	// accessors

	static iluCardinal iluGetCppLanguageIndex() { return sm_card_cpp_language_index;}

	static ILUCPP_BOOL iluGetRunningThreaded() { return sm_b_running_threaded;}

	// returns id of current thread
	static iluCardinal iluGetThreadID();

	//////////////////////////////////////////////////////////////////
	// member variables


	//  pointer to ilu's object type mutex
	static ilu_Mutex sm_object_type_mutex;

private:

	// identifies the C++ language runtime
	static iluCardinal sm_card_cpp_language_index;

	// set to ILUCPP_TRUE if we're running threaded
	static ILUCPP_BOOL sm_b_running_threaded;

	// when running non-native theaded, this should be set to a function that will return a thread unique iluCardinal
	// id of the current thread
	static iluNonNativeThreadIDFunction sm_card_thread_id_function;

	// points to the function used to fork a thread
	static iluForkProc sm_pfunction_forking_procedure;

	// points to list of surrogate creator nodes used when we need to 
	// create a surrogate of a particular class
	static iluSurrogateCreatorNode* sm_p_surrogate_creator_list;

	// mutex used to control access to sm_p_surrogate_creator_list
	static iluMutex sm_surrogate_creator_list_mutex;

	// all the interfaces should place their initialization function on this list
	// this way, all loaded interfaces can be initialized simply with one call to 
	// iluCppRuntime::iluInitialize
	static iluInitializationFunctionNode* sm_p_initialization_function_list;

	// true iff the gc notifier has already been set up
	static ILUCPP_BOOL sm_gc_noter_set;

}; // end class iluCppInternal



//////////////////////////////////////////////////////////////////
// iluObjectTable - An C++ class for developers to derive from to
// provide Object Tables - never directly instantiate this class.


ILU_RUNTIME_PUBLIC_CLASS iluObjectTable {
	
	friend class iluServer;
	
public:
	
	// constructor
	iluObjectTable();

	// copy constructor and assignment operator prevents copying object tables
	iluObjectTable(const iluObjectTable&) { ILUCPP_BAD_OPERATION();	}
	iluObjectTable& operator==(const iluObjectTable&) { ILUCPP_BAD_ASSIGNMENT(); }

	// Called by ILU to create and return a new iluObject* with the specified instance 
	// handle.  ILU retains ownership of pc_instance_handle - i.e. copy it if you need
	// want to hang on to it.  Note that when in this function, you are 'inside' 
	// the object's server - i.e. you hold the locks on the server - this means that when you 
	// create the object, you must specify the 3rd argument to the object's constructor 
	// (b_within_object_table) as true.
	virtual iluObject* iluObjectOfInstanceHandle (iluCString pc_instance_handle /* ILUowned */) = 0;
	
	// Do whatever destroying the Object Table needs to do to free up resources, etc.
	// It gets called when the ilu server it's associated with it is shut down.
	virtual ~iluObjectTable();


protected:

	// returns pointer to the iluServer this object table is associated with
	iluServer* iluGetServer() {return m_p_server;}

private:
	
	// an ilu object table structure, the iluDispatch... members dispatch
	// to the member functions of iluObjectTable
	iluKernelObjectTable iluGetKernelObjectTable() {return m_kernel_object_table_dispatcher;}
	iluKernelObjectTable m_kernel_object_table_dispatcher;

	static iluKernelObject iluDispatchObjectOfInstanceHandle(iluKernelObjectTable pv_iluKernelObjectTable_instance,
		iluCString pc_instance_handle);
	
	static void iluDispatchDestructor(iluKernelObjectTable pv_iluKernelObjectTable_instance);

	// when an iluObjectTable is passed to the constructor for an iluServer, the
	// iluServer constructor (friend) sets this member to itself so that iluObjectOfInstanceHandle
	// can know what server to create the object in.
	void iluSetServer(iluServer* p_server) {m_p_server = p_server;}
	iluServer* m_p_server;

}; // end class iluObjectTable



//////////////////////////////////////////////////////////////////
// iluMainLoop - Subclass from this class if you want to have your
// own version of the main loop. A single threaded application should
// supply all functions.  An application making use of ILU's OS multi-threaded
// operation should not use a different mainloop.  If you're using your
// own thread package, you must supply all functions, and see the comment
// for iluCppRuntime::iluInitialize


ILU_RUNTIME_PUBLIC_CLASS iluMainLoop {

public:

	// copy constructor and assignment operator prevents copying main loops
	iluMainLoop(const iluMainLoop&) { ILUCPP_BAD_OPERATION();	}
	iluMainLoop& operator==(const iluMainLoop&) { ILUCPP_BAD_ASSIGNMENT(); }


	//////////////////////////////////////////////////////////////////
	// Running

	// Runs the main loop until *p_i_stop_on_non_zero is non-zero
	virtual void iluRun (int* p_i_stop_on_non_zero) = 0;

	// Causes the main loop to exit
	virtual void iluExit (int* p_i_stop_on_non_zero) = 0;


	//////////////////////////////////////////////////////////////////
	// Input Output Handlers

	// Input Handlers - When there is input activity on the file descriptor 
	// i_fd, the mainloop will call the registered handler procedure
	// pfunction_input_handler, passing it i_fd and pv_input_handler_arg as arguments.
	// Returns ILUCPP_FALSE if it can't do it's job due to some resource limitation.
	virtual ILUCPP_BOOL	iluRegisterInputHandler (int i_fd,
		void (*pfunction_input_handler)(int i_fd, void* pv_input_handler_arg),
		void* pv_input_handler_arg ) = 0;

	// Returns ILUCPP_FALSE if input on i_fd was being handled, else ILUCPP_TRUE
	// Sets function and arg ptrs to what they were if anything
	virtual ILUCPP_BOOL	iluUnregisterInputHandler (int i_fd,
		void (**ppfunction_input_handler)(int i_fd, void* pv_input_handler_arg),
		void** ppv_input_handler_arg ) = 0;


	// Output Handlers - When it is possible to perform output on the file descriptor 
	// i_fd, the mainloop will call the registered handler procedure
	// pfunction_output_handler, passing it i_fd and pv_output_handler_arg as arguments
	virtual ILUCPP_BOOL	iluRegisterOutputHandler (int i_fd,
		void (*pfunction_output_handler)(int i_fd, void* pv_output_handler_arg),
		void* pv_output_handler_arg ) = 0;

	// Returns ILUCPP_FALSE if output on i_fd had a handler, else ILUCPP_TRUE
	// Sets function and arg ptrs to what they were if anything
	virtual ILUCPP_BOOL	iluUnregisterOutputHandler (int i_fd,
		void (**ppfunction_output_handler)(int i_fd, void* pv_output_handler_arg),
		void** ppv_output_handler_arg ) = 0;


	//////////////////////////////////////////////////////////////////
	// Alarms - An alarm is an active object which can be set to 
	// asynchronously invoke a procedure with an argument at a specified time.

	// Creates an alarm.  An alarm may be something like a pointer to a
	// structure that has some internal structure, but from the point of
	// view of an alarm user, it's just a handle that is used to specify
	// a particular alarm to be set or cleared
	virtual iluAlarm iluCreateAlarm() = 0;

	// Sets up an alarm to call the handler procedure pfunction_alarm_handler,
	// passing it pv_alarm_handler_arg as an argument, when the alarm_time
	// occurs.
	virtual void iluSetAlarm(iluAlarm the_alarm, 
		iluFineTime alarm_time, 
		void (*pfunction_alarm_handler)(void* pv_alarm_handler_arg), 
		void* pv_alarm_handler_arg) = 0;

	// Cancels the alarm (effectivly sets the alarm time to infinity)
	virtual void iluClearAlarm (iluAlarm the_alarm) = 0;

	// Destroys the alarm (if alarm is set, does not invoke)
	virtual void iluDestroyAlarm (iluAlarm the_alarm) = 0;

	// utility function to set the pointed to ilu_FineTime to a time i_secs + i_msecs in the future
	static void iluSetFineTimeFromNow(ilu_FineTime* p_finetime, ilu_integer i_secs, ilu_cardinal i_msecs);


	//////////////////////////////////////////////////////////////////
	// Setting the Main Loop to be used

	// Call iluSetMainLoop set your mainloop as the one for ilu to use.
	// It should called before any ilu initialization.
	static void iluSetMainLoop(iluMainLoop* p_mainloop_instance);
	static iluMainLoop* iluGetMainLoop() {return m_p_mainloop;}


	//////////////////////////////////////////////////////////////////
	// When you haven't set the main loop (i.e. you're using ILU's default
	// loop), you can call these functions to create, set and unset alarms.
	// (If you set your own main loop, just call its alarm functions.)
	
	static iluAlarm iluDefaultLoopCreateAlarm() { return ilu_CreateAlarm(); }
	
	static void iluDefaultLoopSetAlarm(iluAlarm the_alarm, iluFineTime alarm_time,
		void (*pfunction_alarm_handler)(void* pv_alarm_handler_arg), 
		void* pv_alarm_handler_arg);
	
	static void iluDefaultLoopClearAlarm(iluAlarm the_alarm) { ilu_UnsetAlarm(the_alarm); }
	
	static void iluDefaultLoopDestroyAlarm (iluAlarm the_alarm) { ilu_DestroyAlarm(the_alarm); }

	private:

		//////////////////////////////////////////////////////////////////
		// an ilu kernel mainloop structure, the iluDispatch... members dispatch
		// to the functions of the iluMainLoop set in m_p_mainloop
		
		static ilu_MainLoop m_kernel_to_cpp_loop_dispatcher;
		
		static void iluDispatchRun (int* p_i_stop_on_non_zero);
		
		static void iluDispatchExit (int* p_i_stop_on_non_zero);
		
		static iluBoolean iluDispatchRegisterInputHandler (int i_fd,
			void (*pfunction_input_handler)(int i_fd, void* pv_input_handler_arg),
			void* pv_input_handler_arg );
		
		static iluBoolean iluDispatchUnregisterInputHandler (int i_fd,
			void (**ppfunction_input_handler)(int i_fd, void* pv_input_handler_arg),
			void** ppv_input_handler_arg );		
		
		static iluBoolean	iluDispatchRegisterOutputHandler (int i_fd,
			void (*pfunction_output_handler)(int i_fd, void* pv_output_handler_arg),
			void* pv_output_handler_arg );
		
		static iluBoolean iluDispatchUnregisterOutputHandler (int i_fd,
			void (**ppfunction_output_handler)(int i_fd, void* pv_output_handler_arg),
			void** ppv_output_handler_arg );		
				
		static iluAlarm iluDispatchCreateAlarm();
		
		static void iluDispatchSetAlarm(iluAlarm the_alarm, 
			iluFineTime alarm_time, 
			void (*pfunction_alarm_handler)(void* pv_alarm_handler_arg), 
			void* pv_alarm_handler_arg);
		
		static void iluDispatchClearAlarm (iluAlarm the_alarm);

		static void iluDispatchDestroyAlarm (iluAlarm the_alarm);
		
		

	// holds pointer to the main loop that was either set via a call to 
	// iluSetMainLoop (before initialization), or the default main
	// loop (set if the user does not call iluSetMainLoop before initialization.
	static iluMainLoop* m_p_mainloop;

}; // end class iluMainLoop


//////////////////////////////////////////////////////////////////
// iluServerRelocator - abstract class that wraps up the relocator functionality

ILU_RUNTIME_PUBLIC_CLASS iluServerRelocator {

    public:

	// This function should return a new pinfo and tinfo for the connection
	// to be relocated to.  It should return iluTRUE if new pinfo and tinfo
	// are returned, and iluFALSE if new pinfo and tinfo are not returned.
	// Ownership of storage for the returned pinfo and tinfo are passed from
	// the callee to the caller upon return.
	virtual iluBoolean doRelocation(iluProtocolInfo *pinfo, iluTransportInfo *tinfo) = 0;

}; // end iluServerRelocator


//////////////////////////////////////////////////////////////////
// iluServer - provides a the C++ view of a kernel server object

ILU_RUNTIME_PUBLIC_CLASS iluServer {

	friend void iluCppRuntime::iluInitialize(ILUCPP_BOOL);

public:
	
	// copy constructor and assignment operator prevents copying iluServers
	iluServer(const iluServer&) { ILUCPP_BAD_OPERATION();	}
	iluServer& operator==(const iluServer&) { ILUCPP_BAD_ASSIGNMENT(); }

	//////////////////////////////////////////////////////////////////
	// constructor and destructor 
	
	// Creates a server.  If no id is specified, one is automatically created based on
	// based on time, hostname, and process id.  If p_object_table is null, a default
	// object table implementation is used.  If b_addport is TRUE, a port is created and added to the
	// server using the specified protocol and transport, and becomes the default
	// port of the server.  pc_protocol_type and transport_info default to whatever 
	// the default protocol and transport are currently set to. (done in the constructor itself
	// rather than in the default arg initializers due to a g++ bug) Caller owns pc_server_id
	// p_object_table, pc_protocol_type, transport_info, and p_passport. p_passport points 
	// to an iluPassport, defaulted to null -  this passport containing an ILU GSS identity, 
	// which is used as the identity of the principal offering the service, and put into the connection 
	// information in the string binding handle of objects on that server
	iluServer(char* pc_server_id = NULL, 
		  iluObjectTable* p_object_table = ((iluObjectTable*) NULL) /* ILUowned */, 
		  iluProtocolInfo pc_protocol_type = ((iluProtocolInfo) NULL),
		  iluTransportInfo transport_info = ((iluTransportInfo) NULL),
		  iluPassport* p_passport = ((iluPassport*) NULL),
		  ILUCPP_BOOL b_addport = ILUCPP_TRUE
		  );
	
	//////////////////////////////////////////////////////////////////
	// destructor - basically whacks the kernel server and breaks
	// all associations between kernel objects in this server and 
	// and their language specific objects.Indirectly also deletes any
	// iluObjectTable used with this iluServer.
	virtual ~iluServer();
	
	
	//////////////////////////////////////////////////////////////////
	// Adding ports	
	
	// Adds another port to an existing server If b_become_default_port is 
	// ILUCPP_TRUE the new port will become the default port for this server.
	// p_passport points to an iluPassport, defaulted to null. this passport containing 
	// an ILU GSS identity, which is used as the identity of the principal offering the 
	// service, and put into the connection information in the string binding handle 
	// of objects on that server.  If b_public is FALSE, the cinfo for the port will
	// be private -- not added to SBHs.  Caller owns the arguments.
	virtual void iluAddPort (iluProtocolInfo pc_protocol_type,
				 iluTransportInfo transport_info,
				 ILUCPP_BOOL b_become_default_port = ILUCPP_FALSE,
				 iluPassport* p_passport = NULL,
				 ILUCPP_BOOL b_public = ILUCPP_TRUE
				 );
	

	//////////////////////////////////////////////////////////////////
	// Controlling cinfo
	
	// iluGetCInfo returns the native cinfo of one of the server's ports.  If b_public
	// is true, it will return the cinfo of the first public port; otherwise it will
	// return the cinfo of the first private port.  It returns iluTRUE if a port of
	// the specified type was found, iluFALSE if not.  The caller owns the returned
	// pinfo and tinfo, and is responsible for freeing them.
	//
	// iluAddCInfo adds the specified cinfo to the list of public cinfos for the
	// server, so that objects exported from the server have that cinfo.  The caller
	// retains ownership of the pc_pinfo and pc_tinfo arguments.

	virtual ILUCPP_BOOL iluGetCInfo (iluProtocolInfo *pp_pinfo,
					 iluTransportInfo *pp_tinfo,
					 ILUCPP_BOOL b_public=ILUCPP_FALSE);
	virtual void iluAddCInfo (const iluProtocolInfo pc_pinfo,
				  const iluTransportInfo transport_info);


	//////////////////////////////////////////////////////////////////
	// Relocation
	//
	// This allows relocation of an incoming request to another server.
	// The application code must supply an instance of iluServerRelocator.
	// The "p_relocator" parameter may be NULL to disable relocation.

	virtual iluServerRelocator* iluSetRelocator (iluServerRelocator *p_relocator);


	//////////////////////////////////////////////////////////////////
	// Running

	// This runs the main, outer loop of an iluServer. It never returns
	// if p_i_stop_on_non_zero isn't supplied, else it returns when 
	// *p_i_stop_on_non_zero is non zero. If you're running threaded
	// this routine simply goes into a sleep loop,
	virtual void iluRun(int *p_i_stop_on_non_zero = NULL);

	
	//////////////////////////////////////////////////////////////////
	// Defaults
	
	// Get and set the default protocol used when adding a port on a 
	// server - initialized to whatever is set to be the default
	// in the kernel (found in iluconf.h or iluwin.h).  Callee owns pc_new_default_protocol.
	static iluProtocolInfo iluGetDefaultProtocol();
	static void iluSetDefaultProtocol(iluProtocolInfo  pc_new_default_protocol);
	
	// Get and set the default transports used when adding a port on a 
	// server - initialized to whatever is set to be the default
	// in the kernel (found in iluconf.h or iluwin.h)
	// Callee owns pc_new_default_transport_info.
	static const iluTransportInfo iluGetDefaultTransport();
	static void iluSetDefaultTransport(iluTransportInfo ppc_new_default_transport_info);
	
	// Returns the default iluServer, creating one if need be.
	static iluServer& iluGetDefaultServer();
	
	// Sets the default iluServer, returns old default, which is NULL if no default currently is set
	static iluServer* iluSetDefaultServer(iluServer& new_default_server);


	//////////////////////////////////////////////////////////////////
	// Stub and internal use only

	// grab and release the mutex that's used to ensure exclusive access
	// to the kernel server
	void iluEnterServerMutex(iluClass the_class);
	void iluExitServerMutex(iluClass the_class);

	// accessor for the iluServer's kernel server
	iluKernelServer iluGetKernelServer();

	// accessor for the iluServer's kernel server id
	const char* /* ILUowned */ iluGetKernelServerId();

private:
	
	// protocol and transport to use by default - initialized to whatever is set to be the default
	// in the kernel (found in iluconf.h or iluwin.h)
	static char* sm_pc_default_protocol;
	static iluTransportInfo sm_ppc_default_transport_info;

	// for in memory transport
	static char* sm_pc_in_memory_protocol_info;
	static iluTransportInfo sm_ppc_in_memory_transport_info;

	// sm_card_servercount (initially 0) and m_pc_hostname (this machine's hostname)
	// are used to automatically generate unique server id's when needed.
	static iluCardinal sm_card_server_counter;
	static char sm_c_hostname[];
		
	// Iff a default server is needed, then this member points to it.
	static iluServer* sm_p_default_server;
	
    // the kernel server of this c++ server
	iluKernelServer m_kernel_server;
	
}; // end class iluServer



//////////////////////////////////////////////////////////////////
// the argument passed to alarm handler when doing scheduled dec refs

class iluScheduledDecrementReferenceCountArg {
	
public: 

	iluScheduledDecrementReferenceCountArg(iluObject* p_object_to_be_decreffed,	
		iluAlarm an_alarm, iluMainLoop* p_mainloop) :
		m_p_object_to_be_decreffed(p_object_to_be_decreffed), 
		m_alarm(an_alarm), 
		m_p_mainloop(p_mainloop) {}

	~iluScheduledDecrementReferenceCountArg() {
		if (m_p_mainloop) {
			m_p_mainloop->iluClearAlarm(m_alarm);
			m_p_mainloop->iluDestroyAlarm(m_alarm);
		}
		else {
			iluMainLoop::iluDefaultLoopClearAlarm(m_alarm);
			iluMainLoop::iluDefaultLoopDestroyAlarm(m_alarm);	
		}
	}

	iluObject* m_p_object_to_be_decreffed;

private:

	iluAlarm m_alarm;
	iluMainLoop* m_p_mainloop;
};



//////////////////////////////////////////////////////////////////
// iluObject - The most base class for all ILU C++ objects.  All objects inherit either 
// directly or indirectly from this class. All non-static member functions are virtual
// to allow creative overrides (at your own risk of course). iluObjects
// cannot be copied or assigned.
ILU_RUNTIME_PUBLIC_CLASS iluObject {

	friend ILU_RUNTIME_PUBLIC_CLASS iluBaseCall;
	friend ILU_RUNTIME_PUBLIC_CLASS iluSurrogateCall;
	friend void iluCppRuntime::iluInitialize(ILUCPP_BOOL);

public:
	
	//////////////////////////////////////////////////////////////////
	// constructor and destructor 

	// copy constructor and assignment operator prevents copying iluObjects
	iluObject(const iluObject&) { ILUCPP_BAD_OPERATION();	}
	iluObject& operator==(const iluObject&) { ILUCPP_BAD_ASSIGNMENT(); }

	// If no instance handle is specified, then the value of a monotonicaly
	// increasing, iluServer specific counter will be used to generate one.  
	// If no server is specified, then the default server will be used. 
	// (The default server is generated automatically if needed, and has the
	// an id based on time, hostname, and process id.)  Caller owns pc_instance_handle.
	// The new object has a reference count of 1
	// If b_within_object_table is true, then it is assumed the object is being created
	// inside an iluObjectTable's iluObjectOfInstanceHandle function, meaning that
	// the locks on the server should not be modified.
	iluObject(iluClass the_Class, char* pc_instance_handle = NULL, 
		iluServer& the_server = iluServer::iluGetDefaultServer(),
		ILUCPP_BOOL b_within_object_table = ILUCPP_FALSE);
	
	// default constructor - used when creating a surrogate
	// The new object has a reference count of 0
	iluObject();

	// Destructor ensures that this object is completely disassociated from the ilu kernel 
	// The most specific destructor of an object should call iluDeactivate
	// on the object to block any further incoming calls, and wait for any 
	// ongoing calls to complete.  Next it should perform any object specific
	// cleanup.  Finally, the destructor in iluObject will break the association
	// between the kernel object and this object, allowing the kernel object
	// to be potentially freed.
	virtual ~iluObject();
	

	// given a string binding handle (e.g. as obtained from iluObjectToString)
	// returns an iluObject* for that object, with the reference count incremented.
	static iluObject* iluStringToObject (char* pc_string_binding_handle);

	//////////////////////////////////////////////////////////////////
	// Simple Object Publication Operations 

	// publishes binding information for this object in the binding service
	// Has no effect on object reference count
	virtual ILUCPP_BOOL iluPublish ();

	// Removes binding information for this object from the binding service
	// Has no effect on object reference count
	virtual ILUCPP_BOOL	iluWithdraw ();

	// Used by stubber generated iluLookup functions in derived classes to 
	// lookup an object in the binding service based on its instance and server id
	// and class. Increments reference count of object.
	static void* iluLookup (char* pc_instance_handle, char* pc_server_id, iluClass the_class);


	//////////////////////////////////////////////////////////////////
	// Object activation deactivation and destruction
	// (activate and deactivate are really BOA-like kinds of operations)
	// Use is only sensible in threaded situations.

	// Ensures this object is available from the outside (i.e. possible to 
	// to do remote invocations on)
	virtual void iluActivate();

	// Ensures this object is not available from the outside. This must
	// be the first thing called by the most specific destructor of an 
	// object. If if isn't, then the potential exists (in multithread case) 
	// for a call to come in for an object that's in the middle of
	// destruction - a bad thing!  This function blocks until there are
	// zero ongoing calls.
	virtual void iluDeactivate();


	//////////////////////////////////////////////////////////////////
	// Called by iluUnlinkKernelObject - you can override this virtual 
	// function in your objects to do whatever you like when the association
	// between your object and the kernel object is broken - e.g. delete yourself
	// The implementation in iluObject deletes this.
	virtual void iluKernelObjectUnlinked ();


	//////////////////////////////////////////////////////////////////
	// Accessors

	// Returns the iluServer that this object resides in
	// this is NULL is the object is a surrogate
	virtual iluServer* iluGetServer ();

	// Returns the objects instance id
	virtual const char* /* ILUowned */ iluId ();

	// Returns the id of the object's ilu_Server
	virtual const char* /* ILUowned */ iluServerId ();


	//////////////////////////////////////////////////////////////////
	// Informational 

	// Returns the ILU string binding handle for the object
	// caller get ownership of the string
	virtual iluCString	iluObjectToString ();

	// returns true if the object is of a collectible class
	ILUCPP_BOOL iluIsCollectibleObject() { 
		return (ilu_CollectibleP(m_class) ? ILUCPP_TRUE : ILUCPP_FALSE); }

#ifdef IIOP_PROTOCOL

	// Returns a string which is the object's name and contact information
	// as specified by the CORBA IIOP spec - caller gets ownership of the string
	// May return NULL if the object is not exported through an IIOP ilu_Port
	virtual iluCString	iluObjectToIORString ();

#endif

#ifdef HTTP_PROTOCOL

	// Returns a string which is the object's name and contact information
	// as specified by an HTTP URL - caller gets ownership of the string
	// May return NULL if the object is not exported through an HTTP ilu_Port
	virtual iluCString	iluObjectToURLString ();

#endif

	// Returns true if the true object exists, and the process
	// serving it can be contacted, otherwise false
	ILUCPP_BOOL iluPing();

	// Return the ilu class name and type id - primarily informational use
	virtual const char*	/* ILUowned */	iluClassName ();
	virtual const char*	/* ILUowned */	iluClassId   ();
	
	// returns true if the objects really represent the same thing
	ILUCPP_BOOL _is_equivalent(iluObject* p_obj){ 
		return (((this == p_obj) || 
				(iluGetKernelObject() == p_obj->iluGetKernelObject())) ? ILUCPP_TRUE : ILUCPP_FALSE);
	}


	// returns true if the objects are in the same ilu server
	// used for example to determine if objects are siblings
	ILUCPP_BOOL iluInSameServer(const iluObject* p_obj){ 
		return (((this == p_obj) || 
				(strcmp((CONST_CAST(iluObject * const, this))->iluServerId(), 
						(CONST_CAST(iluObject * const, p_obj))->iluServerId()) == 0)) ? 
						ILUCPP_TRUE : ILUCPP_FALSE);
	}


	//////////////////////////////////////////////////////////////////
	// Stub and internal use only

	// Downcasting 
	virtual void* iluDowncast (iluClass class_to_cast_down_to);

	// returns the kernel object of this C++ object
	virtual iluKernelObject iluGetKernelObject();

	// associates the passed kernel object with this object - used only for 
	// creating surrogates. The object has its reference count set to 0
	virtual void iluAssociateKernelObject(iluKernelObject a_kernel_object);

	//////////////////////////////////////////////////////////////////
	// Reference count operations - when an object is first created, it has
	// a reference count of one.  If the reference count ever goes to zero,
	// delete is called on this.
	virtual void iluIncrementReferenceCount();
	virtual void iluDecrementReferenceCount();

	virtual void _release() {iluDecrementReferenceCount();}

	virtual void iluScheduledDecrementReferenceCount(iluCardinal seconds_from_now, 
		iluCardinal plus_msec_from_now);

	virtual iluCardinal iluGetReferenceCount();

	static iluObject* _duplicate(iluObject* p_obj){ 
		if (p_obj) p_obj->iluIncrementReferenceCount();
		return p_obj;
	}

	static iluObject* _narrow(iluObject* p_obj) {
		return ((iluObject*) (p_obj ? (p_obj->iluDowncast(ilu_rootClass)) : NULL));
	}


	// the alarm handler for doing scheduled dec refs
	static void iluScheduledDecrementReferenceCountHandler(void* pv_alarm_handler_arg);



	//////////////////////////////////////////////////////////////////
	// Normally for ilu cpp runtime use only - returns the m_class set at construction time
	virtual iluClass iluGetClassRecord();

	//////////////////////////////////////////////////////////////////
	// For ilu cpp runtime use only - used during iluServer deletion to break 
	// the linkage between an iluObject and it's kernel object
	// Note there are some thread safety issues here, but we're going to assume
	// that if an app decided to destroy an iluServer, that it has sense 
	// enough not to perform operations on the objects that were served by that server,
	// and has done reasonable things first e.g. withdraws, deactivations, etc.
	static int iluUnlinkKernelObject (iluKernelObject the_kernel_object, 
			ilu_refany pv_iluserver );

	
private:

	// grab and release the mutex that's used to ensure exclusive access
	// to the kernel server
	void iluEnterServerMutex(iluClass the_class) {
		if (m_p_cpp_server)
			m_p_cpp_server->iluEnterServerMutex(the_class);
		else 
			ilu_EnterServer(ilu_ServerOfObject(m_kernel_object), the_class);
	}

	void iluExitServerMutex(iluClass the_class) {
		if (m_p_cpp_server)
			m_p_cpp_server->iluExitServerMutex(the_class);
		else 
			ilu_ExitServer(ilu_ServerOfObject(m_kernel_object), the_class);
	}


    // the kernel object of this c++ object
	iluKernelObject		m_kernel_object;

	// the c++ server representing the kernel server for this c++ object
	// NULL if this is a surrogate object
	iluServer*			m_p_cpp_server;	
	
	// retains the 'proof' a publish operation returns so that it can be withdrawn
	char*				m_pc_publish_proof;

	// the object's instance handle - note this points to the
	// m_kernel_object's instance handle, so it should never be deleted
	char*				m_pc_instance_handle;

	// reference count used with duplicate and release
	iluInteger			m_i_reference_count;

	// the class of this object
	iluClass			m_class;

	// mutex used to control access to operations on object incrementing and
	// decrementing of reference counts. Note that this is a global mutex used 
	// for all iluObjects.  Since the iluIncrementReferenceCount and 
	// iluDeccrementReferenceCount are short quick operations, having a 
	// single global mutex seemed a better choice than to burden every object
	// with its own mutex. Note: something to potentially reconsider though.
	static iluMutex sm_reference_count_mutex;

	//////////////////////////////////////////////////////////////////
	// sm_card_object_counter (initially 0) is
	// used to automatically generate unique object id's when needed.
	static iluCardinal sm_card_object_counter;

	
}; // end class iluObject


//////////////////////////////////////////////////////////////////
// iluPassport - encapsulates ilu_Passport functionality

ILU_RUNTIME_PUBLIC_CLASS iluPassport {
	
public:
	
	// constructor - creates and returns a passport, optionally containing the specified identity
	iluPassport(iluIdentityInfo p_identity_info = NULL);

	// Internal use only: this constructor is used (only) by true side stubs
	iluPassport(ilu_Passport kernel_passport);
	
	// destructor - frees any associated identities in addition to freeing the passport
	// also removes any entries from the passport hash table that reference it
	virtual ~iluPassport();
		
	// Get and set the passport being used for outgoing calls - in the multi-threaded case, this is per-thread
	// Set returns the old iluPassport.  Note that before your thread exits, you should either call 
	// iluSetPassport(NULL), or delete the iluPassport in use (assuming it's only in use for a single thread).
	static iluPassport* iluGetPassport();
	static iluPassport* iluSetPassport(iluPassport* p_passport);
	
	// get the passport of the caller
	static iluPassport* /* ILUowned */ iluGetCallerPassport();

	// Internal use only: used by the true side stubs (only) to set the passport associated with an incoming call
	static void iluSetCallerPassport(iluPassport* p_passport);

	// adds identity to Passport.  Only one identity of each type is allowed.
	virtual void iluAddIdentity(iluIdentityInfo p_identity_info /* ILUowned */);
	
	// returns identity of specified type, if present else null
	virtual iluIdentityInfo /* ILUowned */ iluFindIdentity(iluIdentityType p_identity_type);
	
	// returns a copy of the passed identity
	static iluIdentityInfo iluCopyIdentity(iluIdentityInfo p_identity_info);
	
	// returns the (kernel) ilu_Passport
	ilu_Passport /* ILUowned */ iluGetIluPassport() {return m_p_passport;}

	// set up the passport hash table
	static void iluInitialize();

		
protected:
	
	// Sets r_keyvalue to some value for which the passport table has an entry for 
	// this passport.  Returns ILUCPP_TRUE if valid, else ILUCPP_FALSE if none found
	virtual ILUCPP_BOOL iluFindSomeKeyOfMe(void** ppv_keyvalue);

	// (kernel) passport
	ilu_Passport m_p_passport;

	// whether or not to destroy the m_p_passport when this iluPassport is deleted
	// if m_b_destroy_on_destruct is true, the (internal kernel) ilu_Passport structure is destroyed
	// when the ILUPassport is deleted - normally only true side stubs would set this to ILUCPP_FALSE
	// since in that case the (internal kernel) ilu_Passport is destroyed as a result of the ilu_call
	// exiting
	ILUCPP_BOOL m_b_destroy_on_destruct;

	// hash table (and lock) used to store each thread's passport (if any) for outgoing and incoming calls
	static iluHashTable sm_outgoing_passport_hash_table;
	static iluMutex sm_outgoing_passport_table_mutex;
	static iluHashTable sm_incoming_passport_hash_table;
	static iluMutex sm_incoming_passport_table_mutex;

	// used for linking passports into a list
	iluPassport* m_p_next;
};



#ifdef SECURE_TRANSPORT

//////////////////////////////////////////////////////////////////
// iluGSS - encapsulates GSS functionality

ILU_RUNTIME_PUBLIC_CLASS iluGSS {
	
public:
	
	static iluIdentityInfo iluAcquireGSSIdentity (gss_cred_id_t gss_credential);
	
	static ILUCPP_BOOL iluDecodeGSSIdentity (
		iluIdentityInfo p_identity_info,	// input; retain; info to decode
		gss_name_t* p_name,					// output; name in identity 
		iluFineTime* p_good_till_time,		// output; good-till
		gss_OID mechanism,					// input; actual mechanism desired; optional 
		ILUCPP_BOOL* p_b_local,				// if TRUE, local; otherwise remote 
		iluCardinal* p_card_flags			// connection flags, as in gss_inquire_context 
		);
	
	static gss_cred_id_t iluAcquireGSSCredForName (
		char* pc_name,
		iluCardinal card_lifetime,
		gss_OID mechanism,
		ILUCPP_BOOL b_accept_only
		);
	
	static  iluCString iluGSSNameToString (gss_name_t name);
};

#endif /* def SECURE_TRANSPORT */



//////////////////////////////////////////////////////////////////
// iluWString_var Class - analog to CORBA(String_var) only for
// ILU Characters (16 bit)

ILU_RUNTIME_PUBLIC_CLASS iluWString_var {
public:
	iluWString_var();
	iluWString_var(iluCharacter* pc_wstring);
	iluWString_var(const iluCharacter* pc_wstring);
	iluWString_var(const iluWString_var& r_wstring_to_copy);
	~iluWString_var();
	
	iluWString_var &operator=(iluCharacter* pc_wstring);
	iluWString_var &operator=(const iluCharacter* pc_wstring);
	iluWString_var &operator=(const iluWString_var& r_wstring_to_copy);
	operator iluCharacter*();
	operator const iluCharacter*() const;
	iluCharacter &operator[](iluCardinal card_index);
	iluCharacter operator[](iluCardinal card_index) const;
	
	// following is for use by ILU stubs 
	iluCharacter*& iluStringVarReference() const;
	
	// accessors on the m_b_release member
	void iluSetRelease(ILUCPP_BOOL b_release_on_destruct) const;
	ILUCPP_BOOL iluGetRelease() const;
	
	// returns true if the _var contains a null pointer
	ILUCPP_BOOL iluIsNull() const {
		return (m_pc_wstring ? ILUCPP_FALSE : ILUCPP_TRUE); 
	}
	
private:
	iluCharacter* m_pc_wstring;
	// whether or not a delete should be done when the var destructs
	ILUCPP_BOOL m_b_release;
};


//////////////////////////////////////////////////////////////////////
// The iluGarbageCollectorCallback class is meant to have nor more than
// a single instance of itself created.  This instance is what server's
// ping on in a client to determine whether or not the client is still
// alive.

class iluGarbageCollectorCallback : public iluObject {
	
public:
	
	// Constructs a new iluGarbageCollectorCallback using pc_instance_handle as the instance
	// identifier, and puts the object under the specified iluServer.
	iluGarbageCollectorCallback(char* pc_instance_handle, iluServer& r_an_ilu_server);
	
	// initialize to use of this class of objects, and create a single instance of a 
	// garbage collector object
	static void iluInitialize();
	
	// for ilu cpp runtime - stub use only - returns the m_ILUClassRecord member variable
	static iluClass iluGetILUClassRecord() {
		return m_ILUClassRecord;
	}
	
	// for use in narrowing
	virtual void* iluDowncast (iluClass class_to_cast_down_to);

	~iluGarbageCollectorCallback();   // destructor		
	

private:
	
	// holds the kernel class for this kind of object
	static iluClass m_ILUClassRecord;
	
	iluGarbageCollectorCallback();    // default constructor - should never get used
	
	// points to the one and only garbage collector object
	static iluGarbageCollectorCallback* sm_p_the_garbage_collector_object;

};  // end class iluGarbageCollectorCallback


//////////////////////////////////////////////////////////////////////
// iluMutexer is a helper class used to ensure that mutexes are released when
// functions are exited.  The constructor grabs the mutex - the destructor 
// releases it

ILU_RUNTIME_PUBLIC_CLASS iluMutexer {
	
public:

	// copy constructor and assignment operator prevents copying iluMutexer
	iluMutexer(const iluMutexer&) { ILUCPP_BAD_OPERATION();	}
	iluMutexer& operator==(const iluMutexer&)  { ILUCPP_BAD_ASSIGNMENT(); }

	iluMutexer(iluMutex a_mutex);
	~iluMutexer();

private:
	iluMutex m_mutex;

};  // end class iluMutexer



//////////////////////////////////////////////////////////////////////
// iluActivationInfo is the information associated with kernel objects
// in a iluObjectActivationTable

class iluActivationInfo {

public:
	
	//////////////////////////////////////////////////////////////////////
	// constructor and destructor
	iluActivationInfo();
	~iluActivationInfo();

	//////////////////////////////////////////////////////////////////////
	// accessors and adjustments

	void iluIncrementInvocations() { m_card_number_ongoing_invocations++; }

	// decrement the number of invocations and signal the condition if zero
	void iluDecrementInvocations();

	iluCardinal iluGetInvocations() { return m_card_number_ongoing_invocations; }

	ILUCPP_BOOL iluGetActive() { return m_b_active; }
	void iluSetActive(ILUCPP_BOOL b_active) { m_b_active = b_active; }


	// deletes the pointed to iluActivationInfo - used in iluObjectActivationTable
	// operations
        static void iluDeleteActivationInfo(ilu_refany pv_an_activation_info) {
            delete ((iluActivationInfo*)pv_an_activation_info);
        }


	// returns when the number of invocations is zero
	void iluWaitForZeroInvocations(iluMutex a_mutex); 

	//////////////////////////////////////////////////////////////////////
	// member variables

	// whether or not the object is currently active - initially ILUCPP_TRUE
	ILUCPP_BOOL m_b_active;

	// how many ilu method calls are currently operating on this object
	// this is either an incoming ilu methods on a true object, or outgoing
	// ilu method calls on a surrogate - initially 1
	iluCardinal m_card_number_ongoing_invocations;

	// condition that is signalled when m_card_number_ongoing_invocations
	// reaches 0, and unsignalled otherwise
	iluCondition m_activation_condition;

};  // end class iluActivationInfo


//////////////////////////////////////////////////////////////////////
// iluObjectActivationTable associates kernel objects with iluActivationInfo
// objects.  It's consulted to determine whether an object is active,
// inactive, or how many calls it's currently involved in (in this language runtime)

class iluObjectActivationTable {
	
public:
	
	// constructor and destuctor
	iluObjectActivationTable();
	~iluObjectActivationTable();

	// the one and only object activation table
	static iluObjectActivationTable* sm_p_object_activation_table;

	// If the object is not in the table, it make a new active entry for it.
	// Marks the object active 
	void iluActivate(iluKernelObject a_kernel_object);

	// waits till the invocations count on the object reaches zero and then
	// marks the object inactive - returns ILUCPP_FALSE if it had to be added to 
	// the table, else ILUCPP_TRUE
	ILUCPP_BOOL iluDeactivate(iluKernelObject a_kernel_object);

	// If the object is not in the table, it make a new active entry for it.
	// If the object is active, increments the invocations count of the object and
	// returns ILUCPP_TRUE, else returns ILUCPP_FALSE;
	ILUCPP_BOOL iluIncrementInvocations(iluKernelObject a_kernel_object);

	// If the object is in the table, decrements the invocations count 
	// of the object and returns ILUCPP_TRUE, else returns ILUCPP_FALSE.  If the count reaches,
	// zero any threads waiting on the condition in the iluActivationInfo associated 
	// with a_kernel_object (i.e. a call to iluDeactivate) are awakened.
	ILUCPP_BOOL iluDecrementInvocations(iluKernelObject a_kernel_object);

	// removes the entry for a_kernel_object from the table - returns
	// ILUCPP_FALSE if a_kernel_object wasn't in the table, else ILUCPP_TRUE;
	ILUCPP_BOOL iluRemove(iluKernelObject a_kernel_object);

	
private:
	
	// hash table used to store the association between kernel objects 
	// and their activation information
	iluHashTable m_activation_hash_table;

	// mutex used to control access to m_hash_table
	iluMutex m_activation_table_mutex;
	
}; // end class iluObjectActivationTable



//////////////////////////////////////////////////////////////////
// Overloading wrappers:
// By putting various types in wrappers, we are able to use the 
// insertion (<<) and extraction (>>) operators on iluBaseCalls.

// Wrappers for certain primitive ilu types that C++ cannot distinguish
// from other primitive types through simple use of a typedef


//////////////////////////////////////////////////////////////////

class iluOptionalWrapper {
public:
	
	iluOptionalWrapper() : m_present(ILUCPP_FALSE) {}
	iluOptionalWrapper(void* pv_something) : m_present(pv_something ? ILUCPP_TRUE : ILUCPP_FALSE) {}

	void iluSetFrom (void* pv_something) {
		m_present = (pv_something ? ILUCPP_TRUE : ILUCPP_FALSE);
	}

	
	ILUCPP_BOOL m_present;
}; // end class iluOptionalWrapper


//////////////////////////////////////////////////////////////////

class iluEnumWrapper {
public:

	iluEnumWrapper(iluDummyEnum& r_enum) : m_r_enum(r_enum) {}

	iluDummyEnum& m_r_enum;
}; // end class iluEnumWrapper


//////////////////////////////////////////////////////////////////

class iluBoolWrapper {
public:

	iluBoolWrapper(ILUCPP_BOOL& r_bool) : m_r_bool(r_bool) {}
	iluBoolWrapper& operator= (ILUCPP_BOOL a_bool) {
		m_r_bool = a_bool; 
		return *this; 
	}
	iluBoolWrapper& operator= (iluBoolean a_bool) {
		m_r_bool = (a_bool ? ILUCPP_TRUE : ILUCPP_FALSE);
		return *this;
	}
	operator iluBoolean() {return (m_r_bool ? iluTRUE : iluFALSE);}

	ILUCPP_BOOL& m_r_bool;
}; // end class iluBoolWrapper


//////////////////////////////////////////////////////////////////

class iluCharacterWrapper {
public:
	
	iluCharacterWrapper(iluShortCardinal& r_shortcardinal) : m_r_shortcardinal(r_shortcardinal) {}
	
	iluShortCardinal& m_r_shortcardinal;
}; // end class iluCharacterWrapper


//////////////////////////////////////////////////////////////////
// Wrappers for certain primitive ilu types that require passing more
// than one argument when having an iluBaseCall (or derived) object
// deal with them.  By putting them in these wrappers, we are able
// to use the insertion (<<) and extraction (>>) operators and
// assist in deletion when needed.

// for surrogate side in, out parameters and return values
#define ILUCPP_DELETE_NEVER			0x00

// for true side in, inout, out parameters and returnvalues
#define ILUCPP_DELETE_ON_DESTRUCT	0x01

// (obsolete) for surrogate side inout parameters (obsolete - used before 
// support for retrys (due to protocol redirect) which forced deletion of 
// inout parameters to be delayed till input, since they may have to
// be sent multiple times
#define ILUCPP_DELETE_ON_OUTPUT		0x02

// for surrogate side inout parameters
#define ILUCPP_DELETE_ON_INPUT		0x04
	

//////////////////////////////////////////////////////////////////

class iluCStringWrapper {
public:
	
	iluCStringWrapper(iluCString& r_cstring, iluCardinal& r_card_length, 
		iluByte b_delete_when = ILUCPP_DELETE_NEVER) 
		: m_r_cstring(r_cstring), m_r_card_length(r_card_length), 
		m_b_delete_when(b_delete_when) {}	

	iluCStringWrapper(const char*& r_cstring, iluCardinal& r_card_length, 
		iluByte b_delete_when = ILUCPP_DELETE_NEVER) 
		: m_r_cstring(CONST_CAST(char*&, r_cstring)), m_r_card_length(r_card_length), 
		m_b_delete_when(b_delete_when) {}	

	~iluCStringWrapper() {
		if (m_b_delete_when & ILUCPP_DELETE_ON_DESTRUCT)
			delete m_r_cstring;
	}

	void iluDeleteIfNeededOnOutput() {
		if (m_b_delete_when & ILUCPP_DELETE_ON_OUTPUT)
			delete m_r_cstring;
	}

	void iluDeleteIfNeededOnInput() {
		if (m_b_delete_when & ILUCPP_DELETE_ON_INPUT)
			delete m_r_cstring;
	}

	iluCString&  m_r_cstring;
	iluCardinal& m_r_card_length; 
	iluByte   m_b_delete_when;

}; // end class iluCStringWrapper


//////////////////////////////////////////////////////////////////

class iluCStringVecWrapper {
public:
	
	iluCStringVecWrapper(iluCString& r_cstring, iluCardinal a_card_length,
		iluByte b_delete_when = ILUCPP_DELETE_NEVER) 
		: m_r_cstringvec(r_cstring), m_card_length(a_card_length),
		m_b_delete_when(b_delete_when) {}
	
	~iluCStringVecWrapper() {
		if (m_b_delete_when & ILUCPP_DELETE_ON_DESTRUCT)
			delete m_r_cstringvec;
	}

	void iluDeleteIfNeededOnOutput() {
		if (m_b_delete_when & ILUCPP_DELETE_ON_OUTPUT)
			delete m_r_cstringvec;
	}

	void iluDeleteIfNeededOnInput() {
		if (m_b_delete_when & ILUCPP_DELETE_ON_INPUT)
			delete m_r_cstringvec;
	}

	iluCString& m_r_cstringvec;
	iluCardinal m_card_length; 
	iluByte  m_b_delete_when;

}; // end class iluCStringVecWrapper


//////////////////////////////////////////////////////////////////

class iluWStringWrapper {
public:
	
	iluWStringWrapper(iluWString& r_wstring, iluCardinal& r_card_length,
		iluByte b_delete_when = ILUCPP_DELETE_NEVER) 
		: m_r_wstring(r_wstring), m_r_card_length(r_card_length),
		m_b_delete_when(b_delete_when) {}

	iluWStringWrapper(const iluCharacter*& r_wstring, iluCardinal& r_card_length,
		iluByte b_delete_when = ILUCPP_DELETE_NEVER) 
		: m_r_wstring(CONST_CAST(iluCharacter*&, r_wstring)), m_r_card_length(r_card_length),
		m_b_delete_when(b_delete_when) {}
	
	~iluWStringWrapper() {
		if (m_b_delete_when & ILUCPP_DELETE_ON_DESTRUCT)
			delete m_r_wstring;
	}

	void iluDeleteIfNeededOnOutput() {
		if (m_b_delete_when & ILUCPP_DELETE_ON_OUTPUT)
			delete m_r_wstring;
	}

	void iluDeleteIfNeededOnInput() {
		if (m_b_delete_when & ILUCPP_DELETE_ON_INPUT)
			delete m_r_wstring;
	}

	iluWString&  m_r_wstring;
	iluCardinal& m_r_card_length; 
	iluByte   m_b_delete_when;
	
}; // end class iluWStringWrapper


//////////////////////////////////////////////////////////////////

class iluWStringVecWrapper {
public:
	
	iluWStringVecWrapper(iluWString& r_wstring, iluCardinal a_card_length,
		iluByte b_delete_when = ILUCPP_DELETE_NEVER) 
		: m_r_wstringvec(r_wstring), m_card_length(a_card_length),
		m_b_delete_when(b_delete_when) {}
	
	~iluWStringVecWrapper() {
		if (m_b_delete_when & ILUCPP_DELETE_ON_DESTRUCT)
			delete m_r_wstringvec;
	}

	void iluDeleteIfNeededOnOutput() {
		if (m_b_delete_when & ILUCPP_DELETE_ON_OUTPUT)
			delete m_r_wstringvec;
	}

	void iluDeleteIfNeededOnInput() {
		if (m_b_delete_when & ILUCPP_DELETE_ON_INPUT)
			delete m_r_wstringvec;
	}

	iluWString& m_r_wstringvec;
	iluCardinal m_card_length; 
	iluByte  m_b_delete_when;
	
}; // end class iluWStringVecWrapper


//////////////////////////////////////////////////////////////////

class iluBytesWrapper {
public:
	
	iluBytesWrapper(unsigned char*& r_pc_buffer, iluCardinal& r_card_length,
		iluByte b_delete_when = ILUCPP_DELETE_NEVER) 
		: m_r_pc_buffer(r_pc_buffer), m_r_card_length(r_card_length),
		m_b_delete_when(b_delete_when) {}

	iluBytesWrapper(const unsigned char*& r_pc_buffer, iluCardinal& r_card_length,
		iluByte b_delete_when = ILUCPP_DELETE_NEVER) 
		: m_r_pc_buffer(CONST_CAST(unsigned char*&, r_pc_buffer)), m_r_card_length(r_card_length),
		m_b_delete_when(b_delete_when) {}

	~iluBytesWrapper() {
		if (m_b_delete_when & ILUCPP_DELETE_ON_DESTRUCT)
			delete m_r_pc_buffer;
	}

	void iluDeleteIfNeededOnOutput() {
		if (m_b_delete_when & ILUCPP_DELETE_ON_OUTPUT)
			delete m_r_pc_buffer;
	}

	void iluDeleteIfNeededOnInput() {
		if (m_b_delete_when & ILUCPP_DELETE_ON_INPUT)
			delete m_r_pc_buffer;
	}

	unsigned char*& m_r_pc_buffer;
	iluCardinal&    m_r_card_length; 
	iluByte      m_b_delete_when;
}; // end class iluBytesWrapper


//////////////////////////////////////////////////////////////////

class iluOpaqueWrapper {
public:
	
	iluOpaqueWrapper(unsigned char*& r_pc_buffer, iluCardinal a_card_length,
		iluByte b_delete_when = ILUCPP_DELETE_NEVER) 
		: m_r_pc_buffer(r_pc_buffer), m_card_length(a_card_length),
		m_b_delete_when(b_delete_when) {}

	iluOpaqueWrapper(const unsigned char*& r_pc_buffer, iluCardinal a_card_length,
		iluByte b_delete_when = ILUCPP_DELETE_NEVER) 
		: m_r_pc_buffer(CONST_CAST(unsigned char*&, r_pc_buffer)), m_card_length(a_card_length),
		m_b_delete_when(b_delete_when) {}

	~iluOpaqueWrapper() {
		if (m_b_delete_when & ILUCPP_DELETE_ON_DESTRUCT)
			delete m_r_pc_buffer;
	}

	void iluDeleteIfNeededOnOutput() {
		if (m_b_delete_when & ILUCPP_DELETE_ON_OUTPUT)
			delete m_r_pc_buffer;
	}

	void iluDeleteIfNeededOnInput() {
		if (m_b_delete_when & ILUCPP_DELETE_ON_INPUT)
			delete m_r_pc_buffer;
	}

	unsigned char*& m_r_pc_buffer;
	iluCardinal    m_card_length; 
	iluByte     m_b_delete_when;
}; // end class iluOpaqueWrapper


//////////////////////////////////////////////////////////////////

ILU_RUNTIME_PUBLIC_CLASS iluObjectWrapper {
public:
	
	// constructors used to build a wrapper for output
	iluObjectWrapper(iluObject& a_object, ILUCPP_BOOL b_discriminator) 
		: m_kernelobject(a_object.iluGetKernelObject()), 
		m_pv_iluobject(&a_object), 
		m_b_discriminator(b_discriminator), 
		m_class(a_object.iluGetClassRecord()), 
		m_b_incremented_activation(ILUCPP_FALSE),
		m_b_do_refcount_decrement(ILUCPP_FALSE) {}

	iluObjectWrapper(iluObject* p_object, ILUCPP_BOOL b_discriminator, iluClass a_class = NULL) 
		: m_kernelobject((p_object ? p_object->iluGetKernelObject() : ((iluKernelObject) NULL))), 
		m_pv_iluobject(p_object), 
		m_b_discriminator(b_discriminator), 
		m_class((a_class ? a_class : p_object->iluGetClassRecord())), 
		m_b_incremented_activation(ILUCPP_FALSE),
		m_b_do_refcount_decrement(ILUCPP_FALSE) {}
	
	// constructor used to build a wrapper for input
	iluObjectWrapper(ILUCPP_BOOL b_discriminator, iluClass a_class) 
		: m_kernelobject(NULL), 
		m_pv_iluobject(NULL), 
		m_b_discriminator(b_discriminator), 
		m_class(a_class), 
		m_b_incremented_activation(ILUCPP_FALSE), 
		m_b_do_refcount_decrement(ILUCPP_FALSE)  {}	


	// default constructor
	iluObjectWrapper() 
		: m_kernelobject(NULL), 
		m_pv_iluobject(NULL), 
		m_b_discriminator(ILUCPP_FALSE), 
		m_class(NULL), 
		m_b_incremented_activation(ILUCPP_FALSE), 
		m_b_do_refcount_decrement(ILUCPP_FALSE)  {}	

	// recalculates attributes based on the (void*) object pointer - used for true side
	// inout situations
	void iluReviseForObject(iluObject* what_m_pv_iluobject_now_is);

	// destructor takes care of decrementing activation and reference counting if needed
	~iluObjectWrapper();

	iluKernelObject m_kernelobject;			// kernel object for this object
	void* m_pv_iluobject;					// pointer to iluObject that gets downcast to actual object
	ILUCPP_BOOL m_b_discriminator;			// if it's a discriminator
	iluClass m_class;						// it's class
	ILUCPP_BOOL m_b_incremented_activation;	// whether the activation count was incremented for this object
	ILUCPP_BOOL m_b_do_refcount_decrement;	// whether iluDecrementReferenceCount should be called on destruction


}; // end class iluObjectWrapper


//////////////////////////////////////////////////////////////////

class iluSequenceWrapper {
public:
	
	iluSequenceWrapper(const iluCardinal& r_card_count) : m_p_card_count((iluCardinal*) &r_card_count) {}
	
	iluCardinal* m_p_card_count;
}; // end class iluSequenceWrapper


//////////////////////////////////////////////////////////////////

class iluUnionWrapper {
public:
	
	iluUnionWrapper(iluCardinal card_discriminator, iluTypeKind a_typekind) 
		: m_card_discriminator(card_discriminator), m_typekind(a_typekind) {}
		
	iluCardinal m_card_discriminator;
	iluTypeKind m_typekind;
}; // end class iluUnionWrapper



//////////////////////////////////////////////////////////////////
// states that a iluTrueCall or iluSurrogateCall goes through

typedef enum _ilu_cpp_call_state { 
	ilucall_initial,					// just constructed

	ilucall_request_read,				// finished reading in the request, true side
	ilucall_sending_reply,				// sizing/sending reply parameters, true side
	ilucall_sending_exception,			// sizing/sending exception parameters, true side
	ilucall_sending_request,			// sending request arguments, surrogate side
	ilucall_getting_reply				// get reply called, reading in results/exception, surrogate side
} iluCallState;

 
//////////////////////////////////////////////////////////////////
// iluBaseCall is a common abstract base class for iluTrueCall and iluSurrogateCall.
// It encapsulates an iluCall to provide:  Automatic call of FinishCall
// in its destructor to make flow of control easier to deal with (since
// if a problem occurs in one of its methods, throwing an exception,
// we're guaranteed to have it's destructor run) ; It hangs on to the
// iluCall so we don't have to pass it as an argument to every member 
// function call.


ILU_RUNTIME_PUBLIC_CLASS iluBaseCall {

public:

	// constructor
	iluBaseCall(iluCall a_call, ILUCPP_BOOL b_can_raise_exceptions = ILUCPP_FALSE);

	// overloads to run manipulators
	iluBaseCall& operator<<(iluBaseCall& (* pfunction_manipulator)(iluBaseCall& r_basecall)) {
		(*pfunction_manipulator)(*this); return *this; }
	iluBaseCall& operator>>(iluBaseCall& (* pfunction_manipulator)(iluBaseCall& r_basecall)) {
		(*pfunction_manipulator)(*this); return *this; }
	iluBaseCall& operator+=(iluBaseCall& (* pfunction_manipulator)(iluBaseCall& r_basecall)) {
		(*pfunction_manipulator)(*this); return *this; }

	// input primitives
	iluBaseCall& operator>>(iluByte& r_byte);
	iluBaseCall& operator>>(iluBoolWrapper& r_boolean_wrapper);
	iluBaseCall& operator>>(iluOptionalWrapper& r_optional_wrapper);
	iluBaseCall& operator>>(iluCardinal& r_cardinal);
	iluBaseCall& operator>>(iluCharacterWrapper& r_character_wrapper);
	iluBaseCall& operator>>(iluShortCharacter& r_shortcharacter);
	iluBaseCall& operator>>(iluEnumWrapper& r_enum);
	iluBaseCall& operator>>(iluInteger& r_integer);
	iluBaseCall& operator>>(iluReal& r_real);
	iluBaseCall& operator>>(iluShortCardinal& r_shortcardinal);
	iluBaseCall& operator>>(iluShortInteger& r_shortinteger);
	iluBaseCall& operator>>(iluShortReal& r_shortreal);
	iluBaseCall& operator>>(iluLongCardinal& r_long_cardinal);
	iluBaseCall& operator>>(iluLongInteger& r_longinteger);
	iluBaseCall& operator>>(iluLongReal& r_longreal);
	iluBaseCall& operator>>(iluCStringWrapper& r_cstring_wrapper);
	iluBaseCall& operator>>(iluCStringVecWrapper& r_cstringvec_wrapper);
	iluBaseCall& operator>>(iluWStringWrapper& r_wstring_wrapper);
	iluBaseCall& operator>>(iluWStringVecWrapper& r_wstringvec_wrapper);
	iluBaseCall& operator>>(iluBytesWrapper& r_bytes_wrapper);
	iluBaseCall& operator>>(iluOpaqueWrapper& r_opaque_wrapper);
	iluBaseCall& operator>>(iluObjectWrapper& r_object_wrapper);
	iluBaseCall& operator>>(iluSequenceWrapper& r_sequence_wrapper);
	iluBaseCall& operator>>(iluUnionWrapper& r_union_wrapper);

	// output primitives
	// Note: perhaps the output array should be wrapped somehow?
	iluBaseCall& iluOutputArray (iluCardinal card_array_length);	
	iluBaseCall& operator<<(iluByte a_byte);
	iluBaseCall& operator<<(iluBoolWrapper& r_boolean_wrapper);
	iluBaseCall& operator<<(iluOptionalWrapper& r_optional_wrapper);
	iluBaseCall& operator<<(iluCardinal a_cardinal);
	iluBaseCall& operator<<(iluCharacterWrapper& r_character_wrapper);
	iluBaseCall& operator<<(iluShortCharacter a_shortcharacter);
	iluBaseCall& operator<<(iluEnumWrapper& r_enum);
	iluBaseCall& operator<<(iluInteger a_integer);
	iluBaseCall& operator<<(iluReal a_real);
	iluBaseCall& operator<<(iluShortCardinal a_shortcardinal);
	iluBaseCall& operator<<(iluShortInteger a_shortinteger);
	iluBaseCall& operator<<(iluShortReal a_shortreal);
	iluBaseCall& operator<<(iluLongCardinal& r_long_cardinal);
	iluBaseCall& operator<<(iluLongInteger& r_longinteger);
	iluBaseCall& operator<<(iluLongReal& r_longreal);
	iluBaseCall& operator<<(iluCStringWrapper& r_cstring_wrapper);
	iluBaseCall& operator<<(iluCStringVecWrapper& r_cstringvec_wrapper);
	iluBaseCall& operator<<(iluWStringWrapper& r_wstring_wrapper);
	iluBaseCall& operator<<(iluWStringVecWrapper& r_wstringvec_wrapper);
	iluBaseCall& operator<<(iluBytesWrapper& r_bytes_wrapper);
	iluBaseCall& operator<<(iluOpaqueWrapper& r_opaque_wrapper);
	iluBaseCall& operator<<(iluObjectWrapper& r_object_wrapper);
	iluBaseCall& operator<<(iluSequenceWrapper& r_sequence_wrapper);
	iluBaseCall& operator<<(iluUnionWrapper& r_union_wrapper);

	// sizing primitives
	ILUCPP_BOOL   iluNeedsSizing();  // returns ILUCPP_TRUE if the call needs sizing operations
	// Note: perhaps the sized array should be wrapped somehow?
	iluBaseCall& iluSizeArray (iluCardinal card_array_length);	
	iluBaseCall& operator+=(iluByte a_byte);
	iluBaseCall& operator+=(iluBoolWrapper& r_boolean_wrapper);
	iluBaseCall& operator+=(iluOptionalWrapper& r_optional_wrapper);
	iluBaseCall& operator+=(iluCardinal a_cardinal);
	iluBaseCall& operator+=(iluCharacterWrapper& r_character_wrapper);
	iluBaseCall& operator+=(iluShortCharacter a_shortcharacter);
	iluBaseCall& operator+=(iluEnumWrapper& r_enum);
	iluBaseCall& operator+=(iluInteger a_integer);
	iluBaseCall& operator+=(iluReal a_real);
	iluBaseCall& operator+=(iluShortCardinal a_shortcardinal);
	iluBaseCall& operator+=(iluShortInteger a_shortinteger);
	iluBaseCall& operator+=(iluShortReal a_shortreal);
	iluBaseCall& operator+=(iluLongCardinal& r_long_cardinal);
	iluBaseCall& operator+=(iluLongInteger& r_longinteger);
	iluBaseCall& operator+=(iluLongReal& r_longreal);
	iluBaseCall& operator+=(iluCStringWrapper& r_cstring_wrapper);
	iluBaseCall& operator+=(iluCStringVecWrapper& r_cstringvec_wrapper);
	iluBaseCall& operator+=(iluWStringWrapper& r_wstring_wrapper);
	iluBaseCall& operator+=(iluWStringVecWrapper& r_wstringvec_wrapper);
	iluBaseCall& operator+=(iluBytesWrapper& r_bytes_wrapper);
	iluBaseCall& operator+=(iluOpaqueWrapper& r_opaque_wrapper);
	iluBaseCall& operator+=(iluObjectWrapper& r_object_wrapper);
	iluBaseCall& operator+=(iluObject* p_iluobject);
	iluBaseCall& operator+=(iluSequenceWrapper& r_sequence_wrapper);
	iluBaseCall& operator+=(iluUnionWrapper& r_union_wrapper);


	//////////////////////////////////////////////////////////////////
	// Mode related

	// Used immediately after entering of reply mode - returns ILUCPP_TRUE if an
	// exception is present, and sets r_card_exception_number to 1 +
	// the index into method's exception vector  
	ILUCPP_BOOL iluExceptionPresent(iluCardinal& r_card_exception_number);

	// begin sizing exception
	iluBaseCall& iluSizeExceptionMode (iluCardinal card_exception_number);


	//////////////////////////////////////////////////////////////////
	// accessors

	iluCallState iluGetState() { return m_state; }

	void iluSetState(iluCallState new_state) { m_state = new_state; }

	iluCall iluGetCall() { return m_call; }

	iluCardinal iluGetSize() { return m_card_size; }

	iluCardinal iluGetExceptionNumber() { return m_card_exception_number; }

	void iluSetExceptionNumber(iluCardinal new_exception_number) { 
		m_card_exception_number = new_exception_number; }

	ILUCPP_BOOL iluCanRaiseExceptions() { return m_b_exceptions;}

	// the catch in stubs can use iluSetErrorType so that the
	// call destructor can know what kind of error state might have occurred
	void iluSetErrorType(iluErrorType an_error_type) {m_error_type = an_error_type;}

	// adds the card_to_be_added to the m_card_size member
	void iluAddToSize(iluCardinal card_to_be_added) { m_card_size += card_to_be_added; }

	// returns whether or not the call should be retried (due to a protocol redirect), 
	// and sets the internal retry variable to false;
	ILUCPP_BOOL iluShouldRetryCall() {
		ILUCPP_BOOL b_retry_call = m_b_retry_call; 
		m_b_retry_call = ILUCPP_FALSE; 
		return b_retry_call;
	}

	// sets the retry call member to true
	void iluSetRetryCall() { m_b_retry_call = ILUCPP_TRUE; }


	//////////////////////////////////////////////////////////////////
	// so the overloaded stream operators in iluBaseCall can tell if they
	// are actually operating on a iluTrueCall or iluSurrogateCall

	virtual ILUCPP_BOOL iluIsTrueCall() = 0;
	virtual ILUCPP_BOOL iluIsSurrogateCall() = 0;


protected:

	// the iluCall that this object is for
	iluCall m_call;

	// state we're in
	iluCallState m_state;

	// used to size arguments and replies
	iluCardinal m_card_size;

	// when there's an exception, we save the exception number here
	iluCardinal m_card_exception_number;

	// whether or not exceptions can be generated from the method being used on this call
	ILUCPP_BOOL m_b_exceptions;

	// initially 0, it's set true if ilu_GetReply indicated that we should retry the call
	// (due to a protocol redirect)
	ILUCPP_BOOL m_b_retry_call;

	// holds the type of ilu error (if any) that may have occurred during the call
	iluErrorType m_error_type;

}; // end class iluBaseCall


//////////////////////////////////////////////////////////////////
// Manipulators

ILU_RUNTIME_PUBLIC iluBaseCall& iluEndSequence (iluBaseCall& r_basecall);
ILU_RUNTIME_PUBLIC iluBaseCall& iluEndUnion (iluBaseCall& r_basecall);

ILU_RUNTIME_PUBLIC iluBaseCall& iluInputArray (iluBaseCall& r_basecall);
ILU_RUNTIME_PUBLIC iluBaseCall& iluEndArray (iluBaseCall& r_basecall);

ILU_RUNTIME_PUBLIC iluBaseCall& iluSizeRecord (iluBaseCall& r_basecall);
ILU_RUNTIME_PUBLIC iluBaseCall& iluOutputRecord (iluBaseCall& r_basecall);
ILU_RUNTIME_PUBLIC iluBaseCall& iluInputRecord (iluBaseCall& r_basecall);
ILU_RUNTIME_PUBLIC iluBaseCall& iluEndRecord (iluBaseCall& r_basecall);

ILU_RUNTIME_PUBLIC iluBaseCall& iluRequestMode (iluBaseCall& r_basecall);
ILU_RUNTIME_PUBLIC iluBaseCall& iluRequestSentMode (iluBaseCall& r_basecall); 
ILU_RUNTIME_PUBLIC iluBaseCall& iluGetReplyMode (iluBaseCall& r_basecall);
ILU_RUNTIME_PUBLIC iluBaseCall& iluParametersFinishedMode (iluBaseCall& r_basecall);
ILU_RUNTIME_PUBLIC iluBaseCall& iluSizeReplyMode (iluBaseCall& r_basecall);
ILU_RUNTIME_PUBLIC iluBaseCall& iluSendReplyMode (iluBaseCall& r_basecall);
ILU_RUNTIME_PUBLIC iluBaseCall& iluSendExceptionMode (iluBaseCall& r_basecall);


//////////////////////////////////////////////////////////////////
// iluTrueCall is meant for use by true stubs only. It encapsulates 
// some of the standard control operations for incoming ILU calls


ILU_RUNTIME_PUBLIC_CLASS iluTrueCall : public iluBaseCall {

public:

	// Constructor and destructor
	iluTrueCall(iluCall a_call, ILUCPP_BOOL b_can_raise_exceptions);
	virtual ~iluTrueCall();

	virtual ILUCPP_BOOL iluIsTrueCall() {return ILUCPP_TRUE;}
	virtual ILUCPP_BOOL iluIsSurrogateCall() {return ILUCPP_FALSE;}

protected:

	// the passport (if any) associated with this call
	iluPassport* m_p_passport;


}; // end class iluTrueCall


//////////////////////////////////////////////////////////////////
// iluSurrogateCall is meant for use by surrogate stubs only.  It 
// encapsulates some of the standard control operations for outgoing 
// ILU calls


ILU_RUNTIME_PUBLIC_CLASS iluSurrogateCall : public iluBaseCall {

public:

	// Constructor and destructor
	iluSurrogateCall(iluCall a_call, iluCardinal card_method_number, iluObject* p_discriminator_object, iluClass p_class);
	virtual ~iluSurrogateCall();

	virtual ILUCPP_BOOL iluIsTrueCall() {return ILUCPP_FALSE;}
	virtual ILUCPP_BOOL iluIsSurrogateCall() {return ILUCPP_TRUE;}

private:

	// whether or not a call was started - if so, ilu_FinishCall must
	// eventually be called, else it should not be called.
	ILUCPP_BOOL m_b_call_started;

}; // end class iluSurrogateCall


#ifdef ADD_VARIANT_SUPPORT

//////////////////////////////////////////////////////////////////
// iluPickleCall is meant for use by Corba any insertion and extraction
// only


ILU_RUNTIME_PUBLIC_CLASS iluPickleCall : public iluBaseCall {
	
public:
	
	// Constructor
	iluPickleCall(iluCall a_call, void* p_typecode, iluPickle* p_pickle);
	
	virtual ILUCPP_BOOL iluIsTrueCall() {return ILUCPP_FALSE;}
	virtual ILUCPP_BOOL iluIsSurrogateCall() {return ILUCPP_FALSE;}
	
	iluPickleCall& iluOutputPickle ();
	iluPickleCall& iluInputPickle ();
	iluPickleCall& iluEndPickle ();
	
private:
	
	// pointer to the typecode object for the type we'll be pickling
	void* m_p_typecode;
	
	// pointer to the pickle we'll be operating on
	iluPickle* m_p_pickle;
	
}; // end class iluPickleCall



#endif /* ADD_VARIANT_SUPPORT */


//////////////////////////////////////////////////////////////////
// End of File
//////////////////////////////////////////////////////////////////


#endif				/* ifndef __ilucpp_H_ */
