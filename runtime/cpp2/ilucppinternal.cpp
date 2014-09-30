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
/* $Id: ilucppinternal.cpp,v 1.38 1999/08/05 03:50:37 janssen Exp $ */


// include ILU C++ header file
#include "corba.hpp"

// for sprintf
#include <stdio.h>


//////////////////////////////////////////////////////////////////
// some development - debugging support

#ifdef ENABLE_DEBUGGING

#ifdef ILU_OS_THREADED
#include <threads.h>
#endif

char* iluCppTagFormatString (char* pc_tagged_format_string, const char* pc_formatstring_to_tag) {
	
#ifdef ILU_OS_THREADED
	sprintf(pc_tagged_format_string, "ILUC++: Thread 0x%x, %s", GET_CURRENT_THREAD(), pc_formatstring_to_tag);
#else
	sprintf(pc_tagged_format_string, "ILUC++: %s", pc_formatstring_to_tag);
#endif
	return pc_tagged_format_string;
}

#endif // ENABLE_DEBUGGING

const char *iluCppMinorErrorDescription (ilu_ErrorType et, unsigned long minor)
{
  const char *d = ilu_GetMinorDescrFromCodes(et, minor);
  return ((d == NULL) ? "<no description for minor code>" : d);
}


//////////////////////////////////////////////////////////////////////
// iluMutexer is a helper class used to ensure that mutexes are released when
// functions are exited.  The constructor grabs the mutex - the destructor 
// releases it

iluMutexer::iluMutexer(iluMutex a_mutex) : m_mutex(a_mutex) {
	iluError an_error;
	ILU_CLER(an_error);
	
	// get exclusive access
	ilu_EnterMutex(m_mutex, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);		
}



iluMutexer::~iluMutexer() {
	iluError an_error;
	ILU_CLER(an_error);
	
	// release exclusive access
	ilu_ExitMutex(m_mutex, iluFALSE, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);		
}



//////////////////////////////////////////////////////////////////
// iluCppInternal - Abstract class that provides various calls that are specificly 
// meant only for use by the C++ runtime or stubs.  Not meant to ever be subclassed.






//////////////////////////////////////////////////////////////////
// a struct used to pass a call and method to a thread when we
// have a call on a connection that should fork a thread per request

typedef void (*_stubproc)(iluCall);

typedef struct {
	iluCall	m_call;
	_stubproc m_pfunction_method_stubproc;
} iluThreadPerRequestInvocationArgument;



//////////////////////////////////////////////////////////////////
// globals and statics

// mutex used for object type operations
ilu_Mutex iluCppInternal::sm_object_type_mutex;

// identifies the C++ language runtime
iluCardinal iluCppInternal::sm_card_cpp_language_index;

// set to ILUCPP_TRUE if we're running threaded
ILUCPP_BOOL iluCppInternal::sm_b_running_threaded;

// points to list of surrogate creator nodes used when we need to 
// create a surrogate of a particular class
iluSurrogateCreatorNode* iluCppInternal::sm_p_surrogate_creator_list;

// mutex for surrogate creator list
iluMutex iluCppInternal::sm_surrogate_creator_list_mutex;

// all the interfaces should place their initialization function on this list
// this way, all loaded interfaces can be initialized simply with one call to 
// iluCppRuntime::iluInitialize
iluInitializationFunctionNode* iluCppInternal::sm_p_initialization_function_list;

// true iff the gc notifier has already been set up
ILUCPP_BOOL iluCppInternal::sm_gc_noter_set = ILUCPP_FALSE;

// when running non-native threaded, this should be set to 
// the function that will return a thread unique iluCardinal id of the current thread
iluNonNativeThreadIDFunction iluCppInternal::sm_card_thread_id_function = NULL;


// points to the function used to fork a thread
#ifdef ILU_OS_THREADED
iluForkProc iluCppInternal::sm_pfunction_forking_procedure = REINTERPRET_CAST(iluForkProc, ilu_OSForkNewThread);
#else
iluForkProc iluCppInternal::sm_pfunction_forking_procedure;
#endif

//////////////////////////////////////////////////////////////////
// Garbage Collection

// Called by (surrogate) stubs produced for Collectible objects to ensure that a callback
// object is created to be the target of pings from true objects.    It is 
// created in its own server.

void iluCppInternal::iluEnsureGCCallback() {
	iluGarbageCollectorCallback::iluInitialize();
}



// used to get around some compilers complaining about passing a c++
// function where a c function is expected.
extern "C" {
  typedef iluBoolean(*C_iluGCNoter)(iluKernelObject, int);
}



// Called by (true) stubs produced for Collectible objects to ensure that a 
// a client's interest in a collectible object can effect whether the true object is
// kept in memory (has a ref count on it)

void iluCppInternal::iluEnsureGCNoter() {
	if (sm_gc_noter_set)
		return;
	ilu_SetNoter(REINTERPRET_CAST(C_iluGCNoter, iluGCNoter), sm_card_cpp_language_index);
	sm_gc_noter_set = ILUCPP_TRUE;
}


// the function that is called by the kernel when a Collectible object has
// or ceases to have clients
iluBoolean iluCppInternal::iluGCNoter(iluKernelObject a_kernel_object, int i_interested) {
	
	if ((!a_kernel_object) ||				// ignore if we really don't have a kernel object
		(!ilu_TrueInstanceP(a_kernel_object)) || // or it's a not a collectible true object
		(!ilu_CollectibleP(ilu_ClassOfObject(a_kernel_object))))
		return iluTRUE;
	
	iluObject* p_ilu_object;
	
	// get the language specific object corresponding to this kernel object
	p_ilu_object = (iluObject*)ilu_GetLanguageSpecificObject(a_kernel_object,
		sm_card_cpp_language_index);
	
	if (!p_ilu_object) {
		ILUCPP_DEBUG5("iluCppInternal::iluGCNoter - no lso for %s kernel object %s in server %s, interest = %i\n", 
			(ilu_ClassOfObject(a_kernel_object))->cl_name,
			ilu_IhOfObject(a_kernel_object), ilu_IDOfServer(ilu_ServerOfObject(a_kernel_object)), i_interested);
		return iluTRUE;
	}
	
	if (i_interested) {
		ILUCPP_DEBUG3("iluCppInternal::iluGCNoter - kernel is interested in %s object %s\n", p_ilu_object->iluClassName(), p_ilu_object->iluId());
		p_ilu_object->iluIncrementReferenceCount();
	}
	else {
		ILUCPP_DEBUG3("iluCppInternal::iluGCNoter - kernel is disinterested in %s object %s\n", p_ilu_object->iluClassName(), p_ilu_object->iluId());
		// we cant just do a regular dec ref here here because if refcount reaches 0, the object destructs, which would 
		// cause lock problems as well as violate the constraints outlined in the iluxport.h's
		// documentation on ilu_ObjectNoter.  So we do a deferred dec ref scheduled 5 seconds from now
		p_ilu_object->iluScheduledDecrementReferenceCount(5, 0);
	}

	return iluTRUE;
}



//////////////////////////////////////////////////////////////////
// Type definition related


//////////////////////////////////////////////////////////////////
// Grab the mutex that's used to ensure exclusive access for 
// calls involving object type definitions

void iluCppInternal::iluEnterObjectTypeMutex() {
	
	iluError an_error;	// Note: seems we need this initialization
	ILU_CLER(an_error);

	ilu_EnterMutex(sm_object_type_mutex, &an_error);
	
	ILUCPP_DEBUG1("iluCppInternal::iluEnterObjectTypeMutex\n"); 
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
}


//////////////////////////////////////////////////////////////////
// Release the mutex that's used to ensure exclusive access for 
// calls involving object type definitions

void iluCppInternal::iluExitObjectTypeMutex() {
	
	iluError an_error;	// Note: seems we need this initialization
	ILU_CLER(an_error);

	ilu_ExitMutex(sm_object_type_mutex, iluFALSE, &an_error);
	
	ILUCPP_DEBUG1("iluCppInternal::iluExitObjectTypeMutex\n"); 
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
}



//////////////////////////////////////////////////////////////////
// adds the named exception to the (internal) table of exceptions for the named interface

ilu_Exception iluCppInternal::iluDefineException(iluCString pc_interface, 
												 iluCString pc_exception,
												 iluCString pc_type_id_of_exception_value) {
	
	ILU_ERRS((internal, no_memory))  an_error;
	iluException   exception_to_return;
	
	exception_to_return = ilu_DefineException(pc_interface, pc_exception, pc_type_id_of_exception_value, &an_error);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	ILUCPP_DEBUG3("iluCppInternal::iluDefineException - %s :: %s defined\n", 
	       pc_interface, pc_exception );
	
	return exception_to_return;
}




//////////////////////////////////////////////////////////////////
// adds the named object type to the system

iluClass iluCppInternal::iluDefineObjectType(
											 iluCString pc_classname,
											 iluCString pc_brand,
											 iluCString pc_type_id,
											 iluCString pc_singleton, // protocol info if class is a singleton, else NULL
											 ILUCPP_BOOL b_optional,
											 ILUCPP_BOOL b_collectible,
											 iluCString pc_docstring,
											 iluCardinal card_number_of_methods,
											 iluCardinal card_number_of_superclasses,	
											 iluCString* ppc_array_of_superclass_ids) {
	
	ILU_ERRS((internal, no_memory))  an_error;
	iluClass       class_to_return;
	
	class_to_return = ilu_DefineObjectType(
		pc_classname, 
		pc_brand, 
		pc_type_id,
		pc_singleton, 
		(b_optional ? iluTRUE : iluFALSE), 
		(b_collectible ? iluTRUE : iluFALSE), 
		pc_docstring,
		card_number_of_methods, 
		card_number_of_superclasses,
		ppc_array_of_superclass_ids, 
		&an_error);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	ILUCPP_DEBUG2("iluCppInternal::iluDefineObjectType - %s defined\n", pc_classname);
	
	return class_to_return;
}


//////////////////////////////////////////////////////////////////
// Defines the i'th method of the specified class 

iluMethod iluCppInternal::iluDefineMethod(iluClass class_method_is_part_of,
									 iluCardinal card_ith_method,
									 iluCString pc_method_name,
									 iluCardinal card_method_id,
									 ILUCPP_BOOL b_cacheable,
									 ILUCPP_BOOL b_asynchronous,
									 iluCardinal card_number_of_exceptions_it_can_raise,
									 iluException* ppc_array_of_exception_names,
									 iluCardinal card_number_of_arguments,
									 iluCString pc_return_type_id) {
	
	ILU_ERRS((internal, no_memory))  an_error;
	iluMethod return_method;

	return_method = ilu_DefineMethod(
		class_method_is_part_of, 
		card_ith_method, 
		pc_method_name, 
		card_method_id, 
		(b_cacheable ? iluTRUE : iluFALSE),
		(b_asynchronous ? iluTRUE : iluFALSE), 
		card_number_of_exceptions_it_can_raise,
		ppc_array_of_exception_names, 
		card_number_of_arguments,
		pc_return_type_id,
		&an_error);
	
	ILUCPP_DEBUG3("iluCppInternal::iluDefineMethod - %s :: %s defined\n", 
	       class_method_is_part_of->cl_name, pc_method_name);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);

	return return_method;
}


//////////////////////////////////////////////////////////////////
// Defines the index'th arg of method m.

ILUCPP_BOOL iluCppInternal::iluDefineMethodArg(iluMethod method,
											  iluCardinal card_index,
											  iluCString pc_argname,
											  ILUCPP_BOOL	b_sibling,
											  iluArgDirection e_direction,
											  iluCString pc_typeid) {
	
	ILU_ERRS((internal, no_memory))  an_error;
	ILUCPP_BOOL b_return;
	
	b_return = (ilu_DefineMethodArg(method,
		card_index,
		pc_argname,
		(b_sibling ? iluTRUE : iluFALSE),
		e_direction,
		pc_typeid, 
		&an_error) ? ILUCPP_TRUE : ILUCPP_FALSE);
	
	ILUCPP_DEBUG2("iluCppInternal::iluDefineMethodArg - %s\n", pc_argname);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return b_return;
}



//////////////////////////////////////////////////////////////////
// register a new sequence type 

iluType iluCppInternal::iluRegisterSequenceType (
												 iluCString pc_name,			// name 
												 iluCString pc_interface_name,	// interface-name 
												 iluCString pc_interface_brand,	// interface-brand, OPTIONAL 
												 iluCString pc_uid,				// UID 
												 iluTypeUID pc_type_uid,		// base type 
												 iluCardinal card_limit,		// limit (0 if none) 
												 ILUCPP_BOOL * p_b_new			// OUT:  true if new registration 	
												 ) {
	iluError an_error;
	iluType return_value;
	iluBoolean b_localnew;
	
	return_value = ilu_RegisterSequenceType (pc_name, pc_interface_name, pc_interface_brand,
		pc_uid, pc_type_uid, card_limit, &b_localnew, &an_error);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	*p_b_new = (b_localnew ? ILUCPP_TRUE : ILUCPP_FALSE);
	
	return return_value;
}


//////////////////////////////////////////////////////////////////
// Register a new array type 

iluType iluCppInternal::iluRegisterArrayType (
											  iluCString pc_name,				// name 
											  iluCString pc_interface_name,		// interface-name 
											  iluCString pc_interface_brand,	// interface-brand, OPTIONAL 
											  iluCString pc_uid,				// UID 
											  iluTypeUID pc_type_uid,			// base type 
											  iluCardinal card_num_dimensions,	// number of dimensions 
											  iluCardinal * p_card_dim_seq,		// pointer to sequence of dimensions 
											  ILUCPP_BOOL * p_b_new				// OUT:  true if new registration 	
											  ) {
	iluError an_error;
	iluType return_value;
	iluBoolean b_localnew;
	
	return_value = ilu_RegisterArrayType (pc_name, pc_interface_name, pc_interface_brand,
		pc_uid, pc_type_uid, card_num_dimensions, p_card_dim_seq, &b_localnew, &an_error);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	*p_b_new = (b_localnew ? ILUCPP_TRUE : ILUCPP_FALSE);
	
	return return_value;
}


//////////////////////////////////////////////////////////////////
// call this function to begin registering a new record type 

iluType iluCppInternal::iluRegisterRecordType (
											   iluCString pc_name,				// name 
											   iluCString pc_interface_name,	// interface-name 
											   iluCString pc_interface_brand,	// interface-brand, OPTIONAL 
											   iluCString pc_uid,				// UID 
											   iluCardinal card_num_fields,		// number of fields 
											   ILUCPP_BOOL * p_b_new			// OUT:  true if new registration 	
											   ) {
	iluError an_error;
	iluType return_value;
	iluBoolean b_localnew;
	
	return_value = ilu_RegisterRecordType (pc_name, pc_interface_name, pc_interface_brand,
		pc_uid, card_num_fields, iluFALSE, ILU_NIL, &b_localnew, &an_error);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	*p_b_new = (b_localnew ? ILUCPP_TRUE : ILUCPP_FALSE);
	
	return return_value;
}


//////////////////////////////////////////////////////////////////
// call this function repeatedly to fill in the fields of a newly registered record type

ILUCPP_BOOL iluCppInternal::iluRegisterRecordField (
													iluType the_type,				// record type 
													iluCardinal card_field_index,	// field index 
													iluCString pc_field_name,		// field name 
													iluTypeUID pc_type_uid			// field type 
													) {
	iluError an_error;
	iluBoolean b_return;
	
	b_return = ilu_RegisterRecordField(the_type, card_field_index, 
		pc_field_name, pc_type_uid, &an_error);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return (b_return ? ILUCPP_TRUE : ILUCPP_FALSE);
}


//////////////////////////////////////////////////////////////////
// call this function to begin registering a new enumeration type 

iluType iluCppInternal::iluRegisterEnumerationType (
													iluCString pc_name,				// name 
													iluCString pc_interface_name,	// interface-name 
													iluCString pc_interface_brand,	// interface-brand, OPTIONAL 
													iluCString pc_uid,				// UID 
													iluCardinal card_num_elements,	// number of elements 
													ILUCPP_BOOL * p_b_new			// OUT:  true if new registration 	
													) {
	iluError an_error;
	iluType return_value;
	iluBoolean b_localnew;
	
	return_value = ilu_RegisterEnumerationType (pc_name, pc_interface_name, pc_interface_brand,
		pc_uid, card_num_elements, &b_localnew, &an_error);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	*p_b_new = (b_localnew ? ILUCPP_TRUE : ILUCPP_FALSE);
	
	return return_value;
}


//////////////////////////////////////////////////////////////////
// call this function repeatedly to fill in the fields of a newly registered enum type 

ILUCPP_BOOL iluCppInternal::iluRegisterEnumerationElement (
														   iluType the_type,				// enumeration type 
														   iluCardinal card_element_num,	// which element 
														   iluCString pc_element_name,		// element name 
														   iluCardinal card_element_value	// element value 
														   ) {
	iluError an_error;
	iluBoolean b_return;
	
	b_return = ilu_RegisterEnumerationElement(the_type, card_element_num, 
		pc_element_name, card_element_value, &an_error);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return (b_return ? ILUCPP_TRUE : ILUCPP_FALSE);
}


//////////////////////////////////////////////////////////////////
// call this function to begin registering a new union type 

iluType iluCppInternal::iluRegisterUnionType (
											  iluCString pc_name,				// name 
											  iluCString pc_interface_name,		// interface-name 
											  iluCString pc_interface_brand,	// interface-brand, OPTIONAL 
											  iluCString pc_uid,				// UID 
											  iluTypeUID pc_type_uid,			// discriminant type 
											  iluCardinal card_num_arms,		// number of arms 
											  iluCardinal card_default_arm,		// default arm (0 for none { } 1-n otherwise) 
											  ILUCPP_BOOL b_others_allowed,		// others allowed 
											  ILUCPP_BOOL *	p_b_new				// OUT:  true if new registration 	
											  ) {
	iluError an_error;
	iluType return_value;
	iluBoolean b_localnew;
	
	return_value = ilu_RegisterUnionType (pc_name, pc_interface_name, pc_interface_brand,
		pc_uid, pc_type_uid, card_num_arms, card_default_arm, 
		(b_others_allowed ? iluTRUE : iluFALSE), &b_localnew, &an_error);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	*p_b_new = (b_localnew ? ILUCPP_TRUE : ILUCPP_FALSE);
	
	return return_value;
}


//////////////////////////////////////////////////////////////////
// call this function repeatedly to fill in the fields of a newly registered union type 

iluUnionArm iluCppInternal::iluRegisterUnionArm (
									 ilu_Type the_type,				// union type 
									 iluCardinal card_arm_number,	// which arm? 
									 iluCString pc_arm_name,		// arm name 
									 iluTypeUID pc_type_uid,		// arm type 
									 iluCardinal card_num_values	// number of values 
									 ) {
	iluError an_error;
	iluUnionArm return_value;

	return_value = ilu_RegisterUnionArm (the_type, card_arm_number, pc_arm_name,
		pc_type_uid, card_num_values, &an_error);
		
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return return_value;
 }


//////////////////////////////////////////////////////////////////
// if there was more than one possible value for a union arm, fill them in here 

ILUCPP_BOOL iluCppInternal::iluRegisterUnionArmValue (
													  iluUnionArm arm,					// arm to fill in 
													  iluCardinal card_value_index,		// value index 
													  iluConstantValue	constant_value	// value 
													  ) {
	iluError an_error;
	iluBoolean b_return;
	
	b_return = ilu_RegisterUnionArmValue(arm, card_value_index, constant_value, &an_error);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return (b_return ? ILUCPP_TRUE : ILUCPP_FALSE);
}


//////////////////////////////////////////////////////////////////
// register an optional type 

iluType iluCppInternal::iluRegisterOptionalType (
												 iluCString pc_name,			// name 
												 iluCString pc_interface_name,	// interface-name 
												 iluCString pc_interface_brand,	// interface-brand, OPTIONAL 
												 iluCString pc_uid,				// UID 
												 iluTypeUID pc_type_uid,		// base type for optional 
												 ILUCPP_BOOL * p_b_new			// OUT:  true if new registration 	
												 ) {
	iluError an_error;
	iluType return_value;
	iluBoolean b_localnew;
	
	return_value = ilu_RegisterOptionalType (pc_name, pc_interface_name, pc_interface_brand,
		pc_uid, pc_type_uid, &b_localnew, &an_error);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	*p_b_new = (b_localnew ? ILUCPP_TRUE : ILUCPP_FALSE);
	
	return return_value;
}


//////////////////////////////////////////////////////////////////
// register an alias for a type 

iluType iluCppInternal::iluRegisterAliasType (
											  iluCString pc_name,				// name 
											  iluCString pc_interface_name,		// interface-name 
											  iluCString pc_interface_brand,	// interface-brand, OPTIONAL 
											  iluCString pc_uid,				// UID 
											  iluTypeUID pc_type_uid,			// base type 
											  ILUCPP_BOOL *	p_b_new				// OUT:  true if new registration 	
											  ) {
	iluError an_error;
	iluType return_value;
	iluBoolean b_localnew;
	
	return_value = ilu_RegisterAliasType (pc_name, pc_interface_name, pc_interface_brand,
		pc_uid, pc_type_uid, &b_localnew, &an_error);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	*p_b_new = (b_localnew ? ILUCPP_TRUE : ILUCPP_FALSE);
	
	return return_value;
}


//////////////////////////////////////////////////////////////////
// register an object type 

iluType iluCppInternal::iluRegisterObjectType (
											   iluCString pc_name,				// name 
											   iluCString pc_interface_name,	// interface-name 
											   iluCString pc_interface_brand,	// interface-brand, OPTIONAL 
											   iluCString pc_uid,				// UID 
											   iluClass the_class,				// class ptr from iluDefineObjectType 
											   ILUCPP_BOOL * p_b_new			// OUT:  true if new registration 	
											   ) {
	iluError an_error;
	iluType return_value;
	iluBoolean b_localnew;
	
	return_value = ilu_RegisterObjectType (pc_name, pc_interface_name, pc_interface_brand,
		pc_uid, the_class, &b_localnew, &an_error);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	*p_b_new = (b_localnew ? ILUCPP_TRUE : ILUCPP_FALSE);
	
	return return_value;
}


//////////////////////////////////////////////////////////////////
// add simple type 

iluType iluCppInternal::iluRegisterPrimitiveType (
												  iluCString pc_name,				// name 
												  iluCString pc_interface_name,		// interface-name 
												  iluCString pc_interface_brand,	// interface-brand, OPTIONAL 
												  iluCString pc_uid,				// UID 
												  iluTypeKind type_kind,			// which primitive type 
												  ILUCPP_BOOL *	p_b_new				// OUT:  true if new registration 	
												  ) {
	iluError an_error;
	iluType return_value;
	iluBoolean b_localnew;
	
	return_value = ilu_RegisterPrimitiveType(pc_name, pc_interface_name, pc_interface_brand, 
		pc_uid, type_kind, &b_localnew, &an_error);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	*p_b_new = (b_localnew ? ILUCPP_TRUE : ILUCPP_FALSE);
	
	return return_value;
}


//////////////////////////////////////////////////////////////////
// called when the specified class is completely defined (i.e. after all methods 
// for the class have been defined

void iluCppInternal::iluObjectTypeDefined(iluClass class_completely_defined) {
	
	ILU_ERRS((internal, no_memory))  an_error;
	
	ilu_ObjectTypeDefined(class_completely_defined, &an_error);
	
	ILUCPP_DEBUG2("iluCppInternal::iluObjectTypeDefined - class %s completely defined\n",
	       class_completely_defined->cl_name);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
}


//////////////////////////////////////////////////////////////////
// initialization

// Walks down each node in the list, calls the nodes function, and 
// removes that node from the list.

void iluCppInternal::iluCallInitializationFunctions(iluInitializationFunctionNode** pp_initialization_function_list) {
	
	iluInitializationFunctionNode* p_node_to_delete;
	
	ILUCPP_DEBUG1("iluCppInternal::iluCallInitializationFunctions entered\n");
	
	// walk down the list, calling and cleaning up as we go
	while (*pp_initialization_function_list) {

		// keep a pointer to the node we will run
		p_node_to_delete = *pp_initialization_function_list;
		
		// take the node we're about to run off the head of the list
		*pp_initialization_function_list = p_node_to_delete->m_p_next_node;

		// run the node's initialization function
		p_node_to_delete->iluRunInitializer();
		
		ILUCPP_DEBUG1("iluCppInternal::iluCallInitializationFunctions ran an initialization function\n");
				
		// delete the former head of the list
		delete p_node_to_delete;
	}
	
	ILUCPP_DEBUG1("iluCppInternal::iluCallInitializationFunctions exiting\n");
}


//////////////////////////////////////////////////////////////////
// Creates a iluInitializationFunctionNode for the list

void iluCppInternal::iluAddInitializationFunction(iluInitializationFunctionNode** pp_initialization_function_list, 
												  iluPFunctionInitializer pf_initialize) {
	
	ILUCPP_DEBUG1("iluCppInternal::iluAddInitializationFunction called\n");
	
	// make up a new node at the front of the list
	iluInitializationFunctionNode* p_new_node = 
		new iluInitializationFunctionNode(pf_initialize, *pp_initialization_function_list);
	
	*pp_initialization_function_list = p_new_node;
	
}


//////////////////////////////////////////////////////////////////
// Tells the runtime what function to call when a surrogate for an object of
// the specified class is needed.

void iluCppInternal::iluRegisterSurrogateCreator(iluClass surrogate_class, 
												 iluPFunctionSurrogateCreator pfunction_surrogate_creator ) {
	
	ILUCPP_DEBUG2("iluCppInternal::iluRegisterSurrogateCreator called for class %s\n", surrogate_class->cl_name );
	iluSetSurrogateCreator(surrogate_class, pfunction_surrogate_creator);
}


//////////////////////////////////////////////////////////////////
// called by the iluCppRuntime::iluSetSurrogateCreator
iluPFunctionSurrogateCreator iluCppInternal::iluSetSurrogateCreator(iluClass surrogate_class,
																	iluPFunctionSurrogateCreator pfunction_surrogate_creator ) {
	
	iluSurrogateCreatorNode* p_node_walker = sm_p_surrogate_creator_list;
	iluPFunctionSurrogateCreator old_creator_function;
	
	ILUCPP_DEBUG2("iluCppInternal::iluSetSurrogateCreator setting function for class %s\n", surrogate_class->cl_name); 
	
	// ensure exclusive access
	iluMutexer list_access_mutex(iluCppInternal::sm_surrogate_creator_list_mutex);
	{		
		// go down the list of surrogate creators, replacing if we get a match
		while (p_node_walker) {
			
			if (p_node_walker->m_class == surrogate_class) {
				old_creator_function = p_node_walker->m_pf_surrogate_creator;
				p_node_walker->m_pf_surrogate_creator = pfunction_surrogate_creator;
				return old_creator_function;
			}		
			p_node_walker = p_node_walker->m_p_next_node;
		}
	}
	
	ILUCPP_DEBUG2("iluCppInternal::iluSetSurrogateCreator didn't find node for a %s, adding one\n", surrogate_class->cl_name); 
	
	// make up a new node at the front of the list
	iluSurrogateCreatorNode* p_new_node = 
		new iluSurrogateCreatorNode(surrogate_class, pfunction_surrogate_creator, sm_p_surrogate_creator_list);

	sm_p_surrogate_creator_list = p_new_node;
	
	return 0;
}


//////////////////////////////////////////////////////////////////
// calls the surrogate creator function for the specified class, and returns 
// an iluObject* to the surrogate, or NULL if the class isn't found.

iluObject* iluCppInternal::iluCreateSurrogate(iluClass surrogate_class, 
											  iluKernelObject a_kernelobject) {
	
	iluSurrogateCreatorNode* p_node_walker = sm_p_surrogate_creator_list;
	iluObject* p_new_surrogate;
	
	ILUCPP_DEBUG2("iluCppInternal::iluCreateSurrogate trying to create a %s surrogate\n", surrogate_class->cl_name); 

	// ensure exclusive access
	iluMutexer creator_mutex(iluCppInternal::sm_surrogate_creator_list_mutex);

	// go down the list of surrogate creators, returning if we get a hit
	while (p_node_walker) {
		
		p_new_surrogate = p_node_walker->iluRunSurrogateCreatorIfClass(surrogate_class, a_kernelobject);
		if (p_new_surrogate) {
			ILUCPP_DEBUG2("iluCppInternal::iluCreateSurrogate surrogate created for a %s\n", surrogate_class->cl_name); 
			return p_new_surrogate;
		}
		
		p_node_walker = p_node_walker->m_p_next_node;
	}
	ILUCPP_DEBUG2("iluCppInternal::iluCreateSurrogate no surrogate creator found for class %s\n", surrogate_class->cl_name); 
	
	return NULL;
}




//////////////////////////////////////////////////////////////////
// exception related

// Thows the appropriate corba system exception for the iluError

void iluCppInternal::iluThrowExceptionFromError(ilu_Error* p_error, const char* pc_filename, int i_line_number) {
	
	iluCardinal card_minor_code;
	iluInteger i_major_code;
	
	ILUCPP_DEBUG4("iluCppInternal::iluThrowExceptionFromError called - error %s, file %s, line %i\n", 
	       ILU_ERR_NAME(*p_error), pc_filename, i_line_number);
	
	card_minor_code = ilu_CORBAizeSystemErr(p_error, &i_major_code);
	
	switch (i_major_code + ILU_ERRTYP(unknown)) {
		
	case CORBA_ERRTYP(unknown) : {
		NAME_OUTSIDE_SCOPE(CORBA, UNKNOWN) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
								 }
	case CORBA_ERRTYP(bad_param) : {
		NAME_OUTSIDE_SCOPE(CORBA, BAD_PARAM) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
								   }
	case CORBA_ERRTYP(no_memory) : {
		NAME_OUTSIDE_SCOPE(CORBA, NO_MEMORY) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
								   }
	case CORBA_ERRTYP(imp_limit) : {
		NAME_OUTSIDE_SCOPE(CORBA, IMP_LIMIT) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
								   }
	case CORBA_ERRTYP(comm_failure) : {
		NAME_OUTSIDE_SCOPE(CORBA, COMM_FAILURE) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
									  }
	case CORBA_ERRTYP(inv_objref) : {
		NAME_OUTSIDE_SCOPE(CORBA, INV_OBJREF) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
									}
	case CORBA_ERRTYP(no_permission) : {
		NAME_OUTSIDE_SCOPE(CORBA, NO_PERMISSION) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
									   }
	case CORBA_ERRTYP(internal) : {
		NAME_OUTSIDE_SCOPE(CORBA, INTERNAL) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
								  }
	case CORBA_ERRTYP(marshal) : {
		NAME_OUTSIDE_SCOPE(CORBA, MARSHAL) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
								 }
	case CORBA_ERRTYP(initialize) : {
		NAME_OUTSIDE_SCOPE(CORBA, INITIALIZE) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
									}
	case CORBA_ERRTYP(no_implement) : {
		NAME_OUTSIDE_SCOPE(CORBA, NO_IMPLEMENT) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
									  }
	case CORBA_ERRTYP(bad_typecode) : {
		NAME_OUTSIDE_SCOPE(CORBA, BAD_TYPECODE) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
									  }
	case CORBA_ERRTYP(bad_operation) : {
		NAME_OUTSIDE_SCOPE(CORBA, BAD_OPERATION) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
									   }
	case CORBA_ERRTYP(no_resources) : {
		NAME_OUTSIDE_SCOPE(CORBA, NO_RESOURCES) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
									  }
	case CORBA_ERRTYP(no_response) : {
		NAME_OUTSIDE_SCOPE(CORBA, NO_RESPONSE) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
									 }
	case CORBA_ERRTYP(persist_store) : {
		NAME_OUTSIDE_SCOPE(CORBA, PERSIST_STORE) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
									   }
	case CORBA_ERRTYP(bad_inv_order) : {
		NAME_OUTSIDE_SCOPE(CORBA, BAD_INV_ORDER) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
									   }
	case CORBA_ERRTYP(transient) : {
		NAME_OUTSIDE_SCOPE(CORBA, TRANSIENT) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
								   }
	case CORBA_ERRTYP(free_mem) : {
		NAME_OUTSIDE_SCOPE(CORBA, FREE_MEM) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
								  }
	case CORBA_ERRTYP(inv_ident) : {
		NAME_OUTSIDE_SCOPE(CORBA, INV_IDENT) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
								   }
	case CORBA_ERRTYP(inv_flag) : {
		NAME_OUTSIDE_SCOPE(CORBA, INV_FLAG) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
								  }
	case CORBA_ERRTYP(intf_repos) : {
		NAME_OUTSIDE_SCOPE(CORBA, INTF_REPOS) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
									}
	case CORBA_ERRTYP(bad_context) : {
		NAME_OUTSIDE_SCOPE(CORBA, BAD_CONTEXT) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
									 }
	case CORBA_ERRTYP(obj_adapter) : {
		NAME_OUTSIDE_SCOPE(CORBA, OBJ_ADAPTER) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
									 }
	case CORBA_ERRTYP(data_conversion) : {
		NAME_OUTSIDE_SCOPE(CORBA, DATA_CONVERSION) the_exception(card_minor_code, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
										 }
	default:  {
		NAME_OUTSIDE_SCOPE(CORBA, INTERNAL) the_exception(ilu_im_badEnumValue, NAME_OUTSIDE_SCOPE(CORBA, COMPLETED_MAYBE));
		throw (the_exception);
			  }
}


}


//////////////////////////////////////////////////////////////////
// thread and connection related


//////////////////////////////////////////////////////////////////
// calls ilu_OSForkNewThread and throws an exception if it returns an error

void iluCppInternal::iluThrowingFork(void (*pfunction_procedure) (void* pv_argument), 
									 void* pv_argument) {
	
	ILU_ERRS((no_memory, no_resources, internal)) an_error;
	
	ILUCPP_DEBUG1("iluCppInternal::iluThrowingFork() calling forking procedure\n");
	
	sm_pfunction_forking_procedure(pfunction_procedure, pv_argument, &an_error);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
}


//////////////////////////////////////////////////////////////////
// When OS threading is used, forks threads to monitor any new outgoing connections

void iluCppInternal::iluCreateMonitorsForNewOutgoingConnections(void*) {
	
	ilu_Connection  new_connection;
	iluError an_error;	// Note: seems we need this initialization
	ILU_CLER(an_error);
	
	// continually loop looking for new connections that should be monitored
	while (1) {
		new_connection = ilu_OtherNewConnection(&an_error);
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
		
		if (new_connection != NULL) {
			
			ILUCPP_DEBUG2("iluCppInternal::iluCreateMonitorsForNewOutgoingConnections forking iluMonitorOutgoingConnection for connection %p\n",
				new_connection);
			
			// we got one, so fork a monitoring thread
			iluThrowingFork(iluMonitorOutgoingConnection, new_connection);
		}
	}
}


//////////////////////////////////////////////////////////////////
// calls ilu's procedure to monitor an outgoing connection when OS threading is used

void iluCppInternal::iluMonitorOutgoingConnection(void* pv_connection) {
	
	ilu_Connection  a_connection = (iluConnection) pv_connection;
	
	ILUCPP_DEBUG2("iluCppInternal::iluMonitorOutgoingConnection - monitoring %p\n", a_connection);
	
	ILU_ERRS((IoErrs)) an_error;
	
	ilu_OutgoingConnectionThreadProc(a_connection, &an_error);

	ILUCPP_DEBUG2("iluCppInternal::iluMonitorOutgoingConnection - ilu_OutgoingConnectionThreadProc returned on %p\n", a_connection);

	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
}


//////////////////////////////////////////////////////////////////
// when a connection request arrives at the port when OS threading is used, fork
// a thread to handle input on it

void iluCppInternal::iluThreadedReadConnectionRequests (void* pv_port) {
	
	iluPort the_port = (iluPort) pv_port;
	iluConnection  a_connection;
	iluBoolean b_closed;
	iluError an_error;
	
	// wait for a connection request to arrive at the port
	while (ilu_WaitForPortConnectionRequest(the_port, &an_error) != ILUCPP_FALSE) {
		
		// get hold of the connection
		a_connection = ilu_HandleNewConnection(the_port, &b_closed, &an_error);
		
		if (b_closed) {
			ilu_DoneWithPort(the_port, &an_error);
			ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
			ILUCPP_DEBUG2("iluCppInternal::iluThreadedReadConnectionRequests - no fork for closed connection %p\n", a_connection);
			return;
		}
		
		if (a_connection) {
			ILUCPP_DEBUG2("iluCppInternal::iluThreadedReadConnectionRequests() - connection %p\n", a_connection);			
			iluThrowingFork(iluRunConnection, a_connection);			
			ILUCPP_DEBUG2("iluCppInternal::iluThreadedReadConnectionRequests - forked for connection %p\n", a_connection);
		}		
		// connection must be nil - check for error
		else if (ILU_ERROK(an_error)) { // spurious call
			ILUCPP_DEBUG2("iluCppInternal::iluThreadedReadConnectionRequests() - spurious call on port %p\n", the_port);
		}
		else {
			// if an error, must be out of resources or have a lock problem - there's
			// really nothing todo except abondon the port
			ILUCPP_DEBUG2("iluCppInternal::iluThreadedReadConnectionRequests() - abandoning port %p due to error\n", the_port);
			ILU_HANDLED(an_error);
			ilu_DoneWithPort(the_port, &an_error);
			ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
			return;
		}		
	}

	if (ILU_ERROK(an_error)) { // ilu_WaitForPortConnectionRequest must have had an error
		ILUCPP_DEBUG2("iluCppInternal::iluThreadedReadConnectionRequests() - ilu_WaitForPortConnectionRequest returned False - error %s\n", ILU_ERR_NAME(an_error));
		ILU_HANDLED(an_error);
	}

}



//////////////////////////////////////////////////////////////////////
// used to get around some compilers complaining about passing a c++
// function where a c function is expected.

extern "C" {typedef void(*C_iluNonThreadedServiceRequest_proc)(ilu_refany);}



//////////////////////////////////////////////////////////////////
// gets called when when running single threaded and a connection request arrives 
// at the port

void iluCppInternal::iluNonThreadedReadConnectionRequests (void* pv_port) {
	
	iluPort the_port = (iluPort) pv_port;
	iluConnection		a_connection;
	iluBoolean			b_closed;
	iluError			an_error;
	
	// get the new connection to the port
	a_connection = ilu_HandleNewConnection(the_port, &b_closed, &an_error);
	
	if (b_closed) {
		ILUCPP_DEBUG1("iluCppInternal::iluNonThreadedReadConnectionRequests(), ilu_HandleNewConnection said port closed\n");
		ilu_DoneWithPort(the_port, &an_error);
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
		return;
	}
	
	if (a_connection) {
		ILUCPP_DEBUG2("iluCppInternal::iluNonThreadedReadConnectionRequests() - connection %p\n", a_connection);
		
		// set the function to call when input comes in on the connection
		ilu_SetConnectionInputHandler(a_connection, 
					      REINTERPRET_CAST(C_iluNonThreadedServiceRequest_proc, iluNonThreadedServiceRequest),
					      a_connection, &an_error);
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
		return;
	}
	
	// connection must be nil - check for error
	if (ILU_ERROK(an_error)) { // spurious call
		ILUCPP_DEBUG2("iluCppInternal::iluNonThreadedReadConnectionRequests() - spurious call on port %p\n", the_port);
		return;
	}
	else {
		// if an error, must be out of resources or have a lock problem - there's
		// really nothing todo except abondon the port
		ILUCPP_DEBUG2("iluCppInternal::iluNonThreadedReadConnectionRequests() - abandoning port %p due to error\n", the_port);
		ILU_HANDLED(an_error);
		ilu_DoneWithPort(the_port, &an_error);
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
		return;
	}		
}



//////////////////////////////////////////////////////////////////
// handles input on a connection when OS threading is used

void iluCppInternal::iluRunConnection (void* pv_connection) {
	
	iluConnection a_connection = (iluConnection) pv_connection;
	iluError an_error;
	
	ILUCPP_DEBUG2("iluCppInternal::iluRunConnection() - running %p\n", a_connection);
	
	while (1) {
		
		if (!ilu_BlockingWaitForInputOnConnection(a_connection, (iluFineTime *) 0))
			break;
		
		ILUCPP_DEBUG2("iluCppInternal::iluRunConnection() - calling  iluServiceRequest on connection %p\n", a_connection);
		
		// service the request
		if (iluServiceRequest(a_connection))
			break;
	}

	ilu_DoneServingConnection(a_connection, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);

}



//////////////////////////////////////////////////////////////////
// when a connection handles concurrent requests, this function gets
// run in a thread to run the stub procedure and finish up

void iluCppInternal::iluPerformThreadedRequest(void* pv_invocation_arg) {
	
	iluThreadPerRequestInvocationArgument* p_invocation_arg = 
		(iluThreadPerRequestInvocationArgument*) pv_invocation_arg;
	
	ILUCPP_DEBUG1("iluCppInternal::iluPerformThreadedRequest() - calling stub\n");
	
	// run the method's stub
	(*(p_invocation_arg->m_pfunction_method_stubproc)) (p_invocation_arg->m_call);

	// free the passed argument (previously allocated in iluCppInternal::iluServiceRequest)
	ilu_free(p_invocation_arg->m_call);
	ilu_free(p_invocation_arg);
}


//////////////////////////////////////////////////////////////////
// service a request - called either from the thread handling the connection
// (iluRunConnection) in the OS threaded case, or in the single threaded case
// from ilu who has been told (ilu_SetConnectionRequestHandler) that this is the
// function to call when a input comes in on a connection
// 
// Returns TRUE iff connection closed.
// Main Invariant holds
//  before: L2 disjoint {arg's callmu, iomu}
//  after:  L2     >=   {conn's callmu, iomu} if result==ilu_good_request,
//  after:  L2 disjoint {conn's callmu, iomu} if result!=ilu_good_request 

ILUCPP_BOOL iluCppInternal::iluServiceRequest (void* pv_connection) {
	
	iluConnection		a_connection = (iluConnection) pv_connection;
	ilu_RcvReqStat		request_status;
	iluClass			a_class;
	iluMethod			a_method;
	iluBoolean			b_initted;
	iluCardinal			card_serial_number;
	ilu_Call_s			a_call_s;
	iluError			an_error;
	_stubproc			stub_procedure;
	iluCall				a_call = &a_call_s;
	iluThreadPerRequestInvocationArgument* p_invocation_arg = NULL;
	
	ILUCPP_DEBUG2("iluCppInternal::iluServiceRequest() entered - connection %p\n", pv_connection);
	
	// turn off requests on the connection (note does nothing in threaded case)
	iluDisableRequests((iluCall) 0, a_connection);
	
	if (sm_b_running_threaded && ilu_ThreadPerRequest(a_connection)) {
		// if we're supposed to fork a thread for every request
		// allocate memory to hold a call and method that will be passed 
		// to the thread actually running the call
		p_invocation_arg = (iluThreadPerRequestInvocationArgument*)
			ilu_MallocE(sizeof(iluThreadPerRequestInvocationArgument), &an_error);
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
		p_invocation_arg->m_call = (iluCall) ilu_MallocE(sizeof(ilu_Call_s), &an_error);
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
		a_call = p_invocation_arg->m_call;
	}
	
	ILUCPP_DEBUG1("iluCppInternal::iluServiceRequest() calling ilu_ReceiveRequest()\n");
	
	// start to receive the request
	request_status = ilu_ReceiveRequest(a_call, &b_initted, a_connection,
		&a_class, &a_method, &card_serial_number, &an_error);
	if (ILU_ERRNOK(an_error) && p_invocation_arg) {
		ilu_free(p_invocation_arg->m_call);
		ilu_free(p_invocation_arg);
	}
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	// if it's a request
	if (request_status == ilu_RcvReqStat_request) {
		
		// get a hold of the stub procedure for this method
		// XXX when/if we allow different language implementations of the same
		// method, and dont' use a language index based approach for the 
		// ilu_SetMethodStubProc, ilu_GetMethodStubProc calls, then the next statement 
		// will have to find the stub-procedure based on discriminator object 
		// class [use a_class] and method name [ use method_name(a_method) ]
		// Also, the true side stubs init procedure will have to register this info somehow
		// (hash table?) instead of calling ilu_SetMethodStubProc
		stub_procedure = REINTERPRET_CAST(_stubproc, ilu_GetMethodStubProc(a_method, sm_card_cpp_language_index));
		
		// if it's NULL, we're in trouble, so die
		_ilu_Assert(stub_procedure != NULL, "iluCppInternal::iluFinalServiceRequest NULL stub procedure");
		
		ILUCPP_DEBUG3("iluCppInternal::iluServiceRequest - invoking stub %p on call %p \n", stub_procedure, a_call);
		
		if (p_invocation_arg) {
			// we should be forking a thread
			p_invocation_arg->m_pfunction_method_stubproc = stub_procedure;
			iluThrowingFork(iluPerformThreadedRequest, p_invocation_arg);
		}
		
		// invoke the stub procedure
		else (*((void (*) (iluCall)) stub_procedure)) (a_call);
		
		return ILUCPP_FALSE;
	}
	
	ILUCPP_DEBUG1("iluCppInternal::iluServiceRequest() - request_status != ilu_RcvReqStat_request\n");
	
	if (b_initted) { // if the call was initialized, we must perform finish call
		ilu_FinishCall(a_call, &an_error);
		ILUCPP_ERRWARN(an_error, "iluCppInternal::iluServiceRequest, ilu_FinishCall", ILUCPP_TRUE);
		ILUCPP_DEBUG2("iluCppInternal::iluServiceRequest - finished call %p\n", a_call);
	}
	
	// see if we're supposed to quit the connection
	if (request_status == ilu_RcvReqStat_quit) {
		return ILUCPP_TRUE;
	}
	
	if (!(a_call->ca_reqs_enabled))
		// turn requests back on for the connection
		iluEnableRequests(a_call, a_connection);
	
	return ILUCPP_FALSE;
}


//////////////////////////////////////////////////////////////////
// just calls the iluServiceRequest and cleans up if we're done with the connection

void iluCppInternal::iluNonThreadedServiceRequest (void* pv_arg) {
  if (iluServiceRequest(pv_arg)) 
    if (!ilu_ConnectionServingP((iluConnection) pv_arg)) {
      iluError an_error;
      ilu_DoneServingConnection((iluConnection) pv_arg, &an_error);
      ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
    }
}


//////////////////////////////////////////////////////////////////
// disables requests on a connection

ILUCPP_BOOL iluCppInternal::iluDisableRequests(iluCall call, iluConnection connection) {
	
	// if we're threaded do nothing since new threads will be forked for new connections
	if (sm_b_running_threaded)
		return ILUCPP_TRUE;
	
	ILUCPP_BOOL	b_result;
	iluError	an_error;
	
	// set to no input handler for the connection
	b_result = (ilu_SetConnectionInputHandler(connection, 0, NULL, &an_error) ? ILUCPP_TRUE : ILUCPP_FALSE);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	if (!b_result) {
		
		ILUCPP_ERRWARN(an_error, "iluCppInternal::iluDisableRequests, ilu_SetConnectionInputHandler returned false", ILUCPP_TRUE);
	}
	else if (call != ILU_NIL)
		call->ca_reqs_enabled = ILUCPP_FALSE;
	
	ILUCPP_DEBUG3("iluCppInternal::iluDisableRequests - disabled connection %p on call %p\n", connection, call);
	
	return b_result;
}


//////////////////////////////////////////////////////////////////
// enables requests on a connection

ILUCPP_BOOL iluCppInternal::iluEnableRequests(iluCall call, iluConnection connection) {
	
	// if we're threaded do nothing since new threads will be forked for new connections
	if (sm_b_running_threaded)
		return ilu_TRUE;
	
	ILUCPP_BOOL	b_result;
	iluError	an_error;
	
	// set back to the input handler for the connection
	b_result = (ilu_SetConnectionInputHandler(connection, 
						  REINTERPRET_CAST(C_iluNonThreadedServiceRequest_proc, iluNonThreadedServiceRequest),
						  connection, &an_error) 
		? ILUCPP_TRUE : ILUCPP_FALSE);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	if (!b_result) {
		
		ILUCPP_ERRWARN(an_error, "iluCppInternal::EnableRequests, ilu_SetConnectionInputHandler returned false", ILUCPP_TRUE);
	}
	else if (call != ILU_NIL)
		call->ca_reqs_enabled = ILUCPP_TRUE;
	
	ILUCPP_DEBUG3("iluCppInternal::iluEnableRequests - enabled connection %p on call %p\n", connection, call);
	
	return b_result;
}



//////////////////////////////////////////////////////////////////
// returns ILUCPP_TRUE iff we're running in a mode where we have 
// condition variables

ILUCPP_BOOL iluCppInternal::iluHaveConditions() {
	
	iluError	an_error;
	iluCondition a_condition;
	
	a_condition = ilu_CreateCondition(CONST_CAST(char*, ""), CONST_CAST(char*, ""), &an_error);
	
	if (a_condition) {		
		ilu_DestroyCondition(a_condition);
		return ILUCPP_TRUE;
	}
	
	ILU_HANDLED(an_error);
	return ILUCPP_FALSE;
}


//////////////////////////////////////////////////////////////////
// calls the iluDecrementInvocations function of the activation table - here
// so we don't have to export the activation table to the stubs
ILUCPP_BOOL iluCppInternal::iluDecrementInvocations(iluKernelObject a_kernel_object) {
	return iluObjectActivationTable::sm_p_object_activation_table->iluDecrementInvocations(a_kernel_object);
}


// returns id of current thread
iluCardinal iluCppInternal::iluGetThreadID() {
	if (!sm_b_running_threaded) 
		return 0;
	if (sm_card_thread_id_function) // must be running non native threaded
		return (*sm_card_thread_id_function)();
#ifdef ILU_OS_THREADED
	if (ilu_Fork == ilu_OSForkNewThread)
	  return GET_CURRENT_THREAD();
#endif
	return 0;
}


//////////////////////////////////////////////////////////////////
// End of File
//////////////////////////////////////////////////////////////////




