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
/* $Id: ilucall.cpp,v 1.37 1999/08/16 19:53:11 larner Exp $ */


// include ILU CORBA C++ header file
#include "corba.hpp"

/*
What the C++ Stubs/Runtime do with Object Reference Counts

True Side

IN objects - duplicate coming in, release called on them at stub exit
INOUT objects - duplicate coming in, release called on them at stub exit
OUT objects - release called on them at stub exit
Return objects - release called on them at stub exit


Surrogate Side

IN objects - nothing done
INOUT objects - release going out, duplicate called on them at stub return
OUT objects - duplicate called on them at stub return
Return objects - duplicate called on them at stub return

*/

//////////////////////////////////////////////////////////////////
// recalculates attributes based on the (void*) object pointer - used for true side
// inout situations

	void iluObjectWrapper::iluReviseForObject(iluObject* what_m_pv_iluobject_now_is) {
	
	
	// just return  if things haven't changed
	if ((what_m_pv_iluobject_now_is && (what_m_pv_iluobject_now_is->iluGetKernelObject() == m_kernelobject)) || // still same object
		((!what_m_pv_iluobject_now_is) && (!m_kernelobject))) // both null 
		return;
	
	// object has been changed - remove any activation count on the old object
	if (m_b_incremented_activation && m_kernelobject)
		
		// tell the activation table to decrement the counts on our kernel object
		if (! iluCppInternal::iluDecrementInvocations(m_kernelobject)) {
		ILU_ERRPRINTF("Error Warning iluObjectWrapper::iluReviseForObject()() - kernel object not in activation table!\n");
	}
	
	// note that we don't release since if the server implementation changed the 
	// parameter - it was obligated to do the release per the CORBA spec

	// set the kernel object and class to agree, ensure our member is set in case 
	// some external pointer was used instead of the member itself
	m_kernelobject = (what_m_pv_iluobject_now_is ? what_m_pv_iluobject_now_is->iluGetKernelObject() : NULL);
	m_class = ((what_m_pv_iluobject_now_is ? what_m_pv_iluobject_now_is->iluGetClassRecord() : m_class)); 
	m_pv_iluobject = what_m_pv_iluobject_now_is;

	m_b_incremented_activation = ILUCPP_FALSE;
}



//////////////////////////////////////////////////////////////////
// iluObjectWrapper destructor
// destructor takes care of decrementing activation and reference counting if needed

iluObjectWrapper::~iluObjectWrapper() {
	
	if (m_b_incremented_activation && m_kernelobject)
		
		// tell the activation table to decrement the counts on our kernel object
		if (! iluCppInternal::iluDecrementInvocations(m_kernelobject)) {
		ILU_ERRPRINTF("Error Warning iluObjectWrapper::~iluObjectWrapper()() - kernel object not in activation table!\n");
		}
	
	if (m_b_do_refcount_decrement && m_kernelobject) {
		// get the language specific object corresponding to this kernel object
		// note we can't cast the m_pv_iluobject to an iluObject because
		// it's a void* pointing at what the object really is (some subclass of iluObject)
		iluKernelServer the_kernel_server = ilu_ServerOfObject(m_kernelobject);
		
		ilu_EnterServer(the_kernel_server, m_class);
		
		iluObject* p_ilu_object = (iluObject*)ilu_GetLanguageSpecificObject(m_kernelobject,
			iluCppInternal::iluGetCppLanguageIndex()); 
		
		// exit the object's server
		ilu_ExitServer(the_kernel_server, m_class);
		
		p_ilu_object->iluDecrementReferenceCount();
	}	
}


//////////////////////////////////////////////////////////////////
// iluBaseCall is a common base class for iluTrueCall and iluSurrogateCall.
// It encapsulates an iluCall to provide:  Automatic call of FinishCall
// in its destructor to make flow of control easier to deal with (since
// if a problem occurs in one of its methods, throwing an exception,
// we're guaranteed to have it's destructor run) ; It hangs on to the
// iluCall so we don't have to pass it as an argument to every member 
// function call.


// constructor
iluBaseCall::iluBaseCall(iluCall a_call, ILUCPP_BOOL b_can_raise_exceptions) {
	
	// retain the call
	m_call = a_call;
	
	// initialize state
	m_state = ilucall_initial;
	
	// used to size arguments and replies
	m_card_size = 0;
	
	// when there's an exception, we save the exception number here
	m_card_exception_number = 0;
	
	// whether or not exceptions can be generated from the method being used on this call
	m_b_exceptions = b_can_raise_exceptions;
	
	// init to no call retry
	m_b_retry_call = ILUCPP_FALSE;

	// init to no error during call
	m_error_type = ILU_ERRTYP(success);

	ILUCPP_DEBUG1("iluBaseCall::iluBaseCall() constructed\n");
	
}


//////////////////////////////////////////////////////////////////
// input primitives
//////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////
// input a byte

iluBaseCall& iluBaseCall::operator>>(iluByte& r_byte) { 
	
	iluError	an_error;
	
	ilu_InputByte(m_call, &r_byte, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// input a boolean

iluBaseCall& iluBaseCall::operator>>(iluBoolWrapper& r_boolean_wrapper) { 
	iluError	an_error;
	iluBoolean  b_value;		
	ilu_InputBoolean(m_call, &b_value, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	r_boolean_wrapper.m_r_bool = (b_value ? ILUCPP_TRUE : ILUCPP_FALSE);
	return *this;
}


//////////////////////////////////////////////////////////////////
// input an optional

iluBaseCall& iluBaseCall::operator>>(iluOptionalWrapper& r_optional_wrapper)  { 
	
	iluError	an_error;
	iluBoolean b_present;
	ilu_InputOptional(m_call, &b_present, (iluType)ILU_NIL, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	r_optional_wrapper.m_present = (b_present ? ILUCPP_TRUE : ILUCPP_FALSE);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// input a cardinal

iluBaseCall& iluBaseCall::operator>>(iluCardinal& r_cardinal)   { 
	
	iluError	an_error;
	
	ilu_InputCardinal(m_call, &r_cardinal, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// input a character

iluBaseCall& iluBaseCall::operator>>(iluCharacterWrapper& r_character_wrapper) { 
	
	iluError	an_error;
	
	ilu_InputCharacter(m_call, &(r_character_wrapper.m_r_shortcardinal), &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// input a short character

iluBaseCall& iluBaseCall::operator>>(iluShortCharacter& r_shortcharacter) { 
	
	iluError	an_error;
	
	ilu_InputShortCharacter(m_call, &r_shortcharacter, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// input an enum

iluBaseCall& iluBaseCall::operator>>(iluEnumWrapper& r_enum) { 
	
	iluError	an_error;
	iluShortCardinal scard_temp;

	ilu_InputEnum(m_call, &scard_temp, (iluType)ILU_NIL, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);

	r_enum.m_r_enum = (enum iluDummyEnum)scard_temp;
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// input an integer

iluBaseCall& iluBaseCall::operator>>(iluInteger& r_integer) { 
	
	iluError	an_error;
	
	ilu_InputInteger(m_call, &r_integer, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// input a real

iluBaseCall& iluBaseCall::operator>>(iluReal& r_real) { 
	iluError	an_error;
	
	ilu_InputReal(m_call, &r_real, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// input a short cardinal

iluBaseCall& iluBaseCall::operator>>(iluShortCardinal& r_shortcardinal) { 
	
	iluError	an_error;
	
	ilu_InputShortCardinal(m_call, &r_shortcardinal, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// input a short integer

iluBaseCall& iluBaseCall::operator>>(iluShortInteger& r_shortinteger) { 
	
	iluError	an_error;
	
	ilu_InputShortInteger(m_call, &r_shortinteger, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// input a short real

iluBaseCall& iluBaseCall::operator>>(iluShortReal& r_shortreal) { 
	
	iluError	an_error;
	
	ilu_InputShortReal(m_call, &r_shortreal, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// input a long cardinal

iluBaseCall& iluBaseCall::operator>>(iluLongCardinal& r_long_cardinal) { 
	
	iluError	an_error;
	
	ilu_InputLongCardinal(m_call, &r_long_cardinal, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// input a long integer

iluBaseCall& iluBaseCall::operator>>(iluLongInteger& r_longinteger) { 
	
	iluError	an_error;
	
	ilu_InputLongInteger(m_call, &r_longinteger, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// input a long real

iluBaseCall& iluBaseCall::operator>>(iluLongReal& r_longreal) { 
	
	iluError	an_error;
	
	ilu_InputLongReal(m_call, &r_longreal, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// input a cstring

iluBaseCall& iluBaseCall::operator>>(iluCStringWrapper& r_cstring_wrapper) { 
	
	iluError	an_error;

	// in case we're an inout
	r_cstring_wrapper.iluDeleteIfNeededOnInput();

	ilu_InputString(m_call, &(r_cstring_wrapper.m_r_cstring), 
		&(r_cstring_wrapper.m_r_card_length), 0, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// input a cstring vec

iluBaseCall& iluBaseCall::operator>>(iluCStringVecWrapper& r_cstringvec_wrapper) { 
	
	iluError	an_error;

	// in case we're an inout
	r_cstringvec_wrapper.iluDeleteIfNeededOnInput();

	ilu_InputStringVec(m_call, &(r_cstringvec_wrapper.m_r_cstringvec), 
		r_cstringvec_wrapper.m_card_length, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// input a wstring

iluBaseCall& iluBaseCall::operator>>(iluWStringWrapper& r_wstring_wrapper) { 
	
	iluError	an_error;
	
	// in case we're an inout
	r_wstring_wrapper.iluDeleteIfNeededOnInput();

	ilu_InputWString(m_call, &(r_wstring_wrapper.m_r_wstring), 
		&(r_wstring_wrapper.m_r_card_length), 0, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// input a wstring vec

iluBaseCall& iluBaseCall::operator>>(iluWStringVecWrapper& r_wstringvec_wrapper) { 
	
	iluError	an_error;
	
	// in case we're an inout
	r_wstringvec_wrapper.iluDeleteIfNeededOnInput();

	ilu_InputWStringVec(m_call, &(r_wstringvec_wrapper.m_r_wstringvec), 
		      r_wstringvec_wrapper.m_card_length, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// input bytes

iluBaseCall& iluBaseCall::operator>>(iluBytesWrapper& r_bytes_wrapper) { 
	
	iluError	an_error;
	
	// in case we're an inout
	r_bytes_wrapper.iluDeleteIfNeededOnInput();

	ilu_InputBytes(m_call, &(r_bytes_wrapper.m_r_pc_buffer), 
		&(r_bytes_wrapper.m_r_card_length), 0, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// input an opaque 

iluBaseCall& iluBaseCall::operator>>(iluOpaqueWrapper& r_opaque_wrapper) { 
	
	iluError	an_error;
	
	// in case we're an inout
	r_opaque_wrapper.iluDeleteIfNeededOnInput();

	ilu_InputOpaque(m_call, &(r_opaque_wrapper.m_r_pc_buffer), 
		r_opaque_wrapper.m_card_length, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// input an object

iluBaseCall& iluBaseCall::operator>>(iluObjectWrapper& r_object_wrapper) { 
	
	iluError	an_error;
	iluObject*	p_ilu_object;
	iluClass	surrogate_class;
	iluClass	wrapper_class = r_object_wrapper.m_class;
	
	ILUCPP_DEBUG1("iluBaseCall::operator>> entered\n");

	// if we're surrogate side and our wrapper's object isn't NULL,
	// then we must be a wrapper for an inout, so we'll perform now
	// the deactivate and release on the object that went out
	if (iluIsSurrogateCall() && r_object_wrapper.m_pv_iluobject) {
		if (iluCppInternal::iluGetRunningThreaded()) {
			if (! iluObjectActivationTable::sm_p_object_activation_table->iluDecrementInvocations(r_object_wrapper.m_kernelobject)) {
				// object must be not marked active						
				// cons up and throw an error
				ILU_ERR_CONS1(inv_objref, &an_error, minor, ilu_iom_inst_nf, ilu_FALSE);			
				ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
			}
			r_object_wrapper.m_b_incremented_activation = ILUCPP_FALSE;
		}
		((iluObject*)(r_object_wrapper.m_pv_iluobject))->iluDecrementReferenceCount();
	}
	

	// if it's the discriminator for a singleton
	if (r_object_wrapper.m_b_discriminator && wrapper_class->cl_singleton) {
		r_object_wrapper.m_kernelobject = ilu_GetCallSingleton(m_call, &an_error);
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	} 
	else {
		// just an ordinary object
		ilu_InputObjectID(m_call, &(r_object_wrapper.m_kernelobject), 
			(r_object_wrapper.m_b_discriminator ? iluTRUE : iluFALSE),
			wrapper_class, &an_error);
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	}
	
	ILUCPP_DEBUG2("iluBaseCall::operator>> got kernel object %p\n", r_object_wrapper.m_kernelobject);
	
	if (r_object_wrapper.m_kernelobject == NULL) { // must be the null object
		r_object_wrapper.m_pv_iluobject = NULL;
		return *this;
	}

	// now Inside(object's server, class)
	
	if (iluIsTrueCall() && iluCppInternal::iluGetRunningThreaded()) {
		
		// if we're on the true side, inc the activation count on the object
		if (! iluObjectActivationTable::sm_p_object_activation_table->iluIncrementInvocations(r_object_wrapper.m_kernelobject)) {
			
			// object must be not marked active
			
			// exit the object's server
			ilu_ExitServer(ilu_ServerOfObject(r_object_wrapper.m_kernelobject), wrapper_class);
			
			// cons up and throw an error
			ILU_ERR_CONS1(inv_objref, &an_error, minor, ilu_iom_inst_nf, ilu_FALSE);			
			ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
			
		}
		// indicate in the wrapper that an incrementation was performed - it's
		// destructor will decrement it
		r_object_wrapper.m_b_incremented_activation = ILUCPP_TRUE;
	}
	
	
	// get the language specific object corresponding to this kernel object
	p_ilu_object = (iluObject*)ilu_GetLanguageSpecificObject(r_object_wrapper.m_kernelobject,
		iluCppInternal::iluGetCppLanguageIndex());
	
	ILUCPP_DEBUG3("iluBaseCall::operator>> found lso %p for kernel object %p\n", p_ilu_object, r_object_wrapper.m_kernelobject);
	
	if (p_ilu_object == NULL) {
		// we didn't find an object already present for this, so create a new surrogate
		if ((surrogate_class = ilu_ClassOfObject(r_object_wrapper.m_kernelobject)) != NULL)
			p_ilu_object = iluCppInternal::iluCreateSurrogate(surrogate_class, 
			r_object_wrapper.m_kernelobject);
	}
	
	// exit the object's server
	ilu_ExitServer(ilu_ServerOfObject(r_object_wrapper.m_kernelobject), wrapper_class);
	
	if (p_ilu_object == NULL) { // had no object true or no ability to create surrogate
		// cons up and throw an error
		ILU_ERR_CONS1(inv_objref, &an_error, minor, ilu_iom_inst_nf, ilu_FALSE);
		
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	}
	
	// adjust refcounts - bump up by one for incoming objects, but if we're
	// on the true side, set up the wrapper to do a release when it destructs
	// else don't do a release on destruction
	p_ilu_object->iluIncrementReferenceCount();
	r_object_wrapper.m_b_do_refcount_decrement = iluIsTrueCall();


	ILUCPP_DEBUG4("iluBaseCall::operator>> downcasting lso %p for kernel object %p to %s\n", 
		p_ilu_object, r_object_wrapper.m_kernelobject, wrapper_class->cl_name);
	
	// We now call the virtual function iluDowncast on the p_ilu_object.  The will invoke 
	// iluDowncast on the most specific object that the language specific object 'really' is.
	// The iluDowncast member function will return a void pointer that is really pointing to the
	// most specific object. We leave this pointer in the wrapper's m_pv_iluobject member
	// for casting (back in the stub) to the most specific object type
	
	r_object_wrapper.m_pv_iluobject = p_ilu_object->iluDowncast(wrapper_class);
	
	return *this;	
}


//////////////////////////////////////////////////////////////////
// input a sequence 

iluBaseCall& iluBaseCall::operator>>(iluSequenceWrapper& r_sequence_wrapper) {
	
	iluError	an_error;
	
	ilu_InputSequence(m_call, r_sequence_wrapper.m_p_card_count, 0, (iluType)ILU_NIL, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// input a union

iluBaseCall& iluBaseCall::operator>>(iluUnionWrapper& r_union_wrapper) { 
	
	iluError	an_error;
	
	ilu_InputUnion(m_call, &(r_union_wrapper.m_card_discriminator), 
		r_union_wrapper.m_typekind, (iluType)ILU_NIL, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}



//////////////////////////////////////////////////////////////////
// output primitives
//////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////
// output an array

iluBaseCall& iluBaseCall::iluOutputArray (iluCardinal card_array_length)  { 	
	
	iluError an_error;
	
	ilu_OutputArray(m_call, card_array_length, (iluType)ILU_NIL, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// output a byte

iluBaseCall& iluBaseCall::operator<<(iluByte a_byte) { 
	
	iluError an_error;
	
	ilu_OutputByte(m_call, a_byte, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// output a boolean

iluBaseCall& iluBaseCall::operator<<(iluBoolWrapper& r_boolean_wrapper) { 
	
	iluError	an_error;
	iluBoolean  b_value = (r_boolean_wrapper.m_r_bool ? iluTRUE : iluFALSE);
	ilu_OutputBoolean(m_call, b_value, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// output an optional

iluBaseCall& iluBaseCall::operator<<(iluOptionalWrapper& r_optional_wrapper) { 
	
	iluError an_error;
	
	ilu_OutputOptional(m_call, (r_optional_wrapper.m_present ? iluTRUE : iluFALSE), (iluType)ILU_NIL, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// output a cardinal

iluBaseCall& iluBaseCall::operator<<(iluCardinal a_cardinal) { 
	
	iluError an_error;
	
	ilu_OutputCardinal(m_call, a_cardinal, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// output a character

iluBaseCall& iluBaseCall::operator<<(iluCharacterWrapper& r_character_wrapper) { 
	
	iluError	an_error;
	
	ilu_OutputCharacter(m_call, r_character_wrapper.m_r_shortcardinal, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// output a short character

iluBaseCall& iluBaseCall::operator<<(iluShortCharacter a_shortcharacter)  { 
	
	iluError	an_error;
	
	ilu_OutputShortCharacter(m_call, a_shortcharacter, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// output an enum

iluBaseCall& iluBaseCall::operator<<(iluEnumWrapper& r_enum) { 
	
	iluError	an_error;
	iluShortCardinal scard_temp = r_enum.m_r_enum;

	ilu_OutputEnum(m_call, scard_temp, (iluType)ILU_NIL, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// output an integer

iluBaseCall& iluBaseCall::operator<<(iluInteger a_integer) { 
	
	iluError an_error;
	
	ilu_OutputInteger(m_call, a_integer, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// output a real

iluBaseCall& iluBaseCall::operator<<(iluReal a_real)  { 
	
	iluError an_error;
	
	ilu_OutputReal(m_call, a_real, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}



//////////////////////////////////////////////////////////////////
// output a short cardinal

iluBaseCall& iluBaseCall::operator<<(iluShortCardinal a_shortcardinal)  { 
	
	iluError	an_error;
	
	ilu_OutputShortCardinal(m_call, a_shortcardinal, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// output a short integer

iluBaseCall& iluBaseCall::operator<<(iluShortInteger a_shortinteger) {	
	
	iluError	an_error;
	
	ilu_OutputShortInteger(m_call, a_shortinteger, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// output a short real

iluBaseCall& iluBaseCall::operator<<(iluShortReal a_shortreal) { 
	
	iluError	an_error;
	
	ilu_OutputShortReal(m_call, a_shortreal, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// output a long cardinal

iluBaseCall& iluBaseCall::operator<<(iluLongCardinal& r_long_cardinal) { 
	
	iluError	an_error;
	
	ilu_OutputLongCardinal(m_call, r_long_cardinal, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// output a long integer

iluBaseCall& iluBaseCall::operator<<(iluLongInteger& r_longinteger) { 
	
	iluError	an_error;
	
	ilu_OutputLongInteger(m_call, r_longinteger, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// output a long real

iluBaseCall& iluBaseCall::operator<<(iluLongReal& r_longreal)  { 
	
	iluError	an_error;
	
	ilu_OutputLongReal(m_call, r_longreal, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// output a cstring

iluBaseCall& iluBaseCall::operator<<(iluCStringWrapper& r_cstring_wrapper)  { 
	
	iluError	an_error;
	
	ilu_OutputString(m_call, r_cstring_wrapper.m_r_cstring, 
		r_cstring_wrapper.m_r_card_length, 0, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);

	r_cstring_wrapper.iluDeleteIfNeededOnOutput();

	return *this;
}


//////////////////////////////////////////////////////////////////
// output a cstring vec

iluBaseCall& iluBaseCall::operator<<(iluCStringVecWrapper& r_cstringvec_wrapper)  { 
	
	iluError	an_error;
	
	ilu_OutputStringVec(m_call, r_cstringvec_wrapper.m_r_cstringvec, 
		      r_cstringvec_wrapper.m_card_length, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);

	r_cstringvec_wrapper.iluDeleteIfNeededOnOutput();

	return *this;
}


//////////////////////////////////////////////////////////////////
// output a wstring

iluBaseCall& iluBaseCall::operator<<(iluWStringWrapper& r_wstring_wrapper)  { 
	
	iluError	an_error;
	
	ilu_OutputWString(m_call, r_wstring_wrapper.m_r_wstring, 
		r_wstring_wrapper.m_r_card_length, 0, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	r_wstring_wrapper.iluDeleteIfNeededOnOutput();

	return *this;
}


//////////////////////////////////////////////////////////////////
// output a wstring vec

iluBaseCall& iluBaseCall::operator<<(iluWStringVecWrapper& r_wstringvec_wrapper)  { 
	
	iluError	an_error;
	
	ilu_OutputWStringVec(m_call, r_wstringvec_wrapper.m_r_wstringvec, 
		r_wstringvec_wrapper.m_card_length, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	r_wstringvec_wrapper.iluDeleteIfNeededOnOutput();

	return *this;
}


//////////////////////////////////////////////////////////////////
// output bytes

iluBaseCall& iluBaseCall::operator<<(iluBytesWrapper& r_bytes_wrapper)  { 
	
	iluError	an_error;
	
	ilu_OutputBytes(m_call, r_bytes_wrapper.m_r_pc_buffer, 
		r_bytes_wrapper.m_r_card_length, 0, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	r_bytes_wrapper.iluDeleteIfNeededOnOutput();

	return *this;
}


//////////////////////////////////////////////////////////////////
// output an opaque 

iluBaseCall& iluBaseCall::operator<<(iluOpaqueWrapper& r_opaque_wrapper)  { 
	
	iluError	an_error;
	
	ilu_OutputOpaque(m_call, r_opaque_wrapper.m_r_pc_buffer, 
		r_opaque_wrapper.m_card_length, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	r_opaque_wrapper.iluDeleteIfNeededOnOutput();

	return *this;
}


//////////////////////////////////////////////////////////////////
// output a object

iluBaseCall& iluBaseCall::operator<<(iluObjectWrapper& r_object_wrapper) { 
	
	iluError an_error;
	iluKernelServer the_kernel_server;
	iluClass the_class = r_object_wrapper.m_class;

	if ((iluIsSurrogateCall()) && iluCppInternal::iluGetRunningThreaded() &&
		r_object_wrapper.m_b_incremented_activation != ILUCPP_TRUE) {
		
		// if we're on the surrogate side, and this wrapper hasn't already incremented 
		// the activation count (it might have already if we're in a 'retry' due
		// to a protocol redirect)  inc the activation count on the object
		if (! iluObjectActivationTable::sm_p_object_activation_table->iluIncrementInvocations(r_object_wrapper.m_kernelobject)) {
			
			// object must be not marked active						
			// cons up and throw an error
			ILU_ERR_CONS1(inv_objref, &an_error, minor, ilu_iom_inst_nf, ilu_FALSE);			
			ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
			
		}
		// indicate in the wrapper that an incrementation was performed - it's
		// destructor will decrement it
		r_object_wrapper.m_b_incremented_activation = ILUCPP_TRUE;
	}
	
	
	// don't send anything if it's a singleton discriminator
	if ((r_object_wrapper.m_class)->cl_singleton && r_object_wrapper.m_b_discriminator)
		return *this;
	
	if (r_object_wrapper.m_kernelobject) {
		
		the_kernel_server = ilu_ServerOfObject(r_object_wrapper.m_kernelobject);
		
		ilu_EnterServer(the_kernel_server, the_class);
	}
	
	ILUCPP_DEBUG3("iluBaseCall::operator<< outputting kernel object %p of class %s\n", 
		r_object_wrapper.m_kernelobject, the_class->cl_name);
	
	ilu_OutputObjectID(m_call, r_object_wrapper.m_kernelobject, (r_object_wrapper.m_b_discriminator ? iluTRUE : iluFALSE),
		r_object_wrapper.m_class, &an_error);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);

	// adjust refcounts
	// if we're true, ensure release is done when wrapper destructs, else not
	r_object_wrapper.m_b_do_refcount_decrement = iluIsTrueCall();
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// output a sequence 

iluBaseCall& iluBaseCall::operator<<(iluSequenceWrapper& r_sequence_wrapper)  {
	
	iluError	an_error;
	
	ilu_OutputSequence(m_call, *(r_sequence_wrapper.m_p_card_count), 0, (iluType)ILU_NIL, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// output a union

iluBaseCall& iluBaseCall::operator<<(iluUnionWrapper& r_union_wrapper)  { 
	
	iluError	an_error;
	
	ilu_OutputUnion(m_call, r_union_wrapper.m_card_discriminator, 
		r_union_wrapper.m_typekind, (iluType)ILU_NIL, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}



//////////////////////////////////////////////////////////////////
// sizing primitives
//////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////
// returns ILUCPP_TRUE if the call needs sizing operations

ILUCPP_BOOL   iluBaseCall::iluNeedsSizing() {
	return (ilu_CallNeedsSizing(m_call) ? ILUCPP_TRUE : ILUCPP_FALSE);
}


//////////////////////////////////////////////////////////////////
// size an array

iluBaseCall& iluBaseCall::iluSizeArray (iluCardinal card_array_length)  { 	
	
	iluError an_error;
	
	m_card_size += ilu_SizeOfArray(m_call, card_array_length, (iluType)ILU_NIL, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}

//////////////////////////////////////////////////////////////////
// size a byte

iluBaseCall& iluBaseCall::operator+=(iluByte a_byte) { 
	
	iluError an_error;
	
	m_card_size += ilu_SizeOfByte(m_call, a_byte, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// size a boolean

iluBaseCall& iluBaseCall::operator+=(iluBoolWrapper& r_boolean_wrapper) { 
	
	iluError an_error;
	iluBoolean  b_value = (r_boolean_wrapper.m_r_bool ? iluTRUE : iluFALSE);	
	m_card_size += ilu_SizeOfBoolean(m_call, b_value, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// size an optional

iluBaseCall& iluBaseCall::operator+=(iluOptionalWrapper& r_optional_wrapper) { 
	
	iluError an_error;
	iluBoolean b_present = (r_optional_wrapper.m_present ? iluTRUE : iluFALSE);
	m_card_size += ilu_SizeOfOptional(m_call, b_present, (iluType)ILU_NIL, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// size a cardinal

iluBaseCall& iluBaseCall::operator+=(iluCardinal a_cardinal) { 
	
	iluError an_error;
	
	m_card_size += ilu_SizeOfCardinal(m_call, a_cardinal, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// size a character

iluBaseCall& iluBaseCall::operator+=(iluCharacterWrapper& r_character_wrapper) { 
	
	iluError an_error;
	
	m_card_size += ilu_SizeOfCharacter(m_call, r_character_wrapper.m_r_shortcardinal, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// size a ShortCharacter

iluBaseCall& iluBaseCall::operator+=(iluShortCharacter a_shortcharacter) { 
	
	iluError an_error;
	
	m_card_size += ilu_SizeOfShortCharacter(m_call, a_shortcharacter, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// size an enum

iluBaseCall& iluBaseCall::operator+=(iluEnumWrapper& r_enum) { 
	
	iluError an_error;
	iluShortCardinal scard_temp = r_enum.m_r_enum;

	m_card_size += ilu_SizeOfEnum(m_call, scard_temp, (iluType)ILU_NIL, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// size an integer

iluBaseCall& iluBaseCall::operator+=(iluInteger a_integer) {	
	
	iluError an_error;
	
	m_card_size += ilu_SizeOfInteger(m_call, a_integer, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// size a real

iluBaseCall& iluBaseCall::operator+=(iluReal a_real) { 
	
	iluError an_error;
	
	m_card_size += ilu_SizeOfReal(m_call, a_real, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// size a ShortCardinal

iluBaseCall& iluBaseCall::operator+=(iluShortCardinal a_shortcardinal) {	
	
	iluError an_error;
	
	m_card_size += ilu_SizeOfShortCardinal(m_call, a_shortcardinal, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// size a ShortInteger

iluBaseCall& iluBaseCall::operator+=(iluShortInteger a_shortinteger) {	
	
	iluError an_error;
	
	m_card_size += ilu_SizeOfShortInteger(m_call, a_shortinteger, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// size a ShortReal

iluBaseCall& iluBaseCall::operator+=(iluShortReal a_shortreal) {	
	
	iluError an_error;
	
	m_card_size += ilu_SizeOfShortReal(m_call, a_shortreal, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// size a LongCardinal

iluBaseCall& iluBaseCall::operator+=(iluLongCardinal& r_long_cardinal) {	
	
	iluError an_error;
	
	m_card_size += ilu_SizeOfLongCardinal(m_call, r_long_cardinal, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// size a LongInteger

iluBaseCall& iluBaseCall::operator+=(iluLongInteger& r_longinteger) {	
	
	iluError an_error;
	
	m_card_size += ilu_SizeOfLongInteger(m_call, r_longinteger, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// size a LongReal

iluBaseCall& iluBaseCall::operator+=(iluLongReal& r_longreal) {	
	
	iluError an_error;
	
	m_card_size += ilu_SizeOfLongReal(m_call, r_longreal, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// size a String

iluBaseCall& iluBaseCall::operator+=(iluCStringWrapper& r_cstring_wrapper) {	
	
	iluError an_error;
	
	m_card_size += ilu_SizeOfString(m_call, r_cstring_wrapper.m_r_cstring, 
		r_cstring_wrapper.m_r_card_length, 0, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// size a CStringVec

iluBaseCall& iluBaseCall::operator+=(iluCStringVecWrapper& r_cstringvec_wrapper) {	
	
	iluError an_error;
	
	m_card_size += ilu_SizeOfStringVec(m_call, r_cstringvec_wrapper.m_r_cstringvec, 
		r_cstringvec_wrapper.m_card_length, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// size a WString

iluBaseCall& iluBaseCall::operator+=(iluWStringWrapper& r_wstring_wrapper) {	
	
	iluError an_error;
	
	m_card_size += ilu_SizeOfWString(m_call, r_wstring_wrapper.m_r_wstring, 
		r_wstring_wrapper.m_r_card_length, 0, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// size a WStringVec

iluBaseCall& iluBaseCall::operator+=(iluWStringVecWrapper& r_wstringvec_wrapper) {	
	
	iluError an_error;
	
	m_card_size += ilu_SizeOfWStringVec(m_call, r_wstringvec_wrapper.m_r_wstringvec, 
		r_wstringvec_wrapper.m_card_length, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// size bytes

iluBaseCall& iluBaseCall::operator+=(iluBytesWrapper& r_bytes_wrapper) {	
	
	iluError an_error;
	
	m_card_size += ilu_SizeOfBytes(m_call, r_bytes_wrapper.m_r_pc_buffer, 
		r_bytes_wrapper.m_r_card_length, 0, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// size an opaque

iluBaseCall& iluBaseCall::operator+=(iluOpaqueWrapper& r_opaque_wrapper) {	
	
	iluError an_error;
	
	m_card_size += ilu_SizeOfOpaque(m_call, r_opaque_wrapper.m_r_pc_buffer,
		r_opaque_wrapper.m_card_length, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// size an object

iluBaseCall& iluBaseCall::operator+=(iluObjectWrapper& r_object_wrapper) { 
	
	// don't add anything if it's a singleton discriminator
	if ((r_object_wrapper.m_class)->cl_singleton && r_object_wrapper.m_b_discriminator)
		return *this;
	
	iluError an_error;
	iluKernelServer the_kernel_server;
	iluClass the_class = r_object_wrapper.m_class;

	if (r_object_wrapper.m_kernelobject) {
		
		the_kernel_server = ilu_ServerOfObject(r_object_wrapper.m_kernelobject);
		
		ilu_EnterServer(the_kernel_server, the_class);
	}
	
	m_card_size += ilu_SizeOfObjectID(m_call, r_object_wrapper.m_kernelobject,  
				    (r_object_wrapper.m_b_discriminator ? iluTRUE : iluFALSE), r_object_wrapper.m_class, &an_error);
	
	if (r_object_wrapper.m_kernelobject) {
		ilu_ExitServer(the_kernel_server, the_class);
	}
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}

iluBaseCall& iluBaseCall::operator+=(iluObject* p_iluobject) { 
	
	// note this call shouldn't be used to size discriminators
	
	iluError an_error;
	iluKernelServer the_kernel_server;
	iluClass the_class = NULL;

	if (p_iluobject && p_iluobject->iluGetKernelObject()) {
		the_kernel_server = ilu_ServerOfObject(p_iluobject->iluGetKernelObject());
		the_class = p_iluobject->iluGetClassRecord();
		
		ilu_EnterServer(the_kernel_server, the_class);
	}
	
	m_card_size += ilu_SizeOfObjectID(m_call, p_iluobject->iluGetKernelObject(),  
				    iluFALSE, the_class, &an_error);
	
	if (the_class) { // we know we entered the server 
		ilu_ExitServer(the_kernel_server, the_class);
	}
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}

//////////////////////////////////////////////////////////////////
// size a sequence

iluBaseCall& iluBaseCall::operator+=(iluSequenceWrapper& r_sequence_wrapper)  {
	
	iluError	an_error;
	
	m_card_size += ilu_SizeOfSequence(m_call, *(r_sequence_wrapper.m_p_card_count), 0, (iluType)ILU_NIL, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}


//////////////////////////////////////////////////////////////////
// size a union

iluBaseCall& iluBaseCall::operator+=(iluUnionWrapper& r_union_wrapper) { 
	
	iluError	an_error;
	
	m_card_size += ilu_SizeOfUnion(m_call, r_union_wrapper.m_card_discriminator, 
		r_union_wrapper.m_typekind, (iluType)ILU_NIL, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return *this;
}



//////////////////////////////////////////////////////////////////
// Mode related
//////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////
// Used immediately after entering of reply mode - returns ILUCPP_TRUE if an
// exception is present, and sets r_card_exception_number to 1 +
// the index into method's exception vector  

ILUCPP_BOOL iluBaseCall::iluExceptionPresent(iluCardinal& r_card_exception_number) {
	
	ILUCPP_DEBUG2("iluBaseCall::iluExceptionPresent() %lu\n", m_card_exception_number);
	
	if (m_card_exception_number == 0) 
		return ILUCPP_FALSE;
	
	r_card_exception_number = m_card_exception_number;
	
	return ILUCPP_TRUE;
}


//////////////////////////////////////////////////////////////////
// begin sizing exception

iluBaseCall& iluBaseCall::iluSizeExceptionMode (iluCardinal card_exception_number) {
	
	iluError an_error;
	
	ILUCPP_DEBUG1("iluBaseCall::iluSizeExceptionMode() entered\n");
	
	// going to size exception parameters need exception number, true side
	// save our exception number
	m_card_exception_number = card_exception_number;
	
	m_card_size += ilu_BeginSizingException(m_call, m_card_exception_number, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	m_state = ilucall_sending_exception; // advance our state
	
	return *this;
}



//////////////////////////////////////////////////////////////////
// Manipulators
//////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////
// end a sequence

ILU_RUNTIME_PUBLIC iluBaseCall& iluEndSequence (iluBaseCall& r_basecall) { 	
	
	iluError an_error;
	
	ILUCPP_DEBUG1("iluBaseCall::iluEndSequence() called\n");
	
	ilu_EndSequence(r_basecall.iluGetCall(), &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return r_basecall;
}

//////////////////////////////////////////////////////////////////
// end a union

ILU_RUNTIME_PUBLIC iluBaseCall& iluEndUnion (iluBaseCall& r_basecall) { 	
	
	iluError an_error;
	
	ILUCPP_DEBUG1("iluBaseCall::iluEndUnion() called\n");
	
	ilu_EndUnion(r_basecall.iluGetCall(), &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return r_basecall;
}

//////////////////////////////////////////////////////////////////
// end an Array

ILU_RUNTIME_PUBLIC iluBaseCall& iluEndArray (iluBaseCall& r_basecall) { 	
	
	iluError an_error;
	
	ILUCPP_DEBUG1("iluBaseCall::iluEndArray() called\n");
	
	ilu_EndArray(r_basecall.iluGetCall(), &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return r_basecall;
}


//////////////////////////////////////////////////////////////////
// size a record

ILU_RUNTIME_PUBLIC iluBaseCall& iluSizeRecord (iluBaseCall& r_basecall) { 	
	
	iluError an_error;
	
	ILUCPP_DEBUG1("iluBaseCall::iluSizeRecord() called\n");
	
	r_basecall.iluAddToSize (ilu_SizeOfRecord(r_basecall.iluGetCall(), (iluType)ILU_NIL, &an_error));
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return r_basecall;
}


//////////////////////////////////////////////////////////////////
// end a record

ILU_RUNTIME_PUBLIC iluBaseCall& iluEndRecord (iluBaseCall& r_basecall) { 	
	
	iluError an_error;
	
	ILUCPP_DEBUG1("iluBaseCall::iluEndRecord() called\n");
	
	ilu_EndRecord(r_basecall.iluGetCall(), &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return r_basecall;
}


//////////////////////////////////////////////////////////////////
// input an array

ILU_RUNTIME_PUBLIC iluBaseCall& iluInputArray (iluBaseCall& r_basecall)  { 	
	
	iluError an_error;
	
	ilu_InputArray(r_basecall.iluGetCall(), (iluType)ILU_NIL, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return r_basecall;
}


//////////////////////////////////////////////////////////////////
// input a record

ILU_RUNTIME_PUBLIC iluBaseCall& iluInputRecord (iluBaseCall& r_basecall) { 	
	
	iluError an_error;
	
	ilu_InputRecord(r_basecall.iluGetCall(), (iluType)ILU_NIL, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return r_basecall;
}


//////////////////////////////////////////////////////////////////
// output a record

ILU_RUNTIME_PUBLIC iluBaseCall& iluOutputRecord (iluBaseCall& r_basecall) {	
	
	iluError an_error;
	
	ilu_OutputRecord(r_basecall.iluGetCall(), (iluType)ILU_NIL, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return r_basecall;
}


//////////////////////////////////////////////////////////////////
// Start a request

ILU_RUNTIME_PUBLIC iluBaseCall& iluRequestMode (iluBaseCall& r_basecall) { 
	
	iluError		an_error;
	
	ILUCPP_DEBUG1("iluBaseCall::iluRequestMode() entered\n");
	
	ilu_StartRequest(r_basecall.iluGetCall(), r_basecall.iluGetSize(), &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	r_basecall.iluSetState(ilucall_sending_request);  // advance state
	
	return r_basecall;
	
}


//////////////////////////////////////////////////////////////////
// Finish a request

ILU_RUNTIME_PUBLIC iluBaseCall& iluRequestSentMode (iluBaseCall& r_basecall) { 
	
	iluError		an_error;
	
	ILUCPP_DEBUG1("iluBaseCall::iluRequestSentMode() entered\n");
	
	ilu_FinishRequest(r_basecall.iluGetCall(), &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return r_basecall;
}



//////////////////////////////////////////////////////////////////
// Get a Reply

ILU_RUNTIME_PUBLIC iluBaseCall& iluGetReplyMode (iluBaseCall& r_basecall) { 
	
	iluError		an_error;
	iluCardinal		card_successcode;
	iluProtocolException a_protocol_exception;
	iluConnection	p_new_connection = NULL;
	
	ILUCPP_DEBUG1("iluBaseCall::iluGetReplyMode() entered\n");
	
	a_protocol_exception = ilu_GetReply(r_basecall.iluGetCall(), &card_successcode, &p_new_connection, &an_error);

	if (p_new_connection) {
		// note that p_new_connection non-nil can only happen in threaded case
		iluCppInternal::iluThrowingFork(iluCppInternal::iluMonitorOutgoingConnection, p_new_connection);
	}

	// see if we got a transient 'retry error' (due to a protocol redirect)
	if ((an_error.ilu_type == ILU_ERRTYP(transient))  &&
		(ILU_ERRSEL(transient, an_error).minor == ilu_tm_retry)) {

		// free up the error
		ILU_HANDLED(an_error);

		// indicate that we had a retry
		r_basecall.iluSetRetryCall();

		r_basecall.iluSetState(ilucall_initial);  // reset state

		return r_basecall;
	}

	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	if (a_protocol_exception != ilu_ProtocolException_Success) {
		
		// Note: How come protocol exceptions are the same things as errors???
		ILUCPP_DEBUG2("Error Warning: iluGetReplyMode got a protocol exception %u\n", a_protocol_exception);
		ilu_MapProtocolExceptionToError (a_protocol_exception, &an_error, ILU_NIL);
		ILUCPP_DEBUG1("iluGetReplyMode consed an error, about to throw it\n");
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	}
	
	if(card_successcode != 0) {
		// it's an exception being returned
		ILUCPP_DEBUG1("iluBaseCall::iluGetReplyMode() an exception is being returned\n");
		r_basecall.iluSetExceptionNumber(card_successcode);
	}
	
	r_basecall.iluSetState(ilucall_getting_reply); // advance state
	
	return r_basecall;
}



//////////////////////////////////////////////////////////////////
// when you're done reading in a request

ILU_RUNTIME_PUBLIC iluBaseCall& iluParametersFinishedMode (iluBaseCall& r_basecall) { 
	
	iluError		an_error;
	iluCall			the_call = r_basecall.iluGetCall();
	iluConnection	the_connnection = ilu_ConnectionOfCall(the_call);
	
	ILUCPP_DEBUG1("iluBaseCall::iluParametersFinishedMode() entered\n");
	
	// we're done reading in the request
	ilu_RequestRead(r_basecall.iluGetCall(), &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	r_basecall.iluSetState(ilucall_request_read); // advance our state
	
	if (ilu_ThreadPerRequest(the_connnection))
		iluCppInternal::iluEnableRequests(the_call, the_connnection);
	
	return r_basecall;
}


//////////////////////////////////////////////////////////////////
// after the true method call, begin sizing reply args

ILU_RUNTIME_PUBLIC iluBaseCall& iluSizeReplyMode (iluBaseCall& r_basecall) { 
	
	iluError		an_error;
	iluCardinal		card_reply_overhead;
	
	ILUCPP_DEBUG1("iluBaseCall::iluSizeReplyMode() entered\n");
	
	card_reply_overhead = ilu_BeginSizingReply(r_basecall.iluGetCall(), (r_basecall.iluCanRaiseExceptions() ? iluTRUE : iluFALSE), &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);

	r_basecall.iluAddToSize(card_reply_overhead);
	
	r_basecall.iluSetState(ilucall_sending_reply); // advance our state
	
	return r_basecall;
}


//////////////////////////////////////////////////////////////////
// after sizing reply values, actually send them

ILU_RUNTIME_PUBLIC iluBaseCall& iluSendReplyMode (iluBaseCall& r_basecall) {
	
	iluError		an_error;
	iluCall		the_call = r_basecall.iluGetCall();
	iluConnection	the_connnection = ilu_ConnectionOfCall(the_call);
	
	ILUCPP_DEBUG1("iluBaseCall::iluSendReplyMode() entered\n");
	
	if (ilu_ThreadPerRequest(the_connnection))
		iluCppInternal::iluDisableRequests(the_call, the_connnection);
	
	ilu_BeginReply(r_basecall.iluGetCall(), (r_basecall.iluCanRaiseExceptions() ? iluTRUE : iluFALSE), r_basecall.iluGetSize(), &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	r_basecall.iluSetState(ilucall_sending_reply); // advance our state
	
	return r_basecall;
}


//////////////////////////////////////////////////////////////////
// begin exception

ILU_RUNTIME_PUBLIC iluBaseCall& iluSendExceptionMode (iluBaseCall& r_basecall) { 
	
	iluError		an_error;
	iluCall			the_call = r_basecall.iluGetCall();
	iluConnection	the_connnection = ilu_ConnectionOfCall(the_call);
	
	ILUCPP_DEBUG1("iluBaseCall::iluSendExceptionMode() entered\n");
	
	if (ilu_ThreadPerRequest(the_connnection))
		iluCppInternal::iluDisableRequests(the_call, the_connnection);
	
	ilu_BeginException(the_call, r_basecall.iluGetExceptionNumber(), 
		r_basecall.iluGetSize(), &an_error);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	r_basecall.iluSetState(ilucall_sending_exception); // advance our state
	
	return r_basecall;
}



//////////////////////////////////////////////////////////////////
// iluTrueCall is meant for use by true stubs only. It encapsulates 
// some of the standard control operations for incoming ILU calls
//////////////////////////////////////////////////////////////////


// Constructor and destructor
iluTrueCall::iluTrueCall(iluCall a_call, ILUCPP_BOOL b_can_raise_exceptions) : iluBaseCall(a_call, b_can_raise_exceptions) { 
	
	// set up the passport
	ilu_Passport call_passport = ilu_CallerPassportOfCall(m_call);
	m_p_passport = new iluPassport(call_passport);
	iluPassport::iluSetCallerPassport(m_p_passport);

	ILUCPP_DEBUG1("iluTrueCall::iluTrueCall constructed\n");	
}


//////////////////////////////////////////////////////////////////
// destructor - perform proper termination depending on the state we got to

iluTrueCall::~iluTrueCall() {
	
	iluError an_error;
	ILU_CLER(an_error);

	ILUCPP_DEBUG1("iluTrueCall::~iluTrueCall destructor entered\n");
	
	switch (m_state) {
		
	case ilucall_initial:				// ERROR must have had bad input params
		
		ILUCPP_DEBUG1("iluTrueCall::~iluTrueCall() state ilucall_initial\n");
		ILUCPP_ASSIGN_ERROR(m_error_type, an_error);
		break;
				
	case ilucall_request_read: 
		// Could be an Asynchronous method call, or an exception occurred 
		// that the method wasn't declared to return (e.g. a system
		// exception) during actual true method call.  Note that in the 
		// async case, the  subsequent call to ILUCPP_ASSIGN_ERROR
		// will be with m_error_type set to ILU_ERRTYP(success) from when 
		// the call object was constructed, so we're not really saying an
		// error occurred.

		ILUCPP_DEBUG1("iluTrueCall::~iluTrueCall() state ilucall_request_read\n");
		ILUCPP_ASSIGN_ERROR(m_error_type, an_error);
		break;
		
	case ilucall_sending_reply:					// /sizing/sending reply parameters, true side
		
		ILUCPP_DEBUG1("iluTrueCall::~iluTrueCall() state ilucall_sending_reply\n");
		
		// finish off the reply if we didn't get here due to an error
		if (m_error_type == ILU_ERRTYP(success)) {
			ilu_FinishReply(m_call, &an_error);
			ILUCPP_ERRWARN(an_error, "iluTrueCall::~iluTrueCall() calling ilu_FinishReply", ILUCPP_FALSE);
		}
		else {
			ILUCPP_ASSIGN_ERROR(m_error_type, an_error);
		}
		break;
		
	case ilucall_sending_exception:			// sending exception parameters, true side
		ILUCPP_DEBUG1("iluTrueCall::~iluTrueCall() state ilucall_sending_exception\n");
		
		// finish off the exception if we didn't get here due to an error
		if (m_error_type == ILU_ERRTYP(success)) {
			ilu_FinishException(m_call, &an_error);
			ILUCPP_ERRWARN(an_error, "iluTrueCall::~iluTrueCall() calling ilu_FinishException", ILUCPP_FALSE);
		}
		else {
			ILUCPP_ASSIGN_ERROR(m_error_type, an_error);
		}
		break;
		
	default:
		_ilu_Assert(ilu_FALSE, "iluTrueCall::~iluTrueCall() - bad iluCallState");
	}
	
	ILUCPP_DEBUG1("iluTrueCall::~iluTrueCall() finishing call\n");
	
	// finish the call
	ilu_FinishCall(m_call, &an_error);
	ILUCPP_ERRWARN(an_error, "iluTrueCall::~iluTrueCall() calling ilu_FinishCall", ILUCPP_TRUE);
	ILUCPP_DEBUG2("iluTrueCall::~iluTrueCall() - finished call %p\n", m_call);
	
	// free the passport of any
	delete m_p_passport;

	if (!(m_call->ca_reqs_enabled))
		// turn requests back on for the connection
		iluCppInternal::iluEnableRequests(m_call, ilu_ConnectionOfCall(m_call));
	
	ILUCPP_DEBUG1("iluTrueCall::~iluTrueCall() destruction completed\n");
	
}



//////////////////////////////////////////////////////////////////
// iluSurrogateCall is meant for use by surrogate stubs only.  It 
// encapsulates some of the standard control operations for outgoing 
// ILU calls
//////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////
// Constructor

iluSurrogateCall::iluSurrogateCall(iluCall a_call, iluCardinal card_method_number, 
								   iluObject* p_discriminator_object, iluClass p_class) : iluBaseCall(a_call)  {
	
	iluClass		the_class = p_class;
	iluMethod		the_method = the_class->cl_methods + card_method_number;
	iluKernelServer	the_kernel_server = ilu_ServerOfObject(p_discriminator_object->iluGetKernelObject());
	iluConnection	new_connection;
	iluError		an_error;
	
	ILUCPP_DEBUG1("iluSurrogateCall::iluSurrogateCall() constructor starting call\n");

	iluPassport* p_the_cpp_passport = iluPassport::iluGetPassport();
	ilu_Passport the_kernel_passport = (p_the_cpp_passport ? p_the_cpp_passport->iluGetIluPassport() : NULL);
	m_b_call_started = (ilu_StartCall(a_call, the_kernel_server, the_class, the_method, 
		iluCppInternal::iluGetCppLanguageIndex(), the_kernel_passport, &new_connection, &an_error) ? ILUCPP_TRUE : ILUCPP_FALSE);
	
	if (new_connection != NULL)
		// we got a new connection, so fork a monitoring thread
		// Note that new connections will only be returned in a multi-threaded runtime
		iluCppInternal::iluThrowingFork(iluCppInternal::iluMonitorOutgoingConnection, new_connection);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
}



//////////////////////////////////////////////////////////////////
// Destructor

iluSurrogateCall::~iluSurrogateCall() {

	iluError an_error;
	ILU_CLER(an_error);
	ILUCPP_BOOL b_call_ok;
	
	ILUCPP_DEBUG1("iluSurrogateCall::~iluSurrogateCall destructor entered\n");
	
	switch (m_state) {  // showing all states here for clarity
		
	case ilucall_getting_reply:	// get reply called, reading in results/exception
		
		ILUCPP_DEBUG1("iluSurrogateCall::~iluSurrogateCall() state ilucall_getting_reply\n");
		
		// finish off the replyread  if we didn't get here due to an error
		if (m_error_type == ILU_ERRTYP(success)) {			
			ilu_ReplyRead(m_call, &an_error);
			ILUCPP_ERRWARN(an_error, "iluSurrogateCall::~iluSurrogateCall()", ILUCPP_FALSE);
		}
		
		break;
		
	case ilucall_initial:			// must have had a problem starting call or sizing
		
		break;
		
	case ilucall_sending_request:	// sending request arguments
	
		break;
		
	default:
		_ilu_Assert(ilu_FALSE, "iluSurrogateCall::~iluSurrogateCall() - bad iluCallState");
	}
	
	// assign any error we may have in the call to an_error (so that it
	// can be appropriately passed to ilu_FinishCall if necessary)
	ILUCPP_ASSIGN_ERROR(m_error_type, an_error);
		
	b_call_ok = ILU_ERROK(an_error) ? ILUCPP_TRUE : ILUCPP_FALSE;

	// at this point, if an_error is OK, then we must have had a successful call
	// so far, else there must have been an exception along the way that the stub 
	// will be throwing
	
	ILUCPP_DEBUG1("iluSurrogateCall::~iluSurrogateCall() finishing call\n");
	
	if (m_b_call_started) {		// if the call was started, we must finish it
		ilu_FinishCall(m_call, &an_error);
		
		if (b_call_ok) {
			// if everything in the call was OK then allow the ilu_FinishCall,
			// result to throw an exception representing ilu_FinishCall's problem if needed
			// note that this gives preference to any exception thrown by the stub
			ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
		}
	}
	
	ILUCPP_DEBUG1("iluSurrogateCall::~iluSurrogateCall() destruction completed\n");
}


#ifdef ADD_VARIANT_SUPPORT

//////////////////////////////////////////////////////////////////
// iluPickleCall is meant for use by Corba any insertion and extraction
// only
//////////////////////////////////////////////////////////////////


// Constructor
iluPickleCall::iluPickleCall(iluCall a_call, void* p_typecode, iluPickle* p_pickle) : 
	iluBaseCall(a_call), m_p_typecode(p_typecode), m_p_pickle(p_pickle) { 

	iluError an_error;
	ILU_CLER(an_error);

	ilu_StartPickle (m_call, (iluType)ILU_NIL, &an_error);

	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);

	ILUCPP_DEBUG1("iluPickleCall::iluPickleCall constructed\n");	
}


iluPickleCall& iluPickleCall::iluOutputPickle () {

	iluError		an_error;

	ilu_WritePickle (m_call, m_card_size, (char*)(((NAME_OUTSIDE_SCOPE(CORBA, TypeCode_ptr)) m_p_typecode)->id()), &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);

	return *this;
}


iluPickleCall& iluPickleCall::iluInputPickle () {

	iluError		an_error;

	ilu_ReadPickle (m_call, *m_p_pickle, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);

	return *this;
}

iluPickleCall& iluPickleCall::iluEndPickle () {

	iluError		an_error;

	ilu_EndPickle (m_call, m_p_pickle, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);

	return *this;
}

#endif /* ADD_VARIANT_SUPPORT */

//////////////////////////////////////////////////////////////////
// End of File
//////////////////////////////////////////////////////////////////


