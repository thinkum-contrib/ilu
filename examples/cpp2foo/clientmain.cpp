/**
$Id: clientmain.cpp,v 1.22 1999/09/16 03:16:02 pnewman Exp $
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
///////////////////////////////////////////////////////////////////////
//  foo client side program for corba2 cpp2 c++ ilu 
// Last edited by Mike Spreitzer October 8, 1998 10:39 pm PDT

#include <stdio.h>
#include <iostream.h>
#include <math.h>

#ifdef WIN32
#include <winsock.h>
#else
extern "C" int gethostname(char *name, int namelen);
#endif

#include "foo-cppsurrogate.hpp"


// msvc seems to have a problem calling a superclasses virtual member function
// when in nested classes or namespace mode
#if (defined(_MSC_VER) && (defined(CPLUSPLUSMAPPING_NAMESPACES) || defined(CPLUSPLUSMAPPING_NESTEDCLASSES)))
#define SKIP_CUSTOM_SURROGATE_TEST 1
#endif


///////////////////////////////////////////////////////////////////////
// Usage string

char g_c_usage[] = 
"Usage:  fooclient [ HOSTNAME [mt]]\n\
\tHOSTNAME - used as part of the id for the server - defaults to this host\n\
\tmt - if present, sets ilu to multithreaded\n";


///////////////////////////////////////////////////////////////////////
// Show a system exception

void show_system_exception( const CORBA(SystemException)& the_exception) {
	cout << "Got SystemException " << the_exception.exception_name() << ", " <<
		"minor = " << the_exception.minor() << " (" << the_exception.minor_desc() << "), completed = ";
	switch (the_exception.completed()) {
	case CORBA(COMPLETED_YES): cout << "YES"; break;
	case CORBA(COMPLETED_NO): cout << "NO"; break;
	case CORBA(COMPLETED_MAYBE): cout << "MAYBE"; break;
	default: cout << "??? unknown exception completion !!!";
	}
	cout << endl;
}


#ifndef SKIP_CUSTOM_SURROGATE_TEST

///////////////////////////////////////////////////////////////////////
//  check out ability to do 'filtering' by supplying our own surrogate subclass

class foo_bar_specialized_surrogate : public virtual foo_surrogate(bar) {
	
public:
	
	// override the zap function
	virtual CORBA(Boolean) zap ( CORBA(Long) inarg,
        CORBA(Octet)& inoutarg,
        CORBA(Double)& outarg
		) throw (CORBA(SystemException), foo(zapexception));
	
	// Function used to create a foo_bar_specialized_surrogate
	static iluObject* create_surrogate (iluKernelObject a_kernel_object);
	
};


// Create a foo_bar_specialized_surrogate
iluObject* foo_bar_specialized_surrogate::create_surrogate (iluKernelObject a_kernel_object) {
	
	foo_bar_specialized_surrogate* p_new_object = new foo_bar_specialized_surrogate();
	p_new_object->iluAssociateKernelObject(a_kernel_object);
	return p_new_object;
}


// implementation of zap for our specialized surrogate
// which merely prints out a message and calls it's 
// parent class implementation.. In a real application, such an
// override might do logging, cacheing, filtering, whatever.
CORBA(Boolean) foo_bar_specialized_surrogate::zap (
												   CORBA(Long) inarg,
												   CORBA(Octet)& inoutarg,
												   CORBA(Double)& outarg
												   ) throw (CORBA(SystemException), foo(zapexception)) {
	
	cout << "foo_bar_specialized_surrogate zap method calling parent's zap method" << endl;
	return foo_surrogate(bar)::zap(inarg, inoutarg, outarg);
}

#endif // SKIP_CUSTOM_SURROGATE_TEST


///////////////////////////////////////////////////////////////////////
// check out Object <=> String

CORBA(Boolean) doobjectandstring(foo(bar_var)& mybar_var) {
	
	CORBA(Boolean) b_return = ILUCPP_TRUE;
	char* pc_stringified_reference = NULL;
	
	pc_stringified_reference = mybar_var->iluObjectToString();
	if (!pc_stringified_reference) {
		cout << "iluObjectToString failed" << endl;
		return ILUCPP_FALSE;
	}
	else {
		iluObject* p_ilu_object = iluObject::iluStringToObject(pc_stringified_reference);
		iluCppRuntime::iluFree(pc_stringified_reference);
		if (!p_ilu_object) {
			cout << "iluObject::iluStringToObject failed" << endl;
			return ILUCPP_FALSE;
		}
		else {
			foo(bar_ptr) p_bar_object = foo(bar)::_narrow(p_ilu_object);
			if (p_bar_object != ((foo(bar_ptr))mybar_var)) {
				cout << "p_bar_object != ((foo(bar_ptr))mybar_var)" << endl;
				b_return = ILUCPP_FALSE;
			}
			else 
				cout << "Object <=> String succeeded" << endl;
			p_bar_object->_release();
		}
	}
	
	return b_return;
}


///////////////////////////////////////////////////////////////////////
// check out Ping

CORBA(Boolean) doping(foo(bar_var)& mybar_var) {
	return mybar_var->iluPing();
}

	
///////////////////////////////////////////////////////////////////////
// call the zap method (primitive types)

CORBA(Boolean) dozap(CORBA(Long) inarg, foo(bar_var)& mybar_var, CORBA(Boolean) b_verbose = ILUCPP_TRUE) {
	
	CORBA(Boolean) bool_return_value;
	CORBA(Octet)  inoutarg = 0xaa;
	CORBA(Double) outarg;
	CORBA(Boolean) b_return = ILUCPP_TRUE;
	
	try {
		bool_return_value = mybar_var->zap(inarg, inoutarg, outarg);
	}
	catch (const foo(zapexception)& the_exception) {
		cout << "dozap Got foo::zapexception, value is " << the_exception._value() << endl;
		if (the_exception._value() != inarg)
			return ILUCPP_FALSE;
		return ILUCPP_TRUE;
	}
	catch (const CORBA(SystemException)& the_exception) {
		show_system_exception (the_exception);		
		return ILUCPP_FALSE;
	}
	catch (...) {
		cout << "dozap Got some sort of exception" << endl;
		return ILUCPP_FALSE;
	}
	
	if (outarg != 3.14159) 
		b_return = ILUCPP_FALSE;
	if (inoutarg != (CORBA(Octet)(~ 0xaa)))
		b_return = ILUCPP_FALSE;
	if (bool_return_value != ILUCPP_TRUE) 
		b_return = ILUCPP_FALSE;
	
	if (b_verbose)
		cout << "zap returned " 
		<< ((bool_return_value == ILUCPP_TRUE) ? "True" : "False")
		<< ", inoutarg 0x" 
		<< hex
		<< (unsigned int) inoutarg 
		<< ", outarg " 
		<< outarg 
		<< endl ; 
	
	return b_return;
}


///////////////////////////////////////////////////////////////////////
// exerciseObject tables

CORBA(Boolean) doobjecttableexercise(foo(bar_var)& mybar_var) {
	
	CORBA(Boolean) b_return = ILUCPP_TRUE;
	char* pc_stringified_reference;
	iluCString str_encodedContactInfo;
	char* pc_sibling_reference;
	foo(bar_var) mysibling_var;
	foo(bar_ptr) mysibling_ptr;
	iluObject* p_object;

	// form an sbh for a sibling of mybar_var, with instance handle foo_instance_via_table	

	// first get a hold of the contact information for the mybar_var object,
	// bar parsing up it's string binding handle
	pc_stringified_reference = mybar_var->iluObjectToString();
	if (!pc_stringified_reference) {
		cout << "iluObjectToString failed" << endl;
		return ILUCPP_FALSE;
	}

	if (!iluCppRuntime::iluParseSBH(pc_stringified_reference, NULL, NULL, NULL, &str_encodedContactInfo)) {
		cout << "iluParseSBH failed" << endl;
		iluCppRuntime::iluFree(pc_stringified_reference);
		return ILUCPP_FALSE;
	}

	// now make up an SBH representing a sibling
	pc_sibling_reference = 
		iluCppRuntime::iluFormSBHUsingContactInfo (mybar_var->iluServerId(), "foo_instance_via_table",
			mybar_var->iluGetClassRecord(), str_encodedContactInfo);

	// can now free the pc_stringified_reference here since str_encodedContactInfo pointed into it
	// and we no longer need str_encodedContactInfo
	iluCppRuntime::iluFree(pc_stringified_reference);

	p_object = iluObject::iluStringToObject(pc_sibling_reference);

	mysibling_ptr = (p_object ? foo(bar)::_narrow(p_object) : NULL);

	if (!mysibling_ptr) {
		cout << "Couldn't get sibling from " << pc_sibling_reference << endl;
		iluCppRuntime::iluFree(pc_sibling_reference);
		return ILUCPP_FALSE;
	}

	// let the var manage it
	mysibling_var = mysibling_ptr;

	// try zap on it to see if it really works
	if (dozap(0, mysibling_var, ILUCPP_FALSE))
		cout <<  "Successful zap on sibling " << pc_sibling_reference << endl;
	else {
		cout <<  "Unsuccessful zap on sibling " << pc_sibling_reference << endl;
	b_return = ILUCPP_FALSE;
	}

	iluCppRuntime::iluFree(pc_sibling_reference);

	return b_return;
}


///////////////////////////////////////////////////////////////////////
// try calling the pass_obj method

CORBA(Boolean) dopass_obj (foo(bar_var)& mybar_var) {
	
	foo(bar_ptr) passobj_inarg = mybar_var;
	foo(bar_ptr) passobj_inoutarg = mybar_var;
	foo(bar_ptr) passobj_outarg;
	foo(bar_ptr) passobj_return;
	CORBA(Boolean) b_good = ILUCPP_FALSE;
	
	// dupicate our inout arg since we want to hang onto it
	// refcount should now be two (one for original, one for
	// the duplicate
	foo(bar)::_duplicate(passobj_inoutarg);
	
	// refcount should now be two (one for original, one for
	// the duplicate we just did for the inout)
	cout << "mybar_var->iluGetReferenceCount() = " << mybar_var->iluGetReferenceCount()  
		<< (mybar_var->iluGetReferenceCount() == 2 ? " Good" : " Bad") << endl;
	
	try {
		passobj_return = mybar_var->passobj(passobj_inarg, passobj_inoutarg, passobj_outarg);
	}	
	catch (const CORBA(SystemException)& the_exception) {
		show_system_exception (the_exception);		
		return ILUCPP_FALSE;
	}
	catch (...) {
		cout << "Got some sort of exception" << endl;
		return ILUCPP_FALSE;
	}
	
	cout << "passobj returned " <<
		(((passobj_inarg == passobj_inoutarg) && 
		(passobj_inoutarg == passobj_outarg) &&
		(passobj_outarg == passobj_return)) ? "All Match" : "Don't match!")
		<< endl ; 
	
	// refcount should now be four (one for original, and one each for
	// the inout, out and return values)
	cout << "mybar_var->iluGetReferenceCount() = " << mybar_var->iluGetReferenceCount()
		<< (mybar_var->iluGetReferenceCount() == 4 ? " Good" : " Bad") << endl;
	
	if ((passobj_inarg == passobj_inoutarg) && 
		(passobj_inoutarg == passobj_outarg) &&
		(passobj_outarg == passobj_return) &&
		(mybar_var->iluGetReferenceCount() == 4)) 
		b_good = ILUCPP_TRUE;
	
	passobj_inoutarg->_release();
	passobj_outarg->_release();
	passobj_return->_release();

	return b_good;
}


///////////////////////////////////////////////////////////////////////
// try calling the passenum method

CORBA(Boolean) dopass_enum(foo(bar_var)& mybar_var) {	
	
	foo(enumtype) passenum_inarg = foo(red);
	foo(enumtype) passenum_inoutarg = foo(orange);
	
	foo(enumtype) passenum_outarg = foo(violet); 
	foo(enumtype) passenum_return = foo(violet);
	CORBA(Boolean) b_return = ILUCPP_TRUE;

	try {
		passenum_return = mybar_var->passenum(passenum_inarg, passenum_inoutarg, passenum_outarg);
	}	
	catch (const CORBA(SystemException)& the_exception) {
		show_system_exception (the_exception);		
		return ILUCPP_FALSE;
	}
	catch (...) {
		cout << "Got some sort of exception" << endl;
		return ILUCPP_FALSE;
	}
	
	if (passenum_inoutarg != foo(yellow)) {
		cout << "passenum_inoutarg not yellow " << (int)passenum_inoutarg << endl;
		b_return = ILUCPP_FALSE;
	}
	if (passenum_outarg != foo(green)) {
		cout << "passenum_outarg not green " << (int)passenum_outarg << endl;
		b_return = ILUCPP_FALSE;
	}
	if (passenum_return != foo(blue)) {
		cout << "passenum_return not blue " << (int)passenum_return << endl;
		b_return = ILUCPP_FALSE;
	}

	cout << "passenum returned " 
		<< ((passenum_inoutarg == foo(yellow) &&
		passenum_outarg == foo(green) &&
		passenum_return == foo(blue)
		) ? " Good" : " Bad")
		<< endl ; 

	return b_return;
}


///////////////////////////////////////////////////////////////////////
// try calling the passlatinstring method

CORBA(Boolean) dopasslatinstring(foo(bar_var)& mybar_var) {
	
	foo(LatinString) passstring_inarg = CONST_CAST(iluCString, "ToServer_inarg");
	foo(LatinString) passstring_inoutarg = CORBA(string_dup)("ToServer_inoutarg");
	
	foo(LatinString) passstring_outarg; 
	foo(LatinString) passstring_return;
	CORBA(Boolean) b_return = ILUCPP_TRUE;

	try {
		passstring_return = mybar_var->passlatinstring(passstring_inarg, passstring_inoutarg, passstring_outarg);
	}	
	catch (const CORBA(SystemException)& the_exception) {
		show_system_exception (the_exception);		
		return ILUCPP_FALSE;
	}
	catch (...) {
		cout << "Got some sort of exception" << endl;
		return ILUCPP_FALSE;
	}
	
	if (strcmp(passstring_inarg, "ToServer_inarg") != 0) {
		cout << "passstring_inarg disturbed" << endl;
		b_return = ILUCPP_FALSE;
	}
	if (strcmp(passstring_inoutarg, "FromServer_inoutarg") != 0) {
		cout << "passstring_inoutarg wrong" << endl;
		b_return = ILUCPP_FALSE;
	}
	if (strcmp(passstring_outarg, "FromServer_outarg") != 0) {
		cout << "passstring_outarg wrong" << endl;
		b_return = ILUCPP_FALSE;
	}
	if (strcmp(passstring_return, "FromServer_return") != 0) {
		cout << "passstring_return wrong" << endl;
		b_return = ILUCPP_FALSE;
	}

	cout << "passstring returned " 
		<< passstring_inoutarg << ", "
		<< passstring_outarg << ", "
		<< passstring_return << endl ;
	CORBA(string_free)(passstring_inoutarg);
	CORBA(string_free)(passstring_return);

	return b_return;
}


///////////////////////////////////////////////////////////////////////
// try calling the passunicodestring method

CORBA(Boolean) dopassunicodestring(foo(bar_var)& mybar_var) {
	
	foo(UnicodeString) passustring_inarg = iluCppRuntime::iluCharStringFromShortCharString("ToServer_inarg");
	foo(UnicodeString) passustring_inoutarg = iluCppRuntime::iluCharStringFromShortCharString("ToServer_inoutarg");
	foo(UnicodeString) passustring_outarg; 
	foo(UnicodeString) passustring_return;
	CORBA(Boolean) b_return = ILUCPP_TRUE;

	try {
		passustring_return = mybar_var->passunicodestring(passustring_inarg, passustring_inoutarg, passustring_outarg);
	}	
	catch (const CORBA(SystemException)& the_exception) {
		show_system_exception (the_exception);		
		return ILUCPP_FALSE;
	}
	catch (...) {
		cout << "Got some sort of exception" << endl;
		return ILUCPP_FALSE;
	}
	
	if (! iluCppRuntime::iluCharStringShortCharStringEqual(passustring_inarg, "ToServer_inarg")) {
		cout << "passustring_inarg disturbed" << endl;
		b_return = ILUCPP_FALSE;
	}
	
	if (! iluCppRuntime::iluCharStringShortCharStringEqual(passustring_inoutarg, "FromServer_inoutarg")) {
		cout << "passustring_inoutarg wrong" << endl;
		b_return = ILUCPP_FALSE;
	}
	
	if (! iluCppRuntime::iluCharStringShortCharStringEqual(passustring_outarg, "FromServer_outarg")) {
		cout << "passustring_outarg wrong" << endl;
		b_return = ILUCPP_FALSE;
	}
	
	if (! iluCppRuntime::iluCharStringShortCharStringEqual(passustring_return, "FromServer_return")) {
		cout << "passustring_return wrong" << endl;
		b_return = ILUCPP_FALSE;
	}
	
	iluCppRuntime::iluFree(passustring_inarg);
	iluCppRuntime::iluFree(passustring_inoutarg);
	iluCppRuntime::iluFree(passustring_return);
	
	cout << "passustring returned " << (b_return ? "Good" : "Bad") << endl;

	return b_return;
}


///////////////////////////////////////////////////////////////////////
// try calling the passfixedrecord method

CORBA(Boolean) dopassfixedrecord(foo(bar_var)& mybar_var) {	
	
	foo(fixedrecordtype) passfixedrecord_inarg;
	foo(fixedrecordtype) passfixedrecord_inoutarg;
	
	foo(fixedrecordtype) passfixedrecord_outarg; 
	foo(fixedrecordtype) passfixedrecord_return;
	CORBA(Boolean) b_return = ILUCPP_TRUE;

	passfixedrecord_inarg.fixedrecordinteger = 12345;
	passfixedrecord_inarg.fixedrecordenum = foo(orange);

	passfixedrecord_inoutarg.fixedrecordinteger = 67890;
	passfixedrecord_inoutarg.fixedrecordenum = foo(yellow);

	
	try {
		passfixedrecord_return = mybar_var->passfixedrecord(passfixedrecord_inarg,
			passfixedrecord_inoutarg, passfixedrecord_outarg);
	}	
	catch (const CORBA(SystemException)& the_exception) {
		show_system_exception (the_exception);		
		return ILUCPP_FALSE;
	}
	catch (...) {
		cout << "Got some sort of exception" << endl;
		return ILUCPP_FALSE;
	}
	
	// outarg should be copy of inarg
	if (12345 != passfixedrecord_outarg.fixedrecordinteger) {
		cout << "passfixedrecord_outarg.fixedrecordinteger bad " << endl;
		b_return = ILUCPP_FALSE;
	}
	if (foo(orange) != passfixedrecord_outarg.fixedrecordenum) {
		cout << "passfixedrecord_outarg.fixedrecordenum bad " << endl;
		b_return = ILUCPP_FALSE;
	}
	//  return should be what inout arg was
	if (67890 != passfixedrecord_return.fixedrecordinteger) {
		cout << "passfixedrecord_return.fixedrecordinteger bad " << endl;
		b_return = ILUCPP_FALSE;
	}
	if (foo(yellow) != passfixedrecord_return.fixedrecordenum) {
		cout << "passfixedrecord_return.fixedrecordenum bad " << endl;
		b_return = ILUCPP_FALSE;
	}

	//  inout arg should be one more in each field
	if (67891 != passfixedrecord_inoutarg.fixedrecordinteger) {
		cout << "passfixedrecord_inoutarg.fixedrecordinteger bad " << endl;
		b_return = ILUCPP_FALSE;
	}
	if (foo(green) != passfixedrecord_inoutarg.fixedrecordenum) {
		cout << "passfixedrecord_inoutarg.fixedrecordenum bad " << endl;
		b_return = ILUCPP_FALSE;
	}

	if (b_return)
		cout << "passfixedrecord succeeded" << endl;

	return b_return;
}


///////////////////////////////////////////////////////////////////////
// try calling the passvariablerecord method

CORBA(Boolean) dopassvariablerecord(foo(bar_var)& mybar_var) {	
	
	foo(variablerecordtype) passvariablerecord_inarg;
	foo(variablerecordtype) passvariablerecord_inoutarg;
	
	foo(variablerecordtype)* p_passvariablerecord_outarg; 
	foo(variablerecordtype)* p_passvariablerecord_return;
	CORBA(Boolean) b_return = ILUCPP_TRUE;
	char instringvalue[] = "passvariablerecord_inarg";
	char inoutstringvalue[] = "passvariablerecord_inoutarg";
	char* p_instringvalue = new char[strlen(instringvalue) + 1];
	char* p_inoutstringvalue = new char[strlen(inoutstringvalue) + 1];
	strcpy(p_instringvalue, instringvalue);
	strcpy(p_inoutstringvalue, inoutstringvalue);

	passvariablerecord_inarg.variablerecordstring = p_instringvalue;
	passvariablerecord_inarg.variablerecordinteger = 12345;
	passvariablerecord_inarg.variablerecordobject = mybar_var;	

	passvariablerecord_inoutarg.variablerecordstring = p_inoutstringvalue;
	passvariablerecord_inoutarg.variablerecordinteger = 67890;
	passvariablerecord_inoutarg.variablerecordobject = mybar_var;
	
	try {
		p_passvariablerecord_return = mybar_var->passvariablerecord(passvariablerecord_inarg,
			passvariablerecord_inoutarg, p_passvariablerecord_outarg);
	}	
	catch (const CORBA(SystemException)& the_exception) {
		show_system_exception (the_exception);		
		return ILUCPP_FALSE;
	}
	catch (...) {
		cout << "Got some sort of exception" << endl;
		return ILUCPP_FALSE;
	}
	
	// outarg should be copy of inarg
	if (strcmp(p_passvariablerecord_outarg->variablerecordstring, instringvalue) != 0) {
		cout << "p_passvariablerecord_outarg->variablerecordstring bad " << endl;
		b_return = ILUCPP_FALSE;
	}
	if (12345 != p_passvariablerecord_outarg->variablerecordinteger) {
		cout << "p_passvariablerecord_outarg->variablerecordinteger bad " << endl;
		b_return = ILUCPP_FALSE;
	}
	if (mybar_var.iluGetObjectPointer() != (p_passvariablerecord_outarg->variablerecordobject).iluGetObjectPointer()) {
		cout << "p_passvariablerecord_outarg->variablerecordobject bad " << endl;
		b_return = ILUCPP_FALSE;
	}


	//  return should be what inout arg was
	if (strcmp(p_passvariablerecord_return->variablerecordstring, inoutstringvalue) != 0) {
		cout << "p_passvariablerecord_return->variablerecordstring bad " << endl;
		b_return = ILUCPP_FALSE;
	}
	if (67890 != p_passvariablerecord_return->variablerecordinteger) {
		cout << "p_passvariablerecord_return->variablerecordinteger bad " << endl;
		b_return = ILUCPP_FALSE;
	}
	if (mybar_var.iluGetObjectPointer() != (p_passvariablerecord_return->variablerecordobject).iluGetObjectPointer()) {
		cout << "p_passvariablerecord_return->variablerecordobject bad " << endl;
		b_return = ILUCPP_FALSE;
	}


	//  inout arg check
	if (strcmp(passvariablerecord_inoutarg.variablerecordstring, "returned passvariablerecord_inoutarg") != 0) {
		cout << "passvariablerecord_inoutarg.variablerecordstring bad " << endl;
		b_return = ILUCPP_FALSE;
	}
	if (67891 != passvariablerecord_inoutarg.variablerecordinteger) {
		cout << "passvariablerecord_inoutarg.variablerecordinteger bad " << endl;
		b_return = ILUCPP_FALSE;
	}
	if (mybar_var.iluGetObjectPointer() != (passvariablerecord_inoutarg.variablerecordobject).iluGetObjectPointer()) {
		cout << "passvariablerecord_inoutarg.variablerecordobject bad " << endl;
		b_return = ILUCPP_FALSE;
	}

	delete p_passvariablerecord_outarg;
	delete p_passvariablerecord_return;


	if (b_return)
		cout << "passvariablerecord succeeded" << endl;

	return b_return;
}

///////////////////////////////////////////////////////////////////////
// try calling the passfixedarray method

CORBA(Boolean) dopassfixedarray(foo(bar_var)& mybar_var) {
	
	foo(integerarray) in_array = {
		{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9},
		{ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19},
		{ 20, 21, 22, 23, 24, 25, 26, 27, 28, 29},
		{ 30, 31, 32, 33, 34, 35, 36, 37, 38, 39},
		{ 40, 41, 42, 43, 44, 45, 46, 47, 48, 49}
	};
	foo(integerarray) inout_array = {
		{ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19},
		{ 110, 111, 112, 113, 114, 115, 116, 117, 118, 119},
		{ 120, 121, 122, 123, 124, 125, 126, 127, 128, 129},
		{ 130, 131, 132, 133, 134, 135, 136, 137, 138, 139},
		{ 140, 141, 142, 143, 144, 145, 146, 147, 148, 149}
	};

	// use these intermediate slice variables to get around G++ (2.7.2) problem with 
 	// distinguishing between conversion to const T and conversion to T
	foo(integerarray_slice)* p_inarg_slice = foo(integerarray_dup)(in_array);
 	foo(integerarray_slice)* p_inoutarg_slice = foo(integerarray_dup)(inout_array);
 	foo(integerarray_slice)* p_outarg_slice = foo(integerarray_alloc)();
#ifdef __GNUC__
	foo(integerarray_slice)* p_return_slice;
#endif

	foo(integerarray_var) passfixedarray_inarg_var(p_inarg_slice);
	foo(integerarray_var) passfixedarray_inoutarg_var(p_inoutarg_slice);
	foo(integerarray_var) passfixedarray_outarg_var(p_outarg_slice);


#if (defined(__SUNPRO_CC))
	/* apparently the Sunpro C++ compiler (4.1 here) has some strange bug which *actually will copy* an
	   array onto the stack! if we pass the passfixedarray_WHATEVERarg_var as the 
	   argument to the call to mybar_var->passfixedarray, invoking the conversion to 
	   foo(integerarray)& in the process.  However, if we make an intermediate reference
	   as below, and pass that, we don't have the problem.
	*/
	foo(integerarray)& r_fixedarray_inarg = passfixedarray_inarg_var;
	foo(integerarray)& r_fixedarray_inoutarg = passfixedarray_inoutarg_var;
	foo(integerarray)& r_fixedarray_outarg = passfixedarray_outarg_var;
#endif

	foo(integerarray_var) passfixedarray_return_var;
	CORBA(Long) index0, index1;
	CORBA(Boolean) b_return = ILUCPP_TRUE;

	try {

#if (defined(__SUNPRO_CC))
		passfixedarray_return_var = mybar_var->passfixedarray(r_fixedarray_inarg,
			r_fixedarray_inoutarg, r_fixedarray_outarg);

#elif (defined (__GNUC__))
		// use these intermediate slice variables to get around G++ (2.7.2) problem with 
 		// distinguishing between conversion to const T and conversion to T
		p_return_slice = mybar_var->passfixedarray(p_inarg_slice,
			p_inoutarg_slice, p_outarg_slice);
		passfixedarray_return_var = p_return_slice;

#else
		passfixedarray_return_var = mybar_var->passfixedarray(passfixedarray_inarg_var,
			passfixedarray_inoutarg_var, passfixedarray_outarg_var);
#endif

	}	
	catch (const CORBA(SystemException)& the_exception) {
		show_system_exception (the_exception);		
		return ILUCPP_FALSE;
	}
	catch (...) {
		cout << "Got some sort of exception" << endl;
		return ILUCPP_FALSE;
	}
	
	// make some refs so we can use []'s
#ifdef __GNUC__
	foo(integerarray)& inargref =  *((foo(integerarray)*)p_inarg_slice);
	foo(integerarray)& outargref = *((foo(integerarray)*)p_outarg_slice);
	foo(integerarray)& inoutargref = *((foo(integerarray)*)p_inoutarg_slice);
	foo(integerarray)& returnref = *((foo(integerarray)*)p_return_slice);
#else
	foo(integerarray)& inargref = passfixedarray_inarg_var;
	foo(integerarray)& outargref = passfixedarray_outarg_var;
	foo(integerarray)& inoutargref = passfixedarray_inoutarg_var;
	foo(integerarray)& returnref = passfixedarray_return_var;
#endif

	// outarg should be copy of inarg
	for (index0 = 0; index0 < 5; index0++) {
		for (index1 = 0; index1 < 10; index1++) {
			if (inargref[index0][index1] != outargref[index0][index1]) {
				cout << "passfixedarray_outarg bad " << endl;
				b_return = ILUCPP_FALSE;
				break;
			}
		}
		if (b_return == ILUCPP_FALSE) break;
	}
	
	//  return should be what inout arg was
	for (index0 = 0; index0 < 5; index0++) {
		for (index1 = 0; index1 < 10; index1++) {
			if (inout_array[index0][index1] != returnref[index0][index1]) {
				cout << "passfixedarray_return bad " << endl;
				b_return = ILUCPP_FALSE;
				break;
			}
		}
		if (b_return == ILUCPP_FALSE) break;
	}

	//  inout arg should be one more in each field
	for (index0 = 0; index0 < 5; index0++) {
		for (index1 = 0; index1 < 10; index1++) {
			if (inout_array[index0][index1] + 1 != inoutargref[index0][index1]) {
				cout << "passfixedarray_inoutarg bad " << endl;
				b_return = ILUCPP_FALSE;
				break;
			}
		}
		if (b_return == ILUCPP_FALSE) break;
	}

	if (b_return)
		cout << "passfixedarray succeeded" << endl;

	return b_return;
}

///////////////////////////////////////////////////////////////////////
// try calling the passvariablearray method

CORBA(Boolean) dopassvariablearray(foo(bar_var)& mybar_var) {
	
        // cout << "dopassvariablearray - entered" << endl;

	foo(bararray) in_array = {
		{ mybar_var, mybar_var, mybar_var},
		{ mybar_var, mybar_var, mybar_var}
	};
	foo(bararray) inout_array = {
		{ mybar_var, mybar_var, mybar_var},
		{ mybar_var, mybar_var, mybar_var}
	};


	// manually modify the refcounts to get around some G++ problems 
        // (various on 2.7.2, egcs) about running copy constructors
        // destructors

         if( mybar_var->iluGetReferenceCount() < 13) { 
            int count = mybar_var->iluGetReferenceCount();
	    cout << "gnuc dopassvariablearray - mybar_var count after init = "
                << mybar_var->iluGetReferenceCount() << endl;
            while(mybar_var->iluGetReferenceCount() < 13) 
                 mybar_var->iluIncrementReferenceCount();
          }

	// use these intermediate slice variables to get around G++ (2.7.2) problem with 
 	// distinguishing between conversion to const T and conversion to T
	foo(bararray_slice)* p_inarg_slice = foo(bararray_dup)(in_array);
 	foo(bararray_slice)* p_inoutarg_slice = foo(bararray_dup)(inout_array);
#ifdef __GNUC__
	foo(bararray_slice)* p_return_slice;
#endif


	foo(bararray_var) passvariablearray_inarg_var(p_inarg_slice);
	foo(bararray_var) passvariablearray_inoutarg_var(p_inoutarg_slice);
	foo(bararray_slice)* passvariablearray_outarg_slice;


#ifdef __SUNPRO_CC
	/* apparently the Sunpro C++ compiler (4.1 here) has some strange bug which *actually will copy* an
	   array onto the stack! if we pass the passvariablearray_WHATEVERarg_var as the 
	   argument to the call to mybar_var->passvariablearray, invoking the conversion to 
	   foo(integerarray)& in the process.  However, if we make an intermediate reference
	   as below, and pass that, we don't have the problem.
	*/
	foo(bararray)& r_variablearray_inarg = passvariablearray_inarg_var;
	foo(bararray)& r_variablearray_inoutarg = passvariablearray_inoutarg_var;
#endif

	foo(bararray_var) passvariablearray_return_var;
	CORBA(Long) index0, index1;
	CORBA(Boolean) b_return = ILUCPP_TRUE;
	
	try {

#if (defined(__SUNPRO_CC))
		passvariablearray_return_var = mybar_var->passvariablearray(r_variablearray_inarg,
			r_variablearray_inoutarg, passvariablearray_outarg_slice);

#elif (defined (__GNUC__))
		// use these intermediate slice variables to get around G++ (2.7.2) problem with 
 		// distinguishing between conversion to const T and conversion to T
		p_return_slice = mybar_var->passvariablearray(p_inarg_slice,
			p_inoutarg_slice, passvariablearray_outarg_slice);
		passvariablearray_return_var = p_return_slice;

#else
		passvariablearray_return_var = mybar_var->passvariablearray(passvariablearray_inarg_var,
			passvariablearray_inoutarg_var, passvariablearray_outarg_slice);
#endif

	}	
	catch (const CORBA(SystemException)& the_exception) {
		show_system_exception (the_exception);		
		return ILUCPP_FALSE;
	}
	catch (...) {
		cout << "Got some sort of exception" << endl;
		return ILUCPP_FALSE;
	}

	foo(bararray_var) passvariablearray_outarg_var (passvariablearray_outarg_slice);

	// make some refs so we can use []'s
#ifdef __GNUC__
	foo(bararray)& inargref =  *((foo(bararray)*)p_inarg_slice);
	foo(bararray)& outargref = *((foo(bararray)*)passvariablearray_outarg_slice);
	foo(bararray)& inoutargref = *((foo(bararray)*)p_inoutarg_slice);
	foo(bararray)& returnref = *((foo(bararray)*)p_return_slice);
#else
	foo(bararray)& inargref = passvariablearray_inarg_var;
	foo(bararray)& outargref = passvariablearray_outarg_var;
	foo(bararray)& inoutargref = passvariablearray_inoutarg_var;
	foo(bararray)& returnref = passvariablearray_return_var;
#endif
	// outarg should be copy of inarg
	for (index0 = 0; index0 < 2; index0++) {
		for (index1 = 0; index1 < 3; index1++) {
			if ((inargref[index0][index1]).iluGetObjectPointer() != 
				(outargref[index0][index1]).iluGetObjectPointer()) {
				cout << "passvariablearray_outarg bad " << endl;
				b_return = ILUCPP_FALSE;
				break;
			}
		}
		if (b_return == ILUCPP_FALSE) break;
	}
	
	//  return should be what inout arg was
	for (index0 = 0; index0 < 2; index0++) {
		for (index1 = 0; index1 < 3; index1++) {
			if ((returnref[index0][index1]).iluGetObjectPointer() !=
				(inoutargref[index0][index1]).iluGetObjectPointer()){
				cout << "passvariablearray_return bad " << endl;
				b_return = ILUCPP_FALSE;
				break;
			}
		}
		if (b_return == ILUCPP_FALSE) break;
	}

	//  inout arg should  be copy of inarg
	for (index0 = 0; index0 < 2; index0++) {
		for (index1 = 0; index1 < 3; index1++) {
			if ((inargref[index0][index1]).iluGetObjectPointer() != 
				(inoutargref[index0][index1]).iluGetObjectPointer()) {
				cout << "passvariablearray_inoutarg bad " << endl;
				b_return = ILUCPP_FALSE;
				break;
			}
		}
		if (b_return == ILUCPP_FALSE) break;
	}

	if (b_return)
		cout << "passvariablearray succeeded" << endl;

	return b_return;
}


///////////////////////////////////////////////////////////////////////
// try calling the passunboundedsequence method

CORBA(Boolean) dopassunboundedsequence(foo(bar_var)& mybar_var) {	
	
	foo(unboundedlongseq) passunboundedlongseq_inarg(5);
	foo(unboundedlongseq) passunboundedlongseq_inoutarg(3);
	
	foo(unboundedlongseq)* p_passunboundedlongseq_outarg; 
	foo(unboundedlongseq)* p_passunboundedlongseq_return;
	CORBA(Boolean) b_return = ILUCPP_TRUE;
	iluCardinal index;

	passunboundedlongseq_inarg.length(5);
	for (index = 0; index < 5; index++) 
		passunboundedlongseq_inarg[index] = index;

	passunboundedlongseq_inoutarg.length(3);
	for (index = 0; index < 3; index++) 
		passunboundedlongseq_inoutarg[index] = index + 100;
	
	try {
		p_passunboundedlongseq_return = mybar_var->passunboundedsequence(passunboundedlongseq_inarg,
			passunboundedlongseq_inoutarg, p_passunboundedlongseq_outarg);
	}	
	catch (const CORBA(SystemException)& the_exception) {
		show_system_exception (the_exception);		
		return ILUCPP_FALSE;
	}
	catch (...) {
		cout << "Got some sort of exception" << endl;
		return ILUCPP_FALSE;
	}
	
	// outarg should be a copy of inarg
	if (p_passunboundedlongseq_outarg->length() != passunboundedlongseq_inarg.length()) {
		cout << "p_passunboundedlongseq_outarg->length() bad" << endl;
		b_return = ILUCPP_FALSE;
	}
	else {		
		for (index = 0; index < p_passunboundedlongseq_outarg->length(); index++) {
			if ((*p_passunboundedlongseq_outarg)[index] != passunboundedlongseq_inarg[index]) {
				cout << "(*p_passunboundedlongseq_outarg)[index]() bad" << endl;
				b_return = ILUCPP_FALSE;
				break;
			}
		}
	}

	// return should be what inout arg was
	if (p_passunboundedlongseq_return->length() != 3) {
		cout << "p_passunboundedlongseq_outarg->length() bad" << endl;
		b_return = ILUCPP_FALSE;
	}
	else {		
		for (index = 0; index < p_passunboundedlongseq_return->length(); index++) {
			if ((*p_passunboundedlongseq_return)[index] != (CORBA(Long)(index + 100))) {
				cout << "(*p_passunboundedlongseq_return)[index] bad" << endl;
				b_return = ILUCPP_FALSE;
				break;
			}
		}
	}

	// inout arg should be a copy of inarg
	if (passunboundedlongseq_inoutarg.length() != passunboundedlongseq_inarg.length()) {
		cout << "passunboundedlongseq_inoutarg.length() bad" << endl;
		b_return = ILUCPP_FALSE;
	}
	else {		
		for (index = 0; index < passunboundedlongseq_inoutarg.length(); index++) {
			if (passunboundedlongseq_inoutarg[index] != passunboundedlongseq_inarg[index]) {
				cout << "passunboundedlongseq_inoutarg[index] bad" << endl;
				b_return = ILUCPP_FALSE;
				break;
			}
		}
	}

	delete p_passunboundedlongseq_outarg;
	delete p_passunboundedlongseq_return;

	if (b_return)
		cout << "passunboundedlongseq succeeded" << endl;

	return b_return;
}


///////////////////////////////////////////////////////////////////////
// try calling the passboundedsequence method

CORBA(Boolean) dopassboundedsequence(foo(bar_var)& mybar_var) {	
	
	foo(boundedbarseq) passboundedbarseq_inarg;
	foo(boundedbarseq) passboundedbarseq_inoutarg;
	
	foo(boundedbarseq)* p_passboundedbarseq_outarg; 
	foo(boundedbarseq)* p_passboundedbarseq_return;
	CORBA(Boolean) b_return = ILUCPP_TRUE;
	iluCardinal index;

	passboundedbarseq_inarg.length(2);
	for (index = 0; index < 2; index++) 
		passboundedbarseq_inarg[index] = mybar_var;

	passboundedbarseq_inoutarg.length(1);
	for (index = 0; index < 1; index++) 
		passboundedbarseq_inoutarg[index] = mybar_var;
	
	try {
		p_passboundedbarseq_return = mybar_var->passboundedsequence(passboundedbarseq_inarg,
			passboundedbarseq_inoutarg, p_passboundedbarseq_outarg);
	}	
	catch (const CORBA(SystemException)& the_exception) {
		show_system_exception (the_exception);		
		return ILUCPP_FALSE;
	}
	catch (...) {
		cout << "Got some sort of exception" << endl;
		return ILUCPP_FALSE;
	}
	
	// outarg should be a copy of inarg
	if (p_passboundedbarseq_outarg->length() != passboundedbarseq_inarg.length()) {
		cout << "p_passboundedbarseq_outarg->length() bad" << endl;
		b_return = ILUCPP_FALSE;
	}
	else {		
		for (index = 0; index < p_passboundedbarseq_outarg->length(); index++) {
			if (((*p_passboundedbarseq_outarg)[index]).iluGetObjectPointer() != 
				(passboundedbarseq_inarg[index]).iluGetObjectPointer()) {
				cout << "(*p_passboundedbarseq_outarg)[index]() bad" << endl;
				b_return = ILUCPP_FALSE;
				break;
			}
		}
	}

	// return should be what inout arg was
	if (p_passboundedbarseq_return->length() != 1) {
		cout << "p_passboundedbarseq_outarg->length() bad" << endl;
		b_return = ILUCPP_FALSE;
	}
	else {		
		for (index = 0; index < p_passboundedbarseq_return->length(); index++) {
			if (((*p_passboundedbarseq_return)[index]).iluGetObjectPointer() != 
				mybar_var.iluGetObjectPointer()) {
				cout << "(*p_passboundedbarseq_return)[index] bad" << endl;
				b_return = ILUCPP_FALSE;
				break;
			}
		}
	}

	// inout arg should be a copy of inarg
	if (passboundedbarseq_inoutarg.length() != passboundedbarseq_inarg.length()) {
		cout << "passboundedbarseq_inoutarg.length() bad" << endl;
		b_return = ILUCPP_FALSE;
	}
	else {		
		for (index = 0; index < passboundedbarseq_inoutarg.length(); index++) {
			if ((passboundedbarseq_inoutarg[index]).iluGetObjectPointer() != 
				(passboundedbarseq_inarg[index]).iluGetObjectPointer()) {
				cout << "passboundedbarseq_inoutarg[index] bad" << endl;
				b_return = ILUCPP_FALSE;
				break;
			}
		}
	}

	delete p_passboundedbarseq_outarg;
	delete p_passboundedbarseq_return;

	if (b_return)
		cout << "passboundedbarseq succeeded" << endl;

	return b_return;
}


///////////////////////////////////////////////////////////////////////
// try calling the dopassvariableunion method

CORBA(Boolean) dopassvariableunion(foo(bar_var)& mybar_var) {	
	
	foo(variableuniontype) passvariableuniontype_inarg;
	foo(variableuniontype) passvariableuniontype_inoutarg;
	
	foo(variableuniontype)* p_passvariableuniontype_outarg; 
	foo(variableuniontype)* p_passvariableuniontype_return;
	CORBA(Boolean) b_return = ILUCPP_TRUE;

	passvariableuniontype_inarg.integerarm(2);
	passvariableuniontype_inoutarg.bararm(mybar_var);
	CORBA(UShort) original_inoutarg_discriminator = passvariableuniontype_inoutarg._d();
	
	try {
		p_passvariableuniontype_return = mybar_var->passvariableunion(passvariableuniontype_inarg,
			passvariableuniontype_inoutarg, p_passvariableuniontype_outarg);
	}	
	catch (const CORBA(SystemException)& the_exception) {
		show_system_exception (the_exception);		
		return ILUCPP_FALSE;
	}
	catch (...) {
		cout << "Got some sort of exception" << endl;
		return ILUCPP_FALSE;
	}
	
	// outarg should be a copy of inarg
	if (p_passvariableuniontype_outarg->_d() != passvariableuniontype_inarg._d()) {
		cout << "p_passvariableuniontype_outarg->_d() bad" << endl;
		b_return = ILUCPP_FALSE;
	}
	else {		
		if (p_passvariableuniontype_outarg->integerarm() != passvariableuniontype_inarg.integerarm()) {
			cout << "p_passvariableuniontype_outarg->integerarm() bad" << endl;
			b_return = ILUCPP_FALSE;
		}
	}

	// return should be what inout arg was
	if (p_passvariableuniontype_return->_d() != original_inoutarg_discriminator) {
		cout << "p_passvariableuniontype_return->_d() bad" << endl;
		b_return = ILUCPP_FALSE;
	}
	else {		
		if (p_passvariableuniontype_return->bararm() != 
			mybar_var.iluGetObjectPointer()) {
			cout << "p_passvariableuniontype_return->bararm() bad" << endl;
			b_return = ILUCPP_FALSE;
		}
	}

	// inout arg should be a copy of inarg
	if (passvariableuniontype_inoutarg._d() != passvariableuniontype_inarg._d()) {
		cout << "p_passvariableuniontype_inoutarg._d() bad" << endl;
		b_return = ILUCPP_FALSE;
	}
	else {		
		if (passvariableuniontype_inoutarg.integerarm() != passvariableuniontype_inarg.integerarm()) {
			cout << "p_passvariableuniontype_inoutarg.integerarm() bad" << endl;
			b_return = ILUCPP_FALSE;
		}
	}

	delete p_passvariableuniontype_outarg;
	delete p_passvariableuniontype_return;

	if (b_return)
		cout << "passvariableuniontype succeeded" << endl;

	return b_return;
}

///////////////////////////////////////////////////////////////////////
// try calling the dopassoptionals method

CORBA(Boolean) dopassoptionals(foo(bar_var)& mybar_var) {	
	
	foo(optionalinteger) passoptionalinteger_inarg = NULL;
	foo(optionalbarobject) passoptionalbarobject_inoutarg = mybar_var;
	foo(optionalbarobject) passoptionalbarobject_outarg; 
	foo(optionalinteger) passoptionalinteger_return;

	// dupicate our inout arg since we want to hang onto it
	// refcount should now be two (one for original, one for
	// the duplicate
	foo(bar)::_duplicate(passoptionalbarobject_inoutarg);

	CORBA(Boolean) b_return = ILUCPP_TRUE;
	
	try {
		passoptionalinteger_return = mybar_var->passoptionals(passoptionalinteger_inarg,
			passoptionalbarobject_inoutarg, passoptionalbarobject_outarg);
	}	
	catch (const CORBA(SystemException)& the_exception) {
		show_system_exception (the_exception);		
		return ILUCPP_FALSE;
	}
	catch (...) {
		cout << "Got some sort of exception" << endl;
		return ILUCPP_FALSE;
	}
	
	// outarg should be mybar_var's object
	if ((! passoptionalbarobject_outarg) || 
		(passoptionalbarobject_outarg != mybar_var.iluGetObjectPointer())) {
		cout << "passoptionalbarobject_outarg bad" << endl;
		b_return = ILUCPP_FALSE;
	}
	else 
		passoptionalbarobject_outarg->_release();


	// return should be 40
	if ((!passoptionalinteger_return) || (*passoptionalinteger_return != 40)) {
		cout << "passoptionalinteger_return bad" << endl;
		b_return = ILUCPP_FALSE;
	}

	// inout arg should be NULL
	if (passoptionalbarobject_inoutarg) {
		cout << "passoptionalbarobject_inoutarg bad" << endl;
		b_return = ILUCPP_FALSE;
	}

	// undo the dup we did on the inout arg in the beginning
	mybar_var->_release();

	delete passoptionalinteger_return;

	if (b_return)
		cout << "passoptionals succeeded" << endl;

	return b_return;
}


#if (defined(ADD_VARIANT_SUPPORT))

///////////////////////////////////////////////////////////////////////
// try calling the passsimpleanys method

CORBA(Boolean) dopasssimpleanys(foo(bar_var)& mybar_var) {	
	
	CORBA(Any) passsimpleanys_inarg;
	CORBA(Any) passsimpleanys_inoutarg;
	
	CORBA(Any)* p_passsimpleanys_outarg; 
	CORBA(Any)* p_passsimpleanys_return;

	CORBA(Long) l_inarg_any_value = 2;
	CORBA(Boolean) inout_boolean;
	CORBA(Any)::to_boolean inout_to_boolean(inout_boolean);
	CORBA(string) inoutstring = CONST_CAST(iluCString, "InOut string in an Any");
	CORBA(string) pc_inout_any_string = new char[strlen(inoutstring) + 1];
	CORBA(string) pc_return_any_string;
	strcpy(pc_inout_any_string, inoutstring);
	CORBA(Any)::from_string a_from_string (pc_inout_any_string, strlen(pc_inout_any_string));
	CORBA(Any)::to_string a_to_string (pc_return_any_string, 0);
	CORBA(Boolean) b_return = ILUCPP_TRUE;
	CORBA(Double) d_out_double;

	passsimpleanys_inarg <<= l_inarg_any_value;
	passsimpleanys_inoutarg <<= a_from_string;
	
	try {
		p_passsimpleanys_return = mybar_var->passsimpleanys(passsimpleanys_inarg,
			passsimpleanys_inoutarg, p_passsimpleanys_outarg);
	}	
	catch (const CORBA(SystemException)& the_exception) {
		show_system_exception (the_exception);		
		return ILUCPP_FALSE;
	}
	catch (...) {
		cout << "Got some sort of exception" << endl;
		return ILUCPP_FALSE;
	}
	
	// outarg should be square root of inarg
	if (! ((*p_passsimpleanys_outarg) >>= d_out_double)) {
		cout << "p_passsimpleanys_outarg extraction bad " << endl;
		b_return = ILUCPP_FALSE;
	}
	else if ((pow(d_out_double, 2.0) < (l_inarg_any_value - .01)) || 
		(pow(d_out_double, 2.0) > (l_inarg_any_value + .01))) {
			cout << "p_passsimpleanys_outarg extraction value bad " << endl;
		b_return = ILUCPP_FALSE;
	}

	//  return should be what inout arg was
	if (! ((*p_passsimpleanys_return) >>= a_to_string)) {
		cout << "p_passsimpleanys_return extraction bad" << endl;
		b_return = ILUCPP_FALSE;
	}
	else if (strcmp(inoutstring, a_to_string.val) != 0) {
			cout << "p_passsimpleanys_return extraction value bad" << endl;
		b_return = ILUCPP_FALSE;
	}

	//  inout arg check
	if (! (passsimpleanys_inoutarg >>= inout_to_boolean)) {
		cout << "passsimpleanys_inoutarg extraction bad" << endl;
		b_return = ILUCPP_FALSE;
	}
	else if (! inout_boolean) {
			cout << "passsimpleanys_inoutarg extraction value bad" << endl;
		b_return = ILUCPP_FALSE;
	}

	delete p_passsimpleanys_outarg; 
	delete p_passsimpleanys_return;

	if (b_return)
		cout << "passsimpleanys succeeded" << endl;

	return b_return;
}

///////////////////////////////////////////////////////////////////////
// try calling the passarrayanys method

CORBA(Boolean) dopassarrayanys(foo(bar_var)& mybar_var) {	

	int i_xindex, i_yindex;
	CORBA(Boolean) b_return = ILUCPP_TRUE;

	CORBA(Any) passarrayanys_inarg;
	CORBA(Any) passarrayanys_inoutarg;
	
	CORBA(Any)* p_passarrayanys_outarg; 
	CORBA(Any)* p_passarrayanys_return;
	foo(bararray_forany)  out_barforany;
	foo(integerarray_forany)  return_integerforany;
	foo(integerarray_forany)  inout_integerforany;

	// define an integer array
	foo(integerarray) in_integerarray = {
		{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9},
		{ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19},
		{ 20, 21, 22, 23, 24, 25, 26, 27, 28, 29},
		{ 30, 31, 32, 33, 34, 35, 36, 37, 38, 39},
		{ 40, 41, 42, 43, 44, 45, 46, 47, 48, 49}
	};

	// make up a bararray
	foo(bararray_slice)*  p_inout_barslice = foo(bararray_alloc)();
	
	for (i_xindex = 0; i_xindex < 2; i_xindex++) {
		for (i_yindex = 0; i_yindex < 3; i_yindex++)
			p_inout_barslice[i_xindex][i_yindex] = mybar_var;
	}

	// make up an integerarray_forany, setting to copy the argument when we insert into an any
	foo(integerarray_forany) in_integer_forany (in_integerarray);
	// tell the in_integer_forany not to release since the array's actually on the stack
	in_integer_forany.iluSetRelease(ILUCPP_FALSE); 

	// make up an bararray_forany, passing ownership of the argument when we insert into an any
	foo(bararray_forany) inout_bar_forany (p_inout_barslice, ILUCPP_TRUE); 

	// insert the foranys into the anys
	passarrayanys_inarg <<= in_integer_forany;
	passarrayanys_inoutarg <<= inout_bar_forany;
	
	try {
		p_passarrayanys_return = mybar_var->passarrayanys(passarrayanys_inarg,
			passarrayanys_inoutarg, p_passarrayanys_outarg);
	}	
	catch (const CORBA(SystemException)& the_exception) {
		show_system_exception (the_exception);		
		return ILUCPP_FALSE;
	}
/*	catch (...) {
		cout << "Got some sort of exception" << endl;
		return ILUCPP_FALSE;
	}
*/	
	
	// outarg should be what inout arg was
	if (! ((*p_passarrayanys_outarg) >>= out_barforany)) {
		cout << "p_passarrayanys_outarg extraction bad " << endl;
		b_return = ILUCPP_FALSE;
	}
	else {
#ifdef __GNUC__
		// get around G++ (2.7.2) problem with 
 		// distinguishing between conversion to const T and conversion to T
		foo(bararray)& the_out_array = *((foo(bararray)*)(out_barforany.iluGetSlice()));
#else
		foo(bararray)& the_out_array = out_barforany;  
#endif
		for (i_xindex = 0; i_xindex < 2; i_xindex++) {
			for (i_yindex = 0; i_yindex < 3; i_yindex++) {
				if (!(the_out_array[i_xindex][i_yindex])->_is_equivalent(mybar_var)) {
					cout << "p_passarrayanys_outarg extraction value bad " << endl;
					b_return = ILUCPP_FALSE;
				}
			}
		}
	}

	//  return should be what inarg was with all the elements doubled
	if (! ((*p_passarrayanys_return) >>= return_integerforany)) {
		cout << "p_passarrayanys_return extraction bad" << endl;
		b_return = ILUCPP_FALSE;
	}
	else {
#ifdef __GNUC__
		// get around G++ (2.7.2) problem with 
 		// distinguishing between conversion to const T and conversion to T
		foo(integerarray)& the_return_array = *((foo(integerarray)*)(return_integerforany.iluGetSlice()));
#else
		foo(integerarray)& the_return_array = return_integerforany;
#endif
		for (i_xindex = 0; i_xindex < 5; i_xindex++) {
			for (i_yindex = 0; i_yindex < 10; i_yindex++) {
				if (the_return_array[i_xindex][i_yindex] != (2 * in_integerarray[i_xindex][i_yindex])) {
					cout << "p_passarrayanys_return extraction value bad" << endl;
					b_return = ILUCPP_FALSE;
				}
			}
		}
	}

	//  inout should be what inarg was
	if (! (passarrayanys_inoutarg >>= inout_integerforany)) {
		cout << "passarrayanys_inoutarg extraction bad" << endl;
		b_return = ILUCPP_FALSE;
	}
	else {
#ifdef __GNUC__
		// get around G++ (2.7.2) problem with 
 		// distinguishing between conversion to const T and conversion to T
		foo(integerarray)& the_inout_array = *((foo(integerarray)*)(inout_integerforany.iluGetSlice()));
#else
		foo(integerarray)& the_inout_array = inout_integerforany;
#endif
		for (i_xindex = 0; i_xindex < 5; i_xindex++) {
			for (i_yindex = 0; i_yindex < 10; i_yindex++) {
				if (the_inout_array[i_xindex][i_yindex] != in_integerarray[i_xindex][i_yindex]) {
					cout << "passarrayanys_inoutarg extraction value bad" << endl;
					b_return = ILUCPP_FALSE;
				}
			}
		}
	}
	
	delete p_passarrayanys_outarg; 
	delete p_passarrayanys_return;

	if (b_return)
		cout << "passarrayanys succeeded" << endl;

	return b_return;
}


///////////////////////////////////////////////////////////////////////
// try calling the passassortedanys method

CORBA(Boolean) dopassassortedanys(foo(bar_var)& mybar_var) {	
	
	CORBA(Boolean) b_return = ILUCPP_TRUE;
	CORBA(Any) passassortedanys_inarg;
	CORBA(Any) passassortedanys_inoutarg;
	
	CORBA(Any)* p_passassortedanys_outarg; 
	CORBA(Any)* p_passassortedanys_return;

	// set up inarg
	foo(fixedrecordtype) inarg_any_value;
	inarg_any_value.fixedrecordinteger = 12345;
	inarg_any_value.fixedrecordenum = foo(orange);
	passassortedanys_inarg <<= inarg_any_value;

	foo(optionalbarobject_forany) inoutarg_sent_value;
	passassortedanys_inoutarg <<= inoutarg_sent_value;  // should send 'bar not present'

	
	try {
		p_passassortedanys_return = mybar_var->passassortedanys(passassortedanys_inarg,
			passassortedanys_inoutarg, p_passassortedanys_outarg);
	}	
	catch (const CORBA(SystemException)& the_exception) {
		show_system_exception (the_exception);		
		return ILUCPP_FALSE;
	}
	catch (...) {
		cout << "Got some sort of exception" << endl;
		return ILUCPP_FALSE;
	}
	
	foo(fixedrecordtype)* p_inoutarg_received_value;
	//  inout arg check should be copy of inarg
	if (! (passassortedanys_inoutarg >>= p_inoutarg_received_value)) {
		cout << "passassortedanys_inoutarg extraction bad" << endl;
		b_return = ILUCPP_FALSE;
	}
	else if (inarg_any_value.fixedrecordinteger != p_inoutarg_received_value->fixedrecordinteger ||
		inarg_any_value.fixedrecordenum != p_inoutarg_received_value->fixedrecordenum) {
			cout << "passassortedanys_inoutarg extraction value bad" << endl;
		b_return = ILUCPP_FALSE;
	}


	// outarg should be an optional bar set up to contain a pointer to the mybar_var's object
	foo(optionalbarobject_forany)* p_outarg_value;
	if (! ((*p_passassortedanys_outarg) >>= p_outarg_value)) {
		cout << "p_passassortedanys_outarg extraction bad " << endl;
		b_return = ILUCPP_FALSE;
	}
	else if (p_outarg_value->m_p_value != mybar_var.iluGetObjectPointer()) {
			cout << "p_passassortedanys_outarg extraction value bad " << endl;
		b_return = ILUCPP_FALSE;
	}

	//  return should be a variable union, set to integer 78
	foo(variableuniontype)* p_return_value;
	if (! ((*p_passassortedanys_return) >>= p_return_value)) {
		cout << "p_passassortedanys_return extraction bad" << endl;
		b_return = ILUCPP_FALSE;
	}
	else if (p_return_value->_d() != 2 || p_return_value->integerarm() != 78) {
			cout << "p_passassortedanys_return extraction value bad" << endl;
		b_return = ILUCPP_FALSE;
	}


	delete p_passassortedanys_outarg; 
	delete p_passassortedanys_return;

	if (b_return)
		cout << "passassortedanys succeeded" << endl;

	return b_return;
}


///////////////////////////////////////////////////////////////////////
// try calling the dotoywithanys method

CORBA(Boolean) dotoywithanys(foo(bar_var)& mybar_var) {	
	
	CORBA(Boolean) b_return = ILUCPP_TRUE;
	CORBA(Any) anany;
	foo(bar_ptr) p_bar_retriever;
	iluObject* p_iluobject_retriever;

	// dup since we'll be putting into an any
	foo(bar)::_duplicate(mybar_var);

	// put it into the any
	anany <<= (foo(bar_ptr))mybar_var;

	// cause it to be pickled
	anany.iluPickleValue();

	// suck it back out as a bar object
	if (! (anany >>= p_bar_retriever)) {
		cout << "dotoywithanys extraction as foo(bar) failure" << endl;
		return ILUCPP_FALSE;
	}

	// put it back in
	anany.iluPickleValue();

	// suck it back out as an ilu object
	if (! (anany >>= p_iluobject_retriever)) {
		cout << "dotoywithanys extraction as iluObject failure" << endl;
		return ILUCPP_FALSE;
	}

	// put it back in
	anany.iluPickleValue();

	// see if we can suck it back out again as a bar object
	if (! (anany >>= p_bar_retriever)) {
		cout << "dotoywithanys extraction as foo(bar) (after iluObject) failure" << endl;
		return ILUCPP_FALSE;
	}


	if (b_return)
		cout << "dotoywithanys succeeded" << endl;

	return b_return;
}

#endif // (defined(ADD_VARIANT_SUPPORT))



///////////////////////////////////////////////////////////////////////
// mainsub

int mainsub(int ac, char **av) {
	
	char pc_hostname[1024];			/* holds the name of the host the server is on */
	char pc_serverid[128];			/* holds server id */	
	CORBA(Boolean) b_failure = ILUCPP_FALSE;
	
	// our object
	foo(bar_var) mybar_var; 
	
	if ((ac > 3)) {		/* check for proper command line args */
		cout << g_c_usage << endl;
		return 1;
	}
	
	if (ac > 1)				/* get hostname */
		strcpy (pc_hostname, av[1]);
	else
		gethostname(pc_hostname, sizeof(pc_hostname));
	
	if (ac > 2)	{ 
		// set up the runtime for threaded operation
		iluCppRuntime::iluInitialize(ILUCPP_TRUE);
	}
	else 	{ 
		// set up the runtime for non threaded operation
		iluCppRuntime::iluInitialize(ILUCPP_FALSE);
	}

#ifndef SKIP_CUSTOM_SURROGATE_TEST
	// to test the ability to specialize surrogates (to do filtering, etc), we tell
	// the runtime to use our surrogate creator function for creating foo(bar) surrogates
	iluCppRuntime::iluSetSurrogateCreator(foo(bar)::iluGetILUClassRecord(),
		foo_bar_specialized_surrogate::create_surrogate);
#endif // SKIP_CUSTOM_SURROGATE_TEST

	/* create server id */
	strcpy(pc_serverid, pc_hostname);
	strcat(pc_serverid, ".parc.xerox.com.fooserver");
	
	cout << "looking up object foo_instance_0 at server " << pc_serverid << endl;
	
	// get an object ptr - note that this object has a refcount of 1
	try {
		mybar_var =  foo(bar)::iluLookup (
                         CONST_CAST(iluCString,"foo_instance_0"), pc_serverid);
	}
	catch(...) {
		cout << "foo::bar::iluLookup Exception" << endl;
		return -1;
	} 


	// run each of the tests
	if ((! doobjectandstring(mybar_var)) || (mybar_var->iluGetReferenceCount() != 1)) {
		cout << "doobjectandstring FAILED" << endl;
		b_failure = ILUCPP_TRUE;
	} 
 
	if ((! doping(mybar_var)) || (mybar_var->iluGetReferenceCount() != 1)) {
		cout << "doping FAILED" << endl;
		b_failure = ILUCPP_TRUE;
	} 

	if ((! dozap(0, mybar_var)) || (mybar_var->iluGetReferenceCount() != 1)) {
		cout << "dozap (no exception) FAILED" << endl;
		b_failure = ILUCPP_TRUE;
	}

	if ((! dozap(100, mybar_var)) || (mybar_var->iluGetReferenceCount() != 1)) {
		cout << "dozap (exception) FAILED" << endl;
		b_failure = ILUCPP_TRUE;
	}

	if ((! doobjecttableexercise(mybar_var)) || (mybar_var->iluGetReferenceCount() != 1)) {
		cout << "doobjecttableexercise FAILED" << endl;
		b_failure = ILUCPP_TRUE;
	}

	if ((! dopass_obj(mybar_var)) || (mybar_var->iluGetReferenceCount() != 1)) {
		cout << "dopass_obj FAILED" << endl;
		b_failure = ILUCPP_TRUE;
	}

	if ((! dopass_enum(mybar_var)) || (mybar_var->iluGetReferenceCount() != 1)) {
		cout << "dopass_enum FAILED" << endl;
		b_failure = ILUCPP_TRUE;
	}

	if ((! dopasslatinstring(mybar_var)) || (mybar_var->iluGetReferenceCount() != 1)) {
		cout << "dopasslatinstring FAILED" << endl;
		b_failure = ILUCPP_TRUE;
	}

	if ((! dopassunicodestring(mybar_var)) || (mybar_var->iluGetReferenceCount() != 1)) {
		cout << "dopassunicodestring FAILED" << endl;
		b_failure = ILUCPP_TRUE;
	}

	if ((! dopassfixedrecord(mybar_var)) || (mybar_var->iluGetReferenceCount() != 1)) {
		cout << "dopassfixedrecord FAILED" << endl;
		b_failure = ILUCPP_TRUE;
	} 

	if ((! dopassvariablerecord(mybar_var)) || (mybar_var->iluGetReferenceCount() != 1)) {
		cout << "dopassvariablerecord FAILED" << endl;
		b_failure = ILUCPP_TRUE;
	}

	if ((! dopassfixedarray(mybar_var)) || (mybar_var->iluGetReferenceCount() != 1)) {
		cout << "dopassfixedarray FAILED" << endl;
		b_failure = ILUCPP_TRUE;
	}


	if ((! dopassvariablearray(mybar_var)) || (mybar_var->iluGetReferenceCount() != 1)) {
                int count = mybar_var->iluGetReferenceCount();
                if( mybar_var->iluGetReferenceCount() > 1) { 
	           cout << "after dopassvariablearray - mybar_var count = "
                       << mybar_var->iluGetReferenceCount() << endl;
                   while(mybar_var->iluGetReferenceCount() > 1) 
                        mybar_var->iluDecrementReferenceCount();
                   cout << "dopassvariablearray FAILED refcount" << endl;
                }
                else {
                    cout << "dopassvariablearray FAILED" << endl;
	            b_failure = ILUCPP_TRUE;
                 }
	}


	if ((! dopassunboundedsequence(mybar_var)) || (mybar_var->iluGetReferenceCount() != 1)) {
		cout << "dopassunboundedsequence FAILED" << endl;
		b_failure = ILUCPP_TRUE;
	}	

	if ((! dopassboundedsequence(mybar_var)) || (mybar_var->iluGetReferenceCount() != 1)) {
		cout << "dopassboundedsequence FAILED" << endl;
		b_failure = ILUCPP_TRUE;
	} 

	if ((! dopassvariableunion(mybar_var)) || (mybar_var->iluGetReferenceCount() != 1)) {
		cout << "dopassvariableunion FAILED" << endl;
		b_failure = ILUCPP_TRUE;
	}

	if ((! dopassoptionals(mybar_var)) || (mybar_var->iluGetReferenceCount() != 1)) {
		cout << "dopassoptionals FAILED" << endl;
		b_failure = ILUCPP_TRUE;
	}
	
#if (defined(ADD_VARIANT_SUPPORT))

	if ((! dotoywithanys(mybar_var)) || (mybar_var->iluGetReferenceCount() != 1)) {
		cout << "dotoywithanys FAILED" << endl;
		b_failure = ILUCPP_TRUE;
	}

	if ((! dopasssimpleanys(mybar_var)) || (mybar_var->iluGetReferenceCount() != 1)) {
		cout << "dopasssimpleanys FAILED" << endl;
		b_failure = ILUCPP_TRUE;
	}
		
	if ((! dopassarrayanys(mybar_var)) || (mybar_var->iluGetReferenceCount() != 1)) {
		cout << "dopassarrayanys FAILED" << endl;
		b_failure = ILUCPP_TRUE;
	}	

	if ((! dopassassortedanys(mybar_var)) || (mybar_var->iluGetReferenceCount() != 1)) {
		cout << "dopassassortedanys FAILED" << endl;
		b_failure = ILUCPP_TRUE;
	}

#endif // (defined(ADD_VARIANT_SUPPORT))

	if (b_failure) {
		cout << "One or more tests FAILED" << endl;
		return -1;
	}
	else {
		cout << "All tests Succeeded" << endl;
	}
		
	/* try closing the server at this point to verify monitor outgoing connections sees it
	while (1) {
	_sleep(5);
	}
	*/
	return 0;
}


///////////////////////////////////////////////////////////////////////
// main

int main(int ac, char **av) {
	int i_return_value;
	
	i_return_value = mainsub(ac, av);
	
	
	return i_return_value;
}


///////////////////////////////////////////////////////////////////////
//  End of file
///////////////////////////////////////////////////////////////////////

