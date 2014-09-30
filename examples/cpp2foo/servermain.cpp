/**
$Id: servermain.cpp,v 1.15 1999/08/03 01:58:40 janssen Exp $
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
// ILU cpp2 implementation of foo server
// Last edited by Mike Spreitzer October 8, 1998 10:40 pm PDT


#include <iostream.h>
#include <math.h>

#ifdef WIN32
#include <winsock.h>
#else
extern "C" int gethostname(char *name, int namelen);
#endif

#include "foo-cpptrue.hpp"


///////////////////////////////////////////////////////////////////////
// implementation class

class foo_bar_impl : public virtual foo(bar) {
	
public:
	
	// constructor
	foo_bar_impl(char* pc_instance_handle, iluServer& r_an_ilu_server= iluServer::iluGetDefaultServer(),
		CORBA(Boolean) b_within_object_table = ILUCPP_FALSE) : 
	  iluObject(iluGetILUClassRecord(), pc_instance_handle, r_an_ilu_server, b_within_object_table) {}
	  
	  virtual ~foo_bar_impl() {
		  iluDeactivate();
	  }
	  
	  // methods
	  virtual CORBA(Boolean) zap( CORBA(Long)  inarg,
		  CORBA(Octet)&  inoutarg,
		  CORBA(Double)& outarg ) 
		  throw (CORBA(SystemException), foo(zapexception));

	 virtual foo(bar_ptr) passobj (foo(bar_ptr) inarg, foo(bar_ptr)& inoutarg, foo(bar_ptr)& outarg)
		  throw (CORBA(SystemException));

	 virtual foo(enumtype) passenum (foo(enumtype) inarg, foo(enumtype)& inoutarg, foo(enumtype)& outarg)
		  throw (CORBA(SystemException));

	 virtual foo(LatinString) passlatinstring (foo(const_LatinString) inarg, foo(LatinString)& inoutarg, foo(LatinString)& outarg);

	 virtual foo(UnicodeString) passunicodestring (foo(const_UnicodeString) inarg, foo(UnicodeString)& inoutarg, foo(UnicodeString)& outarg);

	 virtual foo(fixedrecordtype)	passfixedrecord(
		 const foo(fixedrecordtype)& inarg, 
		 foo(fixedrecordtype)& inoutarg, 
		 foo(fixedrecordtype)& outarg);
	 
	 virtual foo(variablerecordtype)* passvariablerecord(
		 const foo(variablerecordtype)& inarg, 
		 foo(variablerecordtype)& inoutarg, 
		 foo(variablerecordtype)*& outarg);
	 
	 virtual foo(integerarray_slice)* passfixedarray(
		 const foo(integerarray) inarg, 
		 foo(integerarray) inoutarg, 
		 foo(integerarray) outarg);
	 
	 virtual foo(bararray_slice)* passvariablearray(
		 const foo(bararray) inarg, 
		 foo(bararray) inoutarg, 
		 foo(bararray_slice)*& outarg);

	 virtual foo(unboundedlongseq)* passunboundedsequence(
		 const foo(unboundedlongseq)& inarg, 
		 foo(unboundedlongseq)& inoutarg, 
		 foo(unboundedlongseq)*& outarg);
	 
	 virtual foo(boundedbarseq)* passboundedsequence(
		 const foo(boundedbarseq)& inarg, 
		 foo(boundedbarseq)& inoutarg, 
		 foo(boundedbarseq)*& outarg);

	 virtual foo(variableuniontype)* passvariableunion(
		 const foo(variableuniontype)& inarg, 
		 foo(variableuniontype)& inoutarg, 
		 foo(variableuniontype)*& outarg);

	 virtual foo(optionalinteger) passoptionals(
		 foo(const_optionalinteger) inarg, 
		 foo(optionalbarobject)& inoutarg, 
		 foo(optionalbarobject)& outarg);

#if (defined(ADD_VARIANT_SUPPORT))
	 virtual CORBA(Any)* passsimpleanys(
			  const CORBA(Any)& inarg, 
			  CORBA(Any)& inoutarg, 
			  CORBA(Any)*& outarg);

	 virtual CORBA(Any)* passarrayanys(
			  const CORBA(Any)& inarg, 
			  CORBA(Any)& inoutarg, 
			  CORBA(Any)*& outarg);

	 virtual CORBA(Any)* passassortedanys(
			  const CORBA(Any)& inarg, 
			  CORBA(Any)& inoutarg, 
			  CORBA(Any)*& outarg);
#endif // (defined(ADD_VARIANT_SUPPORT))

};


///////////////////////////////////////////////////////////////////////
// method implementation

// methods
CORBA(Boolean) foo_bar_impl::zap( CORBA(Long)  inarg,
		  CORBA(Octet)&  inoutarg,
		  CORBA(Double)& outarg ) 
		  throw (CORBA(SystemException), foo(zapexception))
		  {
	
	
	cout << "foo_bar_impl::zap entered" << endl;
	cout << "inarg = " << inarg << ", inoutarg  = 0x" << hex << (unsigned int)inoutarg << dec << endl;
	
	if (inarg > 0) {
		// raise zapexception
		foo(zapexception) a_zapexception(inarg);
		throw(a_zapexception);
	}
	
	outarg = 3.14159;
	inoutarg = ~(inoutarg);

	// xxx danl temp robustness test
	// ILUCPP_NYI();

	return ilu_TRUE;
}


foo(bar_ptr) foo_bar_impl::passobj (foo(bar_ptr) inarg, foo(bar_ptr)& inoutarg, foo(bar_ptr)& outarg)
		  throw (CORBA(SystemException)) {
	
	cout << "foo_bar_impl::passobj entered" << endl;

	// refcount should now be four (one for original, one for being the discriminator 
	// one for inarg one for inoutarg (or five if it's collectible)
	cout << "inarg->iluGetReferenceCount() = " << inarg->iluGetReferenceCount()  
		<< (inarg->iluGetReferenceCount() == ((iluCardinal)(inarg->iluIsCollectibleObject() ? 5 : 4)) ? " Good" : " Bad") << endl;

	outarg = inoutarg;

	// duplicate our out and return arg since we want to hang onto it
	// refcount should now be six (note don't duplicate inout arg since
	// we didn't do anything with it
	foo(bar)::_duplicate(outarg);
	foo(bar)::_duplicate(inarg); // what we'll return

	// refcount should now be six (or 7 if it's collectible)
	cout << "inarg->iluGetReferenceCount() = " << inarg->iluGetReferenceCount()  
		<< (inarg->iluGetReferenceCount() == ((iluCardinal)(inarg->iluIsCollectibleObject() ? 7 : 6)) ? " Good" : " Bad") << endl;


	return inarg;
}


foo(enumtype) foo_bar_impl::passenum (foo(enumtype) inarg, foo(enumtype)& inoutarg, foo(enumtype)& outarg)
		  throw (CORBA(SystemException)) {
	
	cout << "foo_bar_impl::passenum entered" << endl;

	cout << "Args are"  
		<< ((inarg == foo(red)) && (inoutarg == foo(orange)) ? " Good" : " Bad") << endl;

	inoutarg = foo(yellow); 
	outarg = foo(green);

	return foo(blue);
}


foo(LatinString) foo_bar_impl::passlatinstring (foo(const_LatinString) inarg, foo(LatinString)& inoutarg, foo(LatinString)& outarg) {
	cout << "foo_bar_impl::passlatinstring entered inarg = " << inarg << ", inoutarg = " << inoutarg << endl;
	CORBA(string_free)(inoutarg);
	inoutarg = CORBA(string_dup)("FromServer_inoutarg");
	outarg = CORBA(string_dup)("FromServer_outarg");
	return CORBA(string_dup)("FromServer_return");
}



foo(UnicodeString) foo_bar_impl::passunicodestring (foo(const_UnicodeString) inarg, foo(UnicodeString)& inoutarg, foo(UnicodeString)& outarg) {
	cout << "foo_bar_impl::passunicodestring entered inarg = " << inarg << ", inoutarg = " << inoutarg << endl;

	iluCppRuntime::iluFree(inoutarg);

	inoutarg = iluCppRuntime::iluCharStringFromShortCharString("FromServer_inoutarg");

	outarg = iluCppRuntime::iluCharStringFromShortCharString("FromServer_outarg");

	foo(UnicodeString) returnval = iluCppRuntime::iluCharStringFromShortCharString("FromServer_return");

	return returnval;
}


foo(fixedrecordtype) foo_bar_impl::passfixedrecord(
													  const foo(fixedrecordtype)& inarg, 
													  foo(fixedrecordtype)& inoutarg, 
													  foo(fixedrecordtype)& outarg) {

	cout << "foo_bar_impl::passfixedrecord entered" << endl;

	foo(fixedrecordtype) return_value;
	// make outarg a copy of inarg
	outarg = inarg;

	// make return be what inout arg is
	return_value = inoutarg;

	//  make inout arg one more in each field
	inoutarg.fixedrecordinteger = inoutarg.fixedrecordinteger + 1;
	inoutarg.fixedrecordenum = foo(green);

	return return_value;
}



foo(variablerecordtype)* foo_bar_impl::passvariablerecord(
														  const foo(variablerecordtype)& inarg, 
														  foo(variablerecordtype)& inoutarg, 
														  foo(variablerecordtype)*& outarg) {

	cout << "foo_bar_impl::passvariablerecord entered" << endl;

	foo(variablerecordtype)* p_return_value = new foo(variablerecordtype);
	outarg = new foo(variablerecordtype);

	// outarg should be copy of inarg
	*outarg = inarg;

	//  return should be what inout arg is
	*p_return_value = inoutarg;
	
	inoutarg.variablerecordstring =   CORBA(string_dup)("returned passvariablerecord_inoutarg");
	inoutarg.variablerecordinteger = inoutarg.variablerecordinteger + 1;

	return p_return_value;
}


foo(integerarray_slice)* foo_bar_impl::passfixedarray(
			  const foo(integerarray) inarg, 
			  foo(integerarray) inoutarg, 
			  foo(integerarray) outarg) {
		CORBA(Long) index0, index1;

	cout << "foo_bar_impl::passfixedarray entered" << endl;

	foo(integerarray_slice)* return_value = foo(integerarray_alloc)();

	// make outarg a copy of inarg
	for (index0 = 0; index0 < 5; index0++) {
		for (index1 = 0; index1 < 10; index1++) {
			outarg[index0][index1] = inarg[index0][index1];
		}
	}

	// make return be what inout arg is
	for (index0 = 0; index0 < 5; index0++) {
		for (index1 = 0; index1 < 10; index1++) {
			return_value[index0][index1] = inoutarg[index0][index1];
		}
	}

	//  make inout arg one more in each field
	for (index0 = 0; index0 < 5; index0++) {
		for (index1 = 0; index1 < 10; index1++) {
			inoutarg[index0][index1] = inoutarg[index0][index1] + 1;
		}
	}

	return return_value;
}


foo(bararray_slice)* foo_bar_impl::passvariablearray(
			  const foo(bararray) inarg, 
			  foo(bararray) inoutarg, 
			  foo(bararray_slice)*& outarg) {
		CORBA(Long) index0, index1;

	cout << "foo_bar_impl::passvariablearray entered" << endl;

	foo(bararray_slice)* return_value = foo(bararray_alloc)();
	outarg = foo(bararray_alloc)();

	// make outarg a copy of inarg
	for (index0 = 0; index0 < 2; index0++) {
		for (index1 = 0; index1 < 3; index1++) {
			outarg[index0][index1] = inarg[index0][index1];
		}
	}

	// make return be what inout arg is
	for (index0 = 0; index0 < 2; index0++) {
		for (index1 = 0; index1 < 3; index1++) {
			return_value[index0][index1] = inoutarg[index0][index1];
		}
	}

	//  make inout arg a copy of inarg
	for (index0 = 0; index0 < 2; index0++) {
		for (index1 = 0; index1 < 3; index1++) {
			inoutarg[index0][index1] = inarg[index0][index1];
		}
	}

	return return_value;
}


foo(unboundedlongseq)* foo_bar_impl::passunboundedsequence(
		 const foo(unboundedlongseq)& inarg, 
		 foo(unboundedlongseq)& inoutarg, 
		 foo(unboundedlongseq)*& outarg) {

	cout << "foo_bar_impl::passunboundedsequence entered" << endl;


	// make outarg a copy of inarg
	outarg = new foo(unboundedlongseq)(inarg);

	// make return be what inout arg is
	foo(unboundedlongseq)* p_return_value = new foo(unboundedlongseq)(inoutarg);

	//  make inout arg a copy of inarg
	inoutarg = inarg;

	return p_return_value;
}
	 

foo(boundedbarseq)* foo_bar_impl::passboundedsequence(
		 const foo(boundedbarseq)& inarg, 
		 foo(boundedbarseq)& inoutarg, 
		 foo(boundedbarseq)*& outarg) {

	//cout << "foo_bar_impl::passboundedsequence entered" << endl;

	// make outarg a copy of inarg
	outarg = new foo(boundedbarseq)(inarg);

	//cout << "foo_bar_impl::passboundedsequence made outarg a copy of inarg" << endl;


	// make return be what inout arg is
	foo(boundedbarseq)* p_return_value = new foo(boundedbarseq)(inoutarg);
	//cout << "foo_bar_impl::passboundedsequence  made return be what inout arg is" << endl;


	//  make inout arg a copy of inarg
	inoutarg = inarg;
	//cout << "foo_bar_impl::passboundedsequence  made inout arg a copy of inarg" << endl;


	return p_return_value;
}


foo(variableuniontype)* foo_bar_impl::passvariableunion(
		 const foo(variableuniontype)& inarg, 
		 foo(variableuniontype)& inoutarg, 
		 foo(variableuniontype)*& outarg) {

	cout << "foo_bar_impl::passvariableunion entered" << endl;

	// make outarg a copy of inarg
	outarg = new foo(variableuniontype)(inarg);

	// make return be what inout arg is
	foo(variableuniontype)* p_return_value = new foo(variableuniontype)(inoutarg);

	//  make inout arg a copy of inarg
	inoutarg = inarg;

	return p_return_value;
}

foo(optionalinteger) foo_bar_impl::passoptionals(
		 foo(const_optionalinteger) inarg, 
		 foo(optionalbarobject)& inoutarg, 
		 foo(optionalbarobject)& outarg){

	cout << "foo_bar_impl::passoptionals entered" << endl;

	if (inarg)
		cout << "foo_bar_impl::passoptionals - inarg not null!" << endl;

	if (inoutarg != this)
		cout << "foo_bar_impl::passoptionals - inoutarg not this!" << endl;

	// make outarg this true object
	outarg = this;

	// make return be 40
	foo(optionalinteger) p_return_value = new CORBA(Long);
	*p_return_value = 40;

	//  make inout arg NULL
	inoutarg = NULL;

	return p_return_value;
}


#if (defined(ADD_VARIANT_SUPPORT))

CORBA(Any)* foo_bar_impl::passsimpleanys(
			  const CORBA(Any)& inarg, 
			  CORBA(Any)& inoutarg, 
			  CORBA(Any)*& outarg) {

	cout << "foo_bar_impl::passsimpleanys entered" << endl;

	CORBA(Any)* p_return_value = new CORBA(Any);
	outarg = new CORBA(Any);

	CORBA(Long) l_inarg_any_value = 0;
	CORBA(Double) d_out_any_value = 0.0;

	CORBA(Boolean) inout_boolean = ILUCPP_TRUE;
	CORBA(Any)::from_boolean inout_from_boolean(inout_boolean);

	if (! (inarg >>= l_inarg_any_value)) {
		cout << "passsimpleanys couldn't get long from inarg" << endl;
	}

	d_out_any_value = sqrt((double)l_inarg_any_value);

	(*outarg) <<= d_out_any_value;

	CORBA(string) pc_inout_any_string = 
               CONST_CAST(iluCString,"Not a value");
	CORBA(Any)::to_string a_to_string (pc_inout_any_string, 0);
	if (! (inoutarg >>= a_to_string)) {
		cout << "passsimpleanys couldn't get string from inoutarg" << endl;
	}

	CORBA(string) pc_return_any_string = new char[strlen(pc_inout_any_string) + 1];
	strcpy(pc_return_any_string, pc_inout_any_string);
	CORBA(Any)::from_string a_from_string (pc_return_any_string, 0);
	(*p_return_value) <<= a_from_string;

	inoutarg <<= inout_from_boolean;
	
	return p_return_value;
}


CORBA(Any)* foo_bar_impl::passarrayanys(
										const CORBA(Any)& inarg, 
										CORBA(Any)& inoutarg, 
										CORBA(Any)*& outarg) {
	
	cout << "foo_bar_impl::passarrayanys entered" << endl;
	
	CORBA(Any)* p_return_value = new CORBA(Any);
	outarg = new CORBA(Any);
	
	foo(integerarray_forany)  in_integerforany;
	foo(bararray_forany)	  inout_barforany;
	foo(bararray_forany)      out_barforany;
	foo(integerarray_forany)  inout_integerforany;
	int i_xindex, i_yindex;
	
	
	if (! (inarg >>= in_integerforany)) {
		cout << "passarrayanys couldn't get integerarray_forany from inarg" << endl;
	}
	
	if (! (inoutarg >>= inout_barforany)) {
		cout << "passarrayanys couldn't get bararray_forany from inoutarg" << endl;
	}
	
	// outarg should be what inout arg came in as
	(*outarg) = inoutarg;
	
	//  return should be what inarg was with all the elements doubled
	// make up an intarray
#ifdef __GNUC__
	// get around G++ (2.7.2) problem with 
 	// distinguishing between conversion to const T and conversion to T
	foo(integerarray_slice)*  p_return_integerslice =  foo(integerarray_dup)(in_integerforany.iluGetSlice());
#else
	foo(integerarray_slice)*  p_return_integerslice = foo(integerarray_dup)(in_integerforany);
#endif
	for (i_xindex = 0; i_xindex < 5; i_xindex++) {
		for (i_yindex = 0; i_yindex < 10; i_yindex++) {
			p_return_integerslice[i_xindex][i_yindex] = 2 * p_return_integerslice[i_xindex][i_yindex];
		}
	}
	foo(integerarray_forany)  return_integerforany(p_return_integerslice);
	(*p_return_value) <<= return_integerforany;
	
	//  inout should be what inarg was
	inoutarg = inarg;
	
	return p_return_value;
}


CORBA(Any)* foo_bar_impl::passassortedanys(
										const CORBA(Any)& inarg, 
										CORBA(Any)& inoutarg, 
										CORBA(Any)*& outarg) {
	
	cout << "foo_bar_impl::passassortedanys entered" << endl;
	
	CORBA(Any)* p_return_value = new CORBA(Any);
	outarg = new CORBA(Any);
	
	foo(fixedrecordtype)* p_inarg_any_value;
	if (! (inarg >>= p_inarg_any_value))
		cout << "foo_bar_impl::passassortedanys inarg extraction problem" << endl;
		
	foo(optionalbarobject_forany)* p_inoutarg_received_value;
	if (! (inoutarg >>= p_inoutarg_received_value))  // should be 'bar not present'
		cout << "foo_bar_impl::passassortedanys inoutarg extraction problem" << endl;
		else if (p_inoutarg_received_value->m_p_value != NULL)
		cout << "foo_bar_impl::passassortedanys bad inoutarg received" << endl;


	// inout arg going back should be what inarg was - using copying insertion here
	inoutarg <<= (*p_inarg_any_value);

	// duplicate ourselves since we'll be putting this into an optionalbarobject_forany
	foo(bar)::_duplicate(this);
	foo(optionalbarobject_forany)* p_outarg_any_value = new foo(optionalbarobject_forany)(this);
	// noncopying insertion
	(*outarg) <<= p_outarg_any_value;

	// return should be a variable union set to 78
	foo(variableuniontype)* p_return_any_value = new foo(variableuniontype);
	p_return_any_value->integerarm(78);
	(*p_return_value) <<= p_return_any_value;
	
	return p_return_value;
}

#endif //  #if (defined(ADD_VARIANT_SUPPORT))



///////////////////////////////////////////////////////////////////////
// Object Table Testing

class fooserver_objecttable : public iluObjectTable {

	// Called by ILU to create and return a new iluObject* with the specified instance 
	// handle.  ILU retains ownership of pc_instance_handle - i.e. copy it if you need
	// want to hang on to it
	virtual iluObject* iluObjectOfInstanceHandle (iluCString pc_instance_handle /* ILUowned */);

};


// Nothing fancy here, simply create a new foo_bar_impl with the given handle
// to test the object table functionality
iluObject* fooserver_objecttable::iluObjectOfInstanceHandle(iluCString pc_instance_handle) {
	foo(bar_ptr) p_a_true_foo_bar;			// points to true object
	
	// create a true foo object - note here we set the boolean arg to true to 
	// indicate that this is happening inside an object table function
	p_a_true_foo_bar = new foo_bar_impl(pc_instance_handle, *iluGetServer(), ILUCPP_TRUE);

	cout << "fooserver_objecttable::iluObjectOfInstanceHandle(" << pc_instance_handle << ")" << endl;

	return p_a_true_foo_bar;
}


///////////////////////////////////////////////////////////////////////
// Usage string

char g_c_usage[] = 
"Usage:  fooserver  [help | -help | ? | -?]  |  [-mt] [-p protocol_info] [-t transport_info+ ]\n\
\thelp | -help | ? | -? : displays usage only \n\
\t-mt - if present, sets ILU to multithreaded operation\n\
\t-p - if present, sets protocol to use\n\
\t-t - if present, sets transports to use\n";



///////////////////////////////////////////////////////////////////////
// main

int main (int ac, char **av) {
	
	foo(bar_ptr) p_true_foo_bar;			// points to true object
	char pc_serverid[512];					// holds server id 
	char pc_hostname[512];					// holds the name of the host the server is on
	CORBA(Boolean) b_run_threaded = ILUCPP_FALSE;
	char* pc_protocol_info = NULL;
	char* ppc_transport_info[8] = {NULL};
	int i_arg_index = 1;
	
	// check over the arguments
	while (i_arg_index < ac) {
		
		// check for use of specific protocol
		if (strcmp(av[i_arg_index], "-p") == 0) {
			if (i_arg_index++ < ac)
				pc_protocol_info = av[i_arg_index++];
			else {
				cout << g_c_usage;
				return 1;
			}
			continue;
		} 
		
		// check for use of specific transports
		if (strcmp(av[i_arg_index], "-t") == 0) {
			int i_transport_index = 0;
			++i_arg_index;
			while ((i_arg_index < ac) && (av[i_arg_index][0] != '-'))
				ppc_transport_info[i_transport_index++] = av[i_arg_index++];
			ppc_transport_info[i_transport_index] = NULL;
			continue;
		} 
		
		// check whether to run threaded
		if (strcmp(av[i_arg_index], "-mt") == 0) {
			b_run_threaded = ILUCPP_TRUE; i_arg_index++;
			continue;
		} 
		
		// check whether to show usage
		if ((strcmp(av[i_arg_index], "help") == 0) ||
			(strcmp(av[i_arg_index], "-help") == 0) ||
			(strcmp(av[i_arg_index], "?") == 0) ||
			(strcmp(av[i_arg_index], "-?") == 0)) {
			cout << g_c_usage;
			return 1;
		} 
		
		// unknown switch
		cout << g_c_usage;
		return 1;
	}
	

	// set up the runtime
	iluCppRuntime::iluInitialize(b_run_threaded);
	

	// show how we're running
	cout << "Running " << (b_run_threaded ? "threaded" : "single threaded") << endl;
	cout << "Protocol  " << (pc_protocol_info ? pc_protocol_info : iluServer::iluGetDefaultProtocol()) << endl;
	cout << "Transport ";
	{ iluTransportInfo ppc_walker = ( ppc_transport_info[0] != NULL ? ppc_transport_info : iluServer::iluGetDefaultTransport());
	int i_transport_index = 0;
	while (ppc_walker[i_transport_index] != NULL) {
		cout << ppc_walker[i_transport_index] << " ";
		i_transport_index++;
	}
	cout << endl;
	}


	/* create server id */
	gethostname(pc_hostname, sizeof(pc_hostname));
	strcpy(pc_serverid, pc_hostname);
	strcat(pc_serverid, ".parc.xerox.com.fooserver");
	
	// make an object table
	fooserver_objecttable a_fooserver_object_table;

	// create our server
	iluServer server (pc_serverid, &a_fooserver_object_table, 
		pc_protocol_info, 
		( ppc_transport_info[0] != NULL ? ppc_transport_info : ((iluTransportInfo) NULL)));

	// make it our default server
	iluServer::iluSetDefaultServer(server);

	// create a true foo object
	p_true_foo_bar = new foo_bar_impl(
             CONST_CAST(iluCString,"foo_instance_0"), server);
	
	if (p_true_foo_bar == NULL) {
		cout << "Error - couldn't create foo_instance_0 object" << endl;
		return 1;
    }
	
	// publish the true foo object
	if (!p_true_foo_bar->iluPublish()) {
		cout << "Error - couldn't publish foo_instance_0 object" << endl;
		return 1;
    }
	else {
		iluCString pc_object_string = p_true_foo_bar->iluObjectToString();
		cout << "Exported " << pc_object_string << endl;
		iluCppRuntime::iluFree(pc_object_string);
#ifdef IIOP_PROTOCOL
		pc_object_string = p_true_foo_bar->iluObjectToIORString();
		if (pc_object_string) { // may not be exported via IIOP port.
			cout << "IOR is " << pc_object_string << endl;
			iluCppRuntime::iluFree(pc_object_string);
		}
#endif
#ifdef HTTPP_PROTOCOL
		pc_object_string = p_true_foo_bar->iluObjectToURLString();
		if (pc_object_string) { // may not be exported via HTTP port.
			cout << "URL is " << pc_object_string << endl;
			iluCppRuntime::iluFree(pc_object_string);
		}
#endif
	}
	
	// run the server
	server.iluRun();
	
	return 0;
}


///////////////////////////////////////////////////////////////////////
//  End of file
///////////////////////////////////////////////////////////////////////

