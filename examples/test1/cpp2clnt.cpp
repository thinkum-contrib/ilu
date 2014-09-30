/** $Id: cpp2clnt.cpp,v 1.16 1999/08/03 01:52:25 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 11:27 am PDT */

/* ILU's CORBA 2.0 C++ version of Test1 client side */

///////////////////////////////////////////////////////////////////////
// #includes

#include <iostream.h>

#include "Test1-cppsurrogate.hpp"
#include "Test2-cppsurrogate.hpp"
#include "Test3-cppsurrogate.hpp" 


#ifdef SECURE_TRANSPORT
#include <ilugssmech_nil.h>
#include <ilugssns_rfc822.h>
#endif 


///////////////////////////////////////////////////////////////////////
// test result codes

#define TEST1_RUNTIME_INIT_PROBLEM		-1
#define TEST1_U_CSS_to_U_PROBLEM		-2
#define TEST1_f_CSS_to_RO_PROBLEM		-3
#define TEST1_R_ScS_to_F_PROBLEM		-4
#define TEST1_a_RO_PROBLEM				-5
#define TEST1_get_O2_PROBLEM			-6
#define TEST1_OO_A0_to_CSS_PROBLEM		-7
#define TEST1_R_I_A1_to_I_A0_PROBLEM	-8
#define TEST1_get_O3_FALSE_PROBLEM		-9
#define TEST1_get_O3_TRUE_PROBLEM		-10
#define TEST1_R_to_R_PROBLEM			-11


///////////////////////////////////////////////////////////////////////
// output a number of tabs

inline void tabit(int i_numtabs) {
	for(; i_numtabs > 0; i_numtabs--)
		cout << '\t';
}


///////////////////////////////////////////////////////////////////////
// Show a StringVar 

void show(CORBA(String_var)& r_a_string_var, int i_numtabs = 0, const char* pc_prefix = "") {
	tabit(i_numtabs);		
	cout << pc_prefix << r_a_string_var << endl;
}

///////////////////////////////////////////////////////////////////////
// Show a system exception

void show(const CORBA(SystemException)& the_exception, int i_numtabs = 0, const char* pc_prefix = "") {
	tabit(i_numtabs);
	cout << "SystemException: " << pc_prefix << "minor = " << the_exception.minor() << ", completed = ";
		switch (the_exception.completed()) {
	case CORBA(COMPLETED_YES): cout << "YES"; break;
	case CORBA(COMPLETED_NO): cout << "NO"; break;
	case CORBA(COMPLETED_MAYBE): cout << "MAYBE"; break;
	default: cout << "??? unknown exception completion !!!";
	}
	cout << endl;
}


///////////////////////////////////////////////////////////////////////
// Show an E1 exception

void show(const Test1(E1)& the_exception, int i_numtabs = 0, const char* pc_prefix = "") {
	tabit(i_numtabs);
	cout << "Test1(E1) Exception: " << pc_prefix << endl;
		show(the_exception._value(), i_numtabs + 1);
}


///////////////////////////////////////////////////////////////////////
// Show an E2 exception

void show(const Test1(E2)& the_exception, int i_numtabs = 0, const char* pc_prefix = "") {
	tabit(i_numtabs);
	cout << "Test1(E2) Exception: " << the_exception._value() << endl;
}


///////////////////////////////////////////////////////////////////////
// Show an CantCreate exception

void show(const Test1(CantCreate)& the_exception, int i_numtabs = 0, const char* pc_prefix = "") {
	tabit(i_numtabs);
	cout << "Test1(CantCreate) Exception: " << pc_prefix << endl;
}


///////////////////////////////////////////////////////////////////////
// Show an ScS (sequence of short character => char*)

void show(Test1(ScS)& r_a_test1_scs, int i_numtabs = 0, const char* pc_prefix = "") {
	tabit(i_numtabs);
//xxx mapping for seq of short char xxx 	cout << "Test1(ScS): " << pc_prefix << " " << (r_a_test1_scs ? r_a_test1_scs : "NULL") << endl;
}


///////////////////////////////////////////////////////////////////////
// Show an ARRAY OF 3 ScS (array of three char*'s)

void show(Test1(TheA1)& r_a_test1_a1, int i_numtabs = 0, const char* pc_prefix = "") {
	tabit(i_numtabs);	
	cout << "Test1(TheA1): " << pc_prefix << endl;
	for (int i = 0; i < 3; i++)
		show(r_a_test1_a1[i], i_numtabs + 1);
}


///////////////////////////////////////////////////////////////////////
// Show CSS (a Sequence of ARRAY OF 3 ScS (array of three char*'s))

void show(Test1(CSS)& r_a_test1_css, int i_numtabs = 0, const char* pc_prefix = "") {
	tabit(i_numtabs);	
	cout << "Test1(CSS): " << pc_prefix << endl;
	for (unsigned int i = 0; i < r_a_test1_css.length(); i++)
		show(r_a_test1_css[i], i_numtabs + 1);
}


///////////////////////////////////////////////////////////////////////
// Show an I Integer

void show(Test1(I)& r_a_test1_i, int i_numtabs = 0, const char* pc_prefix = "") {
	tabit(i_numtabs);	
	cout << "Test1(I): " << pc_prefix << r_a_test1_i << endl;
}


///////////////////////////////////////////////////////////////////////
// Show a iluShortReal

void show(iluShortReal& r_a_f, int i_numtabs = 0, const char* pc_prefix = "") {
	tabit(i_numtabs);	
	cout << "iluShortReal: " << pc_prefix << r_a_f << endl;
}

///////////////////////////////////////////////////////////////////////
// Show a iluReal

void show(iluReal& r_a_f, int i_numtabs = 0, const char* pc_prefix = "") {
	tabit(i_numtabs);	
	cout << "iluReal: " << pc_prefix << r_a_f << endl;
}


///////////////////////////////////////////////////////////////////////
// Show an R record

void show(Test1(R)& r_a_test1_r, int i_numtabs = 0, const char* pc_prefix = "") {
	tabit(i_numtabs);	
	cout << "Test1(R): " << pc_prefix << endl;
	show(r_a_test1_r.a, i_numtabs + 1);
	show(r_a_test1_r.css, i_numtabs + 1);
	show(r_a_test1_r.i, i_numtabs + 1);
}


///////////////////////////////////////////////////////////////////////
// Show an R0 (optional record)

void show(Test1(RO) p_a_test1_ro, int i_numtabs = 0, const char* pc_prefix = "") {
	tabit(i_numtabs);	
	
	if (p_a_test1_ro) {
		cout << "Test1(RO): " << pc_prefix << "Present"  << endl;
		show(*p_a_test1_ro, i_numtabs + 1);
	}
	else 
		cout << "Test1(RO): " << pc_prefix << "NotPresent" << endl;
	
}


///////////////////////////////////////////////////////////////////////
// Show an O1 (Object)


void show(Test1(O1)* p_a_test1_o1, int i_numtabs = 0, const char* pc_prefix = "") {
	tabit(i_numtabs);	
	if (p_a_test1_o1)
		cout << "Test1(O1): " << pc_prefix << "SBH = " << p_a_test1_o1->iluObjectToString() << " class = " << 
		p_a_test1_o1->iluClassName() << endl;
	else
		cout << "Test1(O1): " << pc_prefix << "NULL" << endl;
}

void show(Test1(O1)& r_a_test1_o1, int i_numtabs = 0, const char* pc_prefix = "") {
	show(&r_a_test1_o1);
}


///////////////////////////////////////////////////////////////////////
// Show an OO (optional O1 object)

void show_opt(Test1(OO) p_a_test1_o1, int i_numtabs = 0, const char* pc_prefix = "") {
	tabit(i_numtabs);		
	if (p_a_test1_o1) {
		cout << "Test1(OO): " << pc_prefix << "Present" << endl;
		show(*p_a_test1_o1, i_numtabs + 1);
	}
	else 
		cout << "Test1(OO): " << pc_prefix << "NotPresent" << endl;
}


///////////////////////////////////////////////////////////////////////
// Show an O2 (Object)


void show(Test1(O2)* p_a_test1_o2, int i_numtabs = 0, const char* pc_prefix = "") {
	tabit(i_numtabs);	
	if (p_a_test1_o2)
		cout << "Test1(O2): " << pc_prefix << "SBH = " << p_a_test1_o2->iluObjectToString() << " class = " << 
		p_a_test1_o2->iluClassName() << endl;
	else
		cout << "Test1(O2): " << pc_prefix << "NULL" << endl;
}

void show(Test1(O2)& r_a_test1_o2, int i_numtabs = 0, const char* pc_prefix = "") {
	show (&r_a_test1_o2);
}


///////////////////////////////////////////////////////////////////////
// Show an O3 (Object)

void show(Test1(O3)* p_a_test1_o3, int i_numtabs = 0, const char* pc_prefix = "") {
	tabit(i_numtabs);	
	if (p_a_test1_o3)
		cout << "Test1(O3): " << pc_prefix << "SBH = " << p_a_test1_o3->iluObjectToString() << " class = " << 
		p_a_test1_o3->iluClassName() << endl;
	else
		cout << "Test1(O3): " << pc_prefix << "NULL" << endl;
}

void show(Test1(O3)& r_a_test1_o3, int i_numtabs = 0, const char* pc_prefix = "") {
	show (&r_a_test1_o3);
}


///////////////////////////////////////////////////////////////////////
// Show a BOOLEAN

void show(iluBoolean& r_a_test1_bool, int i_numtabs = 0, const char* pc_prefix = "") {
	tabit(i_numtabs);		
	cout << "Boolean: " << pc_prefix << (r_a_test1_bool ? "TRUE" : "FALSE") << endl;
}


///////////////////////////////////////////////////////////////////////
//  show a Test1 U union

void show(Test1(U)& r_a_test1_u, int i_numtabs = 0, const char* pc_prefix = "") {

	tabit(i_numtabs);		
	cout << "Test1(U): " << pc_prefix << "discriminator = " << r_a_test1_u._d() << endl;
	CORBA(Boolean) b_value;
	
	switch (r_a_test1_u._d()) {
    case 0:   // Record TheR 
		cout << "R ";
		show(r_a_test1_u._R_arm(), i_numtabs + 1);
		return;
    case 1:   // Optional Record TheR 
		cout << "RO ";
		show(r_a_test1_u._RO_arm(), i_numtabs + 1);
		return;
	case 2:   // CSS - sequence of strings 
		cout << "CSS ";
		show(r_a_test1_u._CSS_arm(), i_numtabs + 1);
		return;
	case 3:   // TheO1 OBJECT
		cout << "O1 ";
		show(r_a_test1_u._O1_arm(), i_numtabs + 1);
		return;
	case 4:   // Optional TheO1 OBJECT
		cout << "OO ";
		show_opt(r_a_test1_u._OO_arm(), i_numtabs + 1);
		return;
	case 5:   // Boolean
		b_value = r_a_test1_u._CORBA_Boolean_arm();
		cout << "BOOLEAN ";
		show(b_value, i_numtabs + 1);	
		return;
	default:
		cout << "(unexpected value)" << endl;
		return;
    }
}

///////////////////////////////////////////////////////////////////////
// Show a AO (passed in a _var)

void show(Test1(A0_var)& r_a_test1_a0_var, int i_numtabs = 0, const char* pc_prefix = "") {
	tabit(i_numtabs);		
	cout << "Test1(A0): " << pc_prefix << endl;
	for (int i = 0; i < 8; i++) {
		tabit(i_numtabs + 1);
// Changed 7/11 psn
#ifdef _MSC_VER
		cout << "0x" << hex << ((unsigned short)(((Test1(A0))r_a_test1_a0_var)[i])) << " ";
#else
		cout << "0x" << hex << ((unsigned short)(r_a_test1_a0_var[i])) << " ";
#endif
	}
	cout << endl;
}




///////////////////////////////////////////////////////////////////////
// doit - run the tests

int doit(int, char **) {
	
	// create object vars that will manage our objects
	Test1(O1_var)   my_test1_object_o1_var;
	Test1(O2_var)   my_test1_object_o2_var;
	Test1(O3_var)   my_test1_object_o3_var;
	
	// lookup the test1::o1 object
	try {
		my_test1_object_o1_var = Test1(O1)::iluLookup (
                   CONST_CAST(iluCString,"Test1_Initial_Object"),
                   CONST_CAST(iluCString,"Test1-Server"));
		cout << "Test1(O1)::iluLookup (\"Test1_Initial_Object\", \"Test1-Server\") Success" << endl;
	}
	catch(...) {
		cout << "Test1(O1)::iluLookup (\"Test1_Initial_Object\", \"Test1-Server\") Exception" << endl;
		return -1;
	}
	
	// create a union and controlling var for it 
	Test1(U)*       p_my_test1_union_u = new Test1(U);	
	Test1(U_var)    my_test1_union_u_var(p_my_test1_union_u);
	p_my_test1_union_u->_CORBA_Boolean_arm(ILUCPP_TRUE);
	
	// CSS is SEQUENCE OF ScS; ScS is SEQUENCE OF SC, SC is SHORT CHARACTER;
	Test1(CSS)* p_my_test1_string_sequence_css = new Test1(CSS)(2); // maximum 2 long
	p_my_test1_string_sequence_css->length(2);  // make it 2 long
	Test1(CSS_var) my_test1_string_sequence_css_var(p_my_test1_string_sequence_css);
	my_test1_string_sequence_css_var[(CORBA(ULong)) 0] = CORBA(string_dup)("hello world");
	my_test1_string_sequence_css_var[(CORBA(ULong)) 1] = CORBA(string_dup)("hello mars");
		
	try {
		my_test1_union_u_var = my_test1_object_o1_var->U_CSS_to_U(my_test1_union_u_var, 
			my_test1_string_sequence_css_var);
		show((Test1(U)&)my_test1_union_u_var, 0, "my_test1_object_o1_var->>U_CSS_to_U, my_test1_union_u ");
	}	
	catch (const Test1(E1)& the_exception) {
		show(the_exception, 0, "my_test1_object_o1_var->U_CSS_to_U ");		
	}
	catch (const Test1(E2)& the_exception) {
		show(the_exception, 0, "my_test1_object_o1_var->U_CSS_to_U ");		
	}
	catch (const CORBA(SystemException)& the_exception) {
		show(the_exception, 0, "my_test1_object_o1_var->U_CSS_to_U ");		
		return TEST1_U_CSS_to_U_PROBLEM;
	}
	catch (...) {
		cout << "my_test1_object_o1_var->U_CSS_to_U got some sort of exception" << endl;
		return TEST1_U_CSS_to_U_PROBLEM;
	}

	// optional RECORD a: A1, css: CSS, i: I END
	// where A1 = ARRAY OF 3 ScS
	Test1(RO) optionalrecord_ro;
	
	try {
		optionalrecord_ro = my_test1_object_o1_var->f_CSS_to_RO(my_test1_string_sequence_css_var);
		show(optionalrecord_ro, 0, "my_test1_object_o1_var->f_CSS_to_RO ");		
	}	
	catch (const Test1(E1)& the_exception) {
		show(the_exception, 0, "my_test1_object_o1_var->f_CSS_to_RO ");		
	}
	catch (const CORBA(SystemException)& the_exception) {
		show(the_exception, 0, "my_test1_object_o1_var->f_CSS_to_RO ");		
		return TEST1_f_CSS_to_RO_PROBLEM;
	}
	catch (...) {
		cout << "my_test1_object_o1_var->f_CSS_to_RO got some sort of exception" << endl;
		return TEST1_f_CSS_to_RO_PROBLEM;
	}
	
	
	float f_somefloat_f;
	
	if (optionalrecord_ro) {
		try {
			f_somefloat_f = my_test1_object_o1_var->R_ScS_to_F(*optionalrecord_ro, "a shortchar seq");
			show(f_somefloat_f, 0, "my_test1_object_o1_var->R_ScS_to_F ");		
		}	
		catch (const CORBA(SystemException)& the_exception) {
			show(the_exception, 0, "my_test1_object_o1_var->R_ScS_to_F ");		
			return TEST1_R_ScS_to_F_PROBLEM;
		}
		catch (...) {
			cout << "my_test1_object_o1_var->R_ScS_to_F got some sort of exception" << endl;
			return TEST1_R_ScS_to_F_PROBLEM;
		}
	}
	
	delete optionalrecord_ro;
	optionalrecord_ro = NULL;
	
	// async call
	try {
		my_test1_object_o1_var->a_RO(optionalrecord_ro);
		cout << "my_test1_object_o1_var->a_RO call successful" << endl;
	}	
	catch (const CORBA(SystemException)& the_exception) {
		show(the_exception, 0, "my_test1_object_o1_var->a_RO ");		
		return TEST1_a_RO_PROBLEM;
	}
	catch (...) {
		cout << "my_test1_object_o1_var->a_RO got some sort of exception" << endl;
		return TEST1_a_RO_PROBLEM;
	}
	
	
	
	try {
		my_test1_object_o2_var = my_test1_object_o1_var->get_O2();
		show((Test1(O2)*)my_test1_object_o2_var, 0, "my_test1_object_o1_var->get_O2 ");		
	}	
	catch (const Test1(CantCreate)& the_exception) {
		show(the_exception, 0, "my_test1_object_o1_var->get_O2 ");		
		return TEST1_get_O2_PROBLEM;
	}
	catch (const CORBA(SystemException)& the_exception) {
		show(the_exception, 0, "my_test1_object_o1_var->get_O2 ");		
		return TEST1_get_O2_PROBLEM;
	}
	catch (...) {
		cout << "my_test1_object_o1_var->get_O2 got some sort of exception" << endl;
		return TEST1_get_O2_PROBLEM;
	}
	
	
	// Test1(A0) A0 = ARRAY OF 8 BYTE
    Test1(A0)      byte_array_a = {1, 2, 0, 3, 4, 0, 5, 6};
	
	// CSS is SEQUENCE OF ScS; ScS is SEQUENCE OF SC, SC is SHORT CHARACTER;
	Test1(CSS_var) my_test1_string_sequence_css2_var;
	
	try {
		my_test1_string_sequence_css2_var = my_test1_object_o2_var->OO_A0_to_CSS(my_test1_object_o1_var, byte_array_a);
	}	
	catch (const Test1(E2)& the_exception) {
		show(the_exception, 0, "my_test1_object_o2_var->OO_A0_to_CSS ");		
	}
	catch (const CORBA(SystemException)& the_exception) {
		show(the_exception, 0, "my_test1_object_o2_var->OO_A0_to_CSS ");		
		return TEST1_OO_A0_to_CSS_PROBLEM;
	}
	catch (...) {
		cout << "my_test1_object_o2_var->OO_A0_to_CSS got some sort of exception" << endl;
		return TEST1_OO_A0_to_CSS_PROBLEM;
	}
	
	
    Test1(A0_var)	byte_array_ap_var;
    Test1(I)		integer_i = 47;
	Test1(R)		record_r;
    Test1(A1)		char_string_array_a1;	// ARRAY OF 3 ScS

    record_r.a[(CORBA(ULong)) 0] = CORBA(string_dup)("this is");
    record_r.a[(CORBA(ULong)) 1] = CORBA(string_dup)("data");
    record_r.a[(CORBA(ULong)) 2] = CORBA(string_dup)("initialization");
	
    record_r.i = 12;
    char_string_array_a1[0] = CORBA(string_dup)("but this");
    char_string_array_a1[1] = CORBA(string_dup)("is");
    char_string_array_a1[2] = CORBA(string_dup)("fun");
	
	try {
		byte_array_ap_var = my_test1_object_o2_var->R_I_A1_to_I_A0(record_r, integer_i, char_string_array_a1);
		show(byte_array_ap_var, 0, "my_test1_object_o2_var->R_I_A1_to_I_A0 ");		
	}	
	catch (const CORBA(SystemException)& the_exception) {
		show(the_exception, 0, "my_test1_object_o2_var->R_I_A1_to_I_A0 ");		
		return TEST1_R_I_A1_to_I_A0_PROBLEM;
	}
	catch (...) {
		cout << "my_test1_object_o2_var->R_I_A1_to_I_A0 got some sort of exception" << endl;
		return TEST1_R_I_A1_to_I_A0_PROBLEM;
	}
	
	CORBA(Boolean) b_got_03 = ILUCPP_FALSE;	
	try {
		my_test1_object_o3_var = my_test1_object_o1_var->get_O3(ILUCPP_FALSE);
		show((Test1(O3)*)my_test1_object_o3_var, 0, " my_test1_object_o1_var->get_O3(ILUCPP_FALSE) ");	
		b_got_03 = ILUCPP_TRUE;	
	}	
	catch (const Test1(CantCreate)& the_exception) {
		show(the_exception, 0, " my_test1_object_o1_var->get_O3(ILUCPP_FALSE) ");		
	}
	catch (const CORBA(SystemException)& the_exception) {
		show(the_exception, 0, " my_test1_object_o1_var->get_O3(ILUCPP_FALSE) ");		
		return TEST1_get_O3_FALSE_PROBLEM;
	}
	catch (...) {
		cout << " my_test1_object_o1_var->get_O3(ILUCPP_FALSE) got some sort of exception" << endl;
		return TEST1_get_O3_FALSE_PROBLEM;
	}
	
	
	if (b_got_03) {
		
		if (my_test1_object_o3_var->iluGetClassRecord() != ((iluClass) ilu_FindClassFromName(CONST_CAST(iluCString,"Test1.O3")))) {
			cout << "Unexpected instance of class " << my_test1_object_o3_var->iluClassName() << " received!" << endl;
		} 
		else {
			
			Test1(RS)*		p_record_sequence_rs = new Test1(RS);
			Test1(RS_var)	record_sequence_rs_var(p_record_sequence_rs);
			Test1(IS_var)	integer_sequence_i2_var;
			Test1(R_var)	record_r_var;
			
			try {
				integer_sequence_i2_var = my_test1_object_o3_var->RS_R_to_R_IS(record_sequence_rs_var, record_r_var);
			}	
			catch (const CORBA(SystemException)& the_exception) {
				show(the_exception, 0, "my_test1_object_o3_var->RS_R_to_R_IS ");		
			}
			catch (...) {
				cout << "my_test1_object_o3_var->RS_R_to_R_IS got some sort of exception" << endl;
			}
			
			
			try {
				my_test1_object_o3_var->O1_U_to_U(my_test1_object_o1_var, my_test1_union_u_var);
				show((Test1(U)&)my_test1_union_u_var, 0, "O1_U_to_U ");
			}	
			catch (const Test1(E2)& the_exception) {
				show(the_exception, 0, "my_test1_object_o3_var->O1_U_to_U ");		
			}
			catch (const CORBA(SystemException)& the_exception) {
				show(the_exception, 0, "my_test1_object_o3_var->O1_U_to_U ");		
			}
			catch (...) {
				cout << "my_test1_object_o3_var->O1_U_to_U got some sort of exception" << endl;
			}
		}
	}
	
	// this next call should return an instance of Test3.O
	b_got_03 = ILUCPP_FALSE;	
	try {
		my_test1_object_o3_var = my_test1_object_o1_var->get_O3(ILUCPP_TRUE);
		show((Test1(O3)*)my_test1_object_o3_var, 0, "my_test1_object_o1_var->get_O3(ILUCPP_TRUE) ");	
		b_got_03 = ILUCPP_TRUE;	
	}	
	catch (const Test1(CantCreate)& the_exception) {
		show(the_exception, 0, "my_test1_object_o1_var->get_O3(ILUCPP_TRUE) ");		
	}
	catch (const CORBA(SystemException)& the_exception) {
		show(the_exception, 0, "my_test1_object_o1_var->get_O3(ILUCPP_TRUE) ");		
		return TEST1_get_O3_TRUE_PROBLEM;
	}
	catch (...) {
		cout << "my_test1_object_o1_var->get_O3(ILUCPP_TRUE) got some sort of exception" << endl;
		return TEST1_get_O3_TRUE_PROBLEM;
	}
				
	if (b_got_03) {
		
		Test1(RS)*		p_record_sequence_rs = new Test1(RS);
		Test1(RS_var)	record_sequence_rs_var(p_record_sequence_rs);
		Test1(IS_var)	integer_sequence_i2_var;
		Test1(R_var)	record_r_var;
		
		try {
			integer_sequence_i2_var = my_test1_object_o3_var->RS_R_to_R_IS(record_sequence_rs_var, record_r_var);
		}	
		catch (const CORBA(SystemException)& the_exception) {
			show(the_exception, 0, "my_test1_object_o3_var->RS_R_to_R_IS ");		
		}
		catch (...) {
			cout << "my_test1_object_o3_var->RS_R_to_R_IS got some sort of exception" << endl;
		}
		
		
		try {
			my_test1_object_o3_var->O1_U_to_U(my_test1_object_o1_var, my_test1_union_u_var);
			show((Test1(U)&)my_test1_union_u_var, 0, "my_test1_object_o3_var->O1_U_to_U ");
		}	
		catch (const Test1(E2)& the_exception) {
			show(the_exception, 0, "my_test1_object_o3_var->O1_U_to_U ");		
		}
		catch (const CORBA(SystemException)& the_exception) {
			show(the_exception, 0, "my_test1_object_o3_var->O1_U_to_U ");		
		}
		catch (...) {
			cout << "my_test1_object_o3_var->O1_U_to_U got some sort of exception" << endl;
		}
		
		if (my_test1_object_o3_var->iluGetClassRecord() == ((iluClass) ilu_FindClassFromName(CONST_CAST(iluCString,"Test3.O")))) {
			Test1(U_var)       u2_var;
			Test3(O_ptr) my_test3_object_o_ptr = Test3(O)::_narrow(my_test1_object_o3_var); 

			if (!my_test3_object_o_ptr) {
				cout << "Test3(O)::_narrow(my_test1_object_o3_var) FAILURE!" << endl;
			}
			else {
				my_test3_object_o_ptr->iluIncrementReferenceCount(); /* inc count since we're making another a var */
				
				Test3(O_var) my_test3_object_o_var(my_test3_object_o_ptr);
				try {
					u2_var = my_test3_object_o_var->I_to_Test1U(397);
					show((Test1(U)&)u2_var, 0, "my_test3_object_o_var->I_to_Test1U ");
				}	
				catch (const Test1(E1)& the_exception) {
					show(the_exception, 0, "my_test3_object_o_var->I_to_Test1U ");		
				}
				catch (const CORBA(SystemException)& the_exception) {
					show(the_exception, 0, "my_test3_object_o_var->I_to_Test1U ");		
				}
				catch (...) {
					cout << "my_test3_object_o_var->I_to_Test1U got some sort of exception" << endl;
				}
			}
		}		
	}				
	
	// this next call should return an instance of Test1.O4 

	try {
		my_test1_object_o3_var = my_test1_object_o1_var->get_O3(ILUCPP_FALSE);
		show((Test1(O3)*)my_test1_object_o3_var, 0, "my_test1_object_o1_var->get_O3(ILUCPP_FALSE) ");	
		b_got_03 = ILUCPP_TRUE;	
	}	
	catch (const Test1(CantCreate)& the_exception) {
		show(the_exception, 0, "my_test1_object_o1_var->get_O3(ILUCPP_FALSE) ");		
		return TEST1_get_O3_FALSE_PROBLEM;
	}
	catch (const CORBA(SystemException)& the_exception) {
		show(the_exception, 0, "my_test1_object_o1_var->get_O3(ILUCPP_FALSE) ");		
		return TEST1_get_O3_FALSE_PROBLEM;
	}
	catch (...) {
		cout << "my_test1_object_o1_var->get_O3(ILUCPP_FALSE) got some sort of exception" << endl;
		return TEST1_get_O3_FALSE_PROBLEM;
	}
	
	if (my_test1_object_o3_var->iluGetClassRecord() == ((iluClass) ilu_FindClassFromName(CONST_CAST(iluCString,"Test1.O4")))) {
		iluReal d_double1 = 12345.6789;
		iluReal d_double2;
		
		Test1(O4_ptr) my_test1_object_o4_ptr = Test1(O4)::_narrow(my_test1_object_o3_var); 
		
		if (!my_test1_object_o4_ptr) {
			cout << "Test1(4)::_narrow(my_test1_object_o3_var) FAILURE!" << endl;
		}
		
		my_test1_object_o4_ptr->iluIncrementReferenceCount(); /* inc count since we're making another a var */

		Test1(O4_var) my_test1_object_o4_var(my_test1_object_o4_ptr);
		
		try {
			d_double2 = my_test1_object_o4_var->R_to_R(d_double1);
			show(d_double2, 0, "my_test1_object_o4_var->R_to_R ");	
		}	
		catch (const CORBA(SystemException)& the_exception) {
			show(the_exception, 0, "my_test1_object_o4_var->R_to_R ");		
			return TEST1_R_to_R_PROBLEM;
		}
		catch (...) {
			cout << "my_test1_object_o4_var->R_to_R got some sort of exception" << endl;
			return TEST1_R_to_R_PROBLEM;
		}
	}
	
	// whew!
	return 0;
}


///////////////////////////////////////////////////////////////////////
// main

int main(int ac, char ** av) {
	
	int i_doit_result;
	
#ifdef SECURE_TRANSPORT
	
	// we need to call these initializers to make sure any incoming secured SBH is parsed properly
	ilugssmech_nil_initialize();
	ilugssns_rfc822_initialize();
#endif /* SECURITY */

	try {
		// set up the runtime for non threaded operation
		iluCppRuntime::iluInitialize(ILUCPP_FALSE);
	}
	catch (const CORBA(SystemException)& the_exception) {
		show (the_exception, 0, "iluCppRuntime::iluInitialize ");		
		return TEST1_RUNTIME_INIT_PROBLEM;
	}
	catch (...) {
		cout << "iluCppRuntime::iluInitialize got some sort of exception" << endl;
		return TEST1_RUNTIME_INIT_PROBLEM;
	}
	
	i_doit_result = doit(ac, av);
	
	if (i_doit_result == 0)
		cout << "SUCCESS" << endl;
	else
		cout << "FAILURE! doit result = " << i_doit_result << endl;
	
	return i_doit_result;
}


///////////////////////////////////////////////////////////////////////
// EOF
///////////////////////////////////////////////////////////////////////
