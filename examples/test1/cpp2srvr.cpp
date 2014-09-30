/** $Id: cpp2srvr.cpp,v 1.13 1999/08/03 01:52:26 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 11:28 am PDT */

/* ILU's CORBA 2.0 C++ version of Test1 server side */

///////////////////////////////////////////////////////////////////////
/* Abstract class hierarchy (neglecting aliasing)

  
    Test1(O1)        Test1(O3)       Test2(P)        Test1(O2)
    | |           | | \ \ \         /
    | +-----+     | |  \ \ --------/-------
    |       |     | |   \ \       /        \
    | +-----------+ |    \ --- \ /          \
    | |     |       |     \     X            \
    | |     +-----+ |      \   / \            \
    | |           | |       \ /   \            \
    Test1(O5)    Test1(O6) Test3(O)  Test1(O4)  Test1(P)
    
      Notes: 
      
        This example does not use implementation inheritance.
        
          No implementation of Test1(O5) and Test1(O6) is provided
          since they are actually commented out in the Test1.isl file
          
*/


///////////////////////////////////////////////////////////////////////
// #includes

#include <iostream.h>

#include "Test1-cpptrue.hpp"
#include "Test2-cpptrue.hpp"
#include "Test3-cpptrue.hpp"


#ifdef SECURE_TRANSPORT

#include <gssapi.h>
#include <ilugssmech_nil.h>
#include <ilugssns_rfc822.h>

iluPassport* SetupNILCredentials (char* pc_name) {
    gss_cred_id_t   credentials;
    iluIdentityInfo indent_info;
    iluPassport* p_passport;
    
    try {
        credentials = iluGSS::iluAcquireGSSCredForName(pc_name,
        GSS_C_INDEFINITE, ilugssmech_nil_OID, ILUCPP_TRUE);
        }
    catch (...) {
        cout << "Can't acquire credentials for name " << pc_name << endl;
        return NULL;
    }
    
    try {
        indent_info = iluGSS::iluAcquireGSSIdentity(credentials);
        }
    catch (...) {
        cout << "Can't create GSS identity for name " << pc_name << endl;
        return NULL;
    }
    
    p_passport = new iluPassport(indent_info);
    
    return p_passport;
}

#endif // SECURE_TRANSPORT


///////////////////////////////////////////////////////////////////////
// implementation class for Test1(O1) objects

class Test1_O1_impl : public virtual Test1(O1) {
    
public:
    // constructor
    Test1_O1_impl(char* pc_instance_handle = NULL, 
        iluServer& r_an_ilu_server = iluServer::iluGetDefaultServer(), 
        CORBA(Boolean) b_within_object_table = ILUCPP_FALSE) : 
    iluObject(iluGetILUClassRecord(), pc_instance_handle, r_an_ilu_server, b_within_object_table) {}
    
    virtual ~Test1_O1_impl() {
        iluDeactivate();
    }
    
    virtual Test1(U) * U_CSS_to_U (const Test1(U)& r_u, const Test1(CSS)&);
    
    virtual Test1(RO) f_CSS_to_RO (const Test1(CSS)&);
    
    virtual CORBA(Float) R_ScS_to_F (const Test1(R)&, Test1(const_ScS));
    
    virtual void a_RO (Test1(const_RO));
    
    virtual Test1(O2_ptr) get_O2 ();
    
    virtual Test1(O3_ptr) get_O3 (CORBA(Boolean) b_subclass);     
};


///////////////////////////////////////////////////////////////////////
// implementation class for Test1(O2) objects

class Test1_O2_impl : public virtual Test1(O2) {
    
public:
    // constructor
    Test1_O2_impl(char* pc_instance_handle = NULL, 
        iluServer& r_an_ilu_server = iluServer::iluGetDefaultServer(),
        CORBA(Boolean) b_within_object_table = ILUCPP_FALSE) : 
    iluObject(iluGetILUClassRecord(), pc_instance_handle, r_an_ilu_server, b_within_object_table) {}
    
    virtual ~Test1_O2_impl() {
        iluDeactivate();
    }
    
    virtual Test1(CSS)* OO_A0_to_CSS (Test1(OO) p_optional_o1, const Test1(A0));

    virtual Test1(A0_slice)* R_I_A1_to_I_A0 (const Test1(R)&, Test1(I)& r_i, const Test1(A1));
};



///////////////////////////////////////////////////////////////////////
// implementation class for Test1(O3) objects

class Test1_O3_impl : public virtual Test1(O3) {
    
public:
    // constructor
    Test1_O3_impl(char* pc_instance_handle = NULL,
        iluServer& r_an_ilu_server = iluServer::iluGetDefaultServer(),
        CORBA(Boolean) b_within_object_table = ILUCPP_FALSE) : 
    iluObject(iluGetILUClassRecord(), pc_instance_handle, r_an_ilu_server, b_within_object_table) {}
    
    virtual ~Test1_O3_impl() {
        iluDeactivate();
    }

    virtual Test1(IS)* RS_R_to_R_IS (const Test1(RS)&, Test1(R)*& r_p_record);

    virtual void O1_U_to_U (Test1(O1_ptr) p_o1, Test1(U)& r_union);
            
    virtual Test1(I) BS_to_I (const Test1(BS)& r_byte_sequence);
};


///////////////////////////////////////////////////////////////////////
// implementation class for Test1(P) objects

class Test1_P_impl : public virtual Test1(P) {
    
public:
    // constructor
    Test1_P_impl(char* pc_instance_handle = NULL,
        iluServer& r_an_ilu_server = iluServer::iluGetDefaultServer(),
        CORBA(Boolean) b_within_object_table = ILUCPP_FALSE) : 
    iluObject(iluGetILUClassRecord(), pc_instance_handle, r_an_ilu_server, b_within_object_table) {}
    
    virtual ~Test1_P_impl() {
        iluDeactivate();
    }
    
    // Test1(O3) methods
    virtual Test1(IS)* RS_R_to_R_IS (const Test1(RS)&, Test1(R)*& r_p_record);

    virtual void O1_U_to_U (Test1(O1_ptr) p_o1, Test1(U)& r_union);
            
    virtual Test1(I) BS_to_I (const Test1(BS)& r_byte_sequence);
    
    // direct superclass methods
    virtual Test1(IS)* m2 (CORBA(Long) l_j);

};


///////////////////////////////////////////////////////////////////////
// implementation class for Test1(O4) objects

class Test1_O4_impl : public virtual Test1(O4) {
    
public:
    // constructor
    Test1_O4_impl(char* pc_instance_handle = NULL,
        iluServer& r_an_ilu_server = iluServer::iluGetDefaultServer(),
        CORBA(Boolean) b_within_object_table = ILUCPP_FALSE) : 
    iluObject(iluGetILUClassRecord(), pc_instance_handle, r_an_ilu_server, b_within_object_table) {}
    
    virtual ~Test1_O4_impl() {
        iluDeactivate();
    }
    
    // Test1(O3) methods
    virtual Test1(IS)* RS_R_to_R_IS (const Test1(RS)&, Test1(R)*& r_p_record);

    virtual void O1_U_to_U (Test1(O1_ptr) p_o1, Test1(U)& r_union);
            
    virtual Test1(I) BS_to_I (const Test1(BS)& r_byte_sequence);
    
    // direct superclass methods
    virtual CORBA(Double) R_to_R (CORBA(Double) d);
};


///////////////////////////////////////////////////////////////////////
// implementation class for Test3(O) objects

class Test3_O_impl : public virtual Test3(O) {
    
public:
    // constructor
    Test3_O_impl(char* pc_instance_handle = NULL,
        iluServer& r_an_ilu_server = iluServer::iluGetDefaultServer(),
        CORBA(Boolean) b_within_object_table = ILUCPP_FALSE) : 
    iluObject(iluGetILUClassRecord(), pc_instance_handle, r_an_ilu_server, b_within_object_table) {}
    
    virtual ~Test3_O_impl() {
        iluDeactivate();
    }
    
    // Test1(O3) methods
    virtual Test1(IS)* RS_R_to_R_IS (const Test1(RS)&, Test1(R)*& r_p_record);

    virtual void O1_U_to_U (Test1(O1_ptr) p_o1, Test1(U)& r_union);
            
    virtual Test1(I) BS_to_I (const Test1(BS)& r_byte_sequence);
    
    // Test2(P) methods
    virtual CORBA(Long) Test3_O_impl::SR_to_I (CORBA(Float) a_float);
    
    // direct superclass methods
    virtual Test1(U)* I_to_Test1U (CORBA(Long) i);
};


///////////////////////////////////////////////////////////////////////
// Usage string

char g_c_usage[] = 
"Usage:  cpp2server  [help | -help | ? | -?]  |  [-mt] [-cred] [security] [-p protocol_info] [-t transport_info+ ]\n\
\thelp | -help | ? | -? : displays usage only \n\
\t-mt - if present, sets ILU to multithreaded operation\n\
\t-cred - if present, sets ILU to use credentials\n\
\t-secure - if present, sets ILU to use security\n\
\t-p - if present, sets protocol to use\n\
\t-t - if present, sets transports to use\n";


///////////////////////////////////////////////////////////////////////
// main

int main (int ac, char **av) {
    
    Test1(O1_ptr) p_true_Test1_O1;          // points to true object
    Test1(O1_ptr) p2_true_Test1_O1;         // another pointer to true object
    CORBA(Boolean) b_run_threaded = ILUCPP_FALSE;
    CORBA(Boolean) b_credentials = ILUCPP_FALSE;
    CORBA(Boolean) b_security = ILUCPP_FALSE;
    char* pc_protocol_info = NULL;
    char** ppc_transport_info;
    char* ppc_cmdline_transport_info[8] = {NULL};
    int i_arg_index = 1;
    iluPassport* p_passport = NULL;

#if defined(SECURE_TRANSPORT)
    char* ppc_secure_transport_info[5] = {
            "sunrpcrm",
            "security_1_Xerox.ILU.GSS.NIL",
            "sunrpcrm", "tcp_0_0", ILU_NIL};
#else
        char** ppc_secure_transport_info = NULL;
#endif /* security filter available */
        
    
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
                ppc_cmdline_transport_info[i_transport_index++] = av[i_arg_index++];
            ppc_cmdline_transport_info[i_transport_index] = NULL;
            continue;
        } 
        
        // check whether to run threaded
        if (strcmp(av[i_arg_index], "-mt") == 0) {
            b_run_threaded = ILUCPP_TRUE; i_arg_index++;
            continue;
        } 

        // check whether to use credentials
        if (strcmp(av[i_arg_index], "-cred") == 0) {
            b_credentials = ILUCPP_TRUE; i_arg_index++;
            continue;
        } 

        // check whether to use security
        if (strcmp(av[i_arg_index], "-secure") == 0) {
            b_security = ILUCPP_TRUE; i_arg_index++;
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

    if (b_security) {
        if (ppc_secure_transport_info == NULL) {
            cout << "Security support not configured into ILU!" << endl;
            return 1;
        }
        else 
            ppc_transport_info = ppc_secure_transport_info;
    }
    else {
        if (ppc_cmdline_transport_info[0] != NULL)
            ppc_transport_info = ppc_cmdline_transport_info;
        else ppc_transport_info = iluServer::iluGetDefaultTransport();
    }

    // show how we're running
    cout << "Running " << (b_run_threaded ? "threaded" : "single threaded") << endl;
    cout << "Protocol  " << (pc_protocol_info ? pc_protocol_info : iluServer::iluGetDefaultProtocol()) << endl;
    cout << "Transport ";
    { iluTransportInfo ppc_walker = ppc_transport_info;
    int i_transport_index = 0;
    while (ppc_walker[i_transport_index] != NULL) {
        cout << ppc_walker[i_transport_index] << " ";
        i_transport_index++;
    }
    cout << endl;
    }

    if (b_credentials) {
#if defined(SECURE_TRANSPORT)
        cout << "setting up NIL GSS credentials..." << endl;
        ilugssmech_nil_initialize();
        ilugssns_rfc822_initialize();
        p_passport = SetupNILCredentials("Xerox.ILU.GSS.RFC822:server@test1.examples.parc.xerox.com");
        if (!p_passport) {
            cout << "Can't initialize NIL credentials" << endl;
            return 1;
        }
#else
        cout << "Security support not configured into ILU!" << endl;
        return 1;
#endif              // security 
    }
    

    // create our server
    iluServer server (CONST_CAST(iluCString,"Test1-Server"), NULL, pc_protocol_info, ppc_transport_info, p_passport);
    
    // make it our default server
    iluServer::iluSetDefaultServer(server);

    // create a true foo object
    p_true_Test1_O1 = new Test1_O1_impl(CONST_CAST(iluCString, "Test1_Initial_Object"), server);
    
    if (p_true_Test1_O1 == NULL) {
        cout << "Error - couldn't create Test1_Initial_Object object" << endl;
        return 1;
    }
    
    // publish the true foo object
    if (!p_true_Test1_O1->iluPublish()) {
        cout << "Error - couldn't publish Test1_Initial_Object object" << endl;
        return 1;
    }
    else {
        iluCString pc_object_string = p_true_Test1_O1->iluObjectToString();
        cout << "Exported " << pc_object_string << endl;
        iluCppRuntime::iluFree(pc_object_string);
    }

    // test the publish and lookup a bit 
    try {
        p2_true_Test1_O1 = Test1(O1)::iluLookup (
           CONST_CAST(iluCString,"Test1_Initial_Object"),
           CONST_CAST(iluCString, "Test1-Server"));
        cout << "Test1(O1)::iluLookup (\"Test1_Initial_Object\", \"Test1-Server\") Success" << endl;
        p2_true_Test1_O1->_release();
    }
    catch(...) {
        cout << "Test1(O1)::iluLookup (\"Test1_Initial_Object\", \"Test1-Server\") Exception" << endl;
        return -1;
    }
    
    // run the server
    server.iluRun();
    
    return 0;
}



///////////////////// Test1_O1_impl methods /////////////////////


Test1(U)* Test1_O1_impl::U_CSS_to_U (const Test1(U)& r_u, const Test1(CSS)& ) {
    Test1(U) * p_result = new Test1(U);
    iluIdentityInfo identity_info;
    iluPassport* p_passport;
    
    cout << "Test1.O1.U-CSS-to-U" << endl;
    
    // get the passport for this call
    p_passport = iluPassport::iluGetCallerPassport();
    
    identity_info = p_passport->iluFindIdentity(ilu_ConnectionIdentity);
    if (identity_info) {
        // show the identity
        cout << " [caller is connection:\"" << ((iluCString)(identity_info->ii_info)) << "\"" << endl;
    }
    
#ifdef SUNRPC_PROTOCOL
    identity_info = p_passport->iluFindIdentity(ilu_SunRPCAuthUnixIdentity);
    if (identity_info) {
        // show the SunRPCAuthUnix identity
        cout << 
            ", sunrpc-authunix:(" << 
            ((unsigned int) ((ilu_SunRPCAuthUnixIdentityInfo) (identity_info->ii_info))->ii_UID) <<
            "," <<
            ((unsigned int) ((ilu_SunRPCAuthUnixIdentityInfo) (identity_info->ii_info))->ii_GID) <<
            ")@" <<
            ((ilu_SunRPCAuthUnixIdentityInfo)(identity_info->ii_info))->ii_hostname <<
            endl;
    }
#endif
    
#ifdef SECURE_TRANSPORT
    identity_info = p_passport->iluFindIdentity(ilu_GSSIdentity);
    if (identity_info) {
        gss_name_t name;
        CORBA(Boolean) b_localp;
        iluCString pc_string;
        
        try {
            iluGSS::iluDecodeGSSIdentity (identity_info, &name, 0, 0, &b_localp, 0);
            try {
                pc_string = iluGSS::iluGSSNameToString (name);
                cout << ", GSS:\"" << pc_string << "\"(" << (b_localp ? "local" : "remote") << ")" << endl;
                iluCppRuntime::iluFree(pc_string);
            }
            catch (...) {
                cout << "Error from iluGSS::iluGSSNameToString" << endl;
            }
        }
        catch (...) {
            cout << "Error from iluGSS::iluDecodeGSSIdentity" << endl;
        }
    }
#endif

    
    cout << "]" << endl;
    
    iluIncrementReferenceCount();
    p_result->_O1_arm(this);
    return p_result;
}


Test1(RO) Test1_O1_impl::f_CSS_to_RO (const Test1(CSS)&) {
    Test1(RO) p_ro = new Test1(R);
    
    p_ro->i = 9;

    p_ro->a[0]= CORBA(string_dup)("hello");
    p_ro->a[1]= CORBA(string_dup)("world");
    p_ro->a[2]= CORBA(string_dup)("!\n");

    cout << "Test1.O1.f-CSS-to-R0" << endl;
    return p_ro;
}


CORBA(Float) Test1_O1_impl::R_ScS_to_F (const Test1(R)&, Test1(const_ScS)) {
    cout << "Test1.O1.R-ScS-to-F returning " << 39.7 << endl;
    return ((CORBA(Float)) 39.7);
}


void Test1_O1_impl::a_RO (Test1(const_RO)) {
    cout << "Test1.O1.a-RO" << endl;
}



Test1(O2_ptr) Test1_O1_impl::get_O2 () {
    
    static Test1(O2_ptr) p_an_o2 = NULL;
    static iluServer* p_singleton_server = NULL;

    cout << "Test1.O1.get-O2" << endl;


    if (p_an_o2 == NULL) {

    // Note that Test1.O2 is a singleton class, of cinfo-type
    // "sunrpc_2_0x3458_3".  This means that it has to be exported via
    // a kernel server with a "sunrpc" port...
    iluCString  singleton_sunrpc_tinfo[] =
            {CONST_CAST(iluCString,"sunrpcrm"),
             CONST_CAST(iluCString, "tcp_0_0"), ILU_NIL};

    p_singleton_server = new iluServer(
         CONST_CAST(iluCString,"Test1-SunRPC-Server"), NULL, 
         CONST_CAST(iluCString, "sunrpc"), singleton_sunrpc_tinfo);

        if (p_singleton_server)
            p_an_o2 = new Test1_O2_impl(NULL, *p_singleton_server);

        if (p_an_o2 == NULL) {
            Test1(CantCreate) cant_create_exception;
            throw(cant_create_exception);
        }
    }
    // incref it so it hangs around after we pass it back
    p_an_o2->iluIncrementReferenceCount();
    return p_an_o2;
}



Test1(O3_ptr) Test1_O1_impl::get_O3 (CORBA(Boolean) b_subclass) {
    
    static Test1(O3_ptr) p_a_1_o3 = NULL;
    static Test1(O4_ptr) p_a_1_o4 = NULL;
    static Test3(O_ptr) p_a_3_o = NULL;
    static int i_one = 0;
    
    cout << "Test1.O1.get-O3" << endl;
    
    if (b_subclass) {
        if (!p_a_3_o) {
            cout << "making Test3.O..." << endl;
            p_a_3_o = new Test3_O_impl();
            if (!p_a_3_o) {
                Test1(CantCreate) cant_create_exception;
                throw(cant_create_exception);
            }
            
        }
        // incref it so it hangs around after we pass it back
        p_a_3_o->iluIncrementReferenceCount();
        cout << "returning Test3.O..." << endl;
        return p_a_3_o;
    }
    
    if (i_one == 0) {
        i_one = 1;
        if (!p_a_1_o3) {
            cout << "making O3..." << endl;
            p_a_1_o3 = new Test1_O3_impl();
            if (!p_a_1_o3) {
                Test1(CantCreate) cant_create_exception;
                throw(cant_create_exception);
            }
        }
        // incref it so it hangs around after we pass it back
        p_a_1_o3->iluIncrementReferenceCount();
        cout << "returning O3..." << endl;
        return p_a_1_o3;        
    }
    
    
    i_one = 0;
    if (!p_a_1_o4) {
        cout << "making O4..." << endl;
        p_a_1_o4 = new Test1_O4_impl();
        if (!p_a_1_o4) {
            Test1(CantCreate) cant_create_exception;
            throw(cant_create_exception);
        }
    }
    
    // incref it so it hangs around after we pass it back
    p_a_1_o4->iluIncrementReferenceCount();
    cout << "returning O4..." << endl;
    return p_a_1_o4;
}


///////////////////// Test1_O2_impl methods /////////////////////


Test1(CSS)* Test1_O2_impl::OO_A0_to_CSS (Test1(OO) p_optional_o1, const Test1(A0)) {
    
    cout << "Test1.o2.OO-A0-to-CSS" << endl;
    if (p_optional_o1 == NULL) {
        Test1(E2) e2exception(7);       
        throw(e2exception);
    }
    return new Test1(CSS);
}


Test1(A0_slice)* Test1_O2_impl::R_I_A1_to_I_A0 (const Test1(R)&, Test1(I)& r_i, const Test1(A1)) {
    
    Test1(A0_slice)* return_value = Test1(A0_alloc)();
    
    cout << "Test1.O2.R-I-A1-to-I-A0" << endl;
    
    for (r_i = 0; r_i < 8; r_i++)
        return_value[r_i] = 1 << (1 + ((6 + r_i) % 8));
    
    return return_value;
}


///////////////////// Test1_O3_impl methods /////////////////////


Test1(IS)* Test1_O3_impl::RS_R_to_R_IS (const Test1(RS)&, Test1(R)*& r_p_record) {

  Test1(IS)* p_int_sequence = new Test1(IS);
  r_p_record = new Test1(R);

  cout << "Test1.O3.RS-R-to-R-IS" << endl;

  r_p_record->i = 3;

  r_p_record->a[0] = CORBA(string_dup)("just");
  r_p_record->a[1] = CORBA(string_dup)("a");
  r_p_record->a[2] = CORBA(string_dup)("string");

  return p_int_sequence;
}


void Test1_O3_impl::O1_U_to_U (Test1(O1_ptr) p_o1, Test1(U)& r_union) {
  cout << "Test1.O3.O1-U-to-U" << endl;
  r_union._O1_arm(p_o1);
}


Test1(I) Test1_O3_impl::BS_to_I (const Test1(BS)& r_byte_sequence) {
  return r_byte_sequence.length() * r_byte_sequence.length();
}


///////////////////// Test1_P_impl methods /////////////////////


Test1(IS)* Test1_P_impl::RS_R_to_R_IS (const Test1(RS)&, Test1(R)*& r_p_record) {

  Test1(IS)* p_int_sequence = new Test1(IS);
  r_p_record = new Test1(R);

  cout << "Test1.P.RS-R-to-R-IS" << endl;

  r_p_record->i = 25179;

  r_p_record->a[0] = CORBA(string_dup)("from");
  r_p_record->a[1] = CORBA(string_dup)("P");
  r_p_record->a[2] = CORBA(string_dup)("string");

  return p_int_sequence;
}



void Test1_P_impl::O1_U_to_U (Test1(O1_ptr) p_o1, Test1(U)& r_union) {
  cout << "Test1.P.O1-U-to-U" << endl;
  r_union._O1_arm(p_o1);
}


Test1(I) Test1_P_impl::BS_to_I (const Test1(BS)& r_byte_sequence) {
  return r_byte_sequence.length();
}



Test1(IS)* Test1_P_impl::m2 (CORBA(Long) l_j) {
  Test1(IS)* p_int_sequence = new Test1(IS)(2);

  p_int_sequence[0] = l_j;
  p_int_sequence[1] = l_j * l_j;
  return p_int_sequence;
}


///////////////////// Test1_O4_impl methods /////////////////////

Test1(IS)* Test1_O4_impl::RS_R_to_R_IS (const Test1(RS)&, Test1(R)*& r_p_record) {

  Test1(IS)* p_int_sequence = new Test1(IS);
  r_p_record = new Test1(R);

  cout << "Test1.O4.RS-R-to-R-IS" << endl;

  r_p_record->i = 25179;

  r_p_record->a[0] = CORBA(string_dup)("from");
  r_p_record->a[1] = CORBA(string_dup)("O4");
  r_p_record->a[2] = CORBA(string_dup)("string");

  return p_int_sequence;
}


void Test1_O4_impl::O1_U_to_U (Test1(O1_ptr) p_o1, Test1(U)& r_union) {
  cout << "Test1.O4.O1-U-to-U" << endl;
  r_union._O1_arm(p_o1);
}

Test1(I) Test1_O4_impl::BS_to_I  (const Test1(BS)& r_byte_sequence) {
    
    cout << "Test1.O4.BS_to_I (" << r_byte_sequence.length() << ": ";
    for (unsigned int i = 0; i < r_byte_sequence.length(); i++) {
        cout << hex << r_byte_sequence[i] << " ";
    }
    cout << dec << ") => " << r_byte_sequence.length() << endl;
    return r_byte_sequence.length();
}

CORBA(Double) Test1_O4_impl::R_to_R (CORBA(Double) d) {
  CORBA(Double) d2 = 1020304.05060708;

  cout << "Test1.O4.R_to_R (" << d << ") => " << d2 << endl;
  return d2;
}


///////////////////// Test3_O_impl methods /////////////////////


Test1(IS)* Test3_O_impl::RS_R_to_R_IS (const Test1(RS)&, Test1(R)*& r_p_record) {

  Test1(IS)* p_int_sequence = new Test1(IS);
  r_p_record = new Test1(R);

  cout << "Test3.O.RS-R-to-R-IS" << endl;

  r_p_record->i = 3;

  r_p_record->a[0] = CORBA(string_dup)("just");
  r_p_record->a[1] = CORBA(string_dup)("a");
  r_p_record->a[2] = CORBA(string_dup)("string");

  return p_int_sequence;
}



void Test3_O_impl::O1_U_to_U (Test1(O1_ptr) p_o1, Test1(U)& r_union) {
  cout << "Test1.O3.O1-U-to-U(0x" << hex << ((unsigned long)p_o1) << ", {" << r_union._d() << "})" << dec << endl;
  r_union._O1_arm(p_o1);
}


Test1(I) Test3_O_impl::BS_to_I (const Test1(BS)& r_byte_sequence) {
  return r_byte_sequence.length() * r_byte_sequence.length();
}


CORBA(Long) Test3_O_impl::SR_to_I (CORBA(Float) a_float) {
  cout << "Test3.O.SR-to-I(" << a_float << ")" << endl;
  return ((CORBA(Long)) a_float);
}


Test1(U)* Test3_O_impl::I_to_Test1U (CORBA(Long) i) {
  Test1(U)* p_u;

  cout << "Test3.O.I-to-Test1U(" << i << ")" << endl;
  p_u = new Test1(U);
  p_u->_CORBA_Boolean_arm(ILUCPP_TRUE);
  return p_u;
}



///////////////////////////////////////////////////////////////////////
//  End of file
///////////////////////////////////////////////////////////////////////

