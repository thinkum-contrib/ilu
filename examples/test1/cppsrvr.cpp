/** $Id: cppsrvr.cpp,v 1.19 1999/08/03 01:52:12 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 11:29 am PDT */

#include <stdio.h>

#include "Test1.hh"
#include "Test3.hh"

iluServer * sunrpcServer;

class Test1_T_O1_impl : public virtual Test1_T_O1 {
public:
  Test1_T_O1_impl(char *instanceHandle, iluServer *server);

  virtual char * ILUGetInstanceHandle();
  virtual iluServer * ILUGetServer();

  virtual Test1_T_U * U_CSS_to_U (Test1Status *_status, Test1_T_U * u, Test1_T_CSS css);
  virtual Test1_T_RO f_CSS_to_RO (Test1Status *_status, Test1_T_CSS css);
  virtual ilu_ShortReal R_ScS_to_F (Test1Status *_status, Test1_T_R * r, Test1_T_ScS s);
  virtual void a_RO (Test1Status *_status, Test1_T_RO ro);
  virtual class Test1_T_O2 * get_O2 (Test1Status *_status);
  virtual class Test1_T_O3 * get_O3 (Test1Status *_status, ilu_Boolean subclass);

private:
  char *ourInstanceHandle;
  iluServer *ourServer;
};


class Test1_T_O2_impl : public virtual Test1_T_O2 {
public:
  virtual iluServer * ILUGetServer();
  virtual Test1_T_CSS OO_A0_to_CSS (Test1Status *_status, Test1_T_OO o, Test1_T_A0 a);
  virtual Test1_T_A0 * R_I_A1_to_I_A0 (Test1Status *_status, Test1_T_R * r, Test1_T_I * i, Test1_T_A1 a);
};


class Test1_T_O3_impl : public virtual Test1_T_O3 {
public:
  virtual Test1_T_IS RS_R_to_R_IS (Test1Status *_status, Test1_T_RS r, Test1_T_R * r2);
  virtual void O1_U_to_U (Test1Status *_status, Test1_T_O1 * o, Test1_T_U * u);
  virtual Test1_T_I BS_to_I (Test1Status *_status, Test1_T_BS b);
};


class Test1_T_P_impl : public virtual Test1_T_P {
public:
  virtual Test1_T_IS RS_R_to_R_IS (Test1Status *_status, Test1_T_RS r, Test1_T_R * r2);
  virtual void O1_U_to_U (Test1Status *_status, Test1_T_O1 * o, Test1_T_U * u);
  virtual Test1_T_I BS_to_I (Test1Status *_status, Test1_T_BS b);
  virtual Test1_T_IS m2 (Test1Status *_status, ilu_Integer j);
};


class Test1_T_O4_impl : public virtual Test1_T_O4 {
public:
  virtual Test1_T_IS RS_R_to_R_IS (Test1Status *_status, Test1_T_RS r, Test1_T_R * r2);
  virtual void O1_U_to_U (Test1Status *_status, Test1_T_O1 * o, Test1_T_U * u);
  virtual Test1_T_I BS_to_I (Test1Status *_status, Test1_T_BS b);
  virtual ilu_Real R_to_R (Test1Status *_status, ilu_Real r);
};


class Test3_T_O_impl : public virtual Test3_T_O {
public:
  virtual Test1_T_IS RS_R_to_R_IS (Test1Status *_status, Test1_T_RS r, Test1_T_R * r2);
  virtual void O1_U_to_U (Test1Status *_status, Test1_T_O1 * o, Test1_T_U * u);
  virtual Test1_T_I BS_to_I (Test1Status *_status, Test1_T_BS b);
  virtual ilu_Integer SR_to_I (Test2Status *_status, ilu_ShortReal i);
  virtual Test1_T_U * I_to_Test1U (Test3Status *_status, ilu_Integer i);
};


static char *strdup(char *s)
{
  char *copy = NULL;
  if (s != NULL)
    {
      copy = new char[strlen(s) + 1];
      strcpy (copy, s);
    }
  return (copy);
}

///////////////////// Test1_T_O1_impl methods /////////////////////

Test1_T_O1_impl::Test1_T_O1_impl(char *instanceHandle, iluServer *server)
{
  this->ourInstanceHandle = instanceHandle;
  this->ourServer = server;
}

char * Test1_T_O1_impl::ILUGetInstanceHandle()
{
  return this->ourInstanceHandle;
}

iluServer * Test1_T_O1_impl::ILUGetServer()
{
  return this->ourServer;
}

Test1_T_U * Test1_T_O1_impl::U_CSS_to_U (Test1Status *_status, Test1_T_U * u, Test1_T_CSS)
{
  Test1_T_U *result = new Test1_T_U;
  ilu_IdentityInfo ident;

  fprintf (stdout,"Test1.O1.U-CSS-to-U");
  ident = (ilu_IdentityInfo) ilu_FindIdentity (_status->callerPassport, ilu_ConnectionIdentity);
  if (ident != ILU_NIL)
    {
      fprintf (stdout," [caller is connection:\"%s\"", (ilu_string) (ident->ii_info));
    }
#ifdef SUNRPC_PROTOCOL
  ident = (ilu_IdentityInfo) ilu_FindIdentity (_status->callerPassport, ilu_SunRPCAuthUnixIdentity);
  if (ident != ILU_NIL)
    {
      fprintf (stdout,", sunrpc-authunix:(%u,%u)@%s",
	      (unsigned int) ((ilu_SunRPCAuthUnixIdentityInfo) (ident->ii_info))->ii_UID,
	      (unsigned int) ((ilu_SunRPCAuthUnixIdentityInfo) (ident->ii_info))->ii_GID,
	      ((ilu_SunRPCAuthUnixIdentityInfo)(ident->ii_info))->ii_hostname);
    }
#endif
#ifdef SECURE_TRANSPORT
  ident = (ilu_IdentityInfo) ilu_FindIdentity (_status->callerPassport, ilu_GSSIdentity);
  if (ident != ILU_NIL)
    {
      ilu_Error err;
      gss_name_t name;
      ilu_boolean localp;
      ilu_string s;

      if (ilu_DecodeGSSIdentity (ident, &name, 0, 0, &localp, 0, &err))
	{
	  if ((s = ilu_GSSNameToString (name, &err)) != ILU_NIL)
	    {
	      fprintf(stdout, "GSS:\"%s\"(%s)", s, (localp ? "local" : "remote"));
	      ilu_free(s);
	    }
	  else
	    {
	      fprintf(stderr, "Error <%s> on attempt to stringify GSS name.\n", ILU_ERR_NAME(err));
	      ILU_HANDLED(err);
	    }
	}
      else
	{
	  fprintf (stderr, "Error <%s> on attempt to display GSS identity.\n", ILU_ERR_NAME(err));
	  ILU_HANDLED(err);
	}
    }
#endif
  fprintf (stdout,"]\n");
  *result = *u;
  _status->returnCode = Test1Reply_Success;
  return result;
}

Test1_T_RO Test1_T_O1_impl::f_CSS_to_RO (Test1Status *_status, Test1_T_CSS)
{
  Test1_T_RO x = new Test1_T_R;

  x->i = 9;
  x->css = new _Test1_T_CSS_sequence;
  x->a[0]= strdup("hi");
  x->a[1]= strdup("hi");
  x->a[2]= strdup("hi");
  fprintf (stdout,"Test1.O1.f-CSS-to-R0\n");
  _status->returnCode = Test1Reply_Success;
  return x;
}

ilu_ShortReal Test1_T_O1_impl::R_ScS_to_F (Test1Status *_status, Test1_T_R *, Test1_T_ScS)
{
  float	f = 39.7;

  fprintf(stdout,"Test1.O1.R-ScS-to-F\n");
  _status->returnCode = Test1Reply_Success;
  return f;
}

void Test1_T_O1_impl::a_RO (Test1Status *_status, Test1_T_RO)
{
  fprintf(stdout,"Test1.O1.a-RO\n");
  _status->returnCode = Test1Reply_Success;
}

class Test1_T_O2 * Test1_T_O1_impl::get_O2 (Test1Status *_status)
{
  static Test1_T_O2 *uc = NULL;

  fprintf (stdout,"Test1.O1.get-O2\n");
  if (uc == NULL)
    uc = new Test1_T_O2_impl;
  if (uc == NULL)
    {
      _status->returnCode = Test1_E_CantCreate;
      return NULL;
    }
  _status->returnCode = Test1Reply_Success;
  return uc;
}

class Test1_T_O3 * Test1_T_O1_impl::get_O3 (Test1Status *_status, ilu_Boolean subclass)
{
  Test1_T_O3 *uc;
  static int one = 0;

  fprintf (stdout,"Test1.O1.get-O3\n");
  if (subclass)
    uc = new Test3_T_O_impl();
  else
    {
      if (one == 0)
	{
	  one = 1;
	  fprintf (stdout,"making O3...\n");
	  uc = new Test1_T_O3_impl();
	}
      else
	{
	  one = 0;
	  fprintf (stdout,"making O4...\n");
	  uc = new Test1_T_O4_impl();
	}
    }
  if (uc == NULL)
    {
      _status->returnCode = Test1_E_CantCreate;
      return NULL;
    }
  _status->returnCode = Test1Reply_Success;
  return uc;
}


///////////////////// Test1_T_O2_impl methods /////////////////////

iluServer * Test1_T_O2_impl::ILUGetServer()
{
  return sunrpcServer;
}

Test1_T_CSS Test1_T_O2_impl::OO_A0_to_CSS (Test1Status *_status, Test1_T_OO o, Test1_T_A0)
{
  fprintf (stdout, "Test1.o2.OO-A0-to-CSS\n");
  if (o == NULL)
    {
      _status->returnCode = Test1_E_E2;
      _status->values.Test1_E_E2_Value = 7;
      return NULL;
    }
  _status->returnCode = Test1Reply_Success;
  return new _Test1_T_CSS_sequence();
}

Test1_T_A0 * Test1_T_O2_impl::R_I_A1_to_I_A0 (Test1Status *_status, Test1_T_R *, Test1_T_I *, Test1_T_A1)
{
  Test1_T_A0 *a2;

  fprintf (stdout,"Test1.O2.R-I-A1-to-I-A0\n");
  a2 = (Test1_T_A0 *) malloc(sizeof(Test1_T_A0));
  _status->returnCode = Test1Reply_Success;
  return a2;
}


///////////////////// Test1_T_O3_impl methods /////////////////////

Test1_T_IS Test1_T_O3_impl::RS_R_to_R_IS (Test1Status *_status, Test1_T_RS, Test1_T_R * r2)
{
  Test1_T_IS is;

  fprintf (stdout,"Test1.O3.RS-R-to-R-IS\n");
  r2->i = 3;
  r2->css = new _Test1_T_CSS_sequence;
  r2->a[0] = strdup("just");
  r2->a[1] = strdup("a");
  r2->a[2] = strdup("string");
  is = new _Test1_T_IS_sequence;
  _status->returnCode = Test1Reply_Success;
  return is;
}

void Test1_T_O3_impl::O1_U_to_U (Test1Status *_status, Test1_T_O1 * o, Test1_T_U * u)
{
  fprintf (stdout,"Test1.O3.O1-U-to-U\n");
  u->discriminator = 3;
  u->value.O1 = o;
  _status->returnCode = Test1Reply_Success;
}

Test1_T_I Test1_T_O3_impl::BS_to_I (Test1Status *_status, Test1_T_BS b)
{
  _status->returnCode = Test1Reply_Success;
  return b->Length() * b->Length();
}


///////////////////// Test1_T_P_impl methods /////////////////////

Test1_T_IS Test1_T_P_impl::RS_R_to_R_IS (Test1Status *_status, Test1_T_RS, Test1_T_R * r2)
{
  Test1_T_IS is;

  fprintf (stdout,"Test1.P.RS-R-to-R-IS\n");
  r2->i = 25179;
  r2->css = new _Test1_T_CSS_sequence;
  r2->a[0] = strdup("from");
  r2->a[1] = strdup("P");
  r2->a[2] = strdup("string");
  is = new _Test1_T_IS_sequence;
  _status->returnCode = Test1Reply_Success;
  return is;
}

void Test1_T_P_impl::O1_U_to_U (Test1Status *_status, Test1_T_O1 * o, Test1_T_U * u)
{
  fprintf (stdout,"Test1.P.O1-U-to-U\n");
  u->discriminator = 3;
  u->value.O1 = (Test1_T_TheO1 *) o;
  _status->returnCode = Test1Reply_Success;
}

Test1_T_I Test1_T_P_impl::BS_to_I (Test1Status *_status, Test1_T_BS b)
{
  _status->returnCode = Test1Reply_Success;
  return b->Length();
}

Test1_T_IS Test1_T_P_impl::m2 (Test1Status *_status, ilu_Integer j)
{
  Test1_T_IS foo;

  foo = new _Test1_T_IS_sequence;
  foo->Append(j);
  foo->Append(j * j);
  _status->returnCode = Test1Reply_Success;
  return foo;
}


///////////////////// Test1_T_O4_impl methods /////////////////////

Test1_T_IS Test1_T_O4_impl::RS_R_to_R_IS (Test1Status *_status, Test1_T_RS, Test1_T_R * r2)
{
  Test1_T_IS is;

  fprintf (stdout,"Test1.O4.RS-R-to-R-IS\n");
  r2->i = 25179;
  r2->css = new _Test1_T_CSS_sequence;
  r2->a[0] = strdup("from");
  r2->a[1] = strdup("P");
  r2->a[2] = strdup("string");
  is = new _Test1_T_IS_sequence;
  _status->returnCode = Test1Reply_Success;
  return is;
}

void Test1_T_O4_impl::O1_U_to_U (Test1Status *_status, Test1_T_O1 * o, Test1_T_U * u)
{
  fprintf (stdout,"Test1.O4.O1-U-to-U\n");
  u->discriminator = 3;
  u->value.O1 = (Test1_T_TheO1 *) o;
  _status->returnCode = Test1Reply_Success;
}

Test1_T_I Test1_T_O4_impl::BS_to_I (Test1Status *_status, Test1_T_BS b)
{
#define GETB(x)		((b->Length() <= (x)) ? 0 : b->Nth(x))

  fprintf (stdout,"Test1.O4.BS_to_I (%ul:  %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x ...) => %ul\n",
	  b->Length(),
	  GETB(0), GETB(1), GETB(2), GETB(3), GETB(4), GETB(5), GETB(6), GETB(7), GETB(8), GETB(9), GETB(10),
	  b->Length());
  _status->returnCode = Test1Reply_Success;
  return b->Length();
}

ilu_Real Test1_T_O4_impl::R_to_R (Test1Status *_status, ilu_Real r)
{
  ilu_real r2 = 1020304.05060708;

  fprintf (stdout,"Test1.O4.R_to_R (%.10f) => %.10f\n", r, r2);
  _status->returnCode = Test1Reply_Success;
  return r2;
}


///////////////////// Test3_T_O_impl methods /////////////////////

Test1_T_IS Test3_T_O_impl::RS_R_to_R_IS (Test1Status *_status, Test1_T_RS, Test1_T_R * r2)
{
  Test1_T_IS is;

  fprintf (stdout,"Test3.O.RS-R-to-R-IS\n");
  r2->i = 3;
  r2->css = new _Test1_T_CSS_sequence;
  r2->a[0] = strdup("just");
  r2->a[1] = strdup("a");
  r2->a[2] = strdup("string");
  is = new _Test1_T_IS_sequence;
  _status->returnCode = Test1Reply_Success;
  return is;
}

void Test3_T_O_impl::O1_U_to_U (Test1Status *_status, Test1_T_O1 * o, Test1_T_U * u)
{
  fprintf (stdout,"Test3.O.O1-U-to-U(0x%lx, {%d})\n",
    (unsigned long) o, u->discriminator);
  u->discriminator = 3;
  u->value.O1 = (Test1_T_TheO1 *) o;
  _status->returnCode = Test1Reply_Success;
}

Test1_T_I Test3_T_O_impl::BS_to_I (Test1Status *_status, Test1_T_BS b)
{
  _status->returnCode = Test1Reply_Success;
  return b->Length() * b->Length();
}

ilu_Integer Test3_T_O_impl::SR_to_I (Test2Status *_status, ilu_ShortReal i)
{
  _status->returnCode = Test1Reply_Success;
  fprintf(stdout,"Test3.O.SR-to-I(%f)\n", i);
  return (ilu_Integer) i;
}

Test1_T_U * Test3_T_O_impl::I_to_Test1U (Test3Status *_status, ilu_Integer i)
{
  Test1_T_U *u;

  fprintf (stdout,"Test3.O.I-to-Test1U(%ld)\n", i);
  u = new Test1_T_U;
  u->discriminator = 5;
  u->value.boolean = ilu_TRUE;
  _status->returnCode = Test3Reply_Success;
  return u;
}


int main (int, char **)
{
#ifdef macintosh
	/*
	 * On the Mac, the C++ auto-initialization is disabled in both the stubbers
	 * and the runtime. This makes it easy (and mandatory) to specify when and where
	 * ILU should be initialized. Perhaps in the future all platforms will have this
	 * option?
	 */

	extern void Test1__InitializeServer(void);
	extern void Test2__InitializeServer(void);
	extern void Test3__InitializeServer(void);

	ILUStartup();
	Test1__Initialize();
	Test2__Initialize();
	Test3__Initialize();
	Test1__InitializeServer();
	Test2__InitializeServer();
	Test3__InitializeServer();
#endif

  iluServer s ("Test1-Server", NULL);
  Test1_T_O1 *uc;
  Test1_T_O1 *uc2;
  char *sunrpc_tinfo[] = { "sunrpcrm", "tcp_0_0", (char *) 0 };

  s.AddPort(NULL, NULL, ilu_TRUE);
  ilu::SetDefaultServer(&s);

  sunrpcServer = new iluServer ("Test1-Server-SunRPC", NULL);
  sunrpcServer->AddPort ("sunrpc", sunrpc_tinfo, ilu_FALSE);

  uc = new Test1_T_O1_impl("Test1_Initial_Object", &s);
  if (!uc->ILUPublish())
    {
      fprintf (stderr,"*** Error, couldn't publish object\n");
      exit(1);
    }

  /* test the publish and lookup a bit */
  uc2 = (Test1_T_O1 *) iluObject::Lookup("Test1-Server",
					 "Test1_Initial_Object",
				    Test1_T_TheO1::ILUClassRecord);
  if (uc2 != uc)
    fprintf (stderr,"*** Error, lookup returns wrong object\n");
  if (uc2 && !uc2->ILUPublish())
    fprintf (stderr,"*** Error, second publish failed\n");

  if (uc != NULL)
    {
      fprintf (stdout, "exported %s\n", uc->ILUStringBindingHandle());
      s.Run();
    }
  else
    {
      fprintf (stdout,"couldn't create object\n");
      exit(1);
    }
  return 1;
}
