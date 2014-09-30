/** $Id: srvr.c,v 1.67 1999/08/03 01:52:29 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 11:36 am PDT */

#include <stdio.h>
#include <math.h>
#include <stdlib.h>	/* for exit() */

#include "Test1.h"
#include "Test3.h"

#include "srvr.h"

ILU_C_Server theServer = ILU_NIL;
ILU_C_Server SunRPCServer = ILU_NIL;

#if (defined(SECURE_TRANSPORT) && defined(SUNRPCRM_TRANSPORT) && defined(TCPIP_TRANSPORT))

#include <gssapi.h>

#include <ilugssmech_nil.h>
#include <ilugssns_rfc822.h>

#ifdef INCLUDE_SSL_SECMECH
#include <ilugssmech_ssl.h>
#include <ilugssns_x509.h>
#include <sys/stat.h>
#include <string.h>

static ilu_boolean
  SSLReadFile (char *path, gss_buffer_t buf)
{
  struct stat stbuf;
  FILE *fp;

  if (stat(path,&stbuf) < 0) {
    perror("stat");
    return ilu_FALSE;
  }
  buf->length=stbuf.st_size;
  buf->value=ilu_must_malloc(buf->length);
  if ((fp=fopen(path,"r"))==NULL) {
    perror("fopen");
    return ilu_FALSE;
  }	
  if (fread(buf->value,1,buf->length,fp) < buf->length) {
    perror("fread");
    return ilu_FALSE;
  }
  fclose(fp);
  return ilu_TRUE;
}

static ilu_Passport SetupSSLCredentials (char *name)
{
  OM_uint32       major, minor;
  gss_cred_id_t   cred;
  ILU_C_ENVIRONMENT env;
  ilu_IdentityInfo info, newinfo;
  ilu_Passport    pp;
  gss_buffer_desc cert_buf, key_buf, name_buf;
  gss_name_t server_name;
  char *p;

  /* acquire a credential to pass to gss_accept_sec_context(): */
  if ((!SSLReadFile ("server.cert", &cert_buf)) ||
      (!SSLReadFile ("server.key", &key_buf)))
    return ILU_NIL;

  /* import server name into namespace */
  p = strchr(name, ':');
  name_buf.length = strlen(p + 1);
  name_buf.value = p + 1;
  major = gss_import_name (&minor, &name_buf, ilugssns_x509_OID, &server_name);
  if (GSS_ERROR(major)) {
    ilu_DebugPrintf("SetupSSLCredentials:  gss_import_name (\"%s\") signals error\n", p + 1);
    return ILU_NIL;
  };
  /* use certificate to verify name to SSL mech */
  major = ilugssmech_ssl_auth_name (&minor, server_name, &cert_buf,&key_buf);
  if (GSS_ERROR(major)) {
    ilu_DebugPrintf("SetupSSLCredentials:  ilugssmech_ssl_auth_name() signals error\n");
    return ILU_NIL;
  };
  cred = ILU_C_AcquireGSSCredForName(name,
				     GSS_C_INDEFINITE,
				     ilugssmech_ssl_OID,
				     ilu_TRUE,
				     &env);
  if (!ILU_C_SUCCESSFUL(&env)) {
    ilu_DebugPrintf("Can't acquire credentials for name \"%s\", err <%s>\n",
		    name, CORBA_exception_id(&env));
    CORBA_exception_free(&env);
    return ILU_NIL;
  }
  info = ILU_C_AcquireGSSIdentity(cred, &env);
  if (!ILU_C_SUCCESSFUL(&env)) {
    ilu_DebugPrintf("Can't create GSS identity for name \"%s\", err <%s>\n",
		    name, CORBA_exception_id(&env));
    CORBA_exception_free(&env);
    return ILU_NIL;
  }
  pp = ILU_C_CreatePassport(&env);
  if (!ILU_C_SUCCESSFUL(&env)) {
    ilu_DebugPrintf("Can't create passport, err <%s>\n",
		    CORBA_exception_id(&env));
    CORBA_exception_free(&env);
    
    return ILU_NIL;
  }
  newinfo = ILU_C_CopyIdentity(info, &env);
  if (!ILU_C_SUCCESSFUL(&env)) {
    ilu_DebugPrintf("Can't copy GSS identity for name \"%s\", err <%s>\n",
		    name, CORBA_exception_id(&env));
    CORBA_exception_free(&env);
    ILU_C_DestroyPassport(pp, &env);
    CORBA_exception_free(&env);
    return ILU_NIL;
  }
  (void) ILU_C_AddIdentity(pp, newinfo, &env);
  if (!ILU_C_SUCCESSFUL(&env)) {
    ilu_DebugPrintf("Can't add GSS identity %x to passport %x, err <%s>\n",
		    (void *) newinfo, (void *) pp, CORBA_exception_id(&env));
    CORBA_exception_free(&env);
    ILU_C_DestroyPassport(pp, &env);
    CORBA_exception_free(&env);
    ILU_C_FreeIdentity(newinfo, &env);
    CORBA_exception_free(&env);
    return ILU_NIL;
  }
  return pp;
}


#endif

static ilu_Passport SetupNILCredentials (char *name)
{
  OM_uint32       major, minor;
  gss_cred_id_t   cred;
  ILU_C_ENVIRONMENT env;
  ilu_IdentityInfo info, newinfo;
  ilu_Passport    pp;

  cred = ILU_C_AcquireGSSCredForName(name,
				     GSS_C_INDEFINITE,
				     ilugssmech_nil_OID,
				     ilu_TRUE,
				     &env);
  if (!ILU_C_SUCCESSFUL(&env)) {
    ilu_DebugPrintf("Can't acquire credentials for name \"%s\", err <%s>\n",
		    name, CORBA_exception_id(&env));
    CORBA_exception_free(&env);
    return ILU_NIL;
  }
  info = ILU_C_AcquireGSSIdentity(cred, &env);
  if (!ILU_C_SUCCESSFUL(&env)) {
    ilu_DebugPrintf("Can't create GSS identity for name \"%s\", err <%s>\n",
		    name, CORBA_exception_id(&env));
    CORBA_exception_free(&env);
    return ILU_NIL;
  }
  pp = ILU_C_CreatePassport(&env);
  if (!ILU_C_SUCCESSFUL(&env)) {
    ilu_DebugPrintf("Can't create passport, err <%s>\n",
		    CORBA_exception_id(&env));
    CORBA_exception_free(&env);
    
    return ILU_NIL;
  }
  newinfo = ILU_C_CopyIdentity(info, &env);
  if (!ILU_C_SUCCESSFUL(&env)) {
    ilu_DebugPrintf("Can't copy GSS identity for name \"%s\", err <%s>\n",
		    name, CORBA_exception_id(&env));
    CORBA_exception_free(&env);
    ILU_C_DestroyPassport(pp, &env);
    CORBA_exception_free(&env);
    return ILU_NIL;
  }
  (void) ILU_C_AddIdentity(pp, newinfo, &env);
  if (!ILU_C_SUCCESSFUL(&env)) {
    ilu_DebugPrintf("Can't add GSS identity %x to passport %x, err <%s>\n",
		    (void *) newinfo, (void *) pp, CORBA_exception_id(&env));
    CORBA_exception_free(&env);
    ILU_C_DestroyPassport(pp, &env);
    CORBA_exception_free(&env);
    ILU_C_FreeIdentity(newinfo, &env);
    CORBA_exception_free(&env);
    return ILU_NIL;
  }
  return pp;
}

#endif /* SECURE_TRANSPORT etc. */

#if (defined(SUNRPCRM_TRANSPORT) && defined(TCPIP_TRANSPORT))

static ilu_string default_tinfo_insecure[3] = {"sunrpcrm", "tcp_0_0", ILU_NIL};

#if defined(SECURE_TRANSPORT)
static ilu_string default_tinfo_secure[5] = {
  "sunrpcrm",
  "gss_1_Xerox.ILU.GSS.NIL",
  "sunrpcrm", "tcp_0_0", ILU_NIL};
#ifdef INCLUDE_SSL_SECMECH
static ilu_string default_tinfo_secure_with_ssl[5] = {
  "sunrpcrm",
  "gss_1_Xerox.ILU.GSS.SSL",
  "sunrpcrm", "tcp_0_0", ILU_NIL};
#else
static ilu_string *default_tinfo_secure_with_ssl = ILU_NIL;
#endif
#else
static ilu_string *default_tinfo_secure = ILU_NIL;
static ilu_string *default_tinfo_secure_with_ssl = ILU_NIL;
#endif /* security filter available */

#else
#error "Don't know which transport to use"
#endif


ilu_TransportInfo DefaultTInfo(ilu_boolean secure, ilu_boolean ssl)
{
  if (!secure)
    return ilu_DefaultTransportInfo();
  else if (ssl)
    return default_tinfo_secure_with_ssl;
  else
    return default_tinfo_secure;
}

static ilu_string proof = NULL;

int
doit(char *pinfo, ilu_TransportInfo tinfo, ilu_boolean threadly,
     ilu_boolean init_credentials, ilu_boolean use_ssl_security)
{
#if defined(_WINDOWS)
  extern void     set_process_windows_messages_alarm(int *pi_stop);
  static int      i_stop = 0;
#endif
  Test1_O1        uc, uc2;
  ilu_Passport    pp = ILU_NIL;
  CORBA_Environment env;

  if (threadly) {
#if (defined(ILU_OS_THREADED))
    ILU_C_USE_OS_THREADS;
#else
    OUTPUT("OS-supplied thread support not configured into ILU!\n");
    exit(-1);
#endif				/* (defined(ILU_OS_THREADED)) */
  }

  Test1__InitializeServer();
  Test3__InitializeServer();
  if (pinfo == NULL)
    pinfo = ilu_DefaultProtocolInfo();
  if (tinfo != NULL)
    0;
  else if (use_ssl_security && (default_tinfo_secure_with_ssl != ILU_NIL)) {
    tinfo = default_tinfo_secure_with_ssl;
    init_credentials = ilu_TRUE;
  } else if (default_tinfo_secure != ILU_NIL) {
    tinfo = default_tinfo_secure;
    init_credentials = ilu_TRUE;
  } else
    tinfo = default_tinfo_insecure;

  if (init_credentials) {
#if defined(SECURE_TRANSPORT)
#if INCLUDE_SSL_SECMECH
    if (use_ssl_security) {
      OUTPUT("setting up SSL GSS credentials...\n");
      ilugssmech_ssl_initialize();
      ilugssns_x509_initialize();
      if ((pp = SetupSSLCredentials
	   ("Xerox.ILU.GSS.X509:/C=US/SP=CA/O=Xerox/OU=PARC/CN=SSLTestServer/")) == ILU_NIL)
	fprintf(stderr, "Can't initialize SSL credentials.\n");
    } else
#endif
      {
	OUTPUT("setting up NIL GSS credentials...\n");
	ilugssmech_nil_initialize();
	ilugssns_rfc822_initialize();
	if ((pp = SetupNILCredentials
	     ("Xerox.ILU.GSS.RFC822:server@test1.examples.parc.xerox.com")) == ILU_NIL)
	  fprintf(stderr, "Can't initialize NIL credentials.\n");
      }
#else
    OUTPUT("Security support not configured into ILU!\n");
    exit(-1);
#endif				/* security */
  }

  theServer = ILU_C_InitializeServer("Test1-Server", NULL, pinfo, tinfo,
				     pp, ilu_TRUE);

  if (theServer == NULL) {
    OUTPUT("*** Error, Couldn't create server!\n");
    exit(1);
  }
  uc = Test1_O1__CreateTrue("Test1_Initial_Object", theServer, NULL);
  if (uc == NULL) {
    OUTPUT("*** Error, couldn't create object!\n");
    exit(1);
  }
  if ((proof = ILU_C_PublishObject(uc)) == NULL) {
    fprintf(stderr, "*** Error, can't publish Test1-Server object!\n");
    exit(1);
  }
  /* test the publish and lookup a bit */
  uc2 = ILU_C_LookupObject("Test1-Server", "Test1_Initial_Object",
			   Test1_O1__MSType);
  if (uc2 != uc) {
    fprintf(stderr, "*** Error, lookup returns wrong object!\n");
    exit(1);
  }
  CORBA_Object_release(uc2, &env);
  uc2 = ILU_NIL;
  CORBA_exception_free(&env);
  OUTPUT("exported %s\n", ILU_C_SBHOfObject(uc));
#if defined(_WINDOWS)
  set_process_windows_messages_alarm(&i_stop);
  ILU_C_Stoppable_Run(&i_stop);
  return 0;
#elif (defined(ILU_OS_THREADED))
  ILU_C_FINISH_MAIN_THREAD(0);
#else
  ILU_C_Run();
  return 0;
#endif
}

#define Test1_U__R 0
#define Test1_U__RO 1
#define Test1_U__CSS 2
#define Test1_U__O1 3
#define Test1_U__OO 4
#define Test1_U__boolean 5

Test1_U        *
server_Test1_TheO1_U_CSS_to_U(Test1_O1 h, Test1_U * u, Test1_CSS * css,
			      ILU_C_ENVIRONMENT * env)
{
  ilu_Passport    p;
  ilu_IdentityInfo ident;
  int             first;
  Test1_U        *ans = CORBA_sequence_Test1_U_allocbuf(1);

  p = ILU_C_CallerIdentity();
  OUTPUT("Test1.O1.U-CSS-to-U");
  if (p != ILU_NIL) {
    first = 0;
    OUTPUT(" [caller is");
    ident = ILU_C_FindIdentity(p, ilu_ConnectionIdentity);
    if (ident != ILU_NIL)
      OUTPUT("%sconnection:\"%s\"",
	     (first++ == 0) ? " " : ", ",
	     (ilu_string) (ident->ii_info));
#ifdef SUNRPC_PROTOCOL
    ident = ILU_C_FindIdentity(p, ilu_SunRPCAuthUnixIdentity);
    if (ident != ILU_NIL)
      OUTPUT("%s sunrpc-authunix:(%u,%u)@%s",
	     (first++ == 0) ? " " : ",",
	     (unsigned int) ((ilu_SunRPCAuthUnixIdentityInfo) (ident->ii_info))->ii_UID,
	     (unsigned int) ((ilu_SunRPCAuthUnixIdentityInfo) (ident->ii_info))->ii_GID,
	     ((ilu_SunRPCAuthUnixIdentityInfo)(ident->ii_info))->ii_hostname);
#endif
#ifdef SECURE_TRANSPORT
    {
      ILU_C_ENVIRONMENT lenv;
      gss_name_t name;
      ilu_boolean localp;
      ilu_string s;

      ident = ILU_C_FindIdentity(p, ilu_GSSIdentity);
      if (ident != ILU_NIL)
	{
	  char buf[1000];
	  ilu_cardinal len;
	  if ((len = ILU_C_DisplayIdentity(ident, buf, sizeof(buf)-1, &lenv)) == 0) {
	    ilu_DebugPrintf ("Error <%s> on attempt to stringify GSS identity.\n",
			     CORBA_exception_id(&lenv));
	    CORBA_exception_free(&lenv);
	  } else {
	    buf[len] = 0;
	    OUTPUT("%sGSS:\"%s\"", (first++ == 0) ? " " : ", ", buf);
	  }

	  if (ILU_C_DecodeGSSIdentity (ident, &name, ILU_NIL, ILU_NIL, &localp, ILU_NIL, &lenv))
	    {
	      if ((s = ILU_C_GSSNameToString (name, &lenv)) != ILU_NIL)
		{
		  OUTPUT("%sGSS:\"%s\"(%s)", (first++ == 0) ? " " : ", ", s, (localp ? "local" : "remote"));
		  ilu_free(s);
		}
	      else
		{
		  ilu_DebugPrintf ("Error <%s> on attempt to stringify GSS name.\n",
				   CORBA_exception_id(&lenv));
		  CORBA_exception_free(&lenv);
		}
	    }
	  else
	    {
	      ilu_DebugPrintf ("Error <%s> on attempt to display GSS identity.\n",
			       CORBA_exception_id(&lenv));
	      CORBA_exception_free(&lenv);
	    }
	}
    }
#endif
    OUTPUT("]");
  }
  OUTPUT("\n");
  ans->_d = Test1_U__O1;
  ans->_u.O1 = CORBA_Object_duplicate(h, env);
  return ans;
}

Test1_RO server_Test1_TheO1_f_CSS_to_RO( Test1_O1 h, Test1_CSS *css, ILU_C_ENVIRONMENT *s )
{
  Test1_RO        x = (Test1_RO) ilu_must_malloc(sizeof(*x));

  x->i = 9;
  Test1_CSS_Init(&x->css, 0, NULL);
  x->a[0] = ILU_C_Strdup("hello");
  x->a[1] = ILU_C_Strdup("world");
  x->a[2] = ILU_C_Strdup("!\n");
  OUTPUT("Test1.O1.f-CSS-to-R0\n");
  return (x);
}

float server_Test1_TheO1_R_ScS_to_F( Test1_O1 h, Test1_R *r, Test1_ScS str, ILU_C_ENVIRONMENT *s )
{
  float	f = 39.7;

  OUTPUT("Test1.O1.R-ScS-to-F returning %f\n", f);
  return( f );
}

void server_Test1_TheO1_a_RO( Test1_O1 h, Test1_RO ro, ILU_C_ENVIRONMENT *s )
{
  OUTPUT("Test1.O1.a-RO\n");
}

Test1_O2 server_Test1_TheO1_get_O2 ( Test1_O1 h, ILU_C_ENVIRONMENT *s)
{
  static Test1_O2 uc = NULL;
  static ILU_C_Server ts = NULL;

  /*
   * Note that Test1.O2 is a singleton class, of cinfo-type
   * "sunrpc_2_0x3458_3".  This means that it has to be exported via
   * a kernel server with a "sunrpc" port...
   */

  OUTPUT("Test1.O1.get-O2\n");
  if (uc == NULL) {
    ilu_string      sunrpctinfo[] = {"sunrpcrm", "tcp_0_0", ILU_NIL};
    if (ts == ILU_NIL) {
      ts = ILU_C_InitializeServer("Test1-SunRPC-Server", NULL, "sunrpc_2_0x3458_3",
				  sunrpctinfo, ILU_NIL, ilu_TRUE);
      if (ts == ILU_NIL) {
	s->_major = ILU_C_USER_EXCEPTION;
	s->returnCode = ex_Test1_CantCreate;
	return (NULL);
      };
    };
    uc = Test1_O2__CreateTrue(NULL, ts, NULL);
  }
  if (uc == NULL) {
    s->_major = ILU_C_USER_EXCEPTION;
    s->returnCode = ex_Test1_CantCreate;
    return (NULL);
  }
  return (CORBA_Object_duplicate(uc, s));
}

Test1_O3 server_Test1_TheO1_get_O3 ( Test1_O1 h, CORBA_boolean b, ILU_C_ENVIRONMENT *s )
{
  static Test1_O3 theObj = ILU_NIL;
  static int      one = 0;
  static Test1_O3 theO3 = ILU_NIL;
  static Test1_O4 theO4 = ILU_NIL;
  static Test3_O  the3_O = ILU_NIL;

  OUTPUT("Test1.O1.get-O3\n");
  if (b) {
    if (the3_O == ILU_NIL) {
      OUTPUT("making Test3.O...\n");
      theObj = the3_O = Test3_O__CreateTrue(NULL, theServer, NULL);
    } else {
      OUTPUT("returning Test3.O...\n");
      theObj = the3_O;
    }
  } else {
    if (one == 0) {
      one = 1;
      if (theO3 == ILU_NIL) {
	OUTPUT("making O3...\n");
	theObj = theO3 = Test1_O3__CreateTrue(NULL, theServer, NULL);
      } else {
	OUTPUT("returning O3...\n");
	theObj = theO3;
      }
    } else {
      one = 0;
      if (theO4 == ILU_NIL) {
	OUTPUT("making O4...\n");
	theObj = theO4 = Test1_O4__CreateTrue(NULL, theServer, NULL);
      } else {
	OUTPUT("returning O4...\n");
	theObj = theO4;
      }
    }
  }
  if (theObj == NULL) {
    s->_major = ILU_C_USER_EXCEPTION;
    s->returnCode = ex_Test1_CantCreate;
    return (NULL);
  }
  return (CORBA_Object_duplicate(theObj, s));
}

Test1_CSS      *
server_Test1_O2_OO_A0_to_CSS(Test1_O2 self, Test1_OO o, Test1_A0 a,
			     ILU_C_ENVIRONMENT * s)
{
  OUTPUT("Test1.o2.OO-A0-to-CSS\n");
  if (o == NULL) {
    s->_major = ILU_C_USER_EXCEPTION;
    s->returnCode = ex_Test1_E2;
    s->ptr = ilu_must_malloc(sizeof(ilu_integer));
    *((ilu_integer *) s->ptr) = 7;
    return (NULL);
  } else {
    Test1_CSS      *ans = CORBA_sequence_Test1_CSS_allocbuf(1);
    Test1_CSS_Init(ans, 0, NULL);
    return (ans);
  }
}

ilu_byte * server_Test1_O2_R_I_A1_to_I_A0 (Test1_O2 self, Test1_R *r, Test1_I *i, Test1_A1 a, ILU_C_ENVIRONMENT *s)
{
  ilu_byte       *a2;

  OUTPUT("Test1.O2.R-I-A1-to-I-A0\n");
  a2 = (ilu_byte *) ilu_must_malloc(8);
#if 1
  for (*i = 0; *i < 8; (*i)++)
    a2[*i] = 1 << (1 + ((6 + *i) % 8));
#else
  (*i) += 2;
  a2[2] = 7;
#endif
  return (a2);
}

Test1_IS       *
server_Test1_O3_RS_R_to_R_IS(Test1_O3 self, Test1_RS * r, Test1_R ** pr2,
			     ILU_C_ENVIRONMENT * s)
{
  Test1_IS       *is = CORBA_sequence_Test1_IS_allocbuf(1);
  Test1_R        *r2 = CORBA_sequence_Test1_R_allocbuf(1);

  OUTPUT("Test1.O3.RS-R-to-R-IS\n");
  r2->i = 3;
  r2->css._maximum = 0;
  r2->css._length = 0;
  r2->css._buffer = NULL;
  r2->a[0] = ILU_C_Strdup("just");
  r2->a[1] = ILU_C_Strdup("a");
  r2->a[2] = ILU_C_Strdup("string");
  *pr2 = r2;
  Test1_IS_Init(is, 0, NULL);
  return (is);
}

Test1_IS       *
server_Test1_P_RS_R_to_R_IS(Test1_P self, Test1_RS * r, Test1_R ** pr2,
			    ILU_C_ENVIRONMENT * s)
{
  Test1_IS       *is = CORBA_sequence_Test1_IS_allocbuf(1);
  Test1_R        *r2 = CORBA_sequence_Test1_R_allocbuf(1);

  OUTPUT("Test1.P.RS-R-to-R-IS\n");
  r2->i = 25719;
  r2->css._maximum = 0;
  r2->css._length = 0;
  r2->css._buffer = NULL;
  r2->a[0] = ILU_C_Strdup("from");
  r2->a[1] = ILU_C_Strdup("P");
  r2->a[2] = ILU_C_Strdup("string");
  *pr2 = r2;
  Test1_IS_Init(is, 0, NULL);
  return (is);
}

Test1_I server_Test1_P_BS_to_I (Test1_P self, Test1_BS *b, ILU_C_ENVIRONMENT *s)
{
  return (b->_length);
}

Test1_IS       *
server_Test1_O4_RS_R_to_R_IS(Test1_O4 self, Test1_RS * r, Test1_R ** pr2,
			     ILU_C_ENVIRONMENT * s)
{
  Test1_IS       *is = CORBA_sequence_Test1_IS_allocbuf(1);
  Test1_R        *r2 = CORBA_sequence_Test1_R_allocbuf(1);

  OUTPUT("Test1.O4.RS-R-to-R-IS\n");
  r2->i = 25719;
  r2->css._maximum = 0;
  r2->css._length = 0;
  r2->css._buffer = NULL;
  r2->a[0] = ILU_C_Strdup("from");
  r2->a[1] = ILU_C_Strdup("O4");
  r2->a[2] = ILU_C_Strdup("string");
  *pr2 = r2;
  Test1_IS_Init(is, 0, NULL);
  return (is);
}

void server_Test1_O4_O1_U_to_U (Test1_O4 self, Test1_O1 o, Test1_U *u, ILU_C_ENVIRONMENT *s)
{
  OUTPUT ("Test1.O4.O1-U-to-U\n");
  Test1_TheU__Free(u);
  u->_d = Test1_U__O1;
  u->_u.O1 = CORBA_Object_duplicate(o, s);
  return;
}

Test1_I server_Test1_O4_BS_to_I (Test1_O4 self, Test1_BS *b, ILU_C_ENVIRONMENT *s)
{
#define GETB(x)		((b->_length <= (x)) ? 0 : b->_buffer[x])

  OUTPUT ("Test1.O4.BS_to_I (%lu:  %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x ...) => %lu\n",
	  b->_length,
	  GETB(0), GETB(1), GETB(2), GETB(3), GETB(4), GETB(5), GETB(6), GETB(7), GETB(8), GETB(9), GETB(10),
	  b->_length);
  return (b->_length);
}

ilu_real server_Test1_O4_R_to_R (Test1_O4 self, ilu_real r, ILU_C_ENVIRONMENT *s)
{
  ilu_real r2 = 1020304.05060708;

  OUTPUT ("Test1.O4.R_to_R (%.10f) => %.10f\n", r, r2);
  return (r2);
}

Test1_I server_Test1_O3_BS_to_I (Test1_P self, Test1_BS *b, ILU_C_ENVIRONMENT *s)
{
  return (b->_length * b->_length);
}

Test1_I server_Test3_O_BS_to_I (Test3_O self, Test1_BS *b, ILU_C_ENVIRONMENT *s)
{
  return (b->_length * b->_length);
}

Test1_IS       *
server_Test1_P_m2(Test1_P self, ilu_integer j, ILU_C_ENVIRONMENT * s)
{
  Test1_IS       *foo = CORBA_sequence_Test1_IS_allocbuf(1);

  Test1_IS_Init(foo, 2, NULL);
  foo->_buffer[0] = j;
  foo->_buffer[1] = j * j;
  return (foo);
}

void server_Test1_O3_O1_U_to_U (Test1_O3 self, Test1_O1 o, Test1_U *u, ILU_C_ENVIRONMENT *s)
{
  OUTPUT ("Test1.O3.O1-U-to-U\n");
  Test1_TheU__Free(u);
  u->_d = Test1_U__O1;
  u->_u.O1 = CORBA_Object_duplicate(o, s);
  return;
}

void server_Test1_P_O1_U_to_U (Test1_P self, Test1_O1 o, Test1_U *u, ILU_C_ENVIRONMENT *s)
{
  OUTPUT ("Test1.P.O1-U-to-U\n");
  Test1_TheU__Free(u);
  u->_d = Test1_U__O1;
  u->_u.O1 = CORBA_Object_duplicate(o, s);
  return;
}

Test1_IS       *
server_Test3_O_RS_R_to_R_IS(Test3_O self, Test1_RS * r, Test1_R ** pr2,
			    ILU_C_ENVIRONMENT * s)
{
  Test1_IS       *is = CORBA_sequence_Test1_IS_allocbuf(1);
  Test1_R        *r2 = CORBA_sequence_Test1_R_allocbuf(1);

  OUTPUT("Test3.O.RS-R-to-R-IS\n");
  r2->i = 3;
  r2->css._maximum = 0;
  r2->css._length = 0;
  r2->css._buffer = NULL;
  r2->a[0] = ILU_C_Strdup("just");
  r2->a[1] = ILU_C_Strdup("a");
  r2->a[2] = ILU_C_Strdup("string");
  *pr2 = r2;
  Test1_IS_Init(is, 0, NULL);
  return (is);
}

void server_Test3_O_O1_U_to_U (Test1_O3 self, Test1_O1 o, Test1_U *u, ILU_C_ENVIRONMENT *s)
{
  OUTPUT ("Test3.O.O1-U-to-U(0x%lx, {%d})\n", (unsigned long) o, u->_d);
  Test1_TheU__Free(u);
  u->_d = Test1_U__O1;
  u->_u.O1 = CORBA_Object_duplicate(o, s);
  return;
}

ilu_integer server_Test3_O_SR_to_I (Test3_O self, ilu_shortreal i, ILU_C_ENVIRONMENT *s)
{
  ilu_integer j = i;

  OUTPUT ("Test3.O.SR-to-I(%f)\n", i);
  return (j);
}

Test1_U        *
server_Test3_O_I_to_Test1U(Test3_O self, Test1_I i,
			   ILU_C_ENVIRONMENT * s)
{
  Test1_U        *u = CORBA_sequence_Test1_U_allocbuf(1);
  OUTPUT("Test3.O.I-to-Test1U(%ld)\n", i);
  u->_d = Test1_U__boolean;
  u->_u.boolean = ilu_TRUE;
  ILU_C_SET_SUCCESSFUL(s);
  return (u);
}
