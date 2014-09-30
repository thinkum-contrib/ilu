/** $Id: client.c,v 1.4 1999/08/30 22:30:14 janssen Exp $
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
/* Last edited by Mike Spreitzer October 8, 1998 10:49 pm PDT */

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>	/* for exit() */
#include <errno.h>	/* for errno */

#include <CosNaming.h>
#include "NamingTest.h"

#define STRINGIFIED_DOMAIN(x)	#x
#define STRINGIFIED_DOMAIN1(x)	STRINGIFIED_DOMAIN(x)
#define THIS_DOMAIN		STRINGIFIED_DOMAIN1(FULLY_QUALIFIED_DOMAIN_NAME)

static int
  CountNameListComponents (ilu_string n)
{
  int count = 0;
  char *p = n;
  if (*p == '/') p++;
  while (*p != 0) {
    if (*p == '\\')
      p++;
    else if (*p == '/')
      count++;
    p++;
  }
  return count + 1;
}

static char *FindComponentEnd (ilu_string n)
{
  char *p = n;
  while (*p != 0) {
    if (*p == '\\')
      p++;
    else if (*p == '/')
      return p;
    p++;
  }
  return ILU_NIL;
}

static char *FindComponentIDField (ilu_string n)
{
  char *p = n;
  while (*p != 0) {
    if (*p == '\\')
      p++;
    else if (*p == '.')
      return p;
    p++;
  }
  return ILU_NIL;
}

static const char hextable[] = "0123456789ABCDEF";
#define HEXDIGIT(x)	(hextable[x])
#define HEXVALUE(x)	\
	 (((x)>='0'&&(x)<='9')?((x)-'0')\
	:(((x)>='A'&&(x)<='F')?((x)-'A'+10)\
	:(((x)>='a'&&(x)<='f')?((x)-'a'+10):16)))

/*
 * Copy s into b, turning quoted chars into plain chars; return next
 * position in b after terminating 0.  Raise inv_objref/minor on
 * quoting botch.  Raise internal/check if b isn't long enough.
 */
static          ilu_string
DeQuoteBuffer(char *s, ilu_cardinal slen,
	      char *b, ilu_cardinal blen, ilu_cardinal * out_len,
	      CORBA_Environment *err)
{
  char           *sp = s, *slim = s + slen;
  char           *bp = b, *blim = b + blen;
  for (; (sp < slim) && (bp < blim); bp++)
    if (*sp == '%') {
      int             hv1, hv2;
      if ((slim - sp <= 2)
	  || ((hv1 = HEXVALUE(sp[1])) == 16)
	  || ((hv2 = HEXVALUE(sp[2])) == 16)) {
	ILU_C_RAISE_SYSTEM(err,MARSHAL,ilu_mm_url_quoted_char,NO);
	return ILU_NIL;
      };
      *bp = (char) ((hv1 << 4) + hv2);
      sp += 3;
    } else
      *bp = *sp++;
  if (!(sp == slim && bp < blim)) {
    ILU_C_RAISE_SYSTEM(err,INTERNAL,0,NO);
    return ILU_NIL;
  };
  *out_len = bp - b;
  *bp++ = 0;
  return (bp);
}

static ilu_string
  DecodeBuffer (char *b, ilu_cardinal len, ilu_cardinal *out_len,
		CORBA_Environment *err)
{
  char		*p, *plim = b + len, *ans;
  ilu_cardinal	count = 0, i;
  for (p = b; p < plim;)
    if (*p == '%') {
      count++;
      p += 3;
    } else
      p++;

  ans = (char *) ilu_malloc(i = len - (count * 2) + 1);
  if (ans == ILU_NIL) {
    ILU_C_RAISE_SYSTEM(err,NO_MEMORY,i,NO);
    return ILU_NIL;
  }
  if (DeQuoteBuffer(b, len, ans, i, out_len, err) == ILU_NIL)
    return ILU_NIL;
  return ans;
}

static CosNaming_Name *
  ParsePathIntoNameList (ilu_string path, CORBA_Environment *err)
{
  int components, i, j;
  CosNaming_Name *n;
  char *component, *ending, *possible_dot;
  ilu_cardinal decoded_len;

  components = CountNameListComponents(path);
  n = (CosNaming_Name *) ilu_malloc(sizeof(*n) + (components * sizeof(CosNaming_NameComponent)));
  if (n == ILU_NIL) {
    ILU_C_RAISE_SYSTEM(err,NO_MEMORY,sizeof(*n) + (components * sizeof(CosNaming_NameComponent)),NO);
    return ILU_NIL;
  };
  n->_buffer = (CosNaming_NameComponent *) (n + 1);
  n->_length = components;
  n->_maximum = n->_length;
  for (component = path, i = 0;  i < components;  i++) {
    ending = FindComponentEnd(component);
    if (ending != ILU_NIL) *ending = 0;
    n->_buffer[i].id = DecodeBuffer(component, strlen(component), &decoded_len, err);
    if (!ILU_C_SUCCESSFUL(err)) goto errexit;
    if ((possible_dot = FindComponentIDField(n->_buffer[i].id)) == ILU_NIL) {
      n->_buffer[i].kind = n->_buffer[i].id + strlen(n->_buffer[i].id);
    } else {
      *possible_dot = 0;
      n->_buffer[i].kind = possible_dot + 1;
    }
    if (ending != ILU_NIL)
      component = ending + 1;
  }
  return n;

 errexit:
  for (j = 0;  j <= i;  j++)
    ilu_free(n->_buffer[j].id);
  ilu_free(n);
  return ILU_NIL;
}

static void
  FreeNameList (CosNaming_Name *n)
{
  int i;

  for (i = 0;  i < n->_length;  i++)
    ilu_free(n->_buffer[i].id);
  ilu_free(n);
}

int main (int ac, char **av)
{
  NamingTest_O	uc;
  char *	ltinfo[10];
  ilu_ProtocolInfo	pinfo = ILU_NIL;
  ilu_TransportInfo	tinfo = ILU_NIL;
  ILU_C_Server	s;
  char *	proof;
  char		sid[1000];
  int		i;
  ilu_boolean	threaded = ilu_FALSE;
  char *	url = ILU_NIL;
  char *	name = ILU_NIL;
  char *	ns = ILU_NIL;
  char *	iorfile = ILU_NIL;
  CORBA_Environment env;
  CosNaming_NamingContext ns_obj, ctx, existing_ctx;
  CosNaming_Name *names;
  ilu_cardinal count = 0;
  CORBA_Object theORB;

  theORB = CORBA_ORB_init (&ac, av, "ilu", &env);
  if (!ILU_C_SUCCESSFUL(&env)) {
    fprintf(stderr, "Can't initialize ORB.  Exception is <%s>.\n",
	    CORBA_exception_id(&env));
    exit(1);
  };

  i = 1;
  while (i < ac) {
    if (strcmp(av[i], "-mt") == 0) {
      threaded = ilu_TRUE;
      ++i;
    } else if (strcmp(av[i], "-name") == 0) {
      if (++i < ac)
	name = av[i++];
      else
	goto usage;
    } else if (strcmp(av[i], "-url") == 0) {
      if (++i < ac)
	url = av[i++];
      else
	goto usage;
    } else if (strcmp(av[i], "-ns") == 0) {
      if (++i < ac)
	ns = av[i++];
      else
	goto usage;
    } else if (strcmp(av[i], "-ior_file") == 0) {
      if (++i < ac)
	iorfile = av[i++];
      else
	goto usage;
    } else
      goto usage;
  }

  if (threaded) {
#if (defined(ILU_OS_THREADED))
    ILU_C_USE_OS_THREADS;
#else
    OUTPUT("OS-supplied thread support not configured into ILU!\n");
    exit(-1);
#endif				/* (defined(ILU_OS_THREADED)) */
  }

  CosNaming__Initialize();
  NamingTest__Initialize();
  sprintf(sid, "NamingTest.server.%s", THIS_DOMAIN);

  if (name != ILU_NIL) {
      if (ns != ILU_NIL) {
	  ns_obj = ILU_C_SBHToObject(ns, CosNaming_NamingContext__MSType, &env);
	  if (!ILU_C_SUCCESSFUL(&env)) {
	      fprintf(stderr, "Can't bind name service using URL <%s>; exception is <%s>\n", ns, CORBA_exception_id(&env));
	      exit(1);
	  };
      } else if (iorfile != ILU_NIL) {
	  char buf[10000];
	  FILE * fp = fopen(iorfile, "r");
	  if (fp == ILU_NIL) {
	      fprintf(stderr, "Can't open IOR file \"%s\".  Errno is \"%s\".\n", iorfile, strerror(errno));
	      exit(1);
	  };
	  if (fgets(buf, sizeof(buf), fp) == ILU_NIL) {
	      fprintf(stderr, "Can't read from open file:  Errno is \"%s\".\n", strerror(errno));
	      fclose(fp);
	      exit(1);
	  };
	  if (buf[strlen(buf)-1] == '\n')
	      buf[strlen(buf)-1] = '\0';
	  ns_obj = ILU_C_SBHToObject(buf, CosNaming_NamingContext__MSType, &env);
	  if (!ILU_C_SUCCESSFUL(&env)) {
	      fprintf(stderr, "Can't bind name service using URL <%s>; exception is <%s>\n", buf, CORBA_exception_id(&env));
	      exit(1);
	  };
      } else {
	  ns_obj = CORBA_ORB_resolve_initial_references(theORB, "NameService", &env);
	  if (!ILU_C_SUCCESSFUL(&env)) {
	      fprintf(stderr, "Can't locate name service with resolve_initial_references; exception is <%s>\n",
		      CORBA_exception_id(&env));
	      exit(1);
	  };
      }
      names = ParsePathIntoNameList (name, &env);
      if (!ILU_C_SUCCESSFUL(&env)) {
	  fprintf(stderr, "Can't parse name <%s>\n", name);
	  exit(1);
      };
      uc = (NamingTest_O) CosNaming_NamingContext_resolve (ns_obj, names, &env);
  } else if (url != ILU_NIL) {
      uc = ILU_C_SBHToObject(url, NamingTest_O__MSType, &env);
  } else {
      fprintf (stderr, "No way to bind object.  No name or IOR specified.\n");
      exit(1);
  }
    
  if (!ILU_C_SUCCESSFUL(&env)) {
      fprintf (stderr, "Can't bind object in server; exception <%s>\n", CORBA_exception_id(&env));
      exit(1);
  };

  printf("handle is %s\n", ILU_C_SBHOfObject(uc));
  count = NamingTest_O_print_hello (uc, &env);
  if (!ILU_C_SUCCESSFUL(&env)) {
    fprintf (stderr, "print_hello failed with err code <%s>\n", CORBA_exception_id(&env));
    exit(1);
  };
  printf ("print_hello() returns %lu\n", (unsigned long) count);

  return 0;


usage:
  fprintf(stderr, "Usage: %s [-mt] [-n NAME] [-ns NAMESERVICE-URL] [-p PINFO] [-t TINFO [TINFO...]]\n", av[0]);
  return 2;
}
