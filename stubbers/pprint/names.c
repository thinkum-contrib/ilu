/** 
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

/*
$Id: names.c,v 1.5 1999/08/03 01:51:01 janssen Exp $
*/

#include <stdio.h>

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO) */

#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <string.h>

#include <stdlib.h>

#define TRUE  1
#define FALSE 0
typedef int Boolean;

#define AND  &&
#define OR   ||
#define NOT  !

#include <iluptype.h>

#define HYPHEN		'-'
#define UNDERSCORE	'_'
#define EOS		'\0'

extern string name_lang_name(Name, string);
extern string name_base_name(Name);
extern void name_set_lang_name(Name, string, string);

#define ISUPPER(a)	(((a)<='Z')&&((a)>='A'))
#define ISLOWER(a)	(((a)<='z')&&((a)>='z'))
#define TOLOWER(a)	((ISLOWER(a))?(a):((a) | 0x20))

static char _hexdigits[] = "0123456789ABCDEF";

#define ISHEX(x) ((((x)>='0')&&((x)<='9'))||(((x)>='a')&&((x)<='f'))||(((x)>='A')&&((x)<='F')))
#define HEXVALUE(x) (((x)<='9')?((x)-'0'):(((x)<='F')?((x)-'A'):(((x)<='f')?((x)-'a'):0)))
#define HEXDIGIT(x) (_hexdigits[((unsigned)x) & 0xF])

Interface defaultInterface = NULL;

void set_default_interface (Interface i)
{
  defaultInterface = i;
}

char *pprint_name(Name n)
{
  char buf[1000];

  sprintf (buf, "Pprint:%s", defaultInterface == NULL ? "" : name_base_name(defaultInterface->name));
  return (name_lang_name(n, buf));
}

void set_pprint_name(Name n, char *name)
{
  char buf[1000];

  sprintf (buf, "Pprint:%s", defaultInterface == NULL ? "" : name_base_name(defaultInterface->name));
  name_set_lang_name(n, buf, name);
}

#define ISBADCHAR(x,q)	(((x)<' ')||((x)>'~')||(strchr(badchars, (x)) != NULL)||((!(q))&&(x)==' '))

static void do_Pprint_fixups (string buf, boolean quoted)
{
  string p, q, newbuf;
  int l, tmp;
  static char *badchars = "\"#";

  for (p = buf;  *p != EOS;  p++)
    if (ISBADCHAR(*p, quoted))
      break;
  if ((*p != EOS) || (NOT quoted AND iluparser_IsKeyword(buf)))
    {
      newbuf = (string) malloc(l = (strlen(buf) * 4));
      if (NOT quoted)
	newbuf[0] = '"';
      for (p = newbuf + ((NOT quoted) ? 1 : 0), q = buf;  *q != EOS;  q++)
	{
	  if ((p - newbuf) < (l - 3))
	    {
	      tmp = p - newbuf;
	      newbuf = (string) realloc(newbuf, l * 2);
	      p = newbuf + tmp;
	    }
	  if (ISBADCHAR(*q, TRUE))
	    {
	      *p++ = '#';
	      *p++ = HEXDIGIT(*q >> 4);
	      *p++ = HEXDIGIT(*q);
	    }
	  else
	    *p++ = *q;
	}
      if (NOT quoted)
	*p++ = '"';
      *p = EOS;
      strcpy (buf, newbuf);
      free(newbuf);
    }
}

string pprint_interface_name (Interface i)
{
  string n;

  if ((n = pprint_name(i->name)) == NULL)
    {
      char buf[500];
      sprintf (buf, "%s", name_base_name(i->name));
      do_Pprint_fixups (buf, FALSE);
      set_pprint_name (i->name, buf);
      n = pprint_name(i->name);
    }
  return (n);
}

static char * pprintTypeName (Type type)
{
  if (type->builtIn)
    {
      switch (type_ur_kind(type))
	{
	case byte_Type:
	  return "BYTE";
	case boolean_Type:
	  return "BOOLEAN";
	case shortcharacter_Type:
	  return "SHORT CHARACTER";
	case character_Type:
	  return "CHARACTER";
	case shortcardinal_Type:
	  return "SHORT CARDINAL";
	case cardinal_Type:
	  return "CARDINAL";
	case longcardinal_Type:
	  return "LONG CARDINAL";
	case shortinteger_Type:
	  return "SHORT INTEGER";
	case integer_Type:
	  return "INTEGER";
	case longinteger_Type:
	  return "LONG INTEGER";
	case shortreal_Type:
	  return "SHORT REAL";
	case real_Type:
	  return "REAL";
	case longreal_Type:
	  return "LONG REAL";
	default:
	  fprintf (stderr, "Bad primitive type %s\n", type_name(type));
	  exit(0);
	}
    }
  return NULL;
}

string pprint_type_name (Type t)
{
  string n;

  if ((n = pprint_name(t->name)) == NULL)
    {
      char buf1[500], buf2[500];
      strcpy (buf1, name_base_name(t->name));
      if (t->builtIn)
	strcpy (buf2, pprintTypeName(t));
      else
	{
	  if (defaultInterface != NULL && t->interface == defaultInterface && t->importInterfaceName == NULL)
	    strcpy (buf2, buf1);
	  else
	    sprintf (buf2, "%s.%s",
		     t->builtIn ? "ilu" :
		     ((t->importInterfaceName == NULL) ? pprint_interface_name(t->interface) : t->importInterfaceName),
		     buf1);
	  do_Pprint_fixups (buf2, FALSE);
	}
      set_pprint_name (t->name, buf2);
      n = pprint_name(t->name);
    }
  return (n);
}

string pprint_argument_name (Argument a)
{
  string n;

  if ((n = pprint_name(a->name)) == NULL)
    {
      char buf[500];
      sprintf (buf, "%s", name_base_name(a->name));
      do_Pprint_fixups (buf, FALSE);
      set_pprint_name (a->name, buf);
      n = pprint_name(a->name);
    }
  return (n);
}

string pprint_procedure_name (Procedure p)
{
  string n;

  if ((n = pprint_name(p->name)) == NULL)
    {
      char buf1[500], buf2[500];
      strcpy (buf1, name_base_name(p->name));
      do_Pprint_fixups (buf1, FALSE);
      if (defaultInterface != NULL && p->interface == defaultInterface && p->object->importInterfaceName == NULL)
	strcpy (buf2, buf1);
      else
	sprintf (buf2, "%s.%s", pprint_interface_name(p->object->interface), buf1);
      set_pprint_name (p->name, buf2);
      n = pprint_name(p->name);
    }
  return (n);
}

string pprint_exception_name (Exception e)
{
  string n;

  if ((n = pprint_name(e->name)) == NULL)
    {
      char buf1[500], buf2[500];
      strcpy (buf1, name_base_name(e->name));
      if (e->builtIn ||
	  defaultInterface != NULL && e->interface == defaultInterface && e->importInterfaceName == NULL)
	strcpy (buf2, buf1);
      else
	sprintf (buf2, "%s.%s",
		 e->importInterfaceName == NULL ? pprint_interface_name(e->interface) : e->importInterfaceName,
		 buf1);
      do_Pprint_fixups (buf2, FALSE);
      set_pprint_name (e->name, buf2);
      n = pprint_name(e->name);
    }
  return (n);
}

string pprint_constant_name (Constant c)
{
  string n;

  if ((n = pprint_name(c->name)) == NULL)
    {
      char buf1[500], buf2[500];
      strcpy (buf1, name_base_name(c->name));
      if (defaultInterface != NULL && c->interface == defaultInterface && c->importInterfaceName == NULL)
	strcpy (buf2, buf1);
      else
	sprintf (buf2, "%s:%s",
		 c->importInterfaceName == NULL ? pprint_interface_name(c->interface) : c->importInterfaceName,
		 buf1);
      do_Pprint_fixups (buf2, FALSE);
      set_pprint_name (c->name, buf2);
      n = pprint_name(c->name);
    }
  return (n);
}

string pprint_simple_name (Name name)
{
  string n;

  if ((n = name_lang_name(name, "simple")) == NULL)
    {
      char buf[2000];
      strcpy (buf, name_base_name(name));
      do_Pprint_fixups (buf, FALSE);
      name_set_lang_name (name, "simple", buf);
      n = name_lang_name(name, "simple");
    }
  return (n);
}

string pprint_string (string s, boolean quoted)
{
  char buf[1000];

  if (s == NULL)
    return (NULL);

  strcpy (buf, s);
  do_Pprint_fixups (buf, quoted);
  return (ilu_strdup(buf));
}
