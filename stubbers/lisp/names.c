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
$Id: names.c,v 1.18 1999/08/03 01:50:09 janssen Exp $
*/

#include "lisp.h"

#define HYPHEN		'-'
#define UNDERSCORE	'_'
#define EOS		'\0'
#define COLON		':'
#define PERIOD		'.'

#define ISUPPER(a)	(((a)<='Z')&&((a)>='A'))
#define ISLOWER(a)	(((a)<='z')&&((a)>='z'))
#define TOLOWER(a)	((ISLOWER(a))?(a):((a) | 0x20))

Interface defaultInterface = NULL;

void set_default_interface (Interface i)
{
  defaultInterface = i;
}

char *lisp_name(Name n)
{
  char buf[1000];

  sprintf (buf, "Lisp:%s", defaultInterface == NULL ? "" : name_base_name(defaultInterface->name));
  return (name_lang_name(n, buf));
}

void set_lisp_name(Name n, char *name)
{
  char buf[1000];

  sprintf (buf, "Lisp:%s", defaultInterface == NULL ? "" : name_base_name(defaultInterface->name));
  name_set_lang_name(n, buf, name);
}

char *old_lisp_name(Name n)
{
  char buf[1000];

  sprintf (buf, "OldLisp:%s", defaultInterface == NULL ? "" : name_base_name(defaultInterface->name));
  return (name_lang_name(n, buf));
}

void set_old_lisp_name(Name n, char *name)
{
  char buf[1000];

  sprintf (buf, "OldLisp:%s", defaultInterface == NULL ? "" : name_base_name(defaultInterface->name));
  name_set_lang_name(n, buf, name);
}

static void do_Lisp_fixups (string buf)
{
  string p;
  boolean lastupper = FALSE;

  for (p = buf, lastupper = FALSE;  *p != '\0';  p += 1)
    {
      if (ISUPPER(*p))
	{
	  if (p != buf && (!lastupper))
	    {
	      memmove (p+1, p, strlen(p) + 2);
	      *p++ = HYPHEN;
	    }
	  lastupper = TRUE;
	  *p = TOLOWER(*p);
	}
      else if ((*p == COLON) OR (*p == HYPHEN) OR (*p == PERIOD))
	lastupper = TRUE;
      else
	lastupper = FALSE;
    }
  *p = '\0';
}

string lisp_interface_name (Interface i)
{
  string n;

  if ((n = lisp_name(i->name)) == NULL)
    {
      char buf[500];
      sprintf (buf, "%s", name_base_name(i->name));
      do_Lisp_fixups (buf);
      set_lisp_name (i->name, buf);
      n = lisp_name(i->name);
    }
  return (n);
}

string lisp_type_name (Type t)
{
  string n;

  if ((n = lisp_name(t->name)) == NULL)
    {
      char buf1[500], buf2[500];
      strcpy (buf1, name_base_name(t->name));
      if (defaultInterface != NULL && t->interface == defaultInterface && t->importInterfaceName == NULL)
	strcpy (buf2, buf1);
      else
	sprintf (buf2, "%s:%s",
		 t->builtIn ? "ilu" :
		 ((t->importInterfaceName == NULL) ? lisp_interface_name(t->interface) : t->importInterfaceName),
		 buf1);
      do_Lisp_fixups (buf2);
      set_lisp_name (t->name, buf2);
      n = lisp_name(t->name);
    }
  return (n);
}

string lisp_argument_name (Argument a)
{
  string n;

  if ((n = lisp_name(a->name)) == NULL)
    {
      char buf[500];
      sprintf (buf, "%s", name_base_name(a->name));
      do_Lisp_fixups (buf);
      set_lisp_name (a->name, buf);
      n = lisp_name(a->name);
    }
  return (n);
}


#define PREFIX_GET      "ilu--prefix-idl-attribute--get-"
#define PREFIX_SET      "ilu--prefix-idl-attribute--set-"
#define PREFIX_LEN      (sizeof(PREFIX_SET)-1)
#define SETF_PREFIX     "(setf "
#define SETF_PREFIX_LEN (sizeof(SETF_PREFIX)-1)


string old_lisp_procedure_name (Procedure p)
{
  string n;

  if ((n = old_lisp_name(p->name)) == NULL)
    {
      char buf1[500], buf2[500];
      strcpy (buf1, name_base_name(p->name));
      do_Lisp_fixups (buf1);
      if (strncmp(buf1, PREFIX_GET, PREFIX_LEN) == 0)
	 memmove(buf1, buf1 + PREFIX_LEN, strlen(buf1 + PREFIX_LEN) + 1);
      if (strncmp(buf1, PREFIX_SET, PREFIX_LEN) == 0) {
	 size_t len = strlen(buf1 + PREFIX_LEN);
	 memcpy(buf1, SETF_PREFIX, SETF_PREFIX_LEN);
         memmove(buf1 + SETF_PREFIX_LEN, buf1 + PREFIX_LEN, len);
	 memcpy(buf1 + SETF_PREFIX_LEN + len, ")", sizeof(")"));
      }
      if (defaultInterface != NULL && p->interface == defaultInterface && p->object->importInterfaceName == NULL)
	strcpy (buf2, buf1);
      else
	sprintf (buf2, "%s:%s", lisp_interface_name(p->object->interface), buf1);
      set_old_lisp_name (p->name, buf2);
      n = old_lisp_name(p->name);
    }
  return (n);
}

string lisp_procedure_name (Procedure p)
{
  string n;
  int l;

  if ((n = lisp_name(p->name)) == NULL)
    {
      char buf1[1000], buf2[1000];
      strcpy (buf1, type_name(p->object));
      do_Lisp_fixups (buf1);
      strcat (buf1, ".");
      l = strlen(buf1);
      strcat (buf1, name_base_name(p->name));
      do_Lisp_fixups (buf1 + l);
      if (strncmp(buf1 + l, PREFIX_GET, PREFIX_LEN) == 0) {
	memmove(buf1 + l, buf1 + l + PREFIX_LEN, strlen(buf1 +l + PREFIX_LEN) + 1);
      } else if (strncmp(buf1 + l, PREFIX_SET, PREFIX_LEN) == 0) {
	 size_t len = strlen(buf1 + l + PREFIX_LEN);
	 strcpy(buf2, SETF_PREFIX);
	 strncpy (buf2 + SETF_PREFIX_LEN, buf1, l);
	 buf2[SETF_PREFIX_LEN + l] = '\0';
	 strcpy (buf2 + SETF_PREFIX_LEN + l, buf1 + l + PREFIX_LEN);
	 strcat (buf2, ")");
	 strcpy (buf1, buf2);
      }
      if (defaultInterface != NULL && p->interface == defaultInterface && p->object->importInterfaceName == NULL)
	strcpy (buf2, buf1);
      else
	sprintf (buf2, "%s:%s", lisp_interface_name(p->object->interface), buf1);
      set_lisp_name (p->name, buf2);
      n = lisp_name(p->name);
    }
  return (n);
}

string lisp_exception_name (Exception e)
{
  string n;

  if ((n = lisp_name(e->name)) == NULL)
    {
      char buf1[500], buf2[500];
      strcpy (buf1, name_base_name(e->name));
      if (defaultInterface != NULL && e->interface == defaultInterface && e->importInterfaceName == NULL)
	strcpy (buf2, buf1);
      else
	sprintf (buf2, "%s:%s",
		 e->importInterfaceName == NULL ? lisp_interface_name(e->interface) : e->importInterfaceName,
		 buf1);
      do_Lisp_fixups (buf2);
      set_lisp_name (e->name, buf2);
      n = lisp_name(e->name);
    }
  return (n);
}

void PrefixedExceptionName (Exception e, Context context)
{
   char buf1[500], buf2[500];
   strcpy (buf1, name_base_name(e->name));
   if (defaultInterface != NULL && e->interface == defaultInterface
       && e->importInterfaceName == NULL)
      sprintf (buf2, "%s", buf1);
   else
      sprintf (buf2, "%s:%s",
	       e->importInterfaceName == NULL
	       ? lisp_interface_name(e->interface) : e->importInterfaceName,
	       buf1);
   do_Lisp_fixups (buf2);
   fprintf (context->file, " %s", buf2);
}

string lisp_constant_name (Constant c)
{
  string n;

  if ((n = lisp_name(c->name)) == NULL)
    {
      char buf1[500], buf2[500];
      strcpy (buf1, name_base_name(c->name));
      if (defaultInterface != NULL && c->interface == defaultInterface && c->importInterfaceName == NULL)
	strcpy (buf2, buf1);
      else
	sprintf (buf2, "%s:%s",
		 c->importInterfaceName == NULL ? lisp_interface_name(c->interface) : c->importInterfaceName,
		 buf1);
      do_Lisp_fixups (buf2);
      set_lisp_name (c->name, buf2);
      n = lisp_name(c->name);
    }
  return (n);
}

string lisp_simple_name (Name name)
{
  string n;

  if ((n = name_lang_name(name, "simple")) == NULL)
    {
      char buf[2000];
      strcpy (buf, name_base_name(name));
      do_Lisp_fixups (buf);
      name_set_lang_name (name, "simple", buf);
      n = name_lang_name(name, "simple");
    }
  return (n);
}

string native_lisp_type (Type type)
{
  string n;

  if ((n = name_lang_name(type->name, "lisp-native")) == NULL)
    {
      enum PrimitiveTypes t = type_basic_type(type);
      string p;

      switch (t)
	{
	case byte_Type:
	  p = "(cl:unsigned-byte 8)";
	  break;

	case boolean_Type:
	  p = "t";
	  break;

	case character_Type:
	  p = "cl:character";
	  break;

	case shortcharacter_Type:
	  p = "cl:standard-char";
	  break;

	case shortinteger_Type:
	  p = "cl:fixnum";
	  break;

	case integer_Type:
	  p = "cl:fixnum";
	  break;

	case shortcardinal_Type:
	  p = "(cl:unsigned-byte 16)";
	  break;

	case cardinal_Type:
	  p = "(cl:unsigned-byte 32)";
	  break;

	case real_Type:
	  p = "cl:double";
	  break;

	case pickle_Type:
	  p = "ilu:pickle";
	  break;

	case shortreal_Type:
	  p = "cl:float";
	  break;

	case object_Type:
	case union_Type:
	case sequence_Type:
	case record_Type:
	case array_Type:
	case enumeration_Type:
	  p = lisp_type_name(type);
	  break;

	default:
	  p = NULL;
	};

      if (p != NULL)
	{
	  char buf[1000];
	  strcpy (buf, p);
	  do_Lisp_fixups (buf);
	  name_set_lang_name (type->name, "lisp-native", buf);
	}
      n = name_lang_name(type->name, "lisp-native");
    }
  return (n);
}

string lisp_string (string s)
{
  char buf[1000];

  if (s == NULL)
    return (NULL);

  strcpy (buf, s);
  do_Lisp_fixups (buf);
  return (ilu_strdup(buf));
}

static void do_C_fixups (string buf)
{
  char *p;

  /* replace hyphen with underscore */

  for (p = buf;  *p != '\0';  p++)
    if (*p == HYPHEN)
      *p = UNDERSCORE;
}

string c_interface_name (Interface i)
{
  string n;

  if ((n = name_lang_name(i->name, "C")) == NULL)
    {
      char buf[500];
      sprintf (buf, "%s", name_base_name(i->name));
      do_C_fixups (buf);
      name_set_lang_name (i->name, "C", buf);
      n = name_lang_name(i->name, "C");
    }
  return (n);
}

string c_type_name (Type t)
{
  string n;

  if ((n = name_lang_name(t->name, "C")) == NULL)
    {
      char buf[500];
      sprintf (buf, "%s%s%s",
	       t->builtIn ? "ilu" : c_interface_name(t->interface),
	       t->builtIn ? "_" : "_T_",
	       name_base_name(t->name));
      do_C_fixups (buf);
      name_set_lang_name (t->name, "C", buf);
      n = name_lang_name(t->name, "C");
    }
  return (n);
}
