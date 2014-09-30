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
$Id: names.c,v 1.23 1999/08/03 01:50:58 janssen Exp $
*/
/* Last edited by Mike Spreitzer December 5, 1995 4:24 pm PST */

#include "cplusplus.h"

#define HYPHEN		'-'
#define UNDERSCORE	'_'
#define EOS		'\0'

string cplusplus_simple_name(Name name);
string cplusplus_string(string s);

enum syntype { synonym_Method, synonym_Type, synonym_Exception, synonym_Constant, synonym_Interface, synonym_Invalid };

struct synonym {
  enum syntype type;
  string realname;
  string classname;
};

list Synonyms = NULL;

boolean ReadSynonyms (string filename)
{
  enum syntype type;
  FILE *f;
  int l;
  char buf[2000];
  string new;
  char *blankpos;
  string blank;
  struct synonym *s;
  static char MethodC[] = "method";
  static char ExceptionC[] = "exception";
  static char TypeC[] = "type";
  static char InterfaceC[] = "interface";
  static char ConstantC[] = "constant";

  if ((f = fopen(filename, "r")) == NULL)
    return (FALSE);
  if (Synonyms == NULL)
    Synonyms = new_list();
  else
    list_clear (Synonyms, FALSE);
  while (fgets(buf, sizeof(buf), f) != NULL)
    {
      buf[(l = strlen(buf)) - 1] = '\0';
      if (l == 1)	/* blank line */
	continue;
      if (buf[0] == '#')	/* comment */
	continue;
      type = synonym_Invalid;
      blankpos = strchr(buf, ' ');
      if (blankpos != NULL)
	{
	  *blankpos = '\0';
	  if (strstr(MethodC, buf) == MethodC)
	    type = synonym_Method;
	  else if (strstr(TypeC, buf) == TypeC)
	    type = synonym_Type;
	  else if (strstr(ConstantC, buf) == ConstantC)
	    type = synonym_Constant;
	  else if (strstr(ExceptionC, buf) == ExceptionC)
	    type = synonym_Exception;
	  else if (strstr(InterfaceC, buf) == InterfaceC)
	    type = synonym_Interface;
	}
      if (type == synonym_Invalid
	  || (blank = strchr(blankpos + 1, ' ')) == NULL)
	{
	  fprintf (stderr, "Warning:  Synonyms file %s contains bad line <%s>.  Ignoring it.\n", filename, buf);
	  continue;
	}
      new = (string ) iluparser_Malloc(l - (blankpos + 1 - buf));
      strncpy (new, blankpos + 1, l - (blankpos + 1 - buf));
      blank = new + (blank - (blankpos + 1));
      *blank = '\0';
      s = (struct synonym *) iluparser_Malloc(sizeof(struct synonym));
      s->type = type;
      s->realname = new;
      s->classname = blank + 1;
      list_insert (Synonyms, s);
    }
  fclose (f);
  return (TRUE);
}

struct synfind {
  enum syntype type;
  string name;
};

static boolean MatchSynonym (struct synonym *s, struct synfind *name)
{
  if (s->type == name->type && strcmp(s->realname, name->name) == 0)
    return (TRUE);
  else
    return (FALSE);
}

static string FindSynonym (enum syntype type, char *interfacename, char *thingname, char *methodname)
{
  char buf[2000];
  struct synfind s;
  struct synonym *syn;

  if (Synonyms == NULL)
    return (NULL);
  sprintf (buf, (type == synonym_Interface) ? "%s"
	   : ((type == synonym_Method) ? "%s.%s.%s" : "%s.%s"),
	   interfacename, thingname, methodname);
  s.type = type;
  s.name = buf;
  syn = (struct synonym *) list_find (Synonyms, (iluparser_FindProc) MatchSynonym, &s);
  if (syn == NULL)
    return (NULL);
  else
    {
      return (syn->classname);
    }
}

static char AttrPrefix[] = "ilu--prefix-idlAttribute-";

static void do_idl_attribute_fixup (string buf)
{
  if (buf[0] == AttrPrefix[0] && strncmp(buf, AttrPrefix, strlen(AttrPrefix)) == 0) {
    /* Strip off prefix. */

    char *src = buf + strlen(AttrPrefix);
    char *dst = buf;

    while ((*dst++ = *src++))
      continue;
  }
}

static void do_Cplusplus_fixups (string buf)
{
  char *p;

  /* replace hyphen with underscore */

  for (p = buf;  *p != '\0';  p++)
    if (*p == '-')
      *p = '_';
}

string cplusplus_interface_name (Interface i)
{
  string n;

  if ((n = name_lang_name(i->name, "C++")) == NULL)
    {
      char buf[500];
      if ((n = FindSynonym (synonym_Interface, name_base_name(i->name), NULL, NULL)) == NULL)
	{
	  sprintf (buf, "%s", name_base_name(i->name));
	  do_Cplusplus_fixups (buf);
	  n = buf;
	}
      name_set_lang_name (i->name, "C++", n);
      n = name_lang_name(i->name, "C++");
    }
  return (n);
}


static char *LocalName(Type type)
{
  if (NOT type->builtIn)
    return NULL;
  else
    {
      switch (type_basic_type(type))
	{
	case integer_Type:
	  return ("ilu_Integer");

	case cardinal_Type:
	  return ("ilu_Cardinal");

	case shortinteger_Type:
	  return ("ilu_ShortInteger");

	case shortcardinal_Type:
	  return ("ilu_ShortCardinal");

	case longinteger_Type:
	  return ("ilu_LongInteger");

	case longcardinal_Type:
	  return ("ilu_LongCardinal");

	case real_Type:
	  return ("ilu_Real");

	case shortreal_Type:
	  return ("ilu_ShortReal");

	case longreal_Type:
	  return ("ilu_LongReal");

	case boolean_Type:
	  return ("ilu_Boolean");

	case byte_Type:
	  return ("ilu_Byte");

	case character_Type:
	  return ("ilu_Character");

	case shortcharacter_Type:
	  return ("ilu_ShortCharacter");

	default:
	  fprintf (stderr, "Error:  No standard C++ name for type %s.\n", type_name(type));
	  return (NULL);
	}
    }
}

string cplusplus_type_name (Type t)
{
  string n;

  if ((n = name_lang_name(t->name, "C++")) == NULL)
    {
      char buf[500];
      if ((n = FindSynonym (synonym_Type, (t->importInterfaceName == NULL) ? interface_name(t->interface) : t->importInterfaceName, type_name(t), NULL)) == NULL)
	{
	  if (t->importInterfaceName != NULL)
	    sprintf (buf, "%s_T_%s", cplusplus_string(t->importInterfaceName), name_lang_name(t->name, "import"));
	  else if (t->builtIn)
	    strcpy (buf, LocalName(t));
	  else
	    sprintf (buf, "%s_T_%s", cplusplus_interface_name(t->interface), name_base_name(t->name));
	  do_Cplusplus_fixups (buf);
	  n = buf;
	}
      name_set_lang_name (t->name, "C++", n);
      n = name_lang_name(t->name, "C++");
    }
  return (n);
}

string cplusplus_return_type(Type type)
{
  string n;
  Type ut = ur_type(type);

  if (type == NULL)
    return ("void");
  
  if ((n = name_lang_name(type->name, "C++return")) == NULL)
    {
      char buf[1000];

      /* objects */
      if (type_basic_type(type) == object_Type)
	sprintf (buf, "class %s *", cplusplus_type_name(type));

      /* object aliases, records, unions, arrays */
      else if (type_basic_type(ut) == object_Type OR
	       TypeIsNonObjectStruct(ut) OR
	       type_basic_type(ut) == array_Type)
	sprintf (buf, "%s *", cplusplus_type_name(type));

      /* everything else */
      else
	sprintf (buf, "%s", cplusplus_type_name(type));

      name_set_lang_name(type->name, "C++return", buf);
      n = name_lang_name(type->name, "C++return");
    }
  return (n);
}

string cplusplus_parameter_type(Type type, ArgDirection passingMode)
{
  string n;
  char *tag;
  Type ut;

  if (type == NULL)
    return ("void");
  
  ut = ur_type(type);

  switch (passingMode) {
  case In:
    tag = "C++parm-In";
    break;
  case Out:
    tag = "C++parm-Out";
    break;
  case InOut:
    tag = "C++parm-InOut";
    break;
  default:
    tag = "C++parm-In";
    fprintf(stderr,
	    "cplusplus_parameter_type given bogus passingMode!\n");
  }

  if ((n = name_lang_name(type->name, tag)) == NULL)
    {
      char buf[1000];
      enum PrimitiveTypes ept;

      /* objects */
      if (type_basic_type(type) == object_Type)
	sprintf (buf, "class %s *%s", cplusplus_type_name(type), passingMode == In ? "" : "*");

      /* records, unions */
      else if (type_ur_kind(type) == object_Type OR TypeIsNonObjectStruct(ut))
	sprintf (buf, "%s *", cplusplus_type_name(type));

      /* arrays */
      else if (type_basic_type(ut) == array_Type)
	sprintf (buf, "%s", cplusplus_type_name(type));

      /* non-string sequences */
      else if (type_basic_type(ut) == sequence_Type && (ept = type_basic_type(type_description(ut)->structuredDes.sequence.type)) != shortcharacter_Type && ept != byte_Type)
	sprintf (buf, "%s", cplusplus_type_name(type));

      /* everything else */
      else
	sprintf (buf, "%s%s", cplusplus_type_name(type), passingMode == In ? "" : " *");

      name_set_lang_name(type->name, tag, buf);
      n = name_lang_name(type->name, tag);
    }
  return (n);
}

static boolean find_reserved (char *name);

string cplusplus_argument_name(Argument a)
{
  string n;

  if ((n = name_lang_name(a->name, "C++")) == NULL)
    {
      char buf[500], buf2[500];
      sprintf (buf, "%s", name_base_name(a->name));
      n = buf;
      do_Cplusplus_fixups (buf);
      if (find_reserved(n))
	{
	  sprintf (buf2, "ilu%s", n);
	  fprintf (stderr, "Warning:  argument \"%s\" will be called \"%s\" in the C++ interface, as simple \"%s\" conflicts with a built-in C++ method of the same name.\n",
		   name_base_name(a->name), buf2, n);
	  n = buf2;
	}
      name_set_lang_name (a->name, "C++", n);
      n = name_lang_name(a->name, "C++");
    }
  return (n);
}

string class_procedure_name (Procedure p)
{
  string n;

  if ((n = name_lang_name(p->name, "C++")) == NULL)
    {
      char buf[500], buf2[500];
      if ((n = FindSynonym (synonym_Method, (p->object->importInterfaceName == NULL) ? interface_name(p->object->interface) : p->object->importInterfaceName, type_name(p->object), name_base_name(p->name))) == NULL)
	{
	  sprintf (buf, "%s", name_base_name(p->name));
	  do_idl_attribute_fixup (buf);
	  do_Cplusplus_fixups (buf);
	  n = buf;
	}
      if (find_reserved(n))
	{
	  sprintf (buf2, "ilu%s", n);
	  fprintf (stderr, "Warning:  ILU method \"%s:%s\" will be called \"%s\" in the CLASS interface, as simple \"%s\" conflicts with a built-in CLASS method of the same name.\n",
		   type_name(p->object), n, buf2, n);
	  n = buf2;
	}
      name_set_lang_name (p->name, "C++", n);
      n = name_lang_name(p->name, "C++");
    }
  return (n);
}

static boolean find_reserved (char *name)
{
  char **p;

  static char *reserved[] = {
    "class", "private", "new", "delete", "public", "volatile", "const", "inline", "virtual", "overload",
    NULL };

  for (p = reserved;  *p != NULL;  p += 1)
    if (strcmp (*p, name) == 0)
      return (TRUE);
  return (FALSE);
}

string cplusplus_exception_name (Exception e)
{
  string n;

  if ((n = name_lang_name(e->name, "C++")) == NULL)
    {
      char buf[500];
      if ((n = FindSynonym (synonym_Exception, (e->importInterfaceName == NULL) ? interface_name(e->interface) : e->importInterfaceName, name_base_name(e->name), NULL)) == NULL)
	{			    
	  if (e->importInterfaceName != NULL)
	    sprintf (buf, "%s_E_%s", cplusplus_interface_name(e->interface),
		     name_lang_name(e->name, "import"));
	  else
	    sprintf (buf, "%s_E_%s", cplusplus_interface_name(e->interface),
		     name_base_name(e->name));
	  do_Cplusplus_fixups (buf);
	  n = buf;
	}
      name_set_lang_name (e->name, "C++", n);
      n = name_lang_name(e->name, "C++");
    }
  return (n);
}

string cplusplus_constant_name (Constant e)
{
  string n;

  if ((n = name_lang_name(e->name, "C++")) == NULL)
    {
      char buf[500];
      if ((n = FindSynonym (synonym_Constant,
			    ((e->importInterfaceName == NULL) ?
			     interface_name(e->interface) :
			     e->importInterfaceName),
			    name_base_name(e->name), NULL)) == NULL)
	{			    
 	  if (e->importInterfaceName != NULL)
 	    sprintf (buf, "%s_C_%s", cplusplus_string(e->importInterfaceName), name_lang_name(e->name, "import"));
 	  else
 	    sprintf (buf, "%s_C_%s", cplusplus_interface_name(e->interface), name_base_name(e->name));
	  do_Cplusplus_fixups (buf);
	  n = buf;
	}
      name_set_lang_name (e->name, "C++", n);
      n = name_lang_name(e->name, "C++");
    }
  return (n);
}

string cplusplus_simple_name (Name name)
{
  string n;

  if ((n = name_lang_name(name, "simple")) == NULL)
    {
      n = ilu_strdup (name_base_name(name));
      do_idl_attribute_fixup (n);
      do_Cplusplus_fixups (n);
      name_set_lang_name (name, "simple", n);
    }
  return (n);
}

string cplusplus_string (string s)
{
  string new = ilu_strdup(s);
  do_Cplusplus_fixups (new);
  return (new);
}
