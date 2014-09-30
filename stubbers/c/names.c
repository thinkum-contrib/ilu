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
/* $Id: names.c,v 1.37 1999/08/03 01:50:29 janssen Exp $ */
/* Last edited by Mike Spreitzer April 2, 1996 3:18 pm PST */

#include "cstubber.h"

#define HYPHEN '-'
#define UNDERSCORE '_'
#define EOS '\0'

#define OMGIDLAttributePrefix "ilu--prefix-idlAttribute-"
#define OMGIDLExceptionNamePrefix "ilu--prefix-idlExceptionType-"
#define OMGIDLExceptionNamePrefixLen strlen(OMGIDLExceptionNamePrefix)

enum syntype {
  synonym_Method, synonym_Type, synonym_Exception, synonym_Constant, synonym_Interface, synonym_Invalid
  };

struct synonym {
  enum syntype type;
  string realname;
  string classname;
};

list Synonyms = NULL;

static boolean find_reserved (name)
     char *name;
{
  char **p;

  static char *reserved[ ] = {
    "auto", "break", "case", "char", "const", "continue", "default", "do",
    "double", "else", "enum", "extern", "float", "for", "goto", "if",
    "int", "long", "register", "return", "short", "signed", "sizeof", "static",
    "struct", "switch", "typedef", "union", "unsigned", "void", "volatile", "while",
    NULL };

  for (p = reserved; *p != NULL; p += 1)
    if (strcmp (*p, name) == 0)
      return (True);
  return (False);
}

boolean ReadSynonyms (string filename)
{
  enum syntype type;
  FILE *f;
  int l;
  char buf[ 2000 ];
  string new;
  char *blankpos;
  string blank;
  struct synonym *s;
  static char MethodC[ ] = "method";
  static char ExceptionC[ ] = "exception";
  static char TypeC[ ] = "type";
  static char InterfaceC[ ] = "interface";
  static char ConstantC[ ] = "constant";

  if ((f = fopen (filename, "r")) == NULL)
    return (False);
  if (Synonyms == NULL)
    Synonyms = new_list ();
  else
    list_clear (Synonyms, False);
  while (fgets (buf, sizeof (buf), f) != NULL) {
    buf[ (l = strlen (buf)) - 1 ] = '\0';
    if (l == 1) /* blank line */
      continue;
    if (buf[ 0 ] == '#') /* comment */
      continue;
    type = synonym_Invalid;
    blankpos = strchr (buf, ' ');
    if (blankpos != NULL) {
      *blankpos = '\0';
      if (strstr (MethodC, buf) == MethodC)
	type = synonym_Method;
      else if (strstr (TypeC, buf) == TypeC)
	type = synonym_Type;
      else if (strstr (ConstantC, buf) == ConstantC)
	type = synonym_Constant;
      else if (strstr (ExceptionC, buf) == ExceptionC)
	type = synonym_Exception;
      else if (strstr (InterfaceC, buf) == InterfaceC)
	type = synonym_Interface;
    }
    if (type == synonym_Invalid
	|| (blank = strchr (blankpos + 1, ' ')) == NULL) {
      error ( "Warning: Synonyms file %s contains bad line <%s>. Ignoring it.\n", filename, buf);
      continue;
    }
    new = CAlloc (l - (blankpos + 1 - buf), char);
    strncpy (new, blankpos + 1, l - (blankpos + 1 - buf));
    blank = new + (blank - (blankpos + 1));
    *blank = '\0';
    s = CAlloc (1, struct synonym);
    s -> type = type;
    s -> realname = new;
    s -> classname = blank + 1;
    list_insert (Synonyms, s);
  }
  fclose (f);
  return (True);
}

struct synfind {
  enum syntype type;
  string name;
};

static boolean MatchSynonym (struct synonym *s, struct synfind *name)
{
  if (s -> type == name -> type && strcmp (s -> realname, name -> name) == 0)
    return (True);
  else
    return (False);
}

static string FindSynonym (enum syntype type, char *interfacename, char *thingname, char *methodname)
{
  char buf[2000];
  struct synfind s;
  struct synonym *syn;

  if (Synonyms == NULL)
    return (NULL);
  sprintf ( buf, (type == synonym_Interface) ? "%s" : ((type == synonym_Method) ? "%s.%s.%s" : "%s.%s"), interfacename, thingname, methodname);
  s.type = type;
  s.name = buf;
  syn = (struct synonym *) list_find (Synonyms, (boolean (*) (void *, void *)) MatchSynonym, &s);
  if (syn == NULL)
    return (NULL);
  else {
    return (syn -> classname);
  }
}

static void do_C_fixups (buf)
     string buf;
{
  char *p;

  /* replace hyphen with underscore */

  for (p = buf; *p != '\0'; p++) {
    if (strncmp (p, OMGIDLAttributePrefix, strlen (OMGIDLAttributePrefix)) == 0) {
      strcpy (p, p + strlen (OMGIDLAttributePrefix));
    }
    if (*p == '-')
      *p = '_';
  }
}

string c_interface_name (i)
     Interface i;
{
  string n;

  if ((n = name_lang_name (i -> name, "C")) == NULL) {
    char buf[ 500 ];
    if ((n = FindSynonym (synonym_Interface, name_base_name (i->name), NULL, NULL)) == NULL) {
      sprintf (buf, "%s", name_base_name (i -> name));
      do_C_fixups (buf);
      n = buf;
    }
    name_set_lang_name (i -> name, "C", n);
    n = name_lang_name (i -> name, "C");
  }
  return (n);
}

static void get_builtin_name (char *buf, Type t)
{
  char *p;
  switch (type_ur_kind(t))
    {
    case integer_Type:
      p = "CORBA_long";
      break;
    case shortinteger_Type:
      p = "CORBA_short";
      break;
    case longinteger_Type:
      p = "CORBA_long_long";
      break;
    case cardinal_Type:
      p = "CORBA_unsigned_long";
      break;
    case shortcardinal_Type:
      p = "CORBA_unsigned_short";
      break;
    case longcardinal_Type:
      p = "CORBA_unsigned_long_long";
      break;
    case character_Type:
      p = "CORBA_wchar";
      break;
    case shortcharacter_Type:
      p = "CORBA_char";
      break;
    case real_Type:
      p = "CORBA_double";
      break;
    case shortreal_Type:
      p = "CORBA_float";
      break;
    case longreal_Type:
      p = "CORBA_long_double";
      break;
    case boolean_Type:
      p = "CORBA_boolean";
      break;
    case byte_Type:
      p = "CORBA_octet";
      break;
    case pickle_Type:
      p = "CORBA_any";
      break;
    }
  strcpy (buf, p);
}

string c_type_name (t)
     Type t;
{
  string n;

  if ((n = name_lang_name (t -> name, "C")) == NULL) {
    char buf[ 500 ];

    if ((n = FindSynonym (synonym_Type, (t -> importInterfaceName == NULL) ? interface_name (t -> interface) : t -> importInterfaceName, type_name (t), NULL)) == NULL)
      {
	if (t -> importInterfaceName != NULL)
	  sprintf( buf, "%s_%s", c_string (t -> importInterfaceName), name_lang_name (t -> name, "import"));
	else if (t->builtIn)
	  get_builtin_name (buf, t);
	else {
	  char *basename = name_base_name(t->name);
	  if (strncmp(basename, OMGIDLExceptionNamePrefix, OMGIDLExceptionNamePrefixLen) == 0)
	    basename = basename + OMGIDLExceptionNamePrefixLen;
	  sprintf( buf, "%s_%s", c_interface_name (t -> interface), basename);
	}
	do_C_fixups (buf);
	n = buf;
      }
    name_set_lang_name (t -> name, "C", n);
    n = name_lang_name (t -> name, "C");
  }
  return (n);
}

string c_typecode_name (Type type)
{
  string n;

  if ((n = name_lang_name (type->name, "C-typecode")) == NULL) {
    static char buf[ 500 ];
    char *tname;

    tname = c_type_name(type);
    sprintf (buf, "TC_%s", tname);
    name_set_lang_name(type->name, "C-typecode", buf);
    n = buf;
  }
  return n;
}

string c_return_type (Type type)
{
  return c_role_type(type, role_Return, FALSE);
}

Role            adRole[3] = {role_In, role_Out, role_InOut};

string c_parameter_type(Type type, ArgDirection passing_mode)
{
  return c_role_type(type, adRole[passing_mode], FALSE);
}

string          roleTag[2][7] = {
  {"Cparm-In", "Cparm-Out", "Cparm-InOut", "Creturn", "Cparm-Exn",
   "Cval", "CinpRet"},
  {"Ctemp-In", "Ctemp-Out", "Ctemp-InOut", "Ctemp-return", "Ctemp-Exn",
   "Cval", "CinpRet"}};

string c_role_type(Type type, Role role, boolean temp)
{
  char           *tag;
  string          n;
  Type            ut;
  enum PrimitiveTypes t;
  if (type == NULL)
    return ("void");
  ut = ur_type(type);
  t = type_basic_type(ut);
  if (role >= 7) {
    tag = "Crole-Bogon";
    fprintf(stderr, "c_parameter_type: given bogus passing_mode!\n");
  } else
    tag = roleTag[temp != 0][role];
  if ((n = name_lang_name(type->name, tag)) == NULL) {
    char            buf[1000];
    if (t == array_Type) {
      Type            et;
      boolean         vbl;
      et = type_description(type)->structuredDes.array.type;
      vbl = VariableLength(et);
      if (role == role_Return || role == role_InpRet ||
	  role == role_Out && vbl && temp)
	sprintf(buf, "%s_slice *", c_type_name(type));
      else if (role == role_Out && vbl)
	sprintf(buf, "%s_slice **", c_type_name(type));
      else if (role == role_Exn)
	sprintf(buf, "%s*", c_type_name(type));
      else
	sprintf(buf, "%s", c_type_name(type));
    } else {
      int             ind = Indirectness(type, role, temp, 0);
      sprintf(buf, "%s%s%s", c_type_name(type),
	      (ind ? "*" : ""),
	      ((ind > 1) ? "*" : ""));
    }
    name_set_lang_name(type->name, tag, buf);
    n = name_lang_name(type->name, tag);
  }
  return (n);
}

int Indirectness(Type type, Role role, boolean temp, int ss)
{
  Type            ut = ur_type(type);
  TypeKind        tk = type_kind(ut);
  if (temp && role != role_Return) {
    int             i = Indirectness(type, role, FALSE, ss);
    return (i > 1);
  }
  if (role == role_Exn)
    return 1;
  if (role == role_Val)
    return 0;
  if (role == role_InpRet) {
    if (tk == array_Type)
      return ss;
/*
    if (TypeIsEitherString(ut))
      return 0;
*/
    return 1;
  }
  switch (tk) {
  case string_Type:
  case fixedpoint_Type:
  case sequence_Type:
    if (!TypeIsEitherString(ut))
      return 1 + (role == role_Out);
  case byte_Type:
  case boolean_Type:
  case character_Type:
  case shortcharacter_Type:
  case shortinteger_Type:
  case integer_Type:
  case longinteger_Type:
  case shortcardinal_Type:
  case cardinal_Type:
  case longcardinal_Type:
  case real_Type:
  case shortreal_Type:
  case longreal_Type:
  case object_Type:
  case optional_Type:
  case reference_Type:
  case enumeration_Type:
    return (role == role_Out || role == role_InOut);
  case union_Type:
  case pickle_Type:
  case record_Type:
    if (role == role_In || role == role_InOut)
      return 1;
    return (role == role_Out) + (VariableLength(ut) != 0);
  case array_Type:
    if (role == role_In || role == role_InOut ||
	role == role_Out && !VariableLength(ut))
      return 0;
    return ss + (role == role_Out);
  default:
    fprintf(stderr,
	    "names.c:Indirectness applied to unexpected kind, %d\n",
	    tk);
    return 0;
  }
}

boolean Sliced(Type type, Role role)
{
  Type            ut = ur_type(type);
  TypeKind        utk = type_kind(ut);
  return (utk == array_Type &&
	  (role == role_Return ||
	   role == role_Out && VariableLength(ut)));
}

string c_argument_name (a)
     Argument a;
{
  string n;

  if ((n = name_lang_name (a -> name, "C")) == NULL) {
    char buf[ 500 ];
    char buf2[ 500 ];

    sprintf (buf, "%s", name_base_name (a -> name));
    n = buf;
    do_C_fixups (buf);
    if (find_reserved (n)) {
      sprintf (buf2, "_%s", n);
      error( "Warning: argument \"%s\" will be called \"_%s\" in the C stubs, as the token \"%s\" conflicts with a C reserved word.\n", name_base_name (a -> name), buf2, n);
      n = buf2;
    }
    name_set_lang_name (a -> name, "C", n);
    n = name_lang_name (a -> name, "C");
  }
  return (n);
}

string c_procedure_name (p)
     Procedure p;
{
  string n;

  if ((n = name_lang_name (p -> name, "C")) == NULL) {
    char buf[ 500 ];

    if ((n = FindSynonym(synonym_Method,
			 ((p->object->importInterfaceName == NULL)
			  ? interface_name (p->object->interface)
			  : p -> object -> importInterfaceName),
			 type_name (p -> object),
			 name_base_name (p -> name))) == NULL) {
      sprintf ( buf, "%s_%s", c_type_name (p -> object), name_base_name (p -> name));
      do_C_fixups (buf);
      n = buf;
    }
    name_set_lang_name (p -> name, "C", n);
    n = name_lang_name (p -> name, "C");
  }
  return (n);
}

string c_exception_name (e1)
     Exception e1;
{
  string n;
  Exception e = e1;

  while (e->import != NULL)
    e = e->import;
  if ((n = name_lang_name (e -> name, "C")) == NULL) {
    char buf[ 500 ];

    if ((n = FindSynonym (synonym_Exception, (e -> importInterfaceName == NULL) ? interface_name (e -> interface) : e->importInterfaceName, name_base_name (e -> name), NULL)) == NULL) {
      sprintf( buf, "%s_%s", c_interface_name (e -> interface), name_base_name (e -> name));
      do_C_fixups (buf);
      n = buf;
    }
    name_set_lang_name (e -> name, "C", n);
    n = name_lang_name (e -> name, "C");
  }
  return (n);
}

string c_constant_name (e)
     Constant e;
{
  string n;

  if ((n = name_lang_name (e -> name, "C")) == NULL) {
    char buf[ 500 ];

    if ((n = FindSynonym(synonym_Constant, (e -> importInterfaceName == NULL) ? interface_name (e -> interface) : e -> importInterfaceName, name_base_name(e->name), NULL)) == NULL) {
      sprintf( buf, "%s_%s", c_interface_name (e -> interface), name_base_name (e -> name));
      do_C_fixups (buf);
      n = buf;
    }
    name_set_lang_name (e -> name, "C", n);
    n = name_lang_name (e -> name, "C");
  }
  return (n);
}

string c_simple_name (Name name)
{
  string n;

  if ((n = name_lang_name (name, "simple")) == NULL) {
    n = ilu_strdup (name_base_name (name));
    do_C_fixups (n);
    name_set_lang_name (name, "simple", n);
  }
  return (n);
}

string c_string (s)
     string s;
{
  string new = ilu_strdup (s);
  do_C_fixups (new);
  return (new);
}

