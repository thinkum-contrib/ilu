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
$Id: pprint.c,v 1.15 1999/08/03 01:51:02 janssen Exp $
*/
/* Last edited by Mike Spreitzer June 16, 1997 3:11 pm PDT */

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

extern char *pprint_type_name (Type);
extern char *pprint_exception_name (Exception);
extern char *pprint_constant_name (Constant);
extern char *pprint_argument_name (Argument);
extern char *pprint_procedure_name (Procedure);
extern char *pprint_interface_name (Interface);
extern void set_default_interface (Interface);
extern char *pprint_string(string, boolean);

boolean Verbose = TRUE;
cardinal CommentCol = 40;
list Includes = NIL;
string ProgramName = NULL;
static boolean FirstInterface = TRUE;
static int count = 0;
Interface CurrentInterface = NULL;

static void PrintRef (LineNumber r, boolean *first)
{
  if (*first)
    {
      printf ("%u", r);
      *first = FALSE;
    }
  else
    printf (", %u", r);
}

static void PrintMethodException (Exception e, boolean *first)
{
  if (*first)
    *first = FALSE;
  else
    printf (", ");
  printf ("%s", pprint_exception_name(e));
}

static void PrintMethodArgument (Argument a, boolean *first)
{
  if (*first)
    *first = FALSE;
  else
    printf (", ");
  printf ("%s%s : %s%s",
	  (a->direction == InOut) ? "INOUT " : ((a->direction == Out) ? "OUT " : ""),
	  pprint_argument_name(a), a->sibling ? "SIBLING " : "",
	  pprint_type_name(a->type));
}

static void PrintMethod (Procedure p, boolean *first)
{
  char buffer[2000];
  int pad;

  if (*first)
    *first = FALSE;
  else
    printf (",\n");
  printf ("    %s%s%s (",
	  p->asynch ? "ASYNCHRONOUS " : "",
	  p->functional ? "FUNCTIONAL " : "",
	  pprint_procedure_name(p));

  if (list_size(p->arguments) > 0)
    {
      boolean first = TRUE;
      list_enumerate (p->arguments, (void (*)(refany, refany)) PrintMethodArgument, &first);
    }
  printf (")");
  if (p->returnType != NULL)
    printf(" : %s", pprint_type_name(p->returnType));
  if (list_size(p->exceptions) > 0)
    {
      boolean first = TRUE;

      printf (" RAISES ");
      list_enumerate(p->exceptions, (void (*) (refany, refany)) PrintMethodException, &first);
      printf (" END");
    }
  if (p->doc_string)
    {
      string s = pprint_string (p->doc_string, TRUE);
      printf ("\n      \"%s\"", s);
      free(s);
    }
}

static void PrintSupertype (Type t, boolean *first)
{
  if (*first)
    {
      printf ("%s", pprint_type_name(t));
      *first = FALSE;
    }
  else
    printf (", %s", pprint_type_name(t));
}

static void PrintScoping (list scoping)
{
  int len, i;

  if (list_size(scoping) > 1) {
    printf ("(* IDL scoping:  ");
    for (i = 0, len = list_size(scoping) - 1;  i < len;  i++) {
      printf ("%s::", list_ref(scoping, i));
    }
    printf ("%s *)\n", list_ref(scoping, i));
  };
}

static void PrintClass (Type t)
{
  int len, pad;
  Class od = class_object(t);

  PrintScoping(t->scoping);
  printf ("TYPE %s = OBJECT", pprint_type_name(t));
  len = strlen(pprint_type_name(t)) + 14;
  if (od->brand)
    {
      string p = pprint_string(od->brand, TRUE);
      printf (" BRAND \"%s\"", p);
      len += strlen(p) + 9;
      free(p);
    }
  if (od->collectible)
    {
      printf (" COLLECTIBLE");
      len += 12;
    }
  if (od->optional)
    {
      printf (" OPTIONAL");
      len += 9;
    }
  pad = (len < CommentCol) ? (CommentCol - len) : 0;
  printf ("\n  ");
  printf (Verbose ? "(* defined on %u" : "(* %u", t->def);
  if (list_size(t->refs) > 0)
    {
      boolean first = TRUE;
      printf (Verbose ? ", referenced on " : "; ");
      list_enumerate (t->refs, (void (*)(refany, refany)) PrintRef, &first);
    }
  else if (Verbose)
    printf (", not referenced");
  {
    string p = pprint_string (t->uid, TRUE);
    printf (", MSTID is \"%s\" *)\n", p);
    free(p);
  }
  len = 0;
  if (od->singleton != NULL)
    {
      string p = pprint_string (od->singleton, TRUE);
      printf ("%sSINGLETON \"%s\"", (len > 0) ? " " : "  ", p);
      len += strlen(p) + 13 + ((len > 0) ? 0 : 1);
      free(p);
    }
  if (od->corba_rep_id != NULL)
    {
      string p = pprint_string (od->corba_rep_id, TRUE);
      printf ("%sTYPEID \"%s\"", (len > 0) ? " " : "  ", p);
      len += strlen(p) + 10 + ((len > 0) ? 0 : 1);
      free(p);
    }
  if (od->superclasses != NIL && list_size(od->superclasses) > 0)
    {
      boolean first = TRUE;
      printf ("%s  SUPERTYPES ", (len > 0) ? "\n" : "");
      list_enumerate (od->superclasses, (iluparser_EnumProc) PrintSupertype, &first);
      printf (" END");
      len = 5;
    }
  if (od->methods != NIL && list_size(od->methods) > 0)
    {
      boolean first = TRUE;

      printf ("%s  METHODS\n", (len > 0) ? "\n" : "");
      list_enumerate (od->methods, (iluparser_EnumProc) PrintMethod, &first);
      printf ("\n  END");
      len = 5;
    }
  if (od->doc_string != NIL)
    {
      string p = pprint_string (od->doc_string, TRUE);
      if (len > 0)
	printf ("\n");
      printf ("  DOCUMENTATION \"%s\"", p);
      free (p);
    }
  printf (";\n\n");
}

static void PrintArrayDim (long dim, int *len)
{
  char buf[40];

  sprintf (buf, (*len == 0) ? "%u" : ", %u", dim);
  *len += strlen(buf);
  printf ("%s", buf);
}

static void PrintEnumValue (EnumField ef, int *len)
{
  string p;

  if (*len > 0)
    {
      printf (", ");
      *len += 2;
    }
  p = pprint_string (ef->name, FALSE);
  printf ("%s", p);
  *len += strlen(p);
  free(p);
}

static void PrintRecordField (Argument f, boolean *first)
{
  if (NOT *first)
    printf (",\n    ");
  else
    {
      printf ("    ");
      *first = FALSE;
    }
  printf ("%s : %s", pprint_argument_name(f), pprint_type_name(f->type));
}

struct unioninfo {
  UnionDescription *d;
  boolean first;
};

static void PrintConstantValue (ConstantValue v)
{
  if (v->type == integer_Type)
    printf ("%s%lu", (v->val.i.sign < 0) ? "-" : "", v->val.i.value);
  else if (v->type == real_Type && v->val.r.fraction == NULL)
    printf ("%s%se%lu", (v->val.i.sign < 0) ? "-" : "", v->val.r.value, v->val.r.exponent);
  else if (v->type == real_Type && v->val.r.fraction != NULL)
    printf ("%s%s.%se%lu", (v->val.i.sign < 0) ? "-" : "", v->val.r.value,
	    v->val.r.fraction, v->val.r.exponent);
  else if (v->type == shortcharacter_Type)
    {
      string s = pprint_string(v->val.s, FALSE);
      printf ("%s", s);
      free(s);
    }
  else if (v->type == boolean_Type)
    printf ("%s", v->val.b ? "TRUE" : "FALSE");
  else {
    fprintf (stderr, "\n*** Bad constant value encountered.");
  }
}

static void PrintUnionArmValuator (ConstantValue v, struct unioninfo *u)
{
  if (u->first)
    u->first = FALSE;
  else
    printf (", ");
  PrintConstantValue (v);    
}

static void PrintUnionField (Argument f, struct unioninfo *u)
{
  if (!u->first)
    printf (",\n    ");
  else
    {
      printf ("    ");
      u->first = FALSE;
    }
  if (f->name->base_name != NIL)
    printf ("%s : ", pprint_argument_name(f));
  printf ("%s", pprint_type_name(f->type));
  if (f == u->d->default_arm)
    printf (" = DEFAULT");
  else if (f->values != NULL && list_size(f->values) > 0)
    {
      boolean oldFirst = u->first;

      printf (" = ");
      u->first = TRUE;
      list_enumerate(f->values, (iluparser_EnumProc) PrintUnionArmValuator, u);
      u->first = oldFirst;
      printf (" END");
    }
}

static int pprint_integer (IntegerLiteral l)
{
  int result=0;
  if(l->negative)
    result+=printf("-");
  if(l->small)
    result+=printf("%d",l->val.direct);
  else
    result+=printf("%s",l->val.string);
  return result;
}

static void PrintType (Type t)
{
  int pad, len;
  TypeDescription d;

  PrintScoping(t->scoping);
  printf ("TYPE %s = ", pprint_type_name(t));
  len = strlen(pprint_type_name(t)) + 8;
  d = type_description(t);
  switch (type_basic_type(t))
    {

    case record_Type:
      printf ("RECORD");
      len += 6;
      break;

    case union_Type:
      if (type_ur_kind(d->structuredDes.uniond.discriminator_type) != void_Type &&
	  type_ur_kind(d->structuredDes.uniond.discriminator_type) != shortcardinal_Type)
	{
	  printf ("%s ", pprint_type_name(d->structuredDes.uniond.discriminator_type));
	  len += 1 + strlen(pprint_type_name(d->structuredDes.uniond.discriminator_type));
	}
      printf ("UNION");
      len += 5;
      break;

    case array_Type:
      {
	int len2= 0;
	printf ("ARRAY OF ");
	len += 9;
	list_enumerate (d->structuredDes.array.dimensions, (iluparser_EnumProc) PrintArrayDim, &len2);
	len += len2;
	printf (" %s;", pprint_type_name(d->structuredDes.array.type));
	len += 2 + strlen(pprint_type_name(d->structuredDes.array.type));
      }      
      break;

    case enumeration_Type:
      {
	int len2 = 0;

	printf ("ENUMERATION ");
	len += 12;
	list_enumerate (d->structuredDes.enumeration, (iluparser_EnumProc) PrintEnumValue, &len2);
	len += len2;
	printf (" END;");
	len += 4;
      }
      break;

    case optional_Type:
      printf ("OPTIONAL %s;", pprint_type_name(d->structuredDes.optional));
      len += 10 + strlen(pprint_type_name(d->structuredDes.optional));
      break;

    case reference_Type:
      if (d->structuredDes.reference.optional) {
	printf ("%sOPTIONAL %s;",
		d->structuredDes.reference.aliased ? "ALIASED " : "",
		pprint_type_name(d->structuredDes.reference.base_type));
	len += 10 + strlen(pprint_type_name(d->structuredDes.reference.base_type)) +
	  strlen(d->structuredDes.reference.aliased ? "ALIASED " : "");
      } else {
	printf ("%sREFERENCE %s;",
		d->structuredDes.reference.aliased ? "ALIASED " : "",
		pprint_type_name(d->structuredDes.reference.base_type));
	len += 11 + strlen(pprint_type_name(d->structuredDes.reference.base_type)) +
	  strlen(d->structuredDes.reference.aliased ? "ALIASED " : "");
      };
      break;

    case string_Type:
      printf ("ILUSTRING");
      len += 9;
      if (d->structuredDes.string.max_length > 0) {
	char buf[100];
	sprintf (buf, " LIMIT %u", d->structuredDes.string.max_length);
	len += strlen(buf);
	printf ("%s", buf);
      };
      if (d->structuredDes.string.charset > 0) {
	char buf[100];
	sprintf (buf, " CHARSET %u", d->structuredDes.string.charset);
	len += strlen(buf);
	printf ("%s", buf);
      };
      if (d->structuredDes.string.language != NULL) {
	char buf[100];
	sprintf (buf, " LANGUAGE \"%s\"", d->structuredDes.string.language);
	len += strlen(buf);
	printf ("%s", buf);
      };
      printf (";"); len +=1;
      break;

    case alias_Type:
      printf ("%s;", pprint_type_name(t->supertype));
      len += 1 + strlen(pprint_type_name(t->supertype));
      break;

    case sequence_Type:
      printf ("SEQUENCE OF %s", pprint_type_name(d->structuredDes.sequence.type));
      len += 12 + strlen(pprint_type_name(d->structuredDes.sequence.type));
      if (d->structuredDes.sequence.limit > 0)
	{
	  char buf[100];
	  sprintf (buf, " LIMIT %u", d->structuredDes.sequence.limit);
	  len += strlen(buf);
	  printf ("%s", buf);
	}
      printf (";");
      len += 1;      
      break;

    case fixedpoint_Type:
      len += printf ("FIXEDPOINT ");
      len += pprint_integer(d->structuredDes.fixed.min_numerator);
      len += printf (" ");
      len += pprint_integer(d->structuredDes.fixed.max_numerator);
      len += printf (" ");
      len += pprint_integer(d->structuredDes.fixed.denominator);
      len += printf (";");
      break;

    default:
      fprintf (stderr, "Unexpected constructed type encountered.\n");
      exit(1);
      break;
    }

  pad = (len < CommentCol) ? (CommentCol - len) : 0;
  if (pad > 0)
    printf ("%*.*s", pad, pad, "");
  else
    printf ("\n  ");
  printf (Verbose ? "(* defined %u" : "(* %u", t->def);
  if (list_size(t->refs) > 0)
    {
      boolean first = TRUE;
      printf (Verbose ? ", referenced on " : "; ");
      list_enumerate (t->refs, (void (*)(refany, refany)) PrintRef, &first);
    }
  printf (" *)\n");

  if (type_basic_type(t) == record_Type)
    {
      boolean first = TRUE;
      list_enumerate (d->structuredDes.record.fields, (iluparser_EnumProc) PrintRecordField, &first);
      printf ("\n  END;\n");
    }
  else if (type_basic_type(t) == union_Type)
    {
      struct unioninfo info;
      info.d = &d->structuredDes.uniond;
      info.first = TRUE;
      list_enumerate (d->structuredDes.uniond.types, (iluparser_EnumProc) PrintUnionField, &info);
      printf ("\n  END%s;\n", d->structuredDes.uniond.others_allowed ? " OTHERS" : "");
    }
}

static void MaybePrintType (Type t)
{
  if (t->importInterfaceName == NULL && type_kind(t) != object_Type && (NOT t->builtIn) &&
      (list_size(t->refs) > 0 || t->def > 0))
    PrintType (t);
}

static void PrintException (Exception e)
{
  int len, pad;
  string p;

  PrintScoping(e->scoping);
  len = strlen(pprint_exception_name(e)) +
    10 /* strlen("EXCEPTION ") */ + 1 /* strlen(";") */ +
    ((exception_type(e) == NULL) ? 0 : (3 + strlen(pprint_type_name(exception_type(e)))));
  printf ("EXCEPTION %s%s%s", pprint_exception_name(e),
	  (exception_type(e) == NULL) ? "" : " : ",
	  (exception_type(e) == NULL) ? "" : pprint_type_name(exception_type(e)));
  if (e->corba_rep_id != NULL)
    {
      p = pprint_string(e->corba_rep_id, TRUE);
      printf (" TYPEID \"%s\"", p);
      len += 11 + strlen(p);
      free(p);
    }
  if (e->doc_string != NULL)
    {
      p = pprint_string(e->doc_string, TRUE);
      printf (" \"%s\"", p);
      len += strlen(p) + 3;
      free(p);
    }
  printf (";");
  pad = (len < CommentCol) ? (CommentCol - len) : 0;
  if (pad > 0)
    printf ("%*.*s", pad, pad, "");
  else
    printf ("\n  ");
  printf ("(* ");
	  
  if (e->builtIn)
    printf ("<built-in>");
  else if (e->def != 0)
    printf (Verbose ? "defined on line %ld" : "%ld", e->def);
  else if (e->importInterfaceName != NULL)
    printf ("from interface \"%s\"", e->importInterfaceName);
  else
    printf ("**** no definition ****");
  if (list_size(e->refs) > 0)
    {
      boolean first = TRUE;
      printf (Verbose ? ", used on " : "; ");
      list_enumerate (e->refs, (void (*)(refany, refany)) PrintRef, &first);
    }
  if (list_size(e->refs) < 1 && Verbose)
    printf (", not used");
  printf (" *)\n");
}

static void PrintConstant (Constant c)
{
  int len, pad;
  char buf[35];

  PrintScoping(c->scoping);
  len = 9 /* strlen("CONSTANT ") */ +
    strlen(pprint_constant_name(c)) + 3 + strlen(pprint_type_name(c->type)) + 1;
  printf ("CONSTANT %s : %s ", name_base_name(c->name), pprint_type_name(c->type));
  if (c->value == NULL)
    {
      sprintf (buf, "??;");
    }
  else
    {
      TypeKind ctk, vtk;
      char *ctypename = name_base_name(c->name);

      ctk = type_ur_kind(c->type);
      vtk = c->value->type;

      switch (ctk)
	{
	case shortinteger_Type:
	case integer_Type:
	case longinteger_Type:
	  if (vtk != integer_Type)
	    fprintf (stderr, "\n*** Constants of type \"%s\" must have an associated integer value.\n", ctypename);
	  else
	    sprintf (buf, "= %s%lu", (c->value->val.i.sign < 0) ? "-" : "", c->value->val.i.value);
	  break;

	case byte_Type:
	case shortcardinal_Type:
	case cardinal_Type:
	case longcardinal_Type:
	  if (vtk != integer_Type)
	    fprintf (stderr, "\n*** Constants of type \"%s\" must have an associated integer value.\n", ctypename);
	  else if (c->value->val.i.sign < 0)
	    fprintf (stderr, "\n*** Constants of type \"%s\" may not be negative.\n", ctypename);
	  else
	    printf ("= %lu", c->value->val.i.value);
	  break;

	case real_Type:
	case shortreal_Type:
	case longreal_Type:
	  if (vtk != real_Type)
	    fprintf (stderr, "\n*** Constants of type \"%s\" must have an associated real value.\n", ctypename);
	  else if (c->value->val.r.fraction == NULL)
	    printf ("= %s%se%lu", (c->value->val.i.sign < 0) ? "-" : "", c->value->val.r.value, c->value->val.r.exponent);
	  else
	    printf ("= %s%s.%se%lu", (c->value->val.i.sign < 0) ? "-" : "", c->value->val.r.value,
		     c->value->val.r.fraction, c->value->val.r.exponent);
	  break;

	case boolean_Type:
	  if (vtk != boolean_Type)
	    fprintf (stderr, "\n*** Constants of type \"%s\" must have an associated boolean value.\n", ctypename);
	  else
	    printf ("= %s", c->value->val.b ? "True" : "False");
	  break;

	case sequence_Type:
	  if (vtk != shortcharacter_Type)
	    fprintf (stderr, "\n*** Constants of type \"%s\" must have an associated string value.\n", ctypename);
	  else
	    {
	      string s = pprint_string(c->value->val.s, TRUE);
	      sprintf (buf, "= \"%s\";", s);
	      free(s);
	    }
	  break;

	case enumeration_Type:
	  if (vtk != shortcharacter_Type)
	    fprintf (stderr, "*** Constants of type \"%s\" must have an associated string value.\n", ctypename);
	  else
	    {
	      string s = pprint_string(c->value->val.s, TRUE);
	      sprintf (buf, "= %s;", s);
	      free(s);
	    }
	  break;

	default:
	  fprintf (stderr, "\n*** Constants of type \"%s\" not allowed.\n", ctypename);
	  break;
	}
    }
  len += strlen(buf);
  pad = (len < CommentCol) ? (CommentCol - len) : 0;
  if (pad > 0)
    printf ("%s%*.*s", buf, pad, pad, "");
  else
    printf ("%s\n  ", buf);
  printf (Verbose ? "(* defined on line %u *)\n" : "(* %u *)\n", c->def);
}

static void PrintImport (Imported s, boolean *first)
{
  if (*first)
    {
      printf ("  IMPORTS");
      *first = FALSE;
    }
  else
    printf (",");
  printf ("\n    \"%s\"", s->name);
  if (s->filename != NULL && strlen(s->filename) > 0)
    printf (" FROM \"%s\"", s->filename);
}

static char *ModTime (char *path)
{
  static char timebuf[30];
  struct stat statbuf;

#ifdef MACOS
	return "Jan 15, 1592";
#else
  stat (path, &statbuf);
  strcpy (timebuf, ctime(&statbuf.st_mtime));
  timebuf[24] = '\0';
  return (timebuf);
#endif /* MACOS */
}

static void generateBoilerplate (FILE *file, list interfaces)
{
  static char *prefixes[2] = { " *", " *" };

  fprintf (file, "(*\n");
  iluparser_MultipleInterfaceBoilerplate (file, interfaces, ProgramName, prefixes);
  fprintf (file, " *)\n\n");
}

static void PrintInterface (Interface s)
{
  int len, pad;
  string p;

  printf ("INTERFACE %s", pprint_interface_name(s));
  len = strlen(pprint_interface_name(s))
    + 10 /* strlen("INTERFACE ") */
      + (list_size(s->imports) > 0) ? 0 : 1;
  if (s->brand != NULL)
    {
      p = pprint_string(s->brand, FALSE);
      printf (" BRAND %s", p);
      len += strlen(p) + 7;
      free(p);
    }
  pad = (len < CommentCol) ? (CommentCol - len) : 0;
  if (pad > 0)
    printf ("%s%*.*s", list_size(s->imports) > 0 ? "" : ";", pad, pad, "");
  else
    printf ("%s\n  ", list_size(s->imports) > 0 ? "" : ";");
  printf ("(* defined %u *)\n", s->def);	  
  if (list_size(s->imports) > 0)
    {
      boolean first = TRUE;
      list_enumerate (s->imports, (void (*)(refany, refany)) PrintImport, &first);
      printf ("\n  END;\n");
    }
  printf ("\n");
}

static boolean SortTypeListByName (Type a, Type b)
{
  /* returns true if a belongs before b */

  if (a->builtIn || b->builtIn)
    return FALSE;
  return (strcmp (pprint_type_name(a), pprint_type_name(b)) > 0);
}

static boolean SortExceptionsByName (Exception a, Exception b)
{
  return (strcmp (pprint_exception_name(a), pprint_exception_name(b)) > 0);
}

static boolean SortConstantsByName (Constant a, Constant b)
{
  return (strcmp (pprint_constant_name(a), pprint_constant_name(b)) > 0);
}

static void PrintParse (Interface s)
{
  set_default_interface (s);

  PrintInterface (s);
  if (list_size(s->types) > 0)
    {
      list_sort (s->types, (iluparser_CompareProc) SortTypeListByName);
      printf ("(*********************** Non-Object Types ************************)\n\n");
      list_enumerate (s->types, (void (*)(refany, refany)) MaybePrintType, s);
      printf ("\n");
    }
  if (list_size(s->classes) > 0)
    {
      list_sort (s->classes, (iluparser_CompareProc) SortTypeListByName);
      printf ("(*********************** Object Types ****************************)\n\n");
      list_enumerate (s->classes, (void (*)(refany, refany)) PrintClass, s);
      printf ("\n");
    }
  if (list_size(s->exceptions) > 0)
    {
      list_sort (s->exceptions, (iluparser_CompareProc) SortExceptionsByName);
      printf ("(*********************** Exceptions ******************************)\n\n");
      list_enumerate (s->exceptions, (void (*)(refany, refany)) PrintException, s);
      printf ("\n");
    }
  if (list_size(s->constants) > 0)
    {
      list_sort (s->constants, (iluparser_CompareProc) SortConstantsByName);
      printf ("(*********************** Constants *******************************)\n\n");
      list_enumerate (s->constants, (void (*)(refany, refany)) PrintConstant, s);
      printf ("\n");
    }

  set_default_interface (NULL);
}

static void PprintInterface (Interface i, refany junk)
{
  PrintParse (i);
}

static void Usage()
{
  fprintf(stderr, "Usage: %s [-I ISLDIRECTORY] ISLFILE [ISLFILE ...]\n", ProgramName);
}

#ifdef WIN16
   	int scan_main (int ac, char **av, char **envp)
#else
   	int main (int ac, char **av, char **envp)
#endif

{
  list s;
  char **interfacename;
  int i_num_isl_files_processed = 0;
  int i_renaming_files = 0;

  if (ac < 2) {
    Usage();
    exit(1);
  }
  Includes = new_list();

  if ((ProgramName = iluparser_GetProgramName(*av)) == NULL)
#if (defined(WIN32) && defined(_WINIO))
    ProgramName = "islpp";
#else
    ProgramName = "islpp";
#endif /* (defined(WIN32) && defined(_WINIO)) */
  ac--;
  av++;
  while(*av[0] == '-') {
    if (strcmp(*av, "-I") == 0)
      list_insert(Includes, *++av);
    else if (strcmp(*av, "-brief") == 0)
      Verbose = FALSE;
   else {
      Usage();
      fprintf (stderr, "%s: Invalid switch \"%s\".\n", ProgramName, *av);
      return 1;
    }
    ac--;
    av++;
  }

  iluparser_RegisterInterfaceDirectories(Includes);

  for(interfacename = av; *interfacename != NULL; interfacename++) {
    if ((s = ParseFile(*interfacename)) == NULL) {
      fprintf (stderr, "%s:  Couldn't find or parse %s.\n", ProgramName, *interfacename);
    }
    generateBoilerplate (stdout, s);
    list_enumerate(s, (void (*)(refany, refany)) PprintInterface, NULL);
  }
#if (defined(WIN32) && defined(_WINIO))
  return 0;
#else
  exit(0);
#endif
}
