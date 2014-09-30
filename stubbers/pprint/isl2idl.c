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
$Id: isl2idl.c,v 1.9 1999/08/03 01:51:02 janssen Exp $
*/
/* Last edited by Mike Spreitzer May 13, 1998 2:04 pm PDT */

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
string DefaultProgramName = "isl2idl";
static boolean FirstInterface = TRUE;
static int count = 0;
Interface CurrentInterface = NULL;

static void PrintRef(refany element, refany rock)
{
  LineNumber      r = (LineNumber) element;
  boolean        *first = (boolean *) rock;
  if (*first) {
    printf("%u", r);
    *first = FALSE;
  } else
    printf(", %u", r);
}

static void PrintMethodException(refany element, refany rock)
{
  Exception       e = (Exception) element;
  boolean        *first = (boolean *) rock;
  if (*first)
    *first = FALSE;
  else
    printf(", ");
  printf("%s", pprint_exception_name(e));
}

typedef struct {
  boolean         first;
  int             left, len;
}               ParmPrintState;

static void PrintMethodArgument(refany element, refany rock)
{
  Argument        a = (Argument) element;
  ParmPrintState *pps = (ParmPrintState *) rock;
  char           *mode;
  int             dl;
  if (pps->first)
    pps->first = FALSE;
  else {
    printf(", ");
    pps->len += 2;
  }
  mode = ((a->direction == InOut) ? "INOUT "
      : (a->direction == Out) ? "OUT " : "");
  dl = strlen(mode) + strlen(pprint_type_name(a->type))
    + strlen(pprint_argument_name(a)) + 1;
  if (pps->len + dl > 73) {
    printf("\n%*.*s", pps->left, pps->left, "");
    pps->len = pps->left;
  }
  printf("%s%s %s",
     mode,
     pprint_type_name(a->type),
     pprint_argument_name(a));
  pps->len += dl;
}

static void PrintMethod(refany element, refany rock)
{
  Procedure       p = (Procedure) element;
  string          mode, ret;
  int             len;

  mode = p->asynch ? "ONEWAY " : "";
  if (p->returnType != NULL)
    ret = pprint_type_name(p->returnType);
  else
    ret = "void";
  printf("  %s %s %s (", mode, ret, pprint_procedure_name(p));
  len = 6 + strlen(mode) + strlen(ret) + strlen(pprint_procedure_name(p));

  if (list_size(p->arguments) > 0) {
    ParmPrintState  pps = {TRUE, 0, 0};
    pps.left = pps.len = len;
    list_enumerate(p->arguments, PrintMethodArgument, &pps);
  }
  printf(")");
  if (list_size(p->exceptions) > 0) {
    boolean         first = TRUE;

    printf("\n    raises (");
    list_enumerate(p->exceptions, PrintMethodException, &first);
    printf(")");
  }
  printf(";\n");
  if (p->doc_string) {
    string          s = pprint_string(p->doc_string, TRUE);
    printf("    /* %s */\n", s);
    free(s);
  }
  printf("\n");
}

static void PrintSupertype(refany element, refany rock)
{
  Type            t = (Type) element;
  int            *plen = (int *) rock;
  if (*plen) {
    printf(", ");
    (*plen) += 2;
  }
  printf("%s", pprint_type_name(t));
  (*plen) += strlen(pprint_type_name(t));
}

static void PrintScoping (list scoping)
{
  int len, i;

  if (list_size(scoping) > 1) {
    printf ("/* IDL scoping:  ");
    for (i = 0, len = list_size(scoping) - 1;  i < len;  i++) {
      printf ("%s::", list_ref(scoping, i));
    }
    printf ("%s */\n", list_ref(scoping, i));
  };
}

static void PrintClass (refany element, refany rock)
{
  Type            t = (Type) element;
  int             len, pad;
  Class           od = class_object(t);

  if (od->collectible) {
    fprintf(stderr, "Can't translate COLLECTIBLE object type %s!\n",
        pprint_type_name(t));
    exit(1);
  }
  if (od->corba_rep_id != NULL) {
    string          p = pprint_string(od->corba_rep_id, TRUE);
    printf("#pragma RepositoryID \"%s\";\n", p);
    free(p);
  }
  printf("interface %s", pprint_type_name(t));
  len = strlen(pprint_type_name(t)) + strlen("interface ");
  if (od->superclasses != NIL && list_size(od->superclasses) > 0) {
    int             len2 = 0;
    printf(": ");
    list_enumerate(od->superclasses, PrintSupertype, &len2);
    len += len2;
  }
  printf("{\n");
  if (od->singleton != NULL) {  /* YYY */
    printf("  // SINGLETON\n");
  }
  if (!od->optional) {      /* YYY */
    printf(" // null objects not included in this object type!\n");
  }
  if (od->brand) {      /* YYY */
    string          p = pprint_string(od->brand, TRUE);
    printf("  // BRAND %s\n", p);
    free(p);
  }
  printf(Verbose ? "  /* defined on %u" : "  /* %u", t->def);
  if (list_size(t->refs) > 0) {
    boolean         first = TRUE;
    printf(Verbose ? ", referenced on " : "; ");
    list_enumerate(t->refs, PrintRef, &first);
  } else if (Verbose)
    printf(", not referenced");
  {
    string          p = pprint_string(t->uid, TRUE);
    printf(",\n     MSTID is \"%s\" */\n\n", p);
    free(p);
  }
  if (od->doc_string != NIL) {
    string          p = pprint_string(od->doc_string, TRUE);
    printf("/* %s */\n", p);
    free(p);
  }
  if (od->methods != NIL && list_size(od->methods) > 0) {
    list_enumerate(od->methods, PrintMethod, NULL);
  }
  printf("};\n\n");
}

static void PrintArrayDim(refany element, refany rock)
{
  long unsigned   dim = (long unsigned) element;
  int            *plen = (int *) rock;
  char            buf[30];
  sprintf(buf, "[%lu]", dim);
  (*plen) += strlen(buf);
  printf("%s", buf);
}

static void PrintEnumValue(refany element, refany rock)
{
  EnumField       ef = (EnumField) element;
  int            *plen = (int *) rock;
  string          p;

  if (*plen > 0) {
    printf(", ");
    *plen += 2;
  }
  p = pprint_string(ef->name, FALSE);
  printf("%s", p);
  *plen += strlen(p);
  free(p);
}

static void PrintRecordField(refany element, refany rock)
{
  Argument        f = (Argument) element;
  printf("  %s %s;\n",
     pprint_type_name(f->type), pprint_argument_name(f));
}

struct unioninfo {
  UnionDescription *d;
  int caseno;
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

static void PrintUnionArmValuator (refany element, refany rock)
{
  ConstantValue   v = (ConstantValue) element;
  struct unioninfo *u = (struct unioninfo *) rock;
  printf("case ");
  PrintConstantValue(v);
  printf(": ");
}

static void PrintUnionField(refany element, refany rock)
{
  Argument        f = (Argument) element;
  struct unioninfo *u = (struct unioninfo *) rock;
  if (f == u->d->default_arm)
    printf("default:");
  else if (f->values != NULL && list_size(f->values) > 0) {
    list_enumerate(f->values, PrintUnionArmValuator, u);
  } else            /* XXX what??? */
    ;
  printf("%s ", pprint_type_name(f->type));
  if (f->name->base_name != NIL)
    printf("%s;\n", pprint_argument_name(f));
  else
    printf("case%d;\n", u->caseno);

  u->caseno++;
}

static void PrintType (Type t)
{
  int             pad, len;
  TypeDescription d;
  string          tname = pprint_type_name(t);

  d = type_description(t);
  switch (type_basic_type(t)) {

  case record_Type:
    printf("struct %s {\n", tname);
    list_enumerate(d->structuredDes.record.fields, PrintRecordField, NULL);
    printf("};");
    len = 2;
    break;

  case union_Type:{
      Type            tagType;
      struct unioninfo info;
      tagType = d->structuredDes.uniond.discriminator_type;
      printf("union %s switch ", tname);
      if (type_ur_kind(tagType) != void_Type &&
      type_ur_kind(tagType) != shortcardinal_Type) {
    printf("(%s)", pprint_type_name(tagType));
    len += 1 + strlen(pprint_type_name(tagType));
      } else
    printf("(unsigned short)");
      printf(" {\n");
      len += 5;
      info.d = &d->structuredDes.uniond;
      info.caseno = 0;
      list_enumerate(d->structuredDes.uniond.types,
             PrintUnionField, &info);
      if (!d->structuredDes.uniond.others_allowed)
    printf("/* and no others!*/");  /* XXX crapout */
      printf("};");
      len = 2;
    }
    break;

  case enumeration_Type:
    printf("enum %s {", tname);
    len = 0;
    list_enumerate(d->structuredDes.enumeration, PrintEnumValue, &len);
    printf("};");
    len += strlen("enum %s {};") + strlen(tname) - 2;
    break;

  case array_Type:
    printf("typedef %s %s",
       pprint_type_name(d->structuredDes.array.type),
       tname);
    len = strlen("typedef %s %s;") + strlen(tname)
      + strlen(pprint_type_name(d->structuredDes.array.type)) - 4;
    list_enumerate(d->structuredDes.array.dimensions, PrintArrayDim,
           &len);
    printf(";");
    break;

  case optional_Type:
    printf("typedef sequence<%s,1> %s;",
       pprint_type_name(d->structuredDes.optional),
       tname);
    len = strlen("typedef sequence<%s,1> %s;") + strlen(tname)
      + strlen(pprint_type_name(d->structuredDes.optional)) - 4;
    break;

  case alias_Type:
    printf("typedef %s %s;", pprint_type_name(t->supertype), tname);
    len = strlen("typedef  ;") + strlen(pprint_type_name(t->supertype))
      + strlen(tname);
    break;

  case sequence_Type:
    if (type_ur_kind(d->structuredDes.sequence.type)
    == shortcharacter_Type) {
      printf("typedef string");
      len = strlen("typedef string ;") + strlen(tname);
      if (d->structuredDes.sequence.limit > 0) {
    char            buf[50];
    sprintf(buf, "<%lu>", d->structuredDes.sequence.limit);
    len += strlen(buf);
    printf("%s", buf);
      }
      printf(" %s;", tname);
    } else {
      printf("typedef sequence<%s",
         pprint_type_name(d->structuredDes.sequence.type));
      len = strlen("typedef sequence<> ;") + strlen(tname)
    + strlen(pprint_type_name(d->structuredDes.sequence.type));
      if (d->structuredDes.sequence.limit > 0) {
    char            buf[50];
    sprintf(buf, ",%lu", d->structuredDes.sequence.limit);
    len += strlen(buf);
    printf("%s", buf);
      }
      printf("> %s;", tname);
    }
    break;

  default:
    fprintf(stderr,
        "Unexpected constructed type (named %s) encountered!\n",
        tname);
    exit(1);
    break;
  }

  pad = (len < CommentCol) ? (CommentCol - len) : 0;
  if (pad > 0)
    printf("%*.*s", pad, pad, "");
  else
    printf("\n  ");
  printf("/* %s %u", (Verbose ? "defined " : ""), t->def);
  if (list_size(t->refs) > 0) {
    boolean         first = TRUE;
    printf(Verbose ? ", referenced on " : "; ");
    list_enumerate(t->refs, PrintRef, &first);
  }
  printf(" */\n\n");
}

static void MaybePrintType(refany element, refany rock)
{
  Type            t = (Type) element;
  if (t->importInterfaceName == NULL && type_kind(t) != object_Type
      && (NOT t->builtIn) && (list_size(t->refs) > 0 || t->def > 0))
    PrintType(t);
}

static void PrintException(refany element, refany rock)
{
  Exception       e = (Exception) element;
  int             len, pad;
  string          p;
  string          ename = pprint_exception_name(e);

  if (e->corba_rep_id != NULL) {
    p = pprint_string(e->corba_rep_id, TRUE);
    printf("#pragma RepositoryID \"%s\"\n", p);
    free(p);
  }
  printf("exception %s {", ename);
  if (!e->type)
    0;
  else if (type_ur_kind(e->type) == record_Type) {
    Type            parmtype = ur_type(e->type);
    TypeDescription d;
    d = type_description(parmtype);
    printf("\n");
    list_enumerate(d->structuredDes.record.fields, PrintRecordField, NULL);
  } else
    printf("\n  %s _parm;\n", pprint_type_name(e->type));
  printf("};\n");
  if (e->doc_string != NULL) {
    p = pprint_string(e->doc_string, TRUE);
    printf("  /* %s */\n", p);
    free(p);
  }
  printf("  /* ");

  if (e->builtIn)
    printf("<built-in>");
  else if (e->def != 0)
    printf(Verbose ? "defined on line %ld" : "%ld", e->def);
  else if (e->importInterfaceName != NULL)
    printf("from interface \"%s\"", e->importInterfaceName);
  else
    printf("**** no definition ****");
  if (list_size(e->refs) > 0) {
    boolean         first = TRUE;
    printf(Verbose ? ", used on " : "; ");
    list_enumerate(e->refs, PrintRef, &first);
  }
  if (list_size(e->refs) < 1 && Verbose)
    printf(", not used");
  printf(" */\n");
}

static void PrintConstant (refany element, refany rock)
{
  Constant        c = (Constant) element;
  int             len = 0;
  int             pad;
  char            buf[35];

  printf("const %s %s = ", pprint_type_name(c->type),
     name_base_name(c->name));
  buf[0] = 0;
  if (c->value == NULL) {
    sprintf(buf, "??;");
  } else {
    TypeKind        ctk, vtk;
    char           *ctypename = pprint_type_name(c->type);

    ctk = type_ur_kind(c->type);
    vtk = c->value->type;

    switch (ctk) {
    case shortinteger_Type:
    case integer_Type:
    case longinteger_Type:
      if (vtk != integer_Type)
    fprintf(stderr,
        "\n*** Constants of type \"%s\" must have an associated integer value.\n",
        ctypename);
      else
    sprintf(buf, "%s%lu", (c->value->val.i.sign < 0) ? "-" : "",
        c->value->val.i.value);
      break;

    case byte_Type:
    case shortcardinal_Type:
    case cardinal_Type:
    case longcardinal_Type:
      if (vtk != integer_Type)
    fprintf(stderr,
        "\n*** Constants of type \"%s\" must have an associated integer value.\n",
        ctypename);
      else if (c->value->val.i.sign < 0)
    fprintf(stderr,
       "\n*** Constants of type \"%s\" may not be negative.\n",
        ctypename);
      else
    printf("%lu", c->value->val.i.value);
      break;

    case real_Type:
    case shortreal_Type:
    case longreal_Type:
      if (vtk != real_Type)
    fprintf(stderr,
        "\n*** Constants of type \"%s\" must have an associated real value.\n",
        ctypename);
      else if (c->value->val.r.fraction == NULL)
    printf("%s%se%lu", (c->value->val.i.sign < 0) ? "-" : "",
           c->value->val.r.value, c->value->val.r.exponent);
      else
    printf("%s%s.%se%lu", (c->value->val.i.sign < 0) ? "-" : "",
           c->value->val.r.value,
           c->value->val.r.fraction, c->value->val.r.exponent);
      break;

    case boolean_Type:
      if (vtk != boolean_Type)
    fprintf(stderr,
        "\n*** Constants of type \"%s\" must have an associated boolean value.\n",
        ctypename);
      else
    printf("%s", c->value->val.b ? "TRUE" : "FALSE");
      break;

    case sequence_Type:
      if (vtk != shortcharacter_Type)
    fprintf(stderr,
        "\n*** Constants of type \"%s\" must have an associated string value.\n",
        ctypename);
      else {
    string          s = pprint_string(c->value->val.s, TRUE);
    printf("\"%s\"", s);
    len += 2 + strlen(s);
    free(s);
      }
      break;

    case enumeration_Type:
      if (vtk != shortcharacter_Type)
    fprintf(stderr,
        "*** Constants of type \"%s\" must have an associated string value.\n",
        ctypename);
      else {
    string          s = pprint_string(c->value->val.s, TRUE);
    printf("%s", s);
    len += strlen(s);
    free(s);
      }
      break;

    default:
      fprintf(stderr,
          "\n*** Constants of type \"%s\" not allowed.\n",
          ctypename);
      break;
    }
  }
  len += strlen(buf) + 1;
  pad = (len < CommentCol) ? (CommentCol - len) : 0;
  if (pad > 0)
    printf("%s;%*.*s", buf, pad, pad, "");
  else
    printf("%s;\n  ", buf);
  printf(Verbose ? "/* defined on line %u */\n" : "/* %u */\n", c->def);
}

static void PrintImport(refany element, refany rock)
{
  Imported        s = (Imported) element;
  if (s->filename != NULL && strlen(s->filename) > 0)
    printf("#include \"%s\"", s->filename);
  else
    printf("#include \"%s.idl\"\n", s->name);
  /* XXX translate to .idl filename */
}

static void generateBoilerplate (FILE *file, list interfaces)
{
  static char    *prefixes[2] = {" *", " *"};

  fprintf(file, "/*\n");
  iluparser_MultipleInterfaceBoilerplate(file, interfaces,
                     ProgramName, prefixes);
  fprintf(file, " */\n\n");
}

static void PrintInterface (Interface s)
{
  int             len, pad;
  string          p;

  list_enumerate(s->imports, PrintImport, NULL);
  printf("module %s {", pprint_interface_name(s));
  len = strlen("module  {")
    + strlen(pprint_interface_name(s));
  pad = (len < CommentCol) ? (CommentCol - len) : 0;
  if (pad > 0)
    printf("%*.*s", pad, pad, "");
  else
    printf("\n  ");
  printf("/* defined %u */\n", s->def);
  if (s->brand != NULL) {   /* YYY */
    p = pprint_string(s->brand, TRUE);
    printf("  // BRAND %s\n", p);
    free(p);
  }
  printf("\n");
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

static boolean FindPtr(refany element, refany rock)
{
  return (element == rock);
}

typedef struct {
    list decllist;
    list stack;
} TopoSortState;

static void EnsureTypeDecld(refany element, refany rock);

static void EnsureCaseDecld(refany element, refany rock)
{
  Argument        f = (Argument) element;
  TopoSortState  *tss = (TopoSortState *) rock;
  EnsureTypeDecld(f->type, tss);
  return;
}

static void PrintTypeName(refany element, refany rock)
{
  Type            t = (Type) element;
  FILE           *to = (FILE *) rock;
  fprintf(to, ", %s", pprint_type_name(t));
}

static void EnsureTypeDecld(refany element, refany rock)
{
  Type            t = (Type) element;
  TopoSortState  *tss = (TopoSortState *) rock;
  TypeKind        tk = type_kind(t);
  TypeDescription d;
  string          name;
  if (t->importInterfaceName)
    return;
  if (list_find(tss->decllist, FindPtr, t))
    return;
  if (list_find(tss->stack, FindPtr, t)) {
    fprintf(stderr, "Can't translate due to recursion in types;\n");
    fprintf(stderr, "cycle is in path %s", pprint_type_name(t));
    list_enumerate(tss->stack, PrintTypeName, stderr);
    fprintf(stderr, ".\n");
    exit(1);
  }
  list_insert(tss->stack, t);
  d = type_description(t);
  switch (tk) {
  case optional_Type:
    EnsureTypeDecld(d->structuredDes.optional, tss);
    break;
  case alias_Type:
    EnsureTypeDecld(t->supertype, tss);
    break;
  case union_Type:{
      Type            tagType;
      tagType = d->structuredDes.uniond.discriminator_type;
      EnsureTypeDecld(tagType, tss);
      list_enumerate(d->structuredDes.uniond.types, EnsureCaseDecld,
             tss);
      break;
    }
  case sequence_Type:
    EnsureTypeDecld(d->structuredDes.sequence.type, tss);
    break;
  case record_Type:
    list_enumerate(d->structuredDes.record.fields, EnsureCaseDecld, tss);
    break;
  case array_Type:
    EnsureTypeDecld(d->structuredDes.array.type, tss);
    break;
  default:
    0;
  }
  list_insert(tss->decllist, t);
  list_remove(tss->stack, t);
  return;
}

static void PrintParse (Interface s)
{
  set_default_interface (s);

  PrintInterface (s);
  if (list_size(s->types) > 0) {
    TopoSortState   tss = {0};
    printf("/*********************** Non-Object Types ************************/\n\n");
    tss.decllist = new_list();
    tss.stack = new_list();
    list_sort(s->types, (iluparser_CompareProc) SortTypeListByName);
    list_enumerate(s->types, EnsureTypeDecld, &tss);
    list_enumerate(tss.decllist, MaybePrintType, s);
    printf("\n");
  }
  if (list_size(s->constants) > 0)
    {
      list_sort (s->constants, (iluparser_CompareProc) SortConstantsByName);
      printf ("/*********************** Constants *******************************/\n\n");
      list_enumerate (s->constants, PrintConstant, s);
      printf ("\n");
    }
  if (list_size(s->exceptions) > 0)
    {
      list_sort (s->exceptions, (iluparser_CompareProc) SortExceptionsByName);
      printf ("/*********************** Exceptions ******************************/\n\n");
      list_enumerate (s->exceptions, PrintException, s);
      printf ("\n");
    }
  if (list_size(s->classes) > 0)
    {
      list_sort (s->classes, (iluparser_CompareProc) SortTypeListByName);
      printf ("/*********************** Object Types ****************************/\n\n");
      list_enumerate (s->classes, PrintClass, s);
      printf ("\n");
    }

  printf("};\n");
  set_default_interface (NULL);
}

static void PprintInterface(refany element, refany rock)
{
  Interface       i = (Interface) element;
  PrintParse(i);
}

static void Usage()
{
       
fprintf(stderr, "Usage: %s [-I ISLDIRECTORY] ISLFILE [ISLFILE ...]\n", 
    (ProgramName ? ProgramName : DefaultProgramName));
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
    ProgramName = DefaultProgramName;
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
    list_enumerate(s, PprintInterface, NULL);
  }
#if (defined(WIN32) && defined(_WINIO))
  return 0;
#else
  exit(0);
#endif
}
