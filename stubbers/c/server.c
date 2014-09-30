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
/* $Id: server.c,v 1.107 1999/08/03 01:50:22 janssen Exp $ */
/* Last edited by Mike Spreitzer April 1, 1997 2:07 pm PST */

#include "cstubber.h"

static boolean FirstClass = TRUE;
static boolean NoDefaultTrueClass = FALSE;
static list NoDefaultTrueClassList = NULL;

static void listArgument(refany elt, refany rock)
{
  Argument        a = (Argument) elt;
  Context         context = (Context) rock;
  int             indv, indp;
  indv = Indirectness(a->type, adRole[a->direction], TRUE, 0);
  indp = Indirectness(a->type, adRole[a->direction], FALSE, 0);
  fprintf(context->file, ", %s%s",
	  ((indp > indv) ? "&" : ""),
	  c_argument_name(a));
}

void 
FreeValue(Type type, string val, Role role, Context context,
	  int indent)
{
  Type            ut = ur_type(type);
  int             indv = Indirectness(ut, role, TRUE, 1);
  int             indp = Indirectness(ut, role_Exn, FALSE, 1);

  if (NeedsFree(ut)) {
    if (Sliced(ut, role)) {
      if (indv != indp) {
	fprintf(stderr, "Bug at %s:%d: %d != %d, for %s, %d.\n",
		__FILE__, __LINE__, indv, indp,
		c_type_name(ut), role);
	fprintf(context->file,
		"%*.*s_ilu_Assert(0, \"FreeValue(%s, %d)\");\n",
		indent, indent, "",
		c_type_name(ut), role);
      }
      fprintf(context->file, "%*.*s%s__Free ((%s)%s);\n",
	      indent, indent, "", c_type_name(ut),
	      c_role_type(ut, role_Exn, FALSE), val);
    } else if (TypeIsString(type)) {
      fprintf(context->file, "%*.*silu_CString__Free ((ilu_CString *) %s%s);\n",
	      indent, indent, "",
	      ((indp > indv) ? "&" : ""),
	      val);
    } else if (type_kind(type) == pickle_Type) {
      fprintf(context->file, "%*.*sCORBA_any__Free ((CORBA_any *) %s%s);\n",
	      indent, indent, "",
	      ((indp > indv) ? "&" : ""),
	      val);
    } else {
      fprintf(context->file, "%*.*s%s__Free (%s%s);\n",
	      indent, indent, "",
	      c_type_name(ut),
	      ((indp > indv) ? "&" : ""),
	      val);
    }
  }
  if (indv)
    fprintf(context->file, "%*.*sCORBA_free(%s);\n", indent, indent, "", val);
}

static void freeArgumentIn(refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  if ((arg->direction == In) &&
      (type_ur_kind(arg->type) == sequence_Type) &&
      (type_ur_kind(type_description(arg->type)->structuredDes.sequence.type) == byte_Type) &&
      (type_description(arg->type)->structuredDes.sequence.limit > 0) &&
      (type_description(arg->type)->structuredDes.sequence.limit <= 65536)) {
    /* nothing to do; stack-allocated */
  } else if (arg->direction == In || arg->direction == InOut)
    FreeValue(arg->type, c_argument_name(arg), adRole[arg->direction],
	      context, 6);
}

static void freeArgumentOut(refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  if (arg->direction == Out)
    FreeValue(arg->type, c_argument_name(arg), adRole[arg->direction],
	      context, 8);
}

static void freeReturnValue (Type type, Context context)
{
  FreeValue (type, "_retvalue", role_Return, context, 8);
}

static void sizeArgument(char *name, Type ut, Role role, Context context)
{
  char            buf[1000];
  int             ind;
  ind = Indirectness(ut, role, TRUE, 0);
  sprintf(buf, "%s%s", (ind ? "*" : ""), name);
  fprintf(context->file, "        _size += ");
  SizeValue(context, ut, buf);
  fprintf(context->file, ";\n");
  fprintf(context->file, "        if (ILU_ERRNOK(*_err)) goto errexit;\n");
}

static void sizeArg(refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  if (arg->direction == In)
    return;
  sizeArgument(c_argument_name(arg), arg->type, adRole[arg->direction],
	       context);
}

static void outputArgument (char *name, Type ut, Role role, Context context)
{
  char            buf[1000];
  int             indp, indv;
  indp = Indirectness(ut, role_In, FALSE, 0);
  indv = Indirectness(ut, role, TRUE, 0);
  sprintf(buf, "%s%s",
	  ((indp < indv) ? "*" : (indp > indv) ? "&" : ""),
	  name);
  MarshallValue(context, ut, buf, 6);
}

static void outputArg(refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  if (arg->direction != In)
    outputArgument(c_argument_name(arg), arg->type,
		   adRole[arg->direction], (Context) rock);
}

static void inputArgument (refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  char            buf[1000];
  int             indp, indv;
  if (arg->direction == Out)
    return;
  indv = Indirectness(arg->type, adRole[arg->direction], TRUE, 0);
  indp = Indirectness(arg->type, role_InOut, FALSE, 0);
  sprintf(buf, "%s%s", (indp > indv) ? "&" : "", c_argument_name(arg));
  UnmarshallValue(context, ur_type(arg->type), arg->def, buf, 2);
}

static void declareArg (refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  Type            type = arg->type;
  string          name = c_argument_name(arg);

  if ((arg->direction == In) &&
      (type_ur_kind(type) == sequence_Type) &&
      (type_ur_kind(type_description(type)->structuredDes.sequence.type) == byte_Type) &&
      (type_description(type)->structuredDes.sequence.limit > 0) &&
      (type_description(type)->structuredDes.sequence.limit <= 65536)) {
    fprintf(context->file, "  CORBA_octet _%s__buffer[%lu];\n",
	    name, type_description(type)->structuredDes.sequence.limit);
    fprintf(context->file, "  %s %s = { %lu, 0, &_%s__buffer[0] };\n",
	    c_role_type(type, adRole[arg->direction], TRUE), name,
	    type_description(type)->structuredDes.sequence.limit, name);
  } else {
    fprintf(context->file, "  %s %s = {0};\n",
	    c_role_type(type, adRole[arg->direction], TRUE), name);
  }
}

static void initType(char *name, Type type, Context context, Role role)
{
  TypeKind        t = type_ur_kind(type);
  int             ind = Indirectness(type, role, TRUE, 1);

  if (ind > 0) {
    fprintf(context->file, "  %s = (%s) 0;\n", name,
	    c_role_type(type, role, TRUE));
  } else if (t == array_Type) {
    fprintf(context->file, "  memset ((void *) %s, 0, sizeof(%s));\n",
	    name, c_type_name(type));
  } else if (t == record_Type || t == union_Type || t == pickle_Type ||
	     NonStringSequence(type)) {
    fprintf(context->file, "  memset ((void *) &%s, 0, sizeof(%s));\n",
	    name, c_type_name(type));
  } else {
    fprintf(context->file, "  %s = (%s) 0;\n",
	    name, c_type_name(type));
  }
}

static void initArgument(refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  if (arg == NULL || arg->type == NULL ||
      (type_kind(arg->type) == void_Type))
    return;
  initType(c_argument_name(arg), arg->type, (Context) rock,
	   adRole[arg->direction]);
}

static void initReturnValue (Type t, Context context)
{
  if (t == NIL || type_kind(t) == void_Type)
    return;
  initType ("_retvalue", t, context, role_Return);
}

static void addExceptionOutputFn (refany elt, refany rock)
{
  Exception       e = (Exception) elt;
  Context         context = (Context) rock;
  Exception       ure = e->import ? e->import : e;
  Type            ut = ur_type(ure->type);
  TypeKind        tk = type_kind(ut);
  char            a;
  char           *p = &a;
  FILE           *f = context->file;

  codeType (ut, &p, role_In);
  fprintf(context->file, "%s\n    { (unsigned int) '%c', ",
	  context->counter == 0 ? "" : ",", a);
  context->counter += 1;

  if (tk == object_Type)
    fprintf (f, "&_%s__ILUType, (ILU_C_SizeFn) ", c_type_name(ut));
  else
    fprintf (f, "(ilu_Class *) 0, (ILU_C_SizeFn) ");

  switch (tk)
    {
    case integer_Type:
      fprintf (f, "ilu_SizeOfInteger, (ILU_C_OutputFn) ilu_OutputInteger, 0, 0");
      break;
      
    case cardinal_Type:
      fprintf (f, "ilu_SizeOfCardinal, (ILU_C_OutputFn) ilu_OutputCardinal, 0, 0");
      break;
      
    case shortinteger_Type:
      fprintf (f, "ilu_SizeOfShortInteger, (ILU_C_OutputFn) ilu_OutputShortInteger, 0, 0");
      break;
      
    case shortcardinal_Type:
      fprintf (f, "ilu_SizeOfShortCardinal, (ILU_C_OutputFn) ilu_OutputShortCardinal, 0, 0");
      break;
      
    case longinteger_Type:
      fprintf (f, "ilu_SizeOfLongInteger, (ILU_C_OutputFn) ilu_OutputLongInteger, 0, 0");
      break;
      
    case longcardinal_Type:
      fprintf (f, "ilu_SizeOfLongCardinal, (ILU_C_OutputFn) ilu_OutputLongCardinal, 0, 0");
      break;

    case character_Type:
      fprintf (f, "ilu_SizeOfCharacter, (ILU_C_OutputFn) ilu_OutputCharacter, 0, 0");
      break;

    case shortcharacter_Type:
      fprintf (f, "ilu_SizeOfShortCharacter, (ILU_C_OutputFn) ilu_OutputShortCharacter, 0, 0");
      break;

    case real_Type:
      fprintf (f, "ilu_SizeOfReal, (ILU_C_OutputFn) ilu_OutputReal, 0, 0");
      break;

    case shortreal_Type:
      fprintf (f, "ilu_SizeOfShortReal, (ILU_C_OutputFn) ilu_OutputShortReal, 0, 0");
      break;

    case longreal_Type:
      fprintf (f, "ilu_SizeOfLongReal, (ILU_C_OutputFn) ilu_OutputLongReal, 0, 0");
      break;

    case byte_Type:
      fprintf (f, "ilu_SizeOfByte, (ILU_C_OutputFn) ilu_OutputByte, 0, 0");
      break;

    case boolean_Type:
      fprintf (f, "ILU_C_SizeOfBoolean, (ILU_C_OutputFn) ILU_C_OutputBoolean, 0, 0");
      break;

    case enumeration_Type:
      fprintf (f, "ilu_SizeOfEnum, (ILU_C_OutputFn) ilu_OutputEnum, 0, 0");
      break;

    case object_Type:
      fprintf(f, "0, 0, 0, 0");
      break;

    case sequence_Type:
      if (TypeIsString(ut))
	fprintf(f, "_ilu_CString__SizeOf, "
		"(ILU_C_OutputFn) _ilu_CString__Output, 0, (ILU_C_FreeFn) ilu_CString__Free");
      else
	fprintf(f, "_%s__SizeOf, (ILU_C_OutputFn) _%s__Output, 0, (ILU_C_FreeFn) %s__Free",
		c_type_name(ut), c_type_name(ut), c_type_name(ut));
      break;
      
#ifdef ADD_VARIANT_SUPPORT
    case pickle_Type:
      fprintf(f, "_CORBA_any__SizeOf, "
	      "(ILU_C_OutputFn) _CORBA_any__Output, 0,"
	      "(ILU_C_FreeFn) _CORBA_any__Free");
      break;
#endif /* ADD_VARIANT_SUPPORT */

    case array_Type:
    case record_Type:
    case union_Type:
    case optional_Type:
    case reference_Type:
    case string_Type:
    case fixedpoint_Type:
      fprintf(f,
	      "_%s__SizeOf, (ILU_C_OutputFn) _%s__Output, 0, ",
	      c_type_name(ut), c_type_name(ut));
      if (NeedsFree(ut))
	fprintf (f, "(ILU_C_FreeFn) %s__Free", c_type_name(ut));
      else
	fprintf (f, "0");
      break;

    case void_Type:
      fprintf (f, "0, 0, 0, 0");
      break;

  default:
    fatal ("Error: Bad parameter type %s.\n", c_type_name (ure->type));
  }
  fprintf (f, " }");
}

static void declareExceptionValOutputFns (list exceptions, Context context)
{
  if (exceptions == NULL || list_size(exceptions) < 1)
    return;

  fprintf (context->file, "  static struct _ILU_C_ExceptionDescription_s _evec[] = {");
  context->counter = 0;
  list_enumerate (exceptions, addExceptionOutputFn, context);
  fprintf (context->file, "};\n\n");
}

static void generateCalleeStub (Procedure  m, Context  context)
{
  enum PrimitiveTypes t = type_basic_type(ur_type(m->returnType));

  if (methodInList(c_simple_name(m->name))) {
    MethodRecordID++;
    return;
  }
  addMethodToList(c_simple_name(m->name));

  fprintf(context->file, "static void _%s_%s__truestub(ilu_Call _call, ilu_Error *_err)\n{\n",
	  c_type_name(context->class), c_simple_name(m->name));
  fprintf(context->file, "  %s _h = {0};\n", c_type_name(context->class));
  fprintf(context->file, "  ILU_C_ENVIRONMENT _status;\n");
  fprintf(context->file, "  int _state = 1; /* keeps track of state of args for resource mgmt purposes */\n");
  list_enumerate(m->arguments, declareArg, context);
  if (type_ur_kind(m->returnType) != void_Type)
    fprintf(context->file, "  %s _retvalue = {0};\n",
	    c_role_type(m->returnType, role_Return, TRUE));
  if (list_size(m->exceptions) > 0)
    fprintf (context->file, "  ilu_Method _the_method = ilu_MethodOfCall(_call);\n");
  declareExceptionValOutputFns (m->exceptions, context);
  fprintf(context->file, "\n");

#if 0
  list_enumerate(m->arguments, initArgument, context);
  initReturnValue(m->returnType, context);
#endif
  fprintf(context->file, "  _status.returnCode = ILU_NIL;\n");
  fprintf(context->file, "  _status.ptr = ILU_NIL;\n");
  fprintf(context->file, "  ILU_CLER(*_err);\n");
  fprintf(context->file, "  _state = 2;\n");
  fprintf(context->file, "  _h = (%s) ",
	  c_type_name(context->class));
  if (SINGLETON(context->class))
    fprintf(context->file, "_ILU_C_GetServerSingleton (_call, _err);\n");
  else
    fprintf(context->file,
	    "_ILU_C_InputObject (_call, _%s__ILUType, ilu_TRUE, _err);\n",
	    c_type_name(context->class));
  fprintf (context->file, "  if (ILU_ERRNOK(*_err))\n");
  fprintf (context->file, "    goto errexit;\n");
  list_enumerate(m->arguments, inputArgument, context);
  fprintf(context->file,
	  "  if (_ILU_C_FinishParameters (_call, _h, _err)) {\n");
  fprintf(context->file, "    _ILU_C_SetCallerContext(ilu_CallerPassportOfCall(_call));\n    ");
  if (t != void_Type)
    fprintf(context->file, "_retvalue = ");
  fprintf(context->file, "%s_%s (_h",
	  c_type_name(context->class), c_simple_name(m->name));
  list_enumerate(m->arguments, listArgument, context);
  fprintf(context->file, ", &_status);\n");
  fprintf(context->file, "    _ILU_C_SetCallerContext(ILU_NIL);\n");
  fprintf(context->file, "    _state = 3;\n");
  if (m->asynch) {
    fprintf(context->file, "    /* asynchronous method -- no reply */\n");
    fprintf(context->file, "  }\n");
    fprintf(context->file, "  if (!_ILU_C_NoReply(_call, _err)) goto errexit;\n");
  } else {
    fprintf(context->file, "    /* check for errors */\n");
    fprintf(context->file, "    if (_status.returnCode == ILU_NIL) {\n");
    fprintf(context->file, "      ilu_cardinal _size = 0;\n");
    fprintf(context->file, "      if (ilu_CallNeedsSizing(_call)) {\n");
    fprintf(context->file, "        _size = ilu_BeginSizingReply(_call, %s, _err);\n",
	(list_size(m->exceptions) > 0) ? "ilu_TRUE" : "ilu_FALSE");
    fprintf(context->file, "        if (ILU_ERRNOK(*_err)) goto errexit;\n");
    if (t != void_Type) {
      sizeArgument ("_retvalue", ur_type(m->returnType), role_Return, context);
    }
    list_enumerate(m->arguments, sizeArg, context);
    fprintf(context->file, "      };\n");
    fprintf(context->file,
	    "      if (!_ILU_C_BeginReply (_call, %s, _size, _err)) goto errexit;\n",
	(list_size(m->exceptions) > 0) ? "ilu_TRUE" : "ilu_FALSE");
    if (type_basic_type(ur_type(m->returnType)) != void_Type)
      outputArgument("_retvalue", ur_type(m->returnType), role_Return,
		     context);
    list_enumerate(m->arguments, outputArg, context);
    fprintf(context->file,
	    "      if (! _ILU_C_FinishReply (_call, _err)) goto errexit;\n");
    fprintf(context->file, "    }\n");
    fprintf(context->file, "    else {\n");
    if (list_size(m->exceptions) > 0) {
      fprintf(context->file,
	      "      _ILU_C_SendException (_call, _evec, &_status, _err);\n");
      fprintf (context->file, "      if (ILU_ERRNOK(*_err)) goto errexit;\n");
    } else {
      fprintf(context->file,
	      "      (void) ILU_ERR_CONS1(bad_param, _err, minor, ilu_bpm_some_raise, 6);\n");
    }
    fprintf(context->file, "    }\n");
    fprintf(context->file, "  }\n\n");
  }
  fprintf (context->file, "marshalError:\n");
  fprintf (context->file, "errexit:\n");
  fprintf(context->file, "  _ILU_C_FinishServingCall(_call, _err);\n");
  fprintf(context->file, "  switch (_state) {\n");	/* used for internal errors -- still free args */
  fprintf(context->file, "    case 3: /* after true call */\n");
  fprintf(context->file, "      if (_status.returnCode == ILU_NIL) {\n");
  if (t != void_Type)
    freeReturnValue(m->returnType, context);
  list_enumerate(m->arguments, freeArgumentOut, context);
  if (list_size(m->exceptions) > 0) {
    fprintf (context->file, "      } else {\n");
    fprintf (context->file, "        ilu_Error _lerr = ILU_INIT_NO_ERR;\n");
    /* note that we have to use the method here, as the call struct is now invalid */
    fprintf (context->file, "        _ILU_C_FreeException(_the_method, _evec, &_status, &_lerr);\n");
    fprintf (context->file, "        ILU_HANDLED(_lerr);\n");
  };
  fprintf(context->file, "      };\n");
  fprintf(context->file, "    case 2: /* input args unmarshalled, but before call */\n");
  list_enumerate(m->arguments, freeArgumentIn, context);
  fprintf(context->file, "      _ILU_C_Object_release(_h);\n");
  fprintf(context->file, "    case 1: /* before unmarshalling any arguments */\n"
	  "break;\n    default:\n      break;\n  };\n");
  fprintf(context->file, "  return;\n}\n\n");
}

static void generateServerClassTable (refany elt, refany rock)
{
  Type            t = (Type) elt;
  Context         c = (Context) rock;

  if (type_basic_type(t) != object_Type)
    return;

  fprintf(c->file, "static ILU_C_Class _%s__DefaultClass = 0;\n\n",
	  c_type_name(t));

  fprintf(c->file, "ILU_C_Class %s__SetDefaultClass(ILU_C_Class c)\n", c_type_name(t));
  fprintf(c->file, "{\n  ILU_C_Class oldclass = _%s__DefaultClass;\n", c_type_name(t));
  fprintf(c->file, "  _%s__DefaultClass = c;\n  return oldclass;\n}\n\n", c_type_name(t));
}

static void ParmPerMethod(refany elt, refany rock)
{
  Procedure       m = (Procedure) elt;
  Context         context = (Context) rock;
  fprintf(context->file, "    server_%s_%s,\n",
	  c_type_name(context->class), c_simple_name(m->name));
  return;
}

static void MethodParmsPerType(refany elt, refany rock)
{
  Type            class = (Type) elt, ut;
  Context         context = (Context) rock;
  Class           od;
  ut = ur_type(class);
  od = class_object(ut);
  if (methodInList(c_type_name(ut)))
    return;
  addMethodToList(c_type_name(ut));
  list_enumerate(od->methods, ParmPerMethod, context);
  if (class_object(ut)->superclasses != NULL)
    list_enumerate(class_object(ut)->superclasses,
		   MethodParmsPerType, context);
}

static boolean matchTypeName (refany eltp, refany tp)
{
  char *name = (char *) eltp;
  Type t = (Type) tp;
  char *tname;
  char *iname = NULL;
  int inamelen;

  tname = strchr(name, '.');
  if (tname == NULL) {
    tname = name;
  } else {
    tname += 1;
    iname = name;
    inamelen = (tname - iname) - 1;
  }
  return ((strcmp(tname, type_name(t)) == 0) &&
	  ((iname == NULL) ||
	   ((strlen(interface_name(type_interface(t))) == inamelen) &&
	    (strncmp(iname, interface_name(type_interface(t)), inamelen) == 0))));
}

static void initDefaultClass (refany elt, refany rock)
{
  Type            t = (Type) elt;
  Context         c = (Context) rock;
  if (type_basic_type(t) != object_Type)
    return;
  if (list_find(NoDefaultTrueClassList, matchTypeName, t) != NULL)
    return;
  fprintf(c->file, "  _%s__DefaultClass = %s__MakeClass(\n",
  c_type_name(t), c_type_name(t));
  clearMethodList();
  c->class = t;
  MethodParmsPerType(t, c);
  fprintf(c->file, "    0 /* no finalization */\n");
  fprintf(c->file, "    );\n");
}

static void classSetupStubs (Type  class, Context  context)
{
  Class  od;

  if  (class == NULL || 
       type_basic_type (class) != object_Type || 
       (od = class_object (class)) == NULL)
    return;
  if  (list_size (od->methods) > 0)
    list_enumerate (od->methods, (void (*)(refany, refany)) generateCalleeStub, context);
}


static void generateServerCodeForClass (Type class, Context context)
{
  if ((type_basic_type(class) != object_Type) ||
      (class_object(class)->local))
    return;

  context->class = class;
  clearMethodList ();
  classSetupStubs (class, context);
}

static void initializeStubPointer (Procedure  method, Context  context)
{
  fprintf (context->file, "  ilu_SetMethodStubProc(&_%s__ILUType->cl_methods[%ld], "
	   "(ilu_StubProc) _%s_%s__truestub, _ILU_C_LanguageIndex);;\n",
	   c_type_name (context->class), MethodRecordID++,
	   c_type_name (context->class /*method->object*/),
	   c_simple_name (method->name));
}

static void setupServerStubsInMethodTable (Type  type, Context  context)
{
  Class od;
  
  if  (type == NULL || 
       type_basic_type (type) != object_Type || 
       ((od = class_object (type)) == NULL) ||
       od->local)
    return;
  fprintf (context->file, "\n");
  list_enumerate (od->methods, (void (*)(refany, refany)) initializeStubPointer, context);
}

static void setupServerStubsTable (Type  type, Context  context)
{
  MethodRecordID = 0;
  FirstClass = True;
  context->class = type;
  setupServerStubsInMethodTable (type, context);
}

static void generateHandles (Type  type, Context  context)
{
  char *n = c_type_name(type);

#ifdef ILU_HTTPNG_OBJECTS
  if (!class_object(type)->local) {
#endif

    fprintf (context->file, "ILU_C_OBJECT %s__CreateTrue  (ilu_string instance_handle, ILU_C_Server server, void * data)\n{\n", n);
    fprintf (context->file, "  if (_%s__DefaultClass == ILU_NIL) return ILU_NIL;\n", n);
    fprintf (context->file, "  return (ILU_C_CreateTrueObject (_%s__DefaultClass, instance_handle, server, data, ilu_FALSE));\n}\n\n", c_type_name (type));
    
    fprintf (context->file, "ILU_C_OBJECT %s__OTCreateTrue  (ilu_string instance_handle, ILU_C_Server server, void * data)\n{\n", n);
    fprintf (context->file, "  if (server == ILU_NIL || instance_handle == ILU_NIL || _%s__DefaultClass == ILU_NIL) return ILU_NIL;\n", n);
    fprintf (context->file, "  return (ILU_C_CreateTrueObject (_%s__DefaultClass, instance_handle, server, data, ilu_TRUE));\n}\n\n", c_type_name (type));

#ifdef ILU_HTTPNG_OBJECTS
  }
#endif

  fprintf (context->file, "void %s__SetDefaultClassFinalization  (ILU_C_FinalizationProc f)\n{\n", n);
  fprintf (context->file, "  if (_%s__DefaultClass != ILU_NIL)\n", n);
  fprintf (context->file, "    _%s__DefaultClass->ilucc_finalize = f;\n}\n\n", n);
}

static void 
generateServerRegistrationCode(Interface interface, Context context)
{
  FILE           *f = context->file;
  char           *interface_name = (char *) c_interface_name(interface);

  list_enumerate(interface->classes,
	     (void (*) (refany, refany)) generateHandles, context);
  fprintf(f, "void %s__InitializeServer(void)\n{\n",
	  interface_name);
  fprintf(f, "  extern void _%s__GeneralInitialization(void);\n",
	  c_interface_name(interface));
  fprintf(f, "  static ilu_boolean initialized = ilu_FALSE;\n");
  fprintf(f, "  if (initialized) return;\n  initialized = ilu_TRUE;\n\n");
  fprintf(f, "  _%s__GeneralInitialization ();\n", interface_name);
  if (!NoDefaultTrueClass)
    list_enumerate(interface->classes, initDefaultClass, context);
  list_enumerate(interface->classes,
       (void (*) (refany, refany)) setupServerStubsTable, context);
  fprintf(f, "}\n\n");
}

void generateServerCode (Interface parse, boolean no_default_true_class, list no_default_true_classes, FILE *file)
{
  struct context_s context;
  char           *pc_interfacename;

  context.file = file;
  context.interface = parse;

  NoDefaultTrueClass = no_default_true_class;
  NoDefaultTrueClassList = no_default_true_classes;
  /*
   * get any translation of what the header file for the interface
   * is
   */
  pc_interfacename = interface_header_name(c_interface_name(parse));

  fprintf(file, "#include <stdio.h>\n\n");
  fprintf(file, "#include \"%s.h\"\n\n", pc_interfacename);

  list_enumerate(parse->classes, generateServerClassTable, &context);

  list_enumerate(parse->classes,
	    (void (*) (refany, refany)) generateServerCodeForClass,
		 &context);

  generateServerRegistrationCode(parse, &context);
}
