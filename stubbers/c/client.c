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
/* $Id: client.c,v 1.104 1999/08/03 01:50:30 janssen Exp $ */
/* Last edited by Mike Spreitzer March 17, 1997 12:51 pm PST */

#include "cstubber.h" 

static void generateInstanceFromSBH (Type type, Context context)
{
  char           *tn = c_type_name(context->class);
  fprintf(context->file,
     "%s %s__CreateFromSBH (char *sbh, ILU_C_ENVIRONMENT * env)\n",
	  tn, tn);
  fprintf(context->file, "{\n");
  fprintf(context->file,
    "  return (%s) (ILU_C_SBHToObject(sbh, _%s__ILUType, env));\n",
	  tn, tn);
  fprintf(context->file, "}\n\n");
}

static void generateInstanceFromURL (Type type, Context context)
{
  char           *tn = c_type_name(context->class);
  fprintf(context->file,
     "%s %s__CreateFromURL (char *url, ILU_C_ENVIRONMENT * env)\n",
	  tn, tn);
  fprintf(context->file, "{\n");
  fprintf(context->file,
    "  return (%s) (ILU_C_SBHToObject(url, _%s__ILUType, env));\n",
	  tn, tn);
  fprintf(context->file, "}\n\n");
}

static void declareCallerReturnValue(Type type, Context context)
{
  if  (type_basic_type (type) != void_Type) {
    fprintf (context->file, "  %s _retvalue;\n", c_return_type (type));
  }
}

static void listArgument (refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  fprintf(context->file, ", %s %s",
	  c_parameter_type(arg->type, arg->direction),
	  c_argument_name(arg));
}

void
generateProcDecl(Procedure m, Context c, boolean urname,
		 char *name_prefix, char *name_suffix)
{
  fprintf(c->file, "%s %s%s_%s%s (%s _handle",
	  ((type_basic_type(m->returnType) == void_Type)
	   ? "void" : c_return_type(m->returnType)),
	  name_prefix, c_type_name(urname ? m->object : c->class),
	  c_simple_name(m->name), name_suffix,
	  c_type_name(c->class));
  list_enumerate(m->arguments, listArgument, c);
  fprintf(c->file, ", ILU_C_ENVIRONMENT *_status)");
}

int methodNdxOf (Procedure p, Type t)
{
  list m;
  int  mNdx;
  listElement *ptr;

  mNdx = 0;
  m =  (class_object (t))->methods;
  if  (m == NULL || m->count < 1)
    return (0);
  ptr = m->head;
  while (ptr) {
    if  (ptr->data == p)
      return (mNdx);
    mNdx++;
    ptr = ptr->next;
  }
  return (mNdx);
}

static void sizeArgument (Argument arg, Context context)
{
  char            b[1000];
  Type            ut = ur_type(arg->type);
  TypeKind        t = type_basic_type(ut);

  if (arg->direction == Out)
    return;
  sprintf(b, "%s%s",
	  (t != array_Type &&
	   (arg->direction == InOut || t == union_Type ||
	    t == record_Type || NonStringSequence(ut)))
	  ? "*" : "",
	  c_argument_name(arg));
  fprintf(context->file, "\n     +");
  SizeValue(context, ut, b);
}

static void outputArgument (Argument arg, Context context)
{
  char b[1000];
  Type ut = ur_type(arg->type);

  if  (arg->direction == Out)
    return;
  else if (arg->direction == InOut)
    {
      enum PrimitiveTypes t;

      t = type_basic_type(ut);
      if (TypeIsString(ut) OR
	  TypeIsWString(ut) OR
	  (t != sequence_Type AND
	   t != string_Type AND
	   t != fixedpoint_Type AND
	   t != union_Type AND
	   t != array_Type AND
	   t != pickle_Type AND
	   t != record_Type))
	{
	  *b = '*';
	  strcpy (b+1, c_argument_name(arg));
	}
      else
	strcpy (b, c_argument_name(arg));
    }
  else
    strcpy (b, c_argument_name(arg));
  MarshallValue (context, ut, b, 2);
}

static void TypeIsObj (Type t, boolean *objarg)
{
  if (type_basic_type(t) == object_Type)
    *objarg = TRUE;
}

static void scanForObjectArg (Argument a, boolean *objarg)
{
  type_recurse (a->type, (void (*) (Type, refany)) TypeIsObj, objarg);
}

static void listArg (Context c, char *op, Type t, char *name)
{
  TypeKind        tk = type_ur_kind(t);
  fprintf(c->file, ",\n                       %s", name);

  if (tk == object_Type)
    fprintf (c->file, ", _%s__ILUType", c_type_name(ur_type(t)));
  else if (TypeIsEitherString(t))
    fprintf(c->file, ", (ilu_cardinal) 0x%lx",
	    (unsigned long) (type_description(t)->
			     structuredDes.sequence.limit));
  else if (type_kind(t) == pickle_Type)
    fprintf(c->file, ", _CORBA_any__%s", op);
  else if ((tk != enumeration_Type) &&
	   (!(ur_type(t)->builtIn)))
    fprintf(c->file, ", _%s__%s", c_type_name(ur_type(t)), op);
}

static void listSizeOfArgs (refany elt, refany rock)
{
  Argument        a = (Argument) elt;
  Context         context = (Context) rock;
  if (a->direction == Out)
    return;
  listArg (context, "SizeOf", a->type, c_argument_name(a));
}

static void listOutputArgs (refany elt, refany rock)
{
  Argument        a = (Argument) elt;
  Context         context = (Context) rock;
  if (a->direction == Out)
    return;
  listArg (context, "Output", a->type, c_argument_name(a));
}

static void listInputArgs (refany elt, refany rock)
{
  Argument        a = (Argument) elt;
  Context         context = (Context) rock;
  if (a->direction == In)
    return;
  listArg (context, "Input", a->type, c_argument_name(a));
}

static void codeInputArg (refany elt, refany rock)
{
  Argument        a = (Argument) elt;
  char          **p = (char **) rock;
  if (a->direction != Out)
    codeType (a->type, p, adRole[a->direction]);
}

static void codeOutputArg (refany elt, refany rock)
{
  Argument        a = (Argument) elt;
  char          **p = (char **) rock;
  if (a->direction != In)
    codeType (a->type, p, adRole[a->direction]);
}

static cardinal declareParmDescriptionBlock (Procedure m, Context c)
{
  cardinal	size = 0;

  size = list_size(m->arguments) + ((m->returnType == NULL) ? 0 : 1);

  if (size > 0)
    fprintf (c->file, "  _ILU_C_ParmDesc __parmsBlock[%u];\n", size);
  return size;
}

static boolean IsScalar (Type type)
{
  enum PrimitiveTypes t;

  if (type == NULL)
    return (False);
  t = type_ur_kind (type);
  return  ((t == byte_Type) ||
	   (t == boolean_Type) ||
	   (t == shortinteger_Type) ||
	   (t == integer_Type) ||
	   (t == longinteger_Type) ||
	   (t == shortcardinal_Type) ||
	   (t == cardinal_Type) ||
	   (t == longcardinal_Type) ||
	   (t == shortreal_Type) ||
	   (t == real_Type) ||
	   (t == longreal_Type) ||
	   (t == shortcharacter_Type) ||
	   (t == character_Type));
}

static void initializeParmDescriptionBlock (Procedure m, Context c)
{
  cardinal	i;
  cardinal	element;
  Argument	a;

  element = 0;
  if (m->returnType != NULL) {
      fprintf (c->file, "  __parmsBlock[%u].parm_in  = 0;\n", element);
      fprintf (c->file, "  __parmsBlock[%u].parm_out = 1;\n", element);
      fprintf (c->file, "  __parmsBlock[%u].parm_needs_assignment   = %u;\n", element,
	       ((VariableLength(m->returnType) &&
		 (!TypeIsEitherString(m->returnType)) &&
		 (type_ur_kind(m->returnType) != pickle_Type))
		|| (type_ur_kind(m->returnType) == array_Type)
		|| (type_ur_kind(m->returnType) == object_Type)) ? 1 : 0);
      fprintf (c->file, "  __parmsBlock[%u].parm_needs_dereference  = 0;\n", element);
      fprintf (c->file, "  __parmsBlock[%u].parm_type = %s;\n", element, c_typecode_name(m->returnType));
      fprintf (c->file, "  __parmsBlock[%u].parm_val  = (void *) &_retvalue;\n", element);
      element++;
  };
  /* do input args */
  for (i = 0;  i < list_size(m->arguments);  i++) {
    a = (Argument) list_ref(m->arguments, i);
    if (a->direction == In) {
      fprintf (c->file, "  __parmsBlock[%u].parm_in  = 1;\n", element);
      fprintf (c->file, "  __parmsBlock[%u].parm_out = 0;\n", element);
      fprintf (c->file, "  __parmsBlock[%u].parm_needs_assignment   = 0;\n", element);
      fprintf (c->file, "  __parmsBlock[%u].parm_needs_dereference  = 0;\n", element);
      fprintf (c->file, "  __parmsBlock[%u].parm_type = %s;\n", element, c_typecode_name(a->type));
      fprintf (c->file, "  __parmsBlock[%u].parm_val  = (void *) %s%s;\n", element,
	       (IsScalar(a->type) ||
		(type_ur_kind(a->type) == enumeration_Type)) ? "&" : "", c_argument_name(a));
      element++;
    } else if (a->direction == InOut) {
      fprintf (c->file, "  __parmsBlock[%u].parm_in  = 1;\n", element);
      fprintf (c->file, "  __parmsBlock[%u].parm_out = 1;\n", element);
      fprintf (c->file, "  __parmsBlock[%u].parm_needs_assignment   = %u;\n", element,
	       (type_ur_kind(a->type) == object_Type) ? 1 : 0);
      fprintf (c->file, "  __parmsBlock[%u].parm_needs_dereference  = %u;\n", element,
	       ((type_ur_kind(a->type) == optional_Type) ||
		(type_ur_kind(a->type) == reference_Type) ||
		(type_ur_kind(a->type) == object_Type) ||
		(TypeIsEitherString(a->type))) ? 1 : 0);
      fprintf (c->file, "  __parmsBlock[%u].parm_type = %s;\n", element, c_typecode_name(a->type));
      fprintf (c->file, "  __parmsBlock[%u].parm_val  = (void *) %s;\n", element,
	       c_argument_name(a));
      element++;
    } else if (a->direction == Out) {
      fprintf (c->file, "  __parmsBlock[%u].parm_in  = 0;\n", element);
      fprintf (c->file, "  __parmsBlock[%u].parm_out = 1;\n", element);
      fprintf (c->file, "  __parmsBlock[%u].parm_needs_assignment   = %u;\n", element,
	       ((VariableLength(a->type) &&
		 (!TypeIsEitherString(a->type)) &&
		 (type_ur_kind(a->type) != pickle_Type))
		|| (type_ur_kind(a->type) == object_Type)) ? 1 : 0);
      fprintf (c->file, "  __parmsBlock[%u].parm_needs_dereference  = 0;\n", element);
      fprintf (c->file, "  __parmsBlock[%u].parm_type = %s;\n", element, c_typecode_name(a->type));
      fprintf (c->file, "  __parmsBlock[%u].parm_val  = (void *) %s;\n", element, c_argument_name(a));
      element++;
    }
  }
}

static void addExceptionInputFn (refany elt, refany rock)
{
  Exception       e = (Exception) elt;
  Context         context = (Context) rock;
  Exception       ure = e->import ? e->import : e;
  Type            ut = ur_type(ure->type);
  TypeKind        tk = type_kind(ut);
  FILE           *f = context->file;

  fprintf(context->file, "%s\n    { ", context->counter == 0 ? "" : ",");
  context->counter += 1;

  if (tk == void_Type)
    fprintf(f, "0, ");
  else
    fprintf(f, "sizeof(%s), ", c_type_name(ut));

  if (tk == object_Type)
    fprintf(f, "&_%s__ILUType, ", c_type_name(ut));
  else
    fprintf(f, "ILU_NIL, ");

  switch (tk) {
  case integer_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputInteger, 0");
    break;
  case cardinal_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputCardinal, 0");
    break;

  case shortinteger_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputShortInteger, 0");
    break;
  case shortcardinal_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputShortCardinal, 0");
    break;

  case longinteger_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputLongInteger, 0");
    break;
  case longcardinal_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputLongCardinal, 0");
    break;

  case character_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputCharacter, 0");
    break;
  case shortcharacter_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputShortCharacter, 0");
    break;

  case real_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputReal, 0");
    break;
  case shortreal_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputShortReal, 0");
    break;
  case longreal_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputLongReal, 0");
    break;

  case byte_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputByte, 0");
    break;
  case boolean_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ILU_C_InputBoolean, 0");
    break;

  case enumeration_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) _ILU_C_Enumeration__Input, 0");
    break;

  case object_Type:
    fprintf(f, "0, 0, 0, 0");
    break;

  case sequence_Type:
    if (TypeIsString(ut))
      fprintf(f, "0, 0, (ILU_C_InputFn) _ilu_CString__Input, (ILU_C_FreeFn) ilu_CString__Free");
    else
      fprintf(f, "0, 0, (ILU_C_InputFn) _%s__Input, (ILU_C_FreeFn) %s__Free", c_type_name(ut), c_type_name(ut));
    break;

#ifdef ADD_VARIANT_SUPPORT
  case pickle_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) _CORBA_any__Input, (ILU_C_FreeFn) CORBA_any__Free");
    break;
#endif

  case string_Type:
  case fixedpoint_Type:
  case array_Type:
  case record_Type:
  case union_Type:
  case optional_Type:
  case reference_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) _%s__Input, ", c_type_name(ut));
    if (NeedsFree(ut)) {
	fprintf(f, "(ILU_C_FreeFn) %s__Free", c_type_name(ut));
    } else
      fprintf(f, "0");
    break;

  case void_Type:
    fprintf(f, "0, 0, 0, 0");
    break;

  default:
    fatal("Error: Bad parameter type %s.\n", c_type_name(ure->type));
  }
  fprintf(f, " }");
}

static void declareExceptionValInputFns (list exceptions, Context context)
{
  if (exceptions == NULL || list_size(exceptions) < 1)
    return;
  fprintf(context->file,
      "  static struct _ILU_C_ExceptionDescription_s _evec[] = {");
  context->counter = 0;
  list_enumerate(exceptions, addExceptionInputFn, context);
  fprintf(context->file, "};\n\n");
}

static void generateSiblingChecks (Argument a, Context context)
{
  if ((type_ur_kind(a->type) == object_Type) &&
      a->sibling &&
      ((a->direction == In) || (a->direction == InOut))) {
    fprintf (context->file, "  if (!_ILU_C_CheckSibling(_handle, %s, _status)) goto errout;\n",
	     c_argument_name(a));
  }
}

static boolean findSibling (Argument a, refany junk)
{
  return (a->sibling);
}

static void generateMethodCode (refany elt, refany rock)
{
  Procedure       m = (Procedure) elt;
  Context         context = (Context) rock;
  cardinal	  parms_size;
  TypeKind        t = type_basic_type(ur_type(m->returnType));

  if (methodInList(c_simple_name(m->name))) {
    MethodRecordID++;
    return;
  }
  addMethodToList(c_simple_name(m->name));

  generateProcDecl(m, context, TRUE, "_", "__clientstub");
  fprintf(context->file, "\n{\n");
  

  /* Declare var to hold return value, if any. */
  declareCallerReturnValue(m->returnType, context);
  /*
   * Declare var to hold possible exception value unmarshalling
   * routines, if any.
   */
  declareExceptionValInputFns(m->exceptions, context);

  /*
   * Declare parameter description block
   */
  parms_size = declareParmDescriptionBlock(m, context);

  /*
   * If method is functional, see if it is cached,     and if so,
   * return the cached value.
   */
  if (IsCacheableMethod(m)) {
    /* eventually... */
  }
  /*
   * If return type is an array, we need to allocate a new copy of
   * it, as GenericCall will be expecting the space to be
   * pre-allocated.  Arrays are the only type for which this is
   * necessary.
   */

  /* Check object types for sibling-ness */
  list_enumerate(m->arguments, (iluparser_EnumProc) generateSiblingChecks, context);

  /* Initialize vector of information for the C runtime */
  initializeParmDescriptionBlock(m, context);

  /* generate call to _ILU_C_VectorCall */

  fprintf(context->file,
	  "  _ILU_C_VectorCall (_%s__ILUType, &_%s__ILUType->cl_methods[%d],\n",
	  c_type_name(m->object), c_type_name(m->object),
	  methodNdxOf(m, m->object));
  fprintf(context->file,
	  "                     %s, _handle, _status, %u, %s);\n",
	  list_size(m->exceptions) < 1 ? "ILU_NIL" : "_evec",
	  parms_size, (parms_size > 0) ? "__parmsBlock" : "ILU_NIL");

  /*
   * If we returned an error, release the storage allocated for an
   * array.
   */

  if (list_find(m->arguments, (iluparser_FindProc) findSibling, context) != NULL)
    fprintf (context->file, " errout:\n");
  if (t == array_Type) {
    fprintf(context->file, "  if (!ILU_C_SUCCESSFUL(_status))\n");
    fprintf(context->file, "    ilu_free(_retvalue);\n");
  }
  if (t != void_Type)
    fprintf(context->file, "  return _retvalue;\n");
  else
    fprintf(context->file, "  return;\n");
  fprintf(context->file, "}\n\n");
}

static void mkPrototype (Procedure m, Context c)
{
  enum PrimitiveTypes t = type_basic_type (m->returnType);

  fprintf (c->file,
	   "%s _%s_%s (%s", c_return_type(m->returnType),
	   c_type_name(m->object), c_simple_name(m->name), c_type_name(m->object));
  list_enumerate (m->arguments, listArgumentTypes, c);
  fprintf (c->file, ", ILU_C_ENVIRONMENT *);\n");
}

static void generatePrototypes (    Type        t,     Context     context)
{
  Class       c;

  if  (t == NULL ||
       type_basic_type (t) != object_Type ||
       (c = class_object (t)) == NULL)
    return;
  list_enumerate (c->methods, (void (*)(refany, refany)) mkPrototype, context);
}

static void generateMethodPtrs(refany elt, refany rock)
{
  Procedure       p = (Procedure) elt;
  Context         c = (Context) rock;
  fprintf(c->file, "    (void(*)()) _%s_%s,\n",
	  c_type_name(p->object), c_simple_name(p->name));
}

static void generateClsPtrs (refany elt, refany rock)
{
  Type            t = (Type) elt;
  Context         c = (Context) rock;
  if (class_object(t)->superclasses != NULL)
    list_enumerate(class_object(t)->superclasses, generateClsPtrs, c);
  fprintf(c->file,
	  "  (_ILU_C_MethodBlock *) &_%s__SurrogateMethodBlock,\n",
	  c_type_name(ur_type(t)));
}

static void generateClassTable (Type t, Context c)
{
  char           *tn = c_type_name(t);
  unsigned int    nsc;

  if (OriginalInterface(t) != c->interface)
    return;
#if 1
  fprintf(c->file,
	  "static ILU_C_Class _%s__SurrogateClass = 0;\n",
	  tn);
#else
  nsc = countSuperClasses(t);
  clearMethodList();
  generatePrototypes(t, c);
  if (list_size(class_object(t)->methods) > 0) {
    fprintf(c->file,
	  "static _ILU_C_Method _%s__SurrogateMethodBlock[] = {\n",
	    c_type_name(t));
    list_enumerate(class_object(t)->methods, generateMethodPtrs, c);
    fprintf(c->file, "};\n\n");
  }
  fprintf(c->file,
  "static _ILU_C_DispatchTableSection _%s__DispatchTable[%u] = {\n",
	  tn, nsc + 1);
  /* This class's data must be first in the list. */
  /* Some runtime machinery depends on this. */
  fprintf(c->file, "  {ILU_NIL /* will be set to _%s__ILUType */,\n",
	  c_type_name(t));
  fprintf(c->file,
   "   ILU_NIL /* will be set to _%s__SurrogateMethodBlock */},\n",
	  c_type_name(t));
  fprintf(c->file, "  {ILU_NIL, ILU_NIL},};\n\n");
  fprintf(c->file,
	  "static _ILU_C_Class_struct _%s__SurrogateClass[1] = {\n",
	  tn);
  fprintf(c->file, "  {_%s__DispatchTable, 0}};\n\n",
	  tn);
#endif
}

static void generateClassCode(refany elt, refany rock)
{
  Type            type = (Type) elt;
  Context         context = (Context) rock;
  context->class = type;

#ifdef ILU_HTTPNG_OBJECTS
  if (class_object(type)->local)
    return;
#endif

  MethodRecordID = 0;
  clearMethodList();
  list_enumerate(class_object(type)->methods, generateMethodCode,
		 context);
  generateClassTable(type, context);
  generateInstanceFromSBH(type, context);
}

static void ParmPerMethod(refany elt, refany rock)
{
  Procedure       m = (Procedure) elt;
  Context         context = (Context) rock;
  fprintf(context->file, "    _%s__clientstub,\n",
	  c_procedure_name(m));
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

static void RegisterSurrogateTypes(refany elt, refany rock)
{
  Type            t = (Type) elt;
  Context         c = (Context) rock;
  if (OriginalInterface(t) != c->interface)
    return;
#ifdef ILU_HTTPNG_OBJECTS
  if (class_object(t)->local)
    return;
#endif
  fprintf(c->file, "  _%s__SurrogateClass = %s__MakeClass(\n",
  c_type_name(t), c_type_name(t));
  clearMethodList();
  MethodParmsPerType(t, c);
  fprintf(c->file, "    0 /* no finalization */\n");
  fprintf(c->file, "    );\n");
  fprintf(c->file, "  _ILU_C_RegisterSurrogateCType (_%s__ILUType,\n",
	  c_type_name(t));
  fprintf(c->file, "\t_%s__SurrogateClass);\n",
	  c_type_name(t));
}

static void InitializeImportedInterfaces(refany elt, refany rock)
{
  Imported        i = (Imported) elt;
  Context         c = (Context) rock;
  Interface       i2 = GetInterface(i->name, i->filename);
  if (i2 == NULL) {
    fprintf(stderr, "Can't find interface <%s>\n", i->name);
    return;
  } else if (strcmp(i->name, "ilu") == 0);
  else
    fprintf(c->file, "  %s__Initialize();\n",
	    c_interface_name(i2));
}

void generateClientCode(Interface interface, FILE *file)
{
  struct context_s context;
  char           *pc_interfacename;

  context.file = file;
  context.interface = interface;

  /*
   * get any translation of what the header file for the interface
   * is
   */
  pc_interfacename = interface_header_name(c_interface_name(interface));

  fprintf(file, "#include \"%s.h\"\n\n", pc_interfacename);

  clearMethodList();
  list_enumerate(interface->classes, generateClassCode, &context);

  fprintf(file, "void %s__Initialize(void)\n{\n",
	  c_interface_name(interface));
  fprintf(file, "  extern void _%s__GeneralInitialization(void);\n\n",
	  c_interface_name(interface));
  fprintf(file, "  static ilu_boolean initialized = ilu_FALSE;\n");
  fprintf(file, "  if (initialized) return;\n");
  fprintf(file, "  initialized = ilu_TRUE;\n\n");
  list_enumerate(interface->imports, InitializeImportedInterfaces,
		 &context);
  fprintf(file, "  _%s__GeneralInitialization();\n",
	  c_interface_name(interface));
  list_enumerate(interface->classes, RegisterSurrogateTypes, &context);
  fprintf(file, "  return;\n");
  fprintf(file, "}\n");
}
