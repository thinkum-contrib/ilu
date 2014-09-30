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

/* $Id: code.c,v 1.187 1999/08/17 20:07:35 spreitze Exp $ */
/* Last edited by Mike Spreitzer May 8, 1998 11:43 am PDT */

#include "cstubber.h" 

#include <version.h>

struct double_s {
  Context         c;
  Type            t;
  unsigned int    id;
  Argument        default_arm;
};

static void classIDs (refany elt, refany rock)
{
  Type            class = (Type) elt;
  Context         context = (Context) rock;
  fprintf(context->file, "\t\"%s\",\n", ur_type(class)->uid);
}

static void generateClassTable (refany elt, refany rock)
{
  Type            class = (Type) elt;
  Context         context = (Context) rock;
  char           *tn;
  Class           o;

  if (type_basic_type(class) != object_Type)
    return;
  tn = c_type_name(class);
  o = class_object(class);

  fprintf(context->file, "ilu_Class _%s__ILUType = NULL;\n", tn);

}

static string
  TypeKindName (Type t)
{
  TypeKind tk = type_kind(t);

  if (tk == byte_Type) {
    return "byte";
  } else if (tk == boolean_Type) {
    return "boolean";
  } else if (tk == character_Type) {
    return "character";
  } else if (tk == shortcharacter_Type) {
    return "shortcharacter";
  } else if (tk == shortinteger_Type) {
    return "shortinteger";
  } else if (tk == integer_Type) {
    return "integer";
  } else if (tk == longinteger_Type) {
    return "longinteger";
  } else if (tk == shortcardinal_Type) {
    return "shortcardinal";
  } else if (tk == cardinal_Type) {
    return "cardinal";
  } else if (tk == longcardinal_Type) {
    return "longcardinal";
  } else if (tk == real_Type) {
    return "real";
  } else if (tk == shortreal_Type) {
    return "shortreal";
  } else if (tk == longreal_Type) {
    return "longreal";
  } else if (tk == object_Type) {
    return "object";
  } else if (tk == pipe_Type) {
    return "pipe";
  } else if (tk == optional_Type) {
    return "optional";
  } else if (tk == reference_Type) {
    return "reference";
  } else if (tk == alias_Type) {
    return "alias";
  } else if (tk == union_Type) {
    return "union";
  } else if (tk == sequence_Type) {
    return "sequence";
  } else if (tk == string_Type) {
    return "string";
  } else if (tk == fixedpoint_Type) {
    return "fixedpoint";
  } else if (tk == record_Type) {
    return "record";
  } else if (tk == array_Type) {
    return "array";
  } else if (tk == enumeration_Type) {
    return "enumeration";
  } else if (tk == pickle_Type) {
    return "pickle";
  } else
    return NIL;
}

static void generateTypeTable (refany elt, refany rock)
{
  Type          t = (Type) elt;
  Context       context = (Context) rock;
  string	tn = c_type_name((Type) elt);
  string	tkn = TypeKindName((Type) elt);

  if (t->importInterfaceName != NULL || t->builtIn)
    return;

  if (TypeIsJustAlias(t) || TypeIsString(t)) {
  } else if (type_kind(t) == enumeration_Type) {
    fprintf(context->file, "struct _ILU_C_IoFnsRegistration_s _%s__IoFns"
	    " = { ilu_enumeration_tk, \"%s\", { 0 }, ILU_NIL, _ILU_C_Enumeration__SizeOf, _ILU_C_Enumeration__Output, _ILU_C_Enumeration__Input, 0 };\n",
	    tn, type_uid(t));
  } else if (type_kind(t) == object_Type) {
    fprintf(context->file, "struct _ILU_C_IoFnsRegistration_s _%s__IoFns"
	    " = { ilu_%s_tk, \"%s\", { 0 }, 0, 0, 0, 0, 0 };\n",
	    tn, tkn, type_uid(t));
  } else {
    if (type_kind(t) == fixedpoint_Type) {
      fprintf (context->file, "struct _ILU_C_FixedPointType_s _%s__Type =\n", tn);
      fprintf (context->file, "  { ILU_NIL, ILU_NIL, { 0 }, 0, 0, ilu_fprs_large };\n");
      fprintf (context->file, "ILU_C_FixedPointType %s__Type;\n", tn);
    }
    fprintf(context->file, "struct _ILU_C_IoFnsRegistration_s _%s__IoFns"
	    " = { ilu_%s_tk, \"%s\", { sizeof(%s) }, ILU_NIL, 0, 0, 0, 0 };\n",
	    tn, tkn, type_uid(t), tn);
  }
}

static void 
generateExceptionEntry(refany elt, refany rock)
{
  Exception e = (Exception) elt;
  Context context = (Context) rock;
  if (e->interface == context->interface && e->import == NULL)
    fprintf(context->file,
	    "ILU_C_ExceptionCode _%s__Exception_%s = ILU_NIL;\n",
	    c_interface_name(e->interface), c_simple_name(e->name));
}

static void 
generateExceptionTable(Interface interface, Context context)
{
  list_enumerate(interface->exceptions, generateExceptionEntry,
		 context);
}

static void setExceptionValue (refany elt, refany rock)
{
  Exception       e = (Exception) elt;
  Context         context = (Context) rock;
  FILE           *f = context->file;
  TypeKind        t;
  Type            ut;

  fprintf(f, "  else if (stat->returnCode == ex_%s) {\n",
	  c_exception_name(e));
  if (exception_type(e) == NULL) {
    fprintf(f, "    stat->ptr = ILU_NIL;\n");
    fprintf(f, "    stat->freeRoutine = ((void (*) (void *)) 0);\n");
  }
  else {
    ut = ur_type(exception_type(e));
    t = type_basic_type(ut);
    if (t == enumeration_Type)
      fprintf(f, "    stat->ptr = (void *) ilu_must_malloc (sizeof (ilu_cardinal));\n");
    else
      fprintf(f, "    stat->ptr = (void *) ilu_must_malloc (sizeof (%s));\n",
	      c_type_name(ut));
    if (t == array_Type) {
      char           *rtn = c_return_type(ut);
      fprintf(f, "    memcpy ((void *)((%s)stat->ptr), (void *) va_arg(ap, %s), sizeof(%s));\n",
	      rtn, rtn, c_type_name(ut));
    } else if (t == enumeration_Type) {
      fprintf(f, "    *(ilu_cardinal *)stat->ptr = (ilu_cardinal) va_arg (ap, %s);\n",
	      c_type_name(ut));
    } else
      fprintf(f, "    *(%s*)stat->ptr = %sva_arg (ap, %s);\n",
	      c_type_name(ut),
	      ((t == record_Type || t == union_Type || t == pickle_Type || t == fixedpoint_Type
		|| t == string_Type || (t == sequence_Type && !TypeIsEitherString(ut)))
	       ? "*" : ""),
	      c_parameter_type(ut, In));
    if (TypeIsString(ut))
      fprintf(f, "    stat->freeRoutine = (void (*) (void *)) ilu_CString__Free;\n");
    else if (t == pickle_Type)
      fprintf(f, "    stat->freeRoutine = (void (*) (void *)) CORBA_any__Free;\n");
    else if (HasFreeRoutine(ut))
      fprintf(f, "    stat->freeRoutine = (void (*) (void *)) %s__Free;\n",
	      c_type_name(UltimateType(ut)));
    else
      fprintf(f, "    stat->freeRoutine = ((void (*) (void *)) 0);\n");
  }
  fprintf(f, "  }\n");
}

static void generateSignalException (Context  context)
{
  fprintf(context->file, "#include <stdarg.h>\n\n");
  fprintf(context->file, "void %s__BindExceptionValue (ILU_C_ENVIRONMENT *stat, ilu_Exception exception, ...)\n",
	  c_interface_name(context->interface));
  fprintf(context->file, "{\n");
  fprintf(context->file, "  va_list ap;\n");
  fprintf(context->file, "  va_start (ap, exception);\n");
  fprintf(context->file, "  stat->_major = ILU_C_USER_EXCEPTION;\n");
  fprintf(context->file, "  stat->returnCode = exception;\n");
  fprintf(context->file, "  if (exception == NULL)\n");
  fprintf(context->file, "    /* no exception */;\n");
  list_enumerate(context->interface->exceptions, setExceptionValue,
		 context);
  fprintf(context->file, "  else\n");
  fprintf(context->file,
	  "    _ilu_Assert(0, \"bad exn given to %s__BindExceptionValue\");\n",
	  c_interface_name(context->interface));
  fprintf(context->file, "  va_end (ap);\n");
  fprintf(context->file, "}\n\n");
}

static void generateExceptionProcs (Context  context)
{
  if (list_size(context->interface->exceptions) > 0)
    generateSignalException (context);
}

static int DimStop;
static int DimCount;

static void dimHeader(refany elt, refany rock)
{
  long            d = (long) elt;
  Context         context = (Context) rock;
  if (DimStop >= 0 && DimCount >= DimStop)
    return;

  fprintf(context->file, "%*.*s{\n",
	  DimCount * 2 + 4, DimCount * 2 + 4, "");
  fprintf(context->file, "%*.*sregister int _i%u;\n",
	  DimCount * 2 + 6, DimCount * 2 + 6, "", DimCount);
  fprintf(context->file,
	  "%*.*sfor (_i%u = 0;  _i%u < %lu;  _i%u += 1)\n",
	  DimCount * 2 + 6, DimCount * 2 + 6, "",
	  DimCount, DimCount, d, DimCount);
  DimCount += 1;
}

static void dimRef(refany elt, refany rock)
{
  char           *buf = (char *) rock;
  if (DimStop >= 0 && DimCount >= DimStop)
    return;

  sprintf(buf + strlen(buf), "[_i%u]", DimCount);
  DimCount += 1;
}

static void dimFooter(refany elt, refany rock)
{
  Context         context = (Context) rock;
  DimCount -= 1;
  if (DimStop >= 0 && DimCount < DimStop)
    return;

  fprintf(context->file, "%*.*s}\n",
	  (DimCount - DimStop) * 2 + 2, (DimCount - DimStop) * 2 + 2, "");
}

static void outputRecordArg (Argument arg, Context context)
{
  char            buf[1000];
  Type            ut = ur_type(arg->type);
  TypeKind        t = type_basic_type(ut);

  sprintf(buf, "(%s_val->%s)",
	  ((t == record_Type || t == union_Type || t == pickle_Type || t == fixedpoint_Type ||
	    t == string_Type || (t == sequence_Type && !TypeIsEitherString(ut)))
	   ? "&" : ""),
	  c_argument_name(arg));
  MarshallValue(context, ut, buf, 2);
}

static void caseConst (refany elt, refany rock)
{
  ConstantValue   val = (ConstantValue) elt;
  struct double_s *s = (struct double_s *) rock;
  char *str;

  switch (val->type)
    {
    case integer_Type:
    case shortinteger_Type:
    case cardinal_Type:
    case shortcardinal_Type:
    case byte_Type:
      fprintf (s->c->file, "    case %s%ld:\n", (val->val.i.sign < 0) ? "-" : "", val->val.i.value);
      break;

    case shortcharacter_Type:
      str = c_string(val->val.s);
      fprintf (s->c->file, "    case %s_%s:\n", c_interface_name (s->t->interface), str);
      free(str);
      break;

    case boolean_Type:
      fprintf (s->c->file, "    case ilu_%s:\n", val->val.b ? "TRUE" : "FALSE");
      break;

    default:       error ("illegal discriminator value\n");
    }
}

static void outputUnionType (Argument a, struct double_s * s)
{
  char            buffer[1000];
  int             mkReferent;
  Type            ut = ur_type(a->type);
  TypeKind        t = type_basic_type(ut);
  char           *name;

  if (a->name->base_name != NULL)
    name = (char *) c_simple_name(a->name);
  else
    name = (char *) c_string(type_name(a->type));
  mkReferent = (t == record_Type || t == union_Type || t == pickle_Type || t == fixedpoint_Type ||
		t == string_Type || (t == sequence_Type && !TypeIsEitherString(ut)));
  sprintf(buffer, "%s_val->_u.%s", (mkReferent) ? "&" : "", name);
  if (a == s->default_arm)
    fprintf(s->c->file, "    default:\n");
  else
    list_enumerate(a->values, caseConst, s);
  MarshallValue(s->c, ut, buffer, 6);
  fprintf(s->c->file, "      break;\n");
}

static void ComputeTotalNumberOfElements(refany elt, refany rock)
{
  unsigned long   dim = (unsigned long) elt;
  unsigned long  *total = (unsigned long *) rock;
  *total = *total * dim;
}

static void FindZero(refany elt, refany rock)
{
  boolean        *z = (boolean *) rock;
  if (elt == 0)
    *z = TRUE;
}

string
  KernelTypeKindName (Type type)
{
  switch (type_kind(type))
    {
      case byte_Type: return("ilu_byte_tk");
      case boolean_Type: return("ilu_boolean_tk");
      case character_Type: return("ilu_character_tk");
      case shortcharacter_Type: return("ilu_shortcharacter_tk");
      case shortinteger_Type: return("ilu_shortinteger_tk");
      case integer_Type: return("ilu_integer_tk");
      case longinteger_Type: return("ilu_longinteger_tk");
      case shortcardinal_Type: return("ilu_shortcardinal_tk");
      case cardinal_Type: return("ilu_cardinal_tk");
      case longcardinal_Type: return("ilu_longcardinal_tk");
      case real_Type: return("ilu_real_tk");
      case shortreal_Type: return("ilu_shortreal_tk");
      case longreal_Type: return("ilu_longreal_tk");
      case object_Type: return("ilu_object_tk");
      case optional_Type: return("ilu_optional_tk");
      case reference_Type: return("ilu_reference_tk");
      case alias_Type: return("ilu_alias_tk");
      case pickle_Type: return("ilu_pickle_tk");
      case union_Type: return("ilu_union_tk");
      case sequence_Type: return("ilu_sequence_tk");
      case string_Type: return("ilu_string_tk");
      case fixedpoint_Type: return("ilu_fixedpoint_tk");
      case record_Type: return("ilu_record_tk");
      case array_Type: return("ilu_array_tk");
      case enumeration_Type: return("ilu_enumeration_tk");
      case pipe_Type: return("ilu_enumeration_tk");
      default: return NULL;
    }
}

static void generateOutputCode (Type type, TypeKind t, Context context)
{
  TypeDescription d = type_description (type);
  char buf[1000];

  fprintf(context->file,
	  "void _%s__Output (ilu_Call _call, %s _val, ilu_Error *_err)\n{\n",
	  c_type_name(type), c_parameter_type(type, In));
  if ((t != optional_Type) && ((t != reference_Type) || (!d->structuredDes.reference.optional))) {
    fprintf (context->file, "  if (_val == NULL) {\n");
    fprintf (context->file, "    ILU_ERR_CONS1(bad_param, _err, minor, ilu_bpm_nil, 0);\n");
    fprintf (context->file, "    return;\n  };\n");
  };

  if (t == array_Type)
    {
      unsigned long   size = 1;
      list            dims = d->structuredDes.array.dimensions;
      int             nDims = list_size(dims);
      boolean         zeroarray = FALSE;
      Type            et = d->structuredDes.array.type;
      TypeKind        etk = type_basic_type(et);

      list_enumerate(dims, FindZero, &zeroarray);

      list_enumerate(dims, ComputeTotalNumberOfElements, &size);

      if (etk == byte_Type OR etk == shortcharacter_Type)
	{
	  DimStop = nDims - 1;
	  DimCount = 0;
	  list_enumerate(dims, dimHeader, context);
	  sprintf (buf, "_val");
	  DimCount = 0;
	  list_enumerate (dims, dimRef, buf);
	  fprintf(context->file,
		  "%*.*silu_Output%s (_call,"
		  /* " (%s)" */
		  " (%s), %lu, _err);\n",
		  nDims * 2, nDims * 2, "",
		  (etk == byte_Type) ? "Opaque" : "StringVec",
		  /* (etk == byte_Type) ? "ilu_bytes" : "ilu_string", */
		  buf,
		  (cardinal) list_ref(dims, nDims - 1));
	  DimCount = nDims - 1;
	  DimStop = 0;
	  list_enumerate(dims, dimFooter, context);
	  DimStop = -1;
	}
      else if (etk == character_Type)
	{
	  DimStop = nDims - 1;
	  DimCount = 0;
	  list_enumerate(dims, dimHeader, context);
	  sprintf (buf, "_val");
	  DimCount = 0;
	  list_enumerate (dims, dimRef, buf);
	  fprintf(context->file,
		  "%*.*silu_OutputWStringVec (_call, _val, %lu, _err);\n",
		  nDims * 2 + 2, nDims * 2 + 2, "",
		  (cardinal) list_ref(dims, nDims - 1));
	  DimCount = nDims - 1;
	  DimStop = 0;
	  list_enumerate(dims, dimFooter, context);
	  DimStop = -1;
	}
      else
	{ 
	  TypeKind eurtk = type_ur_kind(et);

	  fprintf (context->file, "  {\n"
		   "#if defined(ADD_TYPE_REGISTRATION_SUPPORT)\n"
		   "    ilu_boolean blockmove = ilu_FALSE;\n"
		   "    ilu_cardinal aligncode;\n"
		   "    _ILU_C_AlignmentProc alignproc;\n"
		   "#endif\n");

	  fprintf (context->file, "    ilu_OutputArray (_call, %lu, ILU_C_KERNEL_TYPE(%s), _err);"
		   " if (ILU_ERRNOK(*_err)) return;\n", size, c_type_name(type));
	  fprintf (context->file,
		   "#if defined(ADD_TYPE_REGISTRATION_SUPPORT)\n"
		   "    blockmove = _ILU_C_CanMoveAsBlock (_call,  ILU_C_KERNEL_TYPE(%s), %lu, &aligncode, &alignproc, _err);\n"
		   "    if (ILU_ERRNOK(*_err)) goto marshalError;\n"
		   "    if (blockmove) {\n"
		   "      if (alignproc != ((_ILU_C_AlignmentProc)0)) {\n"
		   "        (void) (*alignproc) (_call, aligncode, _err);\n"
		   "        if (ILU_ERRNOK(*_err)) goto marshalError;\n"
		   "      };\n"
		   "      ilu_OutputOpaque (_call, (ilu_bytes) _val, sizeof(%s), _err);\n"
		   "      if (ILU_ERRNOK(*_err)) return;\n"
		   "    } else\n"
		   "#endif\n", c_type_name(ur_type(et)), size, c_type_name(type));

	  DimStop = -1;
	  DimCount = 0;
	  list_enumerate(dims, dimHeader, context);
	  sprintf(buf, "%s_val",
		  ((eurtk == record_Type || eurtk == union_Type || eurtk == pickle_Type ||
		    eurtk == string_Type || eurtk == fixedpoint_Type ||
		    (eurtk == sequence_Type && !TypeIsEitherString(et))))
		  ? "&" : "");
	  DimCount = 0;
	  list_enumerate (dims, dimRef, buf);
	  MarshallValue (context, ur_type(et), buf, 2*nDims+6);
	  DimCount = nDims;
	  list_enumerate(dims, dimFooter, context);
	  fprintf (context->file, "  }\n");
	}

      fprintf (context->file, "  ilu_EndArray (_call, _err);\n");
    }

  else if (t == string_Type)
    {
      fprintf (context->file, "  ilu_OutputEString (_call, _val->_buffer, _val->_length, "
	       "%lu, 0x%lx, _val->_charset, _err);\n",
	       (unsigned long) d->structuredDes.string.max_length,
	       (unsigned long) d->structuredDes.string.charset);
    }

  else if (t == fixedpoint_Type)
    {
      fprintf (context->file,
	       "  ilu_OutputFixedpoint (_call, _val->_numerator, _val->_type->min_numerator, "
	       "_val->_type->max_numerator, _val->_type->denominator, "
	       "_val->_type->fixed_digits, _val->_type->fixed_decimal_places, "
	       "_val->_type->range_size, _err);\n");
    }

  else if (t == sequence_Type)
    {
      Type            et = ur_type(d->structuredDes.sequence.type);
      TypeKind        st = type_basic_type(et);

      if (st == byte_Type)
	{
	  fprintf (context->file, "  ilu_OutputBytes (_call, _val->_buffer, _val->_length, %lu, _err);\n",
		   (unsigned long) d->structuredDes.sequence.limit);
	}
      else if (st == shortcharacter_Type)
	{
	  fprintf (context->file, "  ilu_OutputEString (_call, (ilu_bytes) _val,"
		   " _ILU_C_SafeStrlen(_val), %lu, ILU_StringEncoding_latin1,"
		   " ILU_StringEncoding_latin1, _err);\n",
		   (unsigned long) d->structuredDes.sequence.limit);
	}
      else if (st == character_Type)
	{
	  fprintf (context->file, "  _ILU_C_OutputWString (_call, _val, _ILU_C_SafeWStrlen(_val), %lu, _err);\n",
		   (unsigned long) d->structuredDes.sequence.limit);
	}
      else
	{
	  fprintf (context->file, "  {\n"
		   "#if defined(ADD_TYPE_REGISTRATION_SUPPORT)\n"
		   "    ilu_boolean blockmove = ilu_FALSE;\n"
		   "    ilu_cardinal aligncode;\n"
		   "    _ILU_C_AlignmentProc alignproc;\n"
		   "#endif\n");

	  fprintf (context->file, "    ilu_OutputSequence (_call, "
		   "_val->_length, %lu, ILU_C_KERNEL_TYPE(%s), _err);\n",
		   d->structuredDes.sequence.limit, c_type_name(type));
	  fprintf (context->file, "    if (ILU_ERRNOK(*_err)) return;\n");
	  fprintf (context->file,
		   "#if defined(ADD_TYPE_REGISTRATION_SUPPORT)\n"
		   "    blockmove = _ILU_C_CanMoveAsBlock (_call,  ILU_C_KERNEL_TYPE(%s), _val->_length, &aligncode, &alignproc, _err);\n"
		   "    if (ILU_ERRNOK(*_err)) goto marshalError;\n"
		   "    if (blockmove) {\n"
		   "      if (alignproc != ((_ILU_C_AlignmentProc)0)) {\n"
		   "        (void) (*alignproc) (_call, aligncode, _err);\n"
		   "        if (ILU_ERRNOK(*_err)) goto marshalError;\n"
		   "      };\n"
		   "      ilu_OutputOpaque (_call, (ilu_bytes) _val->_buffer, _val->_length * sizeof(%s), _err);\n"
		   "      if (ILU_ERRNOK(*_err)) return;\n"
		   "    } else\n"
		   "#endif\n", c_type_name(et), c_type_name(et));
	  fprintf (context->file, "    {\n    %s *p;  unsigned long i;\n\n", c_type_name (et));
	  fprintf (context->file, "      for (p = _val->_buffer, i = 0;  i < _val->_length;  p++, i++)\n");
	  sprintf(buf, "%sp",
		  (st == record_Type || st == union_Type || st == pickle_Type ||
		   st == string_Type || st == fixedpoint_Type ||
		   (st == sequence_Type && !TypeIsEitherString(et)))
		  ? "" : "*");
	  MarshallValue (context, et, buf, 8);
	  fprintf (context->file, "    }\n"
		   "    ilu_EndSequence (_call, _err);\n  }\n");
	}
    }
  else if (t == union_Type)
    {
      struct double_s  s;

      s.c = context;
      s.t = ur_type(d->structuredDes.uniond.discriminator_type);
      s.id = 0;
      s.default_arm = d->structuredDes.uniond.default_arm;
      fprintf (context->file, "  ilu_OutputUnion (_call, "
	       "_val->_d, %s, ILU_C_KERNEL_TYPE(%s), _err);\n",
	       KernelTypeKindName(s.t), c_type_name(type));
      fprintf (context->file, "  if (ILU_ERRNOK(*_err)) return;\n");
      fprintf (context->file, "  switch (_val->_d) {\n");
      list_enumerate (d->structuredDes.uniond.types, (void (*)(void *,void *)) outputUnionType, &s);
      if (d->structuredDes.uniond.default_arm != NULL)
	; /* handled in outputUnionType */
      else if (d->structuredDes.uniond.others_allowed)
	{
	  fprintf (context->file, "    default:\n      break;\n");
	}
      else
	{
	  fprintf (context->file, "    default:\n");
	  fprintf (context->file, "      fprintf (stderr, \"_%s__Output:  Bad value %%lu in discriminant.\\n\", (unsigned long) _val->_d);\n      break;\n",
		   c_type_name(type));
	}
      fprintf (context->file, "  };\n  ilu_EndUnion (_call, _err);\n");
    }

  else if (t == record_Type)
    {
      fprintf (context->file, "  {\n    _ILU_C_CRInfo s = ILU_NIL;\n");
      fprintf (context->file, "    if ((_%s__IoFns.properties.value_size & 0x80000000) != 0)\n",
	       c_type_name(type));
      fprintf (context->file, "      s = _ILU_C_GetCRInfo(&_%s__IoFns);\n",
	       c_type_name(type));
      fprintf (context->file, "    if (s != ILU_NIL) { _ILU_C_CRPreOutput (s, _val, _err); ");
      fprintf (context->file, "if (ILU_ERRNOK(*_err)) return; };\n");
      fprintf (context->file, "    ilu_OutputRecord (_call, ILU_C_KERNEL_TYPE(%s), _err);\n",
	       c_type_name(type));
      fprintf (context->file, "    if (ILU_ERRNOK(*_err)) return;\n");
      list_enumerate (d->structuredDes.record.fields, (void (*)(void *,void *)) outputRecordArg, context);
      fprintf (context->file, "    ilu_EndRecord (_call, _err);\n");
      fprintf (context->file, "    if (s != ILU_NIL) { _ILU_C_CRPostOutput (s, _val, _err); ");
      fprintf (context->file, "if (ILU_ERRNOK(*_err)) return; };\n  }\n");
    }
#ifdef ILU_HTTPNG_OBJECTS
  else if (t == object_Type)
    {
      list_enumerate (class_object(type)->state, (void (*)(void *,void *)) outputRecordArg, context);
    }
#endif /* def ILU_HTTPNG_OBJECTS */
#ifndef ILU_REFERENCE_TYPES
  else if (t == optional_Type)
    {
      Type            ut2 = ur_type(d->structuredDes.optional);
      int             indo, indp;
      while (type_kind(ut2) == optional_Type)
	ut2 = ur_type(type_description(ut2)->structuredDes.optional);
      fprintf(context->file,
	 "  ilu_OutputOptional (_call, _val != ILU_NIL, ILU_C_KERNEL_TYPE(%s), _err);\n",
	      c_type_name(type));
      fprintf(context->file,
	"  if (ILU_ERRNOK(*_err) || (_val == ILU_NIL)) return;\n");
      indo = !TypeIsPointer(ut2);
      indp = Indirectness(ut2, role_In, FALSE, 0);
      MarshallValue(context, ut2, (indo > indp) ? "*_val" : "_val", 2);
    }
#else /* ILU_REFERENCE_TYPES are defined */
  else if (t == reference_Type || t == optional_Type)
    {
      Type            ut2 = (t == optional_Type)  ? ur_type(d->structuredDes.optional) :
	ur_type(d->structuredDes.reference.base_type);
      int             indo, indp;
      char *	      first_param;
      boolean	      optional = ((t == optional_Type) || d->structuredDes.reference.optional);
      boolean	      aliased = ((t == reference_Type) && d->structuredDes.reference.aliased);
      while ((type_kind(ut2) == reference_Type) || (type_kind(ut2) == optional_Type)) {
	if (type_kind(ut2) == reference_Type)
	  ut2 = ur_type(type_description(ut2)->structuredDes.reference.base_type);
	else
	  ut2 = ur_type(type_description(ut2)->structuredDes.optional);
      }
      if (aliased || optional) {
	fprintf(context->file,
		"  {\n    ilu_boolean first;\n");
	fprintf(context->file,
		"    ilu_OutputReference (_call, _val != ILU_NIL, &first, %s, _err);\n",
		aliased ? "_val" : "(ilu_ReferenceID) 0");
	fprintf(context->file, "    if (ILU_ERRNOK(*_err)) return;\n");
	fprintf(context->file, "    if (ilu_FALSE ");
	if (aliased)
	  fprintf(context->file, "|| !first ");
	if (optional)
	  fprintf(context->file, "|| (_val == ILU_NIL)");
	fprintf(context->file, ") return;\n  };\n");
      }
      indo = !TypeIsPointer(ut2);
      indp = Indirectness(ut2, role_In, FALSE, 0);
      MarshallValue(context, ut2, (indo > indp) ? "*_val" : "_val", 2);
    }
#endif
  else
    {
      fprintf(stderr, "Bad type %s passed to generateOutputCode\n",
	      c_type_name(type));
      exit(1);
    }

  fprintf (context->file, " marshalError:\n  return;\n}\n\n");
}

static void inputRecordArg(refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  char            buf[1000];
  sprintf(buf, "%s_val->%s",
	  Indirectness(arg->type, role_InOut, FALSE, 0) ? "&" : "",
	  c_argument_name(arg));
  UnmarshallValue(context, ur_type(arg->type), 0, buf, 2);
}

static void inputUnionCase(refany elt, refany rock)
{
  Argument        a = (Argument) elt;
  struct double_s *s = (struct double_s *) rock;
  char            buffer[1000];
  Type            ut = ur_type(a->type);
  char           *name;

  if (a->name->base_name != NULL)
    name = (char *) c_simple_name(a->name);
  else
    name = (char *) c_string(type_name(a->type));

  if (a == s->default_arm)
    fprintf(s->c->file, "    default:\n");
  else
    list_enumerate(a->values, caseConst, s);
  sprintf(buffer, "%s_val->_u.%s",
	  Indirectness(ut, role_InOut, FALSE, 0) ? "&" : "",
	  name);
  UnmarshallValue(s->c, ut, 0, buffer, 6);
  fprintf(s->c->file, "      break;\n");
}

/*
  We generate these for a small number of types:  arrays, records, sequences, unions,
  and optional.  Everything else is done explicitly.

  For arrays, the Input routine takes an optional argument of type ArrayType, and
  returns an element of type "BaseType *" (since arrays cannot be directly returned).
  For sequences, records, and unions, the Input routine takes an optional argument of
  type Type *, and returns an element of type Type *.  For optional, the Input
  routine takes an optional argument of type Type *, and returns a value of type
  Type.
*/

static void generateInputCode (Type type, TypeKind t, Context context)
{
  TypeDescription d = type_description(type);
  char           *name = c_type_name(type);
  char           *parm = c_parameter_type(type, InOut);
  char           *rettype = c_role_type(type, role_InpRet, FALSE);
  FILE           *f = context->file;

  context->class = type;

  fprintf(f,
      "%s _%s__Input (ilu_Call _call, %s _ref, ilu_Error *_err)\n",
	  rettype, name, parm);
  fprintf(f, "{\n  %s _val = _ref;\n\n", rettype);
  if (t == array_Type) {
    char            buf[1000];
    list            dims = d->structuredDes.array.dimensions;
    int             nDims = list_size(dims);
    boolean         zeroarray = FALSE;
    Type            type2 = ur_type(d->structuredDes.array.type);
    TypeKind        t2 = type_basic_type(type2);
    unsigned int i;
    unsigned long size = 1;

    for (i = 0;  i < list_size(dims);  i+= 1)
      size = size * ((unsigned) list_ref(dims, i));

    list_enumerate(dims, FindZero, &zeroarray);

    fprintf(f, "  if (_ref == ILU_NIL)\n");
    fprintf(f, "    _val = (%s) ilu_MallocE(sizeof(%s), _err);\n",
	    rettype, name);
    fprintf(f, "  if (_val == ILU_NIL)  /* malloc failure */\n");
    fprintf(f, "    return _val;\n");

    if (t2 == byte_Type || t2 == shortcharacter_Type) {
      DimStop = nDims - 1;
      DimCount = 0;
      list_enumerate(dims, dimHeader, context);
      DimCount = 0;
      sprintf(buf, "%s", "_val");
      list_enumerate(dims, dimRef, buf);
      fprintf(f, "%*.*s{\n", nDims * 2, nDims * 2, "");
      fprintf(f, "%*.*s%s _tmp = %s;\n",
	      nDims * 2 + 2, nDims * 2 + 2, "",
	      (t2 == byte_Type) ? "ilu_opaque" : "ilu_string",
	      buf);
      fprintf(f, "%*.*silu_Input%s (_call, &_tmp, %lu, _err);\n",
	      nDims * 2 + 2, nDims * 2 + 2, "",
	      (t2 == byte_Type) ? "Opaque" : "StringVec",
	      (cardinal) list_ref(dims, nDims - 1));
      fprintf(f, "%*.*s}\n", nDims * 2, nDims * 2, "");
      DimCount = nDims - 1;
      DimStop = 0;
      list_enumerate(dims, dimFooter, context);
      DimStop = -1;
    } else if (t2 == character_Type) {
      DimStop = nDims - 1;
      DimCount = 0;
      list_enumerate(dims, dimHeader, context);
      DimCount = 0;
      sprintf(buf, "_val");
      list_enumerate(dims, dimRef, buf);
      fprintf(f, "%*.*s{\n", nDims * 2, nDims * 2, "");
      fprintf(f, "%*.*silu_character *_tmp = %s;\n",
	      nDims * 2 + 2, nDims * 2 + 2, "", buf);
      fprintf(f,
	   "%*.*silu_InputWStringVec (_call, &_tmp, %lu, _err);\n",
	      nDims * 2 + 2, nDims * 2 + 2, "",
	      (cardinal) list_ref(dims, nDims - 1));
      fprintf(f, "%*.*s}\n", nDims * 2, nDims * 2, "");
      DimCount = nDims - 1;
      DimStop = 0;
      list_enumerate(dims, dimFooter, context);
      DimStop = -1;
    } else {
      fprintf (f, "  {\n"
	       "#if defined(ADD_TYPE_REGISTRATION_SUPPORT)\n"
	       "    ilu_boolean blockmove = ilu_FALSE;\n"
	       "    ilu_cardinal aligncode;\n"
	       "    _ILU_C_AlignmentProc alignproc;\n"
	       "#endif\n");
      fprintf(f, "    ilu_InputArray (_call, ILU_C_KERNEL_TYPE(%s), _err);\n", name);
      fprintf(f, "    if (ILU_ERRNOK(*_err)) goto marshalError;\n");
      fprintf (context->file,
	       "#if defined(ADD_TYPE_REGISTRATION_SUPPORT)\n"
	       "    blockmove = _ILU_C_CanMoveAsBlock (_call, ILU_C_KERNEL_TYPE(%s), %lu, &aligncode, &alignproc, _err);\n"
	       "    if (ILU_ERRNOK(*_err)) goto marshalError;\n"
	       "    if (blockmove) {\n"
	       "      if (alignproc != ((_ILU_C_AlignmentProc)0)) {\n"
	       "        (void) (*alignproc) (_call, aligncode, _err);\n"
	       "        if (ILU_ERRNOK(*_err)) goto marshalError;\n"
	       "      };\n"
	       "      ilu_InputOpaque (_call, (ilu_bytes *) &_val, sizeof(%s), _err);\n"
	       "      if (ILU_ERRNOK(*_err)) goto marshalError;\n"
	       "    } else\n"
	       "#endif\n", c_type_name(type2), size, c_type_name(type));
      DimCount = 0;
      DimStop = -1;
      list_enumerate(dims, dimHeader, context);
      DimCount = 0;
      sprintf(buf, "%s_val",
	      Indirectness(type2, role_InOut, FALSE, 0) ? "&" : "");
      list_enumerate(dims, dimRef, buf);
      UnmarshallValue(context, d->structuredDes.array.type,
		      type->def, buf, nDims * 2 + 2);
      DimCount = nDims;
      list_enumerate(dims, dimFooter, context);
      fprintf(f, "  }\n");
      fprintf(f, "  ilu_EndArray (_call, _err);\n");
    }
  } else if (t == string_Type) {
    fprintf(f, "  ilu_cardinal len = 0, actual_charset = 0;\n");
    fprintf(f, "  if (_ref == ILU_NIL) {\n");
    fprintf(f, "      _val = ilu_MallocE(sizeof(*_val), _err);\n");
    fprintf(f, "      if (ILU_ERRNOK(*_err)) return _val;\n");
    fprintf(f,
	    "      _val->_maximum = 0x%lx;\n"
	    "      _val->_charset = 0x%lx;\n"
	    "      _val->_language = \"%s\";\n"
	    "      _val->_length = 0;\n"
	    "      _val->_buffer = ILU_NIL;\n"
	    "      };\n", (unsigned long) d->structuredDes.string.max_length,
	    (unsigned long) d->structuredDes.string.charset,
	    (d->structuredDes.string.language) ? d->structuredDes.string.language : "i-default");
    fprintf(f, "  ilu_InputEString (_call, &_val->_buffer, &_val->_length, %lu, "
	    "_val->_charset, &actual_charset, _err);\n",
	    (unsigned long) d->structuredDes.string.max_length);
    fprintf(f, "  if (ILU_ERRNOK(*_err)) goto marshalError;\n");
    fprintf(f, "  if ((_val->_charset > 0) && (actual_charset != _val->_charset)) {\n");
    fprintf(f, "    ILU_ERR_CONS0(codeset_incompatible, _err, 0); goto marshalError;\n");
    fprintf(f, "  };\n");
    fprintf(f, "  _val->_charset = actual_charset;\n");
    fprintf(f, "  _val->_maximum = _val->_length;\n");
  } else if (t == fixedpoint_Type) {
    char *name = c_type_name(type);
    fprintf(f, "  ilu_cardinal len = 0;\n");
    fprintf(f,
	    "  if (_ref == ILU_NIL) {\n"
	    "    _val = ilu_MallocE(sizeof(*_val), _err);\n"
	    "    if (ILU_ERRNOK(*_err)) return _val;\n"
	    "    _val->_numerator = ILU_NIL;\n"
	    "  };\n");
    fprintf(f, "  _val->_type = %s__Type;\n", name);
    fprintf(f,
	    "  ilu_InputFixedpoint (_call, &_val->_numerator,\n"
            "                       %s__Type->min_numerator,\n"
	    "                       %s__Type->max_numerator,\n"
	    "                       %s__Type->denominator,\n"
	    "                       %s__Type->fixed_digits,\n"
	    "                       %s__Type->fixed_decimal_places,\n"
	    "                       %s__Type->range_size, _err);\n",
	    name, name, name, name, name, name);
    fprintf(f, "  if (ILU_ERRNOK(*_err)) goto marshalError;\n");
  } else if (t == sequence_Type) {
    Type            ut2 = ur_type(d->structuredDes.sequence.type);
    TypeKind        t2 = type_basic_type(ut2);

    if (t2 == shortcharacter_Type) {
      fprintf(f, "  ilu_cardinal len = 0;\n");
      fprintf(f, "  ilu_cardinal actual_encoding;\n");
      fprintf(f, "  if (_ref == ILU_NIL) {\n");
      fprintf(f, "      _val = ilu_MallocE(sizeof(*_val), _err);\n");
      fprintf(f, "      if (ILU_ERRNOK(*_err)) return _val;\n");
      fprintf(f, "      *_val = ILU_NIL;}\n");
      fprintf(f, "  ilu_InputEString (_call, (ilu_byte **) _val, &len, %lu, "
	      "ILU_StringEncoding_latin1, &actual_encoding, _err);\n", 
	      (unsigned long) d->structuredDes.sequence.limit);
      fprintf(f, "  if (ILU_ERROK(*_err) && (actual_encoding != ILU_StringEncoding_latin1))\n");
      fprintf(f, "  	  ILU_ERR_CONS1(imp_limit, _err, minor, ilu_ilm_unsupported_charset_encoding, 0);\n");
    } else if (t2 == character_Type) {
      fprintf(f, "  ilu_cardinal len = 0;\n");
      fprintf(f, "  if (_ref == ILU_NIL) {\n");
      fprintf(f, "      _val = ilu_MallocE(sizeof(*_val), _err);\n");
      fprintf(f, "      if (ILU_ERRNOK(*_err)) return _val;\n");
      fprintf(f, "      *_val = ILU_NIL;}\n");
      fprintf(f, "  _ILU_C_InputWString (_call, _val, &len, %lu, _err);\n",
	      (unsigned long) d->structuredDes.sequence.limit);
    } else if (t2 == byte_Type) {
      fprintf(f, "  ilu_cardinal len;\n");
      fprintf(f, "  ilu_cardinal limit = %lu;\n", d->structuredDes.sequence.limit);
      fprintf(f, "  ilu_bytes b = ILU_NIL;\n\n");
      fprintf(f, "  if (_ref == ILU_NIL) {\n");
      fprintf(f, "    _val = (%s *) ilu_MallocE(sizeof(%s), _err);\n",
	      name, name);
      fprintf(f, "    if (_val == ILU_NIL)  /* malloc failure */\n");
      fprintf(f, "      return _val;\n");
      fprintf(f, "  } else {\n");
      fprintf(f, "    if ((_ref->_buffer != ILU_NIL) && (_ref->_maximum > 0)) {\n");
      fprintf(f, "      limit = ((%lu != 0) && (_ref->_maximum > %lu)) ? %lu : _ref->_maximum;\n",
	      d->structuredDes.sequence.limit,
	      d->structuredDes.sequence.limit,
	      d->structuredDes.sequence.limit);
      fprintf(f, "      b = _ref->_buffer;\n");
      fprintf(f, "    };\n  };\n");
      fprintf(f, "  ilu_InputBytes (_call, &b, &len, limit, _err);\n");
      fprintf(f, "  if (ILU_ERRNOK(*_err)) goto marshalError;\n");
      fprintf(f, "  _val->_length = len;\n");
      fprintf(f, "  if ((_ref == ILU_NIL) || (b != _ref->_buffer)) {\n");
      fprintf(f, "    _val->_maximum = len;\n");
      fprintf(f, "    _val->_buffer = b;\n  };\n");
    } else {
      fprintf(f, "  ilu_cardinal _count, _index;\n");
      fprintf(f, "  %s _tmp;\n", c_type_name(ut2));
      fprintf (f, "#if defined(ADD_TYPE_REGISTRATION_SUPPORT)\n"
	       "  ilu_boolean blockmove = ilu_FALSE;\n"
	       "  ilu_cardinal aligncode;\n"
	       "  _ILU_C_AlignmentProc alignproc;\n"
	       "#endif\n\n");
      fprintf(f, "  if (_ref == ILU_NIL) {\n");
      fprintf(f, "    _val = (%s) ilu_MallocE(sizeof (%s), _err);\n",
	      parm, name);
      fprintf(f, "    if (_val == ILU_NIL)  /* malloc failure */\n");
      fprintf(f, "      return _val;\n");
      fprintf(f, "  };\n");
      fprintf(f, "  %s_Init(_val, 0, NULL);\n", name);
      fprintf(f, "  ilu_InputSequence (_call, &_count, %lu, ILU_C_KERNEL_TYPE(%s), _err);\n",
	      d->structuredDes.sequence.limit, name);
      fprintf(f, "  if (ILU_ERRNOK(*_err)) goto marshalError;\n");
      fprintf (context->file,
	       "#if defined(ADD_TYPE_REGISTRATION_SUPPORT)\n"
	       "    blockmove = _ILU_C_CanMoveAsBlock (_call, ILU_C_KERNEL_TYPE(%s), _count, &aligncode, &alignproc, _err);\n"
	       "    if (ILU_ERRNOK(*_err)) goto marshalError;\n"
	       "    if (blockmove) {\n"
	       "      if (alignproc != ((_ILU_C_AlignmentProc)0)) {\n"
	       "        (void) (*alignproc) (_call, aligncode, _err);\n"
	       "        if (ILU_ERRNOK(*_err)) goto marshalError;\n"
	       "      };\n"
	       "      ilu_InputOpaque (_call, (ilu_bytes *) &_val->_buffer, _count * sizeof(%s), _err);\n"
	       "      if (ILU_ERRNOK(*_err)) goto marshalError;\n"
	       "      if (_val->_maximum < _count) _val->_maximum = _count;\n"
	       "      _val->_length = _count;\n"
	       "    } else\n"
	       "#endif\n", c_type_name(ut2), c_type_name(ut2));
      fprintf(f, "    for (_index = 0;  _index < _count;  _index++) {\n");
      UnmarshallValue(context, ut2, type->def,
		      (Indirectness(ut2, role_InOut, FALSE, 0)
		       ? "&_tmp" : "_tmp"),
		      6);
      fprintf(f, "      if (ILU_ERRNOK(*_err))\n");
      fprintf(f, "        return _val;\n");
      fprintf(f, "      %s_Append (_val, %s_tmp);\n",
	      c_type_name(type),
	      Indirectness(ut2, role_In, FALSE, 0) ? "&" : "");
      fprintf(f, "    }\n");
      fprintf(f, "  ilu_EndSequence (_call, _err);\n");
    }
  } else if (t == union_Type) {
    struct double_s s;
    s.c = context;
    s.t = ur_type(d->structuredDes.uniond.discriminator_type);
    s.id = 0;
    s.default_arm = d->structuredDes.uniond.default_arm;

    fprintf(f, "  ilu_cardinal tag;\n\n");
    fprintf(f, "  ilu_InputUnion (_call, &tag, %s, ILU_C_KERNEL_TYPE(%s), _err);\n",
	    KernelTypeKindName(s.t), name);
    fprintf(f, "  if (ILU_ERRNOK(*_err))\n");
    fprintf(f, "    return _val;\n");
    fprintf(f, "  if (_ref == ILU_NIL) {\n");
    fprintf(f, "    _val = (%s) ilu_MallocE(sizeof (%s), _err);\n",
	    parm, name);
    fprintf(f, "    if (_val == ILU_NIL)  /* malloc failure */\n");
    fprintf(f, "      return _val;\n");
    fprintf(f, "    memset((void*) _val, 0, sizeof(%s));\n  };\n", name);
    fprintf(f, "  switch (tag) {\n");
    list_enumerate(d->structuredDes.uniond.types, inputUnionCase, &s);
    if (s.default_arm != NULL);
    else if (d->structuredDes.uniond.others_allowed)
      fprintf(f, "    default:\n      break;\n");
    else {
      fprintf(f, "    default:\n");
      fprintf(f, "      fprintf (stderr, \"_%s__Input:  %s\", %s);\n",
	      c_type_name(type),
	      "Bad value %lu received for discriminant.\\n",
	      "(unsigned long) tag");
      fprintf(f, "      break;\n");
    }
    fprintf(f, "  }\n");
    fprintf(f, "  _val->_d = (%s) tag;\n", c_type_name(s.t));
    fprintf(f, "  ilu_EndUnion (_call, _err);\n");
  } else if (t == record_Type) {
    fprintf(f, "  _ILU_C_CRInfo s = ILU_NIL;\n");
    fprintf(f, "  if ((_%s__IoFns.properties.value_size & 0x80000000) != 0)\n",
	     c_type_name(type));
    fprintf(f, "    s = _ILU_C_GetCRInfo(&_%s__IoFns);\n",
	       c_type_name(type));
    fprintf(f, "  if (_ref == ILU_NIL) {\n");
    fprintf(f, "    if (s == ILU_NIL) {\n");
    fprintf(f, "      _val = (%s) ilu_MallocE (sizeof (%s), _err);\n",
	    parm, name);
    fprintf(f, "      if (ILU_ERRNOK(*_err)) return _val;\n");
    fprintf(f, "      memset((void*) _val, 0, sizeof(%s));\n", name);
    fprintf(f, "    } else {\n");
    fprintf(f, "      _val = (%s) _ILU_C_CRCreate (s, sizeof(%s), _err);\n",
	    parm, name);
    fprintf(f, "      if (ILU_ERRNOK(*_err)) return _val;\n    }\n  };\n");
    fprintf(f, "  ilu_InputRecord (_call, ILU_C_KERNEL_TYPE(%s), _err);\n", name);
    list_enumerate(d->structuredDes.record.fields, inputRecordArg, context);
    fprintf(f, "  ilu_EndRecord (_call, _err);  if (ILU_ERRNOK(*_err)) return _val;\n");
    fprintf(f, "  if (s != ILU_NIL) _ILU_C_CRPostInput (s, _val, _err);\n");
#ifdef ILU_HTTPNG_OBJECTS
  } else if (t == object_Type) {
    list_enumerate(class_object(type)->state, (iluparser_EnumProc) inputRecordArg, context);
#endif /* def ILU_HTTPNG_OBJECTS */
#ifndef ILU_REFERENCE_TYPES
  } else if (t == optional_Type) {
    Type            t2 = ur_type(d->structuredDes.optional);
    TypeKind        utk2;
    boolean         isptr;
    int             indv, indp;
    while (type_kind(t2) == optional_Type)
      t2 = ur_type(type_description(t2)->structuredDes.optional);
    utk2 = type_kind(t2);
    fprintf(f, "  ilu_boolean _present;\n");
    fprintf(f, "  ilu_InputOptional (_call, &_present, ILU_C_KERNEL_TYPE(%s), _err);\n", name);
    fprintf(f, "  if (ILU_ERRNOK(*_err)) return ILU_NIL;\n");
    fprintf(f, "  if (_ref == ILU_NIL) {\n");
    fprintf(f, "    _val = (%s *) ilu_MallocE (sizeof (%s), _err);\n",
	    name, name);
    fprintf(f, "    if (ILU_ERRNOK(*_err)) return _val;\n");
    fprintf(f, "    memset((void *) _val, 0, sizeof(%s));\n  };\n", name);
    fprintf(f, "  if (_present)\n    {\n");
    if (isptr = TypeIsPointer(t2))
      0;
    else {
      fprintf(f, "    *_val = (%s*) ilu_MallocE (sizeof (%s), _err);\n",
	      c_type_name(t2), c_type_name(t2));
      fprintf(f, "    if (ILU_ERRNOK(*_err)) return ILU_NIL;\n");
      fprintf(f, "    memset((void *) *_val, 0, sizeof(%s));\n", c_type_name(t2));

    }
    indv = 1 + !isptr;
    indp = Indirectness(t2, role_InOut, FALSE, 0);
    UnmarshallValue(context, t2, 0, "**_val" + 2 - (indv - indp), 6);
    fprintf(f, "    }\n  else *_val = ILU_NIL;\n");
#else
  } else if (t == reference_Type || t == optional_Type) {
    Type            t2 = (t == optional_Type)  ? ur_type(d->structuredDes.optional) :
      ur_type(d->structuredDes.reference.base_type);
    boolean	      optional = ((t == optional_Type) || d->structuredDes.reference.optional);
    boolean	      aliased = ((t == reference_Type) && d->structuredDes.reference.aliased);
    TypeKind        utk2;
    boolean         isptr;
    int             indv, indp;
    boolean	    possibly_missing = (t == optional_Type) ||
      ((t == reference_Type) && (d->structuredDes.reference.optional || d->structuredDes.reference.aliased));
    while ((type_kind(t2) == reference_Type) || (type_kind(t2) == optional_Type)) {
      if (type_kind(t2) == reference_Type)
	t2 = ur_type(type_description(t2)->structuredDes.reference.base_type);
      else
	t2 = ur_type(type_description(t2)->structuredDes.optional);
    }
    if (possibly_missing) {
      fprintf(f, "  ilu_boolean _present;\n  ilu_cardinal _wire_id;\n  ilu_ReferenceID _id;\n");
      fprintf(f, "  _wire_id = ilu_InputReference (_call, &_present, &_id, _err);\n", name);
      fprintf(f, "  if (ILU_ERRNOK(*_err)) return ILU_NIL;\n");
    };
    fprintf(f, "  if (_ref == ILU_NIL) {\n");
    fprintf(f, "    _val = (%s *) ilu_MallocE (sizeof (%s), _err);\n",
	    name, name);
    fprintf(f, "    if (ILU_ERRNOK(*_err)) return _val;\n");
    fprintf(f, "    memset((void *) _val, 0, sizeof(%s));\n  };\n", name);
    if (possibly_missing)
      fprintf(f, "  if (_present)\n    {\n");
    if (aliased)
      fprintf (f, "      if (_wire_id == 0) {\n        *_val = _id;\n      } else {\n");
    if (isptr = TypeIsPointer(t2))
      0;
    else {
      fprintf(f, "      *_val = (%s*) ilu_MallocE (sizeof (%s), _err);\n",
	      c_type_name(t2), c_type_name(t2));
      fprintf(f, "      if (ILU_ERRNOK(*_err)) return ILU_NIL;\n");
      fprintf(f, "      memset((void *) *_val, 0, sizeof(%s));\n", c_type_name(t2));
    }
    indv = 1 + !isptr;
    indp = Indirectness(t2, role_InOut, FALSE, 0);
    UnmarshallValue(context, t2, 0, "**_val" + 2 - (indv - indp), 6);
    if (aliased) {
      fprintf (f, "      ilu_EndInputReference(_call, _wire_id, *_val, _err);\n      }\n");
    };
    if (possibly_missing) {
      if (d->structuredDes.reference.optional)
      	fprintf (f, "    } else\n      *_val = ILU_NIL;\n");
      else
	fprintf (f, "    } else\n      ILU_ERR_CONS1(marshal, _err, minor, ilu_mm_no_val_for_nonopt_ref, 0);\n");
    }
#endif
  } else {
    fprintf(stderr, "Bad type %s passed to generateInputCode\n",
	    c_type_name(type));
    exit(1);
  }
  fprintf(f,
	  " marshalError:\n"
	  "  if (ILU_ERROK(*_err)) return _val;\n"
	  "  if ((_val != ILU_NIL) && (_ref == ILU_NIL))\n"
	  "    ilu_free(_val);\n"
	  "  _val = ILU_NIL;\n"
	  "  return _val;\n}\n\n");
}

static void sizeUnionType (Argument a, struct double_s *s)
{
  char            buffer[1000];
  char           *name;
  Type            ut = ur_type(a->type);

  if (a->name->base_name != NULL)
    name = (char *) c_simple_name (a->name);
  else
    name = (char *) c_string (type_name (a->type));

  if (a == s->default_arm)
    fprintf (s->c->file, "    default:\n");
  else
    list_enumerate (a->values, caseConst, s);

  sprintf (buffer, "_val->_u.%s", name);
  fprintf (s->c->file, "      size += ");
  SizeValue (s->c, ut, buffer);
  fprintf (s->c->file, ";\n      break;\n");
}

static void sizeRecordArg (Argument arg, Context context)
{
  char            buf[1000];
  Type            ut = ur_type(arg->type);

  sprintf(buf, "_val->%s", c_argument_name(arg));
  fprintf(context->file, "  size += ");
  SizeValue(context, ut, buf);
  fprintf(context->file, ";\n");
}

static void generateSizeofCode (Type type, TypeKind t, Context context)
{
  char buf[1000];
  TypeDescription d = type_description (type);

  fprintf (context->file, "ilu_cardinal _%s__SizeOf (ilu_Call _call, %s _val, ilu_Error *_err)\n{\n",
	   c_type_name(type), c_parameter_type (type, In));
  fprintf (context->file, "  ilu_cardinal size = 0;\n\n");
  if (t == array_Type)
    {
      unsigned long   size = 1;
      list            dims = d->structuredDes.array.dimensions;
      int             nDims = list_size(dims);
      boolean         zeroarray = FALSE;
      Type            et = d->structuredDes.array.type;
      TypeKind        etk = type_basic_type(et);

      list_enumerate(dims, FindZero, &zeroarray);
      list_enumerate(dims, ComputeTotalNumberOfElements, &size);

      if (etk == byte_Type OR etk == shortcharacter_Type)
	{
	  DimStop = nDims - 1;
	  DimCount = 0;
	  list_enumerate(dims, dimHeader, context);
	  sprintf (buf, "_val");
	  DimCount = 0;
	  list_enumerate (dims, dimRef, buf);
	  fprintf(context->file,
		  "%*.*ssize += ilu_SizeOf%s (_call,"
		  /* " (%s)" */
		  " (%s), %lu, _err);\n",
		  nDims * 2, nDims * 2, "",
		  (etk == byte_Type) ? "Opaque" : "StringVec",
		  /* (etk == byte_Type) ? "ilu_bytes" : "ilu_string", */
		  buf,
		  (cardinal) list_ref(dims, nDims - 1));
	  DimCount = nDims;
	  DimStop = 1;
	  list_enumerate(dims, dimFooter, context);
	  DimStop = -1;
	}
      else if (etk == character_Type)
	{
	  DimStop = nDims - 1;
	  DimCount = 0;
	  list_enumerate(dims, dimHeader, context);
	  sprintf (buf, "_val");
	  DimCount = 0;
	  list_enumerate (dims, dimRef, buf);
	  fprintf(context->file,
		  "%*.*ssize += ilu_SizeOfWStringVec (_call, _val, %lu, _err);\n",
		  nDims * 2 + 2, nDims * 2 + 2, "",
		  (cardinal) list_ref(dims, nDims - 1));
	  DimCount = nDims;
	  DimStop = 1;
	  list_enumerate(dims, dimFooter, context);
	  DimStop = -1;
	}
      else
	{
	  fprintf (context->file,
		   "#if defined(ADD_TYPE_REGISTRATION_SUPPORT)\n"
		   "  ilu_boolean blockmove = ilu_FALSE;\n"
		   "  ilu_cardinal aligncode;\n"
		   "  _ILU_C_AlignmentProc alignproc;\n"
		   "#endif\n\n");

	  fprintf (context->file, "  size = ilu_SizeOfArray (_call, %lu, ILU_C_KERNEL_TYPE(%s), _err);\n",
		   size, c_type_name(type));
	  fprintf (context->file, "  if (ILU_ERRNOK(*_err)) return 0;\n");

	  fprintf (context->file,
		   "#if defined(ADD_TYPE_REGISTRATION_SUPPORT)\n"
		   "  blockmove = _ILU_C_CanMoveAsBlock (_call,  ILU_C_KERNEL_TYPE(%s), %lu, &aligncode, &alignproc, _err);\n"
		   "  if (ILU_ERRNOK(*_err)) return 0;\n"
		   "  if (blockmove) {\n"
		   "    if (alignproc != ((_ILU_C_AlignmentProc)0)) {\n"
		   "      size += (*alignproc) (_call, aligncode, _err);\n"
		   "      if (ILU_ERRNOK(*_err)) return 0;\n"
		   "    };\n"
		   "    size += ilu_SizeOfOpaque(_call, (ilu_bytes)_val, sizeof(%s), _err);\n"
		   "    if (ILU_ERRNOK(*_err)) return 0;\n"
		   "  } else\n"
		   "#endif\n", c_type_name(ur_type(et)), size, c_type_name(type));

	  DimStop = -1;
	  DimCount = 0;
	  list_enumerate(dims, dimHeader, context);
	  sprintf(buf, "_val");
	  DimCount = 0;
	  list_enumerate (dims, dimRef, buf);
	  fprintf (context->file, "%*.*ssize += ", nDims*2+6, nDims*2+6, "");
	  SizeValue (context, ur_type(et), buf);
	  fprintf (context->file, ";\n");
	  DimCount = nDims;
	  list_enumerate(dims, dimFooter, context);
	}

      fprintf (context->file, "  ilu_EndArray (_call, _err);\n");
    }

  else if (t == string_Type)
    {
      fprintf (context->file, "  size = ilu_SizeOfEString (_call, _val->_buffer,"
	       " _val->_length, %lu, 0x%lx, _val->_charset, _err);\n",
	       (unsigned long) d->structuredDes.string.max_length,
	       (unsigned long) d->structuredDes.string.charset);
    }

  else if (t == fixedpoint_Type)
    {
      fprintf (context->file, "  size = ilu_SizeOfFixedpoint (_call, _val->_numerator,"
	       " _val->_type->min_numerator, _val->_type->max_numerator,"
	       " _val->_type->denominator, _val->_type->fixed_digits, "
	       "_val->_type->fixed_decimal_places, _val->_type->range_size, _err);\n");
    }

  else if (t == sequence_Type)
    {
      Type            et = d->structuredDes.sequence.type;
      Type            ut = ur_type(et);
      TypeKind        st = type_basic_type(ut);

      if (st == byte_Type)
	{
	  fprintf (context->file, "  size = ilu_SizeOfBytes (_call, _val->_buffer, _val->_length, %lu, _err);\n",
		   (unsigned long) d->structuredDes.sequence.limit);
	}
      else if (st == shortcharacter_Type)
	{
	  fprintf (context->file, "  size = ilu_SizeOfEString (_call, (ilu_bytes) _val,"
		   " _ILU_C_SafeStrlen(_val), %lu, ILU_StringEncoding_latin1,"
		   " ILU_StringEncoding_latin1, _err);\n",
		   (unsigned long) d->structuredDes.sequence.limit);
	}
      else if (st == character_Type)
	{
	  fprintf (context->file, "  size = _ILU_C_SizeOfWString (_call, _val, _ILU_C_SafeWStrlen(_val), %lu, _err);\n",
		   (unsigned long) d->structuredDes.sequence.limit);
	}
      else
	{
	  fprintf (context->file,
		   "#if defined(ADD_TYPE_REGISTRATION_SUPPORT)\n"
		   "  ilu_boolean blockmove = ilu_FALSE;\n"
		   "  ilu_cardinal aligncode;\n"
		   "  _ILU_C_AlignmentProc alignproc;\n"
		   "#endif\n\n");

	  fprintf (context->file, "  size = ilu_SizeOfSequence (_call, "
		   "_val->_length, %lu, ILU_C_KERNEL_TYPE(%s), _err);\n",
		   d->structuredDes.sequence.limit, c_type_name(type));
	  fprintf (context->file, "  if (ILU_ERRNOK(*_err)) return 0;\n");
	  fprintf (context->file,
		   "#if defined(ADD_TYPE_REGISTRATION_SUPPORT)\n"
		   "  blockmove = _ILU_C_CanMoveAsBlock (_call,  ILU_C_KERNEL_TYPE(%s), _val->_length, &aligncode, &alignproc, _err);\n"
		   "  if (ILU_ERRNOK(*_err)) return 0;\n"
		   "  if (blockmove) {\n"
		   "    if (alignproc != ((_ILU_C_AlignmentProc)0)) {\n"
		   "      size += (*alignproc) (_call, aligncode, _err);\n"
		   "      if (ILU_ERRNOK(*_err)) return 0;\n"
		   "    };\n"
		   "    size += ilu_SizeOfOpaque (_call, (ilu_bytes) (_val->_buffer), _val->_length * sizeof(%s), _err);\n"
		   "    if (ILU_ERRNOK(*_err)) return 0;\n"
		   "  } else\n"
		   "#endif\n", c_type_name(ut), c_type_name(et));

	  fprintf (context->file, "  {\n    %s *p;  unsigned long i;\n\n", c_type_name (et));
	  fprintf (context->file, "    for (p = _val->_buffer, i = 0;  i < _val->_length;  p++, i++)\n      size += ");
	  SizeValue (context, ut, "*p");
	  fprintf (context->file, ";\n  }\n  ilu_EndSequence (_call, _err);\n");
	}
    }

  else if (t == union_Type)
    {
      struct double_s  s;

      s.c = context;
      s.t = ur_type(d->structuredDes.uniond.discriminator_type);
      s.id = 0;
      s.default_arm = d->structuredDes.uniond.default_arm;

      fprintf (context->file, "  size = ilu_SizeOfUnion (_call, _val->_d, %s, ILU_C_KERNEL_TYPE(%s), _err);\n",
	       KernelTypeKindName(s.t), c_type_name(type));
      fprintf (context->file, "  if (ILU_ERRNOK(*_err)) return 0;\n");
      fprintf (context->file, "  switch (_val->_d) {\n");
      list_enumerate (d->structuredDes.uniond.types, (void (*)(void *,void *)) sizeUnionType, &s);
      if (s.default_arm != NULL)
	;
      else if (d->structuredDes.uniond.others_allowed)
	fprintf (context->file, "    default:\n      break;\n");
      else
	fprintf (context->file, "    default:\n      fprintf (stderr, \"_%s__SizeOf:  Bad value %%lu in discriminant.\\n\", (unsigned long) _val->_d);\n      break;\n", c_type_name(type));
      fprintf (context->file, "  };\n  ilu_EndUnion (_call, _err);\n");
    }

  else if (t == record_Type)
    {
      fprintf (context->file, "  _ILU_C_CRInfo s = ILU_NIL;\n");
      fprintf (context->file, "  if ((_%s__IoFns.properties.value_size & 0x80000000) != 0)\n",
	       c_type_name(type));
      fprintf (context->file, "    s = _ILU_C_GetCRInfo(&_%s__IoFns);\n",
	       c_type_name(type));
      fprintf (context->file, "    if (s != ILU_NIL) { _ILU_C_CRPreOutput (s, _val, _err); ");
      fprintf (context->file, "if (ILU_ERRNOK(*_err)) return 0; };\n");
      fprintf (context->file, "  size = ilu_SizeOfRecord (_call, ILU_C_KERNEL_TYPE(%s), _err);\n",
	       c_type_name(type));
      fprintf (context->file, "  if (ILU_ERRNOK(*_err)) return 0;\n");
      list_enumerate (d->structuredDes.record.fields, (void (*)(void *,void *)) sizeRecordArg, context);
      fprintf (context->file, "  ilu_EndRecord (_call, _err);\n");
    }
#ifdef ILU_HTTPNG_OBJECTS
  else if (t == object_Type)
    {
      list_enumerate (class_object(type)->state, (iluparser_EnumProc) sizeRecordArg, context);
    }
#endif /* def ILU_HTTPNG_OBJECTS */
#ifndef ILU_REFERENCE_TYPES
  else if (t == optional_Type)
    {
      int             indo;
      Type            ut2 = ur_type(d->structuredDes.optional);
      while (type_kind(ut2) == optional_Type)
	ut2 = ur_type(type_description(ut2)->structuredDes.optional);
      indo = !TypeIsPointer(ut2);
      fprintf(context->file,
	      "  size = ilu_SizeOfOptional (_call, _val != ILU_NIL, ILU_C_KERNEL_TYPE(%s), _err);\n",
	      c_type_name(type));
      fprintf (context->file, "  if (ILU_ERRNOK(*_err)) return 0;\n");
      fprintf (context->file, "  if (_val != ILU_NIL)\n    size += ");
      SizeValue (context, ut2, indo ? "*_val" : "_val");
      fprintf (context->file, ";\n");
    }
#else
  else if (t == reference_Type || t == optional_Type)
    {
      Type            ut2 = (t == optional_Type)  ? ur_type(d->structuredDes.optional) :
	ur_type(d->structuredDes.reference.base_type);
      int             indo, indp;
      char *	      first_param;
      boolean	      optional = ((t == optional_Type) || d->structuredDes.reference.optional);
      boolean	      aliased = ((t == reference_Type) && d->structuredDes.reference.aliased);
      while ((type_kind(ut2) == reference_Type) || (type_kind(ut2) == optional_Type)) {
	if (type_kind(ut2) == reference_Type)
	  ut2 = ur_type(type_description(ut2)->structuredDes.reference.base_type);
	else
	  ut2 = ur_type(type_description(ut2)->structuredDes.optional);
      }
      if (aliased || optional) {
	fprintf(context->file, "  {\n");
	fprintf(context->file, "  ilu_boolean first;\n");
	fprintf(context->file,
		"  size = ilu_SizeOfReference (_call, _val != ILU_NIL, &first, %s, _err);\n",
		aliased ? "_val" : "(ilu_ReferenceID) 0");
	fprintf(context->file, "  if (ILU_ERRNOK(*_err)) return 0;\n");
      };
      fprintf(context->file, "  if (ilu_TRUE");
      if (optional)
	fprintf(context->file, " && (_val != ILU_NIL)");
      if (aliased)
	fprintf(context->file, " && first");
      fprintf(context->file, ")\n    size += ");
      indo = !TypeIsPointer(ut2);
      indp = Indirectness(ut2, role_In, FALSE, 0);
      SizeValue (context, ut2, indo ? "*_val" : "_val");
      fprintf (context->file, ";\n");
      if (aliased || optional)
	fprintf (context->file, "  }\n");
    }
#endif
  else if (t == object_Type)
    ; /* done elsewhere */
  else
    {};

  fprintf (context->file, "  return size;\n}\n\n");
}

static void FreeRecordField(refany elt, refany rock)
{
  Argument        field = (Argument) elt;
  Context         context = (Context) rock;
  char            buf[1000];

  sprintf(buf, "_val->%s", c_argument_name(field));
  FreeValue(field->type, buf, role_Val, context, 2);
}

static void FreeUnionField(refany elt, refany rock)
{
  Argument        a = (Argument) elt;
  struct double_s *s = (struct double_s *) rock;
  char            buffer[1000];
  Type            ut = ur_type(a->type);
  char           *name;

  if (a->name->base_name != NULL)
    name = (char *) c_simple_name(a->name);
  else
    name = (char *) c_string(type_name(a->type));
  sprintf(buffer, "_val->_u.%s", name);
  if (a == s->default_arm)
    fprintf(s->c->file, "    default:\n");
  else
    list_enumerate(a->values, caseConst, s);
  FreeValue(ut, buffer, role_Val, s->c, 6);
  fprintf(s->c->file, "      break;\n");
}

static void generateFreeCode (Type type, TypeKind t, Context context)
{
  TypeDescription d = type_description(type);
  /*
   * Generate the proc to go in the freeRoutine slot of a
   * CORBA_Environment.  The argument is of type c_role_type(type,
   * role_Exn, FALSE).
   */

  fprintf(context->file, "void %s__Free (%s _val)\n{\n",
	  c_type_name(type), c_role_type(type, role_Exn, FALSE));
  fprintf(context->file,
	  "  /* What you put in the freeRoutine member of a CORBA_Environment for an exception parameterized by a %s */\n",
	  c_type_name(type));
  fprintf(context->file,
	  "  /* frees allocated storage inside _val (if any), but does not free(_val) */\n");

  if (t == record_Type) {
    fprintf(context->file, "  _ILU_C_CRInfo s = ILU_NIL;\n");
    fprintf(context->file, "  if (_val == ILU_NIL) return;\n");
    fprintf(context->file, "  if ((_%s__IoFns.properties.value_size & 0x80000000) != 0)\n",
	     c_type_name(type));
    fprintf(context->file, "    s = _ILU_C_GetCRInfo(&_%s__IoFns);\n",
	     c_type_name(type));
    fprintf(context->file, "  if (s != ILU_NIL) _ILU_C_CRFree (s, _val);\n");
    list_enumerate(type_description(type)->structuredDes.record.fields,
		   FreeRecordField, context);
  } else if (t == union_Type) {
    struct double_s s;

    s.c = context;
    s.t = ur_type(d->structuredDes.uniond.discriminator_type);
    s.id = 0;
    s.default_arm = d->structuredDes.uniond.default_arm;

    fprintf(context->file, "  if (_val == ILU_NIL) return;\n");
    fprintf(context->file, "  switch (_val->_d) {\n");
    list_enumerate(d->structuredDes.uniond.types,
		   (void (*) (void *, void *)) FreeUnionField, &s);
    if (d->structuredDes.uniond.default_arm != NULL)
      /* handled in FreeUnionField */
      ;
    else if (d->structuredDes.uniond.others_allowed) {
      fprintf(context->file, "    default:\n      break;\n");
    } else {
      fprintf(context->file, "    default:\n");
      fprintf(context->file, "      fprintf (stderr, \"%s__Free:  Bad value %%lu in discriminant.\\n\", (unsigned long) _val->_d);\n", c_type_name(type));
      fprintf(context->file, "      break;\n");
    }
    fprintf(context->file, "  };\n");
  } else if (t == string_Type) {
    fprintf(context->file, "  if (_val == ILU_NIL) return;\n");
    fprintf(context->file, "  if (_val->_buffer != ILU_NIL) ilu_free(_val->_buffer);\n");
  } else if (t == fixedpoint_Type) {
    fprintf(context->file, "  if (_val == ILU_NIL) return;\n");
    fprintf(context->file, "  if (_val->_numerator != ILU_NIL) ilubignum_FreeValue(_val->_numerator);\n");
  } else if (t == sequence_Type) {
    Type            st = ur_type(d->structuredDes.sequence.type);
    TypeKind        t2 = type_basic_type(st);
    fprintf(context->file, "  if (_val == ILU_NIL) return;\n");
    if (t2 == shortcharacter_Type || t2 == character_Type) {
      fprintf(context->file, "  if (*_val != ILU_NIL)\n");
      fprintf(context->file, "    ilu_free(*_val);\n");
    } else {
      fprintf(context->file, "  if (_val->_buffer != NULL) {\n");
      if (NeedsFree(st)) {
	fprintf(context->file, "    unsigned long i;\n");
	fprintf(context->file,
		"    for (i = 0;  i < _val->_length;  i++)\n");
	FreeValue(st, "_val->_buffer[i]", role_Val, context, 6);
      }
      fprintf(context->file, "    ilu_free(_val->_buffer);\n  };\n");
    }
  } else if (t == array_Type) {

    if (NeedsFree(d->structuredDes.array.type)) {
      char            buf[1000];
      list            dims = d->structuredDes.array.dimensions;
      int             nDims = list_size(dims);

      DimStop = -1;
      DimCount = 0;
      list_enumerate(dims, dimHeader, context);
      sprintf(buf, "(*_val)");
      DimCount = 0;
      list_enumerate(dims, dimRef, buf);
      FreeValue(d->structuredDes.array.type, buf, role_Val, context,
		2 * nDims + 4);
      DimCount = nDims;
      list_enumerate(dims, dimFooter, context);
    } else {
      fprintf(context->file,
	      "  /* simple element type, nothing to free */\n");
    }
  } else if (t == optional_Type) {
    Type            st = ur_type(d->structuredDes.optional);
    int             indo;
    while (type_kind(st) == optional_Type)
      st = ur_type(type_description(st)->structuredDes.optional);
    indo = !TypeIsPointer(st);
    fprintf(context->file, "  /* subtype is %s */\n", c_type_name(st));
    fprintf(context->file, "  if ((_val != ILU_NIL) && (*_val != ILU_NIL)) {\n");
    FreeValue(st, (indo ? "**_val" : "*_val"), role_Val, context, 4);
    if (indo)
      fprintf(context->file, "    ilu_free(*_val);\n");
    fprintf(context->file, "  }\n");
  } else if (t == reference_Type) {
    Type            st = ur_type(d->structuredDes.reference.base_type);
    int             indo;
    while (type_kind(st) == reference_Type)
      st = ur_type(type_description(st)->structuredDes.reference.base_type);
    indo = !TypeIsPointer(st);
    fprintf(context->file, "  /* subtype is %s */\n", c_type_name(st));
    fprintf(context->file, "  if ((_val != ILU_NIL) && (*_val != ILU_NIL)) {\n");
    FreeValue(st, (indo ? "**_val" : "*_val"), role_Val, context, 4);
    if (indo)
      fprintf(context->file, "    ilu_free(*_val);\n");
    fprintf(context->file, "  }\n");
  } else if (t == object_Type) {
    fprintf(context->file, "  CORBA_Environment env = {0};\n");
    fprintf(context->file, "  if ((_val == ILU_NIL) || (*_val == ILU_NIL)) return;\n");
    fprintf(context->file, "  CORBA_Object_release(*_val, &env);\n");
    fprintf(context->file, "  *_val = 0;\n");
  } else if (t == pickle_Type) {
    fprintf(context->file, "  if (_val != ILU_NIL) CORBA_any__Free (_val);\n");
  } else {
    fprintf(stderr, "Bad type %s passed to generateFreeCode\n",
	    c_type_name(type));
    exit(1);
  }
  fprintf(context->file, "}\n\n");
}

static void generateBufAllocCode (Type type, TypeKind t, Context context)
{
  char *s = c_type_name(type);

  fprintf (context->file, "%s *CORBA_sequence_%s_allocbuf (CORBA_unsigned_long _count)\n{\n",
	   s, (type->builtIn && strncmp(s, "CORBA_", 6) == 0) ? s + 6 : s);
  fprintf (context->file, "  %s *_p;\n  CORBA_unsigned_long _size = sizeof(%s) * _count;\n\n",
	   s, s);
  fprintf (context->file, "  if ((_p = (%s *) ilu_malloc(_size)) == ILU_NIL)\n", s);
  fprintf (context->file, "    { _ILU_C_MallocFailure(_size); return 0; }\n");
  fprintf (context->file, "  else\n    { memset((void *) _p, 0, _size);  return _p; }\n}\n\n");
}

static void generateAllocCode (Type type, TypeKind t, Context context)
{
  char *s = c_type_name(type);
  char *p = (type_kind(type) == array_Type) ? c_return_type(type) : c_parameter_type(type, InOut);

  fprintf (context->file, "%s %s__alloc ()\n{\n", p, s);
  fprintf (context->file, "  return ((%s) CORBA_sequence_%s_allocbuf(1));\n}\n\n", p,
	   (type->builtIn && strncmp(s, "CORBA_", 6) == 0) ? s + 6 : s);
}

#ifdef ILU_HTTPNG_OBJECTS
static void generateLocalObjectIOCode (Type type, TypeKind t, Context context)
{
  /* sizeof */
}
#endif /* def ILU_HTTPNG_OBJECTS */

static void generateTypeIoCode (Type type, Context context)
{
  TypeKind        t = type_basic_type(type);

  if (type->supertype != NULL
      OR type->builtIn
      OR type->interface != context->interface
      OR type->importInterfaceName != NULL)
    return;

  if (type->importInterfaceName == NULL AND
      (t == union_Type OR
       t == record_Type OR
       t == optional_Type OR
       t == reference_Type OR
       t == string_Type OR
       t == fixedpoint_Type OR
       (t == sequence_Type AND (!TypeIsString(type))) OR
       ((t == object_Type) AND class_object(type)->local AND (list_size(class_object(type)->state) > 0)) OR
       t == array_Type))
    {
      generateInputCode (type, t, context);
      generateOutputCode (type, t, context);
      generateSizeofCode (type, t, context);
    }

#ifdef ILU_HTTPNG_OBJECTS
  if ((t == object_Type) AND class_object(type)->local)
    generateLocalObjectIOCode (type, t, context);
#endif

  if (HasFreeRoutine(type))
    generateFreeCode (type, t, context);

  generateBufAllocCode(type, t, context);

  if (HasAllocRoutine(type))
    generateAllocCode (type, t, context);
}

static void generateIosCode (Interface  interface,       Context  context)
{
  list_enumerate(interface->types,
		 (void (*) (void *, void *)) generateTypeIoCode,
		 context);
}

static void generateGlobalCode (Interface  interface,  Context  context)
{
  generateExceptionTable (interface, context);
  generateExceptionProcs (context);
  generateIosCode (interface, context);
}

static unsigned methodIdx = 47, exnIdx = 86, stateIdx=133;

static void setupExn (refany elt, refany rock)
{
  Exception       e = (Exception) elt;
  Context         context = (Context) rock;
  fprintf(context->file, "    exns[%u] = ex_%s;\n",
	  exnIdx++, c_exception_name(e));
}

typedef struct {
  Procedure p;
  Context c;
  unsigned count;
} MethodArgContext;

static void generateMethodArgDef (refany elt, refany rock)
{
  Argument	arg = (Argument) elt;
  Context	context = ((MethodArgContext *) rock)->c;
  Procedure	m = ((MethodArgContext *) rock)->p;
  FILE *	f = (((MethodArgContext *) rock)->c)->file;
  unsigned *	pCount = & ((MethodArgContext *) rock)->count;

  fprintf (f, "    ilu_DefineMethodArg (m, %u, \"%s\", %s, %s, \"%s\", &lerr);\n",
	   *pCount, argument_name(arg),
	   arg->sibling ? "ilu_TRUE" : "ilu_FALSE",
	   ((arg->direction == In) ? "ilu_In" :
	    ((arg->direction == Out) ? "ilu_Out" :
	     ((arg->direction == InOut) ? "ilu_InOut" :
	      (fprintf(stderr, "Bad method arg direction\n"), exit(1), "")))),
	   type_uid(arg->type));
  fprintf(f, "    if (ILU_ERRNOK(lerr))\n");
  fprintf(f, "      goto fail2;\n");
  ++*pCount;
}

static void generateMethodDef (refany elt, refany rock)
{
  Procedure       m = (Procedure) elt;
  Context         context = (Context) rock;
  FILE           *f = context->file;
  fprintf(f, "  { ilu_Exception\t");
  if (list_size(m->exceptions) > 0) {
    fprintf(f, "exns[%u];\n", (unsigned int) list_size(m->exceptions));
    exnIdx = 0;
    list_enumerate(m->exceptions, setupExn, context);
  } else
    /* Don't try C's patience with a 0-length array */
    fprintf(f, "*exns = NULL;\n");
  fprintf(f, "    m = ilu_DefineMethod(cl, %u,\n", methodIdx++);
  fprintf(f, "\t\"%s\",\t/*name*/\n", procedure_name(m));
  fprintf(f, "\t%u,\t/*id*/\n", m->id);
  fprintf(f, "\t%u,\t/*functional*/\n", m->functional != 0);
  fprintf(f, "\t%u,\t/*asynch*/\n", m->asynch != 0);
  fprintf(f, "\t%u,\t/*n exns*/\n", (unsigned int) list_size(m->exceptions));
  fprintf(f, "\texns,\t/*exceptions*/\n");
  fprintf(f, "\t%u,\t/*n args*/\n", (unsigned int) list_size(m->arguments));
  fprintf(f, "\t%s%s%s,\t/*return type ID*/\n",
	  (m->returnType == NULL) ? "" : "\"",
	  (m->returnType == NULL) ? "ILU_NIL" : type_uid(m->returnType),
	  (m->returnType == NULL) ? "" : "\"");
  fprintf(f, "\t&lerr);\n");
  fprintf(f, "    if (ILU_ERRNOK(lerr))\n");
  fprintf(f, "      goto fail2;\n");
  {
    MethodArgContext mac;
    mac.p = m;
    mac.c = context;
    mac.count = 0;
    list_enumerate (m->arguments, generateMethodArgDef, (refany) &mac);
  }
  fprintf(f, "  }\n");
}

static void generateStateDef (refany elt, refany rock)
{
  Argument a = (Argument) elt;
  Context context = (Context) rock;
  FILE *f = ((Context) rock)->file;

  fprintf(f, "  s = ilu_DefineObjectState(cl, %u,\n", stateIdx++);
  fprintf(f, "\t\"%s\",\t/* field name */\n", argument_name(a));
  fprintf(f, "\t\"%s\",\t/* field's type UID */\n", type_uid(argument_type(a)));
  fprintf(f, "\t&lerr);\n");
  fprintf(f, "  if (ILU_ERRNOK(lerr))\n     goto fail2;\n");
}

static string
  encode_doc_string_for_C_literal (string ds)
{
  string ns, p, q;
  unsigned count;
  static char oct_digit[] = "01234567";

  for (p = ds, count = 0;  *p != 0;  p++)
    if ((*p < 0x20) || (*p == '\\') || (*p == '"'))
      count += 1;
  ns = malloc(strlen(ds) + (count * 4) + 1);
  for (p = ds, q = ns;  *p != 0;  p++)
    if ((*p < 0x20) || (*p == '\\') || (*p == '"')) {
      *q++ = '\\';
      *q++ = oct_digit[((*p) / 64) & 0x7];
      *q++ = oct_digit[((*p) / 8) & 0x7];
      *q++ = oct_digit[(*p) & 0x7];
    } else {
      *q++ = *p;
    }
  *q = 0;
  return ns;
}

static void RegisterClass (refany elt, refany rock)
{
  Type            class = (Type) elt;
  Context         context = (Context) rock;
  FILE           *f = context->file;
  char           *tn;
  Class           o;
  if ((class->builtIn && class->importInterfaceName == NULL)
      || (type_basic_type(class) != object_Type))
    return;
  tn = c_type_name(class);
  o = class_object(class);
  fprintf(f, "  { ilu_string supers[] = {\n");
  list_enumerate(o->superclasses, classIDs, context);
  fprintf(f, "\tNULL};\n");
  fprintf(f, "    cl = ilu_DefineObjectType(\"%s.%s\",\t/*name*/\n",
	  name_base_name(context->interface->name),
	  name_base_name(class->name));
  if (o->brand == NULL)
    fprintf(f, "\tNULL,\t/*no brand*/\n");
  else
    fprintf(f, "\t\"%s\",\t/*brand*/\n", o->brand);
  fprintf(f, "\t\"%s\",\t/*uid*/\n", class->uid);
  if (o->singleton == NULL)
    fprintf(f, "\tNULL,\t/*singleton*/\n");
  else
    fprintf(f, "\t\"%s\",\t/*singleton*/\n", o->singleton);
  fprintf(f, "\t%s,\t/* optional */\n\t%s,\t/* collectible */\n",
	  o->optional ? "ilu_TRUE" : "ilu_FALSE",
	  o->collectible ? "ilu_TRUE" : "ilu_FALSE");
  if (o->doc_string == NULL)
    fprintf(f, "\tNULL,\t/*doc string*/\n");
  else {
    string s = encode_doc_string_for_C_literal (o->doc_string);
    fprintf(f, "\t\"%s\",\t/*doc string*/\n", s);
    free(s);
  }
  fprintf(f, "\t%u,\t/*n methods*/\n", (unsigned int) list_size(o->methods));
  fprintf(f, "\t%u,\t/*n supers*/\n",
       (o->superclasses == NULL) ? 0 : (unsigned int) list_size(o->superclasses));
  fprintf(f, "\tsupers,\t/* supers */\n");
#ifdef ILU_HTTPNG_OBJECTS
  fprintf(f, "#ifdef ILU_HTTPNG_OBJECTS\n");
  fprintf(f, "\t%u,\t/* n state elements */\n", (unsigned) list_size(o->state));
  fprintf(f, "\t%s,\t/* local? */\n", o->local ? "ilu_TRUE" : "ilu_FALSE");
  fprintf(f, "\t%s,\t/* sealed? */\n", o->sealed ? "ilu_TRUE" : "ilu_FALSE");
  fprintf(f, "#endif /* def ILU_HTTPNG_OBJECTS */\n");
#endif /* def ILU_HTTPNG_OBJECTS */
  fprintf(f, "\t&lerr);\n");
  fprintf(f, "    if (ILU_ERRNOK(lerr))\n");
  fprintf(f, "      goto fail2;\n");
  fprintf(f, "    _%s__ILUType = cl;\n", tn);
  fprintf(f, "  }\n");
  methodIdx = 0;
  list_enumerate(o->methods, generateMethodDef, context);
#ifdef ILU_HTTPNG_OBJECTS
  stateIdx = 0;
  list_enumerate(o->state, generateStateDef, context);
#endif
  return;
}

static boolean AddGCCallback (Type type, Context context)
{
  if ((type->builtIn && type->importInterfaceName == NULL)
      OR (type_basic_type(type) != object_Type)
      OR (NOT class_object(type)->collectible))
    return FALSE;
  fprintf(context->file, "  _ILU_C_EnsureGcClient ();\n");
  return TRUE;
}

static void InitializeImportedInterfaces (refany elt, refany rock)
{
  Imported        i = (Imported) elt;
  Context         c = (Context) rock;
  Interface       i2 = GetInterface(i->name, i->filename);
  if (i2 == NULL) {
    fprintf(stderr, "Can't find interface <%s>\n", i->name);
    return;
  } else if (strcmp(i->name, "ilu") == 0);
  else
    fprintf(c->file, "  _%s__GeneralInitialization();\n",
	    c_interface_name(i2));
}

static void genExnDef(refany elt, refany rock)
{
  Exception       e = (Exception) elt;
  Context         context = (Context) rock;
  FILE           *f = context->file;
  if (e->interface != context->interface || e->import != NULL)
    return;
  fprintf(f, "  _%s__Exception_%s = ilu_DefineException(",
	  c_interface_name(e->interface), c_simple_name(e->name));
  if (e->corba_rep_id == NIL)
    fprintf(f, "\"%s\", \"%s\"", name_base_name(e->interface->name),
	    name_base_name(e->name));
  else
    fprintf(f, "ILU_NIL, \"%s\"", e->corba_rep_id);
  fprintf(f, ", %s%s%s",
	  (exception_type(e) == NULL) ? "" : "\"",
	  (exception_type(e) == NULL) ? "ILU_NIL" : type_uid(exception_type(e)),
	  (exception_type(e) == NULL) ? "" : "\"");
  fprintf(f, ", &lerr);\n");
  fprintf(f, "  if (ILU_ERRNOK(lerr))\n");
  fprintf(f, "    goto fail1;\n");
  return;
}

static void
  printDimension (unsigned long dim, Context context)
{
  fprintf(context->file, ", %lu", dim);
}

#ifdef ADD_TYPE_REGISTRATION_SUPPORT

static void
  RegisterType (void *type_ptr, void *context_arg)
{
  Type t = (Type) type_ptr;
  Context context = (Context) context_arg;

  if (t->importInterfaceName != NIL || t->builtIn)
    return;

  switch (type_kind(t))
    {
    case invalid_Type:
    case void_Type:
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
    case pickle_Type:
      /* All done in the ILU C runtime */
      break;

    case pipe_Type:
      fprintf(stderr, "Unexpected type %s encountered!\n", c_type_name(t));
      exit(1);

    case object_Type:
      {
	Class co = class_object(t);
	fprintf(context->file, "  _%s__IoFns.kernelType = ilu_RegisterObjectType(\"%s\", "
		"\"%s\", %s%s%s, \"%s\",\n    _%s__ILUType, /* object class */\n    &newreg, &lerr);\n",
		c_type_name(t), type_name(t), interface_name(type_interface(t)),
		(type_interface(t)->brand == NULL) ? "" : "\"",
		(type_interface(t)->brand == NULL) ? "ILU_NIL" : type_interface(t)->brand,
		(type_interface(t)->brand == NULL) ? "" : "\"",
		type_uid(t),
		c_type_name(t));
	fprintf(context->file, "  if (ILU_ERRNOK(lerr)) goto fail1;\n");
	break;
      }

#ifndef ILU_REFERENCE_TYPES

    case optional_Type:
      fprintf(context->file, "  _%s__IoFns.kernelType = ilu_RegisterOptionalType(\"%s\", "
	      "\"%s\", %s%s%s, \"%s\",\n    \"%s\", /* base type */\n    &newreg, &lerr);\n",
	      c_type_name(t), type_name(t), interface_name(type_interface(t)),
	      (type_interface(t)->brand == NULL) ? "" : "\"",
	      (type_interface(t)->brand == NULL) ? "ILU_NIL" : type_interface(t)->brand,
	      (type_interface(t)->brand == NULL) ? "" : "\"",
	      type_uid(t),
	      type_uid(type_description(t)->structuredDes.optional));
      fprintf(context->file, "  if (ILU_ERRNOK(lerr)) goto fail1;\n");
      break;

#else

    case optional_Type:
      fprintf(context->file, "  _%s__IoFns.kernelType = ilu_RegisterReferenceType(\"%s\", "
	      "\"%s\", %s%s%s, \"%s\",\n    \"%s\", /* base type */\n"
	      "    ilu_TRUE, /* optionalp */  ilu_FALSE, /* aliasedp */\n    &newreg, &lerr);\n",
	      c_type_name(t), type_name(t), interface_name(type_interface(t)),
	      (type_interface(t)->brand == NULL) ? "" : "\"",
	      (type_interface(t)->brand == NULL) ? "ILU_NIL" : type_interface(t)->brand,
	      (type_interface(t)->brand == NULL) ? "" : "\"",
	      type_uid(t),
	      type_uid(type_description(t)->structuredDes.optional));
      fprintf(context->file, "  if (ILU_ERRNOK(lerr)) goto fail1;\n");
      break;

    case reference_Type:
      fprintf(context->file, "  _%s__IoFns.kernelType = ilu_RegisterReferenceType(\"%s\", "
	      "\"%s\", %s%s%s, \"%s\",\n    \"%s\", /* base type */\n"
	      "    %s, /* optionalp */  %s, /* aliasedp */\n    &newreg, &lerr);\n",
	      c_type_name(t), type_name(t), interface_name(type_interface(t)),
	      (type_interface(t)->brand == NULL) ? "" : "\"",
	      (type_interface(t)->brand == NULL) ? "ILU_NIL" : type_interface(t)->brand,
	      (type_interface(t)->brand == NULL) ? "" : "\"",
	      type_uid(t),
	      type_uid(type_description(t)->structuredDes.reference.base_type),
	      type_description(t)->structuredDes.reference.optional ? "ilu_TRUE" : "ilu_FALSE",
	      type_description(t)->structuredDes.reference.aliased ? "ilu_TRUE" : "ilu_FALSE");
      fprintf(context->file, "  if (ILU_ERRNOK(lerr)) goto fail1;\n");
      break;

#endif /* def ILU_REFERENCE_TYPES */

    case alias_Type:
      fprintf(context->file, "  ilu_RegisterAliasType(\"%s\", \"%s\", %s%s%s, \"%s\",\n"
	      "    \"%s\",	/* base type */\n"
	      "    &newreg, &lerr);\n",
	      type_name(t), interface_name(type_interface(t)),
	      (type_interface(t)->brand == NULL) ? "" : "\"",
	      (type_interface(t)->brand == NULL) ? "ILU_NIL" : type_interface(t)->brand,
	      (type_interface(t)->brand == NULL) ? "" : "\"",
	      type_uid(t),
	      type_uid(ur_type(t)));
      fprintf(context->file, "  if (ILU_ERRNOK(lerr)) goto fail1;\n");
      break;

    case string_Type:
      {
	char *language = type_description(t)->structuredDes.string.language;
	fprintf(context->file, "  _%s__IoFns.kernelType = ilu_RegisterStringType(\"%s\", "
		"\"%s\", %s%s%s, \"%s\",\n"
		"    \"%s\",	/* language of string */\n"
		"    %lu,	/* limit (0 for no limit) */\n"
		"    %lu,       /* charset of string */\n"
		"    &newreg, &lerr);\n",
		c_type_name(t), type_name(t), interface_name(type_interface(t)),
		(type_interface(t)->brand == NULL) ? "" : "\"",
		(type_interface(t)->brand == NULL) ? "ILU_NIL" : type_interface(t)->brand,
		(type_interface(t)->brand == NULL) ? "" : "\"",
		type_uid(t), (language == NULL) ? "i-default" : language,
		type_description(t)->structuredDes.string.max_length,
		type_description(t)->structuredDes.string.charset);
	fprintf(context->file, "  if (ILU_ERRNOK(lerr)) goto fail1;\n");
      }
      break;

    case fixedpoint_Type:
      {
	TypeDescription d = type_description(t);
	IntegerLiteral minnum = type_description(t)->structuredDes.fixed.min_numerator;
	IntegerLiteral maxnum = type_description(t)->structuredDes.fixed.max_numerator;
	IntegerLiteral denom = type_description(t)->structuredDes.fixed.denominator;
	char *name = c_type_name(t);
	char *rs;
	static int counter = 1;

	fprintf(context->file, "  {\n    char *errptr;\n");
	fprintf(context->file, "    %s__Type = &_%s__Type;\n", name, name);
	fprintf(context->file, "    %s__Type->min_numerator = ", name);
	if (minnum == NULL)
	  fprintf(context->file, "ILU_NIL;  /* no minimum */\n");
	else {
	  if (minnum->small)
	    fprintf(context->file, "ilubignum_FromString(\"%s%lu\", ILU_NIL, 0, &errptr); /* minimum numerator */\n",
		    minnum->negative ? "-" : "", minnum->val.direct);
	  else
	    fprintf(context->file, "ilubignum_FromString(\"%s%s\", ILU_NIL, 0, &errptr); /* minimum numerator */\n",
		    minnum->negative ? "-" : "", minnum->val.string);
	  fprintf(context->file, "    if (errptr != ILU_NIL) goto fpfail%d;\n", counter);
	}
	fprintf(context->file, "    %s__Type->max_numerator = ", name);
	if (maxnum == NULL)
	  fprintf(context->file, "ILU_NIL;  /* no maximum */\n");
	else {
	  if (maxnum->small)
	    fprintf(context->file, "ilubignum_FromString(\"%s%lu\", ILU_NIL, 0, &errptr); /* maximum numerator */\n",
		    maxnum->negative ? "-" : "", maxnum->val.direct);
	  else
	    fprintf(context->file, "ilubignum_FromString(\"%s%s\", ILU_NIL, 0, &errptr); /* maximum numerator */\n",
		    maxnum->negative ? "-" : "", maxnum->val.string);
	  fprintf(context->file, "  if (errptr != ILU_NIL) goto fpfail%d;\n", counter);
	}
	fprintf(context->file, "    %s__Type->denominator = ", name);
	if (denom->small)
	  fprintf(context->file, "ilubignum_FromString(\"%s%lu\", ILU_NIL, 0, &errptr); /* denominator */\n",
		  denom->negative ? "-" : "", denom->val.direct);
	else
	  fprintf(context->file, "ilubignum_FromString(\"%s%s\", ILU_NIL, 0, &errptr); /* denominator */\n",
		  denom->negative ? "-" : "", denom->val.string);
	fprintf(context->file, "    if (errptr != ILU_NIL) goto fpfail%d;\n", counter);
	fprintf(context->file, "    %s__Type->fixed_digits = %lu;\n",
		name, (unsigned long) d->structuredDes.fixed.fixed_digits);
	fprintf(context->file, "    %s__Type->fixed_decimal_places = %lu;\n",
		name, (unsigned long) d->structuredDes.fixed.fixed_decimal_places);
	switch (d->structuredDes.fixed.range_size) {
	case ilu_fprs_byte:
	  rs = "ilu_fprs_byte";
	  break;
	case ilu_fprs_shortcardinal:
	  rs = "ilu_fprs_shortcardinal";
	  break;
	case ilu_fprs_cardinal:
	  rs = "ilu_fprs_cardinal";
	  break;
	case ilu_fprs_longcardinal:
	  rs = "ilu_fprs_longcardinal";
	  break;
	case ilu_fprs_shortinteger:
	  rs = "ilu_fprs_shortinteger";
	  break;
	case ilu_fprs_integer:
	  rs = "ilu_fprs_integer";
	  break;
	case ilu_fprs_longinteger:
	  rs = "ilu_fprs_longinteger";
	  break;
	default:
	  rs = "ilu_fprs_large";
	  break;
	}
	fprintf(context->file, "    %s__Type->range_size = %s;\n", name, rs);
	fprintf(context->file, "   fpfail%d:\n", counter);
	fprintf(context->file, "    if (errptr != ILU_NIL) {\n");
	fprintf(context->file, "      ilu_DebugPrintf(\"Can't initialize fixed-point type \\\"%s\\\":  %%s\", errptr);\n", name);
	fprintf(context->file, "      goto fail1;\n");
	fprintf(context->file, "    }\n");
	fprintf(context->file, "  }\n");
	counter += 1;
	fprintf(context->file, "  _%s__IoFns.kernelType = ilu_RegisterFixedpointType(\"%s\", "
		"\"%s\", %s%s%s, \"%s\",\n"
		"    %s__Type->min_numerator,\n"
		"    %s__Type->max_numerator,\n"
		"    %s__Type->denominator,\n"
		"    %s__Type->fixed_digits,\n"
		"    %s__Type->fixed_decimal_places,\n"
		"    %s__Type->range_size,\n"
		"    &newreg, &lerr);\n",
		c_type_name(t), type_name(t), interface_name(type_interface(t)),
		(type_interface(t)->brand == NULL) ? "" : "\"",
		(type_interface(t)->brand == NULL) ? "ILU_NIL" : type_interface(t)->brand,
		(type_interface(t)->brand == NULL) ? "" : "\"",
		type_uid(t), name, name, name, name, name, name);
	fprintf(context->file, "  if (ILU_ERRNOK(lerr)) goto fail1;\n");
      }
      break;

    case sequence_Type:
      if (!TypeIsString(t)) {
	fprintf(context->file, "  _%s__IoFns.kernelType = ilu_RegisterSequenceType(\"%s\", "
		"\"%s\", %s%s%s, \"%s\",\n"
		"    \"%s\",	/* base type of sequence */\n"
		"    %lu,		/* limit (0 for no limit) */\n"
		"    &newreg, &lerr);\n",
		c_type_name(t), type_name(t), interface_name(type_interface(t)),
		(type_interface(t)->brand == NULL) ? "" : "\"",
		(type_interface(t)->brand == NULL) ? "ILU_NIL" : type_interface(t)->brand,
		(type_interface(t)->brand == NULL) ? "" : "\"",
		type_uid(t),
		type_uid(type_description(t)->structuredDes.sequence.type),
		type_description(t)->structuredDes.sequence.limit);
	fprintf(context->file, "  if (ILU_ERRNOK(lerr)) goto fail1;\n");
      }
      break;

    case array_Type:
      fprintf(context->file, "  { /* array type \"%s\" */\n    ilu_cardinal _%s_dims[] = { %lu",
	      c_type_name(t), c_type_name(t),
	      (unsigned long) list_car(type_description(t)->structuredDes.array.dimensions));
      list_enumerate(list_cdr(type_description(t)->structuredDes.array.dimensions),
		     (iluparser_EnumProc) printDimension, context);
      fprintf(context->file, " };\n    _%s__IoFns.kernelType = ilu_RegisterArrayType(\"%s\", "
	      "\"%s\", %s%s%s, \"%s\",\n    \"%s\", /* base type of array */\n"
	      "    %lu,		/* number of dimensions */\n"
	      "    _%s_dims,	/* actual dimensions */\n"
	      "    &newreg, &lerr);\n",
	      c_type_name(t), type_name(t), interface_name(type_interface(t)),
	      (type_interface(t)->brand == NULL) ? "" : "\"",
	      (type_interface(t)->brand == NULL) ? "ILU_NIL" : type_interface(t)->brand,
	      (type_interface(t)->brand == NULL) ? "" : "\"",
	      type_uid(t),
	      type_uid(type_description(t)->structuredDes.array.type),
	      list_size(type_description(t)->structuredDes.array.dimensions),
	      c_type_name(t));
      fprintf(context->file, "    if (ILU_ERRNOK(lerr)) goto fail1;\n  }\n");
      break;

    case record_Type:
      {
	cardinal i;
	Argument a;
	
	fprintf(context->file, "  _%s__IoFns.kernelType = type = ilu_RegisterRecordType(\"%s\", \"%s\", %s%s%s, \"%s\",\n"
                "    %lu,	/* number of fields in the record */\n"
		"    %s,	/* whether or not it is extensible */\n"
		"    %s%s%s,	/* supertype, if any */\n"
		"    &newreg, &lerr);\n",
		c_type_name(t), type_name(t), interface_name(type_interface(t)),
		(type_interface(t)->brand == NULL) ? "" : "\"",
		(type_interface(t)->brand == NULL) ? "ILU_NIL" : type_interface(t)->brand,
		(type_interface(t)->brand == NULL) ? "" : "\"",
		type_uid(t),
		list_size(type_description(t)->structuredDes.record.fields),
		type_description(t)->structuredDes.record.extensible ? "ilu_TRUE" : "ilu_FALSE",
		(type_description(t)->structuredDes.record.supertype == NULL) ? "" : "\"",
		(type_description(t)->structuredDes.record.supertype == NULL) ?
		  "ILU_NIL" : type_uid(type_description(t)->structuredDes.record.supertype),
		(type_description(t)->structuredDes.record.supertype == NULL) ? "" : "\"");
	fprintf(context->file, "  if (ILU_ERRNOK(lerr)) goto fail1;\n");
	fprintf(context->file, "  if (newreg) {\n");
	for (i = 0;  i < list_size(type_description(t)->structuredDes.record.fields);  i++)
	  {
	    a = (Argument) list_ref(type_description(t)->structuredDes.record.fields, i);
	    fprintf(context->file, "    ilu_RegisterRecordField(type, %lu, /* which field */\n"
		    "      \"%s\", /* field name */\n"
		    "      \"%s\", /* UID of field type */\n"
                    "      &lerr);\n", i,
		    argument_name(a), type_uid(argument_type(a)));
	    fprintf(context->file, "    if (ILU_ERRNOK(lerr)) goto fail1;\n");
	  }	
	fprintf(context->file, "  };\n");
      }
      break;

    case enumeration_Type:
      {
	cardinal i;
	EnumField f;
	
	fprintf(context->file, "  _%s__IoFns.kernelType = type = ilu_RegisterEnumerationType(\"%s\", "
		"\"%s\", %s%s%s, \"%s\",\n"
		"    %lu,	/* number of elements in the enum */\n"
		"    &newreg, &lerr);\n",
		c_type_name(t), type_name(t), interface_name(type_interface(t)),
		(type_interface(t)->brand == NULL) ? "" : "\"",
		(type_interface(t)->brand == NULL) ? "ILU_NIL" : type_interface(t)->brand,
		(type_interface(t)->brand == NULL) ? "" : "\"",
		type_uid(t),
		list_size(type_description(t)->structuredDes.enumeration));
	fprintf(context->file, "  if (ILU_ERRNOK(lerr)) goto fail1;\n");
	fprintf(context->file, "  if (newreg) {\n");
	for (i = 0;  i < list_size(type_description(t)->structuredDes.enumeration);  i++)
	  {
	    f = (EnumField) list_ref(type_description(t)->structuredDes.enumeration, i);
	    fprintf(context->file, "    ilu_RegisterEnumerationElement(type, %lu, /* which element */\n"
		    "    \"%s\", /* element name */\n"
		    "    %lu,	/* integer value for element */\n"
		    "    &lerr);\n", i, f->name, (unsigned long) f->id);
	    fprintf(context->file, "    if (ILU_ERRNOK(lerr)) goto fail1;\n");
	  }
	fprintf(context->file, "  };\n");
      }
      break;

    case union_Type:
      {
	cardinal i, n, j, m;
	ConstantValue cv;
	Argument f;
	/* first figure out the index of the default arm */
	if (type_description(t)->structuredDes.uniond.default_arm != NULL)
	  {
	    n = list_size(type_description(t)->structuredDes.uniond.types);
	    for (i = 0;  i < n;  i++)
	      {
		if (((Argument) list_ref(type_description(t)->structuredDes.uniond.types, i)) ==
		    type_description(t)->structuredDes.uniond.default_arm)
		  break;
	      }
	    if (i < n)
	      n = i + 1;
	    else
	      n = 0;		/* probably an error */
	  }
	else
	  n = 0;		/* no default arm */
	/* now register the type */
	fprintf(context->file, "  _%s__IoFns.kernelType = type = ilu_RegisterUnionType(\"%s\", "
		"\"%s\", %s%s%s, \"%s\",\n"
		"    \"%s\",	/* UID of discriminant type */\n"
		"    %lu,	/* number of arms */\n"
		"    %lu,	/* default arm (0 for none) */\n"
		"    %s,	/* invalid discriminant values allowed? (idiot CORBA) */\n"
		"    &newreg, &lerr);\n",
		c_type_name(t), type_name(t), interface_name(type_interface(t)),
		(type_interface(t)->brand == NULL) ? "" : "\"",
		(type_interface(t)->brand == NULL) ? "ILU_NIL" : type_interface(t)->brand,
		(type_interface(t)->brand == NULL) ? "" : "\"",
		type_uid(t),
		type_uid(type_description(t)->structuredDes.uniond.discriminator_type),
		list_size(type_description(t)->structuredDes.uniond.types), n,
		type_description(t)->structuredDes.uniond.others_allowed
		? "ilu_TRUE" : "ilu_FALSE");
	fprintf(context->file, "  if (ILU_ERRNOK(lerr)) goto fail1;\n");
	fprintf(context->file, "  if (newreg) {\n    ilu_ConstantValue_s cv;\n");
	fprintf(context->file, "    ilu_UnionArm arm;\n\n");
	for (i = 0, n = list_size(type_description(t)->structuredDes.uniond.types);  i < n;  i++)
	  {
	    /* register the union arm */
	    f = (Argument) list_ref(type_description(t)->structuredDes.uniond.types, i);
	    m = list_size(f->values);
	    fprintf(context->file, "    arm = ilu_RegisterUnionArm (type, %lu, /* which arm */\n", i);
	    /* union arms may have NIL names, so check... */
	    if (argument_name(f) == NULL)
	      fprintf(context->file, "      ILU_NIL, /* no name for arm */\n");
	    else
	      fprintf(context->file, "      \"%s\", /* name of arm */\n", argument_name(f));
	    /* now finish the arguments to ilu_RegisterUnionArm */
	    fprintf(context->file, "      \"%s\", /* arm type */\n"
		    "      %lu, /* number of values that can select this arm */\n"
		    "      &lerr);\n",
		    type_uid(argument_type(f)), m);
	    fprintf(context->file, "    if (ILU_ERRNOK(lerr)) goto fail1;\n");
	    /* register the possible values for that arm */
	    for (j = 0;  j < m;  j++)
	      {
		cv = (ConstantValue) list_ref(f->values, j);
		switch (type_ur_kind(type_description(t)->structuredDes.uniond.discriminator_type))
		  {
		  case boolean_Type:
		    fprintf(context->file, "    cv.value.boolean_val = %s;\n",
			    cv->val.b ? "ilu_TRUE" : "ilu_FALSE");
		    fprintf(context->file, "    cv.kind = ilu_boolean_cvk;\n");
		    break;
		  case shortinteger_Type:
		    fprintf(context->file, "    cv.value.shortinteger_val = %ld;\n",
			    cv->val.i.value * cv->val.i.sign);
		    fprintf(context->file, "    cv.kind = ilu_shortinteger_cvk;\n");
		    break;
		  case integer_Type:
		    fprintf(context->file, "    cv.value.integer_val = %ld;\n",
			    cv->val.i.value * cv->val.i.sign);
		    fprintf(context->file, "    cv.kind = ilu_integer_cvk;\n");
		    break;
		  case shortcardinal_Type:
		    fprintf(context->file, "    cv.value.shortcardinal_val = %lu;\n",
			    cv->val.i.value);
		    fprintf(context->file, "    cv.kind = ilu_shortcardinal_cvk;\n");
		    break;
		  case cardinal_Type:
		    fprintf(context->file, "    cv.value.cardinal_val = %lu;\n",
			    cv->val.i.value);
		    fprintf(context->file, "    cv.kind = ilu_cardinal_cvk;\n");
		    break;
		  case byte_Type:
		    fprintf(context->file, "    cv.value.byte_val = %lu;\n",
			    cv->val.i.value);
		    fprintf(context->file, "    cv.kind = ilu_byte_cvk;\n");
		    break;
		  case enumeration_Type:
		    fprintf(context->file, "    cv.value.enumeration_val = \"%s\";\n",
			    cv->val.s);
		    fprintf(context->file, "    cv.kind = ilu_enumeration_cvk;\n");
		    break;
		  default:
		    fprintf(stderr, "Unexpected discriminant type encountered!\n");
		    exit(1);
		  }
		fprintf(context->file, "    ilu_RegisterUnionArmValue (arm, %lu, /* which arm value */\n"
			"      &cv, /* actual value */\n"
			"      &lerr);\n", j);
		fprintf(context->file, "    if (ILU_ERRNOK(lerr)) goto fail1;\n");
	      }
	  }
	fprintf(context->file, "  };\n");
      }
      break;
    }
}

#endif /* def ADD_TYPE_REGISTRATION_SUPPORT */

static void
  RegisterTypeIoFns (void *type_ptr, void *context_arg)
{
  Type t = (Type) type_ptr;
  Context context = (Context) context_arg;
  char *tn, *utn;
  TypeKind tk;

  if (t->importInterfaceName != NIL || t->builtIn)
    return;

  tn = c_type_name(t);
  utn = c_type_name(ur_type(t));

  if (TypeIsJustAlias(t))
    tk = alias_Type;
  else
    tk = type_ur_kind(t);

  switch (tk)
    {
    case invalid_Type:
    case alias_Type:
    case void_Type:
      break;

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
    case enumeration_Type:
    case pickle_Type:
      if (type_kind(t) == alias_Type) {
	fprintf(context->file, "  _%s__IoFns.inputFn = _%s__IoFns.inputFn;\n", tn, utn);
	fprintf(context->file, "  _%s__IoFns.outFn = _%s__IoFns.outFn;\n", tn, utn);
	fprintf(context->file, "  _%s__IoFns.sizeFn = _%s__IoFns.sizeFn;\n", tn, utn);
	fprintf(context->file, "  _%s__IoFns.freeFn = _%s__IoFns.freeFn;\n", tn, utn);
      }
      break;

    case object_Type:
      fprintf(context->file, "  _%s__IoFns.properties.object_class = _%s__ILUType;\n",
	      tn, tn);
      break;

    case optional_Type:
    case reference_Type:
    case sequence_Type:
    case array_Type:
    case record_Type:
    case fixedpoint_Type:
    case string_Type:
    case union_Type:
      if (!TypeIsString(t)) {
	fprintf(context->file, "  _%s__IoFns.inputFn = (ILU_C_InputFn) _%s__Input;\n", tn, utn);
	fprintf(context->file, "  _%s__IoFns.outFn = (ILU_C_OutputFn) _%s__Output;\n", tn, utn);
	fprintf(context->file, "  _%s__IoFns.sizeFn = (ILU_C_SizeFn) _%s__SizeOf;\n", tn, utn);
	if (HasFreeRoutine(t)) {
	  fprintf(context->file, "  _%s__IoFns.freeFn = (ILU_C_FreeFn) %s__Free;\n", tn, utn);
	} else {
	  fprintf(context->file, "  _%s__IoFns.freeFn = (ILU_C_FreeFn) 0;\n", tn);
	}
      }
      break;

    default:
      fprintf(stderr, "Unexpected type %s encountered!\n", tn);
      exit(1);
    }
  if (!(TypeIsJustAlias(t) || TypeIsString(t)))
    fprintf(context->file, "  _ILU_C_RegisterIoFns (&_%s__IoFns);\n\n", tn);
}

static void
generateClientRegistrationCode(Interface interface, Context context)
{
  char           *ifcname = (char *) c_interface_name(interface);
  FILE           *f = context->file;

  fprintf(f,
	  "void _%s__GeneralInitialization (void)\n{\n",
	  ifcname);
  fprintf(f, "  static ilu_boolean initialized = ilu_FALSE;\n");
  fprintf(f, "  ilu_Error lerr = ILU_INIT_NO_ERR;\n");
  fprintf(f, "  ilu_Class cl = ILU_NIL;\n");
  fprintf(f, "  ilu_Method m = ILU_NIL;\n");
  fprintf(f, "  ilu_Mutex otmu = ilu_GetOTMutex();\n");
  fprintf(f, "  ilu_boolean newreg;\n\n");
  fprintf(f, "  if (initialized)\n    return;\n");
  fprintf(f, "  initialized = ilu_TRUE;\n");
  fprintf(f, "  _ILU_C_InitializeCRuntime();\n");
  fprintf(f, "  _ILU_C_CheckStubConsistency(\"%s\", \"%s\", \"%s\");\n",
	  interface_name(interface), ILU_VERSION_STRING, ILU_TYPEUID_VERSION_STRING);
  list_enumerate(interface->imports, InitializeImportedInterfaces,
		 context);
  fprintf(f, "  if (!ilu_EnterMutex(otmu, &lerr))\n");
  fprintf(f, "    goto fail2;\n");
  list_enumerate (interface->exceptions, genExnDef, context);
  list_enumerate(interface->classes, RegisterClass, context);
#ifdef ADD_TYPE_REGISTRATION_SUPPORT
  fprintf(f, "#ifdef ADD_TYPE_REGISTRATION_SUPPORT\n{\n  ilu_Type type;\n");
  list_enumerate(interface->types, RegisterType, context);
  fprintf(f, "}\n#endif /* def ADD_TYPE_REGISTRATION_SUPPORT */\n");
#endif /* def ADD_TYPE_REGISTRATION_SUPPORT */
  fprintf(f, "fail1:\n");
  fprintf(f, "  if (!ilu_ExitMutex(otmu, ilu_TRUE, &lerr))\n");
  fprintf(f, "    goto fail2;\n");
  list_enumerate(interface->types, RegisterTypeIoFns, context);
  (void) list_find(interface->classes, (FindProc) AddGCCallback, context);
  fprintf(f, "fail2:\n");
  fprintf(f, "  ILU_MUST_BE_SUCCESS(lerr);\n");
  fprintf(f, "}\n\n");
}

static void generateSequenceCode (refany elt, refany rock)
{
  Type            seq = (Type) elt;
  Context         context = (Context) rock;
  boolean         hasReferent;
  char           *rtn;
  Type            eltType;
  char           *st;
  TypeKind        t;
  char           *tn;
  FILE           *f = context->file;

  if (type_basic_type(seq) != sequence_Type OR
      seq->importInterfaceName != NULL OR
      seq->builtIn)
    return;
  eltType = type_description(seq)->structuredDes.sequence.type;
  t = type_basic_type(ur_type(eltType));
  if (t == shortcharacter_Type || t == character_Type) {
    tn = c_type_name(seq);
    st = c_parameter_type(eltType, In);
    rtn = c_type_name(eltType);

    fprintf(f,
	    "void %s_Every (%s *h, void (*f)(%s *, void *), void * data)\n{\n",
	    tn, tn, rtn);
    fprintf(f, "  %s *p;\n", rtn);
    fprintf(f, "  for (p = *h;  *p != 0;  p++) (*f)(p, data);\n}\n\n");

    fprintf(f, "void %s_Append (%s *h, %s item)\n{\n", tn, tn, rtn);
    fprintf(f,
	 "  _ILU_C_Extend%sString (h, item, (CORBA_boolean) 1);\n",
	    (t == shortcharacter_Type) ? "" : "W");
    fprintf(f, "}\n\n");

    fprintf(f, "void %s_Push (%s *h, %s item)\n{\n", tn, tn, rtn);
    fprintf(f,
	 "  _ILU_C_Extend%sString (h, item, (CORBA_boolean) 0);\n",
	    (t == shortcharacter_Type) ? "" : "W");
    fprintf(f, "}\n\n");

    fprintf(f, "void %s_Pop (%s *h, %s *item)\n{\n", tn, tn, rtn);
    fprintf(f, "  _ILU_C_Pop%sString (h, item);\n}\n\n",
	    (t == shortcharacter_Type) ? "" : "W");

    fprintf(f, "CORBA_unsigned_long %s_Length (%s *h)\n{\n", tn, tn);
    fprintf(f, "  if (h == ILU_NIL || *h == ILU_NIL) return 0;\n");
    if (t == shortcharacter_Type)
      fprintf(f, "  return (strlen((char *)(*h)));\n");
    else
      fprintf(f, "  return _ILU_C_SafeWStrlen((CORBA_wchar *)(*h));\n");
    fprintf(f, "}\n\n");

    fprintf(f, "%s * %s_Nth (%s *h, CORBA_unsigned_long n)\n{\n",
	    rtn, tn, tn);
    fprintf(f, "  if (h == ILU_NIL || *h == ILU_NIL) return ILU_NIL;\n");
    /* cast because of sign fuckups */
    fprintf(f, "  if (n >= %s((%s*)(*h)))\n",
            (t == shortcharacter_Type) ? "strlen" : "_ILU_C_SafeWStrlen",
	    (t == shortcharacter_Type) ? "char" : "CORBA_wchar");
    fprintf(f, "    return ILU_NIL;\n");
    fprintf(f, "else return &((*h)[n]);\n");
    fprintf(f, "}\n\n");

    fprintf(f, "%s %s_Create (CORBA_unsigned_long sz, %s *p)\n{\n",
	    tn, tn, rtn);
    fprintf(f, "  %s s;\n\n  if (p == ILU_NIL)\n", tn);
    fprintf(f, "    {\n      s = ilu_malloc(sz * sizeof(%s));\n", rtn);
    fprintf(f, "      if (s == ILU_NIL) {");
    fprintf(f, "        _ILU_C_MallocFailure(sz * sizeof(%s));", rtn);
    fprintf(f, "        return ILU_NIL; }\n");
    fprintf(f, "      memset((void *) s, 0, sz * sizeof(%s));\n", rtn);
    fprintf(f, "      return s;\n");
    fprintf(f, "    }\n  else\n    return p;\n}\n\n");

    fprintf(f, "void %s_Init (%s *s, CORBA_unsigned_long sz)\n", tn, tn);
    fprintf(f, "{\n  return;\n}\n\n");
  } else {
    tn = c_type_name(seq);
    st = c_parameter_type(eltType, In);
    rtn = c_type_name(eltType);
    hasReferent = (t == record_Type || t == union_Type || t == pickle_Type ||
		   t == string_Type || t == fixedpoint_Type ||
		   (t == sequence_Type &&
		    !TypeIsEitherString(ur_type(eltType))));
    fprintf(f,
    "void %s_Every (%s *h, void (*f)(%s%s, void *), void * data)\n",
	    tn, tn, st, hasReferent ? "" : " *");
    fprintf(f, "{\n");
    fprintf(f, "  _ILU_C_EveryElement (%s, %s, sizeof(%s), %s);\n}\n\n",
	    "(ILU_C_Sequence) h", "(void (*)(void *, void *)) f",
	    rtn, "(void *) data");

    fprintf(f, "void %s_Append (%s *h, %s item)\n{\n", tn, tn, st);
    fprintf(f, "  _ILU_C_AppendGeneric (%s, %s, sizeof(%s));\n",
	    "(ILU_C_Sequence) h",
	    (hasReferent) ? "(char *) item" : "(char *) &item",
	    rtn);
    fprintf(f, "}\n\n");

    fprintf(f, "void %s_Push (%s *h, %s item)\n{\n", tn, tn, st);
    fprintf(f, "  _ILU_C_PushGeneric (%s, %s, sizeof(%s));\n",
	    "(ILU_C_Sequence) h",
	    (hasReferent) ? "(char *) item" : "(char *) &item",
	    rtn);
    fprintf(f, "}\n\n");

    fprintf(f, "void %s_Pop (%s *h, %s %sitem)\n{\n", tn, tn, st, (hasReferent) ? "" : "*");
    fprintf(f, "  _ILU_C_PopGeneric (%s, %s, sizeof(%s));\n}\n\n",
	    "(ILU_C_Sequence) h",
	    "(char *) item",
	    rtn);

    fprintf(f, "CORBA_unsigned_long %s_Length (%s *h)\n{\n", tn, tn);
    fprintf(f, "  if (h == ILU_NIL)\n    return 0;\n");
    fprintf(f, "  else return h->_length;\n}\n\n");

    fprintf(f, "%s * %s_Nth (%s *h, CORBA_unsigned_long n)\n{\n",
	    rtn, tn, tn);
    fprintf(f, "  if (h == ILU_NIL || (n >= h->_length))\n");
    fprintf(f, "    return ILU_NIL;\n");
    fprintf(f, "  else return &(h->_buffer[n]);\n}\n\n");

    fprintf(f, "%s * %s_%s_Create (CORBA_unsigned_long sz, %s %sp)\n",
	    tn, c_interface_name(context->interface),
	    c_simple_name(seq->name), st, hasReferent ? "" : "*");
    fprintf(f, "{\n  %s *s;\n", tn);
    fprintf(f, "  s = (%s *) ilu_malloc(sizeof(%s));\n", tn, tn);
    fprintf(f, "  if (s == ILU_NIL) { _ILU_C_MallocFailure(sizeof(%s)); return ILU_NIL; };\n", tn);
    fprintf(f, "  s->_maximum = sz;\n");
    fprintf(f, "  s->_length = (sz > 0 && p != ILU_NIL) ? sz : 0;\n");
    fprintf(f, "  s->_buffer = (p != ILU_NIL) ? p : ((sz > 0) ? ((%s *) ilu_malloc(sz * sizeof(%s))) : ILU_NIL);\n",
	    rtn, rtn);
    fprintf(f, "  if ((s->_buffer == ILU_NIL) && sz > 0 && p == ILU_NIL) {\n");
    fprintf(f, "    _ILU_C_MallocFailure(sz * sizeof(%s));  ilu_free(s);  return ILU_NIL; };\n", rtn);
    fprintf(f, "  return s;\n}\n\n");

    fprintf(f,
       "void %s_%s_Init (%s *s, CORBA_unsigned_long sz, %s %sp)\n",
	    c_interface_name(context->interface),
       c_simple_name(seq->name), tn, st, (hasReferent ? "" : "*"));
    fprintf(f, "{\n");
    fprintf(f, "  if (sz == 0 && p != NULL)\n    return;\n");
    fprintf(f, "  if (sz > 0)\n    s->_maximum = sz;\n");
    fprintf(f, "  else\n    s->_maximum = 0;\n");
    fprintf(f, "  if (sz > 0 && p != NULL)\n    s->_length = sz;\n");
    fprintf(f, "  else\n    s->_length = 0;\n");
    fprintf(f, "  if (sz > 0 && p == NULL) {\n");
    fprintf(f, "    s->_buffer = (%s *) ilu_malloc (sz * sizeof (%s));\n",
	    rtn, rtn);
    fprintf(f, "    if (s->_buffer == ILU_NIL) {\n");
    fprintf(f, "      s->_length = 0;\n");
    fprintf(f, "      s->_maximum = 0;\n");
    fprintf(f, "      _ILU_C_MallocFailure(sz * sizeof(%s)); }}\n", rtn);
    fprintf(f, "  else\n    s->_buffer = p;\n");
    fprintf(f, "  return;\n}\n\n");
  }
}

static void ParmPerMethod(refany elt, refany rock)
{
  Procedure       m = (Procedure) elt;
  Context         context = (Context) rock;

  fprintf(context->file, "  ");
  generateProcDecl(m, context, TRUE, "(*", "__Impl)\n    ");
  fprintf(context->file, ",\n");
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

static int      methodIndex;

static void SetMethod(refany elt, refany rock)
{
  Procedure       m = (Procedure) elt;
  Context         context = (Context) rock;
  fprintf(context->file,
	  "  method_block[%d] = (_ILU_C_Method) %s__Impl;\n",
	  methodIndex++, c_procedure_name(m));
  return;
}

static void DoObjectTypePart(refany elt, refany rock)
{
  Type            class = (Type) elt, ut;
  Context         context = (Context) rock;
  Class           od;
  ut = ur_type(class);
  od = class_object(ut);
  if (methodInList(c_type_name(ut)))
    return;
  addMethodToList(c_type_name(ut));
  fprintf(context->file,
	  "\n  ans->ilucc_sections[done].ilucdts_introType = _%s__ILUType;\n",
	  c_type_name(ur_type(class)));
  if (list_size(od->methods) > 0) {
    fprintf(context->file,
	    "  method_block = ilu_malloc(%d * sizeof(%s));\n",
	    list_size(od->methods), "_ILU_C_Method");
    fprintf(context->file, "  if (method_block == ILU_NIL)\n");
    fprintf(context->file, "    goto fale1;\n");
    fprintf(context->file,
	    "  ans->ilucc_sections[done++].ilucdts_methods = method_block;\n");
    methodIndex = 0;
    list_enumerate(od->methods, SetMethod, context);
  } else {
    fprintf(context->file,
	    "  ans->ilucc_sections[done++].ilucdts_methods = ILU_NIL;\n");
  }
  if (class_object(ut)->superclasses != NULL)
    list_enumerate(class_object(ut)->superclasses,
		   DoObjectTypePart, context);
  return;
}

void generateClassMakerDecl(Type class, Context context)
{
  Type            ut = ur_type(class);
  FILE           *f = context->file;
  fprintf(f, "ILU_C_Class %s__MakeClass(\n", c_type_name(class));
  clearMethodList();
  MethodParmsPerType(class, context);
  fprintf(f, "  ILU_C_FinalizationProc _finalize)");
}

static void
  addTypeHierarchy (Type t, list l)
{
  if (type_ur_kind(t) != object_Type)
    return;
  if (list_find(l, matchString, t->uid) == NULL) {
    list_push (l, t->uid);
  };
  list_enumerate(class_object(t)->superclasses, (iluparser_EnumProc) addTypeHierarchy, l);
}

static unsigned int
  countSuperClasses (Type t)
{
  list l;
  unsigned int count;

  if (class_object(t)->superclasses == NULL)
    return 1;
  l = new_list();
  list_enumerate (class_object(t)->superclasses, (iluparser_EnumProc) addTypeHierarchy, l);
  count = list_size(l);
  list_clear(l, FALSE);
  free(l);
  return count;
}

static void generateClassConser(refany elt, refany rock)
{
  Type            class = (Type) elt;
  Context         context = (Context) rock;
  Type            ut = ur_type(class);
  FILE           *f = context->file;
  Class           od;
  int             nsc;
  if (type_basic_type(class) != object_Type)
    return;
  od = class_object(ut);
  context->class = ut;
  nsc = countSuperClasses(class);
  generateClassMakerDecl(class, context);
  fprintf(f, "\n{\n");
  fprintf(f, "  ILU_C_Class ans = ilu_malloc(sizeof(*ans));\n");
  fprintf(f, "  _ILU_C_Method *method_block;\n");
  fprintf(f, "  int i, done=0;\n");
  fprintf(f, "  if (!ans) return ans;\n");
  fprintf(f, "  ans->ilucc_finalize = _finalize;\n");
  fprintf(f, "  ans->ilucc_sections = ilu_malloc(%d * sizeof(%s));\n",
	  nsc + 2, "ans->ilucc_sections[0]");
  fprintf(f, "  if (!ans->ilucc_sections)\n");
  fprintf(f, "    goto fale0;\n");
  clearMethodList();
  DoObjectTypePart(ut, context);
  fprintf(f,
     "  ans->ilucc_sections[done].ilucdts_introType = ILU_NIL;\n");
  fprintf(f, "  ans->ilucc_sections[done].ilucdts_methods = ILU_NIL;\n");
  fprintf(f, "  return ans;\n");
  fprintf(f, "fale1:\n");
  fprintf(f, "  for (i=0; i<done; i++) \n");
  fprintf(f, "    ilu_free(ans->ilucc_sections[i].ilucdts_methods);\n");
  fprintf(f, "fale0:\n");
  fprintf(f, "  ilu_free(ans);\n");
  fprintf(f, "  return ILU_NIL;\n");
  fprintf(f, "}\n");
}

static void listArgumentNames (Argument arg, Context context)
{
  fprintf (context->file, ", %s", c_argument_name(arg));
}

static void generateGFofMethod(refany elt, refany rock)
{
  Procedure       m = (Procedure) elt;
  Context         c = (Context) rock;
  int             ndx;
  TypeKind        t = type_basic_type(ur_type(m->returnType));

  generateProcDecl(m, c, TRUE, "", "");
  fprintf(c->file, "\n{\n");
  fprintf(c->file, "  %s (*_f)(%s",
	  (t == void_Type) ? "void" : c_return_type(m->returnType),
	  c_type_name(c->class));
  list_enumerate(m->arguments, listArgumentTypes, c);
  fprintf(c->file, ", ILU_C_ENVIRONMENT *);\n");
  fprintf(c->file, "  if (!_handle) {\n");
  if (t != void_Type)
    fprintf(c->file, "    %s _ret = {0};\n",
	    c_return_type(m->returnType));
  fprintf(c->file, "    ILU_C_RAISE_SYSTEM(%s);\n",
	  "_status, BAD_PARAM, ilu_bpm_nil, NO");
  fprintf(c->file, "    return%s;\n", (t == void_Type) ? "" : " _ret");
  fprintf(c->file, "  }\n");
  fprintf(c->file, "  _f = (%s (*)(%s",
	  (t == void_Type) ? "void" : c_return_type(m->returnType),
	  c_type_name(c->class));
  list_enumerate(m->arguments, listArgumentTypes, c);
  ndx = methodNdxOf(m, m->object);
  fprintf(c->file, ", ILU_C_ENVIRONMENT *))");
  fprintf(c->file, " _ILU_C_FindMethod (_handle, _%s__ILUType, %d);\n",
	  c_type_name(ur_type(c->class)),
  /* This would be the wrong ilu_Class for an inherited method! */
	  ndx);
  fprintf(c->file, "  _status->_major = ILU_C_NO_EXCEPTION;\n");
  fprintf(c->file, "  _status->ptr = ILU_NIL;\n");
  fprintf(c->file, "  _status->freeRoutine = (void(*)(void *)) 0;\n");
  fprintf(c->file, "  _status->returnCode = (ilu_Exception) 0;\n");
  if (t != void_Type)
    fprintf(c->file, "  return (");
  else
    fprintf(c->file, "  ");
  fprintf(c->file, "(*_f)(_handle");
  list_enumerate(m->arguments, (iluparser_EnumProc) listArgumentNames, c);
  fprintf(c->file, ", _status");
  if (t != void_Type)
    fprintf(c->file, ")");
  fprintf(c->file, ");\n}\n\n");
}

static void generateGenericFunctions(refany elt, refany rock)
{
  Type            class = (Type) elt, ut;
  Context         context = (Context) rock;
  Class           od;
  if (type_basic_type(class) != object_Type)
    return;
  ut = ur_type(class);
  od = class_object(ut);
  context->class = ut;
  if (list_size(od->methods) > 0)
    list_enumerate(od->methods, generateGFofMethod, context);
}

static void generateUserDataAccessors (refany elt, refany rock)
{
  Type            type = (Type) elt;
  Context         context = (Context) rock;
  char           *tn;

  if (type_basic_type(type) != object_Type)
    return;
  if (class_object(type)->local)
    return;

  tn = c_type_name(type);
  fprintf (context->file, "void %s__SetUserData (%s self, void *userData)\n", tn, tn);
  fprintf (context->file, "{\n  ((ILU_C_Object *) self)->instanceData = userData;\n}\n\n");
  fprintf (context->file, "void *%s__GetUserData (%s self)\n", tn, tn);
  fprintf (context->file, "{\n  return(((ILU_C_Object *) self)->instanceData);\n}\n\n");
}

void generateCommonCode (Interface interface, FILE *file)
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

  fprintf(file, "#include <stdio.h>\n");
  fprintf(file, "#include <string.h> /* used for error statements */\n");
  fprintf(file, "#include \"%s.h\"\n\n", pc_interfacename);

  list_enumerate(interface->classes, generateClassTable, &context);

  list_enumerate(interface->types, generateTypeTable, &context);

  fprintf(file, "\n\n");
  list_enumerate(interface->classes, generateClassConser, &context);
  list_enumerate(interface->classes, generateGenericFunctions, &context);
  list_enumerate(interface->classes, generateUserDataAccessors,
		 &context);
  generateGlobalCode(interface, &context);
  list_enumerate(interface->types, generateSequenceCode, &context);
  generateClientRegistrationCode(interface, &context);
}
