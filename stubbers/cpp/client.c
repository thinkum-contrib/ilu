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
/* Last edited by Mike Spreitzer December 7, 1995 11:02 pm PST */

/*
$Id: client.c,v 1.46 1999/08/03 01:50:56 janssen Exp $
*/

#include "cplusplus.h"

#ifdef MACOS
#pragma segment client
#endif

static boolean IncludeComma = FALSE;

static boolean hasMethods (Type t)
{
  return (list_size(class_object(t)->methods) > 0);
}

static void generate_methods_shorts (Type class, Context context)
{
  MethodRecordID = 0;
  DefineMethods (class, context);
}

static void listSuper (Type t, Context context)
{
  Type st;

  if (t == NULL)
    return;

  for (st = t;  st->supertype != NULL;  st = st->supertype)
    ;

  if (st->marked)
    return;

  fprintf (context->file, "\telse if (cast_to == %s::ILUClassRecord)\n\t\treturn ((void *)((class %s *) this));\n",
	   cplusplus_type_name(st), cplusplus_type_name(st));
  st->marked = TRUE;
  list_enumerate (class_object(st)->superclasses, (iluparser_EnumProc) listSuper, context);
}

static void generate_CastDown (Type type, Context context)
{
  char *tn = cplusplus_type_name(type);

  fprintf (context->file, "class %s * %s::ILUQuaT (class iluObject *from)\n", tn, tn);
  fprintf (context->file, "{\n\treturn((class %s *) (from->ILUCastDown (%s::ILUClassRecord)));\n}\n\n", tn, tn);

  fprintf (context->file, "void * %s::ILUCastDown (ilu_Class cast_to)\n", tn);
  fprintf (context->file, "{\n\tif (cast_to == NULL)\n\t\treturn((void *)((class iluObject *) this));\n");
  fprintf (context->file, "\telse if (cast_to == %s::ILUClassRecord)\n\t\treturn ((void *) this);\n", tn);
  UnmarkSupertypes (type);
  type->marked = TRUE;
  list_enumerate (class_object(type)->superclasses, (iluparser_EnumProc) listSuper, context);
  fprintf (context->file, "\telse return (NULL);\n}\n\n");
}

static void generate_InstanceFromSBH (Type type, Context context)
{
  char *tn;

  tn = cplusplus_type_name(context->class);

  fprintf (context->file, "class %s * %s::ILUCreateFromSBH (ilu_CString sbh)\n{\n", tn, tn);
  fprintf (context->file, "\treturn (%s *) ilu::SBHToObject(sbh, %s::ILUClassRecord);\n}\n\n", tn, tn);
}

static void generate_staticCreate (Type type, Context context)
{
  char *tn = cplusplus_type_name (type);

  fprintf (context->file, "static class iluObject * Create_%s(ilu_KernelObject obj) {\n", tn);
  fprintf (context->file, "\tclass %s *nobj = new %s;\n", tn, tn);
  fprintf (context->file, "\tnobj->ILUSetRPCObject(obj);\n");
  fprintf (context->file, "\tilu::SetLanguageSpecificObject(obj, (class iluObject *) nobj);\n");
  fprintf (context->file, "\treturn ((class iluObject *) nobj);\n}\n\n");
}

static void InitializeCacheSlot (Procedure m, Context context)
{
  if (IsCacheableMethod(m))
    fprintf (context->file, "\tCached_Value_for__%s__Bound = ilu_FALSE;\n", class_procedure_name(m));
}

static void generate_constructor_destructor (Type type, Context context)
{
  string tn = cplusplus_type_name(context->class);
  Class od = class_object(context->class);

  fprintf (context->file, "%s::%s ()\n{\n",
	   tn, tn);
  fprintf (context->file, "\tthis->ILUInstanceClassRecord = %s::ILUClassRecord;\n", tn);
  fprintf (context->file, "\tthis->ILUSetMostSpecificObject((void *) this);\n");
  list_enumerate (od->methods, (iluparser_EnumProc) InitializeCacheSlot, context);
  fprintf (context->file, "}\n\n");

  fprintf (context->file, "%s::~%s ()\n{\n", tn, tn);
  fprintf (context->file, "}\n\n");
}

void DeclareCallerReturnValue (Type type, Context context)
{
  if (type_ur_kind(type) != void_Type)
    {
      fprintf (context->file, "\t%s _retvalue;\n", cplusplus_return_type(type));
    }
}

static void ListArgument (Argument arg, Context context)
{
  fprintf (context->file, ", %s %s", cplusplus_parameter_type(arg->type, arg->direction), cplusplus_argument_name(arg));
}

static void generate_method_code (Procedure m, Context context)
{
  enum PrimitiveTypes t = type_ur_kind(m->returnType);

  /* print out formal params */

  fprintf(context->file, "%s %s::%s (%sStatus *_status",
	  cplusplus_return_type(m->returnType),
      cplusplus_type_name(context->class), class_procedure_name(m),
	  cplusplus_interface_name(context->interface));
  list_enumerate(m->arguments, (iluparser_EnumProc) ListArgument,
		 context);
  fprintf(context->file, ")\n{\n");

  /* declare local variables */
  DeclareCallerReturnValue(m->returnType, context);
  fprintf(context->file, "\tiluCall_s _call[1];\n");
  fprintf(context->file, "\tilu_Cardinal _dSize, _argSize = 0;\n");
  if (BlockingCall(m))
    fprintf(context->file,
     "\tilu_ProtocolException _perror;\n\tilu_Cardinal _scode;\n");

  /*
   * if method is functional, see if it is cached, and if so, return
   * the cached value
   */

  if (IsCacheableMethod(m)) {
    fprintf(context->file,
	    "\tif (this->Cached_Value_for__%s__Bound)\n\t   {\n",
	    class_procedure_name(m));
    fprintf(context->file, "\t    _status->returnCode = NULL;\n");
    fprintf(context->file, "\t    return (this->Cached_Value_for__%s);\n",
	    class_procedure_name(m));
    fprintf(context->file, "\t   };\n\n");
  }
  /* start code */
  fprintf(context->file,
	  "\tif (!ilu::StartCall (_call, this->ILUGetKernelServer(), %s::ILUClassRecord, MethodRecord_%s_%s))\n",
	  cplusplus_type_name(m->object),
	  cplusplus_type_name(context->class),
	  class_procedure_name(m));
  fprintf(context->file, "\t  goto faild;\n");
  if (!context->class->description->structuredDes.object->singleton)
    MarshS(context, "ilu::SizeOfObjectID(_call, this->ILUGetRPCObject(), ilu_TRUE, NULL)");
  list_enumerate(m->arguments, SizeArgument, context);
  MarshO(context, "ilu::StartRequest (_call, _argSize)");
  if (!context->class->description->structuredDes.object->singleton)
    MarshO(context,
	   "ilu::OutputObjectID (_call, %s, ilu_TRUE, NULL)",
	   "this->ILUGetRPCObject()");
  list_enumerate(m->arguments, EncodeArgument, context);
  MarshO(context, "ilu::FinishRequest (_call)");
  if (BlockingCall(m)) {
    fprintf(context->file,
	    "\t_perror = ilu::WaitForReply (_call, &_scode);\n");
    fprintf(context->file,
	    "\tif (ILU_ERRNOK(_call[0].err)) goto faild;\n");
    fprintf(context->file,
	    "\tif (_perror != ilu_ProtocolException_Success) {\n");
    fprintf(context->file,
	    "\t\t_status->returnCode = ilu::ProtocolError;\n");
    fprintf(context->file,
	    "\t\t_status->values.anyvalue = (ilu_Cardinal) _perror;\n");
    fprintf(context->file, "\t\tgoto returnnow;\n");
    fprintf(context->file, "\t}\n");


    fprintf(context->file, "\tif (_scode == 0)\n\t\t{\n");
    fprintf(context->file, "\t\t_status->returnCode = NULL;\n\t");
    if (t == void_Type)
      fprintf(context->file, "\t/* no return value */\n");
    else {
      char            buffer[100];
      char           *name = "_retvalue";
      enum PrimitiveTypes t = type_ur_kind(m->returnType);
      int             ref;

      if (t == record_Type OR t == union_Type)
	ref = TRUE;
      else if (t == array_Type) {
	TypeDescription d = type_description(m->returnType);
	Type            elemType = d->structuredDes.array.type;
	enum PrimitiveTypes ept = type_ur_kind(elemType);
	int             nDims = list_size(d->structuredDes.array.dimensions);

	if (nDims == 1 &&
	    (ept == byte_Type || ept == shortcharacter_Type)) {
	  ref = FALSE;
	  sprintf(buffer, "* (%s *) &%s",
		  ept == byte_Type ? "ilu_Byte *" : "ilu_CString",
		  name);
	  name = buffer;
	} else
	  ref = TRUE;
      } else
	ref = FALSE;
      UnpackValue(context, m->returnType, m->def, name, ref, TRUE);
      if (IsCacheableMethod(m)) {
	fprintf(context->file,
		"\t\tif (!this->Cached_Value_for__%s__Bound)\n",
		class_procedure_name(m));
	fprintf(context->file, "\t\t\t{\n");
	fprintf(context->file,
		"\t\t\tthis->Cached_Value_for__%s = _retvalue;\n",
		class_procedure_name(m));
	fprintf(context->file, "\t\t\tthis->Cached_Value_for__%s__Bound = ilu_TRUE;\n",
		class_procedure_name(m));
	fprintf(context->file, "\t\t\t};\n");
      }
    }
    list_enumerate(m->arguments, UnpackOutParm, context);
    fprintf(context->file, "\t}");
    if (list_size(m->exceptions) > 0) {
      fprintf(context->file, "\n\telse\n");
      fprintf(context->file,
	      "\t\t%sCatchException (_call, _status, _scode);\n",
	      cplusplus_interface_name(context->interface));
    } else
      fprintf(context->file, ";\n\t/* no exceptions to catch */\n");
    fprintf(context->file, "\tif (!ilu::ReplyRead(_call))\n");
    fprintf(context->file, "\t  goto faild;\n");
  }
  fprintf(context->file, "\tgoto returnnow;\n");
  fprintf(context->file, "faild:\n");
  fprintf(context->file, "\t_status->returnCode = ilu::ProtocolError;\n");
  fprintf(context->file,
	  "\t_status->values.anyvalue = (ilu_Cardinal) _call[0].call.ca_pe;\n");
  fprintf(context->file, "returnnow:\n");
  fprintf(context->file, "\tilu::FinishCall (_call);\n");
  fprintf(context->file, "\treturn");
  if (t != void_Type)
    fprintf(context->file, "(_retvalue)");
  fprintf(context->file, ";\n}\n\n");
}

void generate_class_code (Type type, Context context)
{
  context->class = type;

  fprintf(context->file, "ilu_Class %s::ILUClassRecord = NULL;\n\n",
	  cplusplus_type_name(type));
  generate_methods_shorts(type, context);
  generate_InstanceFromSBH(type, context);
  generate_CastDown(type, context);
  generate_constructor_destructor(type, context);

  list_enumerate((class_object(type))->methods,
	       (iluparser_EnumProc) generate_method_code, context);

  generate_staticCreate(type, context);
}
