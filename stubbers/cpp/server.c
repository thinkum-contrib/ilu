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
$Id: server.c,v 1.36 1999/08/03 01:50:59 janssen Exp $
*/
/* Last edited by Mike Spreitzer July 11, 1996 2:09 pm PDT */

#ifdef WIN32
// dll - added to get declaration for getenv
#include <stdlib.h>
#endif

#include "cplusplus.h"
#include <stdlib.h>	/* for getenv */

static void DeclareCalleeReturnValue (Type type, Context context)
{
  if (type_ur_kind(type) != void_Type) {
    fprintf(context->file, "  %s _retvalue;\n",
	    cplusplus_return_type(type));
  }
}

static void CalleeListArgument (Argument a, Context context)
{
  enum PrimitiveTypes t = type_ur_kind (a->type);
  int takeAddress;
  
  if (t == record_Type || t == union_Type)
    takeAddress = TRUE;
  else if (t == array_Type)
    takeAddress = FALSE;
  else if (t == sequence_Type)
    {
      TypeDescription d = type_description(a->type);
      Type elementType = d->structuredDes.sequence.type;
      enum PrimitiveTypes ept = type_ur_kind(elementType);

      if (ept == shortcharacter_Type || ept == byte_Type)
	takeAddress = a->direction != In;
      else
	takeAddress = FALSE;
    }
  else
    takeAddress = a->direction != In;

  fprintf (context->file, ", %s%s", takeAddress ? "&" : "", cplusplus_argument_name(a));
}

void ReadLocalArg (Argument arg, Context context)
{
  if (arg->direction != Out) {
    enum PrimitiveTypes t = type_ur_kind(arg->type);
    int             allocate = t == object_Type || t == sequence_Type;
    int             ref;

    if (t == array_Type)
      ref = TRUE;
    else if (t == sequence_Type) {
      TypeDescription d = type_description(arg->type);
      Type            elementType = d->structuredDes.sequence.type;
      enum PrimitiveTypes ept = type_ur_kind(elementType);

      if (ept == shortcharacter_Type || ept == byte_Type)
	ref = FALSE;
      else
	ref = TRUE;
    } else
      ref = FALSE;

    UnpackValue(context, arg->type, arg->def,
		cplusplus_argument_name(arg), ref, allocate);
  }
}

static void LocalArgExpression (Argument arg, char *expr)
{
  enum PrimitiveTypes t = type_ur_kind (arg->type);
  int takeAddress;

  if (t == record_Type || t == union_Type)
    takeAddress = TRUE;
  else
    takeAddress = FALSE;

  sprintf (expr, "%s%s", takeAddress ? "&" : "", cplusplus_argument_name(arg));
}

static void SizeLocalArg(refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  if (arg->direction != In) {
    char            argExpr[1000];
    LocalArgExpression(arg, argExpr);
    SizeType(arg->type, argExpr, context);
  }
}

void WriteLocalArg (refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  if (arg->direction != In) {
    char            argExpr[1000];
    LocalArgExpression(arg, argExpr);
    EncodeValue(arg->type, argExpr, context);
  }
}

static void DeclareLocalArgument (refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  enum PrimitiveTypes t = type_ur_kind(arg->type);
  char           *argTypeName;

  if (t == object_Type)
    argTypeName = cplusplus_return_type(arg->type);
  else
    argTypeName = cplusplus_type_name(arg->type);
  fprintf(context->file, "    %s %s", argTypeName,
	  cplusplus_argument_name(arg));
  if (arg->direction == Out && t == sequence_Type) {
    TypeDescription d = type_description(arg->type);
    Type            elementType = d->structuredDes.sequence.type;
    enum PrimitiveTypes ept = type_ur_kind(elementType);

    if (ept != shortcharacter_Type && ept != byte_Type)
      fprintf(context->file, " = new _%s_sequence", argTypeName);
  }
  fprintf(context->file, ";\n");
}

static void FreeLocalArg (Argument arg, Context context)
{
  if (arg->direction != In)
    {
      char argExpr[1000];

      LocalArgExpression (arg, argExpr);
      FreeValue (arg->type, argExpr, context);
    }

  if (type_ur_kind(arg->type) == sequence_Type)
    {
      enum PrimitiveTypes ept = type_ur_kind(type_description(arg->type)->structuredDes.sequence.type);

      if (ept != shortcharacter_Type && ept != byte_Type)
	fprintf (context->file, "\tfree((char *) %s);\n", cplusplus_argument_name(arg));
    }
}

static void GenerateCalleeStub (Procedure m, Context context)
{
  char           *retbuf;
  char           *tn = cplusplus_type_name(m->object);

  fprintf(context->file, "void _%s_%s_stub (iluCall _call)\n{\n",
	  tn, cplusplus_simple_name(m->name));

  fprintf(context->file, "    %sStatus _status;\n",
	  cplusplus_interface_name(m->interface));

  list_enumerate(m->arguments, DeclareLocalArgument, context);

  DeclareCalleeReturnValue(m->returnType, context);
  fprintf(context->file, "    %s *_realobj;\n\n", tn);
  fprintf(context->file,
	  "    _realobj = (%s *) iluObject::InputObject(_call, ilu_TRUE, %s::ILUClassRecord);\n",
	  tn, tn);
  list_enumerate(m->arguments,
		 (iluparser_EnumProc) ReadLocalArg, context);
  MarshO(context, "ilu::FinishParameters(_call, (void *) _realobj)");

  fprintf(context->file, "    _status.returnCode = NULL;\n");
  fprintf(context->file, "    _status.callerPassport = _call->call.ca_caller;\n");
  fprintf(context->file, "    %s_realobj->%s (&_status",
	  ((type_ur_kind(m->returnType) == void_Type)
	   ? ""
	   : "_retvalue = "),
	  class_procedure_name(m));
  list_enumerate(m->arguments,
		 (iluparser_EnumProc) CalleeListArgument, context);
  fprintf(context->file, ");\n\n");
  if (m->asynch &&
      getenv("CPLUSPLUS_SEND_REPLY_ON_ASYNCH") == NULL)
    /* getenv(..) included only for testing */
  {
    fprintf(context->file,
	    "    /* asynchronous method -- no reply */\n");
    fprintf(context->file, "    ilu::NoReply (_call);\n");
  } else {
    fprintf(context->file, "    if (_status.returnCode == NULL) {\n");
    fprintf(context->file, "\tilu_Cardinal _dSize, _argSize = 0;\n");
    if (TypeIsArray(m->returnType))
      retbuf = "((_retvalue == NULL) ? 0 : *_retvalue)";
    else
      retbuf = "_retvalue";

    fprintf(context->file, "\t_argSize=ilu::BeginSizingReply");
    fprintf(context->file, "(_call, %s);\n",
	(list_size(m->exceptions) > 0) ? "ilu_TRUE" : "ilu_FALSE");
    SizeType(m->returnType, retbuf, context);
    list_enumerate(m->arguments, SizeLocalArg, context);
    MarshO(context, "ilu::BeginReply (_call, %s, _argSize)",
	(list_size(m->exceptions) > 0) ? "ilu_TRUE" : "ilu_FALSE");
    if (type_ur_kind(m->returnType) != void_Type) {
      EncodeValue(m->returnType, retbuf, context);
    }
    list_enumerate(m->arguments, WriteLocalArg, context);

    fprintf(context->file, "\tilu::FinishReply (_call);\n\t}\n");
    fprintf(context->file, "    else {\n");
    fprintf(context->file,
	    "\t%s_G::SendException (_call, &_status);\n",
	    cplusplus_interface_name(m->interface));
    fprintf(context->file, "    }\n");
    list_enumerate(m->arguments,
		   (iluparser_EnumProc) FreeLocalArg, context);
    if (type_ur_kind(m->returnType) != void_Type)
      FreeValue(m->returnType, "_retvalue", context);
  }
  fprintf(context->file, "faild:\n");
  fprintf(context->file, "    return;\n}\n\n");
}

static void InitializeStubPointer (Procedure method, Context context)
{
  fprintf (context->file, "\tilu_SetMethodStubProc(MethodRecord_%s_%s, (ilu_StubProc) _%s_%s_stub, ilu::CppLangIdx());\n",
	   cplusplus_type_name(method->object), cplusplus_simple_name(method->name),
	   cplusplus_type_name(method->object), cplusplus_simple_name(method->name));
}

static void SetupServerStubsInMethodTable (Type type, Context context)
{
  Class od;

  if (type == NULL || type_basic_type(type) != object_Type || (od = class_object(type)) == NULL)
    return;

  list_enumerate (od->methods, (iluparser_EnumProc) InitializeStubPointer, context);
}

static void ClassSetupStubs (Type class, Context context)
{
  Class od;

  if (class == NULL || type_basic_type(class) != object_Type || (od = class_object(class)) == NULL)
    return;

  list_enumerate (od->methods, (iluparser_EnumProc) GenerateCalleeStub, context);
}

static void generate_registration_code (Interface interface, Context context)
{
  char           *interface_name = (char *) cplusplus_interface_name(interface);

  fprintf(context->file, "\n\n/* the following is all done to achieve load-time module initialization.\n");
  fprintf(context->file, "   We declare a private class which only has one instance, statically declared.\n");
  fprintf(context->file, "   We use the constructor of the class to do all the initializations we need\n");
  fprintf(context->file, "   for the module, trusting that the single static instance of the class will\n");
  fprintf(context->file, "   be initialized before the user code is given control. */\n\n");

  fprintf(context->file,
	  "class _%s_InitializeServerClass {\n\n public:\n\n",
	  interface_name);
  fprintf(context->file,
	  "  _%s_InitializeServerClass();\n};\n\n",
	  interface_name);

  fprintf(context->file,
	  "#ifndef macintosh\nstatic class _%s_InitializeServerClass _%s_InitializationInstance;\n#endif\n\n",
	  interface_name, interface_name);

  fprintf(context->file, "void %s__InitializeServer(void)\n{\n",
	  interface_name);
  fprintf(context->file, "\tstatic int initialized = 0;\n\n");
  fprintf(context->file, "\tif (initialized) return;\n");
  fprintf(context->file, "\tinitialized = 1;\n\n");
  fprintf(context->file, "\t%s__Initialize();\n", interface_name);
  list_enumerate(interface->classes,
      (iluparser_EnumProc) SetupServerStubsInMethodTable, context);
  fprintf(context->file, "}\n\n");

  fprintf(context->file, "_%s_InitializeServerClass::_%s_InitializeServerClass()\n{\n",
	  interface_name, interface_name);
  fprintf(context->file, "\t%s__InitializeServer();\n}\n\n",
	  interface_name);
}

void generate_server_code (Interface parse, FILE *file)
{
  char*  pc_interfacename;
  struct context_s context;

  context.file = file;
  context.interface = parse;

  fprintf (file, "#include <ilu.hh>\n");
  /* get any translation of what the header file for the interface is */
  pc_interfacename = interface_header_name(cplusplus_interface_name(parse));

  fprintf (file, "#include \"%s.hh\"\n\n", pc_interfacename);

  fprintf (file, "extern \"C\" {\n#include <stdio.h>\n};\n");

  fprintf(file, "extern ilu_Class %sClasses;\n\n",
	  cplusplus_interface_name(parse));

  list_enumerate(parse->classes, (iluparser_EnumProc) ClassSetupStubs,
		 &context);

  MethodRecordID = 0;
  list_enumerate(parse->classes, (iluparser_EnumProc) DefineMethods,
		 &context);
  generate_registration_code (parse, &context);
}
