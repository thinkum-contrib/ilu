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
$Id: class-server.c,v 1.14 1999/08/03 01:50:08 janssen Exp $
*/

#include "lisp.h"

extern void PrefixedExceptionName (Exception e, Context context);

static void OutputMethodArg (Argument v, Context context)
{
  fprintf (context->file, " (%s", lisp_argument_name(v));
  OutputTypeName (v->type, context);
  if (v->direction == InOut)
    fprintf (context->file, " :inout");
  else if (v->direction == Out)
    fprintf (context->file, " :out");
  else if (v->direction == In)
    fprintf (context->file, " :in");
  fprintf (context->file, ")");
}

static void GenerateMethodStub (Procedure m, Context context)
{
  string lisp_type_name(Type t);

  fprintf (context->file, "(ilu:define-method-server-stub %s %s\n |%s.%s.%s|\t; dispatching symbol\n %s\t; singleton-p\n",
	   lisp_procedure_name(m), old_lisp_procedure_name(m),
	   interface_name(type_interface(m->object)), type_name(m->object), procedure_name(m),
	   type_description(m->object)->structuredDes.object->singleton ? "cl:t" : "cl:nil");
  fprintf (context->file, " %s\t; functional-p\n %s\t; asynchronous-p\n ((self %s.impl)",
	   m->functional ? "cl:t" : "cl:nil", m->asynch ? "cl:t" : "cl:nil",
	   lisp_simple_name(m->object->name));
  list_enumerate (m->arguments, (EnumProc) OutputMethodArg, context);
  fprintf (context->file, ")\t; arguments\n (");
  list_enumerate (m->exceptions, (EnumProc) PrefixedExceptionName, context);
  fprintf (context->file, ")\t; exceptions\n");
  if (m->returnType != NULL)
    OutputTypeName (m->returnType, context);
  else
    fprintf (context->file, " cl:nil");
  fprintf (context->file, "\t; return-type\n )\n\n");
}


static void GenerateMethodStubs (Type t, Context context)
{
  Class od;

  if (t == NULL || type_basic_type(t) != object_Type || (od = class_object(t)) == NULL)
    return;

  list_enumerate (od->methods, (EnumProc) GenerateMethodStub, context);
}

static void GenerateSStubs2 (Type class, Context context)
{
  fprintf (context->file, "(ilu:define-server-class %s.impl %s)\n\n",
	   lisp_simple_name(class->name), lisp_type_name(class));

  GenerateMethodStubs (class, context);
}

void GenerateSStubs (Interface interface, FILE *file)
{
  struct context_s context;

  context.file = file;
  context.interface = interface;

  fprintf (file, "(cl:in-package :%s)\n\n", lisp_interface_name(interface));

  list_enumerate (interface->classes, (EnumProc) GenerateSStubs2, &context);
}
