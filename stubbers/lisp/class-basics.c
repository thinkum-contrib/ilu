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
$Id: class-basics.c,v 1.20 1999/08/03 01:50:07 janssen Exp $
*/

#include "lisp.h"

static void OutputArg (Argument v, Context context)
{
  fprintf (context->file, " (%s", lisp_argument_name(v));
  OutputTypeName (v->type, context);
  if (v->direction == InOut)
    fprintf (context->file, " :inout");
  else if (v->direction == Out)
    fprintf (context->file, " :out");
  else if (v->direction == In)
    fprintf (context->file, " :in");
  fprintf (context->file, " \"%s\" %s)",
	   type_uid(v->type), v->sibling ? "t" : "nil");
}

static void OutputExceptionName (Exception e, Context context)
{
  fprintf (context->file, "(");
  if (e->corba_rep_id != NULL)
    fprintf (context->file, " \"%s\"", e->corba_rep_id);
  else
    fprintf (context->file, " \"ilu:%s.%s\"",
	     interface_name(e->interface), exception_name(e));
  if (e->type != NULL)
    fprintf (context->file, " \"%s\"", type_uid(e->type));
  fprintf (context->file, ")");
}

static void OutputMethodSig (Procedure m, Context context)
{
  fprintf (context->file, "  (\"%s\" %s %u %s %s ",
	   procedure_name(m),
	   lisp_procedure_name(m),
	   m->id,
	   m->functional ? "cl:t" : "cl:nil",
	   m->asynch ? "cl:t" : "cl:nil");
  if (m->returnType == NULL)
    fprintf (context->file, " cl:nil");
  else {
    fprintf (context->file, " ( \"%s\" ", type_uid(m->returnType));
    OutputTypeName (m->returnType, context);
    fprintf (context->file, " )");
  }
  fprintf (context->file, " (");
  list_enumerate (m->arguments, (EnumProc) OutputArg, context);
  fprintf (context->file, ") (");
  list_enumerate (m->exceptions, (EnumProc) OutputExceptionName, context);
  fprintf (context->file, "))\n");
}

static void GenerateMethod (Procedure m, Context context)
{
  Class c = type_description(m->object)->structuredDes.object;

  fprintf (context->file, "(ilu:define-method %s %s\n %u\t; id\n"
	   " %s\t; singleton-p\n %s\t; functional\n %s\t; asynch\n",
	   lisp_procedure_name(m), old_lisp_procedure_name(m), m->id, c->singleton ? "cl:t" : "cl:nil",
	   m->functional ? "cl:t" : "cl:nil", m->asynch ? "cl:t" : "cl:nil");
  fprintf (context->file, " (\t; arguments\n  (self");
  OutputTypeName (m->object, context);
  fprintf (context->file, ")");
  list_enumerate (m->arguments, (EnumProc) OutputArg, context);
  fprintf (context->file, ")\n (");
  list_enumerate (m->exceptions, (EnumProc) OutputExceptionName, context);
  fprintf (context->file, ")\t; exceptions\n");
  if (m->returnType == NULL)
    fprintf (context->file, " cl:nil");
  else
    OutputTypeName (m->returnType, context);
  fprintf (context->file, "\t; return type\n)\n\n");
}

static void PrintClassName (Type type, Context context)
{
  Type st = type;

  while (st->supertype != NULL)
    st = st->supertype;

  fprintf (context->file, " %s", lisp_type_name(st));
}

static void PrintClassID (Type type, Context context)
{
  Type st = ur_type(type);

  fprintf (context->file, " \"%s\"", st->uid);
}

void GenerateClassDefinition (Type class, Context context)
{
  Class c;
  static char *ilu_version = NULL;

  if (ilu_version == NULL)
    ilu_version = iluparser_GetILUVersionString();

  if (type_basic_type(class) != object_Type)	/* might be alias of object type */
    return;

  c = type_description(class)->structuredDes.object;

  fprintf (context->file, "(ilu:define-class-type %s\n (", lisp_type_name(class));
  list_enumerate(c->superclasses, (EnumProc) PrintClassName, context);
  fprintf (context->file, ")\t; superclasses\n (");
  list_enumerate(c->superclasses, (EnumProc) PrintClassID, context);
  fprintf (context->file, ")\t; superclass IDs\n %s%s%s\t; singleton?\n %s%s%s\t; authentication\n",
	   (c->singleton == NULL) ? "" : "\"",
	   (c->singleton == NULL) ? "cl:nil" : c->singleton,
	   (c->singleton == NULL) ? "" : "\"",
	   (c->authentication == NULL) ? "" : "\"",
	   (c->authentication == NULL) ? "cl:nil" : c->authentication,
	   (c->authentication == NULL) ? "" : "\"");
  fprintf (context->file, " %s%s%s\t; brand\n %s\t; optional?\n %s\t; collectible?\n (\t; methods\n",
	   (c->brand == NULL) ? "" : "\"",
	   (c->brand == NULL) ? "cl:nil" : c->brand,
	   (c->brand == NULL) ? "" : "\"",
	   (c->optional) ? "cl:t" : "cl:nil",
	   (c->collectible) ? "cl:t" : "cl:nil");
  list_enumerate (c->methods, (EnumProc) OutputMethodSig, context);
  fprintf (context->file, "  )\n \"%s\" \"%s\" \"%s\" %s%s%s \"%s\"\n %s%s%s)\n\n",
	   ilu_version, type_name(class), interface_name(class->interface),
	   (class->interface->brand == NULL) ? "" : "\"",
	   (class->interface->brand == NULL) ? "cl:nil" : class->interface->brand,
	   (class->interface->brand == NULL) ? "" : "\"",
	   class->uid,
	   (c->doc_string == NULL) ? "" : "\"",
	   (c->doc_string == NULL) ? "cl:nil" : c->doc_string,
	   (c->doc_string == NULL) ? "" : "\"");
}

void GenerateClassCode (Type class, Context context)
{
  Class c;

  if (type_basic_type(class) != object_Type)	/* might be an alias */
    return;

  c = type_description(class)->structuredDes.object;

  fprintf (context->file, "(ilu:define-class %s)\t; get-class-fn\n\n",
	   lisp_type_name(class));

  list_enumerate (c->methods, (EnumProc) GenerateMethod, context);
}

