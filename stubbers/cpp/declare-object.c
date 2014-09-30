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
/* Last edited by Mike Spreitzer December 7, 1995 2:18 pm PST */

/*
$Id: declare-object.c,v 1.20 1999/08/03 01:50:59 janssen Exp $
*/
#include "cplusplus.h"

static void DeclareMethodArgument (Argument a, Context context)
{
  fprintf (context->file, ", %s %s", cplusplus_parameter_type(a->type, a->direction), cplusplus_argument_name(a));
}

static void DeclareMethod (Procedure m, Context context)
{
  string s;

  s = cplusplus_return_type(m->returnType);
  fprintf (context->file, "  virtual %s %s (%sStatus *_status",
	   (s == NULL || *s == '\0') ? (string) "void" : s,
	   class_procedure_name(m),
	   cplusplus_interface_name(context->interface));
  list_enumerate (m->arguments, (iluparser_EnumProc) DeclareMethodArgument, context);
  fprintf (context->file, ");\n");
}

boolean IsCacheableMethod (Procedure m)
{
  enum PrimitiveTypes t = type_basic_type(m->returnType);

  return (m->functional && list_size(m->arguments) == 0
	  && (t == object_Type || t == enumeration_Type || t == byte_Type
	      || t == integer_Type || t == shortinteger_Type
	      || t == cardinal_Type || t == shortcardinal_Type
	      || t == real_Type || t == shortreal_Type
	      || t == character_Type || t == shortcharacter_Type));
}

static void DeclareSlotForFunctional (Procedure m, Context context)
{
  if (IsCacheableMethod(m))
    {
      fprintf (context->file, "  %s Cached_Value_for__%s;\n", cplusplus_return_type(m->returnType),
	       class_procedure_name(m));
      fprintf (context->file, "  ilu_Boolean Cached_Value_for__%s__Bound;\n", class_procedure_name(m));
    }
}

static boolean AddComma = FALSE;

static void ListSupertype (Type t, Context context)
{
  Type st = ur_type(t);

  fprintf (context->file, "%spublic virtual %s", AddComma ? ", " : "", cplusplus_type_name(st));
  AddComma = TRUE;
}

void declare_object_type (Type type, Context c)
{
  Class od;

  if (type_basic_type(type) != object_Type)
    return;

  od = class_object(type);

  c->class = type;

  fprintf (c->file, "\n\n/* declaration of C++ class \"%s\"\n   from ILU class \"%s:%s\"  */\n\n",
	   cplusplus_type_name(type), interface_name(type->interface), type_name(type));

  fprintf (c->file, "\nclass %s : ", cplusplus_type_name(type));
  AddComma = FALSE;
  if (od->superclasses != NULL && list_size(od->superclasses) > 0)
    list_enumerate (od->superclasses, (iluparser_EnumProc) ListSupertype, c);
  else
    fprintf (c->file, "%spublic virtual iluObject", AddComma ? ", " : "");
  fprintf (c->file, " {\n\n public:\n\n");

  /* create constructor and destructor */

  fprintf (c->file, "  %s();			// constructor\n", cplusplus_type_name(type));
  fprintf (c->file, "  virtual ~%s();		// destructor\n\n", cplusplus_type_name(type));

  /* add create from SBH and method to retrieve class record */

  fprintf (c->file, " // class procedures\n\n");
  fprintf (c->file, "  static class %s * ILUCreateFromSBH(ilu_CString sbh);\n", cplusplus_type_name(type));
  fprintf (c->file, "  static class %s * ILUQuaT (class iluObject *from);\n\n",
	   cplusplus_type_name(type));

  fprintf (c->file, " // public variables\n\n");

  fprintf (c->file, "  static ilu_Class ILUClassRecord;\n");

  fprintf (c->file, " // methods\n\n");

  fprintf (c->file, "  virtual void * ILUCastDown (ilu_Class cast_to);\n\n");

  list_enumerate (od->methods, (iluparser_EnumProc) DeclareMethod, c);

  /* for any methods that are functional and have no arguments, generate slots in which to
     cache the values */

  fprintf (c->file, "\n // data slots for cacheable methods -- if any\n\n");
  list_enumerate (od->methods, (iluparser_EnumProc) DeclareSlotForFunctional, c);

  /* finish off the class */

  fprintf (c->file, "};\n\n");
}
