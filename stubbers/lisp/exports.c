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
$Id: exports.c,v 1.14 1999/08/03 01:50:12 janssen Exp $
*/

#include "lisp.h"

static char *OMGIDLExceptionTypePrefix = "ilu--prefix-idlExceptionType-";

struct k1 {
  FILE *file;
  Type type;
};

static void ExportMethodName (Procedure m, FILE *file)
{
  char *name = lisp_procedure_name(m);
  /* ignore setf methods */
  if (strncmp(name, "(setf ", 6) != 0
      || name[strlen(name)-1] != ')')
    fprintf (file, "   #:%s\n", name);
  name = old_lisp_procedure_name(m);
  if (strncmp(name, "(setf ", 6) != 0
      || name[strlen(name)-1] != ')')
    fprintf (file, "   #+ilu-old-method-names\n   #:%s\n", name);
}

static void ExportObject (Type type, FILE *file)
{
  struct k1 k;
  Class c;

  c = type_description(type)->structuredDes.object;
  k.file = file;
  k.type = type;
  fprintf (file, "   #:%s\n", lisp_simple_name(type->name));
  list_enumerate (c->methods, (EnumProc) ExportMethodName, file);
}

static void EnumerateEnumerationSymbol (EnumField ef, FILE *file)
{
  fprintf (file, "   #:%s\n", lisp_string(ef->name));
}

static void ExportType (Type type, FILE *file)
{
  enum PrimitiveTypes t = type_basic_type(type);
  TypeDescription d = type_description(type);

  if (type->builtIn || t == invalid_Type || t == void_Type || type->importInterfaceName != NULL)
    return;

  if (strncmp(OMGIDLExceptionTypePrefix, name_base_name(type->name), strlen(OMGIDLExceptionTypePrefix)) == 0)
    return;

  switch (t)
    {
    case enumeration_Type:
      fprintf (file, "   #:%s\n", lisp_simple_name(type->name));
      list_enumerate (d->structuredDes.enumeration, (EnumProc) EnumerateEnumerationSymbol, file);
      break;

    case byte_Type:
    case shortinteger_Type:
    case shortcharacter_Type:
    case character_Type:
    case integer_Type:
    case shortcardinal_Type:
    case cardinal_Type:
    case real_Type:
    case shortreal_Type:
    case union_Type:
    case sequence_Type:
    case optional_Type:
    case alias_Type:
    case array_Type:
      fprintf (file, "   #:%s\n", lisp_simple_name(type->name));
      break;

    case record_Type:
      fprintf (file, "   #:%s\n", lisp_simple_name(type->name));
/*
      k.file = file;
      k.type = type;
      list_enumerate (d->structuredDes.record.fields, ExportRecordField, &k);
*/
      break;

    case object_Type:
      ExportObject (type, file);
      break;

    default:
      break;
    };
}

static void ExportException (Exception e, FILE *file)
{
  if (e->importInterfaceName == NULL)
    fprintf (file, "   #:%s\n", lisp_simple_name(e->name));
}

static void ExportConstant (Constant c, FILE *file)
{
  if (c->importInterfaceName == NULL)
    fprintf (file, "   #:%s\n", lisp_simple_name(c->name));
}

void ExportPackage (Interface interface, FILE *file)
{
  fprintf (file, "(cl:defpackage :%s\n  (:use :common-lisp :ilu)\n  (:shadow\n",
	   lisp_interface_name(interface));
  list_enumerate (interface->constants, (EnumProc) ExportConstant, file);
  list_enumerate (interface->types, (EnumProc) ExportType, file);
  list_enumerate (interface->exceptions, (EnumProc) ExportException, file);
  fprintf (file, "  )\n  (:export\n");
  list_enumerate (interface->constants, (EnumProc) ExportConstant, file);
  list_enumerate (interface->types, (EnumProc) ExportType, file);
  list_enumerate (interface->exceptions, (EnumProc) ExportException, file);
  fprintf (file, "  ))\n\n(cl:in-package :%s)\n\n", lisp_interface_name(interface));
}
