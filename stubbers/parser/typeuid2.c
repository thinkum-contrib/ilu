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
/* Last edited by Mike Spreitzer January 11, 1999 12:04 pm PST */

/*
$Id: typeuid2.c,v 1.27 1999/08/03 01:50:20 janssen Exp $
*/

/*
  This file implements the type hash described in ILUSRC/doc/typehash.tim.
*/


#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO) */

#include <time.h>
#include "iluptype.h"

#ifdef ILU_TYPEUID_V2

#if (defined(WIN32) || defined(WIN16))
/* dll - added to get declaration for exit(int) */
#include <stdlib.h>
#endif

#define TRUE  1
#define FALSE 0

#ifndef macintosh
typedef int Boolean;
#endif


#define AND  &&
#define OR   ||
#define NOT  !

#define DOUBLE_QUOTE	'"'
#define BACKSLASH	'\\'

struct buffer_s {
  unsigned char *data;
  unsigned long size;
  unsigned long used;
};

typedef enum { TypeE, ExceptionE, InterfaceE } e_type;

typedef struct entity_s {
  e_type type;
  union {
    Type type;
    Exception exception;
    Interface interface;
  } value;
} *entity;

static boolean verbose = FALSE;

static char *full_type_name(Type t)
{
  string n;

  if ((n = name_lang_name (t->name, "parser:full")) == NULL)
    {
      char buf[1024];
      sprintf (buf, "%s%s%s.%s",
	       t->builtIn ? "ilu" : interface_name(t->interface),
	       t->builtIn ? "" : (t->interface->brand != NULL) ? "|" : "",
	       t->builtIn ? "" : (t->interface->brand != NULL) ? t->interface->brand : "",
	       type_name(t));
      name_set_lang_name (t->name, "parser:full", buf);
      n = name_lang_name(t->name, "parser:full");
    }
  return (n);
}

static Interface
  exception_interface (Exception e)
{
  if (e->importInterfaceName == NULL)
    return e->interface;
  else
    return exception_interface(e->import);
}

static void print0ToBuffer (struct buffer_s *buf, char *data)
{
  SIZE_T n = strlen(data);

  if ((buf->size - buf->used) < n)
    buf->data = (unsigned char *) iluparser_Realloc(buf->data, buf->size = buf->size + n + 1000);
  memmove (buf->data + buf->used, data, n);
  buf->used += n;
}

static void printmToBuffer(struct buffer_s *buf, char *format,...)
{
  va_list ap;	/* points to each unnamed arg. */
  char buf2[1024];

  va_start(ap,format);
  vsprintf (buf2, format, ap);
  print0ToBuffer (buf, buf2);
  va_end(ap);
}

/* returns TRUE if "t", an alias type, `defines a new type' by explicitly specifying a type UID */
static boolean
  redef_needed (Type t)
{
  if (type_kind(t) != alias_Type)
    return FALSE;
  if (type_uid(t) == NIL)
    return FALSE;
  else if (type_uid(ur_type(t)) == NIL)
    return TRUE;
  else
    return (strcmp(type_uid(t), type_uid(ur_type(t))) != 0);
}

static void
  add_quoted_string (char * brand, struct buffer_s *data)
{
  char * p;
  print0ToBuffer (data, "\"");
  for (p = brand; (p != NIL) && (*p != 0);  p++) {
    if ((*p == DOUBLE_QUOTE) || (*p == BACKSLASH))
      printmToBuffer(data, "\\%c", *p);
    else if ((*p < 0x20) || (*p > 0x7E))
      printmToBuffer(data, "\\%03u", *p);
    else
      printmToBuffer(data, "%c", *p);
  }
  print0ToBuffer (data, "\"");
}

static boolean
  match_entity (void *p1, void *p2)
{
  entity e = (entity) p1;

  return ((p2 != NULL) && ((p2 == ((void *) (e->value.type))) ||
			   (p2 == ((void *) (e->value.interface))) ||
			   (p2 == ((void *) (e->value.exception)))));
}

static void
  add_entity (e_type type, void *ptr, list l)
{
  entity e;

  if (l == NIL) return;

  if (list_find(l, match_entity, ptr) != NULL)
    return;

  e = (entity) malloc(sizeof(*e));
  e->type = type;
  switch (e->type) {
  case TypeE:
    e->value.type = (Type) ptr;
    break;

  case ExceptionE:
    e->value.exception = (Exception) ptr;
    break;

  case InterfaceE:
    e->value.interface = (Interface) ptr;
    break;
  }
  list_insert (l, e);
}

static void add_exnref (Exception exn, struct buffer_s *data, list referenced)
{
  print0ToBuffer (data, "(exn ");
  if (exn->corba_rep_id != NULL) {
    print0ToBuffer(data, "(id ");
    add_quoted_string(exn->corba_rep_id, data);
    print0ToBuffer(data, ")");
  } else {
    printmToBuffer (data, "(ref %s %s)", interface_name(exception_interface(exn)), exception_name(exn));
    add_entity (ExceptionE, exn, referenced);
  }
  printmToBuffer (data, ")");
}

static void add_typeref (Type t, struct buffer_s *data, list referenced)
{
  if ((t->uid != NIL) && t->explicit_uid) {
  } else if (type_kind(t) == alias_Type) {
    add_typeref (t->supertype, data, referenced);
  } else {
    switch (type_kind(t))
      {
      case byte_Type:
	print0ToBuffer (data, "byte");
	break;

      case shortcardinal_Type:
	print0ToBuffer (data, "shortcardinal");
	break;

      case cardinal_Type:
	print0ToBuffer (data, "cardinal");
	break;

      case longcardinal_Type:
	print0ToBuffer (data, "longcardinal");
	break;

      case shortinteger_Type:
	print0ToBuffer (data, "shortinteger");
	break;

      case integer_Type:
	print0ToBuffer (data, "integer");
	break;

      case longinteger_Type:
	print0ToBuffer (data, "longinteger");
	break;

      case shortreal_Type:
	print0ToBuffer (data, "shortreal");
	break;

      case real_Type:
	print0ToBuffer (data, "real");
	break;

      case longreal_Type:
	print0ToBuffer (data, "longreal");
	break;

      case shortcharacter_Type:
	print0ToBuffer (data, "shortcharacter");
	break;

      case character_Type:
	print0ToBuffer (data, "character");
	break;

#if 0
      case longcharacter_Type:
#endif

      case boolean_Type:
	print0ToBuffer (data, "boolean");
	break;

      case pickle_Type:
	print0ToBuffer (data, "pickle");
	break;

      case void_Type:
	print0ToBuffer (data, "void");
	break;

      case object_Type:
      case fixedpoint_Type:
      case string_Type:
      case union_Type:
      case sequence_Type:
      case record_Type:
      case array_Type:
      case enumeration_Type:
      case optional_Type:
      case reference_Type:
	printmToBuffer (data, "(ref %s %s)", interface_name(type_interface(t)), type_name(t));
	add_entity (InterfaceE, type_interface(t), referenced);
	add_entity (TypeE, t, referenced);
	break;

      default:
	fprintf (stderr, "Invalid type kind %d for built-in type %s\n",
		 type_basic_type(t), full_type_name(t));
	exit(1);
      }
  }
}

static void
  add_exceptiondesc (Exception e, struct buffer_s *data, /* OUT */ list referenced)
{
  printmToBuffer (data, "(exception %s %s \"\"",
		  interface_name(exception_interface(e)), exception_name(e));
  if ((e->type != NULL) && (type_ur_kind(e->type) != void_Type)) {
    print0ToBuffer (data, " ");
    add_typeref (e->type, data, referenced);
  } else {
    print0ToBuffer (data, " void");
  }
  print0ToBuffer (data, ")");
}

static void
  add_interfacedesc (Interface i, struct buffer_s *data)
{
  printmToBuffer (data, "(interface %s ", interface_name(i));
  add_quoted_string (i->brand, data);
  print0ToBuffer (data, ")");
}

static void
  add_constant (ConstantValue val, struct buffer_s *data)
{
  switch (val->type)
    {
    case integer_Type:
    case shortinteger_Type:
    case cardinal_Type:
    case shortcardinal_Type:
    case byte_Type:
      printmToBuffer (data, " %s%ld", (val->val.i.sign < 0) ? "-" : "", val->val.i.value);
      break;

    case shortcharacter_Type:
      /* this means enumeration element id */
      printmToBuffer (data, " %s", val->val.s);
      break;

    case boolean_Type:
      printmToBuffer (data, " %s", val->val.b ? "TRUE" : "FALSE");
      break;

    default:
      fprintf (stderr, "Bad constant-value type in union arm\n");
      return;
    }
}

static void
  add_integer_literal (IntegerLiteral lit, struct buffer_s *data)
{
  if (lit->negative)
    print0ToBuffer (data, "-");
  if (lit->small)
    printmToBuffer (data, "%lu", lit->val.direct);
  else
    printmToBuffer (data, "%s", lit->val.string);
}

#define interface_brand(x)		(x)->brand

static string
  argument_direction (Argument arg)
{
  switch (arg->direction) {
  case In:
    return "in";
  case Out:
    return "out";
  case InOut:
    return "inout";
  }
}

static void
  add_typedesc (Type t, struct buffer_s *data, /* OUT */ list referenced)
{
  unsigned int i, j;
  Argument arg;
  EnumField ef;
  Exception exn;
  Class obj;
  Procedure meth;
  struct ilu_integerLiteral_s lit;

  printmToBuffer (data, "(type %s %s ",
		  t->builtIn ? "ilu" : interface_name(type_interface(t)),
		  type_name(t));

  add_quoted_string(t->brand, data);
  switch (type_basic_type(t))
    {
    case array_Type:
      print0ToBuffer (data, " (array ");
      add_typeref (type_description(t)->structuredDes.array.type, data, referenced);
      for (i = 0;  i < list_size(type_description(t)->structuredDes.array.dimensions);  i++) {
	printmToBuffer (data, " (fixed %lu)",
			(unsigned) list_ref(type_description(t)->structuredDes.array.dimensions, i));
      }
      print0ToBuffer (data, ")");
      break;

    case sequence_Type:
      print0ToBuffer (data, " (sequence ");
      add_typeref (type_description(t)->structuredDes.sequence.type, data, referenced);
      printmToBuffer (data, " (variable %lu))",
		      (type_description(t)->structuredDes.sequence.limit == 0) ? 0xFFFFFFFF : ((unsigned long) type_description(t)->structuredDes.sequence.limit));
      break;

    case record_Type:
      print0ToBuffer (data, " (record");
      for (i = 0;  i < list_size(type_description(t)->structuredDes.record.fields); i++) {
	printmToBuffer (data, " (field %s ", argument_name((Argument) list_ref(type_description(t)->structuredDes.record.fields, i)));
	add_typeref (argument_type((Argument) list_ref(type_description(t)->structuredDes.record.fields, i)), data, referenced);
	print0ToBuffer (data, ")");
      };
      print0ToBuffer (data, ")");
      break;
      
    case optional_Type:
      print0ToBuffer (data, " (optional ");
      add_typeref (type_description(t)->structuredDes.optional, data, referenced);
      print0ToBuffer (data, ")");
      break;

    case reference_Type:
      print0ToBuffer (data, " (reference ");
      add_typeref (type_description(t)->structuredDes.reference.base_type, data, referenced);
      printmToBuffer (data, " (optional %s)",
		      type_description(t)->structuredDes.reference.optional ? "true" : "false");
      printmToBuffer (data, " (aliased %s)",
		      type_description(t)->structuredDes.reference.aliased ? "true" : "false");
      print0ToBuffer (data, ")");
      break;

    case union_Type:
      print0ToBuffer (data, " (union ");
      add_typeref (type_description(t)->structuredDes.uniond.discriminator_type, data, referenced);
      for (i = 0;  i < list_size(type_description(t)->structuredDes.uniond.types); i++) {
	arg = (Argument) list_ref(type_description(t)->structuredDes.uniond.types, i);
	print0ToBuffer (data, " (arm ");
	add_typeref (argument_type(arg), data, referenced);
	if (argument_name(arg) != NULL)
	  printmToBuffer (data, "(name %s) ", argument_name(arg));
	printmToBuffer (data, "(%s)",
			(arg == type_description(t)->structuredDes.uniond.default_arm) ? "default" : "");
	for (j = 0;  j < list_size(arg->values);  j++) {
	  print0ToBuffer(data, "(val");
	  add_constant (list_ref(arg->values, j), data);
	  print0ToBuffer(data, ")");
	}
	print0ToBuffer (data, ")");
      }
      if (type_description(t)->structuredDes.uniond.others_allowed)
	print0ToBuffer (data, " ((default) void)");
      print0ToBuffer (data, ")");
      break;

    case enumeration_Type:
      print0ToBuffer (data, " (enumeration");
      for (i = 0;  i < list_size(type_description(t)->structuredDes.enumeration);  i++) {
	ef = (EnumField) list_ref(type_description(t)->structuredDes.enumeration, i);
	printmToBuffer (data, " (element %s %d)", ef->name, ef->id);
      }
      print0ToBuffer (data, ")");
      break;

    case fixedpoint_Type:
      print0ToBuffer (data, " (fixedpoint ");
      add_integer_literal (type_description(t)->structuredDes.fixed.min_numerator, data);
      print0ToBuffer (data, " ");
      add_integer_literal (type_description(t)->structuredDes.fixed.max_numerator, data);
      print0ToBuffer (data, " ");
      lit = *type_description(t)->structuredDes.fixed.denominator;
      if (lit.negative) {
	lit.negative = FALSE;
	print0ToBuffer (data, "1/");
      }
      add_integer_literal (&lit, data);
      print0ToBuffer (data, ")");
      break;

    case string_Type:
      printmToBuffer (data, " (string %lu \"%s\" %u)",
		      type_description(t)->structuredDes.string.max_length,
		      ((type_description(t)->structuredDes.string.language != 0)
		       ? type_description(t)->structuredDes.string.language : ""),
		      type_description(t)->structuredDes.string.charset);
      break;

    case object_Type:
      obj = class_object(t);
      print0ToBuffer (data, " (object");
      if (obj->singleton != NULL)
	printmToBuffer (data, " (singleton \"%s\")", obj->singleton);
      if (obj->optional)
	print0ToBuffer (data, " optional");
      if (obj->collectible)
	print0ToBuffer (data, " collectible");
      for (i = 0;  i < list_size(obj->superclasses);  i++) {
	print0ToBuffer (data, " (supertype ");
	add_typeref ((Type) list_ref(obj->superclasses, i), data, referenced);
	print0ToBuffer (data, ")");
      }
      for (i = 0;  i < list_size(obj->methods); i++) {
	meth = (Procedure) list_ref(obj->methods, i);
	printmToBuffer (data, " (method %s %s%s(returns",
			name_base_name(meth->name),
			meth->asynch ? "asynchronous " : "",
			meth->functional ? "functional " : "");
	if (meth->returnType != NULL && (type_ur_kind(meth->returnType) != void_Type)) {
	  print0ToBuffer (data, " ");
	  add_typeref (meth->returnType, data, referenced);
	} else {
	  print0ToBuffer (data, " void");
	}
	for (j = 0;  j < list_size(meth->exceptions);  j++) {
	  print0ToBuffer (data, " ");
	  exn = (Exception) list_ref(meth->exceptions, j);
	  add_exnref (exn, data, referenced);
	}
	print0ToBuffer (data, ")");
	for (j = 0;  j < list_size(meth->arguments);  j++) {
	  arg = (Argument) list_ref(meth->arguments, j);
	  printmToBuffer (data, " (parameter %s %s ", argument_name(arg), argument_direction(arg));
	  add_typeref (argument_type(arg), data, referenced);
	  if (arg->sibling)
	    print0ToBuffer (data, " sibling");
	  print0ToBuffer(data, ")");
	}
	print0ToBuffer (data, ")");
      }
      print0ToBuffer (data, ")");
      break;

    case alias_Type:
      print0ToBuffer (data, " (redef ");
      add_typeref(under_type(t), data, referenced);
      printmToBuffer (data, " \"%s\")", type_uid(t));
      break;

    default:
      break;
    }
  print0ToBuffer (data, ")");
}  

static void form_typedesc (Type t, struct buffer_s *data)
{
  list referenced_elts;
  unsigned i;
  entity e;

  referenced_elts = new_list();
  add_typeref (t, data, referenced_elts);
  for (i = 0;  i < list_size(referenced_elts);  i++) {
    e = (entity) list_ref(referenced_elts, i);
    switch (e->type) {
    case TypeE:
      add_typedesc (e->value.type, data, referenced_elts);
      break;
    case InterfaceE:
      add_interfacedesc (e->value.interface, data);
      break;
    case ExceptionE:
      add_exceptiondesc (e->value.exception, data, referenced_elts);
      break;
    }
  }
  list_clear(referenced_elts, TRUE);
  free(referenced_elts);
}

#undef ILU_NIL
#include "shs.h"
#include <assert.h>

/*
 * convert a byte-string of len bytes to base-64
 */
static void convbase(unsigned char *bstring,unsigned long len,char *outbuf)
{
  static char *digits = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-+";
  char *result=outbuf;
  unsigned long i;
  unsigned nbits,nframe;
  unsigned long bbuf,topbits;	/* bit buffer */

  /* figure out the framing:  how many leading 0 bits do we place in the
   * MSB positions to make byte-string a multiple of 6 bits?
   */
  /* NB: outermoust % to prevent nframe of 6 */
  nframe=(6 - ((len * 8) % 6)) % 6;

  /* nbits keeps track of the number of bits currently in bbuf;  this
   * tracks the difference between the rate at which we put bits in to
   * bbuf and take bits out:  we shift in 8 bits at a time, and extract
   * 6 bits at a time;  nbits follows this difference.
   */
  for (i=0, nbits=nframe, bbuf=0; i < len; i++) {
    /* shift in another byte into bbuf */
    bbuf=(bbuf << 8) | bstring[i];
    nbits+=8;

    /* take out as many bits as we can in groups of 6 */
    while (nbits >= 6) {
      /* take the 6 most-significant bits first */
      topbits=(bbuf >> (nbits-6));
      *result++=digits[topbits];

      /* mask out the high-order bits we just extracted */
      nbits-=6;
      bbuf=bbuf & ((1 << nbits) - 1);
    }
  }
  /* we can assert nbits==0, because start framing ensures it */
  assert(nbits==0);
  *result='\0';
}

Type iluparser_CString_Type = NULL;

char *FigureTypeUID (Type t)
{
  struct buffer_s buffer;
  unsigned char hash[20];
  SHS_CTX ctx;
  static boolean initialized = FALSE;
  Type t2;

  if (! initialized)
    {
      verbose = (getenv ("ILU_TYPE_UID_VERBOSE") != NULL);
      initialized = TRUE;
    }


  if (type_uid(t) != NULL)
    return (type_uid(t));

  assert((t->importInterfaceName == NULL) || (type_kind(t) == alias_Type));

  if (type_kind(t) == alias_Type || t->importInterfaceName != NULL)
    return (type_uid(t) = ilu_strdup(FigureTypeUID(under_type(t))));

  if (type_kind(t) == object_Type &&
      class_object(t) != NULL &&
      class_object(t)->corba_rep_id != NULL)
    return (type_uid(t) = class_object(t)->corba_rep_id);

  if (verbose && !t->builtIn)
    fprintf(stderr,
	  "figuring 'ilut:' uid for <%s> (addr %p, ifc addr %p) from %s\n",
	    full_type_name(t), t, t->interface,
	    ((t->importInterfaceName != NULL)
	     ? t->importInterfaceName
	     : "(current ifc)"));
  buffer.data = (unsigned char *) iluparser_Malloc(buffer.size = 1024);
  buffer.used = 0;
  form_typedesc (t, &buffer);
  buffer.data[buffer.used] = '\0';

  if (verbose && ! t->builtIn)
    fprintf (stderr, "  buffer is <%*.*s>\n", buffer.used, buffer.used, buffer.data);

  SHSInit(&ctx);
  SHSUpdate (&ctx, buffer.data, buffer.used);
  SHSFinal (hash, &ctx);

/*
  {
    int i;

    fprintf (stderr, "  hash is ");
    for (i = 0;  i < 20;  i += 1)
      fprintf (stderr, "%u ", hash[i]);
    fprintf (stderr, "\n");
  }
*/

  type_uid(t) = (char *) iluparser_Malloc(40);
  strcpy (type_uid(t), "ilut:");
  /* convert to base 64 */
  convbase(hash,20,type_uid(t) + strlen(type_uid(t)));
  iluparser_Free(buffer.data);

  if (verbose && !t->builtIn)
    fprintf (stderr, "  uid for %s is %s\n", type_name(t), type_uid(t));

  if (iluparser_CString_Type == NULL &&
      t->importInterfaceName == NULL &&
      type_interface(t) != NULL &&
      strcmp(interface_name(type_interface(t)), "ilu") == 0 &&
      strcmp(type_name(t), "CString") == 0)
    iluparser_CString_Type = t;

  return (type_uid(t));
}

#endif /* ILU_TYPEUID_V2 */
