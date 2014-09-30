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
$Id: typeuid.c,v 1.57 1999/08/03 01:50:18 janssen Exp $
*/
/* Last edited by Mike Spreitzer June 30, 1998 10:53 pm PDT */


#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO) */

#include <time.h>
#include "iluptype.h"

#ifndef ILU_TYPEUID_V2

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

struct buffer_s {
  unsigned char *data;
  unsigned long size;
  unsigned long used;
};


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

static void ast_recurse(Type t, struct buffer_s *data);

static void recurseEnumFields (EnumField field, struct buffer_s *data)
{
  printmToBuffer (data, " %s", field->name);
}

static void recurseArrayDimensions (unsigned long dim, struct buffer_s *data)
{
  printmToBuffer (data, " %u", dim);
}

static void recurseRecordFields (Argument field, struct buffer_s *data)
{
  if (field->type != NULL)
    {
      print0ToBuffer (data, " ");
      ast_recurse (field->type, data);
    }
}

static void recurseConstantValue (ConstantValue val, struct buffer_s *data)
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
      printmToBuffer (data, " %s", val->val.s);
      break;

    case boolean_Type:
      printmToBuffer (data, " %s", val->val.b ? "TRUE" : "FALSE");
      break;

    default:
      fprintf (stderr, "Bad value type in union arm\n");
      return;
    }
}

static void recurseUnionFields (Argument field, struct buffer_s *data)
{
  print0ToBuffer (data, " (");
  if (field->type != NULL)
    {
      ast_recurse (field->type, data);
    }
  list_enumerate (field->values, (iluparser_EnumProc) recurseConstantValue, data);
  print0ToBuffer (data, ")");
}

static void recurseMethodArgs (Argument field, struct buffer_s *data)
{
  print0ToBuffer (data, " (");
  if (field->type != NULL)
    {
      printmToBuffer (data, "%s %s ",
		      (field->direction == In) ? "in" :
		      ((field->direction == Out) ? "out" : "inout"),
		      field->sibling ? " sibling" : "notsibling");		       
      ast_recurse (field->type, data);
    }
  print0ToBuffer (data, ")");
}

static void recurseException (Exception e, struct buffer_s *data)
{
  printmToBuffer (data, " (%s %s", exception_name(e),
		  (e->interface->brand == NULL) ? "ilu_no_interface_brand" : e->interface->brand);
  if (e->type != NULL && type_basic_type(e->type) != void_Type)
    {
      print0ToBuffer (data, " ");
      ast_recurse (e->type, data);
    }
  print0ToBuffer (data, ")");
}

static void recurseObjectMethods (Procedure proc, struct buffer_s *data)
{
  printmToBuffer (data, "(method %s %s %s", name_base_name(proc->name),
		  proc->asynch ? "asynchronous " : "synchronous",
		  proc->functional ? "functional" : "volatile");
  list_enumerate (proc->arguments, (void (*)(refany, refany)) recurseMethodArgs, data);
  print0ToBuffer (data, " ");
  if (proc->returnType != NULL && type_basic_type(proc->returnType) != void_Type)
    ast_recurse (proc->returnType, data);
  else
    print0ToBuffer (data, "noreturntype");
  list_enumerate (proc->exceptions, (void (*)(refany, refany)) recurseException, data);
  print0ToBuffer (data, ")");
}

static char * figure_metatype (Class c)
{
  static char buf[1024];
  char *p;

  if (c->singleton != NULL)
    {
      strcpy (buf, c->singleton);
      p = strtok(buf, "_");
      if (p == NULL)
	return buf;
      else
	return p;
    }
  else if (c->collectible)
    return "ILU-collectible";
  else
    return "ILU-noncollectible";
}

static int ast_level = 0;

static void ast_recurse (Type t, struct buffer_s *data)
{
  if (t == NULL)
    return;
  if (t->marked)
    {
      print0ToBuffer (data, full_type_name(t));
      return;
    }

  ast_level += 1;

#if 0
  if (verbose)
    {
      int i;
      for (i = 0;  i < ast_level;  i++)
	fprintf (stderr, " .");
      fprintf (stderr, " %s\n", full_type_name(t));
    }
#endif

  t->marked = TRUE;
  if (type_kind(t) == alias_Type)
    ast_recurse (ur_type(t), data);
  else if (t->builtIn)
    {
      printmToBuffer (data, "(%s)", type_name(t));
    }
  else if (type_basic_type(t) == fixedpoint_Type)
    {
      IntegerLiteral min, max, denom;
      printmToBuffer (data, "(fixedpoint ");
      min = type_description(t)->structuredDes.fixed.min_numerator;
      max = type_description(t)->structuredDes.fixed.max_numerator;
      denom = type_description(t)->structuredDes.fixed.denominator;
      if (min) {
	if (min->negative)
	  print0ToBuffer (data, "-");
	if (min->small)
	  printmToBuffer (data, "%lu ", min->val.direct);
	else
	  printmToBuffer (data, "%s ", min->val.string);
      } else {
	print0ToBuffer (data, "nomin ");
      }
      if (max) {
	if (max->negative)
	  print0ToBuffer (data, "-");
	if (max->small)
	  printmToBuffer (data, "%lu ", max->val.direct);
	else
	  printmToBuffer (data, "%s ", max->val.string);
      } else {
	print0ToBuffer (data, "nomax ");
      }
      if (denom->negative)
	print0ToBuffer (data, "-");
      if (denom->small)
	printmToBuffer (data, "%lu", denom->val.direct);
      else
	printmToBuffer (data, "%s", denom->val.string);
      print0ToBuffer (data, ")");
    }
  else if (type_basic_type(t) == string_Type)
    {
      printmToBuffer (data, "(string %lu %lu %s)",
		      type_description(t)->structuredDes.string.max_length,
		      type_description(t)->structuredDes.string.charset,
		      (type_description(t)->structuredDes.string.language != 0) ? type_description(t)->structuredDes.string.language : "");
    }
  else if (type_basic_type(t) == enumeration_Type)
    {
      printmToBuffer (data, "(enumeration non-extensible (supertype)");
      list_enumerate (type_description(t)->structuredDes.enumeration, (void (*)(refany, refany)) recurseEnumFields, data);
      print0ToBuffer (data, ")");
    }
  else if (type_basic_type(t) == record_Type)
    {
      printmToBuffer (data, "(record %s(supertype",
		      type_description(t)->structuredDes.record.extensible ? "extensible " : "");
      if (type_description(t)->structuredDes.record.supertype != NULL) {
	print0ToBuffer (data, " ");
	ast_recurse(type_description(t)->structuredDes.record.supertype, data);
      };
      print0ToBuffer (data, ")");
      list_enumerate (type_description(t)->structuredDes.record.fields, (void (*)(refany, refany)) recurseRecordFields, data);
      print0ToBuffer (data, ")");
    }
  else if (type_basic_type(t) == union_Type)
    {
      print0ToBuffer (data, "(union ");
      ast_recurse (type_description(t)->structuredDes.uniond.discriminator_type, data);
      printmToBuffer (data, " %s ", type_description(t)->structuredDes.uniond.others_allowed ? "openended" : "closed");
      if (type_description(t)->structuredDes.uniond.default_arm == NIL)
	print0ToBuffer (data, "nodefault");
      else
	recurseUnionFields (type_description(t)->structuredDes.uniond.default_arm, data);
      list_enumerate (type_description(t)->structuredDes.uniond.types, (void (*)(refany, refany)) recurseUnionFields, data);
      print0ToBuffer (data, ")");
    }
  else if (type_basic_type(t) == object_Type)
    {
      Class c = type_description(t)->structuredDes.object;

      if (c->corba_rep_id != NULL)
	printmToBuffer (data, "(sealed-object \"%s\")", c->corba_rep_id);
      else
	{
	  printmToBuffer (data, "(object (attributes) (metatype \"%s\") %s %s %s %s %s %s %s notclosure (supertypes",
			  figure_metatype(c),
			  name_base_name(t->name),
			  (c->brand != NULL) ? c->brand : "ilu_no_brand",
			  name_base_name(t->interface->name),
			  (t->interface->brand != NULL) ? t->interface->brand : "ilu_no_interface_brand",
			  c->singleton ? c->singleton : "notsingleton",
			  c->optional ? "optional" : "notoptional",
			  c->collectible ? "collectible" : "notcollectible");
	  if (list_size(type_description(t)->structuredDes.object->superclasses) > 0)
	    {
	      print0ToBuffer (data, " ");
	      list_enumerate (type_description(t)->structuredDes.object->superclasses, (void (*)(refany, refany)) ast_recurse, data);
	    }
	  print0ToBuffer (data, ") (methods");
	  if (list_size(type_description(t)->structuredDes.object->methods) > 0)
	    print0ToBuffer (data, " ");
	  list_enumerate (type_description(t)->structuredDes.object->methods, (void (*)(refany, refany)) recurseObjectMethods, data);
	  print0ToBuffer (data, "))");
	}
    }
  else if (type_basic_type(t) == sequence_Type)
    {
      print0ToBuffer (data, "(sequence ");

      ast_recurse (type_description(t)->structuredDes.sequence.type, data);
      if (type_description(t)->structuredDes.sequence.limit > 0)
	printmToBuffer (data, " %u)", type_description(t)->structuredDes.sequence.limit);
      else
	print0ToBuffer (data, " nolimit)");
    }
  else if (type_basic_type(t) == array_Type)
    {
      print0ToBuffer (data, "(array ");
      ast_recurse (type_description(t)->structuredDes.array.type, data);
      list_enumerate (type_description(t)->structuredDes.array.dimensions,
		      (void (*)(refany, refany)) recurseArrayDimensions, data);
      print0ToBuffer (data, ")");
    }
#ifndef ILU_REFERENCE_TYPES_ONLY
  else if (type_basic_type(t) == optional_Type)
    {
      printmToBuffer (data, "(optional ");
      ast_recurse (type_description(t)->structuredDes.optional, data);
      print0ToBuffer (data, ")");
    }
#endif
  else if (type_basic_type(t) == reference_Type)
    {
      printmToBuffer (data, "(reference%s%s ",
		      type_description(t)->structuredDes.reference.aliased ? " aliased" : "",
		      type_description(t)->structuredDes.reference.aliased ? " optional" : "");      
      ast_recurse (type_description(t)->structuredDes.reference.base_type, data);
      print0ToBuffer (data, ")");
    }
  else
    {
      fprintf (stderr, "Unknown type kind %d for type %s\n",
	       type_basic_type(t), full_type_name(t));
      exit(1);
    }

  ast_level -= 1;
}

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


  if (t->uid != NULL)
    return (t->uid);

  if (type_kind(t) == alias_Type || t->importInterfaceName != NULL)
    return (t->uid = ilu_strdup(FigureTypeUID(ur_type(t))));

  if (type_ur_kind(t) == object_Type &&
      class_object(t) != NULL &&
      class_object(t)->corba_rep_id != NULL)
    {
      t->uid = class_object(t)->corba_rep_id;
      return (t->uid);
    }


  if (verbose && !t->builtIn)
    fprintf(stderr,
	  "figuring uid for <%s> (addr %p, ifc addr %p) from %s\n",
	    full_type_name(t), t, t->interface,
	    ((t->importInterfaceName != NULL)
	     ? t->importInterfaceName
	     : "(current ifc)"));
  buffer.data = (unsigned char *) iluparser_Malloc(buffer.size = 1024);
  buffer.used = 0;
  iluparser_ClearMarks();
  ast_recurse (t, &buffer);
  buffer.data[buffer.used] = '\0';


  if (verbose && ! t->builtIn)
    fprintf (stderr, "  buffer is <%s>\n", buffer.data);


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

  t->uid = (char *) iluparser_Malloc(32);
  strncpy (t->uid, "ilu:", 4);
  /* convert to base 64 */
  convbase(hash,20,t->uid + 4);
  iluparser_Free(buffer.data);

  if (verbose && !t->builtIn)
    fprintf (stderr, "  uid is %s\n", t->uid);

  if (iluparser_CString_Type == NULL &&
      t->importInterfaceName == NULL &&
      strcmp(interface_name(type_interface(t)), "ilu") == 0 &&
      strcmp(type_name(t), "CString") == 0)
    iluparser_CString_Type = t;

  return (t->uid);
}

#endif /* not ILU_TYPEUID_V2 */
