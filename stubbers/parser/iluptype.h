
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
$Id: iluptype.h,v 1.97 1999/09/02 06:10:04 janssen Exp $
*/
/* Last edited by Mike Spreitzer January 30, 1997 12:02 pm PST */
/* Chris Jacobi, April 17, 1998 12:53 pm PDT */

#if (defined(WIN32)||defined(WIN16))
#include <iluwin.h>
#elif defined( macintosh )
#include	<ilumac.h>
#else
#include <iluconf.h>
#endif

/* define dllexport to support building DLLs on Win32 */
#if defined(WIN32)
#if defined(ILU_BUILDING_PARSER)
#define ILU_PARSER_PUBLIC __declspec(dllexport) extern
#define ILU_PARSER_PUBLIC_CLASS  class __declspec(dllexport)
#else
#define ILU_PARSER_PUBLIC __declspec(dllimport) extern
#define ILU_PARSER_PUBLIC_CLASS class __declspec(dllimport)
#endif /* defined(ILU_BUILDING_PARSER) */
#else
#define ILU_PARSER_PUBLIC extern
#define ILU_PARSER_PUBLIC_CLASS class
#endif /* defined(WIN32) */


typedef int boolean;
#define TRUE 1
#define FALSE 0

#define ILU_NIL	((void *) 0)
#define NIL ILU_NIL
#define NULLFN 0
#define NULLCH 0

typedef unsigned long cardinal;

typedef char * string;
typedef unsigned char byte;
typedef char character;
typedef void *refany;

typedef struct list_element_s {
  refany data;
  struct list_element_s *next;
} listElement;

typedef struct list_s {
  listElement *head;
  listElement *tail;
  cardinal count;
} *list;

typedef list set;

typedef struct usagename_s {
  char *lang;
  char *name;
} * usagename;

typedef struct name_s {
  char *base_name;
  set	langnames;	/* set of usagename pairs */
} * Name;

typedef enum {In, Out, InOut} ArgDirection;

#define ISO_8859_1_CHARSET		4

/* RFC 2044 */
#define ISO_UNICODE_UTF8_CHARSET	106

/* RFC 1641 */
#define ISO_UNICODE_1_1_CHARSET		1010

/***********************************************************************
  Types needed by ILU parser

***********************************************************************/

typedef struct ilu_interface_s	*	Interface;
typedef struct ilu_type_s	*	Type;
typedef struct ilu_proc_s	*	Procedure;
typedef struct ilu_argument_s	*	Argument;
typedef struct ilu_exception_s	*	Exception;
typedef struct ilu_typedes_s	*	TypeDescription;
typedef struct ilu_imported_s	*	Imported;
typedef int				ProtocolId;
typedef struct enumerationField_s *	EnumField;
typedef cardinal			LineNumber;
typedef struct ilu_constant_s	*	Constant;
typedef struct ilu_constantvalue_s *	ConstantValue;
typedef struct ilu_class_s	*	Class;
typedef struct ilu_integerLiteral_s *	IntegerLiteral;

enum PrimitiveTypes {
  invalid_Type, void_Type,
  byte_Type, boolean_Type, fixedpoint_Type,
  character_Type, shortcharacter_Type, string_Type,
  shortinteger_Type, integer_Type, longinteger_Type,
  shortcardinal_Type, cardinal_Type, longcardinal_Type,
  real_Type, shortreal_Type, longreal_Type,
  object_Type, pipe_Type, alias_Type, pickle_Type,
  union_Type, sequence_Type, record_Type, array_Type, enumeration_Type,
#ifndef ILU_REFERENCE_TYPES_ONLY
  optional_Type,
#endif
  reference_Type};

typedef enum PrimitiveTypes TypeKind;

struct ilu_integerLiteral_s {
  boolean		small;
  boolean		negative;
  union {
    unsigned long	direct;
    char *		string;
  } val;
};

struct ilu_imported_s {
  string		name;
  string		filename;
};

struct enumerationField_s {
  string		name;
  int			id;
};

struct ilu_interface_s {
  Name			name;
  string		brand;
  set			idirectives;	/* set of lists of strings */
  set			types;		/* set of Type, including object types */
  set			classes;	/* set of Type, only the object types */
  set			exceptions;	/* set of Exception */
  set			imports;	/* set of Imported interfaces */
  set			constants;	/* set of Constants */
  LineNumber		def;		/* line on which the interface definition starts */
  string		filename;	/* file in which interface is defined (if any) */
   void                *module_structure; /* used by genstub*/
};

struct ilu_type_s {		/* A type-reference (in refman
				 * terms) AKA type_stamp (in
				 * ilu.bison terms). */
  Name            name;
  list		  scoping;	/* when derived from OMG IDL, a type may have
				   more than INTERFACE level of nesting.
				   This list of strings contains scope names,
				   in top down order, for this type. */
  TypeDescription description;
  /* When a type_stamp is defined in its file, this is the def. */
  Type            supertype;
  /*
   * When a type_stamp is an alias, or an external reference, this
   * is what it refers to; otherwise NULL.
   */
  set             refs;		/* set of LineNumber */
  LineNumber      def;
  boolean         builtIn;
  string          importInterfaceName;	/* The first component of a
					 * type_stamp that's an
					 * external ref. */
  Interface       interface;	/* in which this type_stamp occurs */
  TypeDescription cached_des;	/* cache for type_basic_type() */
  boolean         marked;	/* mark for recursive type search */
  string          uid;
  boolean	  explicit_uid;	/* true if uid was explicitly specified */
  string	  brand;	/* factored into type UID if non-NULL */
};

struct ilu_proc_s {
  Name			name;
  list			arguments;	/* list of Argument */
  boolean		returnOptional;	/* may be NULL */
  Type			returnType;
  set			exceptions;	/* set of Exception */
  LineNumber		def;
  ProtocolId		id;
  Type			object;		/* for use with methods */
  Interface		interface;
  boolean		functional;
  boolean		asynch;
  boolean		marked;		/* for use with recursive sweep */
  char *		authentication_type;
  char *		doc_string;
};

struct ilu_argument_s {
  Name			name;
  Type			type;
  ArgDirection		direction;
  boolean		sibling;
  list			values;		/* used only for Unions; a list of ConstantValue */
  LineNumber		def;
};

struct ilu_exception_s {
  Name			name;
  list			scoping;	/* when derived from OMG IDL, an exception may have
					   more than INTERFACE level of nesting.
					   This list of strings contains scope names,
					   in top down order, for this exception. */
  Type			type;
  boolean		valueOptional;	/* T if value may be NULL */
  LineNumber		def;
  set			refs;		/* set of LineNumber */
  boolean		builtIn;
  ProtocolId		id;
  string		importInterfaceName;	/* if imported, name of interface */
  Exception		import;			/* imported exception */
  Interface		interface;
  boolean		marked;
  string		corba_rep_id;	/* CORBA RepositoryID, if any */
  char *		doc_string;
};

struct ilu_constant_s {
  Name			name;
  list			scoping;	/* when derived from OMG IDL, a constant may have
					   more than INTERFACE level of nesting.
					   This list of strings contains scope names,
					   in top down order, for this constant. */
  LineNumber		def;
  Interface		interface;
  Type			type;
  string		importInterfaceName;
  Constant		import;
  ConstantValue		value;
};

struct ilu_constantvalue_s {

  /* Constant values are represented in this struct in the following
     manner (where "ctk" is the ur_type_kind of the "type" in the
     ilu_constant_s struct pointing to this one, and "vtk" is the
     "type" field of this struct):

     integer constant:

     (ctk == byte_Type, {short,long}{integer,cardinal}_Type &&
      vtk == integer_Type) && "i" arm valid

     real constant:

     (ctk == {short,long}real_Type && vtk == real_Type) &&
     "i.sign" field is valid && "r" arm valid ("r.fraction" may be NULL)
     OR
     (ctk == {short,long}real_Type && vtk == integer_Type) &&
     "i" arm valid

     boolean constant:

     (ctk == boolean_Type && vtk == boolean_Type) && "b" arm valid

     ilu.CString constant:

     (ctk == sequence_Type && vtk == shortcharacter_Type) &&
     "s" arm valid

     enum constant:

     (ctk == enum_Type && vtk == shortcharacter_Type) &&
     "s" arm valid
  */

  TypeKind type;
  union {
    boolean b;
    struct {
      int sign;	/* -1 for negative, +1 for positive */
      unsigned long value;
    } i;
    struct {
      int sign;	/* -1 for negative, +1 for positive */
      string value;
      string fraction;
      long exponent;
    } r;
    string s;
  } val;
};

struct ilu_class_s {
  list		superclasses;
  string	brand;
  string	singleton;
  boolean	collectible;
  boolean	optional;
  string	authentication;
  string	corba_rep_id;	/* CORBA RepositoryID, if any */
  list		methods;	/* list of Procedure */
  string	doc_string;
  list		state;		/* list of Argument -- "direction" unused,
				   "sibling" TRUE if attr is private */
  boolean	sealed;
  boolean	local;
};

typedef struct {
  Type            discriminator_type;
  list            types;	/* list of Argument */
  Argument        default_arm;
  boolean         others_allowed;	/* other discriminant
					 * values allowed? */
}               UnionDescription;

enum FixedPointRangeSizes {
  ilu_fprs_large = 0,
  ilu_fprs_byte = 1,
  ilu_fprs_shortcardinal = 2,
  ilu_fprs_cardinal = 3,
  ilu_fprs_longcardinal = 4,
  ilu_fprs_shortinteger = 5,
  ilu_fprs_integer = 6,
  ilu_fprs_longinteger = 7
  };

struct ilu_typedes_s {
  TypeKind type;
  union
    {
      struct {
	IntegerLiteral min_numerator;	/* NULL to signal no min */
	IntegerLiteral max_numerator;	/* NULL to signal no max */
	IntegerLiteral denominator;
	int fixed_digits;
	int fixed_decimal_places;
	enum FixedPointRangeSizes range_size;
      } fixed;

      struct {
	unsigned long max_length;
	unsigned charset;
	string language;
      } string;

      struct {
	Type type;
	boolean optional;
	cardinal limit;
      } sequence;

      struct {
	Type type;
	boolean sink_p;
	boolean optional;
      } pipe;

      struct {
	Type type;
	list dimensions;	/* list of dimensions */
	boolean optional;
      } array;

      struct {
	list fields;		/* list of Argument */
	boolean extensible;
	Type supertype;
      } record;

      list enumeration;		/* list of EnumField */

      UnionDescription  uniond;

      Class object;

#ifndef ILU_REFERENCE_TYPES_ONLY
      Type optional;		/* non-optional type of the optional type */
#endif

      struct {
	boolean optional;
	boolean aliased;
	Type base_type;
      } reference;

    } structuredDes;
};

/* communication with the IDL parser */
struct idl_parse{
  list definitions;
  list defined_interfaces;
  list imported_interfaces;
  char *file;
};

/* various exported functionality */
ILU_PARSER_PUBLIC list ParseFile (string filename);	/* list of Interface found in file */
ILU_PARSER_PUBLIC Interface GetInterface (string interfacename, string filename);
ILU_PARSER_PUBLIC char *iluparser_CanonicalPathname (char *file);
ILU_PARSER_PUBLIC char *iluparser_GetProgramName (char *short_name); /* result is NULL or full pathname */
ILU_PARSER_PUBLIC void iluparser_GenerateBoilerplate (FILE *file, Interface parse, char *programName, char *prefixes[2]);
ILU_PARSER_PUBLIC char *iluparser_BoilerPlateString (Interface parse, char *programName, char *prefixes[2]); /* caller owns malloc'ed result */
ILU_PARSER_PUBLIC void iluparser_MultipleInterfaceBoilerplate (FILE *file, list interfaces, char *programName, char *prefixes[2]);
ILU_PARSER_PUBLIC char *iluparser_GetILUVersionString(void);
ILU_PARSER_PUBLIC void iluparser_RegisterInterfaceDirectories(list /* of directory names */);
ILU_PARSER_PUBLIC boolean iluparser_IsKeyword(string potential_keyword);
ILU_PARSER_PUBLIC void iluparser_ClearMarks(void);	/* clear mark bits on all known types */
ILU_PARSER_PUBLIC string iluparser_FindFileInDir(char *,	/*dir*/
						 char *);	/*filename*/
/* return canonical name of file if found in dir, NULL otherwise */
ILU_PARSER_PUBLIC string iluparser_FindFileInIncludes(string);	/* filename */
/* return canonical name of file that appears in a #include preprocessor directive */

ILU_PARSER_PUBLIC void iluparser_SetIdlIncludePath(string path);
/*
 * ParseFile will invoke idl2isl when given an IDL file.  (path) is
 * the (space separated) series of "-I..." arguments to include in
 * these invocations.
 */

ILU_PARSER_PUBLIC string replaceBackslashes(string);
/* Replace backslashes with forward slashes.
 * Allocates new string if some replacement happend, returns the 
 * input string if no replacement happend. 
 */

/* memory management */
ILU_PARSER_PUBLIC void *iluparser_Malloc(unsigned long size);
ILU_PARSER_PUBLIC void iluparser_Free(void *ptr);
ILU_PARSER_PUBLIC void *iluparser_Realloc (void *ptr, unsigned long size);
ILU_PARSER_PUBLIC char * ilu_strdup (char *);
ILU_PARSER_PUBLIC int ilu_strcasecmp (char *s1, char *s2);	/* -n if s1 < s2, 0 if s1 == s2, n if s1 > s2 */

/* list primitives */

typedef void (*iluparser_EnumProc) (refany element, refany rock);
/* do something with element and rock */

typedef boolean (*iluparser_FindProc) (refany element, refany rock);
/* return TRUE if element matches the find criteria */

typedef boolean (*iluparser_CompareProc) (refany element1, refany element2);
/* return TRUE if element1 and element2 should be swapped */

ILU_PARSER_PUBLIC cardinal list_size (list l);
ILU_PARSER_PUBLIC refany list_ref (list l, cardinal index);
ILU_PARSER_PUBLIC boolean list_ref_set (list l, cardinal index, refany value);
ILU_PARSER_PUBLIC list list_cdr (list l);
ILU_PARSER_PUBLIC refany list_car (list l);
ILU_PARSER_PUBLIC void list_insert (list, refany new_element);
ILU_PARSER_PUBLIC boolean list_remove (list, refany element);
ILU_PARSER_PUBLIC void list_clear (list, boolean freeElements);
ILU_PARSER_PUBLIC void list_enumerate (list, iluparser_EnumProc proc, refany rock);
ILU_PARSER_PUBLIC refany list_find (list, iluparser_FindProc proc, refany rock);
ILU_PARSER_PUBLIC list iluparser_new_list(void);
ILU_PARSER_PUBLIC void list_push(list, refany new_element);
#define new_list iluparser_new_list
ILU_PARSER_PUBLIC void list_sort (list, iluparser_CompareProc);

/* type primitives */
ILU_PARSER_PUBLIC TypeDescription type_description(Type);
ILU_PARSER_PUBLIC Type     ur_type(Type);
ILU_PARSER_PUBLIC Type     under_type(Type);
ILU_PARSER_PUBLIC TypeKind type_kind(Type);
ILU_PARSER_PUBLIC TypeKind type_basic_type(Type);	/* = type_kind */
ILU_PARSER_PUBLIC TypeKind type_ur_kind(Type);	/* type_kind(ur_type(..)) */
ILU_PARSER_PUBLIC string   type_name(Type);
ILU_PARSER_PUBLIC Interface type_interface(Type);
#define type_uid(t)  ((t)->uid)
ILU_PARSER_PUBLIC void type_recurse(Type,
			 void (*proc) (Type, refany rock),
			 refany rock);
ILU_PARSER_PUBLIC Class    class_object(Type t);
ILU_PARSER_PUBLIC Type	exception_type(Exception e);
ILU_PARSER_PUBLIC Type	iluparser_CString_Type;

/* function that combines sprintf with malloc
   -- returns the allocated buffer containing the results
   -- returns NIL if an error occurred
*/
char *aprintf (char *format, ...);

/* name primitives */
ILU_PARSER_PUBLIC void name_set_base_name (Name n, string name);
ILU_PARSER_PUBLIC void name_set_lang_name (Name n, string lang, string name);
ILU_PARSER_PUBLIC string name_lang_name (Name n, string lang);
ILU_PARSER_PUBLIC string name_base_name (Name n);

/* accessors for various kinds of names */
ILU_PARSER_PUBLIC string exception_name(Exception e);
#define exception_uid(e)	((e)->corba_rep_id)
ILU_PARSER_PUBLIC string interface_name(Interface i);
ILU_PARSER_PUBLIC string argument_name (Argument a);
#define argument_type(a)  ((a)->type)
ILU_PARSER_PUBLIC string procedure_name (Procedure);
ILU_PARSER_PUBLIC string constant_name (Constant);
#define constant_type(a)  ((a)->type)
