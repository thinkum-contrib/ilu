#ifndef _ILUIDL_H
#define _ILUIDL_H
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
$Id: iluidl.h,v 1.1 1999/07/23 23:16:37 janssen Exp $
*/

#ifdef STANDALONE
typedef int boolean;
#define TRUE 1
#define FALSE 0

typedef enum {In, Out, InOut} ArgDirection;
typedef unsigned long cardinal;
typedef void *refany;
typedef char * string;

typedef struct list_element_s {
  refany data;
  struct list_element_s *next;
} listElement;

typedef struct list_s {
  listElement *head;
  listElement *tail;
  cardinal count;
} *list;

char *aprintf (char *format, ...);
void *iluparser_Malloc(unsigned long size);
void iluparser_Free(void *ptr);
char * ilu_strdup (char *);

typedef void (*iluparser_EnumProc) (refany element, refany rock);
typedef boolean (*iluparser_FindProc) (refany element, refany rock);

list iluparser_new_list(void);
void list_push(list, refany new_element);
void list_insert (list l, refany element);
void list_enumerate (list, iluparser_EnumProc proc, refany rock);
refany list_find (list, iluparser_FindProc proc, refany rock);

string iluparser_FindFileInDir(char *,	/*dir*/
			       char *);	/*filename*/
string iluparser_FindFileInIncludes(string);	/* filename */
#endif /* STANDALONE */

/* IDL subset selection */
#define IDL_STYLE_GUIDE  1
#define IDL_OBV          2

extern int idl_subset;

enum idltype_tag{
  NULLTYPEtag,BASICtag,REFERENCEDtag,
  SEQUENCEtag,STRUCTtag,STRINGtag,ARRAYtag,ENUMtag,UNIONtag,
  DEFINEDtag,ALIAStag,WSTRINGtag,FIXEDtag,NATIVEtag
};
enum idltk{
  idl_void,idl_short,idl_long,idl_float,idl_double,
  idl_unsigned_short,idl_unsigned_long,idl_octet,idl_boolean,
  idl_long_long,idl_unsigned_long_long,idl_wchar,idl_wstring,
  idl_char,idl_any,idl_string,idl_enum,idl_object,idl_valuebase,
  /* untyped values */
  idl_int,idl_fixed,
  /* expressions */
  idl_unary,idl_binary,idl_named
};

typedef struct idl_type{
  enum idltype_tag tag;
  boolean has_scope;
  union{
    /* long f(); */
    enum idltk basic;
    /* foo f(); */
    struct{
      struct idl_name* name;
      struct idl_type *val;
    }referenced;
    /* typedef sequence<long> foo; */
    struct{
      struct idl_type* type;
      struct idl_value* size;
    }sequence;
    /* typedef long foo[42]; */
    struct{
      struct idl_type* type;
      list dimensions;
    }array;
    /* union switch(long){case 3:long i;}; */
    struct{
      struct idl_type* head;
      list body;
      struct idl_name* _default;
      list labels;
    }_union;
    /* typedef fixed<10,2> foo; */
    struct{
      struct idl_value* digits;
      struct idl_value* exponent;
    }fixed;
    /* struct foo{short i;long l;}; */
    list structure;
    /* enum foo{red,blue,green}; */
    list enumerated;
    /* typedef string<10> foo; */
    struct idl_value *stringsize;
    /* typedef struct defined{long i;} alias; */
    struct idl_definition *defined;
    struct idl_type *alias;
  }u;
#ifndef STANDALONE
  refany isl;
#endif
  /* support for anonymous types */
  struct idl_name *name;
  struct idl_definition *anon_def;
} *IDLType;

IDLType new_type();
IDLType new_referenced();

typedef struct idl_name{
  int serial;
  char *file;
  int line;
  struct idl_definition *env;
  struct idl_name *scope;
  char* name;
#ifndef STANDALONE
  char *lifted;
#endif
  struct idl_definition* value;
  list array;
  boolean no_ordering; /* this use may occur before the definition: #pragma */
} *IDLName;

IDLName new_name();

typedef struct idl_value{
  enum idltk tag;
  union{
    int INT;
    unsigned int CHAR;
    boolean BOOL;
    struct{
      double val;
      /* those are used of no difficult computations are performed */
      boolean sign;
      char *integer;
      char *fraction;
      long exponent;
    }FLOAT;
    char* string;
    struct{
      struct idl_type *type;
      struct idl_name *name;
    }enumerated;
    struct{
      boolean negative;
      char *digits;
      int exponent;
    }fixed;
    struct{
      char operator;
      struct idl_value *arg;
    }unary;
    struct{
      char operator;
      struct idl_value *arg1,*arg2;
    }binary;
    struct idl_name *named;
  }u;
  struct idl_value *value;
  refany isl;
} *IDLValue;

IDLValue new_value();
IDLValue new_unary(char,IDLValue);
IDLValue new_binary(IDLValue,char,IDLValue);

enum idldefinition_tag{
  NILtag,TYPEtag,CONSTtag,EXCEPTIONtag,OPERATIONtag,INTERFACEtag,MODULEtag,
  MEMBERtag,PARAMETERtag,CASEtag,ATTRIBUTEtag,INTERFACEFWDtag,
  TYPELISTtag,MEMBERLISTtag,ATTRLISTtag,ENUMVALtag,
  PRAGMA_IDtag,PRAGMA_VERSIONtag,PRAGMA_PREFIXtag,
  PRAGMA_PREFIX_PUSHtag,PRAGMA_PREFIX_POPtag,
  /* OBV */
  VALUEtag, VALUEFWDtag, VALUEBOXtag, STATEtag, STATELISTtag, FACTORYtag
};

typedef struct idl_definition{
  enum idldefinition_tag tag;
  IDLName name;
  union{
    IDLType type;
    struct{
      list definitions;
    }module;
    struct{
      boolean oneway;
      IDLType returntype;
      list parameters;
      refany raises;
      refany context;
    }operation;  /* Also used for value factories */
    struct{
      boolean abstract;
      /* 0 - unresolved, 1 - bases in progress, 2 - bases resolved */
      int resolution_state;
      list bases;
      list definitions;
    }interface;
    struct{
      boolean private;
      IDLType type;
    }state;
    struct{
      boolean private;
      IDLType type;
      list names;
    }statelist;
    struct{
      boolean abstract;
      boolean custom;
      boolean truncatable;
      list bases;
      list supports;
      list definitions;
    }value;
    struct{
      struct idl_type *type;
      struct idl_value *val;
      boolean computed;
    }constant;
    struct{
      ArgDirection direction;
      IDLType type;
    }parameter;
    struct{
      list members;
    }exception;
    struct{
      struct idl_type *type;
      list labels;
    }_case;
    struct{
      boolean readonly;
      struct idl_type *type;
      list names;
      refany set;
    }attribute; /* both attribute and attribute list */
    struct{
      struct idl_type *type;
      list names;
    }typelist;  /* both typelist and member list */
    char *pragma;
    struct idl_type *member;
    struct idl_value *enumval;
  }u;
  struct idl_definition *env;
  char *id;      /* repository ID, either assigned via pragma or computed */
  char *prefix;  /* computed prefix, excluding pragma prefix */
  char *version; /* assigned version, if any */
  refany isl;
} *IDLDefinition;

IDLDefinition new_definition();
void deflist_insert(list,IDLDefinition);
void type_insert(list,IDLType);

typedef struct idl_case{
  IDLValue value;
#ifndef STANDALONE
  Argument isl;
#endif
  IDLDefinition env;
}*IDLCase;

IDLCase new_case();

#define isidlidentchar(x)	((isalnum(x))||((x)=='_'))

/* interface between scanner and parser */
char* idlcurrentfile();
int idlcurrentline();
void idlsetinitialfile(char* file);
int idlerror(char*);

/* support functions */
char* strndup(char* src,int len);
char* iluparser_basename(char* fullname);

/* interface to idl2isl */
IDLDefinition toplevel_module(IDLDefinition d);
IDLDefinition up_find_definition(IDLDefinition d,enum idldefinition_tag t);
list reopen_modules(list alt);
void definition_backlink(refany def,refany env);
void definition_resolvenames(refany def,refany rock);
void definition_check(refany def,refany rock);
void definition_setuid(refany def,refany cur);
void name_warning(IDLName n,char *s);
void idl_name_error(IDLName n,char *s);
void ab_style (refany def, refany rock);

extern IDLType the_char_t, the_wchar_t;
extern list the_result;

#ifndef STANDALONE
extern Type the_CORBA_Object;
char* underscore2hyphen(char *s);
Name new_Name(void);
Type new_Type(void);
TypeDescription new_TypeDescription(void);
#endif

#endif
