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
$Id: idlparser.y,v 1.1 1999/07/23 23:19:04 janssen Exp $
*/

/* This code was contributed by Martin von Löwis */

/*
  Theory of operation:
  The stubbers invoke ParseFile, which in turn calls ParseIDLFile.
  This function first calls the parser, which generates an abstract 
  syntax tree. The flex scanner already resolves all preprocessor macros
  except #pragmas, which are passed as tokens to the parser (FIXME: this
  is not the best way of dealing with pragmas).
  The abstract syntax tree is identical to the concrete syntax, except:
  - nested type definitions as in
    struct foo{
      struct bar{long i;} j;
    };
    are inserted as a definition, i.e. bar is on one level as j.
  - enumeration values are inserted as definitions on the same level
    as the enumeration.
  - for unions, a complex switch type is inserted as a definition into
    the union
  After the parsing is finished, several passes process the tree:
  1. backlink: all names get a pointer to the definition where they are
     used. all definitions get a pointer to the environment scope
  2. resolvenames: all definition references get a pointer to the 
     corresponding definition. Errors are produced for unresolvable names.
  3. check: various consistency checks are performed (ambiguous definitions,
     type references that don't reference types, cycle checks). Also, constant
     values are computed and checked whether they are within range.
  4. setuid: all entities are assigned repository IDs.
  From here on, all transformations are specific to ILU, and happen in
  idl2isl.c
  5. make_toplevel: non-module definitions are inserted into pseudo-modules.
  6. lift: types in nested scopes are lifted to the toplevel module, prefixing
     the name with the relative scoped.
  7. makeisl: all entities get corresponding ISL definitions. While this
     is in progress, ISL definitions of referenced entities might not be there.
  8. update: all cross-scope references are updated so that the ISL points
     to the ISL of the referenced entity.
  9. select_import: the INTERFACEs are sorted into either imported or defined
     ones
  Pending issues:
  - the parser is not really re-entrant. Basically, this is because the
    lexer is not re-entrant.
*/
%{
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#ifndef STANDALONE
#include <iluptype.h>
#endif
#include "iluidl.h"

#if (defined(WIN32) || defined(WIN16))
#include <malloc.h>	/* needed because of include of alloca */
#endif

/* tunable parameters. For 2.0a10 compatibility, all should be 0 */
#define LOCAL_TYPES_HAVE_REP_IDS  1

/* Make the Bison parser produce better error messages */
#define YYERROR_VERBOSE
#include "idlparser-output.c"

  /* missing prototypes */
int yylex(void);
#ifndef STANDALONE
void FigureTypeUID(Type t);
#endif

#define ADD_PRIMITIVE_TYPES  \
  ADD_PRIMITIVE_TYPE (void)\
  ADD_PRIMITIVE_TYPE (short)\
  ADD_PRIMITIVE_TYPE (long)\
  ADD_PRIMITIVE_TYPE (long_long)\
  ADD_PRIMITIVE_TYPE (float)\
  ADD_PRIMITIVE_TYPE (double)\
  ADD_PRIMITIVE_TYPE (unsigned_short)\
  ADD_PRIMITIVE_TYPE (unsigned_long)\
  ADD_PRIMITIVE_TYPE (unsigned_long_long)\
  ADD_PRIMITIVE_TYPE (octet)\
  ADD_PRIMITIVE_TYPE (boolean)\
  ADD_PRIMITIVE_TYPE (char)\
  ADD_PRIMITIVE_TYPE (wchar)\
  ADD_PRIMITIVE_TYPE (object)\
  ADD_PRIMITIVE_TYPE (valuebase)\
  ADD_PRIMITIVE_TYPE (any)

#define ADD_PRIMITIVE_TYPE(x)  IDLType the_##x##_t;
ADD_PRIMITIVE_TYPES
#undef ADD_PRIMITIVE_TYPE

#ifndef STANDALONE
Type the_CORBA_Object;
static Interface the_ilu_module;
static Type the_ilu_CString;
#endif
static IDLType the_string_t;
static IDLType the_fixed_t;
static int next_serial=0;

list the_result=0;
int idl_subset = IDL_OBV;

%}
%union{
  refany any; /*unspecified*/
  IDLDefinition definition;
  list definition_list;
  IDLName name;
  list name_list;
  IDLValue value;
  IDLType type;
  boolean flag;
  list string_list;
  list caselabel_list;
  ArgDirection inout;
  char opname;
  IDLCase caselabel;
}
%type <definition_list> definition_list specification interface_body
%type <definition_list> export_list member_list opt_member_list
%type <definition_list> parameter_dcls param_dcl_list switch_body
%type <definition_list> value_element_list opt_init_param_dcl_list
%type <definition_list> init_param_dcl_list
%type <definition> const_dcl except_dcl module export attr_dcl op_dcl
%type <definition> interface interface_dcl interface_header forward_dcl
%type <definition> struct_type param_dcl pragma constr_type_spec union_type 
%type <definition> enum_type element_spec case type_dcl definition
%type <definition> type_declarator member enumerator
%type <definition> value value_dcl value_abs_dcl value_box_dcl state_member
%type <definition> value_forward_dcl opt_value_inheritance_spec value_header
%type <definition> opt_value_inheritance_base value_element init_dcl
%type <definition> init_param_dcl
%type <name>  scoped_name simple_declarator complex_declarator 
%type <name>  array_declarator declarator
%type <name_list>    opt_raises_expr scoped_name_list enumerator_list
%type <name_list>    declarators
%type <value> const_expr or_expr xor_expr and_expr literal unary_expr
%type <value> shift_expr add_expr mult_expr primary_expr boolean_literal
%type <value> positive_const_int fixed_array_size string_literal

%type <type>  type_spec simple_type_spec base_type_spec 
%type <type>  string_type sequence_type 
%type <type>  switch_type_spec
%type <type>  template_type_spec floating_pt_type integer_type char_type
%type <type>  boolean_type any_type octet_type const_type signed_int
%type <type>  unsigned_int signed_long_int signed_short_int
%type <type>  unsigned_long_int unsigned_short_int op_type_spec 
%type <type>  param_type_spec object_type value_base_type
%type <type>  signed_long_long_int unsigned_long_long_int fixed_pt_const_type
%type <type>  fixed_pt_type wide_string_type wide_char_type

%type <flag>             opt_op_attr opt_readonly opt_abstract opt_truncatable
%type <flag>             private_public
%type <inout>            param_attribute
%type <string_list>      opt_context_expr string_literal_list
%type <opname>           unary_operator
%type <caselabel_list>   case_label_list
%type <caselabel>        case_label

%token <name>   IDENT
%token <value>  BOOL_FALSE BOOL_TRUE CHAR_L FLOAT_L INTEGER_L STRING_L
%token <value>  FIXED_PT_L
%token <flag>   ONEWAY
%token <type>   VOID_T
%token <inout>  IN OUT INOUT
%token <definition>    PRAGMA_ID PRAGMA_VERSION PRAGMA_PREFIX
%token ANY_T ATTRIBUTE BOOLEAN_T CASE CHAR_T
%token CONST CONTEXT DEFAULT DOUBLE_T ENUM EXCEPTION FIXED FLOAT_T
%token INTERFACE LONG_T LSHIFT MODULE OBJECT_T NATIVE OCTET_T RAISES
%token READONLY RSHIFT SCOPE SEQUENCE SHORT_T STRING_L STRING_T STRUCT SWITCH
%token TYPEDEF UNION UNSIGNED_T VOID_T WCHAR_T WSTRING_T
/* OBV */
%token ABSTRACT CUSTOM FACTORY PRIVATE PUBLIC SUPPORTS TRUNCATABLE VALUEBASE VALUETYPE
%%

specification:	definition_list {the_result=$1;}

definition:	type_dcl ';'
	|	const_dcl ';'	
	|	except_dcl ';'
	|	interface ';'
	|	module ';'
	|	value ';'
	|	pragma
	;

module:	MODULE IDENT '{' definition_list '}'
	{	$$=new_definition();$$->tag=MODULEtag;
		$$->name=$2;
		$$->u.module.definitions=$4;
	}

definition_list: definition	
        {       $$=iluparser_new_list();deflist_insert($$,$1);}
	|	definition_list definition
	{	deflist_insert($1,$2);
		$$=$1;
	}
	;

interface:	interface_dcl
	| 	forward_dcl

interface_dcl:	interface_header '{' interface_body '}'
	{	$1->u.interface.definitions=$3;
		$$=$1;
	}

forward_dcl:	opt_abstract INTERFACE IDENT
	{	$$=new_definition();$$->tag=INTERFACEFWDtag;
		$$->name=$3;
		$$->u.interface.abstract = $1;
	}

opt_abstract:	           { $$ = FALSE; }
	|	ABSTRACT   { $$ = TRUE; }

interface_header:	opt_abstract INTERFACE IDENT
	{	$$=new_definition();$$->tag=INTERFACEtag;
		$$->name=$3;
		$$->u.interface.abstract = $1;
		$$->u.interface.bases=NULL;
		$$->u.interface.definitions=NULL;
		$$->u.interface.resolution_state=0;
	}
	|	opt_abstract INTERFACE IDENT ':' scoped_name_list
	{	$$=new_definition();$$->tag=INTERFACEtag;
		$$->name=$3;
		$$->u.interface.abstract = $1;
		$$->u.interface.bases=$5;
		$$->u.interface.definitions=NULL;
		$$->u.interface.resolution_state=0;
	}
	;

interface_body:	/* empty */         {$$=iluparser_new_list();}
	|	export_list

export:	type_dcl ';'
	|	const_dcl ';'
	|	except_dcl ';'
	|	attr_dcl ';'
	|	op_dcl ';'

export_list:	export 
	{ 
		$$=iluparser_new_list();
		deflist_insert($$,$1);
	}
	|	export_list export
	{	$$=$1;deflist_insert($$,$2);
	}

scoped_name:	IDENT
	|	SCOPE IDENT		{$$=$2;$2->scope=new_name();}
	|	scoped_name SCOPE IDENT {$$=$3;$$->scope=$1;}
	;

scoped_name_list:	scoped_name         
	{	$$=iluparser_new_list();
		list_insert($$,$1);
	}
	| scoped_name_list ',' scoped_name  
	{	$$=$1;list_insert($$,$3);
	}
	;

value:		value_dcl
	|	value_abs_dcl
	|	value_box_dcl
	|	value_forward_dcl

value_forward_dcl:	VALUETYPE IDENT
	{	$$ = new_definition();
		$$->tag = VALUEFWDtag;
		$$->name = $2;
		$$->u.value.abstract = FALSE;
		$$->u.value.truncatable = FALSE;
	}
	|	ABSTRACT VALUETYPE IDENT
	{	$$ = new_definition();
		$$->tag = VALUEFWDtag;
		$$->name = $3;
		$$->u.value.abstract = TRUE;
		$$->u.value.truncatable = FALSE;
	}
	

value_box_dcl:	VALUETYPE IDENT type_spec
	{	$$ = new_definition();
		$$->tag = VALUEBOXtag;
		$$->name = $2;
		$$->u.type = $3;
		/* value boxes have their point-of-definition
		   at the end - arrange this by changing the serial */
		$$->name->serial = ++next_serial;
		
	}

value_abs_dcl:	ABSTRACT VALUETYPE IDENT opt_value_inheritance_spec
	'{' interface_body '}'
	{	$$ = $4;
		$$->name = $3;
		$$->u.value.abstract = TRUE;
		$$->u.value.custom = FALSE;
		$$->u.value.definitions = $6;
	}

value_dcl:	value_header '{' value_element_list '}'
	{	$$ = $1;
		$$->u.value.definitions = $3;
	}

value_header:	VALUETYPE IDENT opt_value_inheritance_spec
	{	$$ = $3;
		$$->name = $2;
		$$->u.value.abstract = $$->u.value.custom = FALSE;
	}
	|	CUSTOM VALUETYPE IDENT opt_value_inheritance_spec
	{	$$ = $4;
		$$->name = $3;
		$$->u.value.abstract = FALSE;
		$$->u.value.custom = TRUE;
	}

opt_value_inheritance_spec:	opt_value_inheritance_base
	{	$$ = $1;
		$$->u.value.supports = 0;
	}
	|	opt_value_inheritance_base SUPPORTS scoped_name_list
	{	$$ = $1;
		$$->u.value.supports = $3;
	}

opt_value_inheritance_base:	/*empty*/
	{	$$ = new_definition();
		$$->tag = VALUEtag;
		$$->u.value.truncatable = FALSE;
		$$->u.value.bases = 0;
	}	
	| ':' opt_truncatable scoped_name_list
	{	$$ = new_definition();
		$$->tag = VALUEtag;
		$$->u.value.truncatable = $2;
		$$->u.value.bases = $3;
	}	


opt_truncatable:	/*empty*/
	{	$$ = FALSE;}
	|	TRUNCATABLE
	{	$$ = TRUE;}

value_element_list:	/* empty */
	{	$$ = iluparser_new_list ();}
	|	value_element_list value_element
	{	deflist_insert($1,$2);
		$$ = $1;
	}

value_element:	export
	|	state_member
	|	init_dcl

state_member:	private_public type_spec declarators ';'
	{	$$ = new_definition();
		$$->tag = STATELISTtag;
		$$->u.statelist.private = $1;
		$$->u.statelist.type = $2;
		$$->u.statelist.names = $3;
	}

private_public:	PRIVATE		{ $$ = TRUE; }
	|	PUBLIC		{ $$ = FALSE;}

init_dcl:	FACTORY IDENT '(' opt_init_param_dcl_list ')' ';'
	{	$$ = new_definition();
		$$->tag = FACTORYtag;
		$$->name = $2;
		$$->u.operation.returntype = 0;
		$$->u.operation.parameters = $4;
		$$->u.operation.raises = 0;
		$$->u.operation.context = 0;
	}

opt_init_param_dcl_list:	/*empty*/
	{	$$ = iluparser_new_list();}
	|	init_param_dcl_list

init_param_dcl_list:	init_param_dcl
	{	$$ = iluparser_new_list();
		list_insert($$, $1);
	}
	|	init_param_dcl_list ',' init_param_dcl
	{	list_insert($1, $3);
		$$ = $1;
	}

init_param_dcl:	IN param_type_spec simple_declarator
	{	$$ = new_definition();
		$$->tag = PARAMETERtag;
		$$->u.parameter.direction = In;
		$$->u.parameter.type = $2;
		$$->name = $3;
	}


const_dcl:	CONST const_type IDENT '=' const_expr
	{	$$=new_definition();$$->tag=CONSTtag;
		$$->name=$3;
		$$->u.constant.type=$2;
		$$->u.constant.val=$5;
		$$->u.constant.computed=FALSE;
	}

const_type:	integer_type
	|	char_type
	|	wide_char_type
	|	boolean_type
	|	octet_type
	|	floating_pt_type
	|	string_type
	|	wide_string_type
	|	fixed_pt_const_type
	|	scoped_name
	{
		$$=new_referenced($1);
	}

const_expr:	or_expr
or_expr:	xor_expr
	|	or_expr '|' xor_expr
	{	$$=new_binary($1,'|',$3);}		
	;
xor_expr:	and_expr
	|	xor_expr '^' and_expr
	{	$$=new_binary($1,'^',$3);}		
	;
and_expr:	shift_expr
	|	and_expr '&' shift_expr
	{	$$=new_binary($1,'&',$3);}		
shift_expr:	add_expr
	|	shift_expr LSHIFT add_expr
	{	$$=new_binary($1,'L',$3);}		
	|	shift_expr RSHIFT add_expr
	{	$$=new_binary($1,'R',$3);}		
add_expr:	mult_expr
	|	add_expr '+' mult_expr
	{	$$=new_binary($1,'+',$3);}		
	|	add_expr '-' mult_expr
	{	$$=new_binary($1,'-',$3);}		
mult_expr:	unary_expr
	|	mult_expr '*' unary_expr
	{	$$=new_binary($1,'*',$3);}		
	|	mult_expr '/' unary_expr
	{	$$=new_binary($1,'/',$3);}
	|	mult_expr '%' unary_expr
	{	$$=new_binary($1,'%',$3);}
unary_expr:	unary_operator primary_expr {$$=new_unary($1,$2);}
	|	primary_expr
unary_operator:	'+' 	{$$='+';}
	| '-' 		{$$='-';}
	| '~'		{$$='~';}
primary_expr:	scoped_name	
	{	$$=new_value();$$->tag=idl_named;
		$$->u.named=$1;
	}
	|	literal
	|	'(' const_expr ')' {$$=$2;}
literal:	INTEGER_L
	|	string_literal
	|	CHAR_L
	|	FIXED_PT_L
	|	FLOAT_L
	|	boolean_literal

boolean_literal:	BOOL_TRUE | BOOL_FALSE
positive_const_int:	const_expr  /* check for int, positive is done later */

string_literal: STRING_L
	|	string_literal STRING_L
	{
	  char *s=iluparser_Malloc(strlen($1->u.string)
				   +strlen($2->u.string)+1);
	  strcpy(s,$1->u.string);strcat(s,$2->u.string);
	  $$ = $1;
	  $$->u.string = s;
	  if ($1->tag != $2->tag){
	    fprintf (stderr, "Concatenating narrow and wide string\n");
	    YYERROR;
	  }
	}

type_dcl:	TYPEDEF type_declarator	{$$=$2;}
	|	struct_type		
	|	union_type
	|	enum_type
	|	NATIVE simple_declarator
	{	$$=new_definition();$$->tag=TYPEtag;
		$$->name=$2;
	  	$$->u.type=new_type();$$->u.type->tag=NATIVEtag;
	}

type_declarator:	type_spec declarators
	{	$$=new_definition();$$->tag=TYPELISTtag;
		$$->u.typelist.type=$1;
		$$->u.typelist.names=$2;
	}

type_spec:	simple_type_spec
	|	constr_type_spec       
	{	$$=new_type();$$->tag=DEFINEDtag;
		$$->u.defined=$1;
		$$->name=$1->name;
	}

simple_type_spec:	base_type_spec
	|	template_type_spec
	|	scoped_name            
	{
		$$=new_referenced($1);
	}

base_type_spec:	floating_pt_type
	|	integer_type
	|	char_type
	|	wide_char_type
	|	boolean_type
	|	octet_type
	|	object_type
	|	any_type
	|	value_base_type

template_type_spec:	sequence_type
	|	string_type
	|	wide_string_type
	|	fixed_pt_type

constr_type_spec:	struct_type
	|	union_type
	|	enum_type

declarators:	declarator                 
	{	$$=iluparser_new_list();
		list_insert($$,$1);
	}
	|	declarators ',' declarator
	{	$$=$1;list_insert($$,$3);}

declarator:	simple_declarator
	|	complex_declarator

simple_declarator:	IDENT
complex_declarator:	array_declarator

floating_pt_type:	FLOAT_T  {$$=the_float_t;}
	| 		DOUBLE_T {$$=the_double_t;}

/* long long types as adopted in 96-05-04 */
integer_type:	signed_int
	| 	unsigned_int

signed_int:	signed_long_int
	|	signed_short_int
	|	signed_long_long_int
signed_long_long_int:   LONG_T LONG_T   {$$=the_long_long_t;}
signed_long_int:	LONG_T		{$$=the_long_t;}
signed_short_int:	SHORT_T		{$$=the_short_t;}

unsigned_int:	unsigned_long_int
	|	unsigned_short_int
	|	unsigned_long_long_int
unsigned_long_long_int: UNSIGNED_T LONG_T LONG_T {$$=the_unsigned_long_long_t;}
unsigned_long_int:	UNSIGNED_T LONG_T	 {$$=the_unsigned_long_t;}
unsigned_short_int:	UNSIGNED_T SHORT_T	 {$$=the_unsigned_short_t;}

char_type:	 CHAR_T		{$$=the_char_t;}
wide_char_type:	 WCHAR_T		{$$=the_wchar_t;}
boolean_type:	 BOOLEAN_T	{$$=the_boolean_t;}
octet_type:	 OCTET_T		{$$=the_octet_t;}
any_type:	 ANY_T		{$$=the_any_t;}
object_type:	 OBJECT_T	{$$=the_object_t;}
value_base_type: VALUEBASE	{$$=the_valuebase_t;}

struct_type:	STRUCT IDENT '{' member_list '}'
	{	$$=new_definition();$$->tag=TYPEtag;
		$$->name=$2;
		$$->u.type=new_type();
		$$->u.type->tag=STRUCTtag;
		$$->u.type->has_scope=TRUE;
		$$->u.type->u.structure=$4;
		$$->u.type->name=$2;
	}
member_list:	member			
	{	$$=iluparser_new_list();deflist_insert($$,$1);}
	|	member_list member
	{	deflist_insert($1,$2);$$=$1;}
member:	type_spec declarators ';'
	{	$$=new_definition();$$->tag=MEMBERLISTtag;
		$$->u.typelist.type=$1;
		$$->u.typelist.names=$2;
	}

union_type:	UNION IDENT SWITCH '(' switch_type_spec ')' '{' switch_body '}'
	{	$$=new_definition();$$->tag=TYPEtag;
		$$->name=$2;
		$$->u.type=new_type();$$->u.type->tag=UNIONtag;
		$$->u.type->has_scope=TRUE;
		$$->u.type->u._union.head=$5;
		/* make identifiers in switch_type_spec visible */
		type_insert($8,$5);
		$$->u.type->u._union.body=$8;
		$$->u.type->u._union._default=0;
	}

switch_type_spec:	integer_type
	|	char_type
	|	wide_char_type
	|	boolean_type
	|	octet_type
	|	enum_type
	{	$$=new_type();$$->tag=DEFINEDtag;$$->u.defined=$1;}
	|	scoped_name
	{	$$=new_referenced($1);
	}

switch_body:	case
	{	$$=iluparser_new_list();deflist_insert($$,$1);}
	|	switch_body case
	{	$$=$1;deflist_insert($$,$2);}

case:	case_label_list element_spec ';'
	{	$$=$2;$$->u._case.labels=$1;}

case_label_list:	case_label
	{	$$=iluparser_new_list();list_insert($$,$1);}
	|	case_label_list case_label
	{	$$=$1;list_insert($$,$2);}

case_label:	CASE const_expr ':' 
	{	$$=new_case();$$->value=$2;}
	|	DEFAULT ':'
	{	$$=new_case();}

element_spec:	type_spec declarator
	{	$$=new_definition();$$->tag=CASEtag;
		$$->name=$2;
		$$->u._case.type=$1;
	}

enum_type:	ENUM IDENT '{' enumerator_list '}'
	{	$$=new_definition();$$->tag=TYPEtag;
		$$->name=$2;
		$$->u.type=new_type();
		$$->u.type->tag=ENUMtag;
		$$->u.type->u.enumerated=$4;
	}
enumerator_list: enumerator
	{	$$=iluparser_new_list();list_insert($$,$1);}
	|	enumerator_list ',' enumerator
	{	$$=$1;list_insert($$,$3);}
enumerator:	IDENT
	{	$$=new_definition();$$->tag=ENUMVALtag;
		$$->name=$1;
		$$->u.enumval=new_value();
		$$->u.enumval->tag=idl_enum;
		$$->u.enumval->u.enumerated.type=0;
		$$->u.enumval->u.enumerated.name=$1;
	}

sequence_type:	SEQUENCE '<' simple_type_spec ',' positive_const_int '>'
	{	$$=new_type();$$->tag=SEQUENCEtag;
		$$->u.sequence.size=$5;
		$$->u.sequence.type=$3;
	}
	|	SEQUENCE '<' simple_type_spec '>'
	{	$$=new_type();$$->tag=SEQUENCEtag;
		$$->u.sequence.size=NULL;
		$$->u.sequence.type=$3;
	}

string_type:	STRING_T '<' positive_const_int '>'
	{	$$=new_type();$$->tag=STRINGtag;$$->u.stringsize=$3;}
	|	STRING_T               
	{	$$=the_string_t;}

wide_string_type:	WSTRING_T '<' positive_const_int '>'
	{	$$=new_type();$$->tag=WSTRINGtag;$$->u.stringsize=$3;}
	|		WSTRING_T
	{	$$=new_type();$$->tag=WSTRINGtag;$$->u.stringsize=0;}

array_declarator:	IDENT fixed_array_size
	{	$1->array=iluparser_new_list();list_insert($$->array,$2);
		$$=$1;
	}
	|	array_declarator fixed_array_size
	{	$$=$1;list_insert($$->array,$2);}

fixed_array_size:	'[' positive_const_int ']'	{$$=$2;}

attr_dcl:	opt_readonly ATTRIBUTE param_type_spec simple_declarator
	{	$$=new_definition();$$->tag=ATTRIBUTEtag;
		$$->name=$4;
		$$->u.attribute.readonly=$1;
		$$->u.attribute.type=$3;
		$$->u.attribute.names=0;
	}
	|	attr_dcl ',' simple_declarator
	{	if($1->tag==ATTRIBUTEtag){
			$1->tag=ATTRLISTtag;
			$1->u.attribute.names=iluparser_new_list();
			list_insert($1->u.attribute.names,$1->name);
			$1->name=0;
		}
		$$=$1;list_insert($1->u.attribute.names,$3);
	}

opt_readonly:	/*empty*/	{$$=FALSE;}
	|	READONLY	{$$=TRUE;}

except_dcl:	EXCEPTION IDENT '{' opt_member_list '}' 
	{	$$=new_definition();$$->tag=EXCEPTIONtag;
		$$->name=$2;
		$$->u.exception.members=$4;
	}
opt_member_list:	/*empty*/ {$$=0;}
	|	member_list

op_dcl: opt_op_attr op_type_spec IDENT parameter_dcls 
	opt_raises_expr opt_context_expr
	{	$$=new_definition();$$->tag=OPERATIONtag;
		$$->u.operation.oneway=$1;
		$$->u.operation.returntype=$2;
		$$->name=$3;
		$$->u.operation.parameters=$4;
		$$->u.operation.raises=$5;
		$$->u.operation.context=$6;
	}

opt_op_attr:	/*empty*/	{$$=FALSE;}
	|	ONEWAY		{$$=TRUE;}

op_type_spec:	param_type_spec
	|	VOID_T		{$$=the_void_t;}

parameter_dcls:	'(' param_dcl_list ')'	{ $$=$2;}
	|	'(' ')'			
	{	$$=iluparser_new_list();}

param_dcl_list: param_dcl		
	{ 	$$=iluparser_new_list();list_insert($$,$1);}
	|	param_dcl_list ',' param_dcl
		{ list_insert($1,$3);$$=$1;}

param_dcl:	param_attribute param_type_spec simple_declarator
	{	$$=new_definition();$$->tag=PARAMETERtag;
		$$->u.parameter.direction=$1;
		$$->u.parameter.type=$2;
		$$->name=$3;
	}

param_attribute:	IN	{$$=In;}
	|		OUT 	{$$=Out;}
	|		INOUT	{$$=InOut;}

opt_raises_expr:	/*empty*/		{$$=0;}
	|	RAISES '(' scoped_name_list ')'	{$$=$3;}

opt_context_expr:	/*empty*/			{$$=0;}
	|	CONTEXT '(' string_literal_list ')'	{$$=$3;}

string_literal_list:	string_literal
	{	$$=iluparser_new_list();list_insert($$,$1);}
	| 	string_literal_list ',' string_literal
	{	$$=$1;list_insert($$,$3);}

param_type_spec:	base_type_spec
	|	string_type
	|	wide_string_type
	|	fixed_pt_type
	|	scoped_name
	{
		$$=new_referenced($1);
	}

fixed_pt_type:	FIXED '<' positive_const_int ',' positive_const_int '>'
	{	$$=new_type();$$->tag=FIXEDtag;
		$$->u.fixed.digits=$3;
		$$->u.fixed.exponent=$5;
	}

fixed_pt_const_type:	FIXED	{$$=the_fixed_t;}

pragma:		PRAGMA_VERSION
	|	PRAGMA_ID
	|	PRAGMA_PREFIX

%%

/* Constructors */
IDLDefinition new_definition()
{
  IDLDefinition result;
  result=(IDLDefinition)iluparser_Malloc(sizeof(struct idl_definition));
  result->tag=NILtag;
  result->name=0;
  result->isl=0;
  result->env=0;
  result->prefix=0;
  result->id=0;
  result->version=0;
  return result;
}

IDLType new_type()
{
  IDLType result;
  result=(IDLType)iluparser_Malloc(sizeof(struct idl_type));
  result->tag=NULLTYPEtag;
  result->has_scope=FALSE;
#ifndef STANDALONE
  result->isl=NULL;
#endif
  result->name=NULL;
  result->anon_def=NULL;
  return result;
}

IDLName new_name()
{
  IDLName result=(IDLName)iluparser_Malloc(sizeof(struct idl_name));
  next_serial++;
  result->serial=next_serial;
  result->file=NULL;
  result->line=0;
  result->env=NULL;
  result->scope=NULL;
  result->name=NULL;
#ifndef STANDALONE
  result->lifted=NULL;
#endif
  result->value=NULL;
  result->array=NULL;
  result->no_ordering=0;
  return result;
}

IDLValue new_value()
{
  IDLValue result;
  result=(IDLValue)iluparser_Malloc(sizeof(struct idl_value));
  result->tag=idl_void;
  result->value=NULL;
  result->isl=NULL;
  return result;
}

IDLCase new_case()
{
  IDLCase result;
  result=(IDLCase)iluparser_Malloc(sizeof(struct idl_case));
  result->value=0;
#ifndef STANDALONE
  result->isl=0;
#endif
  return result;
}

/*********************** complex constructors ***************************/

IDLType new_referenced(IDLName n)
{		
  IDLType result=new_type();
  result->tag=REFERENCEDtag;
  result->u.referenced.name=n;
  result->u.referenced.val=0;
  return result;
}

IDLValue new_unary(char opname,IDLValue val)
{
  IDLValue result=new_value();
  result->tag=idl_unary;
  result->u.unary.operator=opname;
  result->u.unary.arg=val;
  return result;
}

IDLValue new_binary(IDLValue val1,char opname,IDLValue val2)
{
  IDLValue result=new_value();
  result->tag=idl_binary;
  result->u.binary.operator=opname;
  result->u.binary.arg1=val1;
  result->u.binary.arg2=val2;
  return result;
}

static boolean
is_anonymous(IDLType t)
{
  switch(t->tag){
  case FIXEDtag:
    /* this is not supported, yet */
  case WSTRINGtag:
    /* if there was an ilu.WString type, this would not be anonymous */
  case ARRAYtag:
  case SEQUENCEtag:
    return t->name==NULL;
  case STRINGtag:
    /* can use ilu.CString */
    if(t->u.stringsize)return t->name==NULL;
    return t->name==NULL;
  default:
    break;
  }
  return FALSE;
}

static void
declare_type(refany name,refany rock)
{
  refany *r=rock;
  IDLType t,t1;
  IDLDefinition d=new_definition();
  d->tag=TYPEtag;
  d->name=name;
  t=r[0];
  if(d->name->array){
    t1=new_type();
    t1->tag=ARRAYtag;
    t1->name=name;
    t1->u.array.type=t;
    t1->u.array.dimensions=d->name->array;
    d->name->array=0;
    t=t1;
  }else{
    /* this is a possible alias definition */
    if(is_anonymous(t))
      t->name=name;
    else{
      t1=new_type();
      t1->tag=ALIAStag;
      t1->name=name;
      t1->u.alias=t;
      t=t1;
    }
  }
  d->u.type=t;
  list_insert(r[1],d);
}
	
static void
declare_member(refany name,refany rock)
{
  refany *r=rock;
  IDLDefinition d=new_definition();
  d->tag=MEMBERtag;
  d->name=name;
  d->u.member=r[0];
  list_insert(r[1],d);
}

static void
declare_state(refany name,refany rock)
{
  refany *r = rock;
  IDLDefinition d = new_definition();
  IDLDefinition orig = r[0];
  d->tag = STATEtag;
  d->name = name;
  d->u.state.type = orig->u.statelist.type;
  d->u.state.private = orig->u.statelist.private;
  list_insert(r[1],d);
}

static void
declare_attribute(refany name,refany rock)
{
  refany *r=rock;
  IDLDefinition a=r[0];
  IDLDefinition d=new_definition();
  d->tag=ATTRIBUTEtag;
  d->name=name;
  d->u.attribute.type=a->u.attribute.type;
  d->u.attribute.readonly=a->u.attribute.readonly;
  d->u.attribute.names=0;
  list_insert(r[1],d);
}

void
type_insert(list l,IDLType t)
{
  if(t->tag!=DEFINEDtag)return;
  /* typedef struct foo{long bar;} foobar,baz; */
  deflist_insert(l,t->u.defined);
}

static void
declare_enum(refany e,refany rock)
{
  refany *r=rock;
  IDLDefinition d=e;
  d->u.enumval->u.enumerated.type=r[0];
  list_insert(r[1],e);
}

void deflist_insert(list l,IDLDefinition d)
{
  refany rock[2];
  switch(d->tag){
  case TYPELISTtag:
    type_insert(l,d->u.typelist.type);
    rock[0]=d->u.typelist.type;
    rock[1]=l;
    list_enumerate(d->u.typelist.names,declare_type,rock);
    break;
  case MEMBERLISTtag:
    type_insert(l,d->u.typelist.type);
    rock[0]=d->u.typelist.type;
    rock[1]=l;
    list_enumerate(d->u.typelist.names,declare_member,rock);
    break;
  case STATELISTtag:
    type_insert(l,d->u.statelist.type);
    rock[0]=d;
    rock[1]=l;
    list_enumerate(d->u.statelist.names,declare_state,rock);
    break;
  case ATTRLISTtag:
    type_insert(l,d->u.attribute.type);
    rock[0]=d;
    rock[1]=l;
    list_enumerate(d->u.attribute.names,declare_attribute,rock);
    break;
  case CASEtag:
    type_insert(l,d->u._case.type);
    list_insert(l,d);
    break;
  case TYPEtag:
    if(d->u.type->tag==ENUMtag){
      /* insert enum values as definitions */
      rock[0]=d->u.type;
      rock[1]=l;
      list_enumerate(d->u.type->u.enumerated,declare_enum,rock);
    }
  default:
    list_insert(l,d);
  }
}

#ifndef STANDALONE
Type new_Type(void);
TypeDescription new_TypeDescription(void);
Name new_Name(void);

static void
primitive_setuid(Type T)
{
  if(!T->uid){
    char buf[100];
    FigureTypeUID(T);
    /* FIXME: work around FigureTypeUid setting the mark */
    sprintf(buf,"(%s)",type_name(T));
    name_set_lang_name(T->name,"parser:full",buf);
  }
}

static boolean
find_ilu_type(refany object,refany string)
{
  Type t=object;
  return strcmp(name_base_name(t->name),string)==0;
}

int init_types()
{
	IDLType v;
	Type t;
	TypeDescription d;
#define ADD_PRIMITIVE_TYPE(n)\
	v=the_##n##_t= new_type();\
	v->tag=BASICtag;\
	v->u.basic=idl_##n;

  ADD_PRIMITIVE_TYPES;

  /* need to get ilu before making a call to FigureTypeUID */
  the_ilu_module = GetInterface("ilu",0);
  if (!the_ilu_module)
    return 0;

#undef ADD_PRIMITIVE_TYPE
#define ADD_PRIMITIVE_TYPE(n,str,tk)\
        v=the_##n##_t;\
	v->isl=t=new_Type();\
	t->description=d=new_TypeDescription();\
	d->type=tk;\
	t->builtIn=TRUE;\
	name_set_base_name(t->name,str);\
	primitive_setuid(t);

  ADD_PRIMITIVE_TYPE (short, "shortinteger", shortinteger_Type);
  ADD_PRIMITIVE_TYPE (long, "integer", integer_Type);
  ADD_PRIMITIVE_TYPE (long_long, "longinteger", longinteger_Type);
  ADD_PRIMITIVE_TYPE (unsigned_short, "shortcardinal", shortcardinal_Type);
  ADD_PRIMITIVE_TYPE (unsigned_long, "cardinal", cardinal_Type);
  ADD_PRIMITIVE_TYPE (unsigned_long_long, "longcardinal", longcardinal_Type);
  ADD_PRIMITIVE_TYPE (double, "real", real_Type);
  ADD_PRIMITIVE_TYPE (float, "shortreal", shortreal_Type);
  ADD_PRIMITIVE_TYPE (octet, "byte", byte_Type);
  ADD_PRIMITIVE_TYPE (boolean, "boolean", boolean_Type);
  ADD_PRIMITIVE_TYPE (char, "shortcharacter", shortcharacter_Type);
  ADD_PRIMITIVE_TYPE (wchar, "character", character_Type);
  ADD_PRIMITIVE_TYPE (any, "pickle", pickle_Type);

  /* not supported by IDL
  ADD_PRIMITIVE_TYPE ( "longreal", longreal_Type);
  */

  /* look for ilu.CORBA-Object */
  the_CORBA_Object = list_find(the_ilu_module->classes,
			       find_ilu_type,"CORBA-Object");
  the_ilu_CString = list_find(the_ilu_module->types,find_ilu_type,"CString");
  /* import this type */
  t=new_Type();
  t->name=new_Name();
  name_set_base_name(t->name,name_base_name(the_CORBA_Object->name));
  name_set_lang_name(t->name,"import","CORBA-Object");
  t->supertype=the_CORBA_Object;
  t->importInterfaceName = "ilu";
  t->interface=the_CORBA_Object->interface;
  t->uid=the_CORBA_Object->uid;
  t->explicit_uid = TRUE;
  t->brand = NULL;
  the_CORBA_Object=t;
  the_object_t = new_type();
  the_object_t->tag=BASICtag;
  the_object_t->u.basic=idl_object;
  the_object_t->isl=the_CORBA_Object;

  /* Not yet */
  the_valuebase_t = NULL;

  the_string_t=new_type();
  the_string_t->tag=STRINGtag;
  the_string_t->u.stringsize=0;
  the_string_t->isl=t=new_Type();
  /* make this type an imported ilu.CString */
  t->name=new_Name();
  name_set_base_name(t->name,name_base_name(the_ilu_CString->name));
  name_set_lang_name(t->name,"import","CString");
  t->supertype=the_ilu_CString;
  t->importInterfaceName="ilu";
  t->interface=the_ilu_CString->interface;
  t->uid=the_ilu_CString->uid;
  t->explicit_uid=TRUE;
  t->brand = NULL;

  the_fixed_t=new_type();
  the_fixed_t->tag=FIXEDtag;
  the_fixed_t->u.fixed.digits=0;
  the_fixed_t->u.fixed.exponent=0;

  /* success */
  return 1;
}
#else /* STANDALONE */
int init_types()
{
  IDLType v;
#define ADD_PRIMITIVE_TYPE(n)\
	v=the_##n##_t= new_type();\
	v->tag=BASICtag;\
	v->u.basic=idl_##n;

  ADD_PRIMITIVE_TYPES;

  the_object_t = new_type();
  the_object_t->tag=BASICtag;
  the_object_t->u.basic=idl_object;

  the_valuebase_t = new_type();
  the_valuebase_t->tag=BASICtag;
  the_valuebase_t->u.basic=idl_valuebase;

  the_string_t=new_type();
  the_string_t->tag=STRINGtag;
  the_string_t->u.stringsize=0;

  the_fixed_t=new_type();
  the_fixed_t->tag=FIXEDtag;
  the_fixed_t->u.fixed.digits=0;
  the_fixed_t->u.fixed.exponent=0;

  /* success */
  return 1;
}
#endif

int idlerror(char *s)
{
#ifdef YYERROR_VERBOSE
  extern char *idltext;
  fprintf(stderr,"%s:%ld:parse error before '%s'\n%s\n",
	  idlcurrentfile(),(long)idlcurrentline(),idltext,s);
#else
  fprintf(stderr,"%s:%ld:%s\n",idlcurrentfile(),idlcurrentline(),s);
#endif
  return 0;
}

void
name_warning(IDLName n,char *s)
{
  if(n)
    fprintf(stderr,"%s:%ld: %s: %s\n",n->file,(long)n->line,n->name,s);
  else
    fprintf(stderr,"<no line>: %s\n",s);
}

void
idl_name_error(IDLName n,char *s)
{
  name_warning(n,s);
  exit(1);
}

/********* Print functions **************************************/
static void 
name_print(FILE *f,IDLName n)
{
  if(n->scope){
    name_print(f,n->scope);
    fprintf(f,"::");
  }else if(n->name)
    fprintf(f,"%s",n->name);
  else
    fprintf(f,"::");
}

void definition_print(FILE* f,IDLDefinition d)
{
  char *n="unknown definition";
  if(d->env)
    definition_print(f,d->env);
  if(d->name && d->name->name)
    n=d->name->name;
  fprintf(f,"::%s",n);
}

/* Returns a list of definitions inside this one. */

static list
get_nested_definitions(IDLDefinition d)
{
  switch (d->tag) {
    /* those should not happen */
  case NILtag:case TYPELISTtag:case MEMBERLISTtag:case ATTRLISTtag:
  case STATELISTtag:
    assert(0);
    break;
  case MODULEtag:
    return d->u.module.definitions;
  case INTERFACEtag:
    return d->u.interface.definitions;
  case VALUEtag:
    return d->u.value.definitions;
  case FACTORYtag:
  case OPERATIONtag:
    return d->u.operation.parameters;
  case EXCEPTIONtag:
    return d->u.exception.members;
  case TYPEtag:
    /* Handled separately. */
    break;
    /* No nested names */
  case CONSTtag:case MEMBERtag:case PARAMETERtag:case CASEtag:
  case ATTRIBUTEtag:case INTERFACEFWDtag:case ENUMVALtag:
  case PRAGMA_IDtag:case PRAGMA_VERSIONtag:case PRAGMA_PREFIXtag:
  case PRAGMA_PREFIX_PUSHtag:case PRAGMA_PREFIX_POPtag:
  case VALUEFWDtag:case VALUEBOXtag:case STATEtag:
    break;
  }
  return 0;
}

/************************* Name lookup ***************************/

IDLDefinition
up_find_definition(IDLDefinition d,enum idldefinition_tag t)
{
  while(d && d->tag!=t)
    d=d->env;
  return d;
}

IDLDefinition
toplevel_module(IDLDefinition d)
{
  if(!d->env)return d;
  return toplevel_module(d->env);
}

static boolean
cmp_name(refany def,refany name)
{
  boolean result;
  IDLDefinition d=def;
  /* pragma prefix does not have a name */
  if(!d->name)return FALSE;
  if(d->name->scope)return FALSE;
  /* catch cases where it doesn't define something */
  switch(d->tag){
  case PRAGMA_PREFIXtag:case PRAGMA_PREFIX_PUSHtag:case PRAGMA_PREFIX_POPtag:
  case PRAGMA_IDtag:
  case PRAGMA_VERSIONtag:
  case INTERFACEFWDtag:
  case VALUEFWDtag:
    return FALSE;
  default:
    break;
  }
  /* IDL is case-insensitive */
  result=ilu_strcasecmp(d->name->name,name)==0;
  /* check for consistent spelling */
  if(result && strcmp(d->name->name,name)!=0)
    idl_name_error(d->name,aprintf("also spelled as '%s'.",name));
  return result;
}

struct lookup_base_s{
  char *name;
  IDLDefinition result;
  IDLDefinition def;
};

static IDLDefinition lookup_name_in_scope(IDLDefinition d,char* n);

static void
lookup_base(refany name,refany rock)
{
  struct lookup_base_s *r=rock;
  IDLName n=name;
  IDLDefinition found=lookup_name_in_scope(n->value,r->name);
  if(r->result && found && r->result!=found){
    idl_name_error(r->def->name,aprintf("ambiguous resolution for %s",r->name));
    return;
  }
  if(found)
    r->result=found;
}

static IDLDefinition
lookup_name_in_scope (IDLDefinition d,char* n)
{
  list locals = get_nested_definitions (d);
  IDLDefinition result = 0;
  if (locals)
    result = list_find (locals, cmp_name, n);
  switch(d->tag){
  case INTERFACEtag:
    if(!result){
      /* search bases */
      struct lookup_base_s s;
      s.name=n;
      s.result=0;
      s.def=d;
      list_enumerate(d->u.interface.bases,lookup_base,&s);
      result=s.result;
    }
    break;
  case VALUEtag:
    if(!result){
      /* search bases */
      struct lookup_base_s s;
      s.name=n;
      s.result=0;
      s.def=d;
      list_enumerate(d->u.value.bases,lookup_base,&s);
      result=s.result;
    }
    break;
    /* These are handled through get_nested.  */
  case MODULEtag:
  case OPERATIONtag:
  case EXCEPTIONtag:
  case FACTORYtag:
    break;
  case TYPEtag:
    switch(d->u.type->tag){
    case STRUCTtag:
      result=list_find(d->u.type->u.structure,cmp_name,n);
      break;
    case UNIONtag:
      result=list_find(d->u.type->u._union.body,cmp_name,n);
      break;
    default:
      break;
    }
    break;
  /* inside the other definitions, there are no more names */
  default:
    break;
  }
  return result;
}
static IDLDefinition
lookup_name (IDLDefinition d,char* n,list toplevel)
{
  IDLDefinition result = lookup_name_in_scope (d, n);
  if(!result) {
    if(!d->env)
      /* not found, need toplevel lookup */
      return list_find(toplevel,cmp_name,n);
    else 
      return lookup_name(d->env,n,toplevel);
  }
  return result;
}

/************************** module reopening pass ******************/

static void list_insert1(refany i,refany l)
{
  list_insert(l,i);
}

static void find_duplicate_modules(refany def,refany l)
{
  IDLDefinition d=def;
  if(d->tag==MODULEtag){
    IDLDefinition first=list_find(l,cmp_name,d->name->name);
    if(first && first->tag==MODULEtag)
      /* insert definitions of def into first */
      list_enumerate(d->u.module.definitions,list_insert1,
		     first->u.module.definitions);
    else
      list_insert(l,d);
  }
  else
    /* add this definition */
    list_insert(l,d);
}

list reopen_modules(list alt);

static void 
module_reopen_modules(refany def,refany rock)
{
  IDLDefinition d=def;
  if(d->tag!=MODULEtag)return;
  d->u.module.definitions=reopen_modules(d->u.module.definitions);
}

list 
reopen_modules(list alt)
{
  list neu=iluparser_new_list();
  list_enumerate(alt,find_duplicate_modules,neu);
  list_enumerate(neu,module_reopen_modules,0);
  /* delete the old list, leaving the items in the new list */
  list_clear(alt,0);
  return neu;
}

/************************** backlink pass **************************/

void definition_backlink(refany def,refany env);

static void
name_backlink(refany name,refany rock)
{
  IDLName n=name;
  while(n){
    n->env=rock;
    n=n->scope;
  }
}

static void
value_backlink(refany val,refany rock)
{
  IDLValue v=val;
  switch(v->tag){
  case idl_named:
    name_backlink(v->u.named,rock);
    break;
  case idl_unary:
    value_backlink(v->u.unary.arg,rock);
    break;
  case idl_binary:
    value_backlink(v->u.binary.arg1,rock);
    value_backlink(v->u.binary.arg2,rock);
    break;
  default:
    /* nothing to do for other value */
    break;
  }
}

static void
type_backlink(refany type,refany rock)
{
  IDLType t=type;
  switch(t->tag){
  case NULLTYPEtag:
  case BASICtag: 
  case NATIVEtag:
    break;
  case REFERENCEDtag:
    name_backlink(t->u.referenced.name,rock);
    break;
  case ALIAStag:
    type_backlink(t->u.alias,rock);
    break;
  case SEQUENCEtag:
    type_backlink(t->u.sequence.type,rock);
    if(t->u.sequence.size)
      value_backlink(t->u.sequence.size,rock);
    break;
  case STRUCTtag:
    list_enumerate(t->u.structure,definition_backlink,rock);
    break;
  case ENUMtag:
    break;
  case UNIONtag:
    type_backlink(t->u._union.head,rock);
    list_enumerate(t->u._union.body,definition_backlink,rock);
    break;
  case ARRAYtag:
    type_backlink(t->u.array.type,rock);
    list_enumerate(t->u.array.dimensions,value_backlink,rock);
    break;
  case WSTRINGtag:
  case STRINGtag:
    if(t->u.stringsize)
      value_backlink(t->u.stringsize,rock);
    break;
  case FIXEDtag:
    if(t->u.fixed.digits){
      value_backlink(t->u.fixed.digits,rock);
      value_backlink(t->u.fixed.exponent,rock);
    }
    break;
  case DEFINEDtag:
    definition_backlink(t->u.defined,rock);
    break;
  }
}

static void
case_backlink(refany Case,refany rock)
{
  IDLCase c=Case;
  IDLDefinition d=rock;
  c->env=d;
  if(c->value)
    value_backlink(c->value,d->env);
}

void
definition_backlink(refany def,refany env)
{
  IDLDefinition d=def;
  d->env=env;
  if(d->name){
#ifndef STANDALONE
    d->name->lifted=underscore2hyphen(d->name->name);
#endif
  }
  list_enumerate (get_nested_definitions (d), definition_backlink, d);
  switch(d->tag){
    /* those should not happen */
  case NILtag:case TYPELISTtag:case MEMBERLISTtag:case ATTRLISTtag:
  case STATELISTtag:
    break;
    /* those don't have nested items */
  case INTERFACEFWDtag:
  case VALUEFWDtag:
    break;
    /* Those are handled through get_nested_definitions.  */
  case FACTORYtag:
  case EXCEPTIONtag:
  case MODULEtag:
    break;
  case INTERFACEtag:
    /* base names are used in the environment of the interface */
    list_enumerate(d->u.interface.bases,name_backlink,d->env);
    break;
  case VALUEtag:
    list_enumerate (d->u.value.bases, name_backlink, d->env);
    list_enumerate (d->u.value.supports, name_backlink, d->env);
    break;
  case CONSTtag:
    type_backlink(d->u.constant.type,d->env);
    value_backlink(d->u.constant.val,d->env);
    break;
  case ENUMVALtag:
    break;
  case OPERATIONtag:
    type_backlink(d->u.operation.returntype,d);
    list_enumerate(d->u.operation.raises,name_backlink,d);
    if(d->u.operation.context)
      name_warning(d->name,"Context not supported");
    break;
  case ATTRIBUTEtag:
    type_backlink(d->u.attribute.type,d);
    break;
  case VALUEBOXtag:
    type_backlink(d->u.type,d);
    break;
  case PARAMETERtag:
    type_backlink(d->u.parameter.type,d);
    break;
  case TYPEtag:
    if(d->u.type->has_scope)
      type_backlink(d->u.type,d);
    else
      type_backlink(d->u.type,d->env);
    break;
  case MEMBERtag:
    /* structure fields don't define a scope */
    type_backlink(d->u.member,d->env);
    list_enumerate (d->name->array, value_backlink, d);
    break;
  case STATEtag:
    type_backlink (d->u.state.type, d->env);
    list_enumerate (d->name->array, value_backlink, d);
    break;
  case CASEtag:
    type_backlink(d->u._case.type,d->env);
    list_enumerate(d->u._case.labels,case_backlink,d);
    list_enumerate (d->name->array, value_backlink, d);
    break;
  case PRAGMA_IDtag:
  case PRAGMA_VERSIONtag:
    name_backlink(d->name,env);
    break;
  case PRAGMA_PREFIXtag:case PRAGMA_PREFIX_PUSHtag:case PRAGMA_PREFIX_POPtag:
    break;
  }
}

/************************* resolvenames pass *************************/
void definition_resolvenames(refany def,refany rock);

static boolean
find_fwd(refany def,refany name)
{
  IDLDefinition d=def;
  IDLName n=name;
  return (d->tag == VALUEFWDtag || d->tag==INTERFACEFWDtag)
    && strcmp(n->name,d->name->name)==0;
}

static void
name_resolvenames(refany name,refany rock)
{
  IDLName n=name;
  if(n->scope){
    /* scoped name */
    if(n->scope->name){
      /* relative scope */
      name_resolvenames(n->scope,rock);
      n->value=lookup_name_in_scope(n->scope->value,n->name);
    }else
      /* absolute scope */
      n->value=list_find(rock,cmp_name,n->name);
  }else if(n->env)
    /* unscoped name used inside a scope */
    n->value=lookup_name(n->env,n->name,rock);
  else
    /* unscoped name used on the toplevel */
    n->value=list_find(rock,cmp_name,n->name);
  if(!n->value)
    idl_name_error(n,"undefined");
  if(n->value->name->serial > n->serial && !n->no_ordering){
    /* this the value is an interface definition,
       a forward declaration would be enough.
       If the interface is used as a base interface,
       the caller will check it */
    if(n->value->tag == INTERFACEtag 
       || n->value->tag == VALUEtag){
      IDLDefinition fwd,mod;
      mod=n->value->env;
      /* the interface is better in a module, if it is in something at all */
      if(mod && mod->tag!=MODULEtag)
	idl_name_error(n,"Internal error, please report");
      if(mod)
	fwd=list_find(mod->u.module.definitions,find_fwd,n->value->name);
      else
	fwd=list_find(rock,find_fwd,n->value->name);
      if(!fwd)
	idl_name_error(n,"missing forward declaration");
      if ((n->value->tag == INTERFACEtag && fwd->tag == VALUEFWDtag)
	  || (n->value->tag == VALUEtag && fwd->tag == INTERFACEFWDtag))
	idl_name_error (n, "illegal forward declaration");
      if(fwd->name->serial > n->serial)
	idl_name_error(n,"used before first forward declaration");
    }else
      /* the name is defined too late */
      idl_name_error(n,"used before definition");
  }
}

static void
value_resolvenames(refany val,refany rock)
{
  IDLValue v=val;
  switch(v->tag){
  case idl_named:
    name_resolvenames(v->u.named,rock);
    break;
  case idl_unary:
    value_resolvenames(v->u.unary.arg,rock);
    break;
  case idl_binary:
    value_resolvenames(v->u.binary.arg1,rock);
    value_resolvenames(v->u.binary.arg2,rock);
    break;
  default:
    break;
  }
}

static void
type_resolvenames(refany type,refany rock)
{
  IDLType t=type;
  switch(t->tag){
  case NULLTYPEtag:
  case BASICtag:
  case NATIVEtag:
    break;
  case REFERENCEDtag:
    name_resolvenames(t->u.referenced.name,rock);
    break;
  case ALIAStag:
    type_resolvenames(t->u.alias,rock);
    break;
  case SEQUENCEtag:
    type_resolvenames(t->u.sequence.type,rock);
    if(t->u.sequence.size)
      value_resolvenames(t->u.sequence.size,rock);
    break;
  case STRUCTtag:
    list_enumerate(t->u.structure,definition_resolvenames,rock);
    break;
  case ENUMtag:
    break;
  case WSTRINGtag:
  case STRINGtag:
    if(t->u.stringsize)
      value_resolvenames(t->u.stringsize,rock);
    break;
  case FIXEDtag:
    if(t->u.fixed.digits){
      value_resolvenames(t->u.fixed.digits,rock);
      value_resolvenames(t->u.fixed.exponent,rock);
    }
    break;
  case UNIONtag:
    type_resolvenames(t->u._union.head,rock);
    list_enumerate(t->u._union.body,definition_resolvenames,rock);
    break;
  case ARRAYtag:
    type_resolvenames(t->u.array.type,rock);
    list_enumerate(t->u.array.dimensions,value_resolvenames,rock);
    break;
  case DEFINEDtag:
    definition_resolvenames(t->u.defined,rock);
    break;
  }
}

static void
case_resolvenames(refany Case,refany rock)
{
  IDLCase c=Case;
  if(c->value)
    value_resolvenames(c->value,rock);
}

static void
interface_resolvenames(refany name,refany rock)
{
  IDLName n=name;
  IDLDefinition d;
  name_resolvenames(n,rock);
  d=n->value;
  if(d->tag!=INTERFACEtag)
    idl_name_error(n,"not an interface");
  definition_resolvenames(d,rock);
}
 
static void
valuetype_resolvenames(refany name,refany rock)
{
  IDLName n=name;
  IDLDefinition d;
  name_resolvenames(n,rock);
  d=n->value;
  if (d->tag != VALUEtag)
    idl_name_error(n,"not an interface");
}
 
static void
pragma_setversionid(IDLDefinition d,char *value,int do_id)
{
  switch(d->tag){
  case TYPEtag:
  case INTERFACEtag:
  case EXCEPTIONtag:
  case CONSTtag:
  case VALUEtag:
  case VALUEBOXtag:
    if(do_id){
      if(d->id)
	idl_name_error(d->name,"Duplicate ID");
      else d->id=value;
    }else{
      if(d->version)
	idl_name_error(d->name,"Duplicate version");
      else d->version=value;
    }
    if(d->id && d->version)
      idl_name_error(d->name,"Both repository ID and version assigned\n");
    break;
  case NILtag:case OPERATIONtag:case MODULEtag:case MEMBERtag:
  case PARAMETERtag:case ATTRIBUTEtag:case FACTORYtag:case STATEtag:
  case TYPELISTtag:case ATTRLISTtag:case MEMBERLISTtag:case STATELISTtag:
  case CASEtag:case INTERFACEFWDtag:case VALUEFWDtag:case ENUMVALtag:
  case PRAGMA_IDtag:case PRAGMA_VERSIONtag:case PRAGMA_PREFIXtag:
  case PRAGMA_PREFIX_PUSHtag:case PRAGMA_PREFIX_POPtag:
    idl_name_error(d->name,do_id ? "Invalid assignment of repository ID":
	       "Invalid assignment of repository version");
  }
}

static void definition_precedes (refany, refany);

void
definition_resolvenames(refany def,refany rock)
{
  IDLDefinition d=def;
  switch(d->tag){
    /* those should not happen */
  case NILtag:case TYPELISTtag:case MEMBERLISTtag:case ATTRLISTtag:
  case STATELISTtag:
    break;
  case MODULEtag:
  case ENUMVALtag:
  case FACTORYtag:
  case EXCEPTIONtag:
    break;
  case INTERFACEtag:
    /* bases first, because local definitions might refer to bases */
    switch(d->u.interface.resolution_state){
    case 0:
      /* normal operation, resolve */
      d->u.interface.resolution_state=1;
      list_enumerate(d->u.interface.bases,interface_resolvenames,rock);
      d->u.interface.resolution_state=2;
      /* local definitions below */
      break;
    case 1:
      /* oops, nested call */
      idl_name_error(d->name,"Cycle in base interface resolution");
      break;
    default:break;
    }
    break;
  case VALUEtag:
    list_enumerate(d->u.value.bases,valuetype_resolvenames,rock);
    list_enumerate(d->u.value.bases,definition_precedes,d->name);
    list_enumerate(d->u.value.supports,name_resolvenames,rock);
    break;
  case VALUEBOXtag:
    type_resolvenames(d->u.type,rock);
    break;
  case CONSTtag:
    type_resolvenames(d->u.constant.type,rock);
    value_resolvenames(d->u.constant.val,rock);
    break;
  case OPERATIONtag:
    type_resolvenames(d->u.operation.returntype,rock);
    /* list_enumerate allows for NULL lists */
    list_enumerate(d->u.operation.raises,name_resolvenames,rock);
    break;
  case ATTRIBUTEtag:
    type_resolvenames(d->u.attribute.type,rock);
    break;
  case STATEtag:
    type_resolvenames(d->u.state.type,rock);
    list_enumerate (d->name->array, value_resolvenames, rock);
    break;
  case MEMBERtag:
    type_resolvenames(d->u.member,rock);
    list_enumerate (d->name->array, value_resolvenames, rock);
    break;
  case CASEtag:
    type_resolvenames(d->u._case.type,rock);
    list_enumerate(d->u._case.labels,case_resolvenames,rock);
    list_enumerate (d->name->array, value_resolvenames, rock);
    break;
  case TYPEtag:
    type_resolvenames(d->u.type,rock);
    break;
  case PARAMETERtag:
    type_resolvenames(d->u.parameter.type,rock);
    break;
  case INTERFACEFWDtag:
    /* The corresponding definition has to be an interface,
       and it must be defined in the same scope */
    if(d->env)
      d->name->value=lookup_name_in_scope(d->env,d->name->name);
    else
      d->name->value=list_find(rock,cmp_name,d->name->name);
    if(!d->name->value)
      idl_name_error(d->name,"no interface for forward declaration");
    else if(d->name->value->tag!=INTERFACEtag)
      idl_name_error(d->name,"corresponding definition is not an interface");
    break;
  case VALUEFWDtag:
    if (d->env)
      d->name->value = lookup_name (d->env, d->name->name, rock);
    else
      d->name->value = list_find (rock, cmp_name, d->name->name);
    if (!d->name->value
	|| d->name->value->env != d->env)
      idl_name_error (d->name, "no valuetype for forward declaration");
    else if (d->name->value->tag != VALUEtag) {
      if (d->name->value->tag == VALUEBOXtag)
	idl_name_error (d->name, "value boxes cannot be forward-declared");
      else
	idl_name_error (d->name, "corresponding definition is not a valuetype");
    } else if (d->name->value->u.value.abstract != d->u.value.abstract)
      idl_name_error (d->name, (d->u.value.abstract ?
			    "corresponding valuetype is not abstract" :
			    "corresponding valuetype is abstract"));
    break;
  case PRAGMA_PREFIXtag:case PRAGMA_PREFIX_PUSHtag:case PRAGMA_PREFIX_POPtag:
    break;
  case PRAGMA_VERSIONtag:
    name_resolvenames(d->name,rock);
    pragma_setversionid(d->name->value,d->u.pragma,FALSE);
    break;
  case PRAGMA_IDtag:
    name_resolvenames(d->name,rock);
    pragma_setversionid(d->name->value,d->u.pragma,TRUE);
    break;
  }
  list_enumerate (get_nested_definitions (d), definition_resolvenames, rock);
}

/************************* check pass        *************************/

static boolean 
identity(refany v1,refany v2)
{
  return v1==v2;
}

static IDLValue value_compute(IDLValue v,list start,IDLName n,IDLType target);
void definition_check(refany def,refany rock);
static IDLType type_compute(IDLType t, IDLValue opt);
static void type_check(IDLType t,IDLName n);

static boolean
isint(enum idltk t)
{
  return t==idl_int || t==idl_octet
    || t==idl_short || t==idl_long || t==idl_long_long 
    || t==idl_unsigned_short || t==idl_unsigned_long || 
    t==idl_unsigned_long_long 
    ;
}

static boolean
isfloat(enum idltk t)
{
  return t==idl_float || t==idl_double;
}

static IDLValue
value_coerce(IDLValue v,IDLType t,IDLName n)
{
  IDLValue v1;
  if(t->tag==BASICtag && v->tag==t->u.basic)
    return v;
  if(t->tag==ENUMtag){
    if(v->tag!=idl_enum)
      idl_name_error(n,"value is not enumerated"); 
    if(type_compute(v->u.enumerated.type, 0)!=t)
      idl_name_error(n,"value is of wrong enumeration");
    return v;
  }
  if(t->tag==STRINGtag){
    if(v->tag!=idl_string)
      idl_name_error(n,"value is not a string");
    return v;
  }
  if(t->tag==WSTRINGtag){
    if(v->tag!=idl_wstring)
      idl_name_error(n,"value is not a wide string");
    return v;
  }
  if(t->tag==FIXEDtag){
    if(v->tag!=idl_fixed)
      idl_name_error(n,"no fixed-point value");
    return v;
  }  
  if(t->tag!=BASICtag)
    idl_name_error(n,"Conversion into complex type not possible");
  if(isint(t->u.basic)){
    boolean ok;
    int I=v->u.INT;
    if(!isint(v->tag))
      idl_name_error(n,"Value is not an integral type");
    switch(t->u.basic){
    case idl_octet:            ok= (      0 <= I) && (I<=0xFF);  break;
    case idl_short:            ok= (-0x8000 <= I) && (I<=0x7FFF);break;
    case idl_unsigned_short:   ok= (      0 <= I) && (I<=0xFFFF);break;
    case idl_long_long: /* FIXME: cannot check long long */
    case idl_long:             ok=(!(I & 0x80000000)); break;
    case idl_unsigned_long_long: /* FIXME: cannot check long long */
    case idl_unsigned_long:    ok=1; break;
    default:
      idl_name_error(0,"Unknown basic type");
    }
    if(!ok)
      idl_name_error(n,"Value out of range");
    v1=new_value();
    *v1=*v;
    v1->tag=t->u.basic;
    return v1;
  }
  if(isfloat(t->u.basic)){
    if(!isfloat(v->tag))
      idl_name_error(n,"Value is not floating point");
    /* FIXME: range check */
    v1=new_value();
    *v1=*v;
    v1->tag=t->u.basic;
    return v1;
  }
  if(t->u.basic==idl_boolean){
    if(v->tag!=idl_boolean)
      idl_name_error(n,"Value is not boolean");
    return v;
  }
  if(t->u.basic==idl_char || t->u.basic==idl_wchar){
    if(v->tag!=idl_char)
      idl_name_error(n,"Value is not a character");
    return v;
  }
  if(t->u.basic==idl_any)
    idl_name_error(n,"Constants of type any are not allowed");
  idl_name_error(n,"Unsupported type for constants");
  return 0;
}

static IDLValue
definition_compute(IDLDefinition d,list start,IDLName n)
{
  if(d->tag==ENUMVALtag)
    return d->u.enumval;
  if(d->u.constant.computed)
    return d->u.constant.val;
  if(list_find(start,identity,d))
    idl_name_error(d->name,"cycle in computation of value");
  if(d->tag!=CONSTtag)
    idl_name_error(n,aprintf("%s is not a constant",d->name->name));
  list_insert(start,d);
  d->u.constant.val=value_compute(d->u.constant.val,start,
				  d->name,type_compute(d->u.constant.type, 0));
  /* in case this was a fixed-point value, need to compute type again */
  d->u.constant.type=type_compute(d->u.constant.type, d->u.constant.val);
  d->u.constant.computed=TRUE;
  return d->u.constant.val;
}

#define MAX_FIXED_PREC 32
static void fixed_sub(IDLValue,IDLValue,IDLValue);

static void 
fixed_zero(char* x)
{
  int i;
  for(i=0;i<MAX_FIXED_PREC;i++)
    x[i]=0;
}

static void
fixed_fill(char* to, char* from)
{
  while (*from)
    *to++ = *from++ -'0';
}

static void
fixed_normalize(IDLValue val)
{
  int i = strlen (val->u.fixed.digits);
  while (val->u.fixed.digits[--i]=='0' && i>0)
    {
      val->u.fixed.digits[i] = '\0';
      val->u.fixed.exponent++;
    }
}  

static void
fixed_add(IDLValue res,IDLValue arg1,IDLValue arg2)
{
  char v1[MAX_FIXED_PREC],v2[MAX_FIXED_PREC];
  int prec1,prec2;
  int msd1, msd2;
  int i,k,ov;
  if (arg1->u.fixed.negative)
    {
      arg1->u.fixed.negative = FALSE;
      fixed_sub(res,arg2,arg1);
      arg1->u.fixed.negative = TRUE;
      return;
    }
  if (arg2->u.fixed.negative)
    {
      arg2->u.fixed.negative = FALSE;
      fixed_sub(res,arg1,arg2);
      arg1->u.fixed.negative = TRUE;
      return;
    }
  fixed_zero(v1);
  fixed_zero(v2);
  prec1 = strlen(arg1->u.fixed.digits);
  prec2 = strlen(arg2->u.fixed.digits);
  msd1 = prec1+arg1->u.fixed.exponent;
  msd2 = prec2+arg2->u.fixed.exponent;
  if (msd1<msd2)
    {
      fixed_fill(v1+msd2-msd1+1, arg1->u.fixed.digits);
      fixed_fill(v2+1,arg2->u.fixed.digits);
      prec1 += msd2-msd1;
    }
  else
    {
      fixed_fill(v2+msd1-msd2+1, arg2->u.fixed.digits);
      fixed_fill(v1+1,arg1->u.fixed.digits);
      prec2 += msd1-msd2;
    }
  for (i=MAX_FIXED_PREC-1, ov=0;i>=0;i--)
    {
      v1[i]+=v2[i]+ov;
      ov = v1[i]/10;
      v1[i] %= 10;
    }
  if (prec1<prec2)
    prec1 = prec2;
  prec1++;
  if (msd1<msd2)
    msd1 = msd2;
  msd1++;
  for (i=0; v1[i]==0; i++)
    msd1--;
  res->u.fixed.digits = iluparser_Malloc (prec1-i+1);
  for (k=0; i<prec1; i++, k++)
    res->u.fixed.digits[k] = '0'+v1[i];
  res->u.fixed.digits[k] = '\0';
  res->u.fixed.exponent = msd1-k;
  res->u.fixed.negative = FALSE;
  fixed_normalize (res);
}

static void
fixed_sub(IDLValue res,IDLValue arg1,IDLValue arg2)
{
  char v1[MAX_FIXED_PREC],v2[MAX_FIXED_PREC];
  int prec1,prec2;
  int msd1, msd2;
  int i,k,ov;
  if (arg2->u.fixed.negative)
    {
      arg2->u.fixed.negative = FALSE;
      fixed_add(res,arg1,arg2);
      arg1->u.fixed.negative = TRUE;
      return;
    }
  if (arg1->u.fixed.negative)
    {
      arg1->u.fixed.negative = FALSE;
      fixed_add(res,arg1,arg2);
      arg1->u.fixed.negative = TRUE;
      res->u.fixed.negative = !res->u.fixed.negative;
      return;
    }
  fixed_zero(v1);
  fixed_zero(v2);
  prec1 = strlen(arg1->u.fixed.digits);
  prec2 = strlen(arg2->u.fixed.digits);
  msd1 = prec1+arg1->u.fixed.exponent;
  msd2 = prec2+arg2->u.fixed.exponent;
  if (msd1<msd2)
    {
      fixed_fill(v1+msd2-msd1+1, arg1->u.fixed.digits);
      fixed_fill(v2+1,arg2->u.fixed.digits);
      prec1 += msd2-msd1;
    }
  else
    {
      fixed_fill(v2+msd1-msd2+1, arg2->u.fixed.digits);
      fixed_fill(v1+1,arg1->u.fixed.digits);
      prec2 += msd1-msd2;
    }
  for (i=MAX_FIXED_PREC-1, ov=0;i>=0;i--)
    {
      v1[i]-=v2[i]+ov;
      if (v1[i]<0)
	{
	  v1[i] += 10;
	  ov = 1;
	}
      else
	ov = 0;
    }
  if (ov)
    {
      /* negative, compute complement */
      res->u.fixed.negative = TRUE;
      for (i=MAX_FIXED_PREC-1, ov=0; i>=0; i--)
	{
	  v1[i] = -ov - v1[i];
	  if (v1[i]<0)
	    {
	      v1[i] += 10;
	      ov = 1;
	    }
	  else
	    ov = 0;
	}
    }
  else
    res->u.fixed.negative = FALSE;
  if (prec1<prec2)
    prec1 = prec2;
  prec1++;
  if (msd1<msd2)
    msd1 = msd2;
  msd1++;
  for (i=0; v1[i]==0; i++)
    msd1--;
  iluparser_Free (res->u.fixed.digits);
  res->u.fixed.digits = iluparser_Malloc (prec1-i+1);
  for (k=0; i<prec1; i++, k++)
    res->u.fixed.digits[k] = '0'+v1[i];
  res->u.fixed.digits[k] = '\0';
  res->u.fixed.exponent = msd1-k;
  fixed_normalize (res);
}

static void
fixed_mul(IDLValue res,IDLValue arg1,IDLValue arg2)
{
  idl_name_error(0,"not implemented: multiplying fixed-point values");
}

static void
fixed_div(IDLValue res,IDLValue arg1,IDLValue arg2)
{
  idl_name_error(0,"not implemented: dividing fixed-point values");
}

static IDLValue
value_compute(IDLValue v,list start,IDLName n,IDLType target)
{
  IDLDefinition named;
  IDLValue v1,v2;
  if(v->value)
    return value_coerce(v->value,target,n);
  switch(v->tag){
  case idl_named:
    named=v->u.named->value;
    v->value=definition_compute(named,start,named->name);
    return value_coerce(v->value,target,n);
  case idl_unary:
    v1=value_compute(v->u.unary.arg,start,n,target);
    if(isint(v1->tag)){
      v->tag=v1->tag;
      switch(v->u.unary.operator){
      case '+':v->u.INT = v1->u.INT;break;
      case '-': /* protect against negative unsigned values! */
	if ((target->tag == BASICtag) && 
	    ((target->u.basic == idl_unsigned_long) ||
	     (target->u.basic == idl_unsigned_short) ||
	     (target->u.basic == idl_unsigned_long_long)
	     )) 	
	  idl_name_error(n,"Invalid negative unsigned value");
	v->u.INT =-v1->u.INT;
	break;
      case '~':v->u.INT =~v1->u.INT;break;
      default:
	idl_name_error(n,"Invalid unary integer operator");
      }
    }else if(isfloat(v1->tag)){
      char op=v->u.unary.operator;
      *v=*v1;
      v->value=0;
      switch(op){
      case '+':break;
      case '-':
	v->u.FLOAT.val=-v->u.FLOAT.val;
	v->u.FLOAT.sign=-v->u.FLOAT.sign;
	break;
      default:
	idl_name_error(n,"Invalid unary float operator");
      }
    }else if(v1->tag == idl_fixed){
      char op=v->u.unary.operator;
      *v=*v1;
      v->value=0;
      switch(op){
      case '+':break;
      case '-':
	v->u.fixed.negative = !v->u.fixed.negative;
	break;
      default:
	idl_name_error(n,"Invalud unary fixed-point operator");
      }
    }else      
      idl_name_error(n,"Invalid operand for unary operator");
    return value_coerce(v,target,n);
  case idl_binary:
    v1=value_compute(v->u.binary.arg1,start,n,target);
    v2=value_compute(v->u.binary.arg2,start,n,target);
    if(isint(v1->tag)){
      v->tag=v1->tag;
      switch(v->u.binary.operator){
	/* FIXME: integer overflow must be detected */
      case '+':v->u.INT = v1->u.INT + v2->u.INT;break;
      case '-':v->u.INT = v1->u.INT - v2->u.INT;break;
      case '*':v->u.INT = v1->u.INT * v2->u.INT;break;
      case '/':v->u.INT = v1->u.INT / v2->u.INT;break;
      case '%':v->u.INT = v1->u.INT % v2->u.INT;break;
      case 'L':v->u.INT = v1->u.INT <<v2->u.INT;break;
      case 'R':v->u.INT = v1->u.INT >>v2->u.INT;break;
      case '&':v->u.INT = v1->u.INT & v2->u.INT;break;
      case '^':v->u.INT = v1->u.INT ^ v2->u.INT;break;
      case '|':v->u.INT = v1->u.INT | v2->u.INT;break;
      default:
	idl_name_error(n,"Invalid binary integer operator");
      }
    }else if(isfloat(v1->tag)){
      char op=v->u.binary.operator;
      v->tag=v1->tag;
      /* literal integral parts not supported here */
      v->u.FLOAT.integer=v->u.FLOAT.fraction=0;
      switch(op){
      case '+':v->u.FLOAT.val = v1->u.FLOAT.val + v2->u.FLOAT.val;break;
      case '-':v->u.FLOAT.val = v1->u.FLOAT.val - v2->u.FLOAT.val;break;
      case '*':v->u.FLOAT.val = v1->u.FLOAT.val * v2->u.FLOAT.val;break;
      case '/':v->u.FLOAT.val = v1->u.FLOAT.val / v2->u.FLOAT.val;break;
      default:
	idl_name_error(n,"Invalid floating point binary operator");
      }
    }else if(v1->tag == idl_fixed){
      char op=v->u.binary.operator;
      v->tag=v1->tag;
      switch (op){
      case '+':fixed_add(v,v1,v2);break;
      case '-':fixed_sub(v,v1,v2);break;
      case '*':fixed_mul(v,v1,v2);break;
      case '/':fixed_div(v,v1,v2);break;
      default:
	idl_name_error(n,"Invalid fixed-point binary operator");
      }
    }else
      idl_name_error(n,"Invalid arguments for binary operators");
    return value_coerce(v,target,n);
  default:
    /* primitive values */
    return value_coerce(v,target,n);
  }
  return 0;
}

static IDLType
type_compute(IDLType t, IDLValue val)
{
  switch(t->tag){
    /* those identify themselves */
  case NULLTYPEtag:case BASICtag:case SEQUENCEtag:case STRINGtag:
  case STRUCTtag:case ARRAYtag:case ENUMtag:case UNIONtag:case WSTRINGtag:
  case NATIVEtag:
    return t;
    /* If a value is given, build type based on value */
  case FIXEDtag:
    {
      IDLValue v;
      if (!val)
	return t;
      assert (val->tag==idl_fixed);
      t = new_type();
      t->tag=FIXEDtag;

      /* compute the number of significant digits */
      v=new_value();
      v->tag=idl_int;
      v->u.INT=strlen(val->u.fixed.digits);
      t->u.fixed.digits=v;

      /* compute the decimal exponent */
      v=new_value();
      v->tag=idl_int;
      v->u.INT=val->u.fixed.exponent;
      t->u.fixed.exponent=v;
      return t;
    }
    /* those is one of the above after one level of indirection */
  case DEFINEDtag:
    return type_compute(t->u.defined->u.type, val);
  case ALIAStag:
    return type_compute(t->u.alias, val);
  case REFERENCEDtag:
    if(t->u.referenced.val)
      return type_compute(t->u.referenced.val, val);
    type_check(t,t->u.referenced.name);
    assert(t->u.referenced.val);
    return type_compute(t->u.referenced.val, val);
  }
  return 0;
}

static boolean
definition_isdefinition(IDLDefinition d)
{
  switch(d->tag){
  case PRAGMA_VERSIONtag:case PRAGMA_IDtag:case PRAGMA_PREFIXtag:
  case PRAGMA_PREFIX_PUSHtag:case PRAGMA_PREFIX_POPtag:
  case INTERFACEFWDtag:case VALUEFWDtag:
    return FALSE;
  default:
    return TRUE;
  }
}

static void
duplicate_check(refany def1,refany def)
{
  IDLDefinition d1=def1,d=def;
  /* identical definitions, no problem */
  if(def1==def)return;
  /* one or the other does not really define anything */
  if(!definition_isdefinition(d) || !definition_isdefinition(d1))
    return;
  /* different names, no problem */
  if(!cmp_name(d,d1->name->name))return;
  /* for same names, only complain if def is after def1 */
  if(d->name->line<d1->name->line)return;
  idl_name_error(d->name,aprintf("already defined in line %d\n",d1->name->line));
}

static void
dimensions_check(refany val,refany name)
{
  IDLValue v=val;
  list l;
  *v=*value_compute(val,l=iluparser_new_list(),name,the_long_t);
  if(v->u.INT<=0)
    idl_name_error(name,"dimension is not positive");
  /* free temporary list */
  list_clear(l,FALSE);
  iluparser_Free(l);
}

static IDLType
type_resolve(IDLType t,list l,IDLName n)
{
  IDLDefinition d;
  if (t->tag!=REFERENCEDtag) return t;
  if (t->u.referenced.val) return t->u.referenced.val;
  d=t->u.referenced.name->value;
  if (d->tag == INTERFACEtag)
    /* referenced interface */
    return t;
  if (d->tag == VALUEtag || d->tag == VALUEBOXtag)
    /* referenced value */
    return t;
  if (d->tag != TYPEtag) {
    idl_name_error (n, aprintf ("%s does not specify a type",d->name->name));
    return 0;
  }
  if (list_find (l, identity, d))
    idl_name_error (n, "cycle in type resolution");
  list_insert (l, d);
  return d->u.type = type_resolve (d->u.type, l, n);
}

/* Check whether a struct or union contains itself recursively,
   either as member type, or inside nested structures or unions. */

static void
type_recursion (refany field, refany type)
{
  IDLDefinition f = field;
  IDLType t;
  switch (f->tag){
  case MEMBERtag:
    t = f->u.member;
    break;
  case CASEtag:
    t = f->u._case.type;
    break;
  case TYPEtag:
    switch (f->u.type->tag) {
    case STRUCTtag:
      list_enumerate (f->u.type->u.structure, type_recursion, type);
      break;
    case UNIONtag:
      list_enumerate (f->u.type->u._union.body, type_recursion, type);
      break;
    default:
      break;
    }
    /* If iterating over the fields did not find an error, it is ok */
    return;
  default:
    /* Nothing else can happen in a union or struct */
    return;
  }
  if ((t->tag == REFERENCEDtag)
      && (t->u.referenced.val == type))
    idl_name_error (f->name, "recursive type");
}
    
static void
type_check(IDLType t,IDLName n)
{
  list l=0;
  switch(t->tag){
  case NULLTYPEtag:
    /* this should not happen */
  case BASICtag:case ENUMtag:case NATIVEtag:
    /* nothing to do */
  case DEFINEDtag:
    /* those will be checked separately */
    break;
  case ALIAStag:
    type_check(t->u.alias,n);
    break;
  case REFERENCEDtag:
    if(t->u.referenced.val)break;
    t->u.referenced.val=type_resolve(t,l=iluparser_new_list(),n);
    break;
  case SEQUENCEtag:
    type_check(t->u.sequence.type,n);
    if(t->u.sequence.size){
      t->u.sequence.size=
	value_compute(t->u.sequence.size,l=iluparser_new_list(),n,the_long_t);
      if(t->u.sequence.size->u.INT<=0)
	idl_name_error(n,"sequence size is not positive");
    }
    break;
  case STRUCTtag:
    list_enumerate (t->u.structure, definition_check,0);
    list_enumerate (t->u.structure, type_recursion, t);
    break;
  case WSTRINGtag:
  case STRINGtag:
    if(t->u.stringsize){
      t->u.stringsize=
	value_compute(t->u.stringsize,l=iluparser_new_list(),n,the_long_t);
      if(t->u.stringsize->u.INT<=0)
	idl_name_error(n,"string size is not positive");
    }
    break;
  case ARRAYtag:
    type_check(t->u.array.type,n);
    list_enumerate(t->u.array.dimensions,dimensions_check,n);
    break;
  case FIXEDtag:
    if(t->u.fixed.digits){
      list l1;
      t->u.fixed.digits=
	value_compute(t->u.fixed.digits,l=iluparser_new_list(),n,the_long_t);
      t->u.fixed.exponent=
	value_compute(t->u.fixed.exponent,l1=iluparser_new_list(),
		      n,the_long_t);
      if(t->u.fixed.digits->u.INT<=0)
	idl_name_error(n,"number of digits not positive");
      list_clear(l1,FALSE);
      iluparser_Free(l1);
      /* l will be cleared below */
    }
    break;
  case UNIONtag:
    type_check (t->u._union.head, n);
    t->u._union.labels = iluparser_new_list ();
    list_enumerate (t->u._union.body, definition_check, 0);
    list_enumerate (t->u._union.body, type_recursion, t);
    break;
  }
  if(l){
    /* free temporary list */
    list_clear(l,FALSE);
    iluparser_Free(l);
  }
}

static boolean
equal_value (refany V1, refany V2)
{
  IDLValue v1 = V1;
  IDLValue v2 = V2;
  assert (v1->tag == v2->tag);
  switch (v1->tag){
    /* Not legal switch types */
  case idl_void:case idl_float:case idl_double:
  case idl_wstring:case idl_any:case idl_string:case idl_object:
  case idl_valuebase:
    /* Those cannot happen */
  case idl_int:case idl_fixed:
  case idl_unary:case idl_binary:case idl_named:
    return TRUE;

  case idl_short:case idl_long:case idl_unsigned_short:
  case idl_unsigned_long:case idl_octet:
  case idl_long_long:case idl_unsigned_long_long:
    return v1->u.INT == v2->u.INT;
  case idl_wchar:
  case idl_char:
    return v1->u.CHAR == v2->u.CHAR;
  case idl_boolean:
    return v1->u.BOOL == v2->u.BOOL;
  case idl_enum:
    return v1->u.enumerated.name == v2->u.enumerated.name;
  }
  return FALSE;
}

static void
caselabel_check(refany label,refany Union)
{
  IDLCase c=label;
  IDLDefinition u=Union;
  IDLName n=c->env->name;
  list l;
  if(c->value){
    c->value=value_compute(c->value,l=iluparser_new_list(),
			   n,type_compute(u->u.type->u._union.head,0));
    list_clear(l,FALSE);
    iluparser_Free(l);
    if (list_find (u->u.type->u._union.labels, equal_value, c->value))
      idl_name_error (n, "duplicate case label");
    list_insert (u->u.type->u._union.labels, c->value);
  }else{ /*default*/
      IDLName n1=u->u.type->u._union._default;
      if(n1 && n!=n1)
	idl_name_error(n,aprintf("default branch already assigned to %s",
			     n1->name));
      else
	u->u.type->u._union._default=n;
  }
}

/* compare location of base interface definition with this interface */
static void
definition_precedes(refany base,refany derived)
{
  IDLName d=derived;
  IDLName b=base;
  if(b->value->name->serial >= d->serial)
    idl_name_error(b,"base interface must precede derived one");
}

/* helper for error message */
static IDLDefinition current_interface;

static char*
i_or_v (IDLDefinition d)
{
  if (d->tag == INTERFACEtag)
    return "interface";
  return "value";
}

/* merge an operation in l, complain if it is already there */
static void
collect_operations1(refany def,refany l)
{
  IDLDefinition d=def;
  IDLDefinition d1;
  if(d->tag!=OPERATIONtag)return;
  if((d1=list_find(l,cmp_name,d->name->name)))
    idl_name_error(d->name,
		   aprintf("When defining %s %s, "
			   "operation conflicts with %s %s\n",
			   i_or_v (current_interface),
			   current_interface->name->name,
			   i_or_v (d1->env),
			   d1->env->name->name));
  else
    list_insert(l,d);
}
			       
/* merge the definitions of one base interface into l */
static void
collect_operations(refany base,refany l)
{
  IDLName b=base;
  list_enumerate (get_nested_definitions (b->value), collect_operations1, l);
}

/* check whether the raises clause contains only exceptions */
static void
exception_check(refany e, refany rock)
{
  IDLName E=e;
  assert(E->value);
  if (E->value->tag != EXCEPTIONtag)
    idl_name_error (E, "not an exception");
}

/* check whether a base is not abstract */
static void
inheritance_check (refany base, refany rock)
{
  IDLDefinition b = base;
  if (!b->u.interface.abstract)
    idl_name_error (b->name, "not an abstract interface");
}

struct vcheck{
  IDLDefinition v;
  int baseno;
};

/* check for value inheritance restrictions */
static void
vinheritance_check(refany b, refany rock)
{
  struct vcheck *check = rock;
  IDLName B = b;
  check->baseno++;
  /* Bases must be values. */
  if (B->value->tag != VALUEtag){
    idl_name_error (B, "not a value");
    return;
  }
  if (check->baseno == 1){
    /* First base: If it is a concrete value,
       the derived value must be concrete. */
    if (check->v->u.value.abstract && !B->value->u.value.abstract)
      idl_name_error (B, "concrete base for abstract value");
    /* Non-custom values may not inherit from custom values */
    if (!check->v->u.value.custom && B->value->u.value.custom)
      idl_name_error (B, "custom base for non-custom value");
  }else{
    /* Other bases must be abstract. */
    if (!B->value->u.value.abstract)
      idl_name_error (B, "more than one concrete base for value");
  }
}

/* check whether n names an interface */
static void
interface_check(refany name, refany rock)
{
  IDLName n = name;
  struct vcheck * check = rock;
  check->baseno++;
  if (n->value->tag != INTERFACEtag)
    idl_name_error (n, "not an interface");
  /* At most the first interface may be non-abstract */
  if ((check->baseno != 1) && !n->value->u.interface.abstract)
      idl_name_error (n, "not an abstract interface");
}

/* Check whether an interface appears twice in a base list. */
static boolean
equal_bases (refany name, refany orig)
{
  IDLName n = name;
  IDLName o = orig;
  /* We'll definitely find ourselves identically. */
  if (name == orig)
    return FALSE;
  /* If it's not the same definition, that's ok. */
  if (n->value != o->value)
    return FALSE;
  return TRUE;
}

static void
duplicate_bases (refany base, refany bases)
{
  IDLName n = base;
  if (list_find (bases, equal_bases, base))
    idl_name_error (n, "Listed twice as a base");
}

static void
oneway_outparam (refany param, refany rock)
{
  IDLDefinition p = param;
  if (p->u.parameter.direction == Out)
      idl_name_error (p->name, "invalid out parameter in oneway operation");
  if (p->u.parameter.direction == InOut)
      idl_name_error (p->name, "invalid inout parameter in oneway operation");
}

static void
oneway_check (IDLDefinition d)
{
  if (d->u.operation.returntype->tag != BASICtag
      || d->u.operation.returntype->u.basic != idl_void)
      idl_name_error (d->name, "oneway operation must not return a value");
  list_enumerate (d->u.operation.parameters, oneway_outparam, d->name);
}

/* Check whether the name of a definition is defined directly in the
   definition as well. */
static void
inner_name_check (IDLDefinition d)
{
  IDLDefinition i = lookup_name_in_scope (d, d->name->name);
  if (i)
    idl_name_error (i->name, "invalid immediate reuse of name");
}   
    
void 
definition_check(refany def,refany rock)
{
  IDLDefinition d=def;
  list l=0;
  list_enumerate(rock,duplicate_check,d);
  switch(d->tag){
  case NILtag:case TYPELISTtag:case MEMBERLISTtag:case ATTRLISTtag:
  case STATELISTtag:
    /* this should not happen */
    break;
  case PRAGMA_IDtag:case PRAGMA_VERSIONtag:case PRAGMA_PREFIXtag:
  case PRAGMA_PREFIX_PUSHtag:case PRAGMA_PREFIX_POPtag:
    /* nothing to do */
    break;
  case MODULEtag:
    /* Name reuse rule */
    inner_name_check (d);
    break;
  case INTERFACEtag:
    /* base interfaces must appear before derived */
    list_enumerate(d->u.interface.bases,definition_precedes,d->name);

    /* base interfaces must not share operation names */
    current_interface=d;
    l=iluparser_new_list();
    list_enumerate(d->u.interface.bases,collect_operations,l);
    /* now also check our own operations */
    list_enumerate(d->u.interface.definitions,collect_operations1,l);

    current_interface=0;
    /* abstract interfaces must not have concrete bases */
    if (d->u.interface.abstract)
      list_enumerate (d->u.interface.bases,inheritance_check,NULL);
    /* Base interfaces must not be listed twice */
    list_enumerate (d->u.interface.bases, duplicate_bases, d->u.interface.bases);
    /* Name reuse rule */
    inner_name_check (d);
    break;
  case VALUEtag:
    {
      struct vcheck check;
      /* base types must appear before derived */
      list_enumerate(d->u.value.bases,definition_precedes,d->name);
      list_enumerate(d->u.value.supports,definition_precedes,d->name);
      /* There are a number of restrictions on inheritance. */
      check.v = d;
      check.baseno = 0;
      list_enumerate(d->u.value.bases, vinheritance_check, &check);
      check.baseno = 0;
      list_enumerate (d->u.value.supports, interface_check, &check);
      if (d->u.value.custom && d->u.value.truncatable)
	idl_name_error (d->name, "custom value must not be truncatable");

      /* base values must not share operation names */
      current_interface = d;
      l=iluparser_new_list();
      list_enumerate(d->u.value.bases,collect_operations,l);
      /* also add operations of supported interfaces */
      list_enumerate(d->u.value.supports,collect_operations,l);
      /* now also check our own operations */
      list_enumerate(d->u.interface.definitions,collect_operations1,l);

      /* Name reuse rule */
      inner_name_check (d);
      break;
    }
  case VALUEBOXtag:
    type_check (d->u.type,d->name);
    break;
  case INTERFACEFWDtag:
  case VALUEFWDtag:
    break;
  case EXCEPTIONtag:
    /* Name reuse rule */
    inner_name_check (d);
    break;
  case CONSTtag:
    /* FIXME: type_check(d->u.constant*/
    definition_compute(d,l=iluparser_new_list(),d->name);
    break;
  case TYPEtag:
    type_check(d->u.type,d->name);
    /* Name reuse rule */
    inner_name_check (d);
    break;
  case OPERATIONtag:
    type_check(d->u.operation.returntype, d->name);
    list_enumerate(d->u.operation.raises,exception_check,0);
    if (d->u.operation.oneway)
      oneway_check (d);
    break;
  case FACTORYtag:
    break;
  case PARAMETERtag:
    type_check(d->u.parameter.type,d->name);
    break;
  case ATTRIBUTEtag:
    type_check(d->u.attribute.type,d->name);
    break;
  case STATEtag:
    type_check(d->u.state.type,d->name);
    list_enumerate (d->name->array, dimensions_check, d->name);
    break;
  case MEMBERtag:
    type_check(d->u.member,d->name);
    list_enumerate (d->name->array, dimensions_check, d->name);
    break;
  case CASEtag:
    type_check(d->u._case.type,d->name);
    list_enumerate(d->u._case.labels,caselabel_check,d->env);
    list_enumerate (d->name->array, dimensions_check, d->name);
    break;
  case ENUMVALtag:
    break;
  }
  list_enumerate(get_nested_definitions (d), definition_check,
		 get_nested_definitions (d));
  if(l){
    /* free list */
    list_clear(l,FALSE);
    iluparser_Free(l);
  }
}

/************************* setuid pass       *************************/

refany 
list_top (list l)
{
  if (!l || !l->head)
    return NULL;
  return l->head->data;
}

void
list_settop (list l, refany val)
{
  if (l->head)
    l->head->data = val;
}

void
list_pop (list l)
{
  if (l->head){
    l->head = l->head->next;
    if (!l->head)
      l->tail = NULL;
    l->count --;
  }
}

void definition_setuid(refany def,refany list);

static void
type_setuid(IDLType t,list p)
{
  list nested = 0;
  switch(t->tag){
    /* those don't have anything with uids inside */
  case NULLTYPEtag:
  case BASICtag:
  case REFERENCEDtag:
  case ENUMtag:
  case ALIAStag:
  case NATIVEtag:
    break;
    /* FIXME: anonymous types don't get a repository id, yet */
  case ARRAYtag:
  case SEQUENCEtag:
  case STRINGtag:
  case WSTRINGtag:
  case FIXEDtag:
    break;
  case UNIONtag:
    type_setuid(t->u._union.head,p);
    nested = t->u._union.body;
    break;
  case STRUCTtag:
    nested = t->u.structure;
    break;
  case DEFINEDtag:
    definition_setuid(t->u.defined,p);
    break;
  }
  if (nested) {
    list_enumerate (nested, definition_setuid, p);
  }
}

void
definition_setuid(refany def,refany p)
{
  list nested = 0;
  IDLDefinition d=def;
  char *prefix = list_top (p);
  list newprefix = iluparser_new_list ();

  if (d->name) {
    if (prefix && *prefix)
      prefix = aprintf("%s/%s",prefix,d->name->name);
    else
      prefix = d->name->name;
    list_push (newprefix, prefix);
    if (!d->id){
      d->id = aprintf("IDL:%s:%s", 
		      list_top (newprefix), 
		      d->version?d->version:"1.0");
    }
  }

  switch (d->tag){
  case NILtag:case TYPELISTtag:case ATTRLISTtag:case MEMBERLISTtag:
  case STATELISTtag:
    idl_name_error(d->name,"Unexpected case in definition_setuid");
    break;
    /* those don't have anything with uids inside */
  case INTERFACEFWDtag:case PARAMETERtag:case MEMBERtag:case CONSTtag:
  case ENUMVALtag:case CASEtag:case VALUEFWDtag:
    break;
  case MODULEtag:
    nested = d->u.module.definitions;
    break;
  case PRAGMA_PREFIXtag:
    list_settop (p, d->u.pragma);
    break;
  case PRAGMA_PREFIX_PUSHtag:
    list_push (p, "");
    break;
  case PRAGMA_PREFIX_POPtag:
    list_pop (p);
    break;
  case VALUEBOXtag:
    type_setuid (d->u.type, p);
    break;
  case INTERFACEtag:
    nested = d->u.interface.definitions;
    break;
  case VALUEtag:
    /* FIXME: hash */
    nested = d->u.value.definitions;
    break;
  case TYPEtag:
    type_setuid(d->u.type,newprefix);
    break;
  case EXCEPTIONtag:
    nested = d->u.exception.members;
    break;
    /* those don't have repository ids */
  case OPERATIONtag:
  case FACTORYtag:
  case ATTRIBUTEtag:
  case STATEtag:
  case PRAGMA_IDtag:
  case PRAGMA_VERSIONtag:
    return;
  }
  if (nested){
    list_enumerate (nested, definition_setuid, newprefix);
  } 
}

/************************* AB style guide pass ***********************/
/* Paragraph numbers are relative to ab/98-06-03 */
static void definition_style (refany def, refany rock);

static void
name211(IDLDefinition d)
{
  char *n = d->name->name;
  if (!isupper (n[0]))
    name_warning(d->name, "first letter not uppercase (style guide)");
  for (; *n; n++)
    if (!isalpha (*n)){
      name_warning (d->name, "only letters allowed here (style guide)");
      return;
    }
}

static void
name212(IDLDefinition d)
{
  char *n = d->name->name;
  if (!islower (n[0]))
    name_warning(d->name, "first letter not lowercase (style guide)");
  while (*n && n++)
    if (!islower (*n) && *n != '_'){
      name_warning (d->name, "only lowercase and _ allowed here (style guide)");
      return;
    }
}

static void
name213(IDLDefinition d)
{
  char *n = d->name->name;
  if (!isupper (n[0]))
    name_warning(d->name, "first letter not uppercase (style guide)");
  for (; *n; n++)
    if (!isupper (*n) && *n != '_'){
      name_warning (d->name, "only uppercase and _ allowed here (style guide)");
      return;
    }
}

static void
type_style (IDLDefinition d)
{
  IDLType t = d->u.type;
  switch (t->tag){
  case ENUMtag:
  case ALIAStag:
    name211(d);
    break;
  case STRUCTtag:
    name211(d);
    list_enumerate (t->u.structure, definition_style, 0);
    break;
  case UNIONtag:
    name211(d);
    list_enumerate (t->u._union.body, definition_style, 0);
    break;
  default:
    /* some compilers need a statement after default: */;
 }
}

static char* well_known_modules[] = {
  /* FIXME: Fill list */
  0
};

/* 1.1/2 */

static void
nested_module_style (IDLDefinition d)
{
  int i;
  for (i = 0; well_known_modules[i]; i++)
    if (strcmp (d->name->name, well_known_modules[i]) == 0)
      name_warning (d->name, "conflicts with well-known module (style guide)");
}

static void
definition_style (refany def, refany rock)
{
  IDLDefinition d = def;
  switch (d->tag){
  case MODULEtag:
    if (d->env)
      nested_module_style (d);
    /* fall through */
  case INTERFACEtag:
  case VALUEtag:
  case EXCEPTIONtag:
    name211 (d);
    break;
  case OPERATIONtag:
  case ATTRIBUTEtag:
  case PARAMETERtag:
  case STATEtag:
  case MEMBERtag:
  case CASEtag:
    name212 (d);
    break;
  case ENUMVALtag:
  case CONSTtag:
    name213 (d);
    break;
  case TYPEtag:
    type_style (def);
    break;
  }
}

/* 1.1/1 */

void ab_style (refany def, refany rock)
{
  IDLDefinition d = def;
  if (!(idl_subset & IDL_STYLE_GUIDE))
    return;
  switch (d->tag) {
    /* Those are ok on top-level */
  case MODULEtag:
  case PRAGMA_IDtag:
  case PRAGMA_VERSIONtag:
  case PRAGMA_PREFIXtag:
  case PRAGMA_PREFIX_PUSHtag:case PRAGMA_PREFIX_POPtag:
    break;
  default:
    name_warning (d->name, "not in a module (style guide)");
  }
  definition_style (def, rock);
}
