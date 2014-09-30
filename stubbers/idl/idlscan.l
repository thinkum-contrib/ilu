%{
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
$Id: idlscan.l,v 1.1 1999/07/23 23:16:37 janssen Exp $
*/
/* This code was contributed by Martin von Löwis */
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <ctype.h>
#ifndef STANDALONE
#include <iluptype.h>
#endif
#include "iluidl.h"
#include "idlparser.h"

struct include_file{
  YY_BUFFER_STATE state;
  char *file;
  int lineno;
};
#define MAX_INCLUDE_DEPTH 20
struct include_file include_stack[MAX_INCLUDE_DEPTH];
static int include_stack_ptr;

static void OpenInclude(char *statement,int brackets);
static void PopInclude();
static void inc_lineno();
static void name_setline(IDLName); 

char *current_cpp;
void CppAppend(char*);
void CppProcess();
void CppDefine(string s);
string CppDefinitionOf(string s);
boolean IfDefed(string s,int len);
boolean CppReplace(string s);

static int CheckKeyWord(char*);

static char* ab_file_comment_seen = 0;
static void ab_file_comment(char*);
static char* ab_guard = 0;
static boolean guard_defined = FALSE;
static boolean omg_pragma = FALSE;

static int ifnesting=0,iffailed=0;
static int cpp_continue_state;

typedef struct cpp_define_s{
	string name;
	string value;
}*cpp_define;

list CppDefines;

#define YY_USER_INIT init_scanner();
void init_scanner();

static IDLDefinition pragma_id(char*);
static IDLDefinition pragma_version(char*);
static IDLDefinition pragma_prefix(char*);

static IDLValue new_string(char*, int unicode);
enum CHARFORMAT{REGCHAR, OCTCHAR, HEXCHAR, UNICHAR, ESCCHAR};
static IDLValue new_char(char*, enum CHARFORMAT);
static IDLValue new_float(char*,char*,int,char*,int,char*);
static IDLValue new_fixed(char*);

struct keyword{
  char *name;
  int token;
  int flags;
};

struct keyword idlkeywords[]={
{"abstract", ABSTRACT, IDL_OBV},
{"any", ANY_T},
{"attribute", ATTRIBUTE},
{"boolean", BOOLEAN_T},
{"case", CASE},
{"char", CHAR_T},
{"const", CONST},
{"context", CONTEXT},
{"custom", CUSTOM, IDL_OBV},
{"default", DEFAULT},
{"double", DOUBLE_T},
{"enum", ENUM},
{"exception", EXCEPTION},
{"factory", FACTORY, IDL_OBV},
{"FALSE", BOOL_FALSE},
{"fixed", FIXED},
{"float", FLOAT_T},
{"in", IN},
{"inout", INOUT},
{"interface", INTERFACE},
{"long", LONG_T},
{"module", MODULE},
{"native", NATIVE},
{"Object", OBJECT_T},
{"octet", OCTET_T},
{"oneway", ONEWAY},
{"out", OUT},
{"private", PRIVATE, IDL_OBV},
{"public", PUBLIC, IDL_OBV},
{"raises", RAISES},
{"readonly", READONLY},
{"sequence", SEQUENCE},
{"short", SHORT_T},
{"string", STRING_T},
{"struct", STRUCT},
{"supports", SUPPORTS, IDL_OBV},
{"switch", SWITCH},
{"TRUE", BOOL_TRUE},
{"truncatable", TRUNCATABLE, IDL_OBV},
{"typedef", TYPEDEF},
{"union", UNION},
{"unsigned", UNSIGNED_T},
{"ValueBase", VALUEBASE, IDL_OBV},
{"valuetype", VALUETYPE, IDL_OBV},
{"void", VOID_T},
{"wchar", WCHAR_T},
{"wstring", WSTRING_T},
{0,0}
};

%}
/* CPP is exclusive: in CPP mode, normal rules are not considered */
%x CPP
%x CPPGET
%x COMMENT
%x FAILED
%option noyywrap
/* nested comments and such using yy_push_state */
%option stack
ID	[[:alpha:]][[:alnum:]_]*
CPID	[[:alpha:]_][[:alnum:]_]*
DEC	[[:digit:]]+
HEX	0x[[:xdigit:]]+
EXP	[Ee][+-]?[[:digit:]]+
COM	\/\*
%%
{COM}		yy_push_state(COMMENT);
<COMMENT>{
	\*\/	yy_pop_state();
	.	/*skip*/
	\n	inc_lineno();
	<<EOF>>	fprintf(stderr,"unterminated comment\n");
}

^[ \t]*#[ \t]*	BEGIN(CPPGET);
<CPPGET>{
	[^\\\n]*	CppAppend(yytext);
	\\\n	inc_lineno();
	\\	CppAppend(yytext);
	\n	inc_lineno();CppProcess();
}
<CPP>{
	{COM}	yy_push_state(COMMENT);
	^include[ \t]+\<[^>]+>[ \t]*	{
		OpenInclude(yytext,1);
		return PRAGMA_PREFIX;
	}
	^include[ \t]+\"[^\"]+\"[ \t]*	{
		OpenInclude(yytext,0);
		return PRAGMA_PREFIX;
	}
	^ifdef[ \t]+{CPID}	{
					if(!IfDefed(yytext,6)){
						cpp_continue_state=FAILED;
						iffailed++;
					}
					ifnesting++;
				}
	^ifndef[ \t]+{CPID}	{	if(IfDefed(yytext,7)){
						cpp_continue_state=FAILED;
						iffailed++;
					}
					ifnesting++;
				}
	^else[ \t]*	cpp_continue_state=FAILED;iffailed++;
	^endif[ \t]*	{
				ifnesting--;
				if(ifnesting<0){
					fprintf(stderr,"unbalanced #endif\n");
					exit(1);
				}
			}
	^define[ \t]+{CPID}.*	{
					CppDefine(yytext);
			}
	^pragma[ \t]+ID[ \t].*	{
				idllval.definition=pragma_id(yytext);
				if(idllval.definition)
				  return PRAGMA_ID;
				else
				  exit(1);
			}
	^pragma[ \t]+version[ \t].* {
				idllval.definition=pragma_version(yytext);
				if(idllval.definition)
				  return PRAGMA_VERSION;
				else
				  exit(1);
			}
	^pragma[ \t]+prefix[ \t].*  {
				idllval.definition=pragma_prefix(yytext);
				if(idllval.definition)
				  return PRAGMA_PREFIX;
				else
				  exit(1);
			}
	.+	fprintf(stderr,"Unknown preprocessor directive %s\n",yytext);
	<<EOF>>		{
			 PopInclude();
			 BEGIN(cpp_continue_state);
			}
}

<FAILED>{
	^#[ \t]*ifdef	ifnesting++;iffailed++;
	^#[ \t]*else	if(--iffailed)iffailed++;else{BEGIN(0);}
	^#[ \t]*endif	ifnesting--;iffailed--;if(!iffailed){BEGIN(0);}
	.		/*skip*/
	\n		inc_lineno();
}

{DEC}		{	idllval.value=new_value();
			idllval.value->tag=idl_int;
			idllval.value->u.INT=strtoul(yytext,0,10);
			return INTEGER_L;
		}
{HEX}		{	idllval.value=new_value();
			idllval.value->tag=idl_int;
			idllval.value->u.INT=strtoul(yytext,0,16);
			return INTEGER_L;
		}
[[:digit:]]+(\.[[:digit:]]*)?[dD]	{
			idllval.value=new_fixed(yytext);
			return FIXED_PT_L;
		}
\.[[:digit:]]+[dD]			{
			idllval.value=new_fixed(yytext);
			return FIXED_PT_L;
		}
[[:digit:]]+{EXP}	{char *e;
			e=strchr(yytext,'e');if(!e)e=strchr(yytext,'E');
			idllval.value=new_float(yytext,yytext,e-yytext,
						0,0,e+1);
			return FLOAT_L;
			}
[[:digit:]]+\.[[:digit:]]*{EXP}?	{
			char *f,*e;
			f=strchr(yytext,'.');
			e=strchr(f+1,'e');
			if(!e)e=strchr(f+1,'E');
			idllval.value=new_float(yytext,yytext,f-yytext,
						f+1,e?e-f-1:strlen(f+1),e);
			return FLOAT_L;
			}
[[:digit:]]*\.[[:digit:]]+{EXP}?	{
			char *f,*e;
			f=strchr(yytext,'.');
			e=strchr(f+1,'e');
			if(!e)e=strchr(f+1,'E');
			idllval.value=new_float(yytext,yytext,f-yytext,
						f+1,e?e-f-1:strlen(f+1),e);
			return FLOAT_L;
			}
\'[[:print:]]\'			{idllval.value = new_char(yytext+1, REGCHAR);
				 return CHAR_L;
				}
\'\\[0-7]{1,3}\'		{idllval.value = new_char(yytext+2, OCTCHAR);
				 return CHAR_L;
				}
\'\\x[[:xdigit:]]{1,2}\'	{idllval.value = new_char(yytext+3, HEXCHAR);
				 return CHAR_L;
				}
\'\\.\'				{idllval.value = new_char(yytext+2, ESCCHAR);
				 return CHAR_L;
				}
L\'[[:print:]]\'		{idllval.value = new_char(yytext+2, REGCHAR);
				 idllval.value->tag = idl_wchar;
				 return CHAR_L;
				}
L\'\\[0-7]{1,3}\'		{idllval.value = new_char(yytext+3, OCTCHAR);
				 idllval.value->tag = idl_wchar;
				 return CHAR_L;
				}
L\'\\x[[:xdigit:]]{1,2}\'	{idllval.value = new_char(yytext+4, HEXCHAR);
				 idllval.value->tag = idl_wchar;
				 return CHAR_L;
				}
L\'\\.\'			{idllval.value = new_char(yytext+3, ESCCHAR);
				 idllval.value->tag = idl_wchar;
				 return CHAR_L;
				}
L\'\\u[[:xdigit:]]{1,4}\'	{idllval.value = new_char(yytext+4, UNICHAR);
				 idllval.value->tag = idl_wchar;
				 return CHAR_L;
				}
\"([^\"\n\t\v\b\r\f\a]|\\\")*\"	{
					idllval.value=new_string(yytext, 0);
					return STRING_L;
				}
L\"([^\"\n\t\v\b\r\f\a]|\\\")*\"	{
					idllval.value=new_string(yytext, 1);
					idllval.value->tag = idl_wstring;
					return STRING_L;
				}
{ID}		{ if(!CppReplace(yytext)){
			int val;
			val = CheckKeyWord(yytext);
			if (val)
				return val;
			idllval.name=new_name();
			idllval.name->name=ilu_strdup(yytext);
			name_setline(idllval.name);
			return IDENT;
		  }
		}
_{ID}		{ if(!CppReplace(yytext)){
			idllval.name=new_name();
			idllval.name->name=ilu_strdup(yytext+1);
			name_setline(idllval.name);
			return IDENT;
		  }
		}			

::		return SCOPE;
>>		return RSHIFT;
\<\<		return LSHIFT;
[-|^&+*/%]	return yytext[0]; /* arithmetic operators */;
[(){},:;=<>]	return yytext[0];
\[		return yytext[0];
\]		return yytext[0];

[ \t]		;
\n		inc_lineno();
.		return yytext[0];

\/\/.*$		{ /* C++ comment */
		  ab_file_comment (yytext);
                } 

<INITIAL><<EOF>>	{
		if(--include_stack_ptr==0)
    				yyterminate();
                 else{
		   yy_switch_to_buffer(include_stack[include_stack_ptr].state);
		   BEGIN(0);
		   if (include_stack[include_stack_ptr].file){
		     idllval.definition = new_definition ();
		     idllval.definition->tag = PRAGMA_PREFIX_POPtag;
		     return PRAGMA_PREFIX;
		   }
		 }
		}
%%

void init_scanner()
{
	CppDefines=iluparser_new_list();
}

/* yy_scan_bytes für defines */

void OpenInclude(char *statement,int brackets)
{
  char *file, *path = 0;
  char *start,*stop;
  if ((idl_subset & IDL_STYLE_GUIDE)
      && omg_pragma)
    fprintf (stderr, "include following omg.org pragma (style guide)\n");
  if(brackets){
    start=strchr(statement,'<');
    assert(start);
    stop=strchr(start+1,'>');
    assert(stop);
  }else{
    start=strchr(statement,'"');
    assert(start);
    stop=strchr(start+1,'"');
    assert(stop);
  }
  *stop='\0';
  file=ilu_strdup(start+1);
  assert(include_stack_ptr<MAX_INCLUDE_DEPTH);
#if defined(WIN32)
#define DIR_SEPARATOR_CHAR '\\'
  if (file[0] != '\\' && file[1] != ':') {
#elif defined(macintosh)
#define DIR_SEPARATOR_CHAR ':'
  if (*file == DIR_SEPARATOR_CHAR) {
#else
#define DIR_SEPARATOR_CHAR '/'
  if (file[0] != DIR_SEPARATOR_CHAR) {
#endif
     /* if file is not an absolute path name then we should first
      * look in the directory of the file that contained the include
      * directive.
      */
     char *current_file, *last_separator;
     int i;
     for (i = include_stack_ptr - 1; include_stack[i].file == 0; --i)
	/*EMPTY*/;
     current_file = include_stack[i].file;
     last_separator = strrchr(current_file, DIR_SEPARATOR_CHAR);
     if (last_separator) {
	*last_separator = 0;
	path = iluparser_FindFileInDir(current_file, file);
	*last_separator = DIR_SEPARATOR_CHAR;
     }
  }
  if (!path)
     path = iluparser_FindFileInIncludes(file);
  if (!path) {
     fprintf (stderr, "file \"%s\" not found in include path.\n", file);
     exit(1);
  }
  include_stack[include_stack_ptr].state=YY_CURRENT_BUFFER;
  include_stack[include_stack_ptr].file=path;
  include_stack[include_stack_ptr].lineno=1;
  include_stack_ptr++;
  idlin=fopen(path,"r");
  if(!idlin){perror(path);exit(1);}
  yy_switch_to_buffer(yy_create_buffer(idlin,YY_BUF_SIZE));
  /* new file is scanned like top-level */
  BEGIN(0);
  idllval.definition = new_definition ();
  idllval.definition->tag=PRAGMA_PREFIX_PUSHtag;
}

void PopInclude()
{
  --include_stack_ptr;
  assert(include_stack_ptr>=0);
  yy_switch_to_buffer(include_stack[include_stack_ptr].state);
  if (include_stack[include_stack_ptr].file)
    iluparser_Free(include_stack[include_stack_ptr].file);
}

/* Remove comment from preprocessor directive */
static void
RemoveComment (char *s)
{
  char *start;
  char *end;
  for (start = s; *start; start++)
    if (start[0] == '/' && start[1] == '/'){
      /* Remove end-of-line comment */
      *start = '\0';
      return;
    }
    else if(start[0] == '/' && start[1] == '*')
      break;
  if (!*start)
    return;
  for (end = start+2; *end; end++)
    if (end[0]=='*' && end[1]=='/')
      break;
  if (!*end)
    return;
  end += 2;
  /* Remove C style comment */
  while (*end)
    *start++ = *end++;
}

void CppAppend(char* s)
{
  char *n;
  if(current_cpp){
    n=iluparser_Malloc(strlen(current_cpp)+strlen(s)+1);
    strcpy(n,current_cpp);
    strcat(n,s);
    iluparser_Free(current_cpp);
    current_cpp=n;
  }else{
    current_cpp=ilu_strdup(s);
  }
  RemoveComment (current_cpp);
}

void CppProcess()
{
  /* FIXME: could avoid copy, should delete buffer, string vs file */
  include_stack[include_stack_ptr].state=YY_CURRENT_BUFFER;
  include_stack[include_stack_ptr].file=0;
  include_stack_ptr++;
  /* this already does the switch_to_buffer */
  yy_scan_string(current_cpp);
  iluparser_Free(current_cpp);
  current_cpp=0;
  /* where to go on after we're done */
  cpp_continue_state=INITIAL;
  BEGIN(CPP);
}

void CppDefine(string s)
{
  string name=ilu_strdup(s+7); /* skip "define " */
  string end;
  cpp_define def;
  while(isspace(*name))name++;
  name=ilu_strdup(name);
  for(end=name+1;*end && isidlidentchar(*end);end++);
  *end='\0';
  end++;
  def=iluparser_Malloc(sizeof(struct cpp_define_s));
  def->name=name;
  def->value=end;
  list_insert(CppDefines,def);
  if (idl_subset & IDL_STYLE_GUIDE){
    if (!ab_guard || guard_defined)
      return;
    /* First define should define the guard */
    if (strcmp (name, ab_guard) != 0)
      fprintf (stderr, "guard %s not defined (style warning)\n", ab_guard);
    guard_defined = TRUE;
  }
}

static boolean define_find(cpp_define item,string s)
{
  return strcmp(item->name,s)==0;
}

string CppDefinitionOf(string s)
{
  cpp_define def=list_find(CppDefines,(iluparser_FindProc)define_find,s);
  return def ? def->value : NULL;
}

boolean IfDefed(string s,int len)
{
  string name=s+len; /* skip "ifdef " */
  string end;
  boolean result;
  while(!isidlidentchar(*name))name++;
  /* Fixme: do ifdef lookup with name and length */
  name=ilu_strdup(name);
  for(end=name+1;*end && isidlidentchar(*end);end++);
  *end='\0';
  result = CppDefinitionOf(name)!=NULL;
  if (idl_subset & IDL_STYLE_GUIDE){
    /* First ifdef should be the guard, it should evaluate false */
    if (!ab_guard)
      ab_guard = name;
    if (result)
      fprintf(stderr, "Guard should not be defined yet (style warning)\n");
  }else
    iluparser_Free(name);
  return result;
}

boolean CppReplace(string s)
{
  string r=CppDefinitionOf(s);
  if(!r)return 0;
  if(include_stack_ptr==MAX_INCLUDE_DEPTH){
    fprintf(stderr,"Recursive macro expansion for %s\n",s);
    exit(1);
  }
  include_stack[include_stack_ptr].state=YY_CURRENT_BUFFER;
  include_stack[include_stack_ptr].file=0;
  include_stack_ptr++;
  yy_scan_string(r);
  return 1;
}

string strndup (string s, int len)
{
  string retval = malloc(len + 1);
  memcpy((char *) retval, (char *) s, len) ;
  retval[len] = 0;
  return retval;
}

static IDLDefinition
name_error(char *str)
{
  fprintf(stderr,"No name given in %s\n",str);
  return 0;
}

static IDLDefinition
value_error(char *str)
{
  fprintf(stderr,"Invalid value in %s\n",str);
  return 0;
}

static IDLName
pragma_name(char* start,char* end,IDLName scope)
{
  IDLName result;
  char *it;
  for(it=start;it!=end;it++)
    if(*it==':')break;
  if(*it==':'){
    if(it[1]!=':' || it[2]==':'){
      fprintf(stderr,"Invalid scope in %*s\n",end-start,start);
      return 0;
    }
  }
  result=new_name();
  result->name=iluparser_Malloc(it-start+1);
  memcpy(result->name,start,it-start);
  result->name[it-start]='\0';
  result->scope=scope;
  name_setline(result);
  result->no_ordering=1;
  if(it!=end)
    return pragma_name(it+2,end,result);
  return result;
}

static IDLDefinition 
pragma_id(char* str)
{
  IDLDefinition result;
  char *name_start,*name_end,*value_start,*value_end;
  char *it;
  it=strstr(str,"ID");
  it+=2;
  while(*it && isspace(*it))it++;
  if(!*it)
    return name_error(str);
  name_start=it;
  while(*it && (isalnum(*it)||(*it=='_')||(*it==':')))it++;
  name_end=it;
  if(name_end==name_start)
    return name_error(str);
  while(*it && isspace(*it))it++;
  if(*it!='"')
    return value_error(str);
  it++;
  value_start=it;
  while(*it && *it!='"')it++;
  if(*it!='"' || it==value_start)
    return value_error(str);
  value_end=it;
  result=new_definition();
  result->tag=PRAGMA_IDtag;
  result->name=pragma_name(name_start,name_end,0);

  result->u.pragma=iluparser_Malloc(value_end-value_start+1);
  memcpy(result->u.pragma,value_start,value_end-value_start);
  result->u.pragma[value_end-value_start]='\0';
  return result;
}

static IDLDefinition 
pragma_version(char* str)
{
  IDLDefinition result=new_definition();
  char *name_start,*name_end,*value_start,*value_end;
  char *it;
  it=strstr(str,"version");
  it+=7;
  while(*it && isspace(*it))it++;
  if(!*it)
    return name_error(str);
  name_start=it;
  while(*it && (isalnum(*it)||(*it=='_')||(*it==':')))it++;
  name_end=it;
  if(name_end==name_start)
    return name_error(str);
  while(*it && isspace(*it))it++;
  if(!isdigit(*it))
    return value_error(str);
  value_start=it;
  while(*it && (isdigit(*it)||*it=='.'))it++;
  if(it==value_start)
    return value_error(str);
  value_end=it;
  result->tag=PRAGMA_VERSIONtag;
  result->name=pragma_name(name_start,name_end,0);

  result->u.pragma=iluparser_Malloc(value_end-value_start+1);
  memcpy(result->u.pragma,value_start,value_end-value_start);
  result->u.pragma[value_end-value_start]='\0';
  return result;
}
  
static IDLDefinition 
pragma_prefix(char* str)
{
  IDLDefinition result;
  char *value_start,*value_end;
  char *it;
  it=strstr(str,"prefix");
  it+=6;
  while(*it && isspace(*it))it++;
  if(*it!='"')
    return value_error(str);
  it++;
  value_start=it;
  while(*it && *it!='"')it++;
  if(*it!='"' || it==value_start)
    return value_error(str);
  value_end=it;
  result=new_definition();
  result->tag=PRAGMA_PREFIXtag;
  result->u.pragma=iluparser_Malloc(value_end-value_start+1);
  memcpy(result->u.pragma,value_start,value_end-value_start);
  result->u.pragma[value_end-value_start]='\0';
  if ((idl_subset & IDL_STYLE_GUIDE)
      && !omg_pragma){
    if (strcmp (result->u.pragma, "omg.org") != 0)
      fprintf (stderr, "wrong pragma in OMG spec (style guide)\n");
    omg_pragma = TRUE;
  }    
  return result;
}

void
idlsetinitialfile(char* file)
{
  include_stack[0].file=file;
  include_stack[0].lineno=1;
  include_stack_ptr=1;
}

static int
current_file_index()
{
  int i;
  if (include_stack_ptr == 0) {
    /* FIXME:  Check that we are really at EOF */
    return 0;
  };
  for(i=include_stack_ptr-1;i>=0;i--)
    if(include_stack[i].file)
      return i;
  fprintf(stderr,"Error:Include stack underflow\n");
  exit(1);
  return 0; /* some compilers complain otherwise */
}

static void
inc_lineno()
{
  int i=current_file_index();
  include_stack[i].lineno++;
}

static void
name_setline(IDLName n)
{
  int i=current_file_index();
  n->file=include_stack[i].file;
  n->line=include_stack[i].lineno;
}

char*
idlcurrentfile()
{
  int i=current_file_index();
  return include_stack[i].file;
}

int
idlcurrentline()
{
  int i=current_file_index();
  return include_stack[i].lineno;
}

static IDLValue
new_char(char *s, enum CHARFORMAT f)
{
  char c;
  IDLValue result = new_value();
  result->tag=idl_char;
  switch (f){
  case REGCHAR:
    result->u.CHAR = *s;
    break;
  case OCTCHAR:
    result->u.CHAR = strtol(s,NULL,8);
    break;
  case HEXCHAR:
    result->u.CHAR = strtol(s,NULL,16);
    break;
  case UNICHAR:
    result->u.CHAR = strtol(s,NULL,16);
    break;
  case ESCCHAR:
    switch(*s){
    case 'n': c='\n';break;
    case 't': c='\t';break;
    case 'v': c='\v';break;
    case 'b': c='\b';break;
    case 'r': c='\r';break;	
    case 'f': c='\f';break;
    case 'a': c='\a';break;
    case '\\': c='\\';break;
    case '\?': c='\?';break;
    case '\'': c='\'';break;
    case '\"': c='\"';break;
    default: 
      result->u.CHAR = *s;break;
    }
    result->u.CHAR = c;
    break;
  }
  return result;
}

static int hexval (char c)
{
  if (c >= '0' && c <= '9')
    return c - '0';
  if (c >= 'a' && c <= 'f')
    return c - 'a';
  if (c >= 'A' && c <= 'F')
    return c - 'F';
  return -1;
}


static int
gethex (char *s, char* s1)
{
  int tmp;

  *s1 = tmp = hexval (*s);
  if (tmp == -1)
    return 0;
  tmp = hexval (s[1]);
  if (tmp == -1)
    return 1;
  *s1 = 16 * *s1 + tmp;
  return 2;
}

static IDLValue
new_string(char *s, int unicode)
{
  char *orig = s;
  char *s1;
  int hexlen;
  IDLValue result=new_value();
  result->tag=idl_string;
  result->u.string=s1=ilu_strdup(s+1);
  /* skip first ", check for last " */
  while(*(++s+1)){
    if(*s=='\\')
      switch(*++s){
      case 'n': *s1='\n';break;
      case 't': *s1='\t';break;
      case 'v': *s1='\v';break;
      case 'b': *s1='\b';break;
      case 'r': *s1='\r';break;	
      case 'f': *s1='\f';break;
      case 'a': *s1='\a';break;
      case '\\': *s1='\\';break;
      case '\?': *s1='\?';break;
      case '\'': *s1='\'';break;
      case '\"': *s1='\"';break;
      case 'x': 
	/* See if it is a hex char */
	if (hexlen = gethex (s, s1)) {
	  s+=hexlen;
	  break;
	}
	/* Undefined behaviour: Not a valid \x escape */
	*s1++ = '\\';
	*s1 = 'x';
	break;
      case 'u':
	if (!unicode)
	  /* \u in non-unicode string */
	  value_error (orig);
      default: *s1=*s;break;
      }
    else
      *s1=*s;
    s1++;
  }
  *s1='\0';
  return result;
}

static IDLValue
new_float(char *text,char *i,int ilen,char* f,int flen,char *e)
{
	IDLValue ret=new_value();
	ret->tag=idl_float;
	ret->u.FLOAT.val=strtod(text,0);
	ret->u.FLOAT.sign=0;
	/* 0 is not allowed for the integer part in a constantvalue_s */
	ret->u.FLOAT.integer=ilen?strndup(i,ilen):"0";
	ret->u.FLOAT.fraction=flen?strndup(f,flen):0;
	/* e is like "E+10" */
	ret->u.FLOAT.exponent=e?strtol(e+1,0,10):0;
	return ret;
}

static IDLValue
new_fixed(char *text)
{
  IDLValue ret = new_value();
  int first, last, dot;
  int i,k;

  ret->tag = idl_fixed;
  ret->u.fixed.negative = FALSE;
  first = last = dot = -1;
  for (i=0;text[i];i++)
    switch (text[i])
      {
      case '.': dot = i; break;
      case '1': case '2': case '3': case '4': case '5':
      case '6': case '7': case '8': case '9':
	if (first == -1)
	  first = i;
	last = i;
	break;
      case 'd': case 'D':
	if (first == -1)
	  first = i-1;
	if (last == -1)
	  last = i-1;
	if (dot == -1)
	  dot = i;
	break;
      }
  ret->u.fixed.digits = iluparser_Malloc (last - first + 2);
  for (i = first, k = 0; i<=last; i++)
    if (i!=dot)
      ret->u.fixed.digits[k++] = text[i];
  ret->u.fixed.digits[k] = '\0';
  if (last>dot)
    ret->u.fixed.exponent = dot-last;
  else
    ret->u.fixed.exponent = dot-last-1;
  return ret;
}

static int
CheckKeyWord(char *name)
{
  /* First, check for exact keyword. */
  int i;
  if (idl_subset & IDL_STYLE_GUIDE){
    /* Before the first keyword, we should have seen the file comment
       and the guard.  */
    if (!ab_file_comment_seen){
      /* 1.5.1 */
      fprintf (stderr, "File does not contain file comment (style guide)\n");
      ab_file_comment_seen = "unnamed file";
    }
    if (!guard_defined){
      /* 1.5.2 */
      fprintf (stderr, "File does not contain guard (style guide)\n");
      guard_defined = TRUE;
    }
    /* 1.5.2/3 not implemented */
    /* 1.5.3 */
    if (!omg_pragma){
      fprintf (stderr, "omg.org pragma missing (style guide)\n");
      omg_pragma = TRUE;
    }
  }
  for (i=0; idlkeywords[i].name; i++)
    if ((idl_subset | idlkeywords[i].flags) == idl_subset
	&& strcmp (idlkeywords[i].name, name) == 0)
      switch (idlkeywords[i].token)
	{
	/* Special-case TRUE and FALSE. */
	case BOOL_TRUE:
	  idllval.value=new_value();
	  idllval.value->tag=idl_boolean;
	  idllval.value->u.BOOL=TRUE;
	  return BOOL_TRUE;
	case BOOL_FALSE:
	  idllval.value=new_value();
	  idllval.value->tag=idl_boolean;
	  idllval.value->u.BOOL=FALSE;
	  return BOOL_FALSE;
	default:
	  return idlkeywords[i].token;
	}
  /* Now, check for keyword difference in case. */
  for (i=0; idlkeywords[i].name; i++)
    if (ilu_strcasecmp (idlkeywords[i].name, name) == 0)
      {
	fprintf (stderr, "Identifier %s conflicts with keyword %s\n",
		 name, idlkeywords[i].name);
	/* Return the keyword. */
	return idlkeywords[i].token;
      }
  /* Success */
  return 0;
}

static void
ab_file_comment (char* text)
{
  if (!(idl_subset & IDL_STYLE_GUIDE))
    return;
  if (strncmp (text, "//File:", 7) == 0)
    ab_file_comment_seen = ilu_strdup (text);
}
