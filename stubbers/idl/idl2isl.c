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

/* These are the IDL front end passes that generate ISL. */

#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdio.h>
#include <assert.h>
#include <ctype.h>

#include <iluptype.h>
#include "iluidl.h"

extern char * FigureTypeUID(Type);
extern int    init_types(void);
extern int    idlparse(void);

/********* Converting to ISL ************************************/

/* copied from ilu.bison */
Name new_Name(void)
{
  Name new = (Name) iluparser_Malloc (sizeof(struct name_s));
  new->base_name = NULL;
  new->langnames = (set) new_list();
  return (new);
}

static Exception new_Exception(void)
{
  Exception new = (Exception) iluparser_Malloc (sizeof(struct ilu_exception_s));
  new->name = new_Name();
  new->scoping = iluparser_new_list();
  new->type = NULL;
  new->valueOptional = FALSE;
  new->refs = new_list();
  new->def = 0;
  new->id = 0;
  new->builtIn = FALSE;
  new->interface = NULL;
  new->importInterfaceName = NULL;
  new->import = NULL;
  new->marked = FALSE;
  new->corba_rep_id = NULL;
  new->doc_string = NULL;
  return (new);
}

Type new_Type(void)
{
  Type new = (Type) iluparser_Malloc (sizeof(struct ilu_type_s));
  new->scoping = iluparser_new_list();
  new->name = (Name) new_Name();
  new->description = NULL;
  new->supertype = NULL;
  new->refs = new_list();
  new->def = 0;
  new->builtIn = FALSE;
  new->importInterfaceName = NULL;
  new->interface = NULL;
  new->cached_des = NULL;
  new->marked = FALSE;
  new->uid = NULL;
  new->explicit_uid = FALSE;
  new->brand = NULL;
  return (new);
}

TypeDescription new_TypeDescription(void)
{
  TypeDescription new = (TypeDescription) iluparser_Malloc(sizeof(struct ilu_typedes_s));
  new->type = invalid_Type;
  memset((char *) new, 0, sizeof(struct ilu_typedes_s));
  return (new);
}

static Procedure new_Procedure(void)
{
  Procedure new = (Procedure) iluparser_Malloc (sizeof(struct ilu_proc_s));
  new->name = new_Name();
  new->object = NULL;
  new->arguments = NULL;
  new->returnType = NULL;
  new->returnOptional = FALSE;
  new->functional = FALSE;
  new->asynch = FALSE;
  new->exceptions = NULL;
  new->def = 0;
  new->id = -1;
  new->interface = NULL;
  new->marked = FALSE;
  /* added MvL */
  new->authentication_type = NULL;
  new->doc_string = NULL;
  return (new);
}

static Argument new_Argument(void)
{
  Argument new = (Argument) iluparser_Malloc (sizeof(struct ilu_argument_s));
  new->name = new_Name();
  new->type = NULL;
  new->values = NULL;
  new->def = 0;
  new->sibling = FALSE;
  new->direction = In;
  return (new);
}

/* end of copy */

char*
underscore2hyphen(char *s)
{
  char *result=ilu_strdup(s);
  for(s=result;*s;s++)
    if(*s=='_')*s='-';
  return result;
}

/* return sign(digits) 10^abs(digits) as an ISL large integer */

static IntegerLiteral
fixedPow(int digits)
{
  static int logmaxint = 0;
  int i;
  IntegerLiteral result = (IntegerLiteral) iluparser_Malloc (sizeof(*result));
  if (digits<0)
    {
      result->negative = TRUE;
      digits = -digits;
    }
  if (!logmaxint)
    for (i = INT_MAX; i>=10; i /= 10)
      logmaxint++;
  if (digits <= logmaxint)
    {
      result->small=TRUE;
      for(i=1;digits;digits--)
	i *= 10;
      result->val.direct = i;
    }
  else
    {
      result->small = FALSE;
      result->val.string = iluparser_Malloc (digits+2);
      result->val.string[0] = '1';
      result->val.string[digits+1] = '\0';
      while (digits--)
	result->val.string[digits+1] = '0';
    }
  return result;
}

/************************* maketoplevel      *************************/

static boolean
find_nonmodules(refany def,refany rock)
{
  IDLDefinition d=def;
  IDLDefinition r=rock;
  switch(d->tag){
    /* those are OK on toplevel */
  case PRAGMA_IDtag:case PRAGMA_VERSIONtag:case PRAGMA_PREFIXtag:
  case PRAGMA_PREFIX_PUSHtag:case PRAGMA_PREFIX_POPtag:
  case MODULEtag:
    return FALSE;
  default:
    return r ? (d->name->file==r->name->file):1;
  }
}

static void
make_toplevel(struct idl_parse *P)
{
  IDLDefinition d,m;
  char *s,*name;
  do{
    d=list_find(P->definitions,find_nonmodules,0);
    if(!d)break;
    m=new_definition();
    m->tag=MODULEtag;
    m->name=new_name();
    m->name->name=name=iluparser_basename(d->name->file);
    /* make sure this is a valid identifier */
    while(*name && !isalpha(*name))
      name++;
    if(!*name)
      idl_name_error(d->name,"source file contains no letters");
    /* if it doesn't start with a letter, copy it */
    if(name!=m->name->name){
      s=m->name->name;
      m->name->name=ilu_strdup(name);
      iluparser_Free(s);
    }
    /* find the first invalid character */
    for(s=m->name->name;*s && (isalnum(*s) || *s=='_');s++)
      ;
    if(*s)*s='\0';
    m->name->lifted=underscore2hyphen(m->name->name);
    m->name->file=d->name->file;
    name_warning(m->name,"warning: introducing module");
    m->u.module.definitions=iluparser_new_list();
    list_insert(P->definitions,m);
    while(d){
      list_insert(m->u.module.definitions,d);
      definition_backlink(d,m);
      list_remove(P->definitions,d);
      d=list_find(P->definitions,find_nonmodules,m);
    }
  }while(1);
}

/************************* lift pass         *************************/

struct lifter{
  char *prefix;
  list lifted;
};

static void
lift_prefix(refany def,refany lift)
{
  IDLDefinition d=def;
  struct lifter *l=lift;
  d->name->lifted=aprintf("%s-%s",l->prefix,d->name->lifted);
  list_insert(l->lifted,d);
}

static void
definition_lift(refany def,refany rock)
{
  IDLDefinition d=def;
  struct lifter l;
  boolean do_lift=FALSE;
  if(!d->name)
    return;
  l.prefix=underscore2hyphen(d->name->name);
  l.lifted=iluparser_new_list();
  switch(d->tag){
  case MODULEtag:
    /* don't lift names directly inside the toplevel module */
    list_enumerate(d->u.module.definitions,definition_lift,d->env?&l:0);
    do_lift=1;
    break;
  case INTERFACEtag:
    list_enumerate(d->u.interface.definitions,definition_lift,&l);
    do_lift=1;
    break;
  case EXCEPTIONtag:
    do_lift=1;
    list_enumerate(d->u.exception.members,definition_lift,&l);
    break;
  case TYPEtag:
    do_lift=1;
    switch(d->u.type->tag){
    case STRUCTtag:
      list_enumerate(d->u.type->u.structure,definition_lift,&l);
      break;
    case UNIONtag:
      list_enumerate(d->u.type->u._union.body,definition_lift,&l);
      break;
    default:break;
    }
    break;
  case CONSTtag:
    do_lift=1;
    break;
  default:
    break;
  }
  /* lift myself as well */
  if(do_lift)
    list_insert(l.lifted,d);
  if(rock)
    list_enumerate(l.lifted,lift_prefix,rock);
  iluparser_Free(l.prefix);
  /* free list */
  list_clear(l.lifted,FALSE);
  iluparser_Free(l.lifted);
}

/************************* makeisl pass      *************************/

static void module_makeisl(IDLDefinition d);
static void interface_makeisl(IDLDefinition d);
static void operation_makeisl(IDLDefinition d);
static void typedef_makeisl(IDLDefinition d);
static void member_makeisl(IDLDefinition d);
static void parameter_makeisl(IDLDefinition d);
static void exception_makeisl(IDLDefinition d);
static void case_makeisl(IDLDefinition d);
static void const_makeisl(IDLDefinition d);
static void attribute_makeisl(IDLDefinition d);
static void value_makeisl(IDLValue v);
static void type_makeisl(IDLType p,IDLDefinition env);

static void
definition_setscoping(IDLDefinition d,list l)
{
  list_push(l,d->name->name);
  while(d->env){
    d=d->env;
    if (d->tag != MEMBERtag)
      list_push(l,d->name->name);
  }
}

static boolean
find_imported(refany imported,refany name)
{
  return strcmp(((Imported)imported)->name,name)==0;
}

static void
add_import(IDLDefinition m,char *name,char *filename)
{
  Interface I=m->isl;
  Imported imp;
  assert(I);
  /* importing self? */
  if(strcmp(name,m->name->name)==0)
    return;
  if(!I->imports)
    I->imports=iluparser_new_list();
  if(list_find(I->imports,find_imported,name))
    return;
  imp=(Imported)iluparser_Malloc(sizeof(*imp));
  imp->name=name;
  imp->filename=filename;
  list_insert(I->imports,imp);
}

static void
definition_makeisl(refany definition,refany rock)
{
  IDLDefinition d=definition;
  switch(d->tag){
    /* those should not happen */
  case NILtag:case TYPELISTtag:case MEMBERLISTtag:case ATTRLISTtag:
    break;
    /* those don't have ISL */
  case INTERFACEFWDtag:
    break;
  case MODULEtag:
    module_makeisl(d);
    break;
  case INTERFACEtag:
    interface_makeisl(d);
    break;
  case EXCEPTIONtag:
    exception_makeisl(d);
    break;
  case CONSTtag:
    const_makeisl(d);
    break;
  case ENUMVALtag:
    break;
  case OPERATIONtag:
    operation_makeisl(d);
    break;
  case ATTRIBUTEtag:
    attribute_makeisl(d);
    break;
  case TYPEtag:
    typedef_makeisl(d);
    break;
  case MEMBERtag:
    member_makeisl(d);
    break;
  case CASEtag:
    case_makeisl(d);
    break;
  case PARAMETERtag:
    parameter_makeisl(d);
    break;
  case PRAGMA_PREFIXtag:
  case PRAGMA_VERSIONtag:
  case PRAGMA_IDtag:
    break;
  }
}

static void
module_makeisl(IDLDefinition d)
{
  Interface new;
  if(d->env==NULL){
    /* toplevel module */
    d->isl=new=(Interface)iluparser_Malloc(sizeof(struct ilu_interface_s));
    new->name=new_Name();
    name_set_base_name(new->name,d->name->lifted);
    new->def=d->name->line;
    new->types=(set)iluparser_new_list();
    new->classes=(set)iluparser_new_list();
    new->imports=iluparser_new_list();
    new->idirectives=iluparser_new_list();
    /* make sure the module knows about the ilu INTERFACE */
    add_import(d,"ilu",NULL);
    new->exceptions=(set)iluparser_new_list();
    new->constants=(set)iluparser_new_list();
    new->brand=NULL;
    new->def=0;
  }
  if(d->u.module.definitions)
    list_enumerate(d->u.module.definitions,definition_makeisl,d);
}

static void
interface_makeisl(IDLDefinition d)
{
  TypeDescription new;
  Type isl;
  new=new_TypeDescription();
  new->type=object_Type;
  new->structuredDes.object=iluparser_Malloc(sizeof(struct ilu_class_s));
  new->structuredDes.object->brand = NULL;
  new->structuredDes.object->corba_rep_id = d->id;
  new->structuredDes.object->collectible = FALSE;
  /* IDL interfaces are always optional */
  new->structuredDes.object->optional = TRUE;
  new->structuredDes.object->singleton = NULL;
  new->structuredDes.object->superclasses = NULL;
  new->structuredDes.object->authentication = NULL;
  new->structuredDes.object->methods = NULL;
  new->structuredDes.object->doc_string = NULL;

  d->isl=isl=new_Type();
  isl->def=d->name->line;
  definition_setscoping(d,isl->scoping);

  isl->description=new;
  isl->interface=toplevel_module(d)->isl;
  /* ISL has the types in reverse order. In order to generate the
     same stub code, items are inserted at front */
  list_push(isl->interface->types,isl);
  /* FixupInterface will add them to the classes list later on
  list_push(isl->interface->classes,isl);*/
  name_set_base_name(isl->name,d->name->lifted);
  
  assert(d->u.interface.definitions);
  list_enumerate(d->u.interface.definitions,definition_makeisl,d);
} 

static void
operation_makeisl(IDLDefinition d)
{
  Class C;
  Procedure new=new_Procedure();
  assert(d->name->scope==NULL);
  d->isl=new;
  name_set_base_name(new->name,d->name->lifted);
  new->def=d->name->line;
  /* IDL operations are never functional */
  new->functional = FALSE;
  new->asynch = d->u.operation.oneway;
  new->interface = toplevel_module(d)->isl;
  /* add parameters */
  new->arguments=iluparser_new_list();
  list_enumerate(d->u.operation.parameters,definition_makeisl,0);
  type_makeisl(d->u.operation.returntype,d);
  /* link with interface/CLASS */
  new->object = up_find_definition(d,INTERFACEtag)->isl;
  C=new->object->description->structuredDes.object;
  if(!C->methods)
    C->methods=iluparser_new_list();
  list_insert(C->methods,new);
  /* FIXME: context */
}

static void
fill_array_dimensions(refany val,refany list)
{
  IDLValue v=val;
  assert(v->tag==idl_long);
  /* Hmm. Casting integers to pointers */
  list_insert(list,(refany)(v->u.INT));
}

static void
enum_makeisl(refany def,refany list)
{
  IDLDefinition d=def;
  EnumField f=iluparser_Malloc(sizeof(struct enumerationField_s));
  f->name=d->name->lifted;
  f->id=-1;
  list_insert(list,f);
}

static void
typedef_makeisl(IDLDefinition d)
{
  Type T;
  TypeDescription D;
  IDLType t;
  list l;
  t=d->u.type;
  d->isl=T=new_Type();
  if(!t->isl)
    t->isl=d->isl;
  T->description=D=new_TypeDescription();
  assert(d->name->name);
  assert(d->name->array==0);
  name_set_base_name(T->name,d->name->lifted);
  definition_setscoping(d,T->scoping);
  T->def = d->name->line;
  T->interface = toplevel_module(d)->isl;
  list_push(T->interface->types,T);
  switch(t->tag){
    /* those should not happen */
  case NULLTYPEtag:case REFERENCEDtag:case BASICtag:
    break;
  case STRUCTtag:
    D->type=record_Type;
    D->structuredDes.record.fields=iluparser_new_list();
    D->structuredDes.record.extensible=0;
    D->structuredDes.record.supertype=(Type)0;
    list_enumerate(t->u.structure,definition_makeisl,0);
    break;
  case ENUMtag:
    D->type=enumeration_Type;
    D->structuredDes.enumeration=l=iluparser_new_list();
    list_enumerate(t->u.enumerated,enum_makeisl,l);
    break;
  case UNIONtag:
    D->type=union_Type;
    D->structuredDes.uniond.discriminator_type=0;
    D->structuredDes.uniond.types=iluparser_new_list();
    D->structuredDes.uniond.default_arm=NULL;
    /* In CORBA, all values of the switch type are valid,
       even if they are not specified in the cases - 
       unless there is a default arm */
    D->structuredDes.uniond.others_allowed=TRUE;
    type_makeisl (t->u._union.head, d);
    list_enumerate(t->u._union.body,definition_makeisl,0);
    break;
  case ARRAYtag:
    D->type=array_Type;
    D->structuredDes.array.type=0; /* update later */
    D->structuredDes.array.dimensions=l=iluparser_new_list();
    list_enumerate(t->u.array.dimensions,fill_array_dimensions,l);
    D->structuredDes.array.optional=FALSE;
    type_makeisl(t->u.array.type,d);
    /* for the dimensions, no special isl is necessary */
    break;
  case STRINGtag:
    if(t->u.stringsize){
      D->type=sequence_Type;
      D->structuredDes.sequence.type=the_char_t->isl;
      D->structuredDes.sequence.optional=TRUE;
      D->structuredDes.sequence.limit=t->u.stringsize->u.INT;
    }else{
      /* alias for ilu.CString */
      T->description=NULL;
      iluparser_Free(D);
      T->supertype=t->isl;
      /*name_set_lang_name(T->name,"import","CString");*/
    }
    break;
  case WSTRINGtag:
    D->type=sequence_Type;
    D->structuredDes.sequence.type=the_wchar_t->isl;
    D->structuredDes.sequence.optional=TRUE;
    if(t->u.stringsize)
      D->structuredDes.sequence.limit=t->u.stringsize->u.INT;
    else
      D->structuredDes.sequence.limit=0;
    break;
  case SEQUENCEtag:
    D->type=sequence_Type;
    D->structuredDes.sequence.type=0; /* update later */
    D->structuredDes.sequence.optional=TRUE;
    if(t->u.sequence.size)
      D->structuredDes.sequence.limit=t->u.sequence.size->u.INT;
    else
      D->structuredDes.sequence.limit=0;
    type_makeisl(t->u.sequence.type,d);
    break;
  case FIXEDtag:
    D->type = fixedpoint_Type;
    D->structuredDes.fixed.min_numerator = fixedPow (-t->u.fixed.digits->u.INT);
    D->structuredDes.fixed.max_numerator = fixedPow (t->u.fixed.digits->u.INT);
    D->structuredDes.fixed.denominator = fixedPow (t->u.fixed.exponent->u.INT);
    break;
  case ALIAStag:
    T->description=NULL;
    iluparser_Free(D);
    /* need to make isl for referenced types as well
       if(is_anonymous(t->u.alias)) */
    type_makeisl(t->u.alias,d);
    break;
  case DEFINEDtag:
    /* this is an alias type */
    T->description=NULL;
    iluparser_Free(D);
    definition_makeisl(t->u.defined,0);
    break;
  case NATIVEtag:
    idl_name_error(d->name,"native types not supported, yet");
    break;
  }
}

static void 
member_makeisl(IDLDefinition d)
{
  Argument A=new_Argument();
  Type T;
  name_set_base_name(A->name,d->name->lifted);
  A->def=d->name->line;
  assert(d->isl==0);
  d->isl=A;
  switch(d->env->tag){
  case TYPEtag:
    /* record type */
    T=d->env->isl;
    break;
  case EXCEPTIONtag:
    /* pseudo record for exceptions */
    T=((Exception)d->env->isl)->type;
    break;
  default:
    idl_name_error(d->name,"unexpected scope for member");
  }
  list_insert(T->description->structuredDes.record.fields,A);
  type_makeisl(d->u.type,d);
}

static void
parameter_makeisl(IDLDefinition d)
{
  Argument A=new_Argument();
  Procedure P=d->env->isl;

  A->direction=d->u.parameter.direction;
  name_set_base_name(A->name,d->name->lifted);
  A->def=d->name->line;
  type_makeisl(d->u.parameter.type,d);
  /* this might not succeed if the type is referenced */
  list_insert(P->arguments,A);
  d->isl=A;
}

/* anonymous type */
static char*
new_anon_type()
{
  static int anon_counter=0;
  return aprintf("AnonType-%d-",++anon_counter);
}

static void
anon_type(IDLType p,IDLDefinition env)
{
  IDLDefinition d=new_definition();
  d->name=p->name=new_name();
  /* name and lifted name are identical (no scoping, no underscores) */
  d->name->lifted=d->name->name=new_anon_type();
  d->name->file=env->name->file;
  d->name->line=env->name->line;
  d->tag=TYPEtag;
  d->env=env;
  d->u.type=p;
  p->anon_def=d;
  definition_makeisl(d,0);
  p->isl=d->isl;
}

static void
type_makeisl(IDLType p,IDLDefinition env)
{
  IDLDefinition d;
  Type T;
  if(p->isl)return;
  switch(p->tag){
    /* those should not happen */
  case NULLTYPEtag:case STRUCTtag:case ENUMtag:case UNIONtag:
    idl_name_error(NULL,"Unexpected type");
    break;
  case BASICtag:
    /* no isl needs to be make here */
    break;
  case REFERENCEDtag:
    /* have to wait until after makeisl to locate the correct isl
       if this is an imported type, make the subtype */
    if(toplevel_module(env)->isl
       !=toplevel_module(p->u.referenced.name->value)->isl){
      Interface I;
      d=p->u.referenced.name->value;
      p->isl=T=new_Type();
      name_set_base_name(T->name, d->name->lifted);
      name_set_lang_name(T->name, "import", d->name->lifted);
      I=toplevel_module(env)->isl;
      list_push(I->types,T);
      /* supertype, importInterfaceName and interface added later */
    }
    break;
  case ALIAStag:
    type_makeisl(p->u.alias,env);
    break;
  case DEFINEDtag:
    /* nothing to do here: the struct/union definition 
       is in the defition list */
    break;
  case SEQUENCEtag:
    if(!p->name)
      anon_type(p,env);
    type_makeisl(p->u.sequence.type,env);
    break;
  case ARRAYtag:
    if(!p->name)
      anon_type(p,env);
    type_makeisl(p->u.array.type,env);
    break;
  case WSTRINGtag:
    anon_type(p,env);
    type_makeisl(the_wchar_t,env);
    break;
  case STRINGtag:
    if(!p->name && p->u.stringsize){
      anon_type(p,env);
      type_makeisl(the_char_t,env);
    }
    break;
  case FIXEDtag:
    if(!p->name)
      anon_type(p,env);
    break;
  case NATIVEtag:
    break;
  }
}

static void
exception_makeisl(IDLDefinition d)
{
  Exception E=new_Exception();
  d->isl=E;
  name_set_base_name(E->name,d->name->lifted);
  definition_setscoping(d,E->scoping);
  E->def=d->name->line;
  E->interface=toplevel_module(d)->isl;
  list_insert(E->interface->exceptions,E);
  /* if this has parameters, set type to a RECORD */
  if(d->u.exception.members){
    Type T;
    TypeDescription D;
    int i;
    E->type=T=new_Type();
    T->description=D=new_TypeDescription();
    name_set_base_name(T->name,aprintf("ilu--prefix-idlExceptionType-%s",
				       d->name->lifted));
    T->scoping=new_list();
    for (i=0;i<list_size(E->scoping)-1;i++)
      list_insert(T->scoping,list_ref(E->scoping,i));
    list_insert(T->scoping,name_base_name(T->name));
    T->def=d->name->line;
    T->interface=toplevel_module(d)->isl;
    list_push(T->interface->types,T);
    D->type=record_Type;
    D->structuredDes.record.fields=iluparser_new_list();
    D->structuredDes.record.extensible=0;
    D->structuredDes.record.supertype=(Type)0;
    list_enumerate(d->u.exception.members,definition_makeisl,0);
  }
}

static void
caselabel_makeisl(refany Case,refany arg)
{
  IDLCase c=Case;
  Argument A=arg;
  if(c->value){
    value_makeisl(c->value);
    c->isl=c->value->isl;
    list_insert(A->values,c->isl);
  }else{
    /* default: */
    IDLDefinition d=c->env->env;
    Type T;
    assert(d && d->tag==TYPEtag && d->u.type->tag==UNIONtag);
    T=d->isl;
    T->description->structuredDes.uniond.default_arm=A;
    T->description->structuredDes.uniond.others_allowed=FALSE;
  }
}

static void
case_makeisl(IDLDefinition d)
{
  Type T;
  Argument A=d->isl=new_Argument();
  name_set_base_name(A->name,d->name->lifted);
  A->values=iluparser_new_list();
  A->def=d->name->line;
  type_makeisl(d->u._case.type,d);
  /* insert labels */
  A->values=iluparser_new_list();
  list_enumerate(d->u._case.labels,caselabel_makeisl,A);
  /* insert case into union */
  T=d->env->isl;
  list_insert(T->description->structuredDes.uniond.types,A);
}

static void
value_makeisl(IDLValue v)
{
  ConstantValue V;
  boolean isint=FALSE,issigned=FALSE;
  if(v->isl)return;
  V=v->isl=iluparser_Malloc(sizeof(struct ilu_constantvalue_s));
  switch(v->tag){
    /* those should not happen */
  case idl_void:case idl_unary:case idl_binary:case idl_named:case idl_any:
  case idl_object:
    idl_name_error(0,"unexpected value in makeisl");
    break;
    /* in the ISL, they are all integer_Type */
  case idl_short:
  case idl_long:
  case idl_long_long:
    isint=TRUE;issigned=TRUE;break;
  case idl_octet:
  case idl_unsigned_short:
  case idl_unsigned_long:
  case idl_unsigned_long_long:
    isint=TRUE;break;
  case idl_string:
    V->type=shortcharacter_Type;
    V->val.s=v->u.string;
    break;
  case idl_enum:
    V->type=shortcharacter_Type;
    V->val.s=v->u.enumerated.name->lifted;
    break;
  case idl_float:case idl_double:
    V->type=real_Type;
    if(v->u.FLOAT.integer || v->u.FLOAT.fraction){
      V->val.r.sign=v->u.FLOAT.sign;
      V->val.r.value=v->u.FLOAT.integer;
      V->val.r.fraction=v->u.FLOAT.fraction;
      V->val.r.exponent=v->u.FLOAT.exponent;
    }else{
      /* FIXME: What is the precision for double? */
      char *buf=aprintf("%.17E",v->u.FLOAT.val);
      char *p,*p1;
      p=buf;
      if(*p=='-'){
	V->val.r.sign=-1;
	p++;
      }else V->val.r.sign=1;
      p1=strchr(p,'.');
      V->val.r.value=strndup(p,p1-p);
      p=p1+1;
      p1=strchr(p,'E');
      V->val.r.fraction=strndup(p,p1-p);
      V->val.r.exponent=strtol(p1+1,0,10);
      free(buf);
    }
    break;
  case idl_char:
    /* FIXME: should use character arm */
    V->type=shortinteger_Type;
    V->val.i.sign=1;
    V->val.i.value=v->u.CHAR;
    break;
  case idl_wchar:
    V->type=shortinteger_Type;
    V->val.i.sign=1;
    V->val.i.value=v->u.CHAR;
    break;
  case idl_boolean:
    V->type=boolean_Type;
    V->val.b=v->u.BOOL;
    break;
  case idl_int:
    idl_name_error(0,"Untyped integer");
    break;
  case idl_fixed:
    V->type=fixedpoint_Type;
    break;
  }
  if(isint){
    V->type=integer_Type;
    if(issigned){
      V->val.i.sign=(v->u.INT<0)?-1:1;
      V->val.i.value=(v->u.INT<0)?-v->u.INT:v->u.INT;
    }else{
      V->val.i.sign=1;
      V->val.i.value=v->u.INT;
    }
  }
}

static void
const_makeisl(IDLDefinition d)
{
  Constant C=iluparser_Malloc(sizeof(struct ilu_constant_s));
  d->isl=C;
  C->name=new_Name();
  C->scoping = iluparser_new_list();
  definition_setscoping(d,C->scoping);
  name_set_base_name(C->name,d->name->lifted);
  C->def=d->name->line;
  C->interface=toplevel_module(d)->isl;
  list_insert(C->interface->constants,C);
  C->type=0;
  C->importInterfaceName=0;
  C->import=0;
  /* if this is a fixed point type, this might be the only place
     where the type is referenced */
  type_makeisl(d->u.constant.type, d);
  value_makeisl(d->u.constant.val);
  C->value=d->u.constant.val->isl;
}

static Procedure
attr_get_set(IDLDefinition d,boolean get)
{
  /* see operation_makeisl */
  Class C;
  Procedure new=new_Procedure();
  name_set_base_name(new->name,aprintf("ilu--prefix-idlAttribute--%s-%s",
				       get?"get":"set",d->name->lifted));
  new->def=d->name->line;
  new->functional=FALSE;
  new->asynch=FALSE;
  new->interface=toplevel_module(d)->isl;
  new->arguments=iluparser_new_list();
  if(get){
    /* add return type later */
  }else{
    Argument A=new_Argument();
    name_set_base_name(A->name,"value");
    A->direction=In;
    A->def=d->name->line;
    /* add parameter type later */
    list_insert(new->arguments,A);
  }
  type_makeisl(d->u.attribute.type,d);
  new->object = up_find_definition(d,INTERFACEtag)->isl;
  C=new->object->description->structuredDes.object;
  if(!C->methods)C->methods=iluparser_new_list();
  list_insert(C->methods,new);
  return new;
}
static void
attribute_makeisl(IDLDefinition d)
{
  /* make pseudo-operations */
  d->isl=attr_get_set(d,TRUE);
  if(!d->u.attribute.readonly)
    d->u.attribute.set=attr_get_set(d,FALSE);
}

/************************* update pass       *************************/

static void definition_update(refany def,refany rock);
static void typedef_update(IDLDefinition d);

static void
interface_insertsuper(refany name,refany def)
{
  IDLName n=name;
  IDLDefinition d=def;
  Type T=d->isl;
  Class C=T->description->structuredDes.object;
  IDLDefinition super=n->value;

  assert(super);
  if(toplevel_module(d)==toplevel_module(super)){
    /* insert ISL supertype into ISL superclasses list */
    list_insert(C->superclasses,super->isl);
  }else{
    /* make subtype */
    IDLDefinition m1,m2;
    Type T=new_Type();
    name_set_base_name(T->name,super->name->lifted);
    name_set_lang_name(T->name,"import",super->name->lifted);
    m1=toplevel_module(d);
    m2=toplevel_module(super);
    T->importInterfaceName=m2->name->lifted;
    /* import INTERFACE of supertype into this INTERFACE */
    add_import(m1,m2->name->lifted,m2->name->file);
    list_insert(((Interface)m1->isl)->types,T);
    list_insert(C->superclasses,T);
  }
}

static void
method_assignid(refany method,refany rock)
{
  Procedure p=method;
  int* pid=rock;
  p->id= ++*pid;
}

static void
interface_update(IDLDefinition d)
{
  Type T;
  Class C;
  int id=0;
  T=d->isl;
  C=T->description->structuredDes.object;
  assert(C->superclasses==NULL);
  C->superclasses=iluparser_new_list();
  if(d->u.interface.bases){
    list_enumerate(d->u.interface.bases,interface_insertsuper,d);
  }else{
    /* implicit superclass ilu.CORBA-Object */
    list_insert(C->superclasses,the_CORBA_Object);
  }
  list_enumerate(d->u.interface.definitions,definition_update,0);
  /* FIXME: should really call ilu.bison->AssignMethodIDs here */
  list_enumerate(C->methods,method_assignid,&id);
  /* assign repository IDs */
}

static void
type_update(IDLType t)
{
  IDLDefinition d;
  Type T;
  switch(t->tag){
    /* those should not happen */
  case NULLTYPEtag:case STRUCTtag:case ENUMtag:case UNIONtag:case NATIVEtag:
    idl_name_error(0,"unexpected case in type_update");
    break;
    /* nothing to be done */
  case BASICtag:
  case WSTRINGtag:
  case STRINGtag:
  case FIXEDtag:
    break;
  case REFERENCEDtag:
    {
      IDLDefinition m1,m2;
      d=t->u.referenced.name->value;
      assert(d && (d->tag==TYPEtag ||d->tag==INTERFACEtag));
      if(d->tag==TYPEtag)
	typedef_update(d);
      m1=toplevel_module(t->u.referenced.name->env);
      m2=toplevel_module(t->u.referenced.name->value);
      if(m1!=m2){
	/* imported type */
	T=t->isl;
	T->supertype=d->isl;
	T->importInterfaceName=m2->name->lifted;
	T->interface=m2->isl;
	add_import(m1,m2->name->lifted,m2->name->file);
      }else
	t->isl=d->isl;
    }
    break;
  case ALIAStag:
    if(t->isl)break;
    type_update(t->u.alias);
    t->isl=t->u.alias->isl;
    break;
  case DEFINEDtag:
    definition_update(t->u.defined,0);
    t->isl=t->u.defined->isl;
    break;
  case SEQUENCEtag:
  case ARRAYtag:
    if(t->anon_def)
      definition_update(t->anon_def,0);
    break;
  }
}

static void
parameter_update(refany param)
{
  IDLDefinition p=param;
  Argument A=p->isl;
  assert(p->tag==PARAMETERtag);
  assert(A);
  type_update(p->u.parameter.type);
  A->type=p->u.parameter.type->isl;
}

static boolean
  compare_imported_exception (refany element, refany rock)
{
  Exception e1 = (Exception) element;
  Exception e2 = (Exception) rock;
  return (e1->importInterfaceName != NULL) && (e1->import == e2);
}

static void
exc_update(refany exc,refany list)
{
  IDLName n=exc;	/* the name of an exception in a raises list */
  IDLDefinition d=n->value;	/* the def of the exception named */
  IDLDefinition m1,m2;
  Interface I;

  assert(d && d->isl);
  m1=toplevel_module(n->env);	/* the Interface in which the exception is referenced */
  m2=toplevel_module(d);	/* the Interface in which the exception is defined */
  if(m1!=m2) {
    add_import(m1,m2->name->lifted,m2->name->file);
    I=m1->isl;
    if (!list_find(I->exceptions,compare_imported_exception,d->isl)) {
      /* make sure exception is in interfaces's list of all used exceptions */
      Exception E1 = (Exception) d->isl;
      Exception E2 = new_Exception();
      *E2=*E1;
      E2->importInterfaceName=interface_name(E1->interface);
      E2->import = E1;
      E2->interface = I;
      list_insert(I->exceptions,E2);
    }
  }
  list_insert(list,d->isl);
}

static void
operation_update(IDLDefinition d)
{
  Procedure P=d->isl;

  assert(d->u.operation.returntype);
  type_update(d->u.operation.returntype);
  assert(P);
  P->returnType=d->u.operation.returntype->isl;
  list_enumerate(d->u.operation.parameters,definition_update,0);
  if(d->u.operation.raises){
    P->exceptions=iluparser_new_list();
    list_enumerate(d->u.operation.raises,exc_update,P->exceptions);
  }
  /* FIXME: context */
}

static void
typedef_update(IDLDefinition d)
{
  IDLType t=d->u.type;
  Type T=d->isl;
  /* The Type's uid indicates that this typedef is uptodate */
  if(T->uid)return;
  /* will be overwritten below #if idl2isl compatible*/
  T->uid=d->id;
  T->explicit_uid = (d->id != NULL);
  switch(t->tag){
    /* those should not happen */
  case NULLTYPEtag:case REFERENCEDtag:case NATIVEtag:
    idl_name_error(d->name,"Unexpected case in typedef_update");
    break;
  case WSTRINGtag:
  case STRINGtag:
  case BASICtag:
    /* this sets the uid for the basic type */
    type_update(t);
    break;
  case SEQUENCEtag:
    type_update(t->u.sequence.type);
    assert(t->u.sequence.type->isl);
    T->description->structuredDes.sequence.type=t->u.sequence.type->isl;
    break;
  case STRUCTtag:
    list_enumerate(t->u.structure,definition_update,0);
    break;
  case ENUMtag:
    break;
  case UNIONtag:
    type_update(t->u._union.head);
    T->description->structuredDes.uniond.discriminator_type=
      t->u._union.head->isl;
    list_enumerate(t->u._union.body,definition_update,0);
    break;
  case ARRAYtag:
    type_update(t->u.array.type);
    assert(t->u.array.type->isl);
    T->description->structuredDes.array.type=t->u.array.type->isl;
    break;
  case FIXEDtag:
    break;
  case ALIAStag:
    type_update(t->u.alias);
    T->supertype=t->u.alias->isl;
    break;
  case DEFINEDtag:
    definition_update(t->u.defined,0);
    assert(t->u.defined->isl);
    T->supertype=t->u.defined->isl;
    break;
  }
#if LOCAL_TYPES_HAVE_REP_IDS
#else
  T->uid=0;
  T->explicit_uid = FALSE;
  if(t->tag==SEQUENCEtag || t->tag==ARRAYtag 
     || t->tag==WSTRINGtag || (t->tag==STRINGtag && t->u.stringsize!=0))
    /* typedef sequence<t>, typedef long */
    FigureTypeUID(T);
  else {
    /* struct, union */
    T->uid=d->id;
    T->explicit_uid = (d->id != NULL);
  }
#endif
}

static void
member_update(IDLDefinition d)
{
  Argument A=d->isl;
  type_update(d->u.type);
  assert(d->u.type->isl);
  A->type=d->u.type->isl;
}

static void
exception_update(IDLDefinition d)
{
  Exception E=d->isl;
  Type T=E->type;
  list_enumerate(d->u.exception.members,definition_update,0);
  E->corba_rep_id=d->id;
  if(T)
    T->uid=E->corba_rep_id;
}

static void
const_update(IDLDefinition d)
{
  Constant C=d->isl;
  type_update(d->u.constant.type);
  C->type=d->u.constant.type->isl;
}

static void
attribute_update(IDLDefinition d)
{
  Procedure P;
  type_update(d->u.attribute.type);
  P=d->isl;
  P->returnType=d->u.attribute.type->isl;
  if(!d->u.attribute.readonly){
    Argument A;
    P=d->u.attribute.set;
    A=list_car(P->arguments);
    A->type=d->u.attribute.type->isl;
  }
}

static void
case_update(IDLDefinition d)
{
  Argument A=d->isl;
  type_update(d->u._case.type);
  A->type=d->u._case.type->isl;
}

static void
definition_update(refany def,refany rock)
{
  IDLDefinition d=def;
  switch(d->tag){
    /* those should not happen */
  case NILtag:case TYPELISTtag:case MEMBERLISTtag:case ATTRLISTtag:
    break;
  case MODULEtag:
    list_enumerate(d->u.module.definitions,definition_update,rock);
    break;
  case INTERFACEtag:
    interface_update(d);
    break;
  case CONSTtag:
    const_update(d);
    break;
  case ENUMVALtag:
    break;
  case EXCEPTIONtag:
    exception_update(d);
    break;
  case OPERATIONtag:
    operation_update(d);
    break;
  case ATTRIBUTEtag:
    attribute_update(d);
    break;
  case PARAMETERtag:
    parameter_update(d);
    break;
  case TYPEtag:
    typedef_update(d);
    break;
  case MEMBERtag:
    member_update(d);
    break;
  case CASEtag:
    case_update(d);
    break;
    /* nothing to do */
  case INTERFACEFWDtag:
  case PRAGMA_IDtag:
  case PRAGMA_PREFIXtag:
  case PRAGMA_VERSIONtag:
    break;
  }
}

static void
select_imported(refany def,refany parse)
{
  struct idl_parse *p=parse;
  IDLDefinition d=def;
  if(d->tag!=MODULEtag)
    /* #pragma */
    return;
  if(strcmp(d->name->file,p->file)==0)
    list_insert(p->defined_interfaces,d->isl);
  else
    list_insert(p->imported_interfaces,d->isl);
}

static void
IDL2ISL(struct idl_parse *P)
{

  list toplevel_prefix;
  /* join modules for re-opening */
  P->definitions=reopen_modules(P->definitions);
  /* backlink, toplevel has no parent */
  list_enumerate(P->definitions,definition_backlink,0);
  /* resolve all names */
  list_enumerate(P->definitions,definition_resolvenames,P->definitions);
  /* perform consistency checks, compute constants */
  list_enumerate(P->definitions,definition_check,P->definitions);
  /* assign repository IDs */
  toplevel_prefix = iluparser_new_list ();
  list_push (toplevel_prefix, "");
  list_enumerate (P->definitions, definition_setuid, toplevel_prefix);
  /* put everything in a module */
  make_toplevel(P);
  /* lift local types etc. to module level */
  list_enumerate(P->definitions,definition_lift,0);
  /* attach an ISL structure everywhere */
  list_enumerate(P->definitions,definition_makeisl,0);
  /* update ISL */
  list_enumerate(P->definitions,definition_update,0);
  /* select modules of toplevel source file */
  list_enumerate(P->definitions,select_imported,P);
}

list ParseIDLFile (struct idl_parse* P)
{
  int stat;
  extern FILE* idlin;

  extern int idl_flex_debug;
  static int initial=1;
  /* default: debugging is off */
  idl_flex_debug=0;
  if(initial){
    if (!init_types())
      return 0;
    initial=0;
  }else{
    fprintf(stderr,"ParseIDLFile invoked twice (for %s)\n",P->file);
    return 0;
  }
  if ((idlin = fopen(P->file, "r")) == NULL)
    {
      fprintf (stderr, "ParseFile:  Error opening file \"%s\" for read.\n", P->file);
      return (NULL);
    }
  idlsetinitialfile(P->file);
  stat = idlparse();

  if(stat)return 0;

  P->defined_interfaces=iluparser_new_list();
  P->imported_interfaces=iluparser_new_list();
  P->definitions=the_result;
  IDL2ISL(P);

  return P->defined_interfaces;
}

/* this should go into pathname.c */
char*
iluparser_basename(char* fullname)
{
  char *result,*h;
#if defined(WIN32) || defined(WIN16)
  for(result=fullname;(h=strpbrk(result,"/\\:"));result=h+1)
    /*nothing*/;
#elif defined(macintosh)
  for(result=fullname;(h=strchr(result,":"));result=h+1)
    ;
#else
  for(result=fullname;(h=strchr(result,'/'));result=h+1)
    ;
#endif
  return ilu_strdup(result);
}
