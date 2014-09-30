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

#include <stddef.h>
#include <assert.h>
#include <stdio.h>
#include <setjmp.h>
#include <string.h>
#include <stdlib.h>
#include "data.h" 
#include "rules.h"
#include "iluptype.h"

jmp_buf GenerateExitEnv;

typedef struct { 
                int  mapped;
                int  typedesc_index;   
} StructCorr; 

#define SIZE_TNV 30000
typedef struct {
        void       *stub_ctl;             /* for comm with stubber */ 
        char       *tree_node_vector;     /* for failed instances  */
        int         current_set_node;  
        int         last_search_node;  
        Rule       *current_rule;      /* for error messages */
        Function   *current_function;  /* during function call*/ 
        StructCorr *struct_ref_map;
} GCtl;


struct GenTree_ {
        struct GenTree_ *next_sibling;
        struct GenTree_ *first_child;
        int              search_node; 
        Rule            *rule;  /* for this node */ 
        Instance        *leaf_args;  /* only for leaf */
        PropDesc        *leaf_arg_desc; 
};
typedef struct GenTree_ GenTree;

extern TypeDesc ST_TYPES[];
extern Instance *new_instance();

Scope    *Scopes;
Rule     *Rules;
Line     *Lines;
RuleNode *RuleNodes;
Edge     *RuleEdges;
PropDesc *PropDescs;
PropDesc *FunPropDescs;
CExp     *CExps;
Function *UFunctions;

PropDesc LIT_INT_DESC =    { "LIT_INT", PropInt, NULL, 0};
PropDesc LIT_STRING_DESC = { "LIT_STRING", PropString, NULL, 0};
PropDesc LIT_NULL_DESC =    { "LIT_NULL", PropNull, NULL, 0};
PropDesc LIT_OPAQUE_DESC  = { "LIT_OPAQUE", PropOpaque, NULL, 0};
PropDesc LIT_LIST_DESC    = { "LIT_LIST", PropList, NULL, 0};
PropDesc LIT_LONGINT_DESC  = { "LIT_LONGINT", PropLongInt, NULL, 0};
PropDesc LIT_STRUCT_DESC  = { "LIT_STRUCT", PropStruct, NULL, 0};

int interpret_rule(GCtl *ctl, int category_index, PropDesc *arg_desc, 
                   Instance *args, GenTree **result_subtree);
void get_current_vs(GCtl *ctl, Instance **instance);
void *referenced_value(Ref *ref);  
int comparevalues(GCtl *ctl, Rule *rule, Ref *ref1, Ref *ref2);
int find_arg_propdesc(GCtl *ctl, Rule *rule,
                      char *struct_type, int use_mapindex, int map_index); 
int find_property(GCtl *ctl, PropDesc firstdesc[],char *name,Ref *out_ref);
int get_symbol_ref(GCtl *ctl, Rule *rule,
                 TypeDesc *ruledesc, Instance *ruleinst, 
                 CExp *constr, Ref *out_ref); 
static int find_rule_path(GCtl *ctl, Rule *rule, RuleNode *cur_node,
              TypeDesc *ruledesc, Instance *ruleinst,
              GenTree **result_node_chain);


/* Notes:
- MAKE ALL REFS INDIRIECT INCLUDING INTS.
-  VERIFY LOCALS  
*/


/************************************************************************/
/* DEBUG UTILITY                                                        */
/************************************************************************/
void print_cexp(GCtl *ctl, int constr_offset, int indent) {

    CExp *constr;

   /* no constraint */
    if(constr_offset == 0) {
       fprintf(stdout, " NONE");
       return;  
    }

   constr = &(CExps[constr_offset]); 

   fprintf(stdout, "\n %*s(", indent," "); 

   switch(constr->op) {  
      case op_unify:
          fprintf(stdout, "UNIFY ");   
          break;
      case op_eqp:
          fprintf(stdout,"EQ ");
          break;
      case op_neqp:
          fprintf(stdout,"NEQ ");
          break;
      case op_and:
          fprintf(stdout,"AND ");
          break;
      case op_or:
          fprintf(stdout,"OR ");
          break;
      case  op_name:
          fprintf(stdout,"NAME");
          break;
      case op_path: 
          fprintf(stdout,"PATH");
          break;
      case op_lit_int:
          fprintf(stdout,"LITINT ");
          break;
      case op_lit_bool:
          fprintf(stdout,"LITBOOL ");
          break;
      case op_lit_longint:   
          fprintf(stdout,"LITLONGINT");
          break;
      case op_lit_null:
          fprintf(stdout,"LITNULL ");
          break;
      case op_lit_string: 
          fprintf(stdout,"LITSTRING ");
          break;
      case op_fn:
          fprintf(stdout,"FN ");
          break;
      case op_arg:
          fprintf(stdout,"ARG ");
          break;
      default:
         fprintf(stdout,"U_OP");

   }

   switch(constr->op) {

       case op_unify:
       case op_eqp:
       case op_neqp:
       case op_and:
       case op_or:
       case op_path: 
       case op_arg:
           print_cexp(ctl, constr->opnd1, indent+4);
           print_cexp(ctl, constr->opnd2, indent+4);
           break;
       case op_fn:
           fprintf(stdout, " %d",(int)constr->literal);
           print_cexp(ctl, constr->opnd1, indent+4);
           break;
       case op_name:
           fprintf(stdout, " %s", (char *)constr->literal);
           break;
       case op_lit_int:
       case op_lit_bool:
           fprintf(stdout," %d ", (int)constr->literal);
           break;
       case op_lit_null:
           fprintf(stdout," NULL");
       case op_lit_string: 
           if(constr->literal != NULL) 
              fprintf(stdout," %s ", (char *)constr->literal);
           else
             fprintf(stdout," NULL");
           break;
       case op_lit_longint: 
           if(constr->literal != NULL) 
              fprintf(stdout," %ld ", *(long int*)constr->literal);
           else
             fprintf(stdout," NULL");
           break;
       default: 
            fprintf(stdout," UNKNOWN ");
 
     }

   fprintf(stdout, ")");
}

/************************************************************************/
typedef enum {gen_error_warning, gen_error} GenErrorType; 

void do_gen_error(GCtl *ctl, Rule *rule, 
                   GenErrorType errortype, char *message) {

  char *etype;

  switch (errortype) {
     case gen_error_warning:
          etype = "Stub Generator Warning";
          break;
     default:
          etype = "Stub Generator Error";
  }

   fprintf(stderr, "\n\n%s: ", etype); 
   if(rule != NULL) 
      fprintf(stderr, "Rule %s: ", rule->rulename); 
   fprintf(stderr,"%s. \n", message); 
  
   if (errortype == gen_error_warning) return; 

   fprintf(stderr,"\nCan not continue stub generation.\n");
   longjmp(GenerateExitEnv, 1);

}


/************************************************************************/
/* utilities for grammar_invoked functions                               */
/************************************************************************/

void get_property_ref(GCtl *ctl, Instance *instance,  
                     char *property, Ref *ref) {
 
   TypeDesc *typedesc;
   PropDesc *firstdesc;
   Instance *lclinstance;
   int       desc_index;
   char      msg[256];
 
   if(instance == NULL) {
      sprintf(msg, "Grammar function procedure %s (or containing call) "
                   "called from rule %s "
                   "attempting to find property %s from NULL instance",
                    ctl->current_function->function_name,
                    ctl->current_rule->rulename,
                    property);
      do_gen_error(ctl, NULL, gen_error, msg); 
   }
 
   lclinstance = instance;
   get_current_vs(ctl, &lclinstance);
   ref->container = lclinstance;
 
   /* find index in ST_TYPES */
   /* should add this in convert.. */
   desc_index = find_arg_propdesc(ctl, NULL, instance->typename,
               0, 0);
   
   typedesc = &(ST_TYPES[desc_index]);
   firstdesc = &(typedesc->propertydesc[0]);
 
   find_property(ctl, firstdesc, property, ref);
   return;
}
 
 
char *get_string_property_value(GCtl *ctl, Instance *instance, 
                     char *property) {
   Ref       outref;  
   char      msg[100];
 
   get_property_ref(ctl, instance, property, &outref); 
  
   if(outref.offset < 0 )  {
     sprintf(msg, "Grammar function procedure... property '%s' not found",
                   property);
     do_gen_error(ctl, NULL, gen_error, msg); 
  }
   if(outref.desc->propertykind != PropString) return NULL;
   return((char *)( (outref.container)->properties[outref.offset]));
}


int get_int_property_value(GCtl *ctl, Instance *instance, 
                     char *property) {
   Ref       outref;  
   char      msg[100];
 
   get_property_ref(ctl, instance, property, &outref); 
  
   if(outref.offset < 0 )  {
     sprintf(msg, "Grammar function procedure... property '%s' not found",
                   property);
     do_gen_error(ctl, NULL, gen_error, msg); 
  }
   if(outref.desc->propertykind != PropInt) return -1;
   return((int)( (outref.container)->properties[outref.offset]));
}


void *get_any_property_value(GCtl *ctl, Instance *instance, 
                     char *property) {
   Ref       outref;  
   char      msg[100];
 
   get_property_ref(ctl, instance, property, &outref); 
 
   if(outref.offset < 0) {
     sprintf(msg, "Grammar function procedure... property '%s' not found",
                   property);
     do_gen_error(ctl, NULL, gen_error, msg); 
   }
 
   return((void *)( (outref.container)->properties[outref.offset]));
}

void set_string_property_value(GCtl *ctl, Instance *instance,
                         char *property, char *newvalue) {
   Ref       outref;  
   char      msg[100];
 
   get_property_ref(ctl, instance, property, &outref); 
 
   if(outref.offset < 0) {
     sprintf(msg, "In set_string property_value, property '%s' not found",
                   property);
     do_gen_error(ctl, NULL, gen_error, msg); 
   }
   ((outref.container)->properties)[outref.offset] = (void *)newvalue;;
}
   



/************************************************************************/
static int value_set_p(GCtl *ctl, Ref *ref) {

     Instance *instance;
     if(ref->container == 0) { /*literal*/ 
         return 1;
     }
     instance = ref->container;
     if(instance->prop_set[ref->offset]==0) return 1;
     else return 0;
}

static void set_value_set(GCtl *ctl, Ref *ref) {

     Instance *instance;
     instance = ref->container;
     instance->prop_set[ref->offset] = (char)0;
}

static void set_value_unset(GCtl *ctl, Ref *ref) {

     Instance *instance;
     instance = ref->container;
     instance->prop_set[ref->offset] = (char)1;
}


/**********************************************************************/
/* get actual value implied by ref structure */ 
void *referenced_value(Ref *ref) {  

  /* if ref is literal */
  if(ref->container == NULL) 
      return ref->loc;
  else
    return *(void **)(ref->loc);
}


/************************************************************************/

static GenTree *new_subtree() {
         return (void *)calloc(1, sizeof(GenTree));
}


/************************************************************************/
static void append_to_chain(GCtl *ctl, GenTree **result_node_chain,
                     GenTree *new_chain) {

  GenTree *cur_chain;

  if(*result_node_chain == NULL) 
     *result_node_chain = new_chain;
  else {
     cur_chain = *result_node_chain; 
     while (cur_chain ->next_sibling != NULL)
           cur_chain = cur_chain->next_sibling; 
     cur_chain->next_sibling = new_chain;   
  }  
}


/************************************************************************/
int find_property(GCtl     *ctl,
                 PropDesc  firstdesc[],
                 char     *name,
                 Ref      *out_ref) {
  int i;

  out_ref->offset = -1;
  /* (MAXPROPERTIES JUST TO AVOID ENDLESS SEARCH ON SYSTEM ERROR */
  for (i = 0; i< MAXPROPERTIES; i++) { 

      if(!strcmp(firstdesc[i].propertyname, name)) {
           out_ref->desc = &(firstdesc[i]);
           out_ref->offset = i;
           break;
      }
     if(firstdesc[i].propertykind == PropLast)
            return 0; 
  }
  return 1;
}


/************************************************************************/
Instance *get_last_vs(GCtl *ctl, Instance *instance) { 

   Instance *this_instance;

   this_instance = instance;
   while(this_instance->next_vs != NULL)   
         this_instance = this_instance->next_vs;
   return this_instance;
}


/************************************************************************/
/* Implements currency scheme. Instance structures chained forward and  */
/* backward and labelled with (gen) search tree node index where set    */
/* in depth first order.  Find latest instance s.t. its index           */
/* not in vector of failed indexes                                      */
void get_current_vs(GCtl *ctl, Instance **instance) {

  Instance *good_instance, *latest_instance;
  
  /* search forward then backward */
  good_instance = NULL; 
  latest_instance = *instance;
  while ( latest_instance) {
     if( ctl->tree_node_vector[latest_instance->set_node] !=1 ) 
         good_instance = latest_instance;
     latest_instance = latest_instance->next_vs;
   }
  if(good_instance == NULL) {
     latest_instance = (*instance)->prev_vs; /* must be one? */
     while ( latest_instance) {
       if( ctl->tree_node_vector[latest_instance->set_node] !=1 ) { 
          good_instance = latest_instance;
          break;
       }
     }
     assert(good_instance != NULL); /*cant all fail? */  
  }

  *instance = good_instance;
   return;

  }


/************************************************************************/
/* should do search once only for each struct type                    */
int find_arg_propdesc(GCtl *ctl, Rule *rule,
                      char *struct_type, int use_mapindex, int map_index) { 
    int i;
    char msg[100]; 

  if(use_mapindex) {
    if(ctl->struct_ref_map[map_index].mapped) 
         return (ctl->struct_ref_map[map_index].typedesc_index);
   }


    for (i = 0; strcmp(ST_TYPES[i].typename,"LastTypeDesc"); i++) {
         if(!strcmp(ST_TYPES[i].typename, struct_type)) {    
              ctl->struct_ref_map[map_index].mapped = 1;
              ctl->struct_ref_map[map_index].typedesc_index = i;
              return i;
          }
    }
     sprintf(msg,
        "\nStructure type %s not found in ST_TYPES.", struct_type);
     /* does not return*/
     do_gen_error(ctl, rule, gen_error, msg);

     return 0; /* SHOULD NEVER GET HERE*/
}            

/**********************************************************************/
/* Get ref described by entry ref (partial symbol path resolution)    */
/* and one additional qualifier                                       */
int get_name_ref(GCtl *ctl, Rule *rule, Ref *cur_ref,
                char *name, Ref *out_ref) { 
 
  int        typeindex;
  TypeDesc  *typedesc;
  PropDesc  *firstdesc;
  Instance  *refinstance;  
  char       msg[100];

 
  switch(cur_ref->desc->propertykind) { 
     case PropLocals:
          firstdesc = &(PropDescs[rule->local_arg_desc_offset]); 
          refinstance =
            (Instance *)(cur_ref->container->properties[cur_ref->offset]);
          /* get latest instance of structure that is still ok */
          get_current_vs(ctl, &refinstance); 
          break;
     case PropArgs:
          firstdesc = &(PropDescs[rule->ext_arg_desc_offset]); 
          refinstance = 
            (Instance *)(cur_ref->container->properties[cur_ref->offset]);
          get_current_vs(ctl, &refinstance); 
          break;
     case PropStruct:
          typeindex = cur_ref->desc->typeindex;
          /* can't go down path here */

           /* indicates local or arg with locally unmapped type */ 
          if(typeindex < 0) {
              typeindex = find_arg_propdesc(ctl, rule, 
                      cur_ref->desc->typename, 1,  - (typeindex));
             cur_ref->desc->typeindex = typeindex;
             if(typeindex < 0) assert(0); 
          } 
          /* if typeindex is 0... Any */
          
          typedesc = &(ST_TYPES[typeindex]);  
          firstdesc = &(typedesc->propertydesc[0]);

          refinstance =
            (Instance *)(cur_ref->container->properties[cur_ref->offset]);
          get_current_vs(ctl, &refinstance); 

          /* if "any" type, look at actual instance */
          if(typeindex == 0) { /* Any */
             typeindex = refinstance->typeindex;  
             assert(typeindex >= 0);
             typedesc = &(ST_TYPES[typeindex]);
             firstdesc = &(typedesc->propertydesc[0]);
           }
          break; 
     default:  
         sprintf(msg, "Qual error; probably attempt to take"
             "property '%s' of string", name);   
         do_gen_error(ctl, rule, gen_error, msg); 
   } /* end switch */ 

  out_ref->container = refinstance;
  if(refinstance != ((Instance *)cur_ref->loc) )
   cur_ref->container->properties[cur_ref->offset] = (void *)
       refinstance;

  find_property(ctl, firstdesc, name, out_ref);  

  if(out_ref->offset < 0 )  {
         sprintf(msg, "Property '%s' not found", name);   
         do_gen_error(ctl, rule, gen_error, msg); 
  }
  return 1;
}


/* FIX THESE TO COMPILE REFS TO INDEXES?                                */
/**********************************************************************/
/* Get loc of value of name described by an entry loc (partial        */
/* symbol path resolution, and the remaining sequence of qualifiers)  */
int  get_end_ref(GCtl *ctl, Rule *rule, Ref *cur_ref,
                     CExp *constr, Ref *out_ref) { 

    Ref   next_ref;
    CExp *p_opnd1, *p_opnd2;

    lclmemset(&next_ref, 0, sizeof(Ref));  
    assert(cur_ref != NULL);
    switch(constr->op) {

         case op_name: 
              get_name_ref(ctl, rule, cur_ref, (char *)(constr->literal), out_ref); 
              break; 
         
         case op_path:

              p_opnd1 = &(CExps[constr->opnd1]);
              assert(p_opnd1->op == op_name);

              get_name_ref(ctl, rule,  cur_ref,
                       (char *)(p_opnd1->literal), &next_ref); 

              p_opnd2 = &(CExps[constr->opnd2]);
              get_end_ref(ctl, rule, &next_ref, p_opnd2, out_ref);
              break;
           default: assert(0); /* rest of op types shouldn't occur */
    } 

   return 1;
   
}

/*******************************************************************/ 
void get_ref_tops(GCtl *ctl, Rule *rule, 
               TypeDesc *ruledesc, Instance *ruleinst,
               Instance *locals, Instance *exts) {

     lclmemset(ruledesc, 0, sizeof(TypeDesc));
     ruledesc->desc_index = 9999;
     ruledesc->typename = "RuleArgs"; 
     ruledesc->propertydesc[0].propertyname = "arg"; 
     ruledesc->propertydesc[0].propertykind =  PropArgs;
     ruledesc->propertydesc[1].propertyname = "local"; 
     ruledesc->propertydesc[1].propertykind =  PropLocals;
     ruledesc->propertydesc[2].propertyname = "Last"; 
     ruledesc->propertydesc[2].propertykind =  PropLast;
     
     ruleinst->typename = "RuleInst"; 
     ruleinst->typeindex = 9999;
     ruleinst->prev_vs = ruleinst->next_vs = NULL;
     ruleinst->storetype = storeargs; 
     ruleinst->set_node = 0;  /* ?? */
     ruleinst->properties[0] = exts;
     ruleinst->properties[1] = locals;

     exts->typename = "ExtArgs";
     exts->typeindex = 9999;
     exts->next_vs = exts->prev_vs = NULL;
     exts->storetype = storeargs; 
     exts->set_node = 0;  /* ?? */

     locals->typename = "LocalArgs"; 
     locals->typeindex = 9999;
     locals->next_vs = locals->prev_vs = NULL;
     locals->storetype = storelocal; 
     locals->set_node = 0;

} 

      
/**********************************************************************/
/* TBD: FIX all these for consistent handling when name not found    */
/* there are two top-level instance specifiers here, distinguished */
/* by the compile-time-added qualifiers arg/local                   */
/* In any of these cases, the stacks are organized like          the */
/* descriptor/instance organization of base data, and can be treated */
/* similarly                                                         */
int get_base_ref(GCtl *ctl, Rule *rule,
                   TypeDesc *ruledesc, Instance* ruleinst,
                   char *name,  Ref *outref) {

   char msg[100];

   /* fills in local or ext desc and offset */
   outref->container = ruleinst;
   find_property(ctl, &ruledesc->propertydesc[0], name, outref);
   if(outref->offset < 0 )  {
         sprintf(msg, "Argument problem for '%s' ", name);   
         do_gen_error(ctl, rule, gen_error, msg); 
  }
   outref->loc = &(outref->container->properties[outref->offset]);
   return 1;
}
   
/*************************************************************************/
/* Invoke the referenced function and return the returned value as a ref */  
int get_fn_val_ref(GCtl *ctl, Rule *rule,
                 TypeDesc *ruledesc, Instance *ruleinst, 
                 CExp *constr, Ref *out_ref) { 

   int       i, argindex;
   CExp      *this_arg, *p_opnd1; 
   Ref       ref1;
   Function *function;
   void     *a[6], *retval;   
   PropDesc *argdescs;
   char      fn_ref_error[256];
   
   function = &(UFunctions[(int)constr->literal]);
   argdescs = &(FunPropDescs[function->arg_desc_offset]);
   argindex = constr->opnd1;
   i=0;

   /* doesn't work for nested function calls */
   ctl->current_rule = rule;
   ctl->current_function = function;

   /* put arg values into arg array */
   while(argindex != 0) { 
         
      this_arg = &(CExps[argindex]);
      assert(this_arg->op == op_arg); 
      p_opnd1 = &(CExps[this_arg->opnd1]);
      get_symbol_ref(ctl, rule, ruledesc, ruleinst, p_opnd1, &ref1);

      a[i] = referenced_value(&ref1);

      if(argdescs[i].propertykind != ref1.desc->propertykind) { 
           sprintf(fn_ref_error,
                 "\nNonmatching function invocation args for" 
                  "function %s in autostubber", function->function_name);
           do_gen_error(ctl, rule, gen_error, fn_ref_error);
      } 

      /* Unless argdesc allows any type, insist that types match */ 
      if(argdescs[i].propertykind == PropStruct && 
        strcmp(argdescs[i].typename, "Any")  &&
        strcmp(argdescs[i].typename, ref1.desc->typename)) {
           sprintf(fn_ref_error,
                 "\nNonmatching function invocation args for" 
                  "function %s in autostubber", function->function_name);
           do_gen_error(ctl, rule, gen_error, fn_ref_error);
      } 
   
      i++;
      if(this_arg->opnd2 == 0) break;
      argindex =  this_arg->opnd2;
      continue;
   }

   /* Invoke function */
   /* note.. long ints are sent as pointers */
   assert(i < 6);
   switch(i) {
    case 0: 
        retval = (function->function_ptr)(ctl, ctl->stub_ctl);
        break;
    case 1: 
        retval = (function->function_ptr)(ctl, ctl->stub_ctl,
                   a[0]);
        break;
    case 2: 
        retval = (function->function_ptr)(ctl, ctl->stub_ctl,
                   a[0], a[1]);
        break;
    case 3: 
        retval = (function->function_ptr)(ctl, ctl->stub_ctl,
                    a[0], a[1], a[2]);
        break;
    case 4: 
        retval = (function->function_ptr)(ctl, ctl->stub_ctl,
                    a[0], a[1], a[2],a[3]);
        break;
    case 5: 
        retval = (function->function_ptr)(ctl, ctl->stub_ctl,
                    a[0], a[1], a[2],a[3], a[4]);
        break;
    default: assert(0);
   }

  /* build result */
  out_ref->container = NULL;
  switch(function->returnkind){   
      case PropInt:
         out_ref->desc = &LIT_INT_DESC;
         break;
      case PropLongInt:
         out_ref->desc = &LIT_LONGINT_DESC;
         break;
      case PropString:
         out_ref->desc = &LIT_STRING_DESC;
         break; 
      case PropOpaque:
         out_ref->desc = &LIT_OPAQUE_DESC;
         break;
      case PropList:
         out_ref->desc = &LIT_LIST_DESC;
         break;
      case PropStruct: 
         out_ref->desc = &LIT_STRUCT_DESC;
         break;
      default: 
        assert(0);
   }
  out_ref->offset = 0;
  out_ref->loc = retval;

  return 1;

}


/**************************************************************************/
/* For path expressions and function references                           */  
/* Path expressions:                                                      */
/*  (op_name name) or (op_path name cexp)  e.g a is arg                   */
/*   a.b   --> (op_path (op_name arg) (op_path (opname a) opname name b)) */
/*   a.b.c --> (op_path (op_name arg) (op_path (opname a)                 */
/*                                  (op_path (opname b) (opname c))))     */
/*   etc.                                                                 */ 
/* This procedure gets loc of value described by full qualified name      */
/* Function references:                                                   */ 
/*   F_abc(args) --> op_fn literal=fn_table_index op_arg...               */ 

int get_symbol_ref(GCtl *ctl, Rule *rule,
                 TypeDesc *ruledesc, Instance *ruleinst, 
                 CExp *constr, Ref *out_ref) { 

    Ref     cur_ref;
    CExp *p_opnd1, *p_opnd2;

    switch(constr->op) {

       case op_fn:
            get_fn_val_ref(ctl, rule, ruledesc, ruleinst, constr, out_ref); 
            break;

       case op_path:
            p_opnd1 = &(CExps[constr->opnd1]);
            assert(p_opnd1->op == op_name);

           /* make this less artificial */            
            get_base_ref(ctl, rule, ruledesc, ruleinst,
                      (char *)p_opnd1->literal, &cur_ref);
            p_opnd2 = &(CExps[constr->opnd2]);
            get_end_ref(ctl, rule, &cur_ref, p_opnd2, out_ref);
            out_ref->loc = &(out_ref->container->properties[out_ref->offset]);
            break;

       /* set up as instance */
       case op_lit_string:
       case op_lit_int: 
       case op_lit_bool: 
       case op_lit_null: 
       case op_lit_longint: 
            out_ref->container = NULL;  /* for literal */
            out_ref->offset = 0;
            out_ref->loc = (void *)(constr->literal);
            switch(constr->op){
                 case op_lit_string:
                       out_ref->desc = &LIT_STRING_DESC;
                      break;
                 case op_lit_bool:
                 case op_lit_int:
                      out_ref->desc = &LIT_INT_DESC;
                      break; 
                 case op_lit_null:
                      out_ref->desc = &LIT_NULL_DESC;
                      break;
                 case op_lit_longint:
                      out_ref->desc = &LIT_LONGINT_DESC;
                      break;
                 default: assert(0);
            } 
            break;
         
       case op_name: 
      default:     
           assert(0);
    }
    return 1;
}


/*************************************************************/
/* This builds call args from rule ref in edge               */
/* argop cexp is (op_arg argref cexp/null)                   */
void build_call_args(GCtl *ctl, Rule *rule, Edge *edge,  
                TypeDesc *ruledesc, Instance *ruleinst,  Instance *call_args,
                PropDesc call_arg_desc[]) {
     
   int   i, argindex;
   CExp *this_arg, *p_opnd1; 
   Ref   ref1;
   Rule *called_rule;  
   char  temparg[100];
   char *stringarg;
   char  msg[256]; 
   
   argindex = edge->first_arg_cexp;
   i=0;

   while(argindex != 0) { 
         
      this_arg = &(CExps[argindex]);
      assert(this_arg->op == op_arg); 
      p_opnd1 = &(CExps[this_arg->opnd1]);
      get_symbol_ref(ctl, rule, ruledesc, ruleinst, p_opnd1, &ref1);

      call_args->properties[i] = referenced_value(&ref1);
      if(value_set_p(ctl, &ref1)) 
          call_args->prop_set[i] = (char)0;
       else call_args->prop_set[i] = (char)1;
      lclmemcpy(&call_arg_desc[i], ref1.desc, sizeof(PropDesc)); 

      /* if  ref is instance, get real type, as far as known */ 
      /* inefficient.. fix */
      if(ref1.desc->propertykind == PropStruct &&
         ref1.desc->typename != NULL &&
        ( ref1.desc->typeindex == 0 ||
         !strcmp(ref1.desc->typename, "Any") )) { 
            if(call_args->properties[i] == NULL) { 
              sprintf(msg, "Unacceptable NULL value for property %s", 
                         ref1.desc->propertyname);
               do_gen_error(ctl, rule, gen_error, msg);
             }
            call_arg_desc[i].typename = 
                 ((Instance *)call_args->properties[i])->typename; 
             call_arg_desc[i].typeindex = 
               find_arg_propdesc(ctl,rule, call_arg_desc[i].typename,
                         0, 0);
      }
      /* convert int arg to string if calling immediate literal rule*/ 
      called_rule = &(Rules[edge->category_index]);
      if(!strcmp(called_rule->rulename, "NULL")) {
            if(ref1.desc->propertykind == PropInt) {
                call_arg_desc[i].propertykind = PropString;
                sprintf(temparg, "%d", (int)call_args->properties[i]); 
                stringarg=(void *)malloc(strlen(temparg) +1);   
                strcpy(stringarg, temparg); 
                call_args->properties[i] = stringarg;
           }
           else if (ref1.desc->propertykind == PropLongInt) {
                call_arg_desc[i].propertykind = PropString;
                sprintf(temparg, "%ld", *((long int*)call_args->properties[i])); 
                stringarg=(void *)malloc(strlen(temparg) +1);   
                strcpy(stringarg, temparg); 
                call_args->properties[i] = stringarg;
           }
       }  
      i++;
      if(this_arg->opnd2 == 0) break;
      argindex =  this_arg->opnd2;
      continue;
   }
   call_arg_desc[i].propertykind = PropLast;
   call_arg_desc[i].propertyname = "Last";
   call_args->storetype = storeargs;
}


/**********************************************************************/
/* This builds call args from ref to called rule, arg ct, and */
/* arg list, for use from external rule call                */
void build_initial_call_args(GCtl *ctl, Rule *called_rule,
                 PropDesc *call_arg_desc) {
     
    int   i;
    PropDesc *rule_arg_desc;

    rule_arg_desc = &(PropDescs[called_rule->ext_arg_desc_offset]);

    for (i = 0; i < MAXPROPERTIES; i++ ){
       call_arg_desc->propertykind = rule_arg_desc->propertykind;
       call_arg_desc->typeindex = rule_arg_desc->typeindex;
       call_arg_desc->typename = rule_arg_desc->typename;
       if(rule_arg_desc->propertykind == PropLast) break; 
       rule_arg_desc++;
       call_arg_desc++;
    }
    if(i == MAXPROPERTIES) assert(0);
    
}
        


/**********************************************************************/
/* for currency, store new value in copy of structure, along with     */
/* search tree node                                                   */
/* What if same node ?                                                */
int unifyvalues(GCtl *ctl, Rule *rule, Ref *lhs_ref, Ref *rhs_ref)  {

  Ref       *r1_ref, *r2_ref;
  Instance *r1;
  Instance *new, *last;

  /* because can't chang args.... */
   if(lhs_ref->container == NULL && rhs_ref->container == NULL) 
      do_gen_error(ctl, rule, gen_error,  
           "Attempting to unify two constants; change op to ==");
   assert(lhs_ref->container->storetype != storeargs);
       
   if( (value_set_p(ctl, lhs_ref)) && value_set_p(ctl, rhs_ref ) ) { 
       if(comparevalues(ctl, rule, lhs_ref, rhs_ref)) return 1;
       else return 0; 
   }

   r1_ref = lhs_ref;
   r2_ref = rhs_ref;
   if(value_set_p(ctl, lhs_ref)) {
         r1_ref = rhs_ref;
         r2_ref = lhs_ref;
   }

   r1  = r1_ref->container; 
   if(r1_ref->container->set_node != ctl->current_set_node  &&
      r1_ref->container->storetype != storelocal ) { 
      new = new_instance();
      lclmemcpy(new, r1_ref->container, sizeof(Instance));  
      new->set_node = ctl->current_set_node;
      last = (Instance *)get_last_vs(ctl, r1); 
      new->prev_vs = last;
      last->next_vs = new; 
      r1 = new; 
   }

  r1->properties[r1_ref->offset] = referenced_value(r2_ref);

  set_value_set(ctl, r1_ref); 

  return 1;

}
       

/**********************************************************************/
int comparevalues(GCtl *ctl, Rule *rule, Ref *ref1, Ref *ref2) {

     void *v1, *v2;
     int error = 0;

     /* Two values don't compare unless both are set           t */
     /* Literal values are always set                          */ 
     if( (!value_set_p(ctl, ref1)) || !value_set_p(ctl, ref2) ) return 0; 

     if( (ref1->desc->propertykind != ref2->desc->propertykind)) { 
        switch (ref1->desc->propertykind) {
             case PropLongInt:
                  if(ref2->desc->propertykind != PropInt && 
                     ref2->desc->propertykind != PropBool ) error = 1;
                     break;
             case PropInt:
                  if(ref2->desc->propertykind != PropLongInt && 
                     ref2->desc->propertykind != PropBool ) error = 1;
                     break;
             case PropBool:
                  if(ref2->desc->propertykind != PropInt && 
                     ref2->desc->propertykind != PropLongInt ) error = 1;
                     break;
             default:
                  if(ref1->desc->propertykind != PropNull && 
                     ref2->desc->propertykind != PropNull) error = 1; 
                   break;
             }
          if(error) do_gen_error(ctl, rule, gen_error,
                     "Comparison of non-comparable properties");  
      }

      v1 = referenced_value(ref1);
      v2 = referenced_value(ref2);

      if(v1 == NULL && v2 == NULL) return 1;

     switch(ref1->desc->propertykind) { 

        case PropString:
           if( ((char*)v1) == NULL || ((char*)v2) == NULL) {
               if(v1 == v2) return 1;
               else return 0;
           }
           if(!strcmp( (char *)v1, (char *)v2 ))
              return 1;
           else return 0;
    
        case PropInt: 
        case PropBool: 

           if(ref2->desc->propertykind != PropLongInt) {
                if( ((int)v1) == ((int)v2) ) return 1; 
                else return 0;
           }
           else if( ((int)v1) == *((long *)(v2))) return 1; 
           else return 0;

        case PropList:
        case PropStruct:  
        case PropNull:
             if( ((char*)v1) == NULL || ((char*)v2) == NULL) {
               if(v1 == v2) return 1;
           }
           return 0;

        case PropLongInt: 
           if(ref2->desc->propertykind == PropLongInt) {
              if( *(long int *)v1 == *(long int *)v2) return 1;
              else return 0;  
            }
           else if( ((int)v2) == *((long *)(v1))) return 1; 
           else return 0;

        default: 
           assert(0); /*system error?*/ 
    }

   return 0; 
             
}



/**********************************************************************/
static int get_predvalue(GCtl *ctl, Rule *rule, 
         TypeDesc *ruledesc, Instance *ruleinst,
         CExp *constr) {

    Ref  ref1, ref2;
    int   predvalue;
    CExp *p_opnd1, *p_opnd2;

   /* no constraint */
    if(constr == 0) return 1;    

    p_opnd1 = &(CExps[constr->opnd1]);
    p_opnd2 = &(CExps[constr->opnd2]);

    switch(constr->op) {

       case op_unify: 
           
          get_symbol_ref(ctl, rule, ruledesc, ruleinst, p_opnd1, &ref1);
          get_symbol_ref(ctl, rule, ruledesc, ruleinst, p_opnd2, &ref2);
          predvalue = unifyvalues(ctl, rule, &ref1, &ref2);
          predvalue = 1;
          break; 

       case op_eqp:
       case op_neqp:
          get_symbol_ref(ctl, rule, ruledesc, ruleinst, p_opnd1,&ref1);
          get_symbol_ref(ctl, rule, ruledesc, ruleinst, p_opnd2,&ref2);
          predvalue = comparevalues(ctl, rule, &ref1, &ref2);
          if(constr->op == op_neqp) predvalue = !predvalue;
          break; 

      case op_and:
          predvalue = get_predvalue(ctl,rule, ruledesc, ruleinst,  p_opnd1); 
          if(!predvalue) predvalue = 0;
          else
            predvalue = get_predvalue(ctl,rule, ruledesc, ruleinst, p_opnd2);
          break;

      case op_or: 
          predvalue = get_predvalue(ctl,rule, ruledesc, ruleinst, p_opnd1); 
          if(!predvalue) predvalue =
               get_predvalue(ctl,rule, ruledesc, ruleinst,  p_opnd2); 
          break; 

      case op_lit_int:
      case op_lit_bool: 
           if( ((int)p_opnd1) != 0) predvalue = 1; 
           else predvalue = 0;
           break;

      case op_lit_null:
           predvalue = 0;
           break;

      case op_lit_string: /* ?? */
           if( ((char*) p_opnd1) != NULL) predvalue = 1; 
           else predvalue = 0;
           break;

      case op_lit_longint: /* ?? */
           if( ((long int*) p_opnd1) != 0) predvalue = 1; 
           else predvalue = 0;
           break;

      default: assert(0);


     }

  return predvalue;

}


/***********************************************************/   
int get_repeat_parameters(GCtl *ctl, int repeat_exp_offset, Rule *rule,
                   TypeDesc *ruledesc, Instance *ruleinst,
                   Ref *var_ref, Ref *index_ref, listElement **list_elem){

  CExp *repeat_cexp, *varopnd, *listopnd, *indexopnd; 
  Ref   list_ref;  
  list  repeat_list;
  int   has_index_ref;
   
  has_index_ref = 0;
  repeat_cexp = &(CExps[repeat_exp_offset]);   
  varopnd = &(CExps[repeat_cexp->opnd1]);  
  listopnd = &(CExps[repeat_cexp->opnd2]);   
  indexopnd = &(CExps[(int)repeat_cexp->literal]);
  
  get_symbol_ref(ctl, rule, ruledesc, ruleinst, varopnd, var_ref);

  get_symbol_ref(ctl, rule, ruledesc, ruleinst,listopnd, &list_ref);

  if(repeat_cexp->literal != 0) {
     get_symbol_ref(ctl, rule, ruledesc, ruleinst,indexopnd, index_ref);
     has_index_ref = 1;  
  }

  if(list_ref.desc->propertykind != PropList) { 
        do_gen_error(ctl, rule, gen_error, "Iteration source not list");  
   }

   repeat_list = referenced_value(&list_ref);

   if(repeat_list == NULL || list_size(repeat_list) == 0)
           *list_elem= NULL;
   else *list_elem = repeat_list->head;

   return (has_index_ref);
}

  
   
/*********************************************************************/
/* starting at given alternative edge of current rule node            */
/* try to find a path that succeeds.  If alternative edge is a repeat */ 
/* effect is like sequence of                                         */
/* r1| TRUE r2| TRUE ..                                               */
/* edge itself succeeds unless set from which repeat drawn is null    */  
/* If edge has contraint equations, try each until success            */ 

int find_edge_path(GCtl *ctl, Rule *rule, Edge *edge, 
                     TypeDesc *ruledesc, Instance *ruleinst, 
                     GenTree **result_node_chain) {

   Instance     call_args;
   int          in_repeat, has_disj, found, current_eq, next_eq;
   int          first_rpt_level, first_disj_level, last_repeat_failed;
   CExp        *p_constr;
   RuleNode    *p_target; 
   GenTree     *category_subtree, *next_result_chain, *this_result_chain;
   PropDesc     arg_descs[MAXPROPERTIES]; 
   listElement *repeat_list_elem;
   Ref          repeat_ref, index_ref;
   Instance     temps, temps1;
   int          ri, has_index_ref, di  /* debugging*/; 
 
   in_repeat = 0;  
   has_disj = 0; 
   found = 0;
   this_result_chain = NULL;
       
   lclmemcpy(&temps, ruleinst->properties[1], sizeof(Instance));

   has_index_ref = 0;
   if(edge->repeat_exp != 0) {
        has_index_ref =  get_repeat_parameters(ctl, edge->repeat_exp,
            rule, ruledesc, ruleinst,
            &repeat_ref, &index_ref, &repeat_list_elem);
         /* edge fails if no elements in list? or ? */
 /*         if(repeat_list_elem == NULL) return 0; */
         in_repeat = 1;
   }

   /* disjuncts pulled to outermost level and act as */
   /* alt category branches                          */
   if(edge->eqs != 0) { 
      has_disj = 0;
      p_constr = &(CExps[edge->eqs]);
      if (p_constr->op == op_or)  {
           has_disj = 1;
#ifdef DEBUG
           fprintf(stdout, "\n");
           print_cexp(ctl, edge->eqs, 2);
           fprintf(stdout, "\n");
#endif
      } 
  }

  first_rpt_level = 1;
  last_repeat_failed = 0;

  for( ri=0 ;
      (first_rpt_level &&  !in_repeat) || repeat_list_elem != NULL; ri++,
      repeat_list_elem = in_repeat ? repeat_list_elem->next : NULL) { 

      first_rpt_level = 0;

     first_disj_level = 1; 
     current_eq = edge->eqs;  
     next_eq = 0;  /* avoid noninitialized var warning */

     /* Each repeat iteration like i<list | TRUE */
     /* But, also, cannot set local vars in repeat eqs and expect */
     /* values to hold -- should add provision for this          */
      lclmemcpy(ruleinst->properties[1], &temps, sizeof(Instance)); 

      lclmemcpy(&temps1, ruleinst->properties[1], sizeof(Instance)); 
     last_repeat_failed = 0;

     for ( di = 0 ;
          (first_disj_level && !has_disj) || current_eq != 0;
          current_eq = has_disj ? next_eq : 0, di++) {

          first_disj_level = 0; 

#ifdef DEBUG
        if(has_disj > 0)
            printf("\ndisj %d", di); 
        if(has_index_ref > 0)
            printf("\nrpt %d", ri); 
#endif

         /* for data currency.. recapture why two variables */
         ctl->last_search_node++;
         ctl->current_set_node = ctl->last_search_node;
         assert(ctl->current_set_node < SIZE_TNV);

         lclmemcpy(ruleinst->properties[1], &temps1, sizeof(Instance)); 
         /* update local inst set node */
         ctl->tree_node_vector[ctl->current_set_node] = 0; 

         if(in_repeat) {
            repeat_ref.container->properties[repeat_ref.offset]
                 = repeat_list_elem->data;
            set_value_set(ctl, &repeat_ref);
            if(has_index_ref) {
               index_ref.container->properties[index_ref.offset]
                    = (void *)ri;   
                set_value_set(ctl, &index_ref);
             } 
         }
                 
         if(edge->eqs != 0) {
            p_constr = &(CExps[current_eq]);
            /* if disjunctive, get real opnd and set up nect */ 
            if(p_constr->op == op_or){
                   next_eq = p_constr->opnd2; 
                   p_constr = &(CExps[p_constr->opnd1]); 
              }
             else next_eq = 0;
 
            if(!get_predvalue(ctl, rule, ruledesc, ruleinst, p_constr)) {
                ctl->tree_node_vector[ctl->current_set_node] = 1;
                continue;
            }
         }
    
         if(edge->category_index != 0)  { /* epsilon .. or TRUE?*/

             build_call_args(ctl, rule, edge, ruledesc, ruleinst,
                      &call_args, &arg_descs[0]);
 
             if(!interpret_rule(ctl, edge->category_index,
                  &arg_descs[0], &call_args, &category_subtree)) {
                  ctl->tree_node_vector[ctl->current_set_node] = 1;
                  continue;
             }
        }

        /* succeeded */
         if(!in_repeat) {
             /* go to target */
              p_target = &(RuleNodes[edge->target_node]);
              next_result_chain = NULL;
              if(!find_rule_path(ctl, rule, p_target,
                  ruledesc, ruleinst, &next_result_chain)) continue;
                  found = 1;
              if( edge->category_index != 0)
                  append_to_chain(ctl, result_node_chain, category_subtree);
              if(next_result_chain != NULL)
                  append_to_chain(ctl, result_node_chain,next_result_chain);
                  break; 
       } 
       else {
          if( edge->category_index != 0)
               append_to_chain(ctl, &this_result_chain, category_subtree);
          found = 1;
          break;  
      }
    } /* end of disjunction loop */

    if(!found) 
         last_repeat_failed = 1;

    if(!in_repeat) break;  

 }  /* end of repeat edge loop */ 

 if(in_repeat) {
         /* go to target */
        found = 0;
        p_target = &(RuleNodes[edge->target_node]);
        next_result_chain = NULL;
        if(find_rule_path(ctl, rule, p_target, ruledesc, ruleinst,
           &next_result_chain))  {
        
           /* this makes no sense... epsilon repeat edge */
           if( edge->category_index != 0)
                append_to_chain(ctl, result_node_chain, this_result_chain);
           if(next_result_chain != NULL)
               append_to_chain(ctl, result_node_chain, next_result_chain);
           found = 1;
       }
  }
   return found;
}



/*******************************************************************/
/* try to find alternative path beginning at current rule node     */
static int find_rule_path(GCtl *ctl, Rule *rule, RuleNode *cur_node,
              TypeDesc *ruledesc, Instance *ruleinst,
              GenTree **result_node_chain) {

   int          edge_index, first;
   Edge        *edge;
   Instance     temps;

   if(cur_node->final) return 1; 

   edge_index = cur_node->first_edge;

   /* to restore locals after each failed alternative                */
   /* not certain this fully integrated with non-local currency scheme*/ 
   lclmemcpy(&temps, ruleinst->properties[1], sizeof(Instance));

   first = 1;
   while(1) {
       if(first) edge_index = cur_node->first_edge;
       else edge_index = edge->next_edge; 
 
       if(edge_index == 0)  {
           if(first) return 1; 
           else break;
       }  

       first = 0;

       /* restore locals */
       lclmemcpy(ruleinst->properties[1], &temps, sizeof(Instance));

       edge = &(RuleEdges[edge_index]);

       if(find_edge_path(ctl, rule, edge, ruledesc, ruleinst, 
                         result_node_chain))  
          return 1;
  }
  return 0;

 }

/*******************************************************************/   
static int validate_args(GCtl *ctl, Rule *rule, PropDesc *arg_desc) {
  
    PropDesc *rule_desc, *temp_arg_desc;

    rule_desc = &(PropDescs[rule->ext_arg_desc_offset]);
    temp_arg_desc = arg_desc;

    while(temp_arg_desc->propertykind != PropLast) {

#ifdef DEBUG
       if(temp_arg_desc->typename != NULL && rule_desc->typename != NULL) { 
         printf("\ncomparing rule_desc_type %s, arg_desc_type %s",
               rule_desc->typename, temp_arg_desc->typename);
         if(!strcmp(rule_desc->typename,temp_arg_desc->typename)) 
               printf( "EQUAL");
      }
#endif


       if( rule_desc->propertykind != temp_arg_desc->propertykind ) 
           return 0;
       if(rule_desc->propertykind == PropStruct) {
           if(rule_desc->typeindex < 0) 
              rule_desc->typeindex = find_arg_propdesc(ctl, rule,
                      rule_desc->typename, 1,-( rule_desc->typeindex));
           if (temp_arg_desc->typeindex < 0) 
             temp_arg_desc->typeindex = find_arg_propdesc(ctl, rule,
                    temp_arg_desc->typename, 1,-(temp_arg_desc->typeindex));
       }
       
       if ( (rule_desc->typeindex != temp_arg_desc->typeindex) &&
             strcmp(rule_desc->typename,"Any"))  /* T*/
                          return 0;
       rule_desc++;
       temp_arg_desc++;
    }

    /* checks for same number of args */
    if(rule_desc->propertykind != PropLast) return 0;

    return 1;
}


/***********************************************************************/   
/* returns 0 for fail, 1 for succeed                                   */
int interpret_rule(GCtl *ctl, int category_index, PropDesc *arg_desc, 
                   Instance *args, GenTree **result_subtree) {

   Rule     *rule;
   Instance  locals, *new, ruleinst;
   GenTree  *subtree, *result_chain;  
   RuleNode *first_node;
   TypeDesc  ruledesc;  

   rule = &(Rules[category_index]);
 
#ifdef DEBUG1
   printf("\n Rule %s",rule->rulename);
#endif

   *result_subtree = subtree = new_subtree();
   subtree->rule = rule;
   subtree->search_node = ctl->last_search_node; 

   result_chain = NULL;

   /* fail if arg and value PropStruct but unmatching type.         */ 
   /* other nonmatches fatal errors (system or other depending on   */
   /* how much checking done at compile time)                       */
   if(!validate_args(ctl, rule, arg_desc)) {
            free(subtree); 
            *result_subtree = NULL;
            return 0;
    }   

   /* if grammar rule, add first succeeding path to tree */
   if(rule->ruletype == ruletype_R) {    
       /* create instance structure for local parameters... FIX */
       new = new_instance();
       lclmemcpy(&locals, new, sizeof(Instance));
       locals.storetype = storelocal;
       locals.set_node = ctl->last_search_node;
       /* 1 means not set */
       lclmemset(&(locals.prop_set[0]), 1, MAXPROPERTIES); 
       ctl->current_set_node = ctl->last_search_node;
       free(new); 

       first_node = &(RuleNodes[rule->first_node]);

       get_ref_tops(ctl, rule, &ruledesc, &ruleinst, &locals, args);

       if(find_rule_path(ctl, rule, first_node,
              &ruledesc, &ruleinst, &result_chain)) {
          subtree->first_child = result_chain;
          return 1;
      }   
      else {
          free(subtree);
          *result_subtree = NULL;
          return 0; 
      }
   }
   
   else if(rule->ruletype == ruletype_L) {
        subtree->leaf_args = new_instance();
        lclmemcpy(subtree->leaf_args, args, sizeof(Instance));
        return 1;
   }

   assert(0);  /* ruletype must be grammar rule or lit rule */
   return 0;  /* should never reach here */
}


/***********************************************************************/   
/* TBD: convert decimal args to strings before leaf store */
static int print_literal_block(GCtl *ctl, GenTree *leaf, FILE *outfile) { 
   int          i, j, first_line, line_ct;
   char         *a[6];
   Line         *line; 
   char          ai[7][50];
   PropDesc     *argdescs;
   PropertyKind  argkind;
   long int      thelong;
    
   first_line = leaf->rule->first_literal_line; 
   line_ct = leaf->rule->line_ct;
   argdescs = &(PropDescs[leaf->rule->ext_arg_desc_offset]);
   for(j = first_line; j < first_line + line_ct; j++) { 
       line = &(Lines[j]); 
        assert(line->arg_ct <= 6);
        for(i = 0; i < line->arg_ct; i++) { 
           a[i]=leaf->leaf_args->properties[line->arg_indices[i]];
           /* must be string or int; if int, convert */
           argkind = argdescs[line->arg_indices[i]].propertykind;
           if(argkind == PropInt) {
               sprintf(&ai[i][0],"%d", (int)a[i]); 
               a[i] = &(ai[i][0]);
           }
           else if (argkind == PropLongInt) {
               thelong = *(long int *)a[i];
               sprintf(&ai[i][0],"%ld", thelong); 
               a[i] = &(ai[i][0]);
            } 
           if (a[i] == NULL)
              do_gen_error(ctl, leaf->rule, gen_error,
               "Null argument to literal block");
         }
        switch (line->arg_ct) {
           case 0:
              fprintf(outfile, line->format_string);
              break;
           case 1:
              fprintf(outfile, line->format_string, a[0]);
              break;
           case 2:
              fprintf(outfile, line->format_string, a[0], a[1]);
              break;
           case 3:
              fprintf(outfile, line->format_string, a[0],a[1], a[2]);
              break;
           case 4:
              fprintf(outfile, line->format_string, a[0], a[1], a[2], a[3]);
              break;
           case 5:
              fprintf(outfile, line->format_string, a[0], a[1], a[2], a[3],
                    a[4]);
              break;
           case 6:
              fprintf(outfile, line->format_string, a[0], a[1], a[2], a[3],
                    a[4],a[5]);
             break;
          default: assert(0);     
      }/*end switch */
     fflush(outfile);
  } /* end for j*/

  free(leaf->leaf_args);
   
  return 1;
}


/**************************************************************/   
/* process tree depth first printing all L_ nodes encountered */
/* and freeing tree nodes as leave them                       */
static int print_output(GCtl *ctl, GenTree *tree, FILE *outfile) {
    GenTree *child, *next_child;

    next_child = NULL;
    if(tree->rule->ruletype == ruletype_L) 
         print_literal_block(ctl, tree, outfile);
    else {
      for(child = tree->first_child; child; child = next_child){ 
         print_output(ctl, child, outfile);
         next_child = child->next_sibling;
#ifndef KEEP_TREE     
         free(child);
#endif
        }
     }
    return 1;
}
    
       
/**************************************************************/   
/* Input should be interface                                  */
int produce_file(GCtl *ctl, Function *functions, PropDesc *funpropdescs, 
                 RuleSet *ruleset, Instance *input){

    FILE    *outfile;
    GenTree *tree; 
    PropDesc  initial_arg_desc[MAXPROPERTIES];
    Instance  initial_arg_instance; 
    Rule     *first_rule;
    int       x;
    char      out_file_name[256], msg[256], *temp_ptr;
     
    Scopes     = ruleset->scopes;
    Rules      = ruleset->rules;
    Lines      = ruleset->lines;
    RuleNodes  = ruleset->rulenodes;
    RuleEdges  = ruleset->ruleedges;
    PropDescs  = ruleset->propdescs; 
    CExps      = ruleset->cexps;
    UFunctions = functions;
    FunPropDescs  = funpropdescs; 

    if(setjmp(GenerateExitEnv) != 0) {
         return 0;
    }

    /* input should be interface */
    if(input != NULL && strcmp(input->typename, "Interface"))
      do_gen_error(ctl, NULL, gen_error,
          "Input to produce file not interface instance");

    lclmemset(&initial_arg_instance, 0, sizeof(Instance)); 
    lclmemset(&initial_arg_desc[0], 0, MAXPROPERTIES *sizeof(PropDesc)); 
    initial_arg_instance.properties[0] = input;
    first_rule =  &(Rules[Scopes[1].first_rule]);
    build_initial_call_args(ctl, first_rule, &initial_arg_desc[0]);
            
     /* in_rule_file_name must be xxxx.rules, e.g., cpp.h.rules        */
     /* associated out_file_name will be intfname-xxxx, e.g., foo-cpp.h*/
     /* unless building special file */
     if(input != NULL) {
       strcpy(out_file_name, get_string_property_value(ctl, input,"isl_name"));
       strcat(out_file_name, "-");
     }
     else strcpy(out_file_name, "ilu-");
     strcat(out_file_name, ruleset->in_rule_file_name);
     temp_ptr = strrchr(out_file_name, '.');
     if(temp_ptr == NULL || (strcmp(temp_ptr+1, "rules")))
         do_gen_error(ctl, NULL, gen_error,
              "Stubber input file name must have suffix '.rules'");
     *temp_ptr = '\0';

     x = Scopes[1].first_rule; /* ?? bug */
     if(!interpret_rule(ctl, x,  &initial_arg_desc[0],
         &initial_arg_instance,  &tree)) {
         sprintf(msg, "Could not generate %s", out_file_name);
         do_gen_error(ctl, NULL, gen_error, msg);
     }

     outfile = fopen(out_file_name, "w");
     if(!print_output(ctl, tree, outfile)) { 
         fprintf(stderr, "Could not print %s", out_file_name);
         return 0;
     }

    return 1;
}

/*****************************************************************/
GCtl *initialize_gen(int struct_type_count, void *stub_ctl) { 

  GCtl *ctl;

  ctl = (void *)calloc(1,sizeof(GCtl));
  ctl->stub_ctl = stub_ctl;
  ctl ->tree_node_vector = (void *)calloc(SIZE_TNV, sizeof(char));
  
  /* ?? */
   ctl->current_set_node = 0;
   ctl->last_search_node = 0;

   ctl ->struct_ref_map = (void *)calloc(struct_type_count,  
                                         sizeof(StructCorr));
  
  return ctl;
}

