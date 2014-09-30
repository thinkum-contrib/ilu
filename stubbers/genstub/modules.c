
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

#include "convert.h"

/*************************************************************************/
/* Procedures to build module tree associated with interface             */
/* (see build_basic_module_structure for interface                       */
/*************************************************************************/

static Module new_module(char *name, Module parent) {
  Module  module; 
  module = calloc(1, sizeof(struct ilu_module_s));
  module->simple_name = name;
  module->containing_module = parent;
  return (module);
}

static char *last_in_scoping(list scoping) {
   char       *return_value;
   listElement *ptr;

   if(scoping == NULL || scoping->head == NULL) return NULL;
   for(ptr = scoping->head; ptr != NULL; ptr = ptr->next) {
        if(ptr->next == NULL) break;
   }
   return_value = (char *)ptr->data;
   return return_value;
}

/*************************************************************************/
/* link pseudo-modules created for CORBA interfaces containng type       */
/* definitions to equivalent object types                               */
static void  fixup_for_CORBA_IDL(Module module) {  
  listElement *ptr, *tptr;
  Module       ctd_module;
  Type         type;
  char        *typename;   

  if(module->contained_modules != NULL && module->contained_types != NULL) {
    for (tptr = module->contained_types->head; tptr; tptr = tptr->next) {
       type = (Type)tptr->data; 
       typename = last_in_scoping(type->scoping);
       if( (type->builtIn != 1) && (type_ur_kind(type)== object_Type)) { 
          for(ptr = module->contained_modules->head; ptr; ptr = ptr->next){ 
            ctd_module = (Module)ptr->data;
            if(!strcmp(ctd_module->simple_name,typename)){ 
               ctd_module->module_object = type;
               break;
             }
           }
        }
     }
  }
  
  if(module->contained_modules != NULL) {
     for(ptr = module->contained_modules->head; ptr; ptr = ptr->next)  {
         ctd_module = (Module)ptr->data;
         if(ctd_module->module_object == NULL)
              fixup_for_CORBA_IDL((Module)ptr->data); 
    }
  }
}

/*************************************************************************/
static void extend_module_tree(Module      this_module,
                       listElement *this_scope_elem,
                        Module     *return_module) { 

   char     *thisname, *cmname;
   Module    next_module;
   listElement *cm_elem, *next_scope_elem;

   thisname = (char *)this_scope_elem->data;

   if(this_module->contained_modules == NULL) {
       this_module->contained_modules = new_list();
   }

   for(cm_elem = this_module->contained_modules->head;
       cm_elem;
       cm_elem = cm_elem->next) {
            cmname = ((Module)cm_elem->data)->simple_name;
            if(strcmp(thisname, cmname)) continue;
            break; 
   } 

   if(cm_elem == NULL)  {  /* not found, make new tree entry  */
           next_module = new_module(thisname, this_module);
           list_insert(this_module->contained_modules, next_module); 
       }
   else next_module = (Module) cm_elem->data; 

   /* see if finished, i.e., if at end of scope name (one before last */
   /* because last is thing being scoped                              */ 
   next_scope_elem = this_scope_elem->next;
   if(next_scope_elem != NULL && next_scope_elem->next ==NULL) {
       *return_module = next_module;  
        return; 
   }  

   assert(next_scope_elem != NULL);
   extend_module_tree(next_module, next_scope_elem, return_module); 
}
   
/*************************************************************************/
/* will add module to tree when first encountered                        */
static void build_module_tree(Module first_module, list scoping,
                              Module *return_module) {

   listElement *this_scope_elem, *next_scope_elem;

   *return_module = NULL;

   /* first name must be interface name, and must match current  */
   this_scope_elem = scoping->head; 
   assert(!strcmp(first_module->simple_name,
              (char *)(this_scope_elem->data)));

   next_scope_elem = this_scope_elem->next;
    /* and next name must exist... name of thing being searched */
   assert(next_scope_elem != NULL);
   /* if at next to last in scoping, finished */
   if(next_scope_elem->next ==NULL) {
        *return_module = first_module;  
         return; 
   }
   extend_module_tree(first_module, next_scope_elem, return_module); 
 }


/*************************************************************************/
/* Add scoping entry to module and its descendants                       */ 
static void add_module_scoping(list current_scoping, Module module ) {

  listElement *ptr;

  module->scoping = new_list();

  if(current_scoping != NULL) {  /* all but outermost */
      for(ptr = current_scoping->head; ptr; ptr = ptr->next)  
          list_insert(module->scoping, ptr->data);
  }

  list_insert(module->scoping, module->simple_name);

  /* carry down scoping */
  if(module->contained_modules != NULL) {
    for(ptr = module->contained_modules->head; ptr; ptr = ptr->next) 
       add_module_scoping(module->scoping, (Module) ptr->data);  
  }

}

   
/*************************************************************************/
/* a basic CORBA module structure for an interface is a list of structures*/
/* each of which contains (a) a list of modules immediately nested within */
/* the interface or the immediately containing module, (b)  a list of     */
/* types immediately contained within the containing module and           */
/* (c) (for name generation purposes) a back pointer to the module parent */
/* This is currently derived from iluptype scoping information because    */
/* it is not directly available from the iluptype structure.. but should  */
/* be.                                                                    */
Module build_basic_module_structure(Interface interface ) { 

 Type         t; 
 Constant     c;
 Exception    e;
 Module       first_module, module;
 listElement *ptr; 

  /* create initial tree node */
  first_module = new_module(name_base_name(interface->name), NULL);

  if(interface->types != NULL) {
    for(ptr = interface->types->head; ptr; ptr = ptr->next) { 
       t = (Type)ptr->data;
       if( t->builtIn || (t->importInterfaceName != NULL)) continue;  
       build_module_tree(first_module, t->scoping, &module);        
       if(module->contained_types == NULL)
           module->contained_types = new_list();
       list_insert(module->contained_types, t); 
     }
  }

  if(interface->constants != NULL) {
    for(ptr = interface->constants->head; ptr; ptr = ptr->next) { 
       c = (Constant)ptr->data;
       if(c->importInterfaceName != NULL) continue;
       build_module_tree(first_module, c->scoping, &module);         
       if(module->contained_constants == NULL)
           module->contained_constants = new_list();
       list_insert(module->contained_constants, c); 
     }
  }

  if(interface->exceptions != NULL) {
    for(ptr = interface->exceptions->head; ptr; ptr = ptr->next) { 
       e = (Exception)ptr->data;
       if(e->importInterfaceName != NULL) continue;
       build_module_tree(first_module, e->scoping, &module);         
       if(module->contained_exceptions == NULL)
           module->contained_exceptions = new_list();
       list_insert(module->contained_exceptions, e); 
    }
  }

 /* do fixup for CORBA IDL, where objects can contain type declarations */  
 /* identify modules corresponding to objects, and                     */
 /* create mutual refererences                                         */ 
  fixup_for_CORBA_IDL(first_module);  

 /* now go through modules and give each its scopename */ 
 /* (for comparisons of referenced type with current scope */ 
  add_module_scoping(NULL, first_module); 
   
  interface->module_structure = first_module; 

  return (first_module);
}

