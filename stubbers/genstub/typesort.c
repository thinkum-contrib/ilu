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

/* NOTE: THIS TYPESORT IS DESIGNED FOR CPLUSPLUS..              */
/* INCLUDED HERE TO GIVE INTERESTING OUTPUT FROM PRINT CONVERTED */ 

/********************************************************************/
/*    Local structure defs                                          */ 
/********************************************************************/

/* pred = thing depended upon */
typedef enum { CatType, CatExc, CatModule, CatExtModule} PartCat;

typedef struct{
          PartCat    pred_cat;       /* type, exc, module, ext module */
          int        pred_fwd_p;
          void      *pred;           /* can be type or exception or module*/
          PartCat    succ_cat;       /* type, def, module*/
          int        succ_fwd_p;
          void      *succ;   /* can be type or exception or module*/
          int        marked;
          int        pred_marked;
} ModDep;

/********************************************************************/
/*             Debug                                                */
/********************************************************************/

/* debug */

void print_dep_list(list l) {
    listElement *ptr;
    ModDep      *md;

    fflush(stdout);
    printf("\n\nMOD DEP LIST \n"); 
    for(ptr = l->head; ptr; ptr = ptr->next) {
       printf("\n");
       md = (ModDep *)(ptr->data);
       printf("PRED ");  
       if(md->pred_cat == CatType )
         printf("TYPE: %s ", name_base_name( ((Type)(md->pred))->name)); 
       else if(md->pred_cat == CatExc)
          printf("EXC: %s ", name_base_name( ((Exception)(md->pred))->name)); 
       else if(md->pred_cat == CatModule)
            printf("MODULE %s ", ((Module)(md->pred))->simple_name);
       else printf("EXTMODULE ... ");
       if(md->pred_fwd_p) printf("%s ","FWD.");
       else printf("%s", "DEF.");

       printf("SUCC ");  
       if(md->succ_cat == CatType )
         printf("TYPE: %s ", name_base_name( ((Type)(md->succ))->name)); 
       else if(md->succ_cat == CatExc)
          printf("EXC %s ", name_base_name( ((Exception)(md->succ))->name)); 
       else printf("MODULE %s ", ((Module)(md->succ))->simple_name);
       if(md->succ_fwd_p) printf("%s ", "FWD. ");
       else printf("%s", "DEF. ");

       printf("PMK %d MK %d",md->pred_marked, md->marked);  
       fflush(stdout);
     }
}

void print_ordering_list(list l) {
    listElement *ptr;
    ContentElem ce;

    printf("\nORDERING\n");
    for(ptr = l->head; ptr; ptr = ptr->next) {
       printf("\n");
       ce = (ContentElem)(ptr->data);
       if(!strcmp(ce->content_type, "fwd_type"))
          printf("\nTYPE FWD: %s ",
                name_base_name( ((Type)(ce->content))->name)); 
       else if(!strcmp(ce->content_type, "type"))
          printf("\nTYPE DEF: %s ",
                name_base_name( ((Type)(ce->content))->name)); 
       else if(!strcmp(ce->content_type, "module"))
          printf("\nMODULE: %s ", ((Module)(ce->content))->simple_name); 
       else printf("\nEXC: %s ",  
                name_base_name( ((Exception)(ce->content))->name)); 
    }
    printf("\n");
}

/********************************************************************/
/*             Utilities                                            */
/********************************************************************/

/* change  to macro or inline */
static void
BUILD_DEP_ENT(list dep_list,  PartCat pcat, int pfwd, void *pvalue,
             PartCat scat, int sfwd, void *svalue) {
         ModDep *md; 
         md = (void * )calloc(1, sizeof(ModDep)); 
         md->pred_cat = pcat;  
         md->pred_fwd_p = pfwd; 
         md->pred = pvalue; 
         md->succ_cat = scat; 
         md->succ_fwd_p = sfwd; 
         md->succ = svalue;
         list_insert(dep_list, md);  
 }

static void
BUILD_ORDER_ENT(list order_list, char * entrytype, void *value)  {
          ContentElem ce; 
          ce = (void * )calloc(1, sizeof(struct ilu_content_elem_s)); 
          ce->content_type = entrytype;  
          ce->content = value; 
          list_insert(order_list, ce);  
   }
          


/* extend l1 by, effectively, inserting all elements of l2 */
/* if l1 is empty,  create it first                      */
/* (will modify l1 but not l2)                           */
static void list_concat(list l1, list l2) {
 
   if(l1 == NULL ) l1 = new_list();

   if(l2 ==NULL || l2->head == NULL) return; 

   if(l1->head == NULL) {
         l1->head = l2->head;
         l1->tail = l2->tail;
         l1->count = l2->count;  
         return;
   }

   l1->count += l2->count;

   assert(l1->tail != NULL);
   l1->tail->next = l2->head;
   l1->tail = l2->tail; 
}


/********************************************************************/
/* If t is contained in m return 1 and outermost containing module */ 
/* within m (which may be m). Otherwise return 0                   */
int relative_contained (Module m, list t_scoping,  Module *rel_module) {
   
   list   m_scoping;
   listElement  *m_ptr, *t_ptr, *c_ptr; 
   char  *t_name;
   Module c_module;

   m_scoping = m->scoping;
   m_ptr = m_scoping->head; 
   t_ptr = t_scoping->head; 

   while (1){
      if(m_ptr == NULL || t_ptr == NULL  ) break;
      if( strcmp((char*)m_ptr->data, (char*)t_ptr->data)) break;
      m_ptr = m_ptr->next; 
      t_ptr = t_ptr->next; 
   }
  /*if m_ptr non-null, t is not directly or indirectly contained in m */
   if(m_ptr != NULL) return 0; 
   if(t_ptr == NULL) *rel_module = m;  /*is object module */
   else if(t_ptr->next == NULL) *rel_module = m;
   /* otherwise get outermost module within m containing t */
   else { 
       t_name = (char *)t_ptr->data; 
       for(c_ptr = m->contained_modules->head; c_ptr; c_ptr=c_ptr->next) {
         c_module = (Module)c_ptr->data;
         if(!strcmp(t_name, c_module->simple_name)) break;
       }
       if(c_ptr == NULL) {
         fprintf(stderr, "System error in def ordering for stubbing");
         assert(0); 
      }
      *rel_module = c_module; 
    }
    return 1;
}

/******************************************************************/
ContentElem new_content_elem() {
        return (void *)calloc(1, sizeof(struct ilu_content_elem_s));
}

/******************************************************************/
int  is_substantive_typedef(Type t) {
   
   if(t->builtIn) return 0; 
   if( t->importInterfaceName != NULL && 
      !strcmp(t->importInterfaceName, "ilu")) return 0;
   return 1;
}

/*********************************************************************/
/* Is this a CORBA interface type that contains declarations */
int is_dep_module_obj(Type t, list dep_module_objs) {

   listElement *ptr;

   if(dep_module_objs == NULL || dep_module_objs->head == NULL) 
         return 0;
    for(ptr = dep_module_objs->head; ptr; ptr= ptr->next) 
      if( t == (Type)ptr->data) return 1;
    return 0;
 }
   

/*********************************************************************/

/* see if type is one for which dependence upon accounted for by */
/* forward definitions.  If so, do not consider as member-type dependency*/
/* MAY NEED REFINEMENT */
int member_type_dependency(Type type) {
   Type t, t1;
   
   t = ur_type(type);
    
   if( is_substantive_typedef(t)  &&
       (t->description->type == record_Type ||
        t->description->type == union_Type || 
        t->description->type == sequence_Type || 
        t->description->type == array_Type ) )
      return 1;

   else if (t->description->type == optional_Type) {
        /* because optional types stored as vars */
        /* and initial defs of array vars are fwd */
        t1 =  ur_type(t->description->structuredDes.optional);
        if(is_substantive_typedef(t1) && t1->description->type==array_Type ) 
           return 1;
   }
   return 0;
}
/*********************************************************************/
/* aux routine for get_type_deps                                     */
int ref_to_objmodule(Type t, Module objmodule) {      

    listElement *ptr;

    if(objmodule == NULL || objmodule->contained_types == NULL)
          return 0;     

    for(ptr =objmodule->contained_types->head; ptr; ptr =ptr->next) {
            if(ptr->data != t) continue;
            else break;
     } 
     if(ptr != NULL) return 1;
     return 0;
}


/*********************************************************************/
/* Add to dl list of dependences of t                                */
/* return 1 if fwd def of t has no deps                              */
/* use objmodule only if processing module represeting CORBA interface  */
int get_type_deps(Type t, list dl, Module objmodule) {

 list         l1, l2;  
 Type         t1; 
 listElement *ptr, *ptr1, *ptr2;
 int          retval; 

 /* if is an alias type, has only fwd def, and dep only on target */ 
 if(t->supertype != NULL) {
          BUILD_DEP_ENT(dl, CatType, 1, t->supertype, CatType, 1, t);
          if(!is_substantive_typedef(t->supertype)) return 1;
 return 0;
 }

 retval = 0;
 /* self dep */
  if(objmodule == NULL)
      BUILD_DEP_ENT(dl, CatType, 1, t, CatType, 0, t) ;


 switch(t->description->type) {

    case array_Type:
     BUILD_DEP_ENT(dl, CatType, 1,
              t->description->structuredDes.array.type, CatType, 1, t); 
     /* arrays have access code in .hpp file */ 
     t1 = ur_type(t->description->structuredDes.array.type);
     if(member_type_dependency(t1)) 
                   BUILD_DEP_ENT(dl, CatType, 0, t1, CatType, 0, t) ;
     break;

    case sequence_Type:
      BUILD_DEP_ENT(dl, CatType, 1,
              t->description->structuredDes.sequence.type, CatType, 1, t);
      break;

    case optional_Type:
       if(is_substantive_typedef(
               ur_type( t->description->structuredDes.optional))) { 
             BUILD_DEP_ENT(dl, CatType, 0,
               ur_type(t->description->structuredDes.optional), CatType, 0, t);
      }
       BUILD_DEP_ENT(dl, CatType, 1,
               t->description->structuredDes.optional, CatType, 1, t);
     break;

    case object_Type:
         l1 = t->description->structuredDes.object->superclasses;
         if( (l1 != NULL) && l1->head != NULL) {
            for(ptr = l1->head; ptr; ptr = ptr->next) {
               t1= ur_type((Type)(ptr->data));
               BUILD_DEP_ENT(dl, CatType, 1, ptr->data, CatType, 1, t); 
               BUILD_DEP_ENT(dl, CatType, 0, t1, CatType, 0, t);
            }
          } 
          l1 = t->description->structuredDes.object->methods;
          for(ptr = l1->head; ptr; ptr=ptr->next) {
               l2 = ((Procedure)(ptr->data))->arguments;
               if(l2 != NULL) {
                 for(ptr1 = l2->head; ptr1; ptr1 = ptr1->next) { 
                   t1 = ((Argument)(ptr1->data))->type;
                   /* if dealing with module obj, don't inc self refs */
                   if(!ref_to_objmodule(t1, objmodule)) 
                       BUILD_DEP_ENT(dl,CatType, 1, t1, CatType, 0, t);
                  }
               }
               t1 = ((Procedure)(ptr->data))->returnType;
               if(t1 != NULL) { 
                 /* if dealing with module obj, don't inc local refs */
                 if(!ref_to_objmodule(t1, objmodule)) 
                    BUILD_DEP_ENT(dl,CatType, 1, t1, CatType, 0, t);
               }

               l2 = ((Procedure) (ptr->data))->exceptions;
               if(l2 != NULL) {
                 for(ptr1 = l2->head; ptr1; ptr1= ptr1->next) { 
                   /* if dealing with module object, don't inc lcl refs */
                   if( (objmodule != NULL) &&
                        objmodule->contained_exceptions != NULL) {
                      for(ptr2 =objmodule->contained_exceptions->head;ptr2;
                               ptr2=ptr2->next) {
                        if(ptr2->data != ptr1->data) continue;
                         else break;
                      } 
                      if(ptr2 != NULL) continue;
                    } 
                    BUILD_DEP_ENT(dl, CatExc,1, ptr1->data, CatType, 0, t);
                  }
                }     
          }
          break;

    case record_Type:
         l1 = t->description->structuredDes.record.fields;
         if( (l1 != NULL) && l1->head != NULL) {
            for(ptr = l1->head; ptr; ptr = ptr->next) {
               t1 = ur_type( ((Argument)(ptr->data))->type);
               if(member_type_dependency(t1))  
                   BUILD_DEP_ENT(dl, CatType, 0, t1, CatType, 0, t) ;
               else
                  BUILD_DEP_ENT(dl, CatType, 1, t1, CatType, 0, t) ;
            }
         }  
         break;  

  case union_Type:
         t1 =t->description->structuredDes.uniond.discriminator_type;
         if(t1 != NULL)
             BUILD_DEP_ENT(dl, CatType, 1, t1, CatType, 0, t) ;
         l1 = t->description->structuredDes.uniond.types;
         if( (l1 != NULL) && l1->head != NULL) {
            for(ptr = l1->head; ptr; ptr = ptr->next) {
               t1 = ur_type( ((Argument)(ptr->data))->type);
                          if(member_type_dependency(t1)) 
                   BUILD_DEP_ENT(dl, CatType, 0, t1, CatType, 0, t) ;
               else 
                   BUILD_DEP_ENT(dl, CatType, 1, t1, CatType, 0, t) ;
            }
         }  
         break;

   default: retval = 0;
 }

 return retval;
}

/*********************************************************************/
/* Note, these used only when extend beyond immediately containing  */
/* module;  within module fwd exception defs first, defs last        */
int get_exc_deps(Exception e, list dl)  {

    if(e->type != NULL) {
         if(member_type_dependency(e->type)) 
              BUILD_DEP_ENT(dl, CatType, 0, ur_type(e->type), CatExc, 0, e) ;
         else BUILD_DEP_ENT(dl, CatType, 1, e->type, CatExc, 0, e) ;
         /* another dep needed here? */ 
         return 1; 
     }    

   else return 0;
}

/*******************************************************************/
/* Analyze dependency list into local and external                  */ 
/*******************************************************************/

/* Separate a dependence list into local and external,             */
/* External if predecessor is not in current module.  If local     */ 
/* divide between local fwd_dep, def_to_fwd, and def_to_def lists  */ 
/* ctd_module non-null if input_list is extl dep list of a ctd module */ 
static void analyze_dependency_list ( 
        Module module, Module ctd_module, list input_list,
        list fwd_dep_list, list def_to_fwd_list, list def_to_def_list,
        list ext_dep_list, list final_ordering ) {

        int          has_int_deps, is_contained, pred_builtin;
        ModDep      *entry, *new_entry;
        list         scoping;
        Module       pred_module;
        listElement *ptr;
     

       /* Separate the ctnd ext deps into local ones and ones to */ 
       /* be forwarded to containing module                      */  
       has_int_deps = 0;
       
#ifdef undef
    {
        Type         t;
       /* if ctd module is really CORBA interface, create phony */ 
       /* dependency so associated object is ordered in container */
          if(ctd_module != NULL && ctd_module->module_object != NULL) {
               t= ctd_module->module_object;
               BUILD_DEP_ENT(fwd_dep_list, CatType, 1, t, CatType, 1, t) ;
         }
    }
#endif

       for(ptr = input_list->head; ptr; ptr = ptr->next) {

          entry = (ModDep *)(ptr->data); 

          if(entry->pred_cat == CatType)
                   scoping = ((Type) (entry->pred))->scoping;
          else if(entry->pred_cat == CatExc)   
                    scoping = ((Exception) (entry->pred))->scoping;
          else  scoping = ((Module)entry->pred)->scoping;

          /* Modify refs to CORBA interfaces containing attrs to type refs*/
          if(ctd_module != NULL && ctd_module->module_object != NULL) {
                  entry->succ_cat = CatType;
                  entry->succ = (void *)(ctd_module->module_object);
                  entry->succ_fwd_p = 0;
           }

          /* Determine relative scope of depended upon type */
          is_contained = 0; 
          pred_builtin = 0;

          /* gets outermost contained module in or = current module*/
          /* if dependency encompassed within current module ...*/ 
          /* or is dependency on built in type or built-in module */ 
          /* (unless is circular dependency to self  */
          if( entry->pred_cat == CatType && ((Type)(entry->pred))->builtIn){
             /* don't add dependency from module object itself here */
             if( entry->succ != (void *)(module->module_object)) { 
               is_contained = 1;
               pred_builtin = 1;
             }
          }
          else if( scoping == NULL) {} 
          else if( (entry->pred != (void *)(module->module_object)) &&
               relative_contained(module, scoping, &pred_module)) 
               is_contained = 1;

           /* if pred in another interface or builtin*/
          if(is_contained == 0 && (module->containing_module==NULL)) { 
               entry->pred_cat = CatExtModule;
               entry->pred = module->containing_module;
               entry->pred_fwd_p = 0;
           }

          /* if depended upon is local, or current is outermost module */
          if(is_contained || module->containing_module == NULL) {

              /* If pred is in contained module                       */  
              /* indicate predecessor is whole module, because module  */
              /*  decls can't be split when interpreted as  C++ classes */ 
               if(is_contained && (!pred_builtin)
                   && pred_module != module && 
                  pred_module->module_object == NULL) {
                   entry->pred_cat = CatModule;
                   entry->pred = (void *)pred_module; 
                   entry->pred_fwd_p = 0;
               }

               /* add to initial mini-sort list */
               if(entry->succ_fwd_p && entry->pred_fwd_p)
                  list_insert(fwd_dep_list, ptr->data);
               else if(entry->pred_fwd_p)
                  list_insert(def_to_fwd_list, ptr->data);
               else list_insert(def_to_def_list, ptr->data);
              
               entry->marked = 0;
               has_int_deps = 1;

          } /* end if relative contained */
     
          /* otherwise if dependency to something outside current    */ 
          /* becomes a dependency of current as a whole, and forwarded*/
          else {
              /*copy so can later free input_list */ 
              new_entry = (void *)malloc(sizeof(ModDep));
              memcpy((void *)new_entry, (void *)entry, sizeof(ModDep));  
             /* unless is dependency of a module_object */
             if(! ( entry->succ == module->module_object ) )  
              {
                new_entry->succ_cat = CatModule;  
                new_entry->succ = (void *)module;
                new_entry->succ_fwd_p = 0;
              }
              list_insert(ext_dep_list, new_entry);
          }
       } /* end for i */

      /* if analyzing ctd module list, and            */ 
      /* if only contained deps are outside "module",  */
      /* ensure contained module in content sequence */
      if( ctd_module != NULL && (!has_int_deps) &&
                 ctd_module->module_object == NULL) {
            BUILD_ORDER_ENT(final_ordering, "module", ctd_module) ;
            ctd_module->marked = 1;
      }

 }

/************************************************************************/
/* Build list of dependencies for immediately contained types of module */
/************************************************************************/

static void build_local_deps(Module module, list local_deps, 
                             list final_ordering){
 
  Type         t; 
  listElement *ptr, *ptr1;
  Module       m; 

  if (module->contained_exceptions != NULL &&
      module->contained_types->head != NULL){
      for(ptr = module->contained_exceptions->head; ptr; ptr = ptr->next) {
         /* exception deps only used when outside module */
         get_exc_deps((Exception)(ptr->data), local_deps); 
       }
   }

  /* add local types with no dependences to final ordering, */
  /* and create dep entry for other deps                     */
  if (module->contained_types != NULL && module->contained_types->head != NULL){
    for(ptr = module->contained_types->head; ptr; ptr = ptr->next) {
         t = (Type)ptr->data;
         /* used to indicate only if forward dep inserted */
         t->marked = 0;  
         /* change to is_substantive_typedef */
         if( !is_substantive_typedef(t)) continue;  
         /* if has module equivalent, already analyzed; just add self def */
         if(t->description != NULL && t->description->type==object_Type) {
            if(module->contained_modules != NULL) {
               for( ptr1 = module->contained_modules->head;
                    ptr1; ptr1=ptr1->next) {
                    m = (Module)(ptr1->data);
                    if(m->module_object == t) {
                      BUILD_DEP_ENT(local_deps, CatType,1, t,CatType, 0, t) ;
                      break;
                    } 
                }
                if(ptr1 != NULL) continue; 
             }
         }
         /* can only be true of fwd dep dependent only on builtin */
         if(get_type_deps(t, local_deps, NULL) != 0) {
             /* put as fwd def of type */
              BUILD_ORDER_ENT(final_ordering, "fwd_type", t)  ;
             t->marked = 1;  
         }
    }
   /* also, if module is actually a corba object, get its deps */  
   if(module->module_object != NULL) {
         get_type_deps(module->module_object, local_deps, module);
    } 

  } 

}


/***********************************************************************/
/* Movement of content of dependency lists to final ordering           */
/***********************************************************************/

/* Process a dep list.  Iterate over list moving elements not dependent */
/* on anything else on list. Continue until no further movement possible*/
/* Mark deps handled.  Also, mark types of fwd defs moved to final     */   
/* to avoid double iteration for those.                                */
/* MAKE SURE ANY LIST PROCESSED HAS ALL DEPS FOR SUCCESSORS            */
/* SO MUST BE FWD_DEP_LIST, OR REMAINDER + EVERYTHING ELSE             */
static list proc_dep_list(list dep_list, list final_ordering) { 

   char  *ce_type;
   int          i, si, did_something, global_did_something;
   unsigned int marked_ct;
   ModDep      *targ_entry, *pred_entry;
   ModDep      *succ_list[50];
   list         new_dep_list; 
   listElement *ptr, *predptr;
   void        *targ_value;   
   PartCat      targ_cat;
   int          targ_fwd_p, first;

   marked_ct = 0;  
   first = 1;
   while(1) {

      did_something = 0;

      for(ptr = dep_list->head; ptr; ptr = ptr->next) {

         targ_entry = (ModDep*)(ptr->data); 
         if(targ_entry->marked) continue;
             

         /* first find and install all the pred fwds && modules not yet */
         /* in ordering.. if not dependent (& not exceptions  */
         if(first) {
            if(targ_entry->pred_marked) continue;

            if(targ_entry->succ_fwd_p &&
               targ_entry->succ_cat == CatType &&
               ((Type)(targ_entry->succ))->marked ) { 
                   targ_entry->marked = 1; 
                   marked_ct++;
                   continue;
           }

            else if(!targ_entry->pred_fwd_p ) {
               if(targ_entry->pred_cat == CatType) continue;
               else if( targ_entry->pred_cat == CatModule 
                     && ((Module)(targ_entry->pred))->marked) { 
                      targ_entry->pred_marked = 1;
                      continue; 
                 }
               else if(targ_entry->pred_cat == CatExtModule) {
                      targ_entry->pred_marked = 1;
                      continue; 
               }
             }
            else if( targ_entry->pred_cat==CatExc) {
                 targ_entry->pred_marked = 1;
                 continue;
            } 
            else if( targ_entry->pred_cat == CatType
                   &&  ( ((Type)(targ_entry->pred))->marked 
                       || !is_substantive_typedef(
                                 ((Type)(targ_entry->pred)) )) ) {
                      targ_entry->pred_marked = 1;
                      ((Type)(targ_entry->pred))->marked = 1; 
                      continue;
              }
              
            targ_value = targ_entry->pred; 
            targ_fwd_p = targ_entry->pred_fwd_p;
            targ_cat   = targ_entry ->pred_cat;
         }
                 
        /* otherwise normal iteration */
        else {
          targ_value = targ_entry->succ; 
          targ_fwd_p = targ_entry->succ_fwd_p;
          targ_cat   = targ_entry->succ_cat;
        }

         si = 0;
         if(targ_entry->marked) continue;

         /* see if all preds of successor already moved or otherwise ok*/
         for(predptr = dep_list->head; predptr; predptr = predptr->next){

             pred_entry = (ModDep*) (predptr->data);
             /* ignore entry if marked */
             if(pred_entry->marked )continue;

             if(targ_value == pred_entry->succ &&
                targ_fwd_p == pred_entry->succ_fwd_p) { 
                /* if pred is fwd and already moved or first(exc) */
                /* mark and ignore                                */   
                if(   pred_entry->pred_marked
                   || (pred_entry->pred_cat == CatModule   
                        &&  ((Module)(pred_entry->pred))->marked )
                   || ( pred_entry->pred_fwd_p  &&
                      ( pred_entry->pred_cat == CatExc  
                        || (pred_entry->pred_cat == CatType
                          && ( ((Type)(pred_entry->pred))->marked 
                             || !is_substantive_typedef(
                                    ((Type)(pred_entry->pred))  )))) ))
                     { pred_entry->marked = 1;
                       pred_entry->pred_marked = 1;
                       if(pred_entry->pred_cat == CatType &&
                            pred_entry->pred_fwd_p) 
                             ((Type)(pred_entry->pred))->marked = 1; 
                        marked_ct ++;
                        continue;
                 } 
                 else break; /* nogood */
            }
            /* if current successor is pred of something else */
            /* list entry so can mark later                   */
            if(targ_value == pred_entry->pred
               && targ_fwd_p == pred_entry->pred_fwd_p )  { 
               succ_list[si] = pred_entry;
               si ++;  
           }
         } /* end for predptr */

        /* if all ok, add succ to final order, mark entries where */
        /* listed as pred                                         */
         if(predptr == NULL) {
            did_something = 1;
            global_did_something = 1;  
            /* Don't build explicit order entries for exceptions */ 
            if(targ_cat != CatExc)  {
              if(targ_cat == CatModule) ce_type = "module";
              else if(targ_fwd_p) ce_type = "fwd_type"; 
              else ce_type = "type";
              BUILD_ORDER_ENT(final_ordering, ce_type, targ_value) ;
            }
            if(targ_cat == CatType && targ_fwd_p) 
                       ((Type)(targ_value))->marked = 1; 
            else if(targ_cat == CatModule) 
                       ((Module)(targ_value))->marked = 1; 
   
            for(i = 0; i < si; i++) { 
                succ_list[i]->pred_marked = 1;
            }
         }

      } /* end for ptr .. another iteration over dep list */

      if( (!did_something) && !first) break;
      first = 0;
   }

  /* now condense dep list and see if anything left */
  if(global_did_something == 0) return dep_list;

  new_dep_list = new_list();

  #ifdef DEBUG
   print_dep_list(dep_list);
   printf("\nDEP LIST SIZE %d MARKED CT %d\n", list_size(dep_list), marked_ct);
  #endif

  if(marked_ct < list_size(dep_list)) {

      for(ptr = dep_list->head; ptr; ptr= ptr->next) {
        if( ((ModDep*)(ptr->data))->marked) free(ptr->data); 
        else list_insert(new_dep_list, ptr->data);
      }
   }
   list_clear(dep_list, 0);
   return new_dep_list;
}

/*********************************************************************/
/* Main procedure                                                    */
/*********************************************************************/
/* Assumes nested IDL modules ordering may be interspersed with types   */
/* But do not begin/resume                                              */
/* i.e. can have M1 { t.. M2 t.. }                                      */ 
/* but not M1 { t... M2-1 t.. M2-2  }                                   */
void module_def_sort(Module module, list ext_dep_list) {
    
  listElement *ptr;
  list        final_ordering;
  list        fwd_dep_list, def_to_fwd_list, def_to_def_list;
  list        ctd_dep_list, local_dep_list, rest_dep_list;
  Module      ctd_module; 

  #if(defined(DEBUG1)||defined(DEBUG))
     printf("\nENTERING SORT FOR MODULE %s\n", module->simple_name); 
  #endif


  final_ordering= new_list();
  fwd_dep_list = new_list();
  def_to_fwd_list = new_list();
  def_to_def_list = new_list();
  local_dep_list = new_list();
  ctd_dep_list = new_list();

  /* process modules in depth_first postorder     */
  /* order their internal defs, and list ext deps */ 
  if(module->contained_modules != NULL) {
   for(ptr = module->contained_modules->head; ptr; ptr = ptr->next) {

       ctd_module = (Module)ptr->data;  
       ctd_module->marked = 0;

       /* recursive part */
       module_def_sort((Module)ptr->data, ctd_dep_list); 

       analyze_dependency_list(module, ctd_module, ctd_dep_list,
                  fwd_dep_list, def_to_fwd_list, def_to_def_list,
                  ext_dep_list, final_ordering);
       list_clear(ctd_dep_list, 0);
     } 
  } /* end if module->contained_modules..*/

  #if(defined(DEBUG1)||defined(DEBUG))
     printf("\nRETURNING TO SORT FOR MODULE %s\n", module->simple_name); 
  #endif

  
   /* Gather dependences of local types. add some fwd defs */
   /* not dependent on anything else to  final ordering    */
   build_local_deps(module, local_dep_list, final_ordering);

   analyze_dependency_list(module, NULL, local_dep_list,
                  fwd_dep_list, def_to_fwd_list, def_to_def_list,
                  ext_dep_list, final_ordering);
  
  /* heuristically process lists so as to foster placement */
  /* of fwd defs first, and limit processing               */
  /* first process fwd_dep_list; has all dependences of local fwd deps */

  #ifdef DEBUG
     print_dep_list(fwd_dep_list);
  #endif

  fwd_dep_list = proc_dep_list(fwd_dep_list, final_ordering);

  #ifdef DEBUG
    print_dep_list(fwd_dep_list);
  #endif

  rest_dep_list = fwd_dep_list;
  list_concat(rest_dep_list, def_to_fwd_list);
  list_concat(rest_dep_list, def_to_def_list);

  #ifdef DEBUG
     print_dep_list(rest_dep_list);
  #endif

  rest_dep_list = proc_dep_list(rest_dep_list, final_ordering);

  #ifdef DEBUG
     print_dep_list(rest_dep_list);
  #endif
  
  #ifdef DEBUG1
     print_ordering_list(final_ordering);
  #endif

  fflush(stdout);
  if(rest_dep_list->head != NULL) {
       fprintf(stderr, "\nProblem in ordering types for stubbing\n");
       fprintf(stderr, "Try to reduce type dependences\n");
       fprintf(stderr, "Cannot continue\n");
       exit(1);
   }

/* THIS CAUSING FREE MEMORY READS
  list_clear(fwd_dep_list, 1);
  list_clear(def_to_fwd_list, 1); 
  list_clear(def_to_def_list, 1);
  list_clear(local_dep_list, 1); 
  list_clear(ctd_dep_list, 1); 
  list_clear(rest_dep_list, 1); 
  free(fwd_dep_list);
  free(def_to_fwd_list);
  free(def_to_def_list);
  free(local_dep_list);
  free(ctd_dep_list);
*/

  #ifdef DEBUG
     printf("\nNEXT IS EXT DEP LIST");
     print_dep_list(ext_dep_list);
  #endif

  module->content_sequence = final_ordering;  
}


/*************************************************************************/
/*  External Interface                                                   */ 
/*************************************************************************/
void top_fwd_def_sort(Module module) {

  list dummy_list;

  dummy_list = new_list();
  module_def_sort(module, dummy_list);
  list_clear(dummy_list, 0);
}
