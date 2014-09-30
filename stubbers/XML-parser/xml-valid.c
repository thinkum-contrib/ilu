
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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <setjmp.h>
#include <string.h>
#include "xml-types.h"
#include "xml-func.h"

#define MAXATTWORDS 20
/* set  define LOOSE_CONTENT to avoid errors on elements */ 
/* defined as PCDATA and used null */
#define LOOSE_CONTENT

/**********************************************************************/
/* for now will just check in current heap.  fix when keep heap lists */
void check_referenced_ids(Ctl *ctl) {  
     
     int       i;
     IdEntry   *id_entries;

     id_entries = &ctl->current_id_heap[0];
     for(i = 0; i < ctl->current_ids_used; i++) {

        if(id_entries[i].defined ==0)  
            do_validity_subst_error(ctl, &id_entries[i].undef_pos, 
                        "First reference to"
                         " undefined id value '%s'",
                          &id_entries[i].id_string);  
                    
      }
}
     

/*******************************************************************/
/*Does special validation processing for some attribute types      */ 
static int validate_attribute_special (Ctl *ctl, AttrSpec *aspec,
                   XString *aname, XString *value,
                   Pos *pos, int no_id, int no_nonid) {
   
  IdEntry     *id_entry; 
  ValueList   *vlist; 
  Entity      *entity;
  
  switch(aspec->attr_kind) {
       case AttrKindID:   
          if(no_id) break;   /* when checking default value in decl*/ 

          /* check if duplicate appearance as id attribute */
          id_entry = search_id_hash(ctl, value, 1);
          if(id_entry->defined) {
              do_validity_subst_error(ctl, pos,
                 "Duplicate id value for attribute '%s'", aname);
                  return 0;
          }
          id_entry->defined = 1; 
          break;

      case AttrKindIDREF:  case AttrKindIDREFS:
         if(no_id) break;   /* when checking default value in decl*/ 

         /* check if id defined and record if not.. recheck at end of doc*/
         id_entry = search_id_hash(ctl, value, 1);
         if(!id_entry->defined) {
                /* put attribute node in entry */
                /* that is, remember one pos at which attr defined */
                /* before use, and check at end                   */
                 lclmemcpy(&id_entry->undef_pos, pos, sizeof(Pos)); 
         }
         break;

      case AttrKindENTITY: case AttrKindENTITIES:
         if(no_nonid) break;   /* when checking default value in doc*/ 
          entity = get_ref_entity_def(ctl, 0, *value, 0);
          if(entity == NULL || !entity->unparsed ) { 
              do_validity_subst_error(ctl, pos,
                 "Value of entity attribute '%s' either not defined or"
                 " not unparsed entity", aname);
                  return 0;
           }
           break;
          
      /* both of these require compare against definition list */
      case AttrKindNOTATION: case AttrKindEnum:
          if(no_nonid) break;   /* when checking default value in doc*/ 
          vlist = aspec->attr_alt_list; 
          for(; vlist; vlist= vlist->next) { 
           if(XStringsEq(ctl, &vlist->value, value)) break;
          } 
          if(!vlist) {
              do_validity_subst_error(ctl, pos,
                 "Value of attribute '%s' not in declared list", aname);
                  return 0;
           }
          break;
               
      default: break;  /* other kinds should not get here */
   }

   return 1;
}

/***********************************************************************/
/* Normalizes and validates single attribute value                      */
/* Used for both document attributes and default declarations           */
/* aspec NULL if not validating.. in which case just minimum normalize  */
int normalize_and_validate_attribute(
          Ctl      *ctl,         AttrSpec *aspec,   
          XString  *aname,       XString  *attr_value, 
          Pos      *pos,         int       is_default) {

    typedef enum {wd_const_none, wd_const_name, wd_const_nmtoken,
                  wd_const_uint, wd_const_int} WdConst;

    XString      awords[MAXATTWORDS], dummy_string;
    uchar        *attr_string, *new_pos;
    int          i, k, m, normalize, prev_ws, multiple, no_more_words;
    int          internal_multi_space, additional_value_checks, new_len;
    int          initial;
    WdConst      wd_constraint;
    SpecialEnum  special;
    AttrKind     attkind;

   normalize = multiple = additional_value_checks = 0; 
   /* set reparse parameters according to attribute kind */

   if(aspec == NULL) attkind = AttrKindCDATA;
   else attkind = aspec->attr_kind;

   switch(attkind) {
       case AttrKindCDATA: 
         normalize = 0;
         wd_constraint = wd_const_none;
         break;

      case AttrKindINT:
           normalize = 1;
           wd_constraint = wd_const_int;
           break;

      case AttrKindUINT:
           normalize = 1;
           wd_constraint = wd_const_uint;
           break;
    
      case AttrKindIDREFS: case AttrKindENTITIES:
         multiple = 1; /* intentional fall thru */
      case AttrKindID:     case AttrKindIDREF:
      case AttrKindENTITY: case AttrKindNOTATION:
         normalize = 1;
         wd_constraint = wd_const_name;
         additional_value_checks = 1;
         break;

      case AttrKindNMTOKENS: 
         multiple = 1;  /* intentional fall thru */
      case AttrKindNMTOKEN:
         normalize = 1;
         wd_constraint = wd_const_nmtoken;
         break;

       case AttrKindNAMES:
         multiple = 1;  /* intentional fall thru */
       case AttrKindNAME:
         normalize = 1;
         wd_constraint = wd_const_nmtoken;
         break;

      case AttrKindEnum:
         normalize = 1;
         wd_constraint = wd_const_nmtoken;
         additional_value_checks = 1;
         break;
         
      default: assert(0);
   }

  /* reparse and normalize */ 
  /* First modify in situ, isolate multiple values, and see if */ 
  /* any internal multi-blanks, in which case have to move     */

  i = k = no_more_words = internal_multi_space = prev_ws = 0;

  attr_string = attr_value->loc; 

  initial = 1; 

  awords[0].loc = NULL; 
  awords[0].len = -1;
  for(i = 0; i < attr_value->len; i += 1+(ctl->is_double)){ 

      /* if is special char, check for ws and do initial normalization*/
      if(get_specials_code(ctl, &attr_string[i], &special, 0,0)) {
         if(special == LINEFEED_CHAR || special == RETURN_CHAR ||
            special == TAB_CHAR || special == BLANK_CHAR) {

            attr_string[i] = '\0';
            attr_string[i +ctl->is_double] = ' ';
           
            if(!(initial || multiple || wd_constraint == wd_const_none))
                  no_more_words = 1;

            if(normalize && !initial) {
                 if(!multiple) no_more_words = 1; 

                 if(prev_ws)  { 
                      if(!multiple) internal_multi_space = 1;
                  }
                 /* otherwise record end of word*/
                 else  { 
                   awords[k].len = (int)&attr_string[i] -
                              (int)awords[k].loc;
                   k++;
                   awords[k].loc = NULL; 
                   awords[k].len = -1;
                   initial = 0; 
                }
            }
  
           prev_ws = 1;   
          continue;
         }
     }/* end if get_specials */             

     /* From here not ws */
     if(no_more_words) {
        do_validity_subst_error(ctl, pos,
          "Value of attribute '%s' must be single word - see spec", aname);
        return 0;  /* indicate validity error for attribute */
     }

     /* update word list */
     if(prev_ws || initial) {
            assert (k < MAXATTWORDS);  
            awords[k].loc = &attr_string[i]; 
            awords[k].len = -1;
     }

     if(initial || prev_ws) {
         switch(wd_constraint) {
           case wd_const_name:
              if(!name_begin_char(ctl, &attr_string[i])) {
                   do_validity_subst_error(ctl, pos,
                     "Attribute '%s' value must start with name begin char",
                   aname);
              return 0;  /* indicate validity error for attribute */
             }
             break;

           case wd_const_int:  
               if(!signed_begin_char(ctl, &attr_string[i])) { 
                   do_validity_subst_error(ctl, pos,
                     "Attribute '%s' value must be integer", aname);
                   return 0;
               }
              break; 
 
           case wd_const_uint:
             if(!digit_char(ctl, &attr_string[i])) {
                   do_validity_subst_error(ctl, pos,
                     "Attribute '%s' value must start with digit",
                     aname);
                   return 0;  /* indicate validity error for attribute */
              } 
              break;

          case wd_const_nmtoken: 
            if(!name_char(ctl, &attr_string[i])) {
              do_validity_subst_error(ctl, pos,
               "Attribute '%s' value must contain only name chars", aname);
              return 0;  /* indicate validation error for attribute */
             }
            break; 

          case wd_const_none:
              break; 
           
         } /* end switch if(initial.. */
     }

    else {
         switch(wd_constraint) {
           case wd_const_name: case wd_const_nmtoken:
              if(!name_char(ctl, &attr_string[i])) {
                   do_validity_subst_error(ctl, pos,
                  "Attribute '%s' value must contain only name chars",
                   aname);
              return 0;  /* indicate validity error for attribute */
             }
            break;

           case wd_const_int: case wd_const_uint: 
             if(!digit_char(ctl, &attr_string[i])) {
                   do_validity_subst_error(ctl, pos,
                     "Attribute '%s' value must be integer",
                     aname);
                 return 0;
              } 
              break;

          case wd_const_none:
              break; 

        } /* end switch(wd_constraint */
     } /* end else */

     prev_ws = 0;
     initial = 0;
  } /* end for(i */
        
  /* Finish off word entries if last incomplete */ 
  /* because of intervening end of string      */
  if( (!initial) && awords[k].len == -1 && awords[k].loc !=NULL) {
      awords[k].len = ((int)&attr_string[i]) -(int)awords[k].loc;
       k++; 
   }
 
  /* Do any further checks needed */ 
  if( aspec && additional_value_checks)  {
    for(m = 0; m < k; m++) 
       validate_attribute_special(ctl, aspec, aname,
               &awords[m], pos, is_default, 0);
  }

  /* if attribute requiring normalization, break off any initial/final ws*/
   if(normalize) {
     attr_value->loc = awords[0].loc; 
     attr_value->len = awords[k-1].loc - awords[0].loc + awords[k-1].len; 
    }

  /* See if full rewriting required - above method betting not*/
  if(normalize && internal_multi_space) {
     /* form fully normalized value by copying words */
     if(ctl->string_buf_pos + attr_value->len < ctl->string_buf_max) 
        get_new_string_buf(ctl, attr_value->len);  
     new_pos = ctl->string_buf_pos; 
     new_len = 0;
     for(i = 0; i < k; k++) {
         add_to_string_buf(ctl, &awords[k], &dummy_string);
         new_len += awords[k].len;
         if(i < k-1) {
            *ctl->string_buf_pos = '\0';
             if(ctl->is_double) ctl->string_buf_pos++;
            *ctl->string_buf_pos = ' ';
             ctl->string_buf_pos++;
          }
     }
     attr_value->loc = new_pos;
     attr_value->len = new_len;

  }     

  /* if attribute has FIXED value, compare with given */ 
  if( aspec && (!is_default) && aspec->default_kind == AttrFixed) {
     if(!XStringsEq(ctl, attr_value, &aspec->default_value)) {
         do_validity_subst_error(ctl, pos,
           "Attribute '%s' value must be same as fixed value of declaration",
                  aname);
         return 0;
      }
  }
        
  return 1;
} 

/*******************************************************/
/* top level check of node attributes if not validating */
void check_attributes(Ctl *ctl, Node *element_node) { 
  Node *attr, *attr1;

  /* check for duplicates */
  for(attr = element_node->first_attr; attr; attr=attr->next) {    
       for(attr1 = attr->next; attr1; attr1 = attr1->next) {
           /* btree index */
           if(attr->index == attr1->index) {
             do_error(ctl, &(attr1->position), &(attr1->position),
                   formation_error, no_recover, 
                  "Attribute appears more than once in same element tag");
            }
         }
      /* NULL arg implies treat as CDATA */
      normalize_and_validate_attribute(
                 ctl, NULL, 
                 &attr->name, &attr->value,
                 &attr->position, 0); 
   }

}
  

/**************************************************************/
/* Does validation that can occur on local basis,     */
/* specifically not presence of id matching idrefs    */ 
void validate_attributes(Ctl *ctl, Node *element_node) {

   char         message[500]; 
   Node        *attr, *save_node;
   ElementSpec *element_spec;
   AttrList    *attspeclist;
   AttrSpec    *attspec;
   char        *aname, *ename;
   

  /* get element spec descriptor..   */
   element_spec = get_element_spec(ctl, &element_node->name);
   if(element_spec == NULL) {
        do_validity_subst_error(ctl, &element_node->position,
                   "Element type '%s' not defined", &element_node->name);
        return; 
   }

   /* and attlist descriptor it locates              */
   attspeclist = element_spec->attrlist; 
    
   /* Match each declared attribute against current attlist */ 
   /* if found, normalize and validate                      */ 
   /* if not found and has default, substitute default      */ 
   /* if not found and required, emit error                 */
   if(attspeclist == NULL) {
       if(element_node->first_attr != NULL) {
            do_validity_subst_error(ctl, &element_node->position,
                          "Attributes of element '%s' not defined",
                           &element_node->name);  
        }
        return;
  }
             
   for(attspec = attspeclist->first_attr_spec; 
      attspec; attspec = attspec->next)  {  

      /*if more than one decl for attr, ignore all but first*/ 
      if(attspec->is_duplicate) continue; 

      for(attr = element_node->first_attr; attr; attr=attr->next){
          if(attr->index  == attspec->index) break; 
      }

      if(attr != NULL) { 
         attr->temp_mark = 1;

         normalize_and_validate_attribute(ctl, attspec,
                        &attr->name, &attr->value, &attr->position, 0);

         /* make sure no duplicate attributes       */ 
         for( attr=attr->next; attr; attr = attr->next) { 
            if(attr->index  == attspec->index) {
               attr->temp_mark = 1;
               do_error(ctl, &attr->position, &attr->position,  
                        formation_error, no_recover, 
                       "Attribute appears more than once in same "  
                       "element tag");
             }
          }
            
      } 

      else if(attspec->default_kind == AttrRequired) {
            do_validity_subst_error(ctl, &element_node->position,
                          "Required attribute '%s' missing from element",
                           &attspec->attr_name);  

      } 

      else if(attspec->default_kind != AttrImplied) { 
          /* use default value and if id/idref check for definition, etc*/ 
          attr = (void *)calloc(1, sizeof(Node));
          attr->node_type = ATTRIBUTE;
          lclmemcpy(&attr->name, &attspec->attr_name, sizeof(XString));
          lclmemcpy(&attr->value, &attspec->default_value, sizeof(XString));
          attr->double_chars = ctl->is_double;
          attr->index = attspec->index;
          save_node = element_node->first_attr;
          element_node->first_attr = attr;
          attr->next = save_node;
          attr->temp_mark = 1;

          validate_attribute_special(ctl, attspec, &attr->name,        
                                     &attr->value,  
                                     &element_node->position, 0, 1);
     }

   } /* end for( attspec = ) */

  /* check that all attributes have been processed; if not,  */   
  /* indicates that missing definition                       */ 
  for(attr = element_node->first_attr; attr; attr=attr->next){
      if(attr->temp_mark) attr->temp_mark = 0;
       else {
          /* approximation.. assumes element & attribute names latin_1 */
          /* for name print                                           */
           aname =  xstr2cstr(&attr->name, ctl->is_double);
           ename =  xstr2cstr(&element_node->name, ctl->is_double); 
           sprintf(message, "Missing declaration for attribute %s" 
                " of element %s", aname, ename);
           valid_error(ctl, &attr->position, message); 
           free(aname); free(ename); 
         }
    } /* end for(attr.. */ 
   
    

}
    


 /*********************************************************************/ 
 /* Validate contained element types for permissibility and  ordering */
 /* Follow all paths, e.g., for  graph 1-a->1 1-a->2                 */
 /* Currently assumes non-epsilon-free, non-deterministic fsm        */
 /* so validation includes backtracking                             */
static int find_path(Ctl *ctl, State *current_state, Node *current_elem,
                     int *empty) { 

   int  inlabel, found;  
   Arc *arc, *save_epsilon_arc;
   Node *this_element, *next_element;

   found = 0;
   
   this_element = current_elem;

   while(this_element != NULL) {
       
        next_element = this_element->next;

        switch (this_element->node_type) {
           case TEXT: 
             inlabel = CDATA_INDEX;
            *empty = 0;
             found = 1;
             break; 
           case ELEMENT: 
             inlabel = this_element->index;
            *empty = 0;
             found = 1;
             break;
           case PI:
           case COMMENT:
             this_element = next_element;
             break;
           default:
              do_phys_error(ctl,
                "Error: out of place markup or System Error", 0);
              return 1; 
         }

      if(found) break;
      continue;
   }

   if(this_element == NULL) {

       if(current_state->is_final) return 1; 

       /* look for sequence of epsilons in path */
       inlabel = EPSILON_INDEX;  
       next_element = NULL;
   }
     
   save_epsilon_arc = NULL;
   for(arc = current_state->arc_set; arc; arc= arc->next) {  
         /* If match  ...*/
         if (arc->label == inlabel) {
            if(find_path(ctl, arc->destination, next_element, empty))
               return 1;
         }

         else if(arc->label == EPSILON_INDEX) save_epsilon_arc = arc;
    }
    if(save_epsilon_arc) {
      if(find_path(ctl, save_epsilon_arc->destination, this_element, empty))
           return 1;
   }
    return 0;
 }
    
/*************************************************************/
void validate_content(Ctl *ctl, Node *element) {

  ElementSpec *element_spec;
  int          empty = 1;

  element_spec = get_element_spec(ctl, &element->name); 
  if(element_spec == NULL) return;  /*will already have done error*/
  
  /* if is declared empty but has content */
  if(element_spec->content_type == CtnKindEmpty) {  
     if(element->first_content != NULL) 
        do_validity_subst_error(ctl, &element->position, 
           "Element '%s' declared empty but has content",
            &element->name);
       return;
  }

 /* Note: some imported tests assume an implicit kleene star for mixed  */

#ifdef LOOSE_CONTENT
  if(element_spec->content_type == CtnKindMixed &&
    element->first_content == NULL) return;
#endif

  if(element_spec->content_type == CtnKindAny) return; 

  /* Validate contained elements for  permissibility and  ordering */
  if(!find_path( ctl, element_spec->first_graph_state,
       element->first_content, &empty)) {

#ifdef LOOSE_CONTENT
      /* same kleene star assumption */ 
      if( empty &&  element_spec->content_type == CtnKindMixed) return;
      else 
#endif

          do_validity_subst_error(ctl, &element->position, 
           "Content of element '%s' inconsistent with declaration",
            &element->name);
  } 
}
    
