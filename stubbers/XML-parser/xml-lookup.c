
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

#include <assert.h>
#include <stdlib.h>
#include <string.h> 
#include "xml-types.h"
#include "xml-func.h"

/********************************************************************/
/* TBD: Make this proc real... keep track of heap allocations, etc */
void *xml_heap_alloc(Ctl *ctl, size_t size) { 
   return(calloc(1, size));
}


/********************************************************************/
/* Hash procedures   (for ids)                                      */
/*********************************************************************/
int btree_get_start_index(Ctl *ctl, LookupType lu_type) {

  switch(lu_type) {
      case lu_element:
          return  2;
      case lu_attribute:
           return 3;
      case lu_pe: 
           return 4;
      case lu_ge: 
           return 5;
      case lu_notation:
           return 6;
       
      default: assert(0);
   }

   assert(0);
   return 0;   
}


/*********************************************************************/
/* BTree Name search routine.                                        */
/* returns index in specified btree, indicated by lu_type            */
/* Currently, btrees hold locators of definitions                    */ 
/* if new, 'outvalue' will be loc in string_buf_pos                  */
/* Note: same btree structure holds many logical btrees, starting   */ 
/* at different positions                                           */
int btree_search(Ctl* ctl, LookupType lu_type, XString *invalue,
              XString *outvalue, int makenew) {

    XString *stored_value;
    int     initial, compare;
    uint16  index;
    XmlBTree *btree;
    BTreeCtl *treectl;
    
    treectl = &ctl->btree_ctl;

    index = btree_get_start_index(ctl, lu_type);

    btree = &(treectl->btree[0]);

    initial = 0;
    if( btree[index].entry_string.loc == NULL) {
            if(!makenew) return 0;
            initial = 1;
    }
    else while(1) {
       stored_value = &(btree[index].entry_string); 
       compare = XStringsCompare(ctl, stored_value, invalue);
       if(compare==0) {
             lclmemcpy(outvalue, stored_value, sizeof(XString));
#ifdef DEBUG_BTREE 
    printf ("\nindex = %d", index);
#endif
             return index;
       }
       if(compare > 0) {
           if(btree[index].larger) { index=btree[index].larger; continue; }
           else if (!makenew) return 0;
           else { btree[index].larger = treectl->first_unused; break; }
         }
        /* compare must be < 0 */
        if(btree[index].smaller) { index=btree[index].smaller; continue; }
        else if (!makenew) return 0;
        else { btree[index].smaller = treectl->first_unused; break; }
    }  /* end while 1 */

    add_to_string_buf(ctl, invalue, outvalue);

    if(treectl->first_unused >= (treectl->allocation) -1) {
         treectl->allocation = treectl->allocation + 100;
         treectl->btree = (void *)realloc(treectl->btree,
                      treectl->allocation * sizeof(XmlBTree));
         btree = treectl->btree;
         lclmemset(&btree[treectl->first_unused], 0,
          (treectl->allocation - treectl->first_unused)
              *sizeof(XmlBTree));
    }

    if(!initial) index = treectl->first_unused;
    lclmemcpy(&(btree[index].entry_string), outvalue, sizeof(XString)) ;
    (treectl->first_unused)++; 
    btree[index].smaller = btree[index].larger = 0;
    btree[index].definition = NULL;
    btree[index].lu_type = lu_type;  /*REMOVE AFTER FULL DEBUG */ 
#ifdef DEBUG_BTREE 
    printf ("\nindex = %d", index);
#endif
    return (index);
}


/*********************************************************************/
/* add pointer to btree entry for index to node representing item */
/* do not override .. to handle precedence of internal subset    */
/* TBD: multiple defs one file                                  */
void btree_add_def (Ctl *ctl, LookupType lu_type, Node *node,uint16 index) {
 
   XmlBTree *tree; 
   BTreeCtl *treectl;

   treectl = &ctl->btree_ctl;
   tree    = treectl->btree;

   assert( tree[index].lu_type == lu_type); 
   if(tree[index].definition == NULL)  tree[index].definition = node;
  /* if redefining built-in,  warn*/ 
  /* Note: cannot check for equivalence because an external */
  /*def of &amp  will not be equal to internal one.. To do the*/
  /* latter, have to dummy up physical buffer and replace */
   else if (tree[index].definition->node_type == ENTITYDECL
           && tree[index].definition->built_in) {
         do_phys_error(ctl, "WARNING: built-in entity redefined, " 
                          "redefinition ignored", 0);
   }
}


/*********************************************************************/
Node *btree_get_def(Ctl *ctl, LookupType lu_type, XString *name) {

   XmlBTree *tree; 
   BTreeCtl *treectl;
   XString   dummy_value;
   uint16    index;

   treectl = &ctl->btree_ctl;
   tree    = treectl->btree;

   index = btree_search(ctl, lu_type, name, &dummy_value, 0);
   if(index == 0) return NULL;
   return(tree[index].definition);

} 

/*********************************************************************/
Node *btree_get_def_from_index(Ctl *ctl,LookupType lu_type,uint16 index) {
   XmlBTree *tree; 
   BTreeCtl *treectl;
    
   treectl = &ctl->btree_ctl;
   tree    = treectl->btree;
   assert(tree[index].lu_type == lu_type);
   return(tree[index].definition);
}
   
   
/*********************************************************************/
ElementSpec *get_element_spec(Ctl *ctl, XString *name) {     
      
     Node *node;
     node = btree_get_def(ctl, lu_element, name); 
     if(node == NULL) return NULL;
     return(ElementSpec *)node->internal_rep;
}


/*********************************************************************/
/* name here includes entity qual ... &/% */ 
Entity* get_ref_entity_def(Ctl* ctl, int parameter_entity, XString name,
                           int strip) {

   XString     name1, dummystring;
   Node       *defnode;
   LookupType  lu_type;
   XmlBTree    *tree;
   BTreeCtl    *treectl;
   uint16       index;

   lclmemcpy(&name1, &name, sizeof(XString));
  if(strip) {
     name1.loc += 1 + ctl->is_double; 
     name1.len -= 1 +ctl->is_double;
   }
   

   treectl = &ctl->btree_ctl; 
   tree = treectl->btree;
   if(parameter_entity) lu_type = lu_pe;
    else lu_type = lu_ge;
  
   index = btree_search(ctl, lu_type, &name1, &dummystring, 0);
   if(index == 0) return 0;

   defnode = tree[index].definition;
   assert(defnode->node_type == ENTITYDECL);
   return ( (Entity*)defnode->internal_rep);

 }


/*********************************************************************/

/* gets character string based on index in btree */
uchar *get_element_name(Ctl *ctl, uint16 index) {

   XmlBTree *tree; 
   BTreeCtl *treectl;

   treectl = &ctl->btree_ctl; 
   tree = treectl->btree;
   return((uchar*) xstr2cstr(&tree[index].entry_string, ctl->is_double));  
}
     

/********************************************************************/
/* Hash procedures   (for ids)                                      */
/* Start with cstring                                               */
/* create a word containing low-order 4 bits of first 8 characters */
/* (scrambled)                                                       */
/* (Converted from a routine using shifting to accomodate alternative*/
/* numeric representations. Results look ok in casual test but could */
/* use further checking                                              */
static uint32 squash_base_form(char * base_form) {
      uint32  i;
      int  theint;

      theint = 0;
      /* mask out high order bits of first four chars */
      /* and shift left 4                         */
      for(i=0; i<4 && i < strlen(base_form); i++) 
         theint = (theint * 16) + (base_form[i]%16); 
      /* mask out high order bits of next four chars */ 
      /* and or with first                             */ 
      for(i=4; i< 8 && (i < (strlen(base_form))); i++) 
         theint = (theint * 16) + (base_form[i]%16); 

      return (theint);
}
         
      
 static uint16 hash_base_form(uint32 squashed_base_form) {
      /* uses (possible misinterpretation of) Knuth method */
      uint32  hashmult = 618033988, hash_result1;
      uint16  hash_result2;

      hash_result1 = hashmult * squashed_base_form;
      hash_result2 =  hash_result1% (IDHASHSIZE-1);
      return (hash_result2);
    }


uint16 find_lex_hash_number(char *base_form) {
   uint32 squashed_base_form;
   uint16 hash_number;

     squashed_base_form = squash_base_form(base_form);
     hash_number = hash_base_form(squashed_base_form);
#ifdef HASH_TEST
     printf("\n%d", hash_number);
#endif
     return(hash_number);
 }

/* Assumes that loc of string in permanent storage */ 
IdEntry *make_new_id_entry(Ctl *ctl, XString *in_string) {
      
   IdEntry *new_entry_loc;

   /* Expand this usage to other structures */
    if(ctl->current_ids_used >= ctl->current_id_alloc) {
       ctl->current_id_heap = (IdEntry *)
                     xml_heap_alloc(ctl, IDHEAPALLOC * sizeof(IdEntry)); 
       ctl->current_id_alloc = IDHEAPALLOC;
       ctl->current_ids_used = 0;
    }
    new_entry_loc = &ctl->current_id_heap[ctl->current_ids_used];
    ctl->current_ids_used++; 
    lclmemcpy(&(new_entry_loc->id_string), in_string, sizeof(XString)); 
    return new_entry_loc;
}


/*searches, and makes new entry if "make new" */
IdEntry *search_id_hash(Ctl *ctl, XString *in_string, int make_new) {
 
   uint16           hash_number;
   char            *cstring_loc;
   IdEntry         *id_chain, *last_id_chain; 

   /* turn into cstring */
   cstring_loc = xstr2cstr(in_string, ctl->is_double);

   /* get hash number */
   hash_number =find_lex_hash_number(cstring_loc);
   free(cstring_loc); 

  /* search for hw match */
   id_chain = ctl->id_hash_table[hash_number];

   if(id_chain == NULL && make_new) {
       id_chain = make_new_id_entry(ctl, in_string); 
       ctl->id_hash_table[hash_number] = id_chain;
       return id_chain;
   }

   for(; id_chain; id_chain= id_chain->next) {
       last_id_chain = id_chain;
       if(XStringsEq(ctl, in_string, &id_chain->id_string)) 
            return id_chain;
   }

   if(make_new) {
       id_chain = make_new_id_entry(ctl, in_string); 
       last_id_chain->next = id_chain;
       return id_chain;
     }

    return NULL;
 } 


#ifdef undef
/* replaced procedures */
/**********************************************************************/
/* Hash procedures   (for ids)                                      */
/* Start with cstring                                               */
/* create a word containing low-order 4 bits of first 8 characters */
/* (scrambled)                                                       */
static uint32 squash_base_form(char * base_form) {
    /* zero  low order 4 bits of first eight characters */
      const char mask = 0x0f;
      int  i;
      union U1 {
         char     thechars[4];
         uint32   theint;
       } U;
      U.theint = 0;
      /* mask out high order bits of first four chars */
      /* and shift left 4                         */
      for(i=0; i<4 && i < strlen(base_form); i++) 
         U.thechars[i] = (base_form[i] & mask) << 4;
      /* mask out high order bits of next four chars */ 
      /* and or with first                             */ 
      for(i=0; i<4 && (i+4 < (strlen(base_form))); i++) 
          U.thechars[i] |= (base_form[i+4] & mask); 

      return (U.theint);
}
         
      
 static uint16 hash_base_form(uint32 squashed_base_form) {
      /* uses (possible misinterpretation of) Knuth method */
      uint32  hashmult = 618033988;
      union U1 {
            uint16  hash_parts[2]; 
            uint32  hash_result;
           } U;
      /* shift knuth constant so most significant bit at left */
      while(hashmult < 0x80000000) hashmult = hashmult << 1;
      /* use fact that result will be low order word */
      U.hash_result = hashmult * squashed_base_form;
      if(U.hash_parts[0] >= IDHASHSIZE) U.hash_parts[0]=U.hash_parts[0]%
                                      IDHASHSIZE; /*kludge*/
      return (U.hash_parts[0]);
    }


uint16 find_lex_hash_number(char *base_form) {
   union U1 {        
           uint16 prefix_squashed_form[2];
           uint32 squashed_base_form;
         } U;
   uint16 hash_number;  
     U.squashed_base_form = squash_base_form(base_form);
     hash_number = hash_base_form(U.squashed_base_form);
     return(hash_number);
 }
#endif


