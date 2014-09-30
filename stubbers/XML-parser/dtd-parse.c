
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

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "xml-types.h"
#include "xml-func.h"

static int parse_dtd_RE(Ctl* ctl, Pos *cur_pos, int initial,
                 OpTree **content_tree, int *ctns_pcdata); 

static void to_graph(Ctl *ctl, Net *net, OpTree *tree, State *first_state,
      State **last_state);

/********************************************************************/
/* DEBUGGING                                                        */
/********************************************************************/

static void print_optree(Ctl *ctl, Pos *pos, int initial,  OpTree *tree) { 

      OpTree *subtree;
      LineIndexEnt *line_ent;
      int           dummy;

      printf("\n\nTree at %p :", tree);  
      if(initial) {
         xml_get_line_data(ctl, pos, &line_ent, &dummy);
         printf(" Line %d ", line_ent->line_number+1);
     }

      printf("\n Op = %d, Id = %d", tree->op, tree->id);
     if(tree->op == OpKindType)
         printf(" ElementTYpe =  %s", get_element_name(ctl, tree->id));
      printf("\nChildren");
      for (subtree = tree->first_opnd; subtree; subtree=subtree->next) {
           printf(" %p ,", subtree);
      } 
      for (subtree = tree->first_opnd; subtree; subtree=subtree->next) {
           print_optree(ctl, NULL, 0, subtree);
      }
 }


void print_xml_element_net(Ctl* ctl, Pos *pos, Net *net) {

  int           i, dummy;
  State        *state;
  Arc          *arc;
  char        *label, *final;
  LineIndexEnt *line_ent;

  printf("\n\nNet %p :", net);  
  xml_get_line_data(ctl, pos, &line_ent, &dummy);
  printf(" Line %d ", line_ent->line_number+1);

  /* Number states in order of creation */  
  i = 0;
  for(state = net->first_state; state; state=state->next) { 
     state->seq_no = i;
     i++;
  }  

  /* list arcs */
  for(state = net->first_state; state; state=state->next) { 
     for(arc = state->arc_set; arc; arc=arc->next) {
         if(arc->label == 0) label = "EPSILON";
         else if(arc->label == 1) label = "CDATA";
         else label = (char *)get_element_name(ctl, (uint16)arc->label);
         if(arc->destination->is_final) final = "F";
         else final = " ";
         printf( "\n  %d  %-12s %d%s", 
             state->seq_no, label, arc->destination->seq_no, final);

       }
   }
  
 }


/*********************************************************/
void build_in_entities(Ctl *ctl) { 

 XString entityname; 
 XString entityvalue; 
 Node               *this_node;
 Entity             *entity;

/* note.. ampersand definition diverges from literal def*/
/* &$38:#38 because not read in and thus substitution not made */
/* during  processing of source buffer                        */


 char *names[5] =  {"lt", "gt", "amp","apos", "quot"};  
 char *values[5] = {"&#60;", "&#62;", "&#38;", "&#39;", "&#34;"};
 int   i;
 
 for(i = 0; i < 5; i++) { 

    cstr2xstr(names[i],  &entityname); 
    cstr2xstr(values[i], &entityvalue);

    entity = (void *)calloc(1, sizeof(Entity));
    entity->internal = 1;
    entity->index = btree_search(ctl, lu_ge, &entityname,
                    &entity->name, 1); 
    add_to_string_buf(ctl, &entityvalue, &entity->entity_string);

    /*will be disconnected                   */
    /* but tbd.. nodes will be put in heaps */
    this_node = (void *)calloc(1, sizeof(Node)); 
    this_node->node_type = ENTITYDECL;
    this_node->internal_rep = (void *)entity;

    this_node ->index = entity->index;
    this_node->built_in = 1;
    lclmemcpy(&this_node->name, &entity->name, sizeof(XString));

     /* add ref to element def in element btree */
    btree_add_def(ctl, lu_ge, this_node, entity->index); 

    free(entityname.loc);
    free(entityvalue.loc);

   }

}



/*********************************************************************** */ 
/* primitive ops for element content model graph creation & modification */
static State *make_state(Ctl *ctl, Net *net) { 

      /* next states for debug only */
      State *new_state, *state;
      new_state = (void *)calloc(1, sizeof(State));
      for(state = net->first_state; state; state = state->next) {
           if(state->next == NULL) {
             state->next = new_state;
             break;
           } 
       }
             
      return (new_state); 
  }


Arc  *make_arc(Ctl* ctl, int label, State *from, State *to)  {
  
  Arc *arc, *temp_arc;

  arc = ((void *)calloc(1, sizeof(Arc)));
  arc->label = label; 
  arc->destination = to; 
  /* add arc to arc_set of source */ 
  temp_arc = from->arc_set;
  from->arc_set = arc; 
  arc->next = temp_arc;
  return arc;
}

/*************************************************************************/
static int check_dtd_WS(Ctl *ctl, Pos *cur_pos, Pos *pos,
                         char *message) {
     if(!get_WS(ctl, pos)) {
         dtd_error(ctl, cur_pos, pos, message);
         return 0;
      } 
     return 1;
}

/*************************************************************************/
/* ExternalId syntax is                                               */
/* (( SYSTEM  SystemLiteral) | (PUBLIC PubidLIteral SystemLiteral)    */
/* but SystemLiteral not required in PUBLIC id for NOTATION dcl      */ 
static int parse_external_id(Ctl *ctl,  Pos *pos, ExternalID *ext_id,
             int small_pubid_ok) {

    char *ext_kws[2] = {"PUBLIC", "SYSTEM"};
    int    ext_type, nosystem;
    char  *c_name, *converted_name;

    ext_id->systemid.loc = ext_id->publicid.loc = NULL;
    ext_id->use_id = NULL;
    nosystem = 0;

    ext_type = parse_keyword(ctl, pos, 2, ext_kws);
    if(ext_type < 0) return 0;

     
     if(!check_dtd_WS(ctl, pos, pos, "Missing WS in ExternalID")) 
             return 0;

     if(ext_type == 0)  /* PUBID*/ {
          ext_id ->is_system = 0;
          if(!parse_qstring(ctl, pos, &ext_id->publicid, 0)) {
               dtd_error_1(ctl, pos, pos, "Error in PUBID literal");
               return 0;
           }

          /* should add check whether small_pubid ok  here*/ 
          if(!get_WS(ctl, pos)) {
             nosystem = 1;
             if(!small_pubid_ok) {
               dtd_error_1(ctl,pos,pos,"Error in PUBLIC/SYSTEM literal");
              return 0;
              }
           }
           else get_opt_WS(ctl, pos); 

      }

    else ext_id->is_system = 1;

    if(!nosystem) {
        if(!parse_qstring(ctl, pos, &ext_id->systemid, 0)) {
           if(! ( (ext_type == 0) && small_pubid_ok)) {
              dtd_error(ctl, pos, pos, "Error in PUBLIC/SYSTEM literal");
              return 0;
           }
        }
     }

 
     /*if caller of parser has provided resolution routine for   */
     /* public id, try it                                        */  
     if( ext_id->is_system == 0 && ctl->convert_pubid_proc != NULL) {
         c_name = xml_get_cstring( (char *)ext_id->publicid.loc,
                              ext_id->publicid.len, ctl->is_double, NULL);
         converted_name = ((XmlCvtPublic) ctl->convert_pubid_proc)(c_name); 
         if(converted_name != NULL) ext_id->use_id = converted_name;
     }
    if(ext_id->use_id == NULL) {
     if(ext_id->systemid.loc != NULL) {
         ext_id->use_id = xml_get_cstring( (char *)ext_id->systemid.loc,
                              ext_id->systemid.len, ctl->is_double, NULL);
      }
      /* otherwise if not notation declaration, error */
      else if(!small_pubid_ok) {
             dtd_error_1(ctl, pos, pos, "Error in External id");
             return 0; 
      }
    }
  
    return 1;
 }

/***********************************************************************/
void expand_attribute_defaults(Ctl *ctl, Pos *pos ,
                              XString *input, XString *output ) {

   
  struct Chain_ {
         uchar         *loc;  
         int            len;
         int            pos; 
         Entity        *entity; 
         struct Chain_ *parent;     
         struct Chain_ *next;
  };
 typedef     struct Chain_ Chain; 
 Chain      *firstinput, *curinput, *entinput,*reminput, *tempinput;
 Chain      *nextinput;
 int         in_entity_ref, in_entity_ref_begin, end_found, addlen, i; 
 int         save_ref_begin, save_ref_len,is_scan_special, result_len;  
 uchar      *this_char;
 SpecialEnum thespecial;
 XString     temp_str;
 Entity     *entity;

 in_entity_ref =  in_entity_ref_begin = end_found = 0; 
 
 firstinput = (void*) calloc(1,sizeof(Chain));
 firstinput->loc = input->loc;
 firstinput->len = input->len;
 
 curinput = firstinput; 
 while(1) {

     if(curinput->pos >= curinput->len) {
         if(curinput->next == NULL) break;
          else curinput = curinput->next;
     }
   
     this_char = &(curinput->loc[curinput->pos]);
     is_scan_special = get_specials_code(ctl, this_char, &thespecial, 0, 0);

     if(!in_entity_ref) { 
         if(is_scan_special &&  thespecial == AMPERSAND_CHAR){
              in_entity_ref = in_entity_ref_begin = 1; 
              save_ref_begin = curinput->pos;
         }
     }
 
     else  {
       if(is_scan_special && thespecial == SEMICOLON_CHAR) { 
          end_found = 1;
          save_ref_len = curinput->pos -save_ref_begin +
                         (1 + ctl->is_double);
       }  
       else if( (in_entity_ref_begin && name_begin_char(ctl, this_char)) 
          || ( (!in_entity_ref_begin) && name_char(ctl, this_char))) 
           in_entity_ref_begin = 0; 

     } 

     (curinput->pos) += 1 + (ctl->is_double);

     if(!end_found) continue; 

     /* here if have found entity ref */

     temp_str.loc = curinput->loc + save_ref_begin;
     temp_str.len = save_ref_len - (1 + ctl->is_double);
     entity = get_ref_entity_def(ctl,0, temp_str, 1); 
     
     if(entity == NULL) {
         do_error(ctl, pos, pos, validation_error, no_recover,
           "Reference to undefined entity," 
           " entity name substituted containing illegal character &");
         in_entity_ref = in_entity_ref_begin = end_found = 0;  
         continue;
     }  

     if(!entity->internal) { 
          do_error(ctl, pos, pos, validation_error, no_recover,
             "Reference to parsed entity not permitted in attribute value");
          in_entity_ref =  in_entity_ref_begin = end_found = 0; 
          continue; 
     }


     /* check for circular reference */
     tempinput = curinput;
     while(tempinput != NULL) {
         if(tempinput->entity != NULL && tempinput->entity == entity) {
                do_error(ctl, pos, pos, validation_error, no_recover,
                      "Circular entity reference");
                in_entity_ref =  in_entity_ref_begin = end_found = 0; 
                break; 
         }
         tempinput = tempinput->parent;

     }
     if(!in_entity_ref) continue;
     
     entinput = NULL;
     if(entity->entity_string.len != 0) {
         entinput = (void*) calloc(1, sizeof(Chain));
         entinput->loc = entity->entity_string.loc;
         entinput->len = entity->entity_string.len;
         entinput->parent = curinput; 
         entinput->entity = entity;
     }

     
     reminput = NULL;
     if(curinput->len > curinput->pos) { 
       reminput = (void*) calloc(1, sizeof(Chain));
       reminput->loc = curinput->loc + curinput->pos; 
       reminput->len =  curinput->len - curinput->pos;
       reminput->parent = curinput;
     }  

     curinput->pos = save_ref_begin;

     if(entinput != NULL){
            curinput->next = entinput;
            curinput = entinput;
            if(reminput!=NULL) curinput->next = reminput; 
     }
     else if(reminput != NULL) {
           curinput->next = reminput;
           curinput = reminput;
     }

     else break;

     in_entity_ref = in_entity_ref_begin = end_found = 0;  

  }/* end while 1*/ 

 /* now collect results */

  if(firstinput->next == NULL)  {
             *output = *input;
              return;
  }
              
  nextinput = firstinput;
  result_len = 0; 
  while(nextinput != NULL) {
     result_len += nextinput->pos;
     nextinput = nextinput->next;
  }

  temp_str.loc = (void *)malloc(result_len); 
  temp_str.len= 0;
  nextinput = firstinput;
  while(nextinput != NULL) {
       addlen=  nextinput->pos;
       for(i = 0; i < addlen; i += 1 + ctl->is_double) { 
             append_xchar(ctl, &temp_str, &nextinput->loc[i]);
      }
      tempinput = nextinput->next;      
      free(nextinput); 
      nextinput = tempinput; 
  }

  add_to_string_buf(ctl, &temp_str, output); 

  return;
}

/***********************************************************************/
/* check attribute defaults for validity */ 
void process_att_defaults(Ctl *ctl, AttrList *alist) {  

      AttrSpec *as;

      for(as = alist->first_attr_spec; as; as= as->next) {

        switch (as->default_kind) {
            case AttrRequired: case AttrImplied: 
                 break;

            case AttrFixed: case AttrOrdinary: 
                  expand_attribute_defaults(ctl, &as->pos,  
                             &as->default_value, &as->default_value);
                  normalize_and_validate_attribute (ctl,  as, 
                         &as->attr_name,  &as->default_value,      
                         &as->pos, 1);
                  break;
            default: assert(0);
         }
      }
  } 


/***********************************************************************/
/* Attach attlists to element specs. Merge if more than one attlist    */
/* for an element, marking repeated attributes as to be ignored       */
static void process_attlists(Ctl *ctl) {
    DtdCtl      *dtd_ctl;
    AttrList    *alist, *alist1;
    AttrSpec    *as, *as1;
    Node        *element; 
    ElementSpec *element_spec;
    char         message[100], *ename, *aname;

    dtd_ctl = ctl->dtd_ctl;
    for(alist= dtd_ctl->first_attr_list; alist; alist = alist->next){

       element = btree_get_def_from_index(ctl, lu_element, alist->index);
       if(element == NULL) return;

       element_spec = (ElementSpec *)element->internal_rep;

       /* if element doesn't yet have associated attribute list */
       /* just link                                             */ 
       if(element_spec->attrlist == NULL) {
             element_spec->attrlist = alist;
             process_att_defaults(ctl, alist);   
             continue;
       }

       /* But if it does...               */
       else {
          ename = xstr2cstr(&element_spec->name, ctl->is_double); 
          sprintf(message, "Multiple attribute lists for element '%s'",
                   ename); 
          do_warning(ctl, &alist->attr_list_node->position, message); 
          free (ename); 

          /*link new attributes after old */ 
          alist1 = element_spec->attrlist; 

          /* scan for dups and mark second as to be ignored */ 
          for(as = alist->first_attr_spec; as; as= as->next) {
              for(as1=alist1->first_attr_spec; as1; as1= as1->next) {
                 if(as->index == as1->index){ 
                     as->is_duplicate = 1;
                     ename = xstr2cstr(&element_spec->name, ctl->is_double); 
                     aname = xstr2cstr(&as->attr_name, ctl->is_double); 
                     sprintf(message, "Multiple declarations for"
                        " attribute '%s' of element '%s'", aname, ename); 
                     free(ename);
                     free(aname);
                     do_warning(ctl, &alist->attr_list_node->position,
                                 message);
                  } 
               }
           } 

         /* now link lists putting second after first */
          for(as1 = alist1->first_attr_spec; as1; as1= as1->next) {
             if(as1->next == NULL) { 
               as1->next = alist->first_attr_spec;
               break;
             }
          }
        } /* end else */

   } /* end for alist */ 

}

/********************************************************/
void parse_dtd_ignore (Ctl *ctl, Pos *cur_pos) {

   uchar  begin_chars[3] = {'<', '!', '['};
   uchar  end_chars[3] = {']', ']', '>'};

   uchar  *next;
   Pos  pos;
   int  begin_ct, found_begin, end_ct, found_end; /*chars in begin/end seqs*/
   int  ignore_begins; /*unmatched ignore sections begins*/

   lclmemcpy(&pos, cur_pos, sizeof(Pos));

   ignore_begins = 1; /* number of non-matched <![ */ 

   found_begin = found_end = 0;  /*number of chars found in sequence */
   begin_ct = 3;
   end_ct = 3;

   pos.ES_state = pos.ES_state | ES_CDATA;

   while(1) {
       /*fails only after eot */ 
       next = next_char(ctl, &pos, CT_Any, 0, 1);
       if(next == NULL) {
          do_error(ctl, &pos, &pos, fatal_error,
            no_recover,
           "Error in conditional section");
     }
 
       /* check for end of data*/
        if(compare_with_special(ctl, next, end_chars[found_end]))
            found_end++;
        else {
            for(found_end = found_end -1; found_end >= 0; found_end--) {
              if(compare_with_special(ctl, next,end_chars[found_end])) {
                   found_end++;
                   break;
                 }
             }
             if(found_end < 0) found_end = 0;
        } 
        if(compare_with_special(ctl, next, begin_chars[found_begin]))
            found_begin++;
 
        if(found_begin == begin_ct){
             found_begin = 0;
             ignore_begins++;
         }

        if(found_end == end_ct) {
             found_end = 0;
             ignore_begins--;
             if(ignore_begins <= 0) break;
        }
   }

   pos.ES_state = cur_pos->ES_state;
   lclmemcpy(cur_pos, &pos, sizeof(Pos));
   return;
  }


/***********************************************************************/
/* for INCLUDE / IGNORE                                                */
int parse_conditional( Ctl    *ctl,
                      Pos    *cur_pos,
                      Node    *parent_node,
                      Node   **dummy_node
                 ) {
 
   char          *conds[2] = {"INCLUDE", "IGNORE"};
   Pos            pos;
   int            result;
   MarkupType     markup_type;
   MarkupReturn   mr;
   Node          *markup_node; 
 
    lclmemcpy(&pos, cur_pos, sizeof(Pos));
    get_opt_WS(ctl, &pos);
 
    result =parse_keyword(ctl, &pos, 2, conds);

    /*if recognized */
    if(result >= 0) {
       get_opt_WS(ctl, &pos);
       /* look for [ */
       if(!next_char(ctl, &pos, CT_Specific,LEFT_SQUARE_CHAR, 1)) {
           do_error(ctl, cur_pos, &pos, dtd_syntax_error,
              no_recover, "Error in conditional, will be treated as IGNORE");
              result = 1;
       }
       else if(pos.ES_state & ES_internal_dtd){ 
            do_error(ctl, cur_pos, &pos, dtd_syntax_error,
               no_recover, "Conditional not permitted in internal dtd, "
                            "will be treated as IGNORE");
             result = 1;
       }
     } 
        
     else {
        do_error(ctl, cur_pos, &pos, syntax_error,
            no_recover,
            "Structure beginning <![ in dtd may only be INCLUDE/IGNORE"
               " will be treated as IGNORE");
       result = 1;
     }  
   
    /* if IGNORE, skip until matching ]]>  */
    if(result == 1) { 
         /* if 0 return reached end of input? */
         parse_dtd_ignore (ctl, &pos);
         pos.ES_state = cur_pos->ES_state;
     }
         
    /* INCLUDE */ 
    else while (1){
        get_opt_WS(ctl, &pos);
        mr = parse_markup(ctl, &pos,  parent_node,
                            &markup_type, &markup_node);
        if(mr == NotMarkup) { /* maybe end of conditional section */
           get_opt_WS(ctl, &pos);
           if( (next_char(ctl, &pos, CT_Specific, RIGHT_SQUARE_CHAR, 1) 
                 != NULL)
              && (next_char(ctl, &pos, CT_Specific, RIGHT_SQUARE_CHAR, 1)
                 != NULL)
              && (next_char(ctl, &pos, CT_Specific, RIGHT_ANGLE_CHAR, 1) 
                 != NULL))
              break;

            else { 
                   dtd_error(ctl, &pos, &pos,
                            "Markup error in conditional section");
                   break;
            }
        }
        else {
           switch(markup_type) {
               case MarkupTypeElementDecl:
               case MarkupTypeAttlistDecl:
               case MarkupTypeEntityDecl:
               case MarkupTypeNotationDecl:
               case MarkupTypePI:
                   if(ctl->dtd_subord_begin == NULL)
                        ctl->dtd_subord_begin = markup_node;
                    else ctl->dtd_last_subord->next = markup_node;
                    ctl->dtd_last_subord = markup_node;
                    break;
               case MarkupTypeComment:
               case MarkupTypeConditional:
                  break;
               default:
                 dtd_error(ctl, cur_pos,&pos,"Misplaced markup type in dtd");
                 return ErrorMarkup;
           } /* end switch */
       } /* end else */

    } /*end while(1) */
    
    *dummy_node = NULL;
    lclmemcpy(cur_pos, &pos, sizeof(Pos));
    return 1;
 }
           

/************************************************************************/
/* TBD: adjust definitions so use most recent, etc                      */ 
int parse_doctype(Ctl *ctl, Pos *cur_pos, Node *parent_node, Node **node) {

    Pos            pos;  
    DtdCtl        *dtd_ctl;
    Node          *this_node, *markup_node, *prev_subord_node;
    XString        ename; 
    MarkupType     markup_type;
    MarkupReturn   mr;
    uint16         eindex, temp_save_ES_state;
    SrcStack      *new_stack_ent;
    char          *ext_name; 
    NotationRef   *nr;

    lclmemcpy(&pos, cur_pos, sizeof(Pos));

    *node = NULL;
    dtd_ctl = calloc(1, sizeof(DtdCtl));
    ctl->dtd_ctl = dtd_ctl;
    this_node = calloc(1, sizeof(Node));  
    this_node->node_type = DOCTYPEDECL;
    this_node ->internal_rep = (void *) dtd_ctl; 

    if( (!get_WS(ctl, &pos)) || !parse_name(ctl, &pos, 0,lu_element, &ename,
                       &eindex)) {
      dtd_error(ctl, cur_pos,&pos,"Missing root element name in doctype");
      return ErrorMarkup;
    }

     lclmemcpy(&dtd_ctl->root_element_name, &ename, sizeof(XString));
     this_node->index = eindex;
     dtd_ctl->index = eindex;
      
     dtd_ctl->has_external_dtd = 0; 
     if(get_WS(ctl, &pos)) { 
        if(parse_external_id(ctl, &pos, &dtd_ctl->external_dtd, 0)) {
           dtd_ctl->has_external_dtd = 1;
        } 
     }

     get_opt_WS(ctl, &pos);

     markup_node = prev_subord_node = NULL;

     if(next_char(ctl,&pos, CT_Specific,LEFT_SQUARE_CHAR, 1) != NULL){

       while(1) {
          pos.ES_state = pos.ES_state | ES_internal_dtd;

          /* provide for well formedness requirement that */
          /* in internal subset pe refs can occur only where markup */
          /* can occur.                                             */
          temp_save_ES_state = pos.ES_state;
          pos.ES_state = pos.ES_state | ES_first_mkup_char; 
          get_opt_WS(ctl, &pos);
          pos.ES_state = temp_save_ES_state; 
         
          mr = parse_markup(ctl, &pos,  parent_node, 
                            &markup_type, &markup_node);
          if(mr == NotMarkup) break; 
          switch(markup_type) {
             case MarkupTypeElementDecl:
             case MarkupTypeAttlistDecl:
             case MarkupTypeEntityDecl:
             case MarkupTypeNotationDecl:
             case MarkupTypePI:
                 if(prev_subord_node == NULL)
                        this_node->first_content = markup_node;
                 else prev_subord_node->next = markup_node;  
                 prev_subord_node = markup_node;
                 break; 
             case MarkupTypeConditional:  /* error identified elsewhere*/
             case MarkupTypeComment:
                  break;
             default:  /* TBD: try to bypass entire dtd */
               dtd_error(ctl, cur_pos,&pos,"Misplaced markup type"); 
          } /* end switch */
        } /*end while 1*/

        get_opt_WS(ctl, &pos);
        if(next_char(ctl,&pos,CT_Specific,RIGHT_SQUARE_CHAR,1)==NULL){
             dtd_error(ctl, cur_pos, &pos, "Problem in doctype");
             return ErrorMarkup;  
        }
       pos.ES_state = cur_pos->ES_state;
     } /*end if left square */

   if(dtd_ctl->has_external_dtd) {
     ext_name = (char *)dtd_ctl->external_dtd.use_id;

     new_stack_ent = load_first_buffer(ctl, ext_name, 0, &pos, NULL);

     pos.ES_state = pos.ES_state | ES_external_dtd;

     /* to handle content chains in conditional sections*/
     ctl->dtd_subord_begin = NULL;
     ctl->dtd_last_subord = NULL;
     
     while(1) {
            get_opt_WS(ctl, &pos);
            mr = parse_markup(ctl, &pos,  parent_node, 
                            &markup_type, &markup_node);
            if(mr == NotMarkup) { /* maybe end of external subset */
                get_opt_WS(ctl, &pos);
                if(next_char(ctl, &pos, CT_Specific, RIGHT_ANGLE_CHAR, 0)
                        != NULL) { 
                   dtd_error(ctl, &pos, &pos, "Markup error in dtd");
                   continue;
                }
                else break;
             } 
             switch(markup_type) {
               case MarkupTypeElementDecl:
               case MarkupTypeAttlistDecl:
               case MarkupTypeEntityDecl:
               case MarkupTypeNotationDecl:
               case MarkupTypePI:
                  if(ctl->dtd_subord_begin == NULL) 
                      ctl->dtd_subord_begin = markup_node;
                  else ctl->dtd_last_subord->next = markup_node; 
                  ctl->dtd_last_subord = markup_node; 
                  break; 
               case MarkupTypeComment:
               case MarkupTypeConditional:
                  break;
               default:  
                 dtd_error(ctl, cur_pos,&pos,"Misplaced markup type in dtd"); 
                 return ErrorMarkup;
           } /* end switch */
    } /* end while 1*/

    if(ctl->dtd_subord_begin != NULL) {
       if(prev_subord_node == NULL)
          this_node->first_content = ctl->dtd_subord_begin;
       else 
          prev_subord_node->next = ctl->dtd_subord_begin; 
       ctl->dtd_subord_begin = ctl->dtd_last_subord = NULL; 
   } 

     pos.ES_state = cur_pos->ES_state;
   } /* end if dtd_ctl->has_external_dtd*/

    pos.ES_state = pos.ES_state | ES_internal_dtd;
    get_opt_WS(ctl, &pos);
    if(next_char(ctl,&pos, CT_Specific,RIGHT_ANGLE_CHAR,1)== NULL){
          dtd_error(ctl, cur_pos, &pos,"Error in doctype syntax");
          return ErrorMarkup;  
     }
     pos.ES_state = cur_pos->ES_state;

    if(ctl->no_validation)  
       fprintf(stderr, "\n-- Dtd syntax error(s),"
                   " validation will be bypassed --\n");
    else {
       /* associate attlists with element decls and merge lists */
       process_attlists(ctl); 
     
      /* check whether any referenced notations still undefined*/ 
      for(nr = ctl->first_notation_ref; nr; nr = nr->next) { 
         if(btree_get_def_from_index(ctl, lu_notation,
                 (uint16) nr->index) == NULL) 
            dtd_error(ctl, &(nr->refpos), &(nr->refpos), 
                     "Undeclared notation in NOTATION attribute decl");
      }
    }

    *node = this_node;
     lclmemcpy(cur_pos, &pos, sizeof(Pos));
     return KnownMarkup;
 }


/************************************************************************/
/* For attlist enumerations (enumerated type or notation type          */
/* entered with initial ( already found                                 */
/* exit with NULL valuelist and error message if problem              */ 
static ValueList* get_attval_disjunction(Ctl *ctl, Pos *cur_pos,
            AttrKind attr_kind, char **message){
   
   Pos      *pos;
   int       name_token_ok;
   uint16    index;
   ValueList *alt, *alts, *prev_alts;
   XString   avalue;
   uint16    entry_ES_state;
   NotationRef *nr, *tempnr;

   alts = NULL;

   entry_ES_state = cur_pos->ES_state; 
   pos = cur_pos;

   name_token_ok = 0;

   if (attr_kind == AttrKindNOTATION) name_token_ok = 1;

   while(1) {
      get_opt_WS(ctl, pos); 

      pos->ES_state = pos->ES_state | ES_attvalue;

      if(attr_kind == AttrKindEnum) {
       /* let attvals share btree with attnames, since non unique anyway*/ 
       /* 1 for name_token_ok                                           */  
        if(!parse_name(ctl, pos,1,lu_attribute, &avalue, &index))
                      break;
      }
      /* otherwise is AttrKindNOTATION */
      else {   
        if(!parse_name(ctl, pos,0,lu_notation, &avalue, &index)) break;

        /* if notation not yet defined, record and check later */
        if(btree_get_def_from_index(ctl, lu_notation, index) == NULL) {
            nr = (void *)calloc(1, sizeof(NotationRef)); 
            nr->index = index;
            lclmemcpy(&(nr->refpos), pos, sizeof(Pos));
            tempnr = ctl->first_notation_ref;
            ctl->first_notation_ref = nr;
            nr->next = tempnr;
         }
      }
      pos->ES_state = entry_ES_state;
      alt = (void *)calloc(1, sizeof(ValueList));
      lclmemcpy(alt, &avalue, sizeof(XString)); 

      if(alts == NULL) alts = alt;
      else prev_alts->next = alt;
      prev_alts = alt;
    
      get_opt_WS(ctl, pos); 
      if(next_char(ctl, pos, CT_Specific, VERTICAL_BAR_CHAR, 1) == NULL)
         break;
      continue;
   }
    pos->ES_state = entry_ES_state;
    if(next_char(ctl, pos, CT_Specific, RIGHT_PAREN_CHAR,1) == NULL){
         *message = "Structure problem in attlist value enumeration";
       return NULL;
    }

   return alts;
}


/*************************************************************************/
int parse_dtd_attlist(Ctl *ctl, Pos *cur_pos,
                     Node *parent_node, Node **node) {
    
    Pos        pos;
    Node      *this_node;
    AttrList  *attlist, *attlist1;
    AttrSpec  *attspec, *prevspec;
    int        index;
    uint16     eindex, aindex;
    XString    ename, aname, value;
    char      *found_error;
    DtdCtl    *dtd_ctl;  
    /* indices associated with AttrKind enum */
    char *attkinds[13] = {"CDATA",  "ID",       "IDREF",   "IDREFS",
                         "ENTITY", "ENTITIES", "NMTOKEN", "NMTOKENS",
                         "NOTATION", "INT", "UINT", "NAME", "NAMES"};
    /* indices associated with DefaultKind enum */
    char *attrdefaultkinds[3] = {"REQUIRED", "IMPLIED", "FIXED"}; 

    lclmemcpy(&pos,cur_pos, sizeof(Pos));

    if( (!get_WS(ctl, &pos)) ||
         !parse_name(ctl, &pos, 0, lu_element, &ename, &eindex)) {
        dtd_error(ctl, cur_pos, &pos,
              "Missing or erroneous element type name in ATTLIST");
       return ErrorMarkup;
    }

    attlist = (void *)calloc(1, sizeof(AttrList));

    *node = NULL;
    this_node = (void *)calloc(1, sizeof(Node));
    this_node->node_type = ATTLISTDECL;
    lclmemcpy(&this_node->position, cur_pos, sizeof(Pos)); 
    this_node->internal_rep = (void *)attlist;

    lclmemcpy(&attlist->element_name, &ename, sizeof(XString));
    attlist->index = eindex;

    prevspec = NULL;
    while(1) {
       found_error = NULL;
       /* get attribute name */
       if(!get_WS(ctl, &pos)) break;

       if(!parse_name(ctl, &pos, 0,lu_attribute, &aname, &aindex)) break;

       attspec = (void *)calloc(1, sizeof(AttrSpec));
 
       lclmemcpy(&attspec->attr_name, &aname, sizeof(XString));
       attspec->index = aindex;

       if(!get_WS(ctl, &pos)) 
           { found_error = "Error in attlist decl"; break; }
 
       /* get attribute type */
       /* see if enumeration */
        if(next_char(ctl, &pos, CT_Specific, LEFT_PAREN_CHAR, 1) != NULL)
            attspec->attr_kind = AttrKindEnum;

        else {
           index =parse_keyword(ctl, &pos, 13, attkinds);  
           assert(index < 13);
           if(index < 0 ) 
               {found_error = "Unknown attribute kind"; break; }
           attspec->attr_kind = index;
     
           if(!get_WS(ctl, &pos))
               {found_error = "Missing whitespace in attlist decl"; break;}
           if(attspec->attr_kind == AttrKindNOTATION ) {
              if(next_char(ctl, &pos, CT_Specific, LEFT_PAREN_CHAR, 1)
                == NULL)
               { found_error = "Error in attlist decl"; break; }
           }
         }

        /* if anything but notation or enum, followed only by DefaultDecl */
        if(  attspec->attr_kind == AttrKindNOTATION
          || attspec->attr_kind == AttrKindEnum)  {
             attspec->attr_alt_list = 
                get_attval_disjunction(ctl, &pos,attspec->attr_kind,
                                    &found_error);
             if(attspec->attr_alt_list == NULL)
                break; 
             if(!get_WS(ctl, &pos)) 
               {found_error ="Missing whitespace in attlist decl"; break;}
        }

        lclmemcpy(&attspec->pos,&pos,sizeof(Pos)); 

       /* now get defaults */

       if(next_char(ctl, &pos, CT_Specific, HASHMARK_CHAR, 1) != NULL) {
           index = parse_keyword(ctl, &pos, 3, attrdefaultkinds);
           if(index < 0 )
               { found_error = "Unknown default kind in attlist "; break;}
           attspec->default_kind = (DefaultKind) index;
       }
       else attspec->default_kind = AttrOrdinary;

       if(attspec->default_kind == AttrFixed) {
             if(!get_WS(ctl, &pos)) 
               { found_error = "Missing whitespace after #FIXED"; break; } 
        } 

       if(  attspec->default_kind == AttrFixed
         || attspec->default_kind == AttrOrdinary) {
         get_opt_WS(ctl, &pos);
         pos.ES_state = pos.ES_state | ES_attvalue;
         if(!parse_qstring(ctl, &pos, &value, 1)) {
            pos.ES_state = cur_pos->ES_state;
            found_error = "Missing or misplaced attribute default value";
             break;
          }
          pos.ES_state = cur_pos->ES_state;
          lclmemcpy(&attspec->default_value, &value, sizeof(XString));

          /* validate default vs. spec after dtd complete*/ 
      }
      
      if(prevspec == NULL) attlist->first_attr_spec = attspec; 
      else prevspec->next = attspec;
      prevspec = attspec;
   } 

   if(found_error != NULL) {
     dtd_error(ctl, &pos, &pos, found_error);
     lclmemcpy(cur_pos, &pos, sizeof(Pos));
     return KnownMarkup;
   }

   if(next_char(ctl, &pos, CT_Specific, RIGHT_ANGLE_CHAR, 1) == NULL) {
        dtd_error(ctl, cur_pos, &pos, "Missing attlist end character"); 
        return ErrorMarkup;
    }

      attlist->attr_list_node = this_node; 
      dtd_ctl = ctl->dtd_ctl;   
      if(dtd_ctl->first_attr_list == NULL)
          dtd_ctl->first_attr_list  = attlist;
      else{
         for(attlist1 = dtd_ctl->first_attr_list; attlist1;
             attlist1 = attlist1->next) {
                if(attlist1->next == NULL) break; 
         }
         assert(attlist1);
         attlist1->next = attlist;
      }
        
   *node = this_node;
   lclmemcpy(cur_pos, &pos, sizeof(Pos));
   return KnownMarkup;
}
    
/*********************************************************************/
/* if content spec contains #PCDATA,  tree must be                     */
/* OpKindCdata                                                         */ 
/*  or                                                                 */
/* OpKindSeq  -child-> OpKindCdata                                     */ 
/*  or                                                                 */
/* OpKindStar -child-> OpKindAlt -child-> OpKindCdata -sib->OpKindType* */
static int verify_mixed_content(Ctl *ctl, OpTree *top_tree) { 

  OpTree *tree2, *tree3, *ot;

  tree2 = top_tree->first_opnd; 
  switch(top_tree->op) {
     case OpKindCdata: 
          return 1;
     case OpKindSeq: 
           if(tree2->op != OpKindCdata || tree2->next != NULL) return 0;
           return 1;
     case OpKindStar: 
          if(tree2->op == OpKindCdata) return 1;
          if( tree2->op != OpKindAlt ||  tree2->next != NULL) return 0;
           tree3 = tree2->first_opnd;        
           if(tree3->op != OpKindCdata) return 0; 
           for(ot = tree3->next; ot; ot=ot->next) {
                 if(ot->op != OpKindType) return 0;
            }
            return 1;
     default: break; 
   }
   
 return 0;
}

/*************************************************************************/
int parse_dtd_notation(Ctl *ctl, Pos *cur_pos, Node *parent_node,
                      Node **node) {
    int                 len;
    Pos                 pos;
    Node               *this_node;
    Notation           *notation;

   lclmemcpy(&pos, cur_pos, sizeof(Pos));

   *node = NULL;

   if(!check_dtd_WS(ctl, cur_pos, &pos,
                     "Missing initial WS in notation decl"))
       return ErrorMarkup;

    len = 0;

    notation = (void *)calloc(1, sizeof(Notation));
        
    if(!parse_name(ctl, &pos, 0, lu_notation, &notation->name,
                   &notation->index)) {
       dtd_error(ctl, cur_pos, &pos, "Missing or incorrect notation name"
               "in notation decl"); 
       return ErrorMarkup;
     }

   if(!check_dtd_WS(ctl, cur_pos, &pos,
                     "Missing WS in notation decl"))
       return ErrorMarkup;

    this_node = (void *)calloc(1, sizeof(Node));

    /* allows variant external id... pubic without system id */
    if(!parse_external_id(ctl, &pos, &notation->external_id, 1)) { 
       dtd_error(ctl, cur_pos, &pos, "Missing or incorrect notation name"
               " in notation decl"); 
       return ErrorMarkup;
    }

    this_node = (void *)calloc(1, sizeof(Node));
    this_node->node_type = NOTATIONDECL;
    this_node->internal_rep = (void *)notation;
    /* redundant stuff....*/
    this_node ->index = notation->index;
    lclmemcpy(&this_node->name, &notation->name, sizeof(XString));

     /* add ref to element def in element btree */
    btree_add_def(ctl, lu_notation, this_node, notation->index); 

    /*now look for final chars */

   get_opt_WS(ctl, &pos); 
   if(next_char(ctl, &pos, CT_Specific, RIGHT_ANGLE_CHAR, 1) == NULL) {
     dtd_error(ctl, cur_pos, &pos, "Structure error in notation_decl");
        free(this_node); free(notation);
        return ErrorMarkup;
   } 

   lclmemcpy(cur_pos, &pos, sizeof(Pos));
   *node = this_node;
   return KnownMarkup; 
 }


/*************************************************************************/
int parse_dtd_entity(Ctl *ctl, Pos *cur_pos, Node *parent_node,
    Node **node) {

    int                 len;
    Pos                 pos, save_pos;
    Node               *this_node;
    Entity             *entity;
    char               *ndata_kw[1] = {"NDATA"};
    LookupType          lu_type;  

   lclmemcpy(&pos, cur_pos, sizeof(Pos));

   *node = NULL;

   if(!check_dtd_WS(ctl, cur_pos, &pos, "Missing WS in entity decl")) 
       return ErrorMarkup;

    len = 0;

    entity = (void *)calloc(1, sizeof(Entity));

    entity->parameter_entity = 0; 
    if(next_char(ctl, &pos, CT_Specific, PERCENT_CHAR, 1) != NULL)  { 
       entity->parameter_entity = 1; 
       if(!check_dtd_WS(ctl, cur_pos, &pos, "Missing WS in entity decl")) 
          return ErrorMarkup;
     }
        
    /* which btree to use for lookup */
    if(entity->parameter_entity) lu_type = lu_pe;
    else lu_type = lu_ge;

    if(!parse_name(ctl, &pos, 0, lu_type, &entity->name, &entity->index)) {
       dtd_error(ctl, cur_pos, &pos, "No element name in element decl"); 
       return ErrorMarkup;
     }

    if(!check_dtd_WS(ctl, cur_pos, &pos, "Missing WS in entity decl")) 
          return ErrorMarkup;

    this_node = (void *)calloc(1, sizeof(Node));
    this_node->node_type = ENTITYDECL;
    this_node->internal_rep = (void *)entity;
    /* redundant stuff....*/
    this_node ->index = entity->index;
    lclmemcpy(&this_node->name, &entity->name, sizeof(XString));

     /* add ref to element def in element btree */
    btree_add_def(ctl, lu_type, this_node, entity->index); 

    lclmemcpy(&save_pos, &pos, sizeof(Pos));
    if(next_char(ctl, &pos, CT_Quote, 0, 1)!= NULL) { 
       /* restore pos because expected by parse_qstring */ 
       /* fix to parse_qstring to distinguish non-result from error */
       lclmemcpy(&pos, &save_pos, sizeof(Pos));  
       pos.ES_state = pos.ES_state | ES_entityvalue;
       if(!parse_qstring(ctl, &pos, &entity->entity_string,1)) {
           do_dtd_subst_error(ctl, cur_pos, &pos,
                      "Error in definition of entity '%s'", &entity->name);
           pos.ES_state = cur_pos->ES_state;
           return ErrorMarkup;
       }
       /* if value of entity is single, sometimes escaped, entity*/ 
       /* escape it                                              */  
       if( entity->entity_string.len == (ctl->is_double +1))  
            get_escaped(ctl, entity->entity_string.loc); 
       pos.ES_state = cur_pos->ES_state;
       entity->internal = 1;
    }       

   /* if is not parameter entity & is external id can also have notation */
    else if(parse_external_id(ctl, &pos, &entity->external_id, 0)) {  
         entity->internal = 0; 
         if((!entity->parameter_entity)
            && next_char(ctl,&pos,CT_WS,0,1) != NULL ){ 
               get_opt_WS(ctl, &pos);
               if(parse_keyword(ctl, &pos, 1, ndata_kw) > -1) {   
                  if( (!get_WS(ctl, &pos)) ||
                      !parse_name(ctl, &pos, 0, lu_notation,
                           &entity->notation, &entity->notation_index)){
                       dtd_error(ctl, cur_pos, &pos,
                                "Error in Notation ref");
                       return ErrorMarkup;
                   }

                   else if(btree_get_def_from_index(ctl,
                           lu_notation, entity->notation_index) == NULL) 
                      do_error(ctl, &pos, &pos, validation_error,
                        no_recover, "Undeclared notation in entity decl");

                   entity->unparsed = 1;
                } /* end if parse_keyword;*/ 
             /*if system id is relative reference, get full reference */ 
             /* using current containing entity                        */ 
          } /* end if (!entity->parameter_entity*/
         xml_convert_relative_ref(ctl, &entity->external_id); 
     } /* end else if(parse_external ... */

    else {  
          /* skip to > */
          dtd_error(ctl, cur_pos, &pos, "Error in entity definition"); 
         return ErrorMarkup;
      }

    /*now look for final chars */

   get_opt_WS(ctl, &pos); 
   if(next_char(ctl, &pos, CT_Specific, RIGHT_ANGLE_CHAR, 1) == NULL) {
     dtd_error(ctl, cur_pos, &pos, "Structure error in entity decl");
        free(this_node); free(entity);
        return ErrorMarkup;
   } 

   lclmemcpy(cur_pos, &pos, sizeof(Pos));
   *node = this_node;
   return KnownMarkup; 
 }


/*************************************************************************/
int parse_dtd_element(Ctl *ctl, Pos *cur_pos, Node *parent_node,
      Node **node) {

    int                len, kw_index, contains_pcdata, rtn;
    uint16              eindex;
    Pos                 pos;
    DtdCtl             *dtd_ctl;
    Node               *this_node;
    OpTree             *content_tree;
    ElementSpec        *espec, *espec1;
    char *keywords[2] = {"ANY", "EMPTY"};
    XString             ename;
    State              *last_state;
    Net                *net;

   lclmemcpy(&pos, cur_pos, sizeof(Pos));

   content_tree = NULL; 
   *node = NULL;

   dtd_ctl = ctl->dtd_ctl;
   if(!check_dtd_WS(ctl, cur_pos, &pos, "No element name in element decl")) 
       return ErrorMarkup;

    len = 0;
    if(!parse_name(ctl, &pos, 0, lu_element, &ename, &eindex)) {
       dtd_error(ctl, cur_pos, &pos, "No element name in element decl"); 
       return ErrorMarkup;
     }

    espec = (void *)calloc(1, sizeof(ElementSpec));

    this_node = (void *)calloc(1, sizeof(Node));
    this_node->node_type = ELEMENTDECL;
    this_node->internal_rep = (void *)espec;
    this_node ->index = eindex;

   if(!check_dtd_WS(ctl,cur_pos,&pos,"Content spec error in  element decl"))
       return ErrorMarkup;

    /*content specified as regular expression? */
    if(next_char(ctl, &pos, CT_Specific, LEFT_PAREN_CHAR, 1)
         !=NULL ) {
      rtn =parse_dtd_RE(ctl, &pos, 1, &content_tree, &contains_pcdata);
      if(rtn <= 0)  { 
         if(rtn == 0)  /* if if rtn == -1, will have skipped to > */ 
            dtd_error(ctl, cur_pos, &pos, "Element content spec error");
         else lclmemcpy(cur_pos, &pos, sizeof(Pos));
         return ErrorMarkup;
      }
      if(contains_pcdata) {
         espec->content_type = CtnKindMixed; 
         if(!verify_mixed_content(ctl, content_tree)){
           do_error(ctl, cur_pos, &pos, validation_error, no_recover,
                 "Illegal mixed content specified");
         }
      }
      else espec->content_type = CtnKindGeneralRE; 
      espec->top_tree = content_tree;
      net = (void *)calloc(1, sizeof(Net)); 
      net->first_state = make_state(ctl, net);
      espec->first_graph_state = net->first_state;
#ifdef DEBUG_TREE
      print_optree(ctl, cur_pos, 1, content_tree);
#endif
      last_state = NULL;
      to_graph(ctl, net, content_tree, 
              espec->first_graph_state,&last_state); 
      last_state->is_final = 1;
#ifdef DEBUG_GRAPH
      print_xml_element_net(ctl, cur_pos, net);
#endif
     } /* end if next_char... LEFT_PAREN */
      
    else {
      kw_index = parse_keyword(ctl, &pos, 2, keywords);  
      if(kw_index == 0) espec->content_type= CtnKindAny; 
      else if(kw_index == 1)  espec->content_type = CtnKindEmpty; 
      else {
         dtd_error(ctl, cur_pos, &pos,"Unknown content spec in element decl");
         free(this_node); free(espec);
         return ErrorMarkup;
     }

   }

   lclmemcpy(&espec->name, &ename, sizeof(XString));
   get_opt_WS(ctl, &pos);

   if(next_char(ctl, &pos, CT_Specific, RIGHT_ANGLE_CHAR, 1) == NULL) {
       dtd_error(ctl, cur_pos, &pos, "Error in element decl, "
                   "probably in content specification");
         free(this_node); free(espec);
        return ErrorMarkup;
    }

   /* add ref to element def in element btree */
   btree_add_def(ctl, lu_element, this_node, eindex); 

   lclmemcpy(cur_pos, &pos, sizeof(Pos));
   *node = this_node;
    dtd_ctl = ctl->dtd_ctl;   
    /* The following chain has probably become unnecessary */
    /* because chained through document ctl on content */
    if(dtd_ctl->first_element_spec == NULL)
          dtd_ctl->first_element_spec = espec;
    else{
        for(espec1 = dtd_ctl->first_element_spec; espec1;
            espec1 = espec1->next) {
             if(espec1->next == NULL) break; 
         }
         assert(espec1);
         espec1->next = espec;
    }   
        
   return KnownMarkup;
 }


/*************************************************************************/
/* TBD - add check for only OR in trees with cdata                       */
static int parse_dtd_RE(Ctl* ctl, Pos *cur_pos, int initial,
                 OpTree **content_tree, int *ctns_pcdata) { 

    Pos            pos;
    ReOpKind       multi_op, repeat_op, base_op, sep_op;
    OpTree        *first_child, *prev_child, *cur_child;
    OpTree        *first_tree, *next_tree, *base_tree;
    int            non_empty, rtn;
    uint16         tindex;
    char          *cdata[1] = {"PCDATA"};
    uchar         *thechar;
    XString        tname; 
    SpecialEnum    special;

    lclmemcpy(&pos, cur_pos, sizeof(Pos));

    multi_op    = base_op = repeat_op = OpKindNone;
    first_child = prev_child = cur_child = NULL;
    non_empty   = 0;  
    
   get_opt_WS(ctl, &pos);

   if(initial) *ctns_pcdata = 0;

   if(initial ||
      next_char(ctl, &pos, CT_Specific, LEFT_PAREN_CHAR, 1)) {

      while(1) { 
         get_opt_WS(ctl, &pos);

         /* look for contained content*/ 
         rtn = parse_dtd_RE(ctl, &pos, 0, &cur_child, ctns_pcdata);  
         if(rtn == -1) {
                lclmemcpy(cur_pos, &pos, sizeof(Pos));
                return (-1);  /* found error, pos at > */
          } 
         else if(rtn == 0) return 0; 

         non_empty = 1; 

         /* chain into children */
         if( first_child == NULL) first_child = cur_child; 
          else prev_child->next = cur_child;

         prev_child = cur_child;

         get_opt_WS(ctl, &pos);

         /* now look for separator (, or |) */ 
         if(next_char(ctl, &pos, CT_Specific, COMMA_CHAR, 1)) 
               sep_op = OpKindSeq;
         else if(next_char(ctl, &pos, CT_Specific,
                VERTICAL_BAR_CHAR, 1)) 
                sep_op = OpKindAlt;
         else break; 
          
        if(! (multi_op == sep_op || multi_op == OpKindNone)) {
           dtd_error(ctl, cur_pos, &pos,
              "Inconsistent separators in element content specification");
               lclmemcpy(cur_pos, &pos, sizeof(Pos));
           return -1; 
         }

         multi_op = sep_op;

     } /* end while 1 */

     if(!next_char(ctl, &pos, CT_Specific, RIGHT_PAREN_CHAR, 1)) {
        dtd_error(ctl, cur_pos, &pos,
              "Missing separator or ) in element content specification");
        lclmemcpy(cur_pos, &pos, sizeof(Pos));
        return -1;  /* indicates error issued and point at > */ 
    }

     if(non_empty== 0)  { 
        dtd_error(ctl, cur_pos, &pos, "Null element content specification");
        lclmemcpy(cur_pos, &pos, sizeof(Pos));
        return -1; 
     }

  } /* end if initial... */ 
             
  else {
     /* look for #PCDATA */
     if(next_char(ctl, &pos, CT_Specific, HASHMARK_CHAR, 1)) {
         if(parse_keyword(ctl, &pos, 1, cdata) == 0) {
                  base_op = OpKindCdata;  
                  *ctns_pcdata = 1;
           } 
      }
     /* otherwise look for specific element type */
     else {
         if(parse_name(ctl, &pos, 0,lu_element, &tname, &tindex)) {
            base_op = OpKindType;
         }
     }
    if(base_op ==  OpKindNone) {
         dtd_error(ctl, cur_pos, &pos, "Unrecognized element content unit");
        lclmemcpy(cur_pos, &pos, sizeof(Pos));
         return -1; 
     }
  }

    /* Look for repeat op (?, *, +) */
    if(base_op != OpKindCdata) {
       thechar = next_char(ctl, &pos, CT_RepeatOp, 0, 1); 
       if(thechar != NULL) {
           get_specials_code(ctl, thechar, &special,0,0);
           switch(special) {
               case PLUS_CHAR:   
                   repeat_op = OpKindPlus; break;
               case STAR_CHAR:   
                   repeat_op = OpKindStar; break;
               case QUESTION_CHAR:   
                   repeat_op = OpKindOpt;  break;
               default: assert(0); /* should not happen*/
            }
       }
    }

    /* Build new subtree consisting of                       */
    /* (repeat_op if any ->) (multi_op -> children |  base_op ) */ 
    /* e.g.,  OpKindStar -> Seq -> OpKindType(A)                */ 
    /*                              OpKindType(B)               */

    if(base_op != OpKindNone)  {
       base_tree = (void *) calloc(1, sizeof(OpTree)); 
       base_tree->op = base_op;
       if(base_op == OpKindType) {
          lclmemcpy(&base_tree->value, &tname, sizeof(XString));
          base_tree->id = tindex;
      }
    }
  
    if(repeat_op != OpKindNone) {
       first_tree = (void *) calloc(1, sizeof(OpTree)); 
       first_tree->op  = repeat_op; 
       if(multi_op != OpKindNone) { 
           next_tree = (void *) calloc(1, sizeof(OpTree)); 
           next_tree->op = multi_op;
           next_tree->first_opnd = first_child;
           first_tree->first_opnd = next_tree;
        }
       else{
          if(first_child != NULL) first_tree->first_opnd = first_child;
          else first_tree->first_opnd = base_tree; 
       }
    }
    else if(multi_op != OpKindNone) {
      first_tree = (void *) calloc(1, sizeof(OpTree)); 
      first_tree->op = multi_op;
      first_tree->first_opnd = first_child;
    } 
    /* if just initial paren */
    else if(base_op == OpKindNone) first_tree = first_child; 

    else first_tree = base_tree;
      
   *content_tree = first_tree;
    lclmemcpy(cur_pos, &pos, sizeof(Pos));
    return 1;
 }
   

/**********************************************************************/ 
/* Transform parse tree representing dtd-style content spec                */ 
/* to finite-state form to facilitate checking                             */
/* For example, SEQ( OPT(TYPE(X))) TYPE(Y) PLUS(TYPE(Z)) )                 */
/* becomes as (state arc-label state)                                       */
/* (1 EPSILON 2) (1  X  2)  (2  Y  3)  (3  Z  4)  (4  Z  4)                */
/* Caller add final at last state; since using epsilons should be ok?       */
static void to_graph(Ctl *ctl, Net *net, OpTree *tree, State *first_state,
      State **last_state){
    
   State    *local_last_state, *cur_state, *new_state;
   OpTree   *tree1;

   switch(tree->op) {  
      case OpKindType:
            /* build first->last */
            if(*last_state != NULL) local_last_state = *last_state;
            else  local_last_state = make_state(ctl,net); 
            make_arc(ctl, tree->id, first_state, local_last_state);
            break; 
      case OpKindSeq:
           /* build first->second->..->last */
            cur_state = first_state;
            if( (*last_state) == NULL) local_last_state = NULL; 
            else local_last_state = *last_state;
            for(tree1= tree->first_opnd; tree1; tree1=tree1->next) {
               if(tree1->next != NULL) new_state = NULL;
               else new_state = local_last_state;

               to_graph(ctl, net, tree1, cur_state, &new_state);
               cur_state = new_state;
            }
            local_last_state = cur_state;
            break; 
      case OpKindAlt: 

            /* build multiple first->last */
            if(*last_state != NULL) local_last_state = *last_state;
            else local_last_state = make_state(ctl,net);
            for(tree1= tree->first_opnd; tree1; tree1=tree1->next) {
               to_graph(ctl, net, tree1, first_state, &local_last_state);
            }
            break;
     case OpKindStar: 
            local_last_state = first_state; 
            tree1= tree->first_opnd; 
            assert(tree1->next == NULL); 
            /* build first->first */ 
            to_graph(ctl, net, tree1, first_state, &local_last_state);
            break;
    case OpKindPlus:
            if( (*last_state) != NULL)  local_last_state = *last_state;
            else local_last_state = NULL;
            tree1 = tree->first_opnd;
            assert(tree1->next == NULL); 
            /* build first->last & last->last */
            to_graph(ctl, net, tree1, first_state, &local_last_state); 
            to_graph(ctl, net, tree1, local_last_state, &local_last_state);
            break;  
     case OpKindOpt:
           /* build first->last & first-epsilon->last */
           local_last_state = NULL; 
           tree1 = tree->first_opnd;
           assert(tree1->next == NULL);
           /*fix to put epsilon arc last */
           to_graph(ctl, net, tree1, first_state, &local_last_state);
           make_arc(ctl, EPSILON_INDEX, first_state, local_last_state); 
           break; 
     case OpKindCdata:
            if(*last_state != NULL) local_last_state = *last_state;
            else  local_last_state = make_state(ctl,net); 
            make_arc(ctl, CDATA_INDEX, first_state, local_last_state);
            break; 
     default: assert(0);
     }

    *last_state = local_last_state;
     return;
  }

