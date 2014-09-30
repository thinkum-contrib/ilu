
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

#define DO_UNICODE
#define PRINT_DOCTREE

jmp_buf XmlError;

static int parse_comment(Ctl *ctl, Pos *cur_pos, Node *parent_node);
static int parse_CDATA( Ctl    *ctl,
                   Pos    *cur_pos,
                   Node    *parent_node,
                   Node   **cdata_node
                 );
static int parse_processing_inst(Ctl *ctl, Pos *cur_pos,
                            Node *parent_node, Node **pi_node);  

static int parse_element( Ctl    *ctl,
                   Pos    *cur_pos,
                   Node    *parent_node,
                   Node   **node
                 );

static int parse_end_element( Ctl    *ctl,
                       Pos    *cur_pos,
                       Node    *parent, 
                       int     *skipped_end_element
                      );
/***********************************************************************/
/* UTILITIES                                                           */
/***********************************************************************/
static Node *store_new_node(Ctl *ctl, Node *temp_node) {
 
  Node *new_node; 

  new_node = (void *)malloc(sizeof(Node)); 
  lclmemcpy(new_node, temp_node, sizeof(Node));
  return(new_node);
}

void  free_subords(Node *parent_node) {

   Node *next, *node;
   next = NULL; 
   for( node = parent_node->first_attr; node ; node = next) { 
        next = node->next;
        free(node);
   }
   for( node = parent_node->first_content; node ; node = next) { 
        next = node->next;
        free(node);
  }
}

#ifdef undef
/***********************************************************************/
static void add_attr_node(Ctl *ctl,         Node *elem_node,
                    Node *subord_node, Node *prev_attr_node) { 
  
   if(prev_attr_node == NULL)
       elem_node->first_attr = subord_node;
   else prev_attr_node->next = subord_node; 
}
#endif
    

/***********************************************************************/
/* invalue is char * and length, outvalue same, is pos in string buf */
void add_to_string_buf(Ctl *ctl, XString *invalue, XString *outvalue) { 

   
   /* add check for buffer size */
   if(ctl->string_buf_pos + invalue->len > ctl->string_buf_max)   
        get_new_string_buf(ctl, invalue->len); 
    outvalue->loc = ctl->string_buf_pos;
    outvalue->len = invalue->len;
    XStringCopy(ctl, outvalue, invalue);
    ctl->string_buf_pos += invalue->len;
}


/***********************************************************************/
int get_WS(Ctl *ctl, Pos *pos) { 
 
 if(!next_char(ctl, pos, CT_WS, 0, 1)) return 0;
 while(1) { if(next_char(ctl, pos, CT_WS, 0,1) ==NULL) break;} 
 return 1;
}

/***********************************************************************/
void get_opt_WS(Ctl* ctl, Pos *pos) {
   while(1) {
        if(next_char(ctl, pos, CT_WS,0,1) ==NULL)
            break;
   } 
   return;
} 


/***********************************************************************/
int get_equals(Ctl *ctl, Pos *pos) {

  get_opt_WS(ctl, pos);
  if(next_char(ctl, pos, CT_Specific, EQUAL_CHAR,1) == NULL) return 0;
  get_opt_WS(ctl, pos);
  return 1;
}

/***********************************************************************/
/* look for keyword, return -1 if not found, else index in keywords    */
/* FIX: avoid taking up buffer space                                   */
int parse_keyword(Ctl *ctl, Pos *cur_pos, int count, char *keywords[]) { 

  Pos   pos;
  int   len, i, j;
  uint16 dummy;
  uchar *kw; 
  XString word; 
   
  lclmemcpy(&pos, cur_pos, sizeof(Pos)); 
  len = 0;

  if(!parse_name(ctl, &pos, 0, no_lu, &word, &dummy)) return -1;

  if(!ctl->is_double) {
     for(i = 0; i < count; i++) {
        *(ctl->string_buf_pos) = '\0';
        if(!strcmp(keywords[i], (char *)word.loc)) {
           lclmemcpy(cur_pos, &pos, sizeof(pos));
           return i;
         }
     }
     return -1;
  }
  
  /* more complicated;  stored name is 2-byte */
  for(i = 0; i < count; i++) { 
     if( (((int)strlen(keywords[i])) *2) != word.len) continue;  
     kw = (uchar*)keywords[i];
     for(j = 0; j < word.len; j = j+2) {
        if(word.loc[j] != 0) return (-1);
        if(word.loc[j+1] != kw[j/2]) break;
     }
     if(j >= word.len) { 
        lclmemcpy(cur_pos, &pos, sizeof(Pos));
        return i;
     } 
  }
  return -1;
}



/***********************************************************************/
int parse_quoted_keyword(Ctl *ctl, Pos *pos, int count,
                 char *keywords[]) {
   
   uchar   *first, *last;
   int      returnval;

   /* TBD: check if treatment of pos ok here */
   first = next_char(ctl, pos, CT_Quote, 0, 1);
   if(first == NULL) return -1;
   returnval = parse_keyword(ctl, pos, count, keywords); 
   if(returnval < 0) return -1;
   last = next_char(ctl, pos, CT_Quote, 0, 1);
   if(last == NULL) return -1;
   if(*first != *last) return -1; 
   if(ctl-> is_double) {
        if(*(first+1) != *(last+1)) return -1; 
    }
   return returnval;
 }


#define TEMPLEN 500
/***********************************************************************/
/* parse-qstring will update pos if first char apost or quote          */
/* if convert, internal escaped chars back                           */
int parse_qstring(Ctl *ctl, Pos *pos, XString *outstring, int convert) { 

    uchar   temp[TEMPLEN]; 
    uchar  *first, *next, *mod_char;
    int     len,  possible_omitted_endquote;
    Pos     save_pos;
    XString value;
    SpecialEnum special;

    possible_omitted_endquote = 0;
    first = next_char(ctl, pos, CT_Quote, 0, 1);
    if(first ==NULL) 
       return 0;
    

    lclmemcpy(&save_pos, pos, sizeof(Pos));
    len = 0;
    while(1) { 
       /* succeeds if not quote  & not &, <, or  eot */  
       next = next_char(ctl, pos, CT_EndQuote, 0, 0);

       if(next == NULL) {  
       
          /* if not quote */
          if( (next = next_char(ctl, pos, CT_Quote, 0, 1)) == NULL ) {
             /* if is in entity def and & or < then not error */
             if( (pos->ES_state & (ES_internal_dtd | ES_external_dtd)) &&  
                 (pos->ES_state & ES_entityvalue) && 
                 ( (next = next_char(ctl, pos, CT_Specific, 
                            AMPERSAND_CHAR,1)) != NULL ||
                    (next = next_char(ctl, pos, CT_Specific, 
                            LEFT_ANGLE_CHAR,1)) != NULL) ) {}
             else
              if((pos->ES_state & (ES_internal_dtd | ES_external_dtd)) &&  
                 (pos->ES_state & ES_attvalue) && 
                 ( (next = next_char(ctl, pos, CT_Specific, 
                            AMPERSAND_CHAR,1)) != NULL )) {}
              else { 
                next=next_char(ctl, pos, CT_Any, 0, 1);

                /* if possible omitted endquote, recover before */
                /* right angle  if has been sensed             */
                if(possible_omitted_endquote) {
                        lclmemcpy(pos, &save_pos, sizeof(Pos));
                        do_error(ctl, pos, pos, syntax_error,
                            recover_before_right_angle,
                           "Invalid char or other error in quoted string");
                        break;
                 }

                /* don't print another message if EOT */
                if(next != NULL) { 
                   /* print message and then add to collection */
                   do_error(ctl, pos, pos, syntax_error, no_recover,
                     "Invalid character or other error in quoted string");

                } /*end if next != NULL */ 
             } /* end  else */
          } /* end if not quote */
         else if(CharsEq(ctl, first, next)) break;
       }

       /* FIX this to reallocate ?*/
       if( len > TEMPLEN -2) {
           if(possible_omitted_endquote) {
                 lclmemcpy(pos, &save_pos, sizeof(Pos));
                 do_error(ctl, pos, pos, syntax_error,
                        recover_before_right_angle,
                        "Invalid char or length problem or other error"
                         "  in quoted string");
             }
           else {
              do_error(ctl, &save_pos, pos, fatal_error,   
                        no_recover, "Quoted string too long"); 
             }
       }

       mod_char = &(temp[len]);
     
       /* might be null if error and end of file */
       if(next == NULL) break; 

       /* set up for recover if omit close quote or bad close quote*/
       if(get_specials_code(ctl, next, &special, 0, 0) &&
           special == RIGHT_ANGLE_CHAR) 
             possible_omitted_endquote = 1;


       append_character(ctl, &(temp[0]), &len, next); 
       /* restore literal value of character generated by CharRef*/
       /* which is also special character                         */
       if(convert) get_unescaped(ctl, mod_char);
    }     

    value.loc = &temp[0];  
    value.len = len;

    add_to_string_buf(ctl, &value, outstring);

    return 1;      
 }


/***********************************************************************/
/* parse_name will update pos if first char name begin                 */
/* put name into temp buffer, lookup in btree                         */
/* return btree_index   & output name                                */
int parse_name(Ctl *ctl, Pos *pos, int can_be_token,
                LookupType lookup,  XString *name, uint16 *index) {
     
    uchar     temp[TEMPLEN]; 
    uchar    *first, *next;
    int      len; 
    XString  value;

    len = 0;
    if(can_be_token) first = next_char(ctl, pos, CT_NameChar, 0,1);
    else first = next_char(ctl, pos, CT_NameBegin, 0,1);
    if(first == NULL) return 0;
    append_character(ctl, &(temp[0]), &len, first); 
    while(1) { 
       next = next_char(ctl, pos, CT_NameChar, 0,1);
       if(next == NULL) break;
       assert(len < TEMPLEN);
       append_character(ctl, &(temp[0]), &len, next); 
    }     

    value.loc = &temp[0];  
    value.len = len;

    /* search will add to string buf if not already there */
    if(lookup != no_lu) *index = btree_search(ctl, lookup, &value, name, 1);
    else  add_to_string_buf(ctl, &value, name);

    return 1;      
 }


/***********************************************************************/
int parse_attribute(Ctl *ctl, Pos *cur_pos, Node **attr_node) {   
   
   Node    temp_node;
   Pos     pos;
   XString aname,value;
   uint16  index;

   lclmemcpy(&pos, cur_pos, sizeof(Pos));

   get_opt_WS(ctl, &pos);

   if(!parse_name(ctl, &pos, 0, lu_attribute, &aname, &index)) return 0; 

   lclmemset(&temp_node, 0, sizeof(Node));

   temp_node.node_type = ATTRIBUTE;
   lclmemcpy(&temp_node.position, cur_pos, sizeof(Pos));
   lclmemcpy(&temp_node.name, &aname, sizeof(XString));
   temp_node.double_chars = ctl->is_double;
   temp_node.index = index;

   if(!get_equals(ctl, &pos)) {
      do_error(ctl,cur_pos, &pos, syntax_error, 
        recover_before_end_element,
        "Attribute name not followed by equals.");
      return 0;
   }

   /* not sure if this ok */
   pos.ES_state = pos.ES_state | ES_attvalue;

   if(!parse_qstring(ctl, &pos, &value, 1)) { 
      do_error(ctl, cur_pos, &pos, syntax_error,
           recover_before_end_element,
          "Missing quote or otherwise incorrect attribute value");
      return 0;
   }

    lclmemcpy(&temp_node.value, &value, sizeof(XString));

   *attr_node = store_new_node(ctl, &temp_node); 

    pos.ES_state = cur_pos->ES_state;
    lclmemcpy(cur_pos, &pos, sizeof(Pos));
    return 1;
 }


/***********************************************************************/
static MarkupType get_descriptor_type(Ctl *ctl, Pos *cur_pos) {

   char *descriptors[5] = {"DOCTYPE", "ELEMENT",
                            "ATTLIST", "ENTITY", "NOTATION"};
   int  index; 

   index = parse_keyword(ctl, cur_pos, 5, descriptors);
   if(index < 0) return MarkupTypeUnknown;

   switch(index) {
    case 0: return MarkupTypeDocType;
    case 1: return MarkupTypeElementDecl;
    case 2: return MarkupTypeAttlistDecl;
    case 3: return MarkupTypeEntityDecl;
    case 4: return MarkupTypeNotationDecl;
    default: break;
   }
    return MarkupTypeUnknown; /* should not happen */
}
   

/***********************************************************************/
MarkupReturn parse_markup(Ctl *ctl, Pos *cur_pos, Node *parent_node,
                 MarkupType *markup_type,  Node **node)
 {
   Pos     pos;
   int    initial_int_buf, initial_int_buf1;  /*for entity nesting check*/
   int    skipped_end_element = 0;

   lclmemcpy(&pos, cur_pos, sizeof(Pos));
   *markup_type = MarkupTypeUnknown;

   initial_int_buf = pos.buffer_number;

   get_opt_WS(ctl, &pos);

   if(next_char(ctl, &pos, CT_Specific, LEFT_ANGLE_CHAR, 1)==NULL) 
       return NotMarkup;

  /*used for checking for properly nested markup.. ignored if other error*/
   initial_int_buf1 = pos.buffer_number;

   *node = NULL;

   if(next_char(ctl, &pos, CT_Specific,EXCLAMATION_CHAR, 1) != NULL) {        
       /* can be comment or CDATA or description element */
       if(next_char(ctl, &pos, CT_Specific, MINUS_CHAR, 1) != NULL){
         /* Don't think it can be anything else if starts !- */
         if(parse_comment(ctl, &pos, parent_node) )  
            *markup_type = MarkupTypeComment;
         else *markup_type = MarkupTypeError;
       }
       else if(next_char(ctl, &pos, CT_Specific,
                  LEFT_SQUARE_CHAR, 1) != NULL) {

           /* conditional not permitted in internal dtd, but */
           /* this allows error to be recovered from */
           if( (pos.ES_state & ES_external_dtd) ||
                pos.ES_state & ES_internal_dtd) {
              *markup_type = MarkupTypeConditional;
               parse_conditional(ctl, &pos, parent_node, node);
          }
               
          /* Don't think it can be anything else if starts ![*/
          else if(parse_CDATA(ctl, &pos, parent_node, node) )  
              *markup_type = MarkupTypeCDATA;
          else *markup_type = MarkupTypeError;
        }
      else {/* must be description element */
          *markup_type =get_descriptor_type(ctl, &pos);
           if(*markup_type == MarkupTypeUnknown) {  
                 do_error(ctl, cur_pos, &pos, syntax_error, 
                      recover_after_element, "Erroneous markup");
                 return UnknownMarkup;
          }
         switch (*markup_type) {
                case MarkupTypeDocType:
                     parse_doctype(ctl, &pos, parent_node, node); 
                     break;
                case MarkupTypeElementDecl:  
                     parse_dtd_element(ctl, &pos, parent_node, node); 
                     break;
                case MarkupTypeAttlistDecl:  
                     parse_dtd_attlist(ctl, &pos, parent_node, node); 
                     break;
                case MarkupTypeEntityDecl:
                     parse_dtd_entity(ctl, &pos, parent_node, node); 
                     break;
                case MarkupTypeNotationDecl:
                     parse_dtd_notation(ctl, &pos, parent_node, node);
                     break;
                default: assert(0);
         } /* end switch */
      }
  }


   else if(next_char(ctl, &pos, CT_Specific, QUESTION_CHAR, 1)
      != NULL){
     /* Doesn't check for xml_decl... that done in buffer access*/
     if(parse_processing_inst(ctl, &pos, parent_node, node)) 
         *markup_type = MarkupTypePI;
      else *markup_type = MarkupTypeError;
    }

   else if(next_char(ctl, &pos, CT_Specific, UP_SLASH_CHAR, 1) 
      != NULL) { 
      /* if bad nesting, try to reparse as child of grandparent */ 
      *markup_type = MarkupTypeEndElement;
      if(!parse_end_element(ctl, &pos, parent_node, &skipped_end_element)){ 
         /* don't bother with valid nesting check if error */
         /* but retain knowledge that EndElement */
         lclmemcpy(cur_pos, &pos, sizeof(Pos));
         return UnknownMarkup;
       }
     }

   else if(parse_element(ctl, &pos, parent_node, node))
       *markup_type = MarkupTypeElement;
   else do_error(ctl,cur_pos, &pos, syntax_error, recover_after_element,  
             "Invalid element syntax"); 

    if(*markup_type == MarkupTypeUnknown || *markup_type== MarkupTypeError)
     {      
        if(end_of_document(ctl, &pos))  {
           /* will not return */
           do_error(ctl, cur_pos, &pos, fatal_error, no_recover,
            "Unexpected end of document");
         }  
       if(*markup_type == MarkupTypeUnknown)
         do_error(ctl, cur_pos, &pos, syntax_error, 
            recover_at_right_angle,
            "Unknown markup type or error in markup");
         lclmemcpy(cur_pos, &pos, sizeof(Pos));
         return UnknownMarkup;
    }

    /* checks if beginning and end of element in same entity.. */ 
    if( ! valid_entity_nesting(ctl, initial_int_buf1, pos.buffer_number)) {
        do_error(ctl, cur_pos, &pos, warning_error,
                  no_recover, "Improperly nested entity references");
     }

    /*TBD: add check for proper parent nesting */
    
   /* if bad nesting, try to reparse as child of grandparent */ 
   if(!skipped_end_element)
          lclmemcpy(cur_pos, &pos, sizeof(Pos));

    return KnownMarkup;
}


/***********************************************************************/
/* end_chars[] and thus char_ct assumed to be ascii                    */
static int parse_end_data (Ctl *ctl, int end_char_ct, uchar end_chars[],
                      Pos *cur_pos, XString *text){

   uchar  *next, *mod_char, *begin_string;
   Pos  pos;
   uchar  bad_end_chars[3] = {']', ']', '>'};
   int  found, badfound, len, bad_end_char_ct;
   XString oldcopy;

   lclmemcpy(&pos, cur_pos, sizeof(Pos));

   found = 0;
   badfound = 0;
   bad_end_char_ct = 3;

   /* if really is explicit CDATA, don't do double check */
   if(end_char_ct == bad_end_char_ct) bad_end_char_ct = 0;
   
   len = 0;

   begin_string = ctl->string_buf_pos;

   while(1) { 
       next = next_char(ctl, &pos, CT_Any, 0, 1);
       if(next == NULL) {
          do_error(ctl, cur_pos, cur_pos, syntax_error,
            recover_after_element,
           "Error in comment, processing instruction, or data string"); 
         lclmemcpy(cur_pos, &pos, sizeof(Pos));
         return 0;
     }
       mod_char = (ctl->string_buf_pos) + len; 
       if(mod_char >= ctl->string_buf_max) {
           oldcopy.loc = ctl->string_buf_pos;
           oldcopy.len = len;
           get_new_string_buf(ctl, len);
           /* just moves from old to new */
           add_to_string_buf(ctl, &oldcopy, text); 
           mod_char = (ctl->string_buf_pos) + len; 
       }
       append_character(ctl, ctl->string_buf_pos, &len, next); 

       /* check for end of data*/
        if(compare_with_special(ctl, mod_char,end_chars[found]))  
            found++;

        /* may be beginning of new end sequence or continuation */
        /* of embedded one                                      */  
        else { 
            for(found = found -1; found >= 0; found--) { 
              if(compare_with_special(ctl, mod_char,end_chars[found])) {
                   found++;
                   break;
                 }
             }
             if(found < 0) found = 0;
         }
        if(found == end_char_ct) break;

        if(bad_end_char_ct != 0) { 
          if(compare_with_special(ctl, mod_char,bad_end_chars[badfound]))  
              badfound++;
          else { 
              for(badfound = badfound -1; badfound >= 0; badfound--) { 
                 if(compare_with_special(ctl, mod_char,
                      bad_end_chars[badfound])) {
                    badfound++;
                    break;
                 }
              }
              if(badfound < 0) badfound = 0;
           }
             
          if(badfound == bad_end_char_ct) {
             do_error(ctl, cur_pos, cur_pos, compatibility_error,
             no_recover, "]]> not in CDATA"); 
             badfound = 0;
          }
       } /* end if(bad_end_char_count != 0) */

       /* resubstitute literal value of CharRef which is special char */
        get_unescaped(ctl, mod_char); 
   } 
    len = len - (end_char_ct + (end_char_ct * ctl->is_double));
    text->loc = ctl->string_buf_pos;
    text->len = len;
    /* if haven't moved to new buffer */
    if(ctl->string_buf_pos == begin_string)
         ctl->string_buf_pos += len; 
    lclmemcpy(cur_pos, &pos, sizeof(Pos));
    return 1;
  }


/***********************************************************************/
/* Parse of specific markup types                                       */
/***********************************************************************/

static int  parse_processing_inst(Ctl *ctl, Pos *cur_pos,
                            Node *parent_node, Node **pi_node){  

   Pos           pos;
   Node          *this_node;
   XString       tname, inst;
   uchar         end_chars[2] = {'?', '>'};
   uint16       dummy;
   uchar        *tloc;


    lclmemcpy(&pos, cur_pos, sizeof(Pos));
   *pi_node = NULL;  

    /* get_target */
    if(!parse_name(ctl, &pos, 0, no_lu, &tname, &dummy)) { 
        do_error(ctl, cur_pos, &pos, syntax_error,
             recover_after_element,  
             "Error in processing instruction");
             lclmemcpy(cur_pos, &pos, sizeof(Pos));
             return 0;
     }  

    /*Exclude various spellings of xml */ 
    /*Ugh.. should write random-case version of get_keyword */
    if(tname.len == 3 +  ( 3 * ctl->is_double)) {
        tloc = tname.loc;
        if(ctl->is_double) {
          if(tloc[0] != 0) goto label_ok; 
          if(tloc[1] != 'x' && tloc[1] != 'X')goto label_ok;
          if(tloc[2] != 0) goto label_ok; 
          if(tloc[3] != 'm' && tloc[3] != 'M')goto label_ok;
          if(tloc[4] != 0) goto label_ok; 
          if(tloc[5] != 'l' && tloc[5] != 'L')goto label_ok;
        }
        else {
          if(tloc[0] != 'x' && tloc[0] != 'X')goto label_ok;
          if(tloc[1] != 'm' && tloc[1] != 'M')goto label_ok;
          if(tloc[2] != 'l' && tloc[2] != 'L')goto label_ok;
        }
        
        do_error(ctl, cur_pos, &pos, syntax_error,
             recover_after_element,  
            "Processing target xml invalid outside xmldecl or textdecl");

        return 1;
   }

   label_ok: 

    get_opt_WS(ctl, &pos);

    /* accumulate instruction */
    pos.ES_state = pos.ES_state | ES_PI;
    /* update pos anyway to reflect recover attempt */ 
    if(!parse_end_data(ctl, 2, end_chars, &pos, &inst)) {
          lclmemcpy(cur_pos, &pos, sizeof(Pos));
          return 0;
     }

    pos.ES_state = cur_pos->ES_state;

    this_node = (void *)calloc(1,sizeof(Node));
    this_node->node_type = PI;
    lclmemcpy(&this_node->position, cur_pos, sizeof(Pos));
    lclmemcpy(&this_node->name, &tname, sizeof(XString));

    this_node->double_chars = ctl->is_double;   

    lclmemcpy(&this_node->value, &inst, sizeof(XString));

    this_node->parent = parent_node;
    lclmemcpy(cur_pos, &pos, sizeof(Pos));
    *pi_node = this_node;
    return 1;
 }

   
/***********************************************************************/
/* TBD: possibly store comment, in which case must save and           */ 
/* unescape escaped characters, ++                                    */ 
static int parse_comment(Ctl *ctl, Pos *cur_pos, Node *parent_node) {

   uchar          end_chars[2] = { '-', '-'}; 
   Pos            pos;
   XString        comment;

   lclmemcpy(&pos, cur_pos, sizeof(Pos));

    /* make sure next is another - */
    if(next_char(ctl, &pos, CT_Specific, MINUS_CHAR, 1) == NULL)
        return 0;

    pos.ES_state = pos.ES_state | ES_COMMENT;

   /* Overcomplication here to take care of SGML compatibility rqmt that */
   /* do not have -- internal to comment                                */
   if(!parse_end_data(ctl, 2, end_chars, &pos, &comment)) {
              lclmemcpy(cur_pos, &pos, sizeof(Pos));
              return 0;
       }

    if(next_char(ctl, &pos, CT_Specific, RIGHT_ANGLE_CHAR, 1) == NULL) {
        do_error(ctl, cur_pos, &pos, compatibility_error,
              recover_after_element, " '--' in comment not at end");
     }

    pos.ES_state = cur_pos->ES_state;

    lclmemcpy(cur_pos, &pos, sizeof(Pos));
    return 1;
 }


/***********************************************************************/
/* for explicit <![CDATA[, entered after first [                       */
static int parse_CDATA( Ctl    *ctl,
                   Pos    *cur_pos,
                   Node    *parent_node,
                   Node   **cdata_node
                 ) {

   uchar          end_chars[3] = {']', ']', '>'};
   char          *cdata[1] = {"CDATA"};

   Node            *this_node;
   Pos            pos; 
   XString        text;

    lclmemcpy(&pos, cur_pos, sizeof(Pos));
   *cdata_node = NULL;  

    if(parse_keyword(ctl, &pos, 1, cdata) < 0) {
        do_error(ctl, cur_pos, &pos, syntax_error,
             recover_after_element,  
             "CDATA must immediately follow ![ ..");
             lclmemcpy(cur_pos, &pos, sizeof(Pos));
             return 0;
     }  

    /* look for [ */
     if(!next_char(ctl, &pos, CT_Specific,LEFT_SQUARE_CHAR, 1)) {
        do_error(ctl, cur_pos, &pos, syntax_error,
             recover_after_element,  
             "Error in CDATA");
             lclmemcpy(cur_pos, &pos, sizeof(Pos));
             return 0;
     }  

    pos.ES_state = pos.ES_state | ES_CDATA;

    /* accumulate rest of string */
    if(!parse_end_data(ctl, 3, end_chars, &pos, &text)) {
           lclmemcpy(cur_pos, &pos, sizeof(Pos));
            return 0;
    } 

    pos.ES_state = cur_pos->ES_state;

    this_node = (void *)calloc(1,sizeof(Node));
    this_node->node_type = TEXT;
    lclmemcpy(&this_node->position, cur_pos, sizeof(Pos));
    lclmemcpy(&this_node->value, &text, sizeof(XString));
    this_node->double_chars = ctl->is_double;

    this_node->parent = parent_node;
    *cdata_node = this_node;
    lclmemcpy(cur_pos, &pos, sizeof(Pos));
    return 1;
 }


/***********************************************************************/
/* unbracketed cdata                                                   */
/***********************************************************************/
static int parse_chardata(
                   Ctl    *ctl,
                   Pos    *cur_pos,
                   Node    *parent_node,
                   Node   **cdata_node
                 ) {

   uchar       *next, *mod_char, *begin_string;
   Node         *this_node;
   Pos          pos;
   int          len;
   XString      oldcopy, dummy;
   uchar  bad_end_chars[3] = {']', ']', '>'};
   int   badfound, bad_end_char_ct;
 

    lclmemcpy(&pos, cur_pos, sizeof(Pos));
   *cdata_node = NULL;  
    begin_string = ctl->string_buf_pos;
    badfound = 0;
    bad_end_char_ct = 3;

    /* Eliminated ws-skip here.. possibly should restore */

    /* Puts char string found in general buffer, but does not */
    /* update buffer position  yet                         */ 

    len = 0;
    while(1) { 
       next = next_char(ctl, &pos, CT_EndCDATA, 0, 0);
       if(next == NULL) {
             next = next_char(ctl, &pos, CT_Specific, AMPERSAND_CHAR, 1);
             if(next != NULL)  {
               do_error(ctl, cur_pos, &pos, syntax_error, no_recover,
                  " '&'  or '<' in cdata and not in entity reference"); 
             }
             else break;
        } 
         /* check for ]]> .. sgml compatibiiity error */
        if(compare_with_special(ctl, next,bad_end_chars[badfound]))  
              badfound++;
        else { 
              for(badfound = badfound -1; badfound >= 0; badfound--) { 
                 if(compare_with_special(ctl, next,
                      bad_end_chars[badfound])) {
                    badfound++;
                    break;
                 }
              }
              if(badfound < 0) badfound = 0;
         }
             
        if(badfound == bad_end_char_ct) {
           do_error(ctl, cur_pos, cur_pos, compatibility_error,
           no_recover, "]]> not in CDATA"); 
           badfound = 0;
        }

       /* speed this up                             */
       /* if is escaped, unescape it                */
       /* (escaped chars are generated for CharRefs */ 
       mod_char = begin_string + len; 
       if(mod_char >= (ctl->string_buf_max-1)) {
           oldcopy.loc = ctl->string_buf_pos;
           oldcopy.len = len;
           get_new_string_buf(ctl, len);
           begin_string = ctl->string_buf_pos; 
           /* just move from old to new.., then reset string_buf_pos  */
           /* because add_to_string_buf will update                 */
           add_to_string_buf(ctl, &oldcopy, &dummy);
           ctl->string_buf_pos = begin_string;
           mod_char = begin_string+len;
       }
       append_character(ctl, begin_string, &len, next); 
       /* resubstitute literal value of character inserted by */   
       /* CharRef which is also special character             */
       get_unescaped(ctl, mod_char);
    }/* end while 1 */

    if(len == 0) {
         lclmemcpy(cur_pos, &pos, sizeof(Pos));
         return 0;
     }  

    this_node = (void *)calloc(1, sizeof(Node));
    this_node->node_type = TEXT;
    lclmemcpy(&this_node->position, cur_pos, sizeof(Pos));

    this_node->value.loc = ctl->string_buf_pos;
    this_node->value.len = len;
    this_node->double_chars = ctl->is_double;
    if(ctl->string_buf_pos == begin_string)
         ctl->string_buf_pos += len; 

    this_node->parent = parent_node;
    lclmemcpy(cur_pos, &pos, sizeof(Pos));
    *cdata_node = this_node;
    return 1;
 }



/***********************************************************************/
static int parse_end_element( Ctl    *ctl,
                       Pos    *cur_pos,
                       Node    *parent, 
                       int     *skipped_end_element
                      ) {

   Pos         pos;
   XString     ename;
   uint16      index;
  
   lclmemcpy(&pos, cur_pos, sizeof(Pos));

   if(!parse_name(ctl, &pos,0,lu_element, &ename, &index)) { 
       do_error(ctl, cur_pos, &pos, syntax_error, recover_at_right_angle,
                  "End element syntax."); 
       lclmemcpy(cur_pos, &pos, sizeof(Pos));
       return 0;
    }

   get_opt_WS(ctl, &pos);

   if(next_char(ctl, &pos, CT_Specific, RIGHT_ANGLE_CHAR, 1)
       == NULL) {
       do_error(ctl, cur_pos, &pos, syntax_error, recover_after_element,
                  "End element syntax."); 
       lclmemcpy(cur_pos, &pos, sizeof(Pos));
       return 0;
     }

   if(!XStringsEq(ctl, &parent->name, &ename)) {
      do_error(ctl, cur_pos, &pos, formation_error, no_recover, 
               "Improperly nested elements or need /> (empty element)");
#ifdef undef
      /* this not appropriate in many cases */
      do_error(ctl, &parent->position, &parent->position,
         formation_error, no_recover,
        "Assumed parent of above improperly nested element");
#endif
      if(parent->parent != NULL &&
         XStringsEq(ctl, &parent->parent->name, &ename)) 
         *skipped_end_element = 1;
      get_opt_WS(ctl, &pos);
      if(end_of_document(ctl, &pos))  {
           /* will not return */
           do_error(ctl, cur_pos, &pos, fatal_error, no_recover,
            "Unexpected end of document");
       }  
      lclmemcpy(cur_pos, &pos, sizeof(Pos));
      return 0;  
    }

   lclmemcpy(cur_pos, &pos, sizeof(Pos));
   return 1;
 }


/***********************************************************************/
static int parse_element( Ctl    *ctl,
                   Pos    *cur_pos,
                   Node    *parent_node,
                   Node   **node
                 ) {

   Node        *this_node, *subord_node, *prev_subord_node;
   Pos         pos;
   int         len, is_empty, finished;
   uint16      index;
   MarkupType  markup_type;
   XString     ename;
  

   lclmemcpy(&pos, cur_pos, sizeof(Pos));
   *node = NULL;  
   is_empty = 0;   

   this_node = (void *)calloc(1, sizeof(Node));

   this_node->parent = parent_node;

   len = 0; 
   if(!parse_name(ctl, &pos,0,lu_element, &ename, &index)) return 0; 

   /* now must have element */
   this_node->node_type = ELEMENT;
   lclmemcpy(&this_node->position, cur_pos, sizeof(Pos));

   lclmemcpy(&this_node->name, &ename, sizeof(XString));

   /* check if root is of correct type */
   if(parent_node->node_type == DOCUMENT){
       if(ctl->dtd_ctl == NULL) 
           ctl->no_validation = 1;
       else if ( ctl->dtd_ctl->index != index)   
           valid_error(ctl, &pos,  
               "Root element type does not agree with doctype decl");
    }
     

   this_node->index = index;

   this_node->double_chars = ctl->is_double;

   subord_node =  prev_subord_node = 0;

  /* parse attributes */
   while(1) { 
     if(!parse_attribute (ctl, &pos, &subord_node)) break;
     if (prev_subord_node == NULL) this_node->first_attr = subord_node;
     else  prev_subord_node->next = subord_node;
     prev_subord_node = subord_node;
     
   }

   if(!ctl->no_validation) validate_attributes(ctl, this_node);
   else check_attributes(ctl, this_node);

   get_opt_WS(ctl, &pos);

   /* look for /> */
   if(next_char(ctl, &pos, CT_Specific, UP_SLASH_CHAR,1 ) != NULL)
              is_empty = 1;  
   if(next_char(ctl, &pos, CT_Specific, RIGHT_ANGLE_CHAR, 1) == NULL)
     {
       do_error(ctl, cur_pos, &pos, syntax_error, recover_after_element,  
             "Invalid element syntax"); 
     }

   subord_node =  prev_subord_node = NULL;

   if(!is_empty) { 
     finished = 0;
     while(!finished) { 
       subord_node = NULL;
       if(parse_markup(ctl, &pos, this_node, &markup_type, &subord_node)
         == NotMarkup) { 
          if(parse_chardata(ctl, &pos, this_node, &subord_node)) 
               markup_type = MarkupTypeCDATA; 
           /* this is flakey */
           else { /* ?? */
             if(end_of_document(ctl, &pos)) 
               do_error(ctl, &pos, &pos, fatal_error, no_recover,
               "Unexpected end of document");
             return 0;
          }
       }
       switch(markup_type) {
                case MarkupTypeElement:
                case MarkupTypeCDATA: 
                case MarkupTypePI:
                       if(subord_node != NULL) {
                         if(this_node->first_content == NULL) 
                              this_node->first_content = subord_node;
                           else  prev_subord_node->next = subord_node; 
                           prev_subord_node= subord_node;
                       }
                       break;
                case MarkupTypeEndElement:
                      finished = 1;    
                      break;
                case MarkupTypeComment: 
                     break;
                case MarkupTypeUnknown: 
                case MarkupTypeError: 
                default:
                     break; /* erroneous markup */
         }
     } /* end while !finished */
   } /* end if !isempty */

   if(!ctl->no_validation) validate_content(ctl, this_node);

   this_node->parent = parent_node;
  *node = this_node; 
   lclmemcpy(cur_pos, &pos, sizeof(Pos));
   return 1;
 }

/***********************************************************************/
static int parse_version_number(Ctl *ctl, Pos *pos) {
   XString      in_vstring;
   char        *ver_str[1] = {"version"};
   uchar         v1[3] = {'1', '.', '0'};
   uchar         v2[6] = {'\0', '1', '\0', '.', '\0', '0'};
   XString       vstring1;
   XString       vstring2;

   vstring1.loc = &v1[0];
   vstring1.len =  3;
   vstring2.loc = &v2[0];
   vstring2.len =  6;

   if(parse_keyword(ctl, pos, 1, ver_str) == -1) return 0; 
      
   if(!get_equals(ctl, pos)) {
     do_error(ctl, pos, pos, syntax_error, no_recover, 
             "Missing = in version clause");
     get_opt_WS(ctl, pos); 
   }

   /* fatal error does not return */ 
   if(!parse_qstring(ctl, pos, &in_vstring, 0)) 
     do_error(ctl, pos, pos, fatal_error, no_recover, 
             "Unparseable version number");

   /* Don't exit on erroneous version number.. there's only 1 */
   if(!(  (ctl->is_double   &&   XStringsEq(ctl,&in_vstring, &vstring2))
        ||((!ctl->is_double) &&  XStringsEq(ctl,&in_vstring, &vstring1)))) 
        do_error(ctl, pos, pos, syntax_error, no_recover,
             "Erroneous version number in version phrase");

   return 1;
}

/***********************************************************************/
 /* TBD: extend encodings to add UTF-8  when have provisions */
 /* return 0 if no encoding, and fatal error (exit) if other error */
 /* except for missing =                                          */
static  int parse_encoding(Ctl *ctl, Pos *pos, EncType *encoding) { 

   char     *enc_str[1] = {"encoding"};
   /* Positions must match EncodingType enumeration */ 
   char     *encodings[4] = { "ASCII", "ISO-8859-1", "UTF-16", "UTF-8"};  

   if(parse_keyword(ctl, pos, 1, enc_str) == -1) return 0;

   if(!get_equals(ctl, pos)) {
         do_error(ctl, pos, pos, syntax_error, no_recover,
              "Missing = in encoding phrase of xml decl");
         get_opt_WS(ctl, pos);
    }

    *encoding = parse_quoted_keyword(ctl, pos, 4, encodings); 

    if( (int)(*encoding) == -1) 
           do_error(ctl, pos, pos, fatal_error, no_recover,
             "Unacceptable encoding type");
    return 1;
}


/***********************************************************************/
/* Errors here possibly should be more stringent                       */
EncType parse_xml_decl(Ctl *ctl, Pos *cur_pos, int reprocess) { 

   Pos           pos;
   EncType       encoding, default_encoding;
   int           yesno, founderror, possible_missing_ws, missing_ws;
   char         *xml_str[1] = {"xml"};
   char         *std_str[1] = {"standalone"};
   char         *yesno_str[2] = {"yes", "no"};
   
   lclmemcpy(&pos, cur_pos, sizeof(Pos));
    
   founderror = possible_missing_ws = missing_ws = 0;
   default_encoding = DEFAULT_8_ENCODING;  /* current default */
    
   get_opt_WS(ctl, &pos); 
   if(next_char(ctl, &pos, CT_Specific, LEFT_ANGLE_CHAR, 1) == NULL) {
     if(!reprocess) do_error(ctl, cur_pos, &pos, syntax_error, no_recover,
           "First char in document file must be <");
      return default_encoding;
   }

  if(next_char(ctl, &pos, CT_Specific, QUESTION_CHAR, 1) == NULL) 
         return default_encoding;

  if(parse_keyword(ctl, &pos, 1, xml_str ) == -1) return 0;

  /* version number required here */
  if(!get_WS(ctl, &pos)) missing_ws = 1;
  if(!parse_version_number(ctl, &pos)) { 
     if(!reprocess)
       do_error(ctl, cur_pos, &pos, syntax_error, recover_at_right_angle,
           "xml decl version number erroneous, misplaced, or missing");
     founderror = 1;
   }

   if(!get_WS(ctl, &pos)) possible_missing_ws = 1;
   
   /* return 0 if no encoding, and encoding of 1 otherwise       */
   /* encoding optional here                                      */
   /* Unacceptable encodings will be found fatal in parse_encoding*/
   if(!parse_encoding(ctl, &pos, &encoding)){ 
       encoding = ENCODING_UNSTATED; 
       possible_missing_ws = 0;
    } 
   else if (possible_missing_ws && !founderror) missing_ws = 1;
   else if(!get_WS(ctl, &pos)) possible_missing_ws = 1;

   get_opt_WS(ctl, &pos);

    /* look for standalone = yes/no */
    if(parse_keyword(ctl, &pos, 1, std_str) >= 0) { 
       if(!get_equals(ctl, &pos)) {
         if(!reprocess) do_error(ctl, cur_pos, &pos, syntax_error,no_recover,
                "Missing = in standalone phrase of xml decl");
          get_opt_WS(ctl, &pos); 
          founderror = 1;
       }
       yesno = parse_quoted_keyword(ctl, &pos, 2, yesno_str);
       if(yesno == 0) ctl->standalone = 1; 
       else if(yesno == 1) ctl->standalone = 0;

       else if(!reprocess)
          do_error(ctl, cur_pos, &pos, syntax_error, recover_after_element,
                   "Standalone type must be quoted yes/no");

      if (possible_missing_ws && !founderror) missing_ws = 1;
    }

   get_opt_WS(ctl, &pos);    

    if(next_char(ctl, &pos, CT_Specific, QUESTION_CHAR, 1) == NULL) {
       if( (!founderror) && !reprocess)
          do_error(ctl, cur_pos, &pos, syntax_error, recover_after_element,
                  "Error in xml decl syntax");
     }

    else if(next_char(ctl, &pos, CT_Specific, RIGHT_ANGLE_CHAR, 1) == NULL) {
       if( (!founderror) && !reprocess)
          do_error(ctl, cur_pos, &pos, syntax_error, recover_after_element,
                  "Error in xml decl syntax");
    } 

    else if(missing_ws && (!founderror)  && !reprocess)  
       do_error(ctl, cur_pos, &pos, warning_error, no_recover,
           "Missing whitespace in xml dcl.");

    lclmemcpy(cur_pos, &pos,sizeof(Pos));
    return encoding;
  }

/***********************************************************************/
EncType parse_text_decl(Ctl *ctl, Pos *cur_pos, int reprocess) {

   Pos           pos;
   EncType       encoding;
   char         *xml_str[1] = {"xml"};
   int           possible_missing_ws, missing_ws;
   
   lclmemcpy(&pos, cur_pos, sizeof(Pos));
    
   possible_missing_ws = missing_ws = 0;
   encoding = DEFAULT_8_ENCODING;  /* current default */
   get_opt_WS(ctl, &pos); 
   if(next_char(ctl, &pos, CT_Specific, LEFT_ANGLE_CHAR, 1) == NULL) {
       return encoding;
   }

   if(next_char(ctl, &pos, CT_Specific, QUESTION_CHAR, 1) == NULL) 
         return encoding;

   if(parse_keyword(ctl, &pos, 1, xml_str ) == -1) 
           return 0;

    /* version number  optional here */
    if( !get_WS (ctl, &pos)) possible_missing_ws = 1;

    if ( parse_version_number(ctl,&pos)) {
          if(possible_missing_ws) missing_ws = 1;
    } 
    else if(!get_WS(ctl, &pos)) possible_missing_ws = 1;   

    get_opt_WS(ctl, &pos);
   
    /* required here */
    if(!parse_encoding(ctl, &pos, &encoding)) {
       if(!reprocess)
             do_error(ctl, cur_pos, &pos, fatal_error, no_recover,
           "Missing or Unrecognized encoding in text dcl;"
           " XML parse terminated");
    } 
     else if(possible_missing_ws) missing_ws = 1;
             
   get_opt_WS(ctl, &pos);    

   if(next_char(ctl, &pos, CT_Specific, QUESTION_CHAR, 1) == NULL) {
     if(!reprocess)
       do_error(ctl, cur_pos, &pos, syntax_error, recover_after_element,
                  "Error in text decl syntax");
   }

   else if(next_char(ctl, &pos, CT_Specific, RIGHT_ANGLE_CHAR, 1) == NULL){ 
      if(!reprocess)
         do_error(ctl, cur_pos, &pos, syntax_error, recover_after_element,
                  "Error in text decl syntax");
  }

   else if(missing_ws && !reprocess)   
       do_error(ctl, cur_pos, &pos, warning_error, no_recover,
           "Missing whitespace in xml dcl.");

   lclmemcpy(cur_pos, &pos,sizeof(Pos));
   return encoding;
 }


/***********************************************************************/
/* Initialization and top level rules                                  */
/***********************************************************************/
int parse_document(Ctl *ctl, Pos *cur_pos, Node *top_node) { 
  Pos         pos;
  int         in_prolog; 
  Node       *subord_node, *prev_subord_node;
  MarkupType  markup_type; 
  
  lclmemcpy(&pos, cur_pos, sizeof(Pos));

  /* build in standard entities, &lt, &gt, etc */
  build_in_entities(ctl);

  in_prolog = 1;
  subord_node = prev_subord_node = NULL;

  /* before root element */
  while(in_prolog) {
    subord_node = NULL;
    if(parse_markup(ctl, &pos, top_node, &markup_type,
            &subord_node) != NotMarkup) { 
        switch(markup_type) { 

           case MarkupTypeComment:
                break; 

           case MarkupTypeDocType:
               if(ctl->dtd_node != NULL)   
                 do_phys_error(ctl, "Multiple doctypes per document",1);
               ctl->dtd_node = subord_node; 
               break;

           case MarkupTypeElement:
                if(ctl->dtd_ctl == NULL && (!ctl->no_validation)) {
                    do_warning(ctl, NULL,  
                         "No dtd; no validation can be performed");
                      ctl->no_validation = 1;
                 }
                 in_prolog = 0;    /* intentional fall through */
           case MarkupTypePI:
              if(subord_node != NULL)  {
                  if(top_node->first_content == NULL)
                       top_node->first_content = subord_node; 
                  else prev_subord_node->next = subord_node; 
                  prev_subord_node = subord_node;
              }
                break;

          default: 
               do_error(ctl, &pos, &pos, syntax_error,
                        recover_after_element,
                    "Misplaced or improperly nested markup type");
               /* but put it in tree anyway */
              if(subord_node != NULL)  {
                  if(top_node->first_content == NULL)
                       top_node->first_content = subord_node; 
                  else prev_subord_node->next = subord_node; 
                  prev_subord_node = subord_node;
              }
              break;
       }    
     }
    else { /* probably misplaced cdata */
         do_error(ctl, &pos, &pos, syntax_error,
                   recover_before_left_angle, 
                  "Probably misplaced text");
                   in_prolog = 0; 
        }
  }

 /* Misc after root element */ 
 while(1) { 
    subord_node = 0;
    if(parse_markup(ctl, &pos, top_node, 
       &markup_type, &subord_node) != NotMarkup) { 
        switch(markup_type) { 
           case MarkupTypeComment:
                break; 
           case MarkupTypePI:
              if(subord_node != NULL)  {
                  if(top_node->first_content == NULL)
                     top_node->first_content = subord_node; 
                   else prev_subord_node->next = subord_node; 
                   prev_subord_node = subord_node;
               }
               break;
           default: do_error(ctl, cur_pos, &pos, syntax_error,
                            no_recover, 
                            "Misplaced markup type");
                   in_prolog = 0; 
                   break;
         }
       }
   else {
       /* not at end of document, trailing CDATA? */ 
      get_opt_WS(ctl, &pos);
      if(next_char(ctl, &pos, CT_Any, 0, 1)!=NULL)  
          do_error(ctl, cur_pos, &pos, syntax_error, no_recover, 
                            "Probably trailing text");
      break;
   }
 }   

  if(!ctl->no_validation)
      check_referenced_ids(ctl);
           
  lclmemcpy(cur_pos, &pos, sizeof(Pos));
  return 1;
}

     
/****************************************************************************/
/* mainly initializes buffers; should move to buffers.c */
static int xml_parse_init(char *initial_file_name, Ctl **ctl_return,Pos **cur_pos) {

  Ctl          *ctl;
  StringBufEnt *string_buf_ent;
  Pos          *pos; 
  IntHeap      *int_heap; 

  ctl = (void *) calloc(1, sizeof(Ctl));  

#ifdef DO_UNICODE  
  ctl->is_double = 1;
#endif

  ctl->int_heap_alloc = INT_HEAP_ALLOC;
  ctl->int_heaps = (void *)calloc(ctl->int_heap_alloc,
                          sizeof(IntHeap));
  ctl->int_heaps_used = 1;
  ctl->int_heap_size = INT_HEAP_SIZE; 
  int_heap = &(ctl->int_heaps[0]);
  int_heap->buffer = (void *)calloc(1,ctl->int_heap_size);
  int_heap->chars_used = 0;
  
  pos = (void *)calloc(1, sizeof(Pos));  
  pos->buffer_number = 0;
  pos->char_number = 0;
  pos->ES_state = 0;

  ctl->int_buf_alloc = INT_BUF_ALLOC;
  ctl->int_bufs = (void *)calloc(ctl->int_buf_alloc,
                          sizeof(IntBufEnt));
  ctl->int_bufs_used = 0;  

  ctl->line_list_alloc = LINELISTALLOC;
  ctl->line_list = (void *)calloc(ctl->line_list_alloc,
                   sizeof(LineIndexEnt));
  ctl->line_ents_used = 0;  

  ctl->first_entity = NULL;

  /* allow multi string bufs */ 
  ctl->string_buf_list_alloc = STRING_BUF_LIST_ALLOC;
  ctl->string_buf_list = (void *)calloc(ctl->string_buf_list_alloc,
                               sizeof(StringBufEnt));
  string_buf_ent =  &(ctl->string_buf_list[0]);
  string_buf_ent->buffer = (void *)calloc(1, STRINGBUFALLOC);
  ctl->string_buf_ents_used = 1; 
  ctl->string_buf_max = string_buf_ent->buffer+(STRINGBUFALLOC-1);
  ctl->string_buf_pos = string_buf_ent->buffer; /* in current string buf */
  ctl->next_is_newline = 0;

  ctl->btree_ctl.btree = (void *)calloc(INIT_BTREE_ALLOC,
                     sizeof(XmlBTree)); 
  ctl->btree_ctl.allocation = INIT_BTREE_ALLOC;
  ctl->btree_ctl.first_unused = FIRST_BTREE_AVAIL;

  ctl->next_entity_instance = 0;

  /* hash table and chains for id attributes */
  ctl->id_hash_table = (void *)calloc(IDHASHSIZE, sizeof(IdEntry*));
  
  ctl->current_id_heap = (IdEntry*)xml_heap_alloc(ctl,
                          IDHEAPALLOC*sizeof(IdEntry*)); 

  ctl->current_id_alloc = IDHEAPALLOC;
  ctl->current_ids_used = 0;

   /* sets up first source buffer, internal buffer, line index entry */
  ctl->current_src_stack =load_first_buffer(ctl, initial_file_name, 1, pos,NULL); 
  if(ctl->current_src_stack == NULL) return 0;  

  *cur_pos = pos;
  *ctl_return = ctl;

  return 1;
}


/***********************************************************************/
int parse_xml(char *filename, Node **treetop,
               void **dctl /*for line number inquiries*/,
               XmlCvtPublic convert_pubid_proc /* may be null */) {

 Ctl *ctl;
 Node *top_node;
 Pos *pos;
 int  status; 
 
 *treetop = NULL;
 status = setjmp(XmlError); 
 if(status != 0) return 0;

 if(!xml_parse_init(filename, &ctl, &pos))  return 0; 

 top_node = (void *)calloc(1, sizeof(Node));

 ctl->top_node = top_node;
 ctl->convert_pubid_proc = convert_pubid_proc;

 top_node->node_type = DOCUMENT;  
 top_node->name.loc  = NULL;
 top_node->name.len  = 0;


 /* This really not permitted.. but */
  get_opt_WS(ctl, pos);

 if(!parse_document(ctl, pos, top_node)) {
   do_error(ctl, pos, pos, syntax_error, no_recover,  
           "Unrecoverable parse error, parse halted");
    return 0;
 }

 *treetop = top_node;
 *dctl = (void *)ctl;
 
 if(ctl->syntax_error || ctl->formation_error || ctl->validation_error)   
       return 0;

 return 1; 
}

