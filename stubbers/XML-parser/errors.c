
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
 

extern jmp_buf XmlError;
/* for debug                   */
void print_pos(Ctl *ctl, Pos *pos) {
   char      astring[5];  
   IntBufEnt *int_buf_ent; 
   int_buf_ent = &ctl->int_bufs[(ctl->int_bufs_used)-1];
   astring[0] = int_buf_ent->buffer[(pos->char_number)-1];
   astring[1] = '\0';
   printf("%s", &astring[0]);
 }


/***********************************************************************/
/*Utilities to get line number and char number from error pos data     */
/***********************************************************************/

static LineIndexEnt *get_last_eq_line(Ctl *ctl,
                                     LineIndexEnt *in_line_ent) {  

   LineIndexEnt *line_ent, *next_line_ent;

   /* get next line entry */
   line_ent = in_line_ent;
   while(1) { 
      if(line_ent->next_line_index != 0) {
         next_line_ent = &ctl->line_list[line_ent->next_line_index];
         if(next_line_ent != NULL &&
            next_line_ent->line_number == line_ent->line_number) {
            line_ent = next_line_ent;
            continue;
         }
      }
     break;
   } /* end while 1 */
   return (line_ent);
 }

   
static LineIndexEnt *get_next_line(Ctl *ctl, LineIndexEnt *in_line_ent) {  

   LineIndexEnt *line_ent;

   /* get next line entry */
   if(in_line_ent->next_line_index == 0) return NULL;
   line_ent = &ctl->line_list[in_line_ent->next_line_index]; 
   return(get_last_eq_line(ctl, line_ent));
 }


void xml_get_line_data (Ctl *ctl, Pos *pos, 
                   LineIndexEnt **rtn_line_ent, int *rtn_char_num) {
   IntBufEnt    *int_buf;
   LineIndexEnt *line_ent, *new_line_ent;
   int           goal_char_num, accum_char_num, char_num, int_buf_index;


   int_buf_index = pos->buffer_number; 
   char_num = pos->char_number;  

   int_buf = &ctl->int_bufs[int_buf_index];
   line_ent = &ctl->line_list[int_buf->line_index];

   /* represents number of characters needed to find correct line ent*/ 
   goal_char_num = int_buf->first_line_pos + char_num; 
   accum_char_num = 0;   

   /*just see if line has continuation replacement*/
   line_ent = get_last_eq_line(ctl, line_ent);

   while(accum_char_num + line_ent->line_len < goal_char_num) {
      accum_char_num += line_ent->line_len;
      new_line_ent = get_next_line(ctl, line_ent);
      if(new_line_ent == NULL) break;
      line_ent = new_line_ent;
   } 

  *rtn_char_num = goal_char_num - accum_char_num; 
  *rtn_line_ent = line_ent;

}

  
static void print_error_source(Ctl *ctl, Pos *pos, int indent, int first) {

   IntBufEnt    *int_buf, *next_int_buf;
   int           i, int_buf_index, char_num, rtn_char_num;
   LineIndexEnt  *rtn_line_ent;
   Pos            new_pos; 
   

   int_buf_index = pos->buffer_number; 
   char_num = pos->char_number;  
   if(int_buf_index < 0 ) return;

   xml_get_line_data (ctl, pos, &rtn_line_ent, &rtn_char_num);

   fprintf(stderr, "\n");
   for(i = 0; i < indent; i++) fprintf(stderr, " ");
   if(first) fprintf(stderr, "in ");
   else  fprintf(stderr, "embedded in ");
   if(rtn_line_ent->file_name != NULL) {
      fprintf(stderr, "file %s, ", &(rtn_line_ent->file_name[0]));
      fprintf(stderr, "line %d, ", rtn_line_ent->line_number);
    }
   else if(rtn_line_ent->entity_name != NULL){ 
     fprintf(stderr, "entity %s, ", xstr2cstr(rtn_line_ent->entity_name,
                                        ctl->is_double));  
   }
   if(!ctl->is_double) 
     fprintf(stderr, "near char %d", rtn_char_num);
   else  
     fprintf(stderr, "near char %d", rtn_char_num/2);
   
   /* now get earlier source if exists */
   int_buf = &ctl->int_bufs[int_buf_index];
   if(int_buf->parent_intbuf > 0) {
       new_pos.buffer_number = int_buf->parent_intbuf;
       next_int_buf = &ctl->int_bufs[int_buf->parent_intbuf]; 
       new_pos.char_number = next_int_buf->chars_used;
       print_error_source(ctl, &new_pos, indent+2, 0); 
   }
 }  
    

void xml_get_line_source(Ctl *ctl, Pos *pos, char **file_name, 
         int *line_number) { 

   IntBufEnt    *int_buf, *next_int_buf;
   int           int_buf_index, char_num, rtn_char_num;
   LineIndexEnt  *rtn_line_ent;
   Pos            new_pos; 
   
   *file_name = " ";
   *line_number = 0;

   if(pos == NULL)  return;

   int_buf_index = pos->buffer_number; 
   char_num = pos->char_number;  
   if(int_buf_index < 0 ) return;

   xml_get_line_data (ctl, pos, &rtn_line_ent, &rtn_char_num);

   if(rtn_line_ent->file_name != NULL) {
         *file_name = (char *)&(rtn_line_ent->file_name[0]);
         *line_number = rtn_line_ent->line_number;
       return;
    }

   /* now get earlier source if exists */
   int_buf = &ctl->int_bufs[int_buf_index];
   if(int_buf->parent_intbuf > 0) {
       new_pos.buffer_number = int_buf->parent_intbuf;
       next_int_buf = &ctl->int_bufs[int_buf->parent_intbuf]; 
       new_pos.char_number = next_int_buf->chars_used;
       xml_get_line_source(ctl, &new_pos, file_name, line_number ); 
   }

 }  
    
/* msg_pos is source position to be used in printing message. */
/* search_pos is initial position for recovery search         */
void do_error(Ctl *ctl,
             Pos *msg_pos, Pos *search_pos,
             ErrorKind errorkind,
             RecoverKind recoverkind,
             char *message) {

  Pos save_pos;

  switch (errorkind) {
      case dtd_syntax_error:
         ctl->syntax_error = 1;
         fprintf(stderr, "\nDTD Syntax Error: ");
         break;
      case syntax_error:
         ctl->syntax_error = 1;
         fprintf(stderr, "\nSyntax Error: ");
         break;
      case formation_error:
         fprintf(stderr,  "\nFormation Error: ");
         ctl->formation_error = 1;
         break;
       case validation_error:
         ctl->validation_error = 1;
         fprintf(stderr,  "\nValidation Error: ");
         break; 
       case compatibility_error:
         ctl->compatibility_error = 1;
         fprintf(stderr,  "\n(SGML) Compatibility Error: ");
         break; 
       case warning_error:
         ctl->warning_error = 1;
         fprintf(stderr,  "\nWarning: ");
         break;
       case fatal_error:
         ctl->fatal_error= 1;
         fprintf(stderr, "\nNonRecoverable Error: ");
         break; 
      default: {}
  }

  fprintf(stderr, message);

  /* print succession of entities, files for error */
  print_error_source(ctl, msg_pos, 2, 1);

  if( (errorkind == syntax_error || errorkind == formation_error ||
       errorkind == dtd_syntax_error)
      && !ctl->no_validation) {

       fprintf(stderr, "\n\n-- After syntax or formation error "
                    "validation will be bypassed --\n");
       ctl->no_validation = 1;
  }

  fprintf(stderr, "\n");
  fflush(stderr); 

  /* exit if fatal parse error */ 
  if(errorkind == fatal_error) 
     longjmp(XmlError, 1);

  /* get next char will stop before eot */
  switch(recoverkind) {

     case recover_before_right_angle:
        while(next_char(ctl, search_pos,
              CT_Specific, RIGHT_ANGLE_CHAR, 0) != NULL) { continue; };
        break;
     case recover_at_right_angle:
        while(next_char(ctl, search_pos,
              CT_Specific, RIGHT_ANGLE_CHAR, 0) != NULL) { continue; };
        next_char(ctl, search_pos, CT_Any, 0, 1); 
        break;
     case recover_after_element:
        /* really recover after end element or before next < */
        while(1) { 
           if(next_char(ctl, search_pos, CT_EndElement, 0, 0) != NULL) 
               continue;
           if(next_char(ctl, search_pos, CT_Specific, RIGHT_ANGLE_CHAR, 1)
               != NULL) 
               break;
           else { 
            lclmemcpy(&save_pos, search_pos, sizeof(Pos));
            if(next_char(ctl, search_pos, CT_Specific, LEFT_ANGLE_CHAR, 1) 
                   != NULL )
                { lclmemcpy(search_pos, &save_pos, sizeof(Pos)); break; } 
            else {
                 /* just skip next unless eot */
                 if(next_char(ctl, search_pos, CT_Any, 0, 1)) continue; 
                 break;
                 }
            }
          }
         break;
     case recover_before_end_element:
        /* leave pos before > or />  or <, whichever first*/ 
        while(1) { 
           if(next_char(ctl, search_pos, CT_EndElement, 0, 0) != NULL) 
               continue;
           lclmemcpy(&save_pos, search_pos, sizeof(Pos));
           if(next_char(ctl, search_pos, CT_Specific, RIGHT_ANGLE_CHAR, 1)
               != NULL) 
                { lclmemcpy(search_pos, &save_pos, sizeof(Pos)); break; } 
           else if( 
              (next_char(ctl, search_pos, CT_Specific, UP_SLASH_CHAR, 1) 
                   != NULL ) &&
              (next_char(ctl, search_pos, CT_Specific, RIGHT_ANGLE_CHAR, 1) 
                   != NULL ) ) 
                  { lclmemcpy(search_pos, &save_pos, sizeof(Pos)); break; } 
           else if(next_char(ctl, search_pos, CT_Specific, LEFT_ANGLE_CHAR, 1) 
                   != NULL )
                  { lclmemcpy(search_pos, &save_pos, sizeof(Pos)); break; } 
           else continue;
          }
         break;

     case recover_before_left_angle:
        while(next_char(ctl, search_pos,
              CT_Specific, LEFT_ANGLE_CHAR, 0) != NULL) { continue; };
        break;
     case no_recover:
        break; 
   }

  if(recoverkind != no_recover) {
             search_pos->ES_state = msg_pos->ES_state;
             lclmemcpy(msg_pos, search_pos, sizeof(Pos)); 
         }

  if(errorkind == fatal_error) longjmp(XmlError, 1);

   return;
}

/* TBD: narrow, and add settings for nonfatal */
void do_phys_error(Ctl *ctl, char *message,int fatal){
  if(fatal) 
     fprintf(stderr, "\nNonRecoverable Error: ");
  else
     fprintf(stderr, "\nError: ");
  fprintf(stderr, message);
  fprintf(stderr, "\n");
  fflush(stderr); 
  if(fatal) {
         ctl->fatal_error = 1;
         longjmp(XmlError, 1);
  }
   return;
}

void do_warning(Ctl *ctl, Pos *pos, char *message) { 
   if(pos != NULL) 
       do_error(ctl, pos, pos, warning_error, no_recover, message);
   else { 
     fprintf(stderr, "\nWarning: ");
     fprintf(stderr, message);
     fprintf(stderr, "\n");
     fflush(stderr); 
  }
  return;
}


/*******************************************************************/
void valid_error(Ctl *ctl, Pos *pos, char *message) {
          do_error(ctl, pos, pos, validation_error, no_recover, message);
}


/*******************************************************************/
void do_validity_subst_error(Ctl *ctl, Pos *pos, char *message,
                             XString *substitution) {
     char* c_subst;
     char  print_message[500];

     /* approximation.. assumes element & attribute names latin_1 */
     /* for name print                                           */
     c_subst = xstr2cstr(substitution, ctl->is_double);
     sprintf(print_message,  message, c_subst);
     valid_error(ctl, pos, print_message); 
     free(c_subst);
  }

/*******************************************************************/

void dtd_error(Ctl *ctl, Pos *cur_pos, Pos *pos, char *message) {
    do_error(ctl, cur_pos, pos, dtd_syntax_error,
             recover_at_right_angle, message);
    ctl->no_validation = 1;
 }

/*******************************************************************/

/* Doesn't move cursor */
void dtd_error_1(Ctl *ctl, Pos *cur_pos, Pos *pos, char *message) {
    do_error(ctl, cur_pos, pos, dtd_syntax_error,
             no_recover, message);
    ctl->no_validation = 1;
 }

/*******************************************************************/
void do_dtd_subst_error(Ctl *ctl, Pos *cur_pos, Pos *pos, char *message,
                             XString *substitution) {
     char* c_subst;
     char  print_message[500];

     /* approximation.. assumes element & attribute names latin_1 */
     /* for name print                                           */
     c_subst = xstr2cstr(substitution, ctl->is_double);
     sprintf(print_message,  message, c_subst);
     dtd_error(ctl, cur_pos, pos, print_message); 
     free(c_subst);
  }


