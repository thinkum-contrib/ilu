
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

#ifndef WIN32
#include <unistd.h> 
#define GETCWD_FN getcwd
#else
#include <direct.h>
#define GETCWD_FN _getcwd
#endif

#include "xml-types.h"
#include "xml-func.h"

#if (defined(WIN32))
#define DIR_SEPARATOR_CHAR '\\'
#define DIR_SEPARATOR_STG  "\\"
#elif defined( macintosh )
#define DIR_SEPARATOR_CHAR ':'
#define DIR_SEPARATOR_STG ":"
#else
#define DIR_SEPARATOR_CHAR '/'
#define DIR_SEPARATOR_STG  "/"
#endif 

static int is_two_byte(FILE *handle); 

static int get_next_external_buffer(Ctl *ctl, SrcStack *source,
                        int firstref); 
   
/*********************************************************************/
static SrcStack *new_src_stack(Ctl *ctl, StackType stacktype, 
            uchar * internal_loc,  int internal_len, uchar *remainder,
             SrcStack *copy_stack) {

      SrcStack *new_stack_ent;

      new_stack_ent = (void *)calloc(1, sizeof(SrcStack));

      switch (stacktype) { 
         
         case StackTypeExternal:
              new_stack_ent->is_external = 1;
              new_stack_ent->is_remainder = 0;
              new_stack_ent->buffer = (void *)malloc(EXTBUFALLOC);
              new_stack_ent->chars_used = 0;
              break;
          case StackTypeInternal:
              lclmemcpy(new_stack_ent, copy_stack, sizeof(SrcStack));
              new_stack_ent->is_external = 0; 
              new_stack_ent->is_remainder = 0;
              new_stack_ent->buffer = internal_loc;
              new_stack_ent->char_count = internal_len;
              new_stack_ent->chars_used = 0;
              new_stack_ent->last_was_return = 0;
              break;
           case StackTypeRemainder:
              lclmemcpy(new_stack_ent, copy_stack, sizeof(SrcStack));
              new_stack_ent->is_external = 0; 
              new_stack_ent->is_remainder = 1;
              new_stack_ent->buffer = (void *)malloc(internal_len +2);
              lclmemcpy(new_stack_ent->buffer, remainder, internal_len);
              new_stack_ent->char_count = internal_len;
              new_stack_ent->chars_used = 0;
              new_stack_ent->last_was_return = 0;
              break;
       }

       return new_stack_ent;
} 


/*********************************************************************/
/* add a new line index entry for new or existing entity              */
static void update_line_index(Ctl *ctl, int inc, int init) { 
     LineIndexEnt *new_line_ent, *old_line_ent;
     SrcStack     *src_stack;

     src_stack = ctl->current_src_stack;
     if(src_stack == NULL) return;
     /* while(!src_stack->is_external) src_stack = src_stack->prev_stack; */

     /* make new line index entry */ 
     if(ctl->line_list_alloc > ctl->line_ents_used +1) {
         new_line_ent = &(ctl->line_list[ctl->line_ents_used]);
         new_line_ent->file_name = &(src_stack->file_name[0]);  
         new_line_ent->entity_name = &(src_stack->entity->name);
         if(init) {
             new_line_ent->line_number = 1;
             new_line_ent->line_len = 0;
         }
         else if(inc) {
            old_line_ent = &(ctl->line_list[src_stack->cur_line_ent]);
            new_line_ent->line_number = (old_line_ent->line_number)+1;
            new_line_ent->line_len = 0;
            old_line_ent->next_line_index = ctl->line_ents_used;
         }
         else {
            old_line_ent = &(ctl->line_list[src_stack->cur_line_ent]);
            new_line_ent->line_number = old_line_ent->line_number;
            new_line_ent->line_len = old_line_ent->line_len;
            old_line_ent->next_line_index = ctl->line_ents_used;
         }
         src_stack->cur_line_ent = ctl->line_ents_used;
         ctl->line_ents_used++;
         return;  
      }
     /* for present */
     else {
         /* doesn't return*/
        do_phys_error(ctl, "Exceeded line entry allocation.", 1 );
      }

}  

/***********************************************************/
/* internal buffers allocated within internal heaps         */
/* new internal buffer always represents current src stack  */
/* TBD: generalize heaps, use for other structures          */
static IntBufEnt *get_new_int_buf(Ctl* ctl, int copy_last) {  
   int        int_buf_index, int_heap_index;
   IntBufEnt *int_buf, *new_int_buf;
   IntHeap   *int_heap;
   LineIndexEnt *line_ent;
   SrcStack     *src_stack;

   int_buf_index = ctl->int_bufs_used -1;
   assert(int_buf_index >= 0);
   int_buf = &(ctl->int_bufs[int_buf_index]);
   if(int_buf_index  >= (ctl->int_buf_alloc -1)) {
        ctl->int_buf_alloc += INT_BUF_ALLOC;  
        ctl->int_bufs = realloc(ctl->int_bufs,
                ctl->int_buf_alloc * sizeof(IntBufEnt));
        int_buf = &(ctl->int_bufs[int_buf_index]);
   }

    (ctl->int_bufs_used)++;
     int_buf_index++;
     new_int_buf = &(ctl->int_bufs[int_buf_index]);
     if(copy_last) 
             lclmemcpy(new_int_buf, int_buf, sizeof(IntBufEnt));
      else  lclmemset(new_int_buf, 0, sizeof(IntBufEnt));
      int_heap_index = ctl->int_heaps_used -1;
      int_heap = &(ctl->int_heaps[int_heap_index]);
      new_int_buf->buffer =&(int_heap->buffer[int_heap->chars_used]);
      new_int_buf->line_index = ctl->current_src_stack->cur_line_ent;
      line_ent = &ctl->line_list[new_int_buf->line_index];
      new_int_buf->first_line_pos = line_ent->line_len; 

       /* set up for entity nesting check  */
      new_int_buf->entity_instance =
          ctl->current_src_stack->entity_instance;

       /* set up for error reporting including embedding */ 
       src_stack = ctl->current_src_stack;
       while(src_stack != NULL &&  
             src_stack->entity_instance==new_int_buf->entity_instance)
                 src_stack = src_stack->prev_stack;
       if(src_stack != NULL && src_stack != ctl->current_src_stack) 
             new_int_buf->parent_intbuf = src_stack->cur_intbuf_ent;
       else new_int_buf->parent_intbuf = -1;

       ctl->current_src_stack->cur_intbuf_ent = int_buf_index;

        new_int_buf->entity = ctl->current_src_stack->entity;
        new_int_buf->is_file = ctl->current_src_stack->is_external;
        return new_int_buf;
}


void xml_convert_relative_ref(Ctl *ctl, ExternalID* external_id) {

    char      full_name[500]; 
    char     *s, *s1,*ext_name, *scheme;
    SrcStack *stack_ent;

    ext_name = external_id->use_id;

   s = NULL;

    /* check for relative reference.  If not, do nothing */
#if (defined(WIN32))
       if ((*ext_name == DIR_SEPARATOR_CHAR) || (*ext_name == '/') || 
           (*(ext_name+1)) == ':') /* drive id */
#elif defined( macintosh )
     if (*ext-name != DIR_SEPARATOR_CHAR)  /* ?? */ 
#else
     if (*ext_name == DIR_SEPARATOR_CHAR)     
#endif
    return;

    else s = strchr(ext_name, ':');
    if(s != NULL && !strncmp(s, "://", 3)) return;
     stack_ent = ctl->current_src_stack;  
     scheme = (char*)&stack_ent->scheme[0];
     strcpy(full_name, scheme);
     /* what are separator conventions for schemed fn's for pc and mac */ 
     strcat(full_name, "://");
     strcat(full_name, (char *)stack_ent->full_name);
     s1 = strrchr(full_name, DIR_SEPARATOR_CHAR); 
     assert(s1 != NULL);
     *(s1+1) = '\0'; 
      assert( (strlen(full_name) + strlen(ext_name)) < 500);
      strcat(full_name, ext_name);
      external_id->use_id = (void *)malloc(strlen(full_name) +2);
      strcpy(external_id->use_id, full_name);  
  }
     
    


/*****************************************************************/
/* Get first bufferful of doc or parsed entity                   */
/* Check relative ref conversion for pc and mac */
SrcStack * load_first_buffer(Ctl *ctl,  char *ext_name, int initial_call, 
                      Pos *pos, Entity *entity) {

    char      full_name_[500]; 
    char       message[500]; 
    char      *given_name, *full_name, *scheme, *local_file_name;
    char     *s, *s1; 
    uchar     *group;
    int        relative, group_len, recoding;
    SrcStack  *old_stack_ent, *new_stack_ent, *new_stack_ent1; 
    EncType    encoding;  
    IntBufEnt  *int_buf;
    LineIndexEnt *line_ent;

    Pos          save_pos;
    IntBufEnt    save_int_buf; 
    LineIndexEnt save_line_ent;
    SrcStack     save_src_stack, *save_current_src_stack; 
    int          save_int_bufs_used, save_line_ents_used;
    
   /* get scheme */
   /* TBD: make this more exact */  
   scheme = NULL;
   relative = 0;
   given_name = NULL;
   full_name = NULL; 

#if (defined(WIN32)) 
       if ( (*ext_name == DIR_SEPARATOR_CHAR) || (*ext_name == '/') || 
          (*(ext_name+1)) == ':') /* drive id */
#elif defined( macintosh )
     if (*ext-name != DIR_SEPARATOR_CHAR)  /* ?? */ 
#else
     if (*ext_name == DIR_SEPARATOR_CHAR)     
#endif
   {
         scheme = "file";
         full_name = ext_name;
         relative = 0;
   }
   else {
        s = strchr(ext_name, ':'); 
        if(s == NULL)  {
             relative = 1;
             given_name = ext_name;
        } 
        else { 
           if(strncmp(s, "://", 3)) {  
             sprintf(message, "Error in URI %s", ext_name); 
             do_phys_error(ctl, message, 1);
           }
           relative  = 0;
           full_name = s+3;  
           *s = '\0';
           if(!strcmp(ext_name, "file")) scheme = "file";
           /* else if(!strcmp(ext_name, "http)) scheme = "http"; */
           else {
             sprintf(message,"%s scheme not acceptable at this time", scheme); 
             do_phys_error(ctl, message, 1);
           }
           *s = ':';  /* restore */  
       }
   }

  /* if relative, get relative to last buffer if exists, else ... */
  if(relative) {
      if(initial_call) { 
         full_name = GETCWD_FN(NULL, 500);
         strcat(full_name,DIR_SEPARATOR_STG);
         strcat(full_name,given_name);
         scheme = "file";
       }
     /* otherwise get last stack entry and assume relative to that */
      else {
          old_stack_ent = ctl->current_src_stack;  
          strcpy(full_name_, (char *)old_stack_ent->full_name);
          scheme = (char*)&old_stack_ent->scheme[0];
          full_name = &full_name_[0]; 
          s1 = strrchr(full_name, DIR_SEPARATOR_CHAR); 
          assert(s1 != NULL);
          *(s1+1) = '\0'; 
          assert( (strlen(full_name) + strlen(given_name)) < 500);
          strcat(full_name, given_name);
      }
   }         

  local_file_name = full_name; 

/* Enable  soon.... 
   if(!strcmp(scheme,"file"))  local_file_name = full_name;
   else {
       local_file_name = get_schemed_file(scheme, full_name);
       if(local_file_name == NULL) {
            sprintf(message,"Cannot access %s", full_name);
            do_phys_error(ctl, message, 1);
       }
   }
*/ 
   new_stack_ent = new_src_stack(ctl, StackTypeExternal,
                   NULL, 0, NULL, NULL);
   new_stack_ent->full_name = (void *)malloc(strlen(full_name)+2);
   new_stack_ent->file_name = (void *)malloc(strlen(local_file_name)+2);
   new_stack_ent->entity = entity;
   strcpy((char *)new_stack_ent->full_name, full_name);
   strcpy((char *)new_stack_ent->file_name, local_file_name);
   strcpy((char *)new_stack_ent->scheme, scheme);
   new_stack_ent->entity_instance = ctl->next_entity_instance;
   (ctl->next_entity_instance)++;

   new_stack_ent->file_handle = fopen(local_file_name, "r");

   if(new_stack_ent->file_handle == NULL) {
        sprintf(message,"Cannot access %s", full_name);
        do_phys_error(ctl, message, 1);  /* exits */
    }

    /* checks first byte of file for ietf unicode indicator */
    if(is_two_byte(new_stack_ent->file_handle))  {
        if(ctl->is_double) new_stack_ent->two_byte = 1;
         else {
              sprintf(message,"Two byte coded file %s not readable in"
                              "current mode", full_name);
              do_phys_error(ctl, message, 1);  /* exits */
          }
     }
    else new_stack_ent->two_byte = 0;

   /* Restore chars read past current cur-pos       */
   /* if embedding new file                         */
   if(!initial_call) { 
      int_buf =   &(ctl->int_bufs[ctl->int_bufs_used-1]);  
      if((uint32)int_buf->chars_used > pos->char_number) {
        group_len = int_buf->chars_used - pos->char_number;
        group = &(int_buf->buffer[pos->char_number]);  
        new_stack_ent1 = new_src_stack(ctl, StackTypeRemainder, 
                      NULL, group_len, group, ctl->current_src_stack); 
#ifdef undef
       /* THIS CREATES ERRORS - ADDED TO AVOID POTENTIAL TRANSLATE BUG*/
        /* indicates that should create new int_buf  when rtn*/
        new_stack_ent1->chars_used = new_stack_ent1->char_count;
#endif
        int_buf->chars_used = pos->char_number;
         /* link new stack ent */
        new_stack_ent1->prev_stack = ctl->current_src_stack;
        ctl->current_src_stack = new_stack_ent1;
        update_line_index(ctl, 0, 0);
     }
     new_stack_ent->prev_stack = ctl->current_src_stack;
   }

    /* 1 indicates first call for file */
    if(!get_next_external_buffer(ctl, new_stack_ent, 1)) {
            sprintf(message,"Error accessing file %s", full_name);
            do_phys_error(ctl, message, 1);  /* exits */
      } 

    ctl->current_src_stack = new_stack_ent;

  /* first call from base document */
  /* set up initial internal buffer and line entry*/
   if(initial_call){
      int_buf = &ctl->int_bufs[0]; 
      lclmemset(int_buf, 0, sizeof(IntBufEnt));
      int_buf->buffer= ctl->int_heaps[0].buffer; 
      int_buf->line_index = 0; 
      int_buf->is_file = 1;
      int_buf->parent_intbuf = -1;
      ctl->int_bufs_used = 1;
      line_ent =  &(ctl->line_list[0]);  
      line_ent->file_name = ctl->current_src_stack->file_name;
      line_ent->line_number = 1;
      line_ent->line_len = 0;
      ctl->current_src_stack->cur_line_ent = 0;
      ctl->current_src_stack->cur_intbuf_ent = 0;  
      ctl->line_ents_used = 1;
    }
   /* otherwise get new source buffer, internal buffer, and line entry */ 
   else { 
     update_line_index(ctl, 0, 1);
     int_buf = get_new_int_buf(ctl, 0); 
     int_buf->chars_used = 0;
     pos->buffer_number = ctl->int_bufs_used -1;
     pos->char_number = 0;
    }

   /* save current position information in case must retranslate */
   /* for different encoding                                     */
    lclmemcpy(&save_pos, pos, sizeof(Pos));
    lclmemcpy(&save_int_buf, int_buf, sizeof(IntBufEnt));  
    line_ent =  &(ctl->line_list[ctl->line_ents_used]);  
    lclmemcpy(&save_line_ent, line_ent, sizeof(LineIndexEnt));
    save_int_bufs_used = ctl->int_bufs_used;
    save_line_ents_used = ctl->line_ents_used;
    save_current_src_stack = ctl->current_src_stack;
    lclmemcpy(&save_src_stack, ctl->current_src_stack, sizeof(SrcStack));
    save_src_stack.reached_end_of_file = 0;
     
   /* parse xml_decl or text_decl */ 
   if(initial_call) encoding = parse_xml_decl (ctl, pos, 0);
   else   encoding =parse_text_decl(ctl, pos, 0);

   new_stack_ent->encoding = encoding;

   /*check compatibility of encoding assumed on read with */ 
   /* declared encoding or default                        */
    recoding = 0;
   if(encoding == ENCODING_UNSTATED) { 
      if(new_stack_ent->two_byte) new_stack_ent->encoding = UTF_16;
      else new_stack_ent->encoding = DEFAULT_8_ENCODING; 
    }   

   /* this should be generalized */
   else switch(encoding) {

       case ASCII:
           if(new_stack_ent->two_byte) {	
              sprintf(message,"File %s encoding specification conflicts"
                 "with actual format", full_name);
               do_phys_error(ctl, message, 1);  /* exits */
           }
           break;
       case ISO_8859_1: 
           if(new_stack_ent->two_byte) {
               sprintf(message,"File %s encoding specification conflicts"
                 "with actual format", full_name);
               do_phys_error(ctl, message, 1);  /* exits */
           }
           if(DEFAULT_8_ENCODING == UTF_8) recoding = 1; 
           break;

       case UTF_8:
           if(new_stack_ent->two_byte) {
               sprintf(message,"File %s encoding specification conflicts"
                 "with actual format", full_name);
               do_phys_error(ctl, message, 1);  /* exits */
           }
           if(DEFAULT_8_ENCODING != UTF_8) recoding = 1; 
           break;

       case UTF_16:
           if(!new_stack_ent->two_byte) {
               sprintf(message,"File %s encoding specification conflicts"
                 "with actual format", full_name);
               do_phys_error(ctl, message, 1);  /* exits */
           }
           break;
       default: assert(0);
  } /*end switch */

  if(recoding) {
         /* restore state */ 
         lclmemcpy(pos, &save_pos,  sizeof(Pos));
         lclmemcpy(int_buf, &save_int_buf, sizeof(IntBufEnt));  
         ctl->line_ents_used = save_line_ents_used;
         line_ent =  &(ctl->line_list[ctl->line_ents_used]);  
         lclmemcpy(line_ent, &save_line_ent, sizeof(LineIndexEnt));
         lclmemcpy(ctl->current_src_stack, &save_src_stack, sizeof(SrcStack));
         ctl->int_bufs_used = save_int_bufs_used;
 
         /* reprocess */
        freopen(local_file_name, "r", new_stack_ent->file_handle);
        if(new_stack_ent->two_byte) fgetc(new_stack_ent->file_handle);
        if(!get_next_external_buffer(ctl, new_stack_ent, 0)) {
            sprintf(message,"Error accessing file %s", full_name);
            do_phys_error(ctl, message, 1);  /* exits */
        } 
       
         /* bypass  xml_decl or text_decl */ 
        if(initial_call) 
          encoding = parse_xml_decl (ctl, pos, 1);
       else  encoding =parse_text_decl(ctl, pos, 1);
   }

  return new_stack_ent; 
 }   
 

static int is_two_byte(FILE *handle) {
   if(fgetc(handle) == 0xfeff) return 1;
   rewind(handle);
   return 0;
}

/*********************************************************************/
int valid_entity_nesting(Ctl *ctl, int first_buf_ind, int final_buf_ind) {
 
   IntBufEnt *int_buf1, *int_buf2;
   SrcStack  *source, *source1;
   int        buf_ind2;

   /* If begin and end internal buffer same, nesting ok w/o further check */  
   if(first_buf_ind == final_buf_ind) return 1;   
 
   /* But several entities may overlap at entity begins or ends, */  
   /* so check surrounding buffers                               */
   /* If have null content, were immediately replaced by definitions */
 
    
    int_buf1 = &ctl->int_bufs[first_buf_ind];  
    int_buf2 = &ctl->int_bufs[final_buf_ind];
    /* set up for current entity just completed */
    buf_ind2 = final_buf_ind;
 
   /* if current source buffers represent same entity instance, also ok*/ 
   /*(was some substitution in the middle                              */
    if(int_buf1->entity_instance == int_buf2->entity_instance) return 1; 
    
    source = source1 = ctl->current_src_stack; 

    if(source == NULL) return 1;

    if(source->entity_instance == int_buf1->entity_instance) return 1;

    /* if current position at very begin of return to parent .. */
    /* because was some trailing whitespace in included entity   */
    int_buf2 = &ctl->int_bufs[buf_ind2];
    while(int_buf2->chars_used == 0) {
        buf_ind2--;
        int_buf2 = &ctl->int_bufs[buf_ind2];
        if(int_buf1->entity_instance == int_buf2->entity_instance)
           return 1;
     }
       
    /* CHECK THIS.. PROBABLY DOESN'T MAKE SENSE */
    while( (source1->chars_used >= source1->char_count) &&
          ((!source1->is_external) || (!source1->reached_end_of_file))) {
          source1 = source1->prev_stack;  
          if(source1 == NULL) return 0; 
          if(source1->entity_instance == int_buf1->entity_instance) return 1; 
    }
    return 0;
}

/*********************************************************************/
/* if return 0, signal that not substituted.  May or may not be error */ 
static int get_entity(Ctl *ctl, Pos *cur_pos, XString char_group, 
               int *is_one_char, uchar *single_char,
               SrcStack **new_stack_ent){ 

    uchar  *num_char, *x_char; 
    Entity *entity;
    IntBufEnt *new_int_buf;
    uint16 codepoint;
    int     parameter_entity = 0;
    char  *ext_name;
    int     in_dtd = ES_internal_dtd | ES_external_dtd; 
    uint16  es_state; 
    XString tempstr;
    SrcStack *tempstack;

    es_state = cur_pos->ES_state;

    /* get entity type from ref */
    *is_one_char = 0;
    if(is_special_p(ctl, char_group.loc, AMPERSAND_CHAR)) { 
        num_char = char_group.loc + 1 + ctl->is_double;
        if(is_special_p(ctl, num_char, HASHMARK_CHAR)) {
           char_group.loc[char_group.len+1] = ';';
           x_char = num_char + 1 + ctl->is_double; 
           /* if is hex, just expand to required size */
           if(is_x(ctl, x_char)) {
             if(!convert_char_parameter(ctl, x_char+1+ctl->is_double,
                         0, &codepoint))
               return 0;
           }
           else {
             if(!convert_char_parameter(ctl, x_char,
                         1, &codepoint))
               return 0;
           }
           
          *is_one_char = 1;

          single_char[0] = codepoint/256; 
          single_char[1] = codepoint%256;
                 
          /* Set as escaped after initial translate, */  
          /* if is xml ctl char < > ', ", & ,       */
          /* by substituting special character      */
          /* If appears again in input (e.g.by entity substitution*/
          /* remove escape                                       */
          get_escaped(ctl, single_char); 
 
         /* determine acceptability conditions for charref */
         if (es_state & in_dtd) { 
            if( !(es_state & (ES_entityvalue | ES_attvalue))) { 
                do_error(ctl, cur_pos, cur_pos, 
                   dtd_syntax_error,  no_recover,
                     "Character reference in dtd outside"
                     " attribute value or entity value"); 
                 return 0;
              }
          }
          return 1;
       } /* if has HASHMARK */
           
       /* now will be general entity */
       parameter_entity = 0;

        if( (es_state & in_dtd) &&
            ( ( es_state & ES_entityvalue) || (es_state & ES_attvalue)) )
               return 0; /* just bypass, will expand when reference */

       entity = get_ref_entity_def(ctl, parameter_entity, char_group,1);
       if(entity== NULL) {
           do_error(ctl, cur_pos, cur_pos, validation_error, no_recover,
             "Reference to undefined entity"); 
              return 0;
       }
   }    
   else { 
       /* parameter entity not recognized outside dtd */ 
       /* or in dtd if in attribute default decl  */
       if(!(es_state & in_dtd)) return 0;
       if(es_state & ES_attvalue) return 0;

       parameter_entity = 1;
       entity = get_ref_entity_def(ctl, parameter_entity, char_group, 1);
       if(entity== NULL) {
           do_error(ctl, cur_pos, cur_pos, validation_error, no_recover,
                  "Reference to undefined entity"); 
              return 0;
       }
   }    


  /*Compare entity type with current ES state */

   if(parameter_entity && !(es_state & ES_first_mkup_char)  
      && (es_state & ES_internal_dtd) 
      && !(es_state & ES_entityvalue)) {
          do_error(ctl, cur_pos, cur_pos, formation_error, no_recover,
                "Reference to parameter entity in internal" 
               " subset not as first character in markup decl"); 
   }

   if(entity->unparsed) {
       /* unparsed entity referenceable only as attribute value */ 
       /* and without the &ref                                 */
          do_phys_error(ctl, "Reference to unparsed entity" 
                        " not permitted except as attribute value", 0);
           return 0;
   }
   /* external parsed entity referenceable only in element content */  
   /* and ignored if in a dtd entity value                         */
   else if( (!parameter_entity) && !entity->internal)  { 
        if( es_state & in_dtd) { 
          if( !(es_state & ES_entityvalue)){ 
            do_error(ctl, cur_pos, cur_pos, validation_error,
               no_recover,  "Reference to parsed entity" 
              " not permitted in dtd except within entity value");
              return 0;
           }
        }
         /* not in dtd */
         else if( es_state & ES_attvalue ) {
           do_error(ctl, cur_pos, cur_pos, formation_error, no_recover,
            "Reference to parsed entity not permitted in attribute value");
              return 0;
         }
    } 
    
  /* internal general entity referenceable in dtd only in EntityValue */
  /* or attribute value default                                     */ 
   else if( (!parameter_entity) && entity->internal ) {    
        if( es_state & in_dtd) { 
           if( es_state & ES_entityvalue)
               return 0; /* just bypass, will expand when reference */
           else if( !(es_state & ES_attvalue)) {
              do_error(ctl, cur_pos, cur_pos, validation_error, no_recover,
                "Reference to internal general entity" 
               " in dtd permitted only in entity value and attribute"
               " default value ");
               return 0;
            }  
        } /* end if es_state */
    }

  /* check for circular ref */  
  tempstack = ctl->current_src_stack; 
  while(tempstack != NULL) {

      if( tempstack->entity == entity) {
              do_error(ctl, cur_pos, cur_pos, validation_error, no_recover,
               "Circular entity reference");
              return 0;
       }
       tempstack = tempstack->prev_stack;
   }


  /* make new source stack entry for entity */
  if(entity->internal) {  
      /*Add blanks around pe references in DTD outside */
      /* attributevalue or entityvalue                 */ 
      /* (should also do for external pe ref?          */
      if(parameter_entity 
         && (!(es_state & ES_attvalue))
         && (!(es_state & ES_entityvalue )) ) {
             get_blank_surrounded(ctl, &tempstr, &entity->entity_string);
            (*new_stack_ent) = new_src_stack(ctl, StackTypeInternal,
                                    tempstr.loc, tempstr.len, 
                                    NULL, ctl->current_src_stack);
         }

     /* if is in attribute value or entityvalue, escape contained quotes*/ 
      else if( (es_state & ES_attvalue) || (es_state & ES_entityvalue)) {
            escape_contained_quotes(ctl, &tempstr, &entity->entity_string); 
            (*new_stack_ent) = new_src_stack(ctl, StackTypeInternal,
                                    tempstr.loc, tempstr.len, 
                                    NULL, ctl->current_src_stack);
     }
     

     else  (*new_stack_ent) = new_src_stack(ctl, StackTypeInternal,
                    entity->entity_string.loc, entity->entity_string.len, 
                    NULL, ctl->current_src_stack);

     /* if is in attribute value or entityvalue, escape contained quotes*/ 
     

     (*new_stack_ent)->entity = entity;
     (*new_stack_ent)->entity_instance = ctl->next_entity_instance;
     (ctl->next_entity_instance)++; 
     (*new_stack_ent)->prev_stack = ctl->current_src_stack;
     ctl->current_src_stack = (*new_stack_ent);
     update_line_index(ctl, 0, 1);
     /* make new internal buffer to represent new entity */
      new_int_buf = get_new_int_buf(ctl, 0);
      new_int_buf->chars_used = 0;
  }

  else {
      ext_name =(char *)entity->external_id.use_id;
     (*new_stack_ent) = load_first_buffer(ctl, ext_name, 0, cur_pos,
                         entity);
  }     
     
  return 1;
 }
 
/*********************************************************************/
/* Get next buffer for doc or external parsed entity                 */
static int get_next_external_buffer(Ctl *ctl, SrcStack *source, int first_ref) { 

    unsigned int i, j, read_count;
    unsigned int twobyte_read_count = EXTBUFALLOC-2;
    unsigned int initial_onebyte_read_count = 500;
    unsigned int normal_onebyte_read_count = EXTBUFALLOC/2;
    uchar buffer[(EXTBUFALLOC/2) +2], first_byte, second_byte, inter;
    EncType encoding; 
    
    source->char_count = 0;
    source->chars_used = 0;
    if(source->reached_end_of_file) return 0;
    clearerr(source->file_handle);

     if(first_ref) {
       read_count = 500;    /* in case have to recode*/
        /* indicates two-byte encoding of file */
       if(fgetc(source->file_handle) == 0xfeff) {
             source->two_byte = 1;
             encoding = UTF_16;
             read_count = twobyte_read_count;
       }
       else {
           rewind(source->file_handle);
           encoding = DEFAULT_8_ENCODING;
           read_count = initial_onebyte_read_count; 
       }
     }

     else {
       if(source->encoding == UTF_16) read_count = twobyte_read_count;
       read_count = normal_onebyte_read_count; 
       encoding = source->encoding; 
     }
       
     /* if two byte source need no translate -  at least at present */
     /* EXTBUFALLOC must be even number                             */
     if(encoding == UTF_16) source->char_count =
         fread( &source->buffer[0], sizeof(uchar), read_count,
                  source->file_handle);

     /* provide for UTF-8, by allowing for read of extra chars  */
     else source->char_count=
          fread( &buffer[0], sizeof(uchar), read_count, source->file_handle);

     if(source->char_count == 0) {
         if(ferror(source->file_handle)) {
            do_phys_error(ctl, "File read error",1); 
        }
        if(feof(source->file_handle)) {
            fclose(source->file_handle);
            source->reached_end_of_file = 1;
        }
     }
    
     if (source->char_count < read_count)
                source->reached_end_of_file = 1;

    if(encoding == UTF_16) return 1; 

    /* if null file, just process normally */
    if(source->char_count == 0) return 1;

    /* if not first ref, know encoding       */
    /* if not that, just plug in initial zero bytes */ 
    if( encoding != UTF_8) { 
       for(i = 0, j = 0; j < source->char_count; j++, i+=2 ) {
          source->buffer[i] = 0x0;
         source->buffer[i+1] = buffer[j];
      }  /* end for i */   
       source->char_count = (2 * j)-1; 
      return 1;
   }

   /* UTF-8 */
   i = 0;
   j = 0;
   while (j < source->char_count ) { 
       /* first char */
       /* if one byte, just expand */
       if( (buffer[j] & 0x80) == 0) {
           first_byte = 0x0;
           second_byte = buffer[j];
           j++;
       }
       /* check for two byte */
       else if(buffer[j] <  0xE0) {
           /* rep is 110xxxxx 10xxxxxx */
           if(j == source->char_count) buffer[j+1] =
                          fgetc(source->file_handle); 
           first_byte = buffer[j] - 0xC0; /* 000xxxxx*/
           first_byte = first_byte  >> 2;  /* 00000xxx*/
           second_byte = buffer[j] << 6;   /* xx000000*/ 
           second_byte = second_byte | (buffer[j+1] - 0x80); 
           j+=2;
      }
      /* three byte */
       /* rep is 1110xxxx 10xxxxxx 10 xxxxxx */ 
      else {
         if(j == source->char_count) buffer[j+1] = 
                          fgetc(source->file_handle); 
         if(j+1 == source->char_count) buffer[j+1] = 
                          fgetc(source->file_handle); 
         first_byte = (buffer[j]-0xE0) << 4;  /* xxxx0000*/
         /* remove leading 10 and shift right 2 */
         inter = ((uchar)(buffer[j+1] -  0x80)) >>2;   /* 0000xxxx*/  
         first_byte = first_byte | inter; 
         second_byte = buffer[j+1] << 6;  /* xx000000 */
         second_byte = second_byte | (buffer[j+2]-0x80);
         j+= 3;
      }
     source->buffer[i] = first_byte;
     source->buffer[i+1] = second_byte;
     i+= 2;
   }
   source->char_count = i;
         
   return 1; 
}
           
           

#ifdef undef
/***********************************************************/
static IntHeap *get_available_int_heap(Ctl *ctl, int len) {
     int      int_heap_index;
     IntHeap *int_heap; 

     int_heap_index = ctl->int_heaps_used -1;
     int_heap = &(ctl->int_heaps[int_heap_index]);
     if(int_heap->chars_used + len + 2 >= ctl->int_heap_size)  {
        if(int_heap_index +1 < ctl->int_heap_alloc) {  
             /* allocate new heap */
              (ctl->int_heaps_used)++;
              int_heap_index++;
              int_heap = &ctl->int_heaps[int_heap_index];
              lclmemset(int_heap, 0, sizeof(IntHeap)); 
              int_heap->buffer = (void *)calloc(ctl->int_heap_size,
                                  sizeof(uchar));
              int_heap->chars_used = 0;
        }
         else {  /* for now */
             do_phys_error(ctl, "Internal buffers exhausted", 1);
         }
     }

      return int_heap;
}
#endif


/*********************************************************************/
static void store_in_internal(Ctl *ctl, uchar *this, IntBufEnt **rtn_int_buf){
    int       int_buf_index, int_heap_index, i;
    IntBufEnt *int_buf, *old_int_buf;
    IntHeap   *int_heap, *first_int_heap, *this_int_heap;  

    int_buf_index = ctl->int_bufs_used -1;
    int_buf = &ctl->int_bufs[int_buf_index];
    int_heap_index = ctl->int_heaps_used -1; 
    int_heap = &ctl->int_heaps[int_heap_index];  

    /* if have used up heap..... */
    if(int_heap->chars_used >= ctl->int_heap_size) { 

       /* have to start new heap and split current logical buffer */
       if(int_heap_index +1 >= ctl->int_heap_alloc) {  

           /* recycle int heaps. ok because very limited backtrack */
           /* (remainder buffers used for source continuation after */
           /* embed                                                */
           first_int_heap = &(ctl->int_heaps[0]);
           lclmemcpy(first_int_heap, int_heap, sizeof(IntHeap));
           for(i =1; i < (ctl->int_heap_alloc-1); i++){ 
               this_int_heap = &(ctl->int_heaps[i]); 
               free(this_int_heap->buffer);
               lclmemset(this_int_heap, 0, sizeof(IntHeap));
           }
           ctl->int_heaps_used = 1;
           int_heap_index = 0;
           int_heap = first_int_heap; 
        }

        /* allocate new heap */
        (ctl->int_heaps_used)++;
        int_heap_index++;
        int_heap = &ctl->int_heaps[int_heap_index];
        lclmemset(int_heap, 0, sizeof(IntHeap)); 
        int_heap->buffer = (void *)calloc(ctl->int_heap_size,
                           sizeof(uchar));
        int_heap->chars_used = 0;

        /* and new logical buffer */ 
        old_int_buf = int_buf; 
        int_buf = get_new_int_buf(ctl, 1); /* copies data */
        int_buf->chars_used = 0;  
        int_buf->is_continuation = 1;
   }

  append_character(ctl, int_buf->buffer,
                         &int_buf->chars_used, this);
  int_heap->chars_used += 1 +ctl->is_double;
  
  *rtn_int_buf = int_buf;   
}

/*********************************************************************/
/* Get next physical character, either input or as part of entity    */ 
/* substitution and add to internal (intbuf) buffer.                 */ 
/* Includes identification and replacement of new entity refs       */ 
/* "Physical" buffers handled as stack, with topmost the current   */
/* buffer-ful of docuemnt, internal or parsed entity                */
/* Some additional stack elements used for special purposes        */ 
static int get_next_external_char(Ctl *ctl, Pos *cur_pos,
                    IntBufEnt **rtn_buf, int *rtn_index) {
       XString    char_group;
       uchar      group_loc[MAX_ENTITY_REF_SIZE], single_char[2];  
       uchar      temp_char[2];
       uchar      *this_char, *newbuf;
       int        in_entity_ref, is_one_char, tr_quote, tr_special;
       int        is_scan_special, in_entity_ref_begin;
       SrcStack  *source, *temp_source, *new_stack_ent, *new_stack_ent1;
       SpecialEnum thespecial;
       uchar      eot_char[2] = {0x0, 0x3};
       uchar      eot1_char[2] = {0x3, 0x0};  
       uint16     ignore_entity_ref =  ES_PI | ES_CDATA | ES_COMMENT; 
       uint16     use_pe_ref =  ES_internal_dtd | ES_external_dtd;
       IntBufEnt *new_int_buf; 
       
       char_group.loc = &group_loc[0];
       char_group.len = 0;
       char_group.loc[0] = '\0';
       char_group.loc[1] = '\0';

       in_entity_ref = in_entity_ref_begin = 0;
       source = ctl->current_src_stack;
       if(source == NULL) {
           if(ctl->is_double){
                     store_in_internal(ctl, eot_char, rtn_buf);
                     *rtn_index = (*rtn_buf)->chars_used -2;
           }
            else{
                      store_in_internal(ctl, eot1_char, rtn_buf);
                     *rtn_index = (*rtn_buf)->chars_used -1;
             }
            return 1;
       }
       
       while(1) {
           /* if unprocessed characters remaining in physical buffer */
           /* get next one...                                       */
           if( source->chars_used < source->char_count ) {
             this_char = &(source->buffer[source->chars_used]);
             source->chars_used += (1 + ctl->is_double);

             /* see if special handling to be done for character */
             /* get_specials also translates escaped chars back*/
             /* if within dtd. Not sure if sufficient         */  
            tr_quote = tr_special = 0;
            if( (cur_pos->ES_state & use_pe_ref) != 0) 
                 tr_quote = tr_special = 1;
            if( (cur_pos->ES_state & ES_entityvalue ) || 
                (cur_pos->ES_state & ES_attvalue))  
                 tr_quote = 0;
             is_scan_special=  get_specials_code
                     ( ctl, this_char, &thespecial, tr_special, tr_quote);

             if(!in_entity_ref) {
                if(is_scan_special) { 
                   if(thespecial == RETURN_CHAR) {
                       /*modify cr to linefeed */
                       get_linefeed(ctl, temp_char);
                       this_char = &temp_char[0];
                       source->last_was_return = 1;
                   }
                   /*if last was cr, ignore following linefeed */  
                    else if(source->last_was_return) {
                        source->last_was_return = 0;
                        if(thespecial == LINEFEED_CHAR) 
                           return(get_next_external_char(ctl, cur_pos,
                               rtn_buf, rtn_index));
                   }

                   /*Ignore entity ref begin completely if not */
                   /*recognized in context                     */  
                   else if( (cur_pos->ES_state & ignore_entity_ref)
                         == 0 ) {
                         if( thespecial == AMPERSAND_CHAR  ||
                           (thespecial == PERCENT_CHAR &&
                            ((cur_pos->ES_state & use_pe_ref) != 0))) {
                              in_entity_ref = 1;
                              in_entity_ref_begin = 1;
                              append_xchar(ctl, &char_group,
                                           this_char);
                    }
                  }
                } /* end if is_scan_special */

                /* if character not beginning or continuation of */ 
                /* entity ref, return it                         */
                if(!in_entity_ref) {
                   store_in_internal(ctl, this_char, rtn_buf); 
                   *rtn_index = (*rtn_buf)->chars_used-(1 +ctl->is_double);
                   return 1;
                }
             } /* end if !in_entity_ref*/

             /* In entity ref; see if this character (a) continues  */ 
             /* ref, (b) completes it (by ;), or (c) indicates      */
             /* that not really entity ref                         */ 
             else  {
                /* TBD: check for empty entity def */ 
                if(is_scan_special) {
                  if(in_entity_ref_begin && thespecial == HASHMARK_CHAR) {
                      in_entity_ref_begin = 0;
                      append_xchar(ctl,&char_group, this_char);
                      continue;
                   } 
                    /* SEMICOLON_CHAR is entity ref end char */
                   else if(thespecial == SEMICOLON_CHAR) break;
                 }
                if((in_entity_ref_begin && name_begin_char(ctl,this_char))
                   || ((!in_entity_ref_begin) && name_char(ctl, this_char)))
                   {
                     in_entity_ref_begin = 0;
                     append_xchar(ctl,&char_group, this_char);
                    }
                else {
                     /* not really  ref. return initial character */
                     /* and put rest in  buffer                  */
                     /* (to avoid repeating this) */
                     /* make new source stack entry for entity */
                     if( ( (cur_pos->ES_state & use_pe_ref) != 0)  
                         && char_group.len > 4)
                       do_error(ctl, cur_pos, cur_pos, warning_error,
                           no_recover,
                         "Possible entity ref not terminated by ;");
                     append_xchar(ctl,&char_group, this_char);
                     newbuf = (void *)calloc(1,char_group.len + 2); 
                     lclmemcpy(newbuf, char_group.loc, char_group.len);  
                     new_stack_ent = (void *)calloc(1, sizeof(SrcStack));
                     new_stack_ent->buffer = newbuf;
                     new_stack_ent->chars_used = 1 +ctl->is_double; 
                     new_stack_ent->char_count = (uint32) char_group.len; 
                     new_stack_ent->is_extra = 1;
                      /* do something about entity asscoiation of entry*/
                     /* link new stack ent */
                     new_stack_ent->prev_stack = source;
                     ctl->current_src_stack = new_stack_ent;
                     /* return the initial character of ref */ 
                     store_in_internal(ctl, char_group.loc, rtn_buf); 
                     *rtn_index = (int) ((*rtn_buf)->chars_used -
                                   (1 + ctl->is_double));
                     return  1;
                 }
             }

           } /* end if(source->chars_used*/

          /* else if at end of source buffer, must get prev source buffer*/
          /* or next external block                                      */ 
         else {
            if(source->is_external && !source->reached_end_of_file) {
                 if(get_next_external_buffer(ctl, source, 0)) continue; 
             }
             if(source != NULL) {
                temp_source = source;
                source = source->prev_stack;
                ctl->current_src_stack = source;
                if(temp_source->is_external) free(temp_source->buffer);
                free(temp_source);
                update_line_index(ctl, 0, 0);  /* don't increment*/ 
             }
 
             if(source != NULL) { 
               /* make new internal buffer to represent continuation */
               /* of previous physical source                        */
                new_int_buf = get_new_int_buf(ctl, 0); 
                new_int_buf->chars_used = 0;
                new_int_buf->is_continuation = 1;
#ifdef undef
                /* THIS CREATES PROBLEMS.. REMOVED */
                /* and fill it if last is Remainder stack with */ 
                /*all characters used                         */
                /* TBD:Also should check for heap overflow */
                if(source->is_remainder &&
                    source->chars_used == source->char_count) {
                    lclmemcpy(new_int_buf->buffer, source->buffer,
                           source->chars_used);
                    new_int_buf->chars_used = (int)source->chars_used;
                    *rtn_buf = new_int_buf;
                    *rtn_index = 0; 
                     return 1;
                  }
#endif
             }
             /* if source  null is end of document */
             else {
                if(ctl->is_double) {
                     store_in_internal(ctl, eot_char, rtn_buf);
                     *rtn_index = (*rtn_buf)->chars_used -2;
                 }
                  else{
                      store_in_internal(ctl, eot1_char, rtn_buf);
                     *rtn_index = (*rtn_buf)->chars_used - 1;
                }
                return 1;
             }

           continue; 
         }
       } /* end while(1) */
       
      /* now should have an entity reference */ 
      is_one_char = 0;
      new_stack_ent = NULL;
      /* if can't find entity or should be ignored - error or not*/ 
      /* just use reference string                              */ 
      /* make new source stack ent to show remainder because may */
      /* have refilled source buffer                           */
      if(!get_entity(ctl, cur_pos, char_group, &is_one_char,
                 &single_char[0], &new_stack_ent)){ 
           /* make new buffer to represent continuation */
            append_xchar(ctl, &char_group, this_char);
            new_stack_ent1 = new_src_stack(ctl,  
                             StackTypeRemainder, NULL, 
                             char_group.len, char_group.loc,
                             ctl->current_src_stack);
            new_stack_ent1->chars_used = 1 +ctl->is_double;
            /* link new stack ent */
            new_stack_ent1->prev_stack = ctl->current_src_stack;
            ctl->current_src_stack = new_stack_ent1;
            store_in_internal(ctl, char_group.loc, rtn_buf);
           *rtn_index = (*rtn_buf)->chars_used -(1 +ctl->is_double);
            return 1;
     }
        
      /* If entity is just single character, return as output */
      /* fix mismatch in single_char */
      if(is_one_char) {
         store_in_internal(ctl, single_char, rtn_buf); 
        *rtn_index = (*rtn_buf)->chars_used -(1 + ctl->is_double);
         return 1;
      }

      /* otherwise everything already linked by get_entity*/
      /* status of int buf will distinguish file entity from others */ 
      *rtn_buf= &ctl->int_bufs[ctl->int_bufs_used-1]; 
      /* if not file entity*/
       if( (*rtn_buf)->chars_used == 0)  
           return(get_next_external_char(ctl, cur_pos, rtn_buf, rtn_index));
       /* otherwise may be stuff in int_buf because of text-decl scan*/
       else if( cur_pos->char_number >= (uint32)((*rtn_buf)->chars_used) ) 
           return(get_next_external_char(ctl, cur_pos, rtn_buf,
                  rtn_index));
       else { 
            *rtn_index = cur_pos->char_number; 
            return 1;
       }
 }


/*********************************************************************/
static void check_line_index( Ctl *ctl) {

     LineIndexEnt *line_ent;

     /*update and then check for new to ensure count newline char */ 
     line_ent = &(ctl->line_list[ctl->line_ents_used-1]);
     line_ent->line_len += 1 + ctl->is_double;

     if(ctl->next_is_newline) { 
          update_line_index(ctl, 1,0);  
          ctl->next_is_newline = 0; 
      }
 }


/*********************************************************************/
/* Entered with cur_pos pointing past last character accepted        */
/* return next char and update cur_pos if matches request            */
uchar *next_char(Ctl *ctl, Pos *cur_pos,
                    CharacterRequest chartype,
                    SpecialEnum      special,
                    int              match) {

     uchar      the_char[10] = "\0\0\0\0"; 
     uchar      *return_char;
     int        int_buf_index, buffer_number, char_num, rtn_index;
     IntBufEnt *int_buf, *rtn_int_buf; 
     

     buffer_number = cur_pos->buffer_number;
     char_num = cur_pos->char_number;
     int_buf = &ctl->int_bufs[buffer_number];

     /* FInd first buffer with to-be-processed characters      */  
     /* account for possibly empty entity substitution buffers */
     if(  (buffer_number < (ctl->int_bufs_used -1)) &&
        char_num >= int_buf->chars_used) {  
        buffer_number++;
        int_buf = &ctl->int_bufs[buffer_number];
        char_num = 0;
        while( buffer_number < (ctl->int_bufs_used -1) ) {
             int_buf = &ctl->int_bufs[buffer_number];
             if(char_num < int_buf->chars_used) break;
             buffer_number++;
        }
        int_buf = &ctl->int_bufs[buffer_number];
     }     
           

     /* if cur pos is in last internal buffer  */
     if(buffer_number == ctl->int_bufs_used -1) {
        /* if at last character added in current internal buffer */
        if(char_num >= int_buf->chars_used) { 
           if( get_next_external_char(ctl, cur_pos, &rtn_int_buf,
                           &rtn_index) == 0) 
              return NULL;
           
           int_buf = rtn_int_buf;

           lclmemcpy(the_char, &(rtn_int_buf->buffer[rtn_index]),
                 1 +ctl->is_double);

           if ( is_special_p(ctl, the_char, LINEFEED_CHAR)) 
               ctl->next_is_newline= 1;

           check_line_index(ctl);  /* update if newline */

           int_buf_index = ctl->int_bufs_used -1;

           /* if next char corresponds to requested type */
           /* modify cur_pos                             */
           if(!matches_request(ctl, the_char, chartype, special, match)) 
                return NULL;

           int_buf = &ctl->int_bufs[int_buf_index];

           cur_pos->buffer_number = int_buf_index;
           cur_pos->char_number = rtn_index+(1 + ctl->is_double);

           return_char = &(int_buf->buffer[rtn_index]); 
#ifdef DEBUG
          fprintf(stderr,"\n  buf %d", cur_pos->buffer_number);  
          fprintf(stderr,"  entity %d", int_buf->entity_instance);
          fprintf(stderr,"  charnum %d", cur_pos->char_number);
          if(ctl->is_double) fprintf(stderr,"  char %s\n", &the_char[1]);
          else   fprintf(stderr,"   char %s\n", &the_char[0]);
          fflush(stderr);
#endif
    
           return return_char;
       }
   } 

   /* otherwise next is character already parsed */
   /* just update current position in internal buffers */

     return_char = &(int_buf->buffer[char_num]);
     
     if(!matches_request(ctl, return_char, chartype, special, match)) 
         return NULL;

     cur_pos->buffer_number = buffer_number;
     cur_pos->char_number = char_num + (1 + ctl->is_double);
#ifdef DEBUG
          int_buf = &ctl->int_bufs[buffer_number];
          fprintf(stderr,"\n Rbuf %d", cur_pos->buffer_number);  
          fprintf(stderr,"  entity %d", int_buf->entity_instance);
          fprintf(stderr,"  charnum %d", cur_pos->char_number);
          if(ctl->is_double) fprintf(stderr,"  char %s\n", &the_char[1]);
          else   fprintf(stderr,"   char %s\n", &the_char[0]);
          fflush(stderr);
#endif
    
     return return_char;
}   

/*********************************************************************/

void get_new_string_buf (Ctl *ctl, int min_len) {

  StringBufEnt *string_buf_ent; 
  int           size;

  /* TBD: change next to allow realloc */ 
  assert(ctl->string_buf_ents_used < STRING_BUF_LIST_ALLOC-1);
  string_buf_ent =  &(ctl->string_buf_list[ctl->string_buf_ents_used]);
  size = STRINGBUFALLOC;
  if(min_len >= size)  size += min_len;
  string_buf_ent->buffer = (void *)calloc(1, size);
  ctl->string_buf_max = string_buf_ent->buffer+(size-1);
  ctl->string_buf_pos = string_buf_ent->buffer; /* in current string buf */
  (ctl->string_buf_ents_used)++;
}


/*********************************************************************/
int end_of_document(Ctl *ctl, Pos *pos){
    IntBufEnt *int_buf;

    if(ctl->current_src_stack == NULL) return 1;
    if( next_char(ctl, pos, CT_Specific, EOT_CHAR, 1) != NULL) {
      int_buf = &ctl->int_bufs[ctl->int_bufs_used -1];
      if(int_buf->entity_instance == 0) return 1;
    }
    return 0;
}
          
