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
#include <ctype.h>
#include "parse.h"

extern jmp_buf NextRuleEnv;
extern jmp_buf NextFunctionEnv;

/*      do something about newlines */
#define NEWLINE '\n'
#define INBUFALLOC   20000
#define OUTBUFALLOC  10000

void do_error(PCtl *ctl,  Pos *pos,
               ErrorType error_type, const char *message);
/************************************************************************/
/*                STRUCTURE ALLOCATION                                   */
/************************************************************************/

/* Macro for allocating arrays of structures rooted in preprocessor ctl */
#define  allocate_x(x)\
static void  allocate_##x##s(PCtl *ctl) {\
     ctl -> x##s = (void *)calloc(initial_##x##_alloc, sizeof(x));\
     ctl -> allocated_##x##_offsets = initial_##x##_alloc;\
     ctl -> next_##x##_offset = 1;}

allocate_x(Line)
allocate_x(CExp)
allocate_x(Rule)
allocate_x(RuleNode)
allocate_x(PropDesc)
allocate_x(Edge)
allocate_x(BufInfo)
allocate_x(InputLine)
allocate_x(Scope)
allocate_x(Function)
allocate_x(CLongInt)
allocate_x(ExtRuleRef)
allocate_x(OutBufInfo)

/*************************************************************************/
/* Macro for getting next structure in arrays rooted in preprocessor ctl  */
/* Zeroes and returns ptr to next available structure of type x,      */
/* reallocating if necessary.  Updates next available structure offset */

#define  new_x(x)\
x  *new_##x(PCtl *ctl) {\
     x  *return_##x;\
     if(ctl->next_##x##_offset >= ctl->allocated_##x##_offsets) {\
           ctl->allocated_##x##_offsets += initial_##x##_alloc;\
           ctl -> x##s = (void *)realloc(ctl -> x##s,\
                       ctl->allocated_##x##_offsets * sizeof(x));\
      }\
      return_##x = &(ctl-> x##s [ctl->next_##x##_offset]);\
      lclmemset((void *)return_##x, 0, sizeof(x));\
      ctl->next_##x##_offset++;\
      return(return_##x);\
}

new_x(Line)
new_x(CExp)
new_x(Rule)
new_x(RuleNode)
new_x(PropDesc)
new_x(Edge)
new_x(BufInfo)
new_x(InputLine)
new_x(Scope)
new_x(Function)
new_x(CLongInt)
new_x(ExtRuleRef)
new_x(OutBufInfo)

/********************************************************************/
void do_array_allocates(PCtl *ctl, int first) {

    allocate_Lines(ctl);
    allocate_CExps(ctl);
    allocate_Rules(ctl);
    allocate_RuleNodes(ctl);
    allocate_PropDescs(ctl);
    allocate_Edges(ctl);
    allocate_BufInfos(ctl);
    allocate_InputLines(ctl);
    allocate_Scopes(ctl);
    if(first) allocate_Functions(ctl);
    allocate_CLongInts(ctl);
    allocate_ExtRuleRefs(ctl);
    allocate_OutBufInfos(ctl);
}

/********************************************************************/
/* Macro for freeing arrays of structures rooted in preprocessor ctl */
#define  free_x(x)\
static void free_##x##s(PCtl *ctl) {\
     free(ctl -> x##s);}

free_x(Line)
free_x(CExp)
free_x(Rule)
free_x(RuleNode)
free_x(PropDesc)
free_x(Edge)
free_x(BufInfo)
free_x(InputLine)
free_x(Scope)
free_x(Function)
free_x(CLongInt)
free_x(ExtRuleRef)
free_x(OutBufInfo)

/********************************************************************/
void do_array_frees(PCtl *ctl) {

    free_Lines(ctl);
    free_CExps(ctl);
    free_Rules(ctl);
    free_RuleNodes(ctl);
    free_PropDescs(ctl);
    free_Edges(ctl);
    free_BufInfos(ctl);
    free_InputLines(ctl);
    free_Scopes(ctl);
    free_CLongInts(ctl);
    free_ExtRuleRefs(ctl);
    free_OutBufInfos(ctl);
}


/************************************************************************/
void free_output_buffers(PCtl *ctl) {
    int        i;
    OutBufInfo *outbufinfo;

    for (i = 0; i < ctl->next_OutBufInfo_offset; i++) {
       outbufinfo = &(ctl->OutBufInfos[i]);
       free(outbufinfo->buffer_loc);
       outbufinfo->buffer_loc = NULL;
    }
 }

void free_input_buffers(PCtl *ctl) {
    int        i;
    BufInfo   *bufinfo;

    for (i = 0; i < ctl->next_BufInfo_offset; i++) {
       bufinfo = &(ctl->BufInfos[i]);
       if(bufinfo->buffer_loc != NULL)
            free(bufinfo->buffer_loc);
       bufinfo->buffer_loc = NULL;
    }
 }



/************************************************************************/
/*                LINE NUMBER RECORDING  AND ACCESS                     */
/************************************************************************/

static void update_input_lines(PCtl *ctl, char *buffer,
                               int  firstlinechar) { 

     InputLine *last_newline, *this_newline;
     if(buffer != ctl->current_buffer) return; 

     last_newline = &(ctl->InputLines[ctl->current_input_line]);
     /* ??*/
     if(last_newline->buffer_number > ctl->current_buffer_number || 
        (last_newline->buffer_number == ctl->current_buffer_number && 
        last_newline->begin_char >= firstlinechar )) return;  

     ctl->current_input_line = ctl->next_InputLine_offset;
                           /* high water mark */ 
     this_newline = new_InputLine(ctl); 
     this_newline->buffer_number = ctl->current_buffer_number; 
     this_newline->begin_char = firstlinechar;
}


/*****************************************************************/
/* For line numbers on errors during initial parse..              */
void get_scan_linenumber(PCtl* ctl, Pos *pos, int *lineno, int *charno) {

   int        this_number;
   InputLine *line_ent;

   /* line number must be <= current line number */ 
   this_number = ctl->current_input_line;
   *charno = -1;
 
   while(this_number > 0) { 

       line_ent = &(ctl->InputLines[this_number]);   

       if(line_ent->buffer_number == pos->bufno) {
            if(line_ent->begin_char <= pos->bufchar) break; 
            else this_number--; 
       }
       else if(line_ent->buffer_number > pos->bufno) this_number--;

       /* problem here..  new buffer in middle of line */ 
       else {
             *charno = INBUFALLOC - 2 - line_ent->begin_char
                      + pos->bufchar;
              break;
      }
   }

   assert(this_number > 0);

   /* TBD: find bug causing computed line number to be actual - 1 */
   *lineno = this_number+1;

   if(*charno < 0) *charno = pos->bufchar - line_ent->begin_char+1; 

}
       

/************************************************************************/
/*                INPUT ACCESS                                          */
/************************************************************************/

/* DO something about newline chars for windows */
/* Sort cases (for comprehensibility)           */
static int char_meets_spec(PCtl *ctl, char *next1, CharType chartype) { 

  unsigned char *next;

  next = (unsigned char *)next1;

  switch(chartype) {

     case  ANY_CHAR:
           return 1;
     case  PERIOD_CHAR:
           if(*next == '.') return 1; 
           else return 0;
     case  COMMA_CHAR: 
           if(*next == ',') return 1; 
           else return 0;
     case  F_CHAR: 
           if(*next == 'F') return 1; 
           else return 0;
     case  EXCL_CHAR:
           if(*next == '!') return 1; 
           else return 0;
     case  HASHMARK_CHAR:
           if(*next == '#') return 1; 
           else return 0;
     case  VERTICAL_BAR_CHAR:
           if(*next == '|') return 1; 
           else return 0;
     case  UNDERSCORE_CHAR:
           if(*next == '_') return 1; 
           else return 0;
     case  COLON_CHAR:
           if(*next == ':') return 1; 
           else return 0;
     case  EQ_CHAR:
           if(*next == '=') return 1; 
           else return 0;
     case  LEFT_PAREN_CHAR:
           if(*next == '(') return 1; 
           else return 0;
     case  RT_PAREN_CHAR:
           if(*next == ')') return 1; 
           else return 0;
     case  LEFT_ANGLE_CHAR: 
           if(*next == '<') return 1; 
           else return 0;
     case  RT_ANGLE_CHAR: 
           if(*next == '>') return 1; 
           else return 0;
     case  MINUS_CHAR: 
           if(*next == '-') return 1; 
           else return 0;
     case  LEFT_BRACE_CHAR:
           if(*next == '{') return 1; 
           else return 0;
     case  RT_BRACE_CHAR:
           if(*next == '}') return 1; 
           else return 0;
     case  LEFT_SQUARE_CHAR: 
           if(*next == '[') return 1; 
           else return 0;
     case  RT_SQUARE_CHAR:
           if(*next == ']') return 1; 
           else return 0;
     case  NL_CHAR:
           if(*next == NEWLINE) return 1;
           else return 0; 
     case  QUOTE_CHAR:
           if(*next == '\"') return 1;
           else return 0; 
     case  NOT_QUOTE_CHAR:
           if(*next != '\"') return 1;
           else return 0; 
     case  NOT_NL_CHAR:
           if(*next != NEWLINE) return 1;
           else return 0; 
     case  NOT_PERIOD_CHAR:
           if(*next != '.') return 1; 
           else return 0;
     case  WS_CHAR:
           if(*next == ' '|| *next == '\t'  || *next == NEWLINE) return 1;
           else return 0;
     case  NOT_WS_CHAR:
           if(!(*next == ' '|| *next == '\t'  || *next == NEWLINE)) return 1;
           else return 0;
     case  BLANK_EQUIV_CHAR:
           if(*next == ' '|| *next == '\t') return 1;
     case  DIGIT_CHAR: 
           if(isdigit(*next)) return 1;
            else return 0;
     case  ALPHA_DIG_CHAR:
           if(isdigit(*next)) return 1; /* intentional fall through */
     case  ALPHA_CHAR:
           if( (*next >= 0x41 && *next < 0x5B)  ||
              (*next >= 0x61 && *next <= 0x7A)  ||
               (*next == 0x5F /* underscore */)   ||
              (*next > 0xC0 && *next != 0xD7 && *next!= 0xF7) || 
              (*next == 0xB7) )
                return 1;
           else return 0;
     case  EOF_CHAR:
           return 0;
     default: 
           assert(0);   /* should not occur here */
 
   }
   return 0; /* should never get here */

}


/*********************************************************************/
char *get_next_input_buffer(PCtl *ctl) {

    int     read_count, char_count;
    char   *buffer;
    int    i;
    BufInfo *BufInfos, *bufinfo;

    if(ctl->reached_end_of_file) return NULL;

    buffer = (void *)malloc(INBUFALLOC);   
    char_count = 0;
    read_count = INBUFALLOC-2;

    clearerr(ctl->input_file_handle);

    char_count = fread(&buffer[0], sizeof(unsigned char), read_count,
                  ctl->input_file_handle);
    
     if (char_count < read_count) {
        ctl->reached_end_of_file = 1;

       /* make sure NL at eof */
        if(buffer[char_count -1]!= '\n')  {
             buffer[char_count] = '\n';
             char_count++; 
         }
      } 
 
    bufinfo = new_BufInfo(ctl);
    bufinfo->buffer_loc = buffer; 
    bufinfo->buffer_size = char_count;


    ctl->current_buffer = buffer;
    ctl->current_buffer_number++;   
    ctl->current_buffer_size = char_count;

     /* free all but last earlier buffers */
     BufInfos = ctl->BufInfos;
     for(i = ctl->current_buffer_number -2; i >= 0; i--) {  
          bufinfo = &(ctl->BufInfos[i]);
          free(bufinfo->buffer_loc);
          bufinfo->buffer_loc = NULL;
     }
   
     return buffer;
}
      

/*******************************************************************/
char *next_chr(PCtl *ctl, Pos *pos, CharType chartype) { 

   int      testmoddirect; 
   int   testchar;
   char    *buffer, *next;
   BufInfo *bufinfo;

  /* Character referenced by pos must be either in previous buffer, */ 
  /* current buffer, or one char past current buffer               */

   testmoddirect = 0;

   /* if in current buffer */
   if(pos->bufno == ctl->current_buffer_number && 
       pos->bufchar < ctl->current_buffer_size ) {
           buffer = ctl->current_buffer;
           testmoddirect = 1; 
   }

   else if(pos->bufno == ctl->current_buffer_number -1) {
      /* all but last buffer must be constant size */ 
      bufinfo = &(ctl->BufInfos[pos->bufno]); 
      /* within last buffer */ 
      if(pos->bufchar < bufinfo->buffer_size) { 
            buffer = bufinfo->buffer_loc;
            testmoddirect = 1;
       }
       /* begin of current buffer */
      else buffer = ctl->current_buffer;
   }

   /* otherwise must need new buffer */ 
   else  {
      assert(pos->bufno == ctl->current_buffer_number &&  
              pos->bufchar >= ctl->current_buffer_size);
               
      buffer = get_next_input_buffer(ctl); 
      if(buffer == NULL) {
           /* FIX THIS */
            if(chartype == EOF_CHAR) return ("0");
            else return (NULL);
      }  
   }

    if(testmoddirect) testchar = pos->bufchar;
    else testchar = 0;
    next =&buffer[testchar];

    /* provide for windows */
    if(buffer[testchar] == '\r') buffer[testchar] = ' ';

    if(*next == NEWLINE) update_input_lines(ctl, buffer, testchar+1);  

    if(char_meets_spec(ctl, next, chartype)) {
       if(testmoddirect) pos->bufchar++;
       else {
           pos->bufno = ctl->current_buffer_number;
           pos->bufchar = 1; 
       }
       return (next);
    }

    else return (NULL); 

}

/*******************************************************************/
char *next_non_ws(PCtl *ctl, Pos *pos, CharType chartype) {

 char *next;
 Pos   tpos; 


 while (next_chr(ctl, pos, WS_CHAR) != NULL) continue;

 lclmemcpy(&tpos, pos, sizeof(Pos));

 if( (next = next_chr(ctl, &tpos, chartype)) != NULL)  
     lclmemcpy(pos, &tpos, sizeof(Pos)); 

 return next;
}

/*******************************************************************/

void skip_ws(PCtl *ctl, Pos *pos) {
 while (next_chr(ctl, pos, WS_CHAR) != NULL) continue;
}


/*****************************************************************/ 
/*        OUTPUT STRING BUFFER HANDLING                          */
/*****************************************************************/ 
 
/*****************************************************************/ 
/* Used by literal block processing                              */  
/*fails if no space, no buffer move permitted here              */
void add_char_to_outbuf(PCtl *ctl, Pos *pos, char newchar) { 

  OutBufInfo *outbufinfo;
  
  outbufinfo = ctl->current_outbufinfo;

  if(outbufinfo->chars_used+2 >= OUTBUFALLOC) {
       do_error(ctl, pos, error_bypass_rule, 
           "Literal line too long, please split");
  }

  *(outbufinfo->next_char_ptr) = newchar;
  outbufinfo->next_char_ptr++; 
  outbufinfo->chars_used++;
}


/*****************************************************************/ 
/* Used by literal block processing                              */  
/*fails if no space, no buffer move                              */
void add_chars_to_outbuf(PCtl *ctl,Pos *pos, char *newchars) { 

  OutBufInfo *outbufinfo;
  int         newlen;
  char       *target;
  
  outbufinfo = ctl->current_outbufinfo;

  newlen = strlen(newchars);

  if(outbufinfo->chars_used +newlen +1 >= OUTBUFALLOC) {
       do_error(ctl, pos, error_bypass_rule, 
           "Literal line more than 512 chars, please split");
  }

  target = outbufinfo->next_char_ptr;
  lclmemcpy(target, newchars, newlen); 
  outbufinfo->next_char_ptr += newlen; 
  outbufinfo->chars_used += newlen;
}


/*****************************************************************/ 
char *new_outbuf(PCtl *ctl) { 

  OutBufInfo *outbufinfo;

  /* this gets next control */
  outbufinfo = new_OutBufInfo(ctl);

  ctl->current_outbufinfo = outbufinfo; 

  outbufinfo->buffer_loc = outbufinfo->next_char_ptr =
          (void *)malloc(OUTBUFALLOC);  

  outbufinfo->chars_used = 0;
  return outbufinfo->buffer_loc;
}


/*****************************************************************/ 
/* Used by literal block processing                              */  
/* create new buffer if no space for next anticipated string     */
/* also, add separating end_of_string to avoid possible problems         */
void check_outbuf_size(PCtl *ctl, int size) { 
   
  OutBufInfo *outbufinfo;

  outbufinfo = ctl->current_outbufinfo;
  
  if(outbufinfo->chars_used + size < OUTBUFALLOC) {
       add_char_to_outbuf(ctl, NULL, '\0'); 
       return;
  }

  new_outbuf(ctl);

}
  
/*****************************************************************/ 
/* create new buffer if can't accomodate full string             */ 
char *add_string_to_outbuf(PCtl *ctl, char *instring) { 

  OutBufInfo *outbufinfo;
  int         newlen;
  char       *target;
  
  outbufinfo = ctl->current_outbufinfo;

  newlen = strlen(instring);

  if(outbufinfo->chars_used +newlen >= OUTBUFALLOC) {
       new_outbuf(ctl);
       outbufinfo = ctl->current_outbufinfo;
  }

  target = outbufinfo->next_char_ptr; 
  strcpy(target, instring);
  
  outbufinfo->next_char_ptr += newlen+1; 
  outbufinfo->chars_used += newlen+1;

  return (target);

}


/*****************************************************************/
/* Temporary... just puts away string. Change to use btree       */  
/* if lookuptype "lookuptype_name                                */
char *lookup_name(PCtl *ctl, LookupType lookuptype, char *name) { 

    return (add_string_to_outbuf(ctl, name)); 

}
  

/*************************************************************************/
/*                ERROR HANDLING                                         */
/*************************************************************************/

void do_error(PCtl *ctl,  Pos *pos,
               ErrorType error_type, const char *message) {

  int    line_number, char_number;
  char  *etype;
  Pos    tpos; 

  switch (error_type) { 
     case error_warning:
          etype = "Warning";   
          break;
     case error_minor: 
     case error_bypass_rule:
     case error_bypass_function:
     case error_misc:
          etype = "Error";
          ctl->error = 1;  
          break; 
     case error_fatal:
          etype = "Severe Error"; 
          break; 
   }   

  if(pos != NULL) {
      get_scan_linenumber(ctl, pos, &line_number, &char_number);  
      fprintf(stderr, "\nLine %d char %d of file %s:\n",
                line_number, char_number, ctl->input_file_name);   
   }
   else fprintf(stderr, "\nGlobal.................\n"); 
   fprintf(stderr, "  %s:   %s\n", etype, message);    
   if(error_type == error_fatal)
       fprintf(stderr, "Parse discontinued\n");
   
  /* recovery */
   switch(error_type) {
 
       case error_bypass_rule:
       case error_bypass_function:
          /* look for . followed by nothing on rest of containing line */
          /* or end scope sequence                                     */
          while(1) {
              while(next_chr(ctl, pos, NOT_NL_CHAR)) continue;
              if(next_chr(ctl, pos, NL_CHAR) == NULL) break;  /* eof */
              if(next_chr(ctl, pos, PERIOD_CHAR)) break; 
              if(next_chr(ctl, pos, EOF_CHAR)) break; 
              
             /* look for end of scope ... four equals */
              lclmemcpy(&tpos, pos, sizeof(Pos));
              if(next_chr(ctl, pos, EQ_CHAR) == NULL) continue;
              if(next_chr(ctl, pos, EQ_CHAR) == NULL) continue;
              if(next_chr(ctl, pos, EQ_CHAR) == NULL) continue;
              if(next_chr(ctl, pos, EQ_CHAR) == NULL) continue;
              lclmemcpy(pos, &tpos, sizeof(Pos));
              break;
         } 

       default: 
           break;
  }

  switch (error_type) { 

     case error_warning:
     case error_minor: 
     case error_misc:
          break; 
     case error_bypass_rule:
          lclmemcpy(&ctl->recover_pos, pos, sizeof(Pos)); 
          longjmp(NextRuleEnv, 1); 
     case error_bypass_function:
          lclmemcpy(&ctl->recover_pos, pos, sizeof(Pos)); 
          longjmp(NextFunctionEnv, 1); 
     case error_fatal:
          exit(1);
   }
   return;
}

/*********************************************************************/
/*     HASH PROCEDURES FOR TYPENAMES                                 */
/*********************************************************************/



/* change these to ilu types */
typedef unsigned int uint32;
typedef unsigned short int uint16;

/********************************************************************/
/* Hash procedures   (for ids)                                      */
/* Start with string                                               */
/* create a word containing low-order 4 bits of first 8 characters */
/* (scrambled)                                                       */
/* (Converted from a routine using shifting to accomodate alternative*/
/* numeric representations. Results look ok in casual test but could */
/* use further checking                                              */
static uint32 squash_base_form(char * base_form) {
      uint32 i;
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
      hash_result2 =  hash_result1% (HASHSIZE-1);
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
static int make_new_structtype_entry(HCtl *hc, char *in_string) {

   int              save_index, new_alloc;
   HashEnt         *hash_begin, *new_ent;
       

     /* should not be necessary here; just structure type names */
    if(hc->next_hashchain_ent >= hc->current_hashchain_alloc) {
       new_alloc = hc->current_hashchain_alloc +INITHASHCHAINALLOC; 
       hc->hash_chains_begin = (void *)realloc(hc->hash_chains_begin,
                                 new_alloc*sizeof(HashEnt));
       hc->current_hashchain_alloc = new_alloc; 
    }
    
    save_index = hc->next_hashchain_ent;
    hash_begin = hc->hash_chains_begin; 
    new_ent = &(hash_begin[save_index]);
    assert(strlen(in_string) < 50); 
    strcpy(new_ent->structtype, in_string);
    new_ent->next = 0;
    hc->next_hashchain_ent++;
    return save_index;
}


/*searches, and makes new entry if "make new" */
int search_structtype_hash(PCtl *ctl, char *in_string, int makenew) {
 
   uint16           hash_number;
   HashEnt         *chains_begin, *chain_ent, *last_chain_ent;
   int              index, next_index;
   HCtl            *hc; 

   hc = ctl->hc; 

   /* get hash number */
   hash_number =find_lex_hash_number(in_string);

  /* search for hw match */
   index = hc->hash_table[hash_number];

   if(index == 0 && makenew) { 
       index = make_new_structtype_entry(hc, in_string); 
       hc->hash_table[hash_number] = index;
       return index;
   }

   chains_begin = hc->hash_chains_begin;

   next_index = index;
   last_chain_ent = NULL; /* to avoid possible compile warning */
   while(next_index  != 0) { 
       chain_ent = &(chains_begin[next_index]);
       last_chain_ent = chain_ent;
       if(!strcmp(in_string, chain_ent->structtype)) 
            return index;
       next_index = chain_ent->next;  
        
   }

   if(makenew) {
       index = make_new_structtype_entry(hc, in_string); 
       last_chain_ent->next = index;
       return index;
     }

    return 0;
 } 



/*********************************************************************/
/*       WRITE OUT COMPILED RULE STRUCTURES AS C FILE                */

/**********************************************************************/

/**********************************************************************/
/* Write literal block lines declaration for current grammar          */

static void write_lines(PCtl *ctl, int serial, FILE *outfile) { 
  int   i, j;
  Line *line_array, *line; 
  char *sbuf;

  line_array = ctl->Lines;

  fprintf(outfile,"\n\n/* ( format string, arg_ct, {arg_indices} )*/\n"); 

  fprintf(outfile, "\nLine Lines%d[] = {", serial); 

  fprintf(outfile, "\n/* 0*/ { NULL, 0},");

   sbuf = ctl->sbuffer;
   *sbuf = '\0';

  for (i = 1; i < ctl->next_Line_offset; i++) {

    if(sbuf + SBUFTEST >= (ctl->sbuffer + SBUFSIZE))  {
         fputs(ctl->sbuffer, outfile);
         sbuf = ctl->sbuffer;
         *sbuf = '\0';
    }
      
    line = &(line_array[i]);
    sbuf += sprintf(sbuf,"\n/*%2d*/\n", i);
    sbuf += sprintf(sbuf, "{ \"%s\", %d",
        line->format_string, line->arg_ct);
    if(line->arg_ct > 0) {
       sbuf += sprintf(sbuf, ", {");
       for(j = 0; j < line->arg_ct; j++) 
          sbuf += sprintf(sbuf, "%d,", line->arg_indices[j]);
       sbuf += sprintf(sbuf, "}");
    }

     sbuf += sprintf(sbuf, "},");
  }
      
  fputs(ctl->sbuffer, outfile);

  fprintf(outfile, "\n};\n");
}
 

/*************************************************************/
/* Write rules declaration for current grammar               */

static void write_rules(PCtl *ctl, int serial, FILE *outfile) { 
  int    i;
  Rule  *rule_array, *rule; 
  Scope *scope_array, *scope; 
  char  *ruletype, *sbuf; 

  rule_array = ctl->Rules;

  fprintf(outfile, "\n\n/*(rule_ix, scope_ix, "
 "ext_arg, argct, local_arg, line_ct, firstline, first_node, node_ct)*/\n");

  scope_array = ctl->Scopes; 
  fprintf(outfile, "\nRule Rules%d[] = {", serial); 

  /* initial null ent */
  fprintf(outfile, "\n{  0, 0,  0,   0,   0,  0,   0,   0,  0,  ruletype_R, NULL },");

   sbuf = ctl->sbuffer;
  *sbuf = '\0';

  for (i = 1; i < ctl->next_Rule_offset; i++) {

    if(sbuf + SBUFTEST >= (ctl->sbuffer + SBUFSIZE))  {
         fputs(ctl->sbuffer, outfile);
         sbuf = ctl->sbuffer;
         *sbuf = '\0';
    }
      
     rule = &(rule_array[i]);

    /* get ruletype from enum value */
    switch(rule->ruletype) {
      case ruletype_R: ruletype = "ruletype_R"; break; 
      case ruletype_L: ruletype = "ruletype_L"; break;
      default: assert(0);
    }

    /* if is first rule in scope, print demarcation comment */
    /* possibly should add optional scope names             */ 
    scope = &(scope_array[rule->scope_index]); 
    if(scope->first_rule == i) 
       sbuf += sprintf(sbuf, "\n\n/*================================="
                        " NEW SCOPE "
                        " ===================================*/\n");

    sbuf += sprintf(sbuf,"\n{ %3d, %2d, ",rule->rule_index,rule->scope_index);
    sbuf += sprintf(sbuf, "%3d, %d, %3d, ", rule->ext_arg_desc_offset, 
             rule->arg_ct, rule->local_arg_desc_offset); 
    sbuf += sprintf(sbuf,"%2d, %3d, ",rule->line_ct, rule->first_literal_line);
    sbuf += sprintf(sbuf, "%3d, %2d, ", rule->first_node, rule->node_ct); 
    if(rule->rulename != NULL)
       sbuf += sprintf(sbuf, " %s, \"%s\" ", ruletype, rule->rulename); 
    else /* generated literal rule */
       sbuf += sprintf(sbuf, " %s, \"%s\" ", ruletype, "NULL"); 
    sbuf += sprintf(sbuf, "},");
  }

  fputs(ctl->sbuffer, outfile);

  fprintf(outfile, "\n};\n");

}

/************************************************************/
/* Write scope declarations for current grammar             */

static void write_scopes(PCtl *ctl, int serial, FILE *outfile) { 
  int   i;
  Scope *scope_array, *scope; 

  scope_array = ctl->Scopes;

  fprintf(outfile, "\n\n/* ( first_rule, rule_count) */\n");

  fprintf(outfile, "\nScope Scopes%d[] = {", serial);

  /* write empty ent */
  fprintf(outfile, "\n{   0,   0 },");

  for (i = 1; i < ctl->next_Scope_offset; i++) {
    scope = &(scope_array[i]);
    fprintf(outfile,"\n{ %3d, %3d },",scope->first_rule,scope->rule_count); 
  }
  fprintf(outfile, "\n};\n");
}


/************************************************************/
/* Write rule network node declarations for current grammar */

static void write_rulenodes(PCtl *ctl, int serial, FILE *outfile) { 

  int       i;
  RuleNode *rulenode_array, *rulenode; 
  char     *sbuf;

  rulenode_array = ctl->RuleNodes;

  fprintf(outfile, "\n\n/* (rule, first_edge, final) */\n");

  fprintf(outfile, "\nRuleNode RuleNodes%d[] = {", serial);   
  fprintf(outfile, "\n/* 0*/  {   0,   0,   0 },");

  sbuf = ctl->sbuffer;
  *sbuf = '\0';

  for (i = 1; i < ctl->next_RuleNode_offset; i++) {
    if(sbuf + SBUFTEST >= (ctl->sbuffer + SBUFSIZE))  {
         fputs(ctl->sbuffer, outfile);
         sbuf = ctl->sbuffer;
         *sbuf = '\0';
    }
    rulenode = &(rulenode_array[i]);
    sbuf += sprintf(sbuf,"\n/*%2d*/ { %3d, %3d, %3d },",
        i, rulenode->rule, rulenode->first_edge, rulenode->final);
  }
  fputs(ctl->sbuffer, outfile);
  fprintf(outfile, "\n};\n");

}


/************************************************************/
/* Write rule network edge declarations for current grammar */

static void write_edges(PCtl *ctl, int serial, FILE *outfile) { 

  int   i;
  Edge *edge_array, *edge; 
  char *sbuf;

  edge_array = ctl->Edges;

  fprintf(outfile,
        "\n\n/* ( cat_ix, firstarg, argct, eqs, target, next_edge, repeat_exp) */\n");

  fprintf(outfile, "\nEdge Edges%d[] = {", serial); 

  fprintf(outfile,
          "\n/* 0*/  {  0, 0,   0,   0,   0,   0,   0, NULL },");

  sbuf = ctl->sbuffer;
  *sbuf = '\0';
  for (i = 1; i < ctl->next_Edge_offset; i++) {
    if(sbuf + SBUFTEST >= (ctl->sbuffer + SBUFSIZE))  {
         fputs(ctl->sbuffer, outfile);
         sbuf = ctl->sbuffer;
         *sbuf = '\0';
    }
    edge = &(edge_array[i]);
    sbuf += sprintf(sbuf,"\n/*%2d*/ ", i);
    sbuf += sprintf(sbuf, " { %3d, ", edge->category_index);
    sbuf += sprintf(sbuf, "%3d, %d,%3d, %3d, %3d, %3d, ", edge->first_arg_cexp,
            edge->arg_ct, edge->eqs, edge->target_node, edge->next_edge,
            edge->repeat_exp);
    sbuf += sprintf(sbuf, "\"%s\" ", edge->category_name);
    sbuf += sprintf(sbuf, "},");
  }
  fputs(ctl->sbuffer, outfile);
  fprintf(outfile, "\n};\n");

} 


/****************************************************************/
/* Write rule network edge constraint decls for current grammar */

static void write_cexps(PCtl *ctl, int serial, FILE *outfile) { 

  int   i, lit;
  CExp *cexp_array, *cexp; 
  char  *op, *sbuf;
  char   tempstring[100];

  cexp_array = ctl->CExps;

  fprintf(outfile, "\n\n/* ( op, literal, opnd1, opnd2) */\n");

  fprintf(outfile, "\nCExp CExps%d[] = {", serial); 

  fprintf(outfile, "\n/* 0*/  { op_name   , (int)0,   0,   0 },");

   sbuf = ctl->sbuffer;
  *sbuf = '\0';

  for (i = 1; i < ctl->next_CExp_offset; i++) {
     cexp = &(cexp_array[i]);

    /* get operator enum type */ 
     switch (cexp->op) {
        case op_unify:       op = "op_unify"; lit = 0; break;
        case op_eqp:         op = "op_eqp";   lit = 0; break;
        case op_neqp:        op = "op_neqp";  lit = 0; break;
        case op_and:         op = "op_and";   lit = 0; break;
        case op_or:          op = "op_or";    lit = 0; break;
        case op_name:        op = "op_name";  lit = 1; break;
        case op_path:        op = "op_path";  lit = 0; break;
        case op_fn:          op = "op_fn";    lit = 1; break;
        case op_arg:         op = "op_arg";   lit = 0; break;  
        case op_repeat:      op = "op_repeat"; lit = 1; break;  
        case op_unk:         op = "op_unk";    lit = 0; break; 
        case op_lit_int:     op = "op_lit_int";     lit = 1; break; 
        case op_lit_bool:    op = "op_lit_bool";    lit = 1; break;
        case op_lit_longint: op = "op_lit_longint"; lit = 1; break;
        case op_lit_string:  op = "op_lit_string";  lit = 1; break;
        case op_lit_null:    op = "op_lit_null";  lit = 1; break;
        default: assert(0);
     }

     if( (sbuf + SBUFTEST) >= (ctl->sbuffer + SBUFSIZE))  {
         fputs(ctl->sbuffer, outfile);
         sbuf = ctl->sbuffer;
         *sbuf = '\0';
     }
         
     sbuf += sprintf(sbuf,"\n/*%2d*/ ", i);
     sbuf += sprintf(sbuf, " { %-11s, ", op);

     /* on initial compile, ints and strings stored as   */ 
     /* string pointers, and printed, respectively, as   */
     /* char * and int.  longints are stored as strings  */
     /* and printed as offsets into longint array, which */
     /* is separately printed                           */

     if(lit == 1) { 
        switch(cexp->op) { 
           case op_lit_bool:
           case op_lit_int:
              sprintf(tempstring, "(void *) %s" ,(char *)cexp->literal); 
              break;

           case op_fn:
           case op_repeat:
              sprintf(tempstring, "(void *) %d" ,(int)cexp->literal); 
              break;

           case op_name:
           case op_lit_string:
              sprintf(tempstring, "(char *) \"%s\"",
                    (char *)cexp->literal);
              break;
           /* for longint, is integer offset into longint table */ 
           /* TBD */
           case op_lit_longint: 
              sprintf(tempstring, "(void *) %s",(char *)cexp->literal); 
               break;
           case op_lit_null:
               sprintf(tempstring, "(void *) %d", 0);
               break;
               
           default: assert(0);  /* all should be covered above */    
        }
     }
     /* "literal" entry not used */ 
     else  sprintf(tempstring, "(int) %d", 0);

     sbuf +=  sprintf(sbuf, "%-25s, %3d, %3d },",
                 tempstring,  cexp->opnd1, cexp->opnd2);
  } /* end for i */

  fputs(ctl->sbuffer, outfile);
  fprintf(outfile, "\n};\n");

}


/*****************************************************************/
/* Write cexp longint operand array for current grammar          */
/* (in preprocessing longint cexp operands stored as strings)    */
/* FIXT THIS */
static void write_longints(PCtl *ctl, int serial, FILE *outfile) { 

  int   i;
  char *clongint_array; 

  clongint_array = ctl->CLongInts;

  fprintf(outfile, "\nlong int LongInts%d[] = {", serial); 
  fprintf(outfile, "\n/* 0*/     0,");

  for (i = 1; i < ctl->next_CLongInt_offset; i++)  {
     fprintf(outfile,"\n/*%2d*/ ", i);
     fprintf(outfile, " %s,",  &clongint_array[i]); 
  }

  fprintf(outfile, "\n};\n");
}


/*********************************************************************/
/* Write category argument/parameter descriptors for current grammar */
/* (in preprocessing longint cexp operands stored as strings)        */

static void write_propdescs(PCtl *ctl, int serial, FILE *outfile) {

  int       i;
  PropDesc *propdesc_array, *propdesc; 
  char      *kind, *sbuf;
  char       tempstring[50];

  propdesc_array = ctl->PropDescs;

  fprintf(outfile,
        "\n\n/* ( propertyname, propertykind, typename, typeindex) */\n");

  fprintf(outfile, "\nPropDesc PropDescs%d[] = {", serial);   
  fprintf(outfile, "\n/* 0*/  { NULL,   PropLast, NULL, 0 },"); 

  sbuf = ctl->sbuffer;
  *sbuf = '\0';

  for (i = 1; i < ctl->next_PropDesc_offset; i++) {
    propdesc = &(propdesc_array[i]);

    /* get propertykind enum value */
     switch (propdesc->propertykind) {
        case PropString:     kind = "PropString";  break;
        case PropList:       kind = "PropList";    break;
        case PropBool:       kind = "PropBool";    break;
        case PropStruct:     kind = "PropStruct";  break;
        case PropInt:        kind = "PropInt";     break;
        case PropLongInt:    kind = "PropLongInt"; break;
        case PropOpaque:     kind = "PropOpaque"; break;
        case PropLast:       kind = "PropLast";     break;
        default: assert(0);
     }

    if(sbuf + SBUFTEST >= (ctl->sbuffer + SBUFSIZE))  {
         fputs(ctl->sbuffer, outfile);
         sbuf = ctl->sbuffer;
         *sbuf = '\0';
    }
    sbuf += sprintf(sbuf,"\n/*%2d*/ ", i);

   if(propdesc->propertyname != NULL)   /* will be from error*/
      sprintf(tempstring, "(char *) \"%s\"", propdesc->propertyname);
   else sprintf(tempstring, "NULL");  
     sbuf += sprintf(sbuf, " { %-20s, %-10s, ", tempstring, kind);

    if(propdesc->propertykind == PropStruct) { 
      if(propdesc->typename != NULL)
         sbuf += sprintf(sbuf, "\"%s\", %d },",
              propdesc->typename, propdesc->typeindex);
      else 
         sbuf += sprintf(sbuf, "\"%s\", %d },", "NULL", propdesc->typeindex);
    }
    else sbuf += sprintf(sbuf, "NULL, 0},");
 }
 fputs(ctl->sbuffer, outfile);
   
 fprintf(outfile, "\n};\n");
}


static void write_functions(PCtl *ctl, int serial, FILE *outfile) {
  int        i, j;
  Function  *function_array, *function; 
  char      *kind;
  char       tempstring[256];
  PropDesc  *args;

  function_array = ctl->Functions;

 /* first write externs */

  for (i = 1; i < ctl->next_Function_offset; i++) {

    function = &(function_array[i]);
    fprintf(outfile, "\nextern void *%s (", function->function_name);

    /* emit one void * for each arg */
    /* plus initial one for ctl */
    args = &(ctl->PropDescs[function->arg_desc_offset]);
    fprintf(outfile, "void *, void * ");
    for (j = 0; j < MAXPROPERTIES; j++) { 
         if(args[j].propertykind != PropLast)  
             fprintf(outfile, ", void *");
         else break;
    }
    fprintf(outfile, " );");

  }
  
  function = &(function_array[i]);

  fprintf(outfile, "\n\n/*(function_ix, function, rtnkind" 
                   " argoffset, argct*/\n");
  
  fprintf(outfile, "\nFunction Functions[] = {"); 

  fprintf(outfile, "\n/* 0*/  { 0, NULL, PropInt, 0,0, NULL },"); 

  for (i = 1; i < ctl->next_Function_offset; i++) {

         function = &(function_array[i]);
         fprintf(outfile,"\n/*%2d*/ ", i);
         fprintf(outfile,"{ %3d, ",function->function_index);

         /* function pointer */
         fprintf(outfile, "%s, ", function->function_name);

         switch (function->returnkind) {
            case PropString:     kind = "PropString";  break;
            case PropInt:        kind = "PropInt";    break;
            case PropOpaque:     kind = "PropOpaque";    break;
            case PropList:       kind = "PropList";    break;
            case PropStruct:       kind = "PropStruct";    break;
           default: assert(0);
         }

         fprintf(outfile, "%-10s, ", kind);
         fprintf(outfile, "%3d,", function->arg_desc_offset);  
         fprintf(outfile, "%2d,", function->arg_ct);  
         sprintf(tempstring," (char *)\"%s\"",function->function_name);
         fprintf(outfile, "%-20s },", tempstring); 
    } /* end for i */

 fprintf(outfile, "\n};\n");

}


/*********************************************************************/
void write_grammar_decls(PCtl *ctl, int first, int serial,
                         char *infile,  FILE *outfile) {

   if(first) {
     fprintf(outfile,
      "\n/* File generated by genstub/parser. Do not edit */"); 

     fprintf(outfile, "\n\n\n");
     fprintf(outfile, "/****************************************"
                       "****************************************");
     fprintf(outfile, "                        DECLS FROM FILE %s\n",
            infile);
     fprintf(outfile, "****************************************"
                      "****************************************/\n");
     fflush(outfile);

     fprintf(outfile, "\n\n#include <stddef.h>");
     fprintf(outfile, "\n#include \"rules.h\"\n");
     fprintf(outfile, "\n#include \"data.h\"\n");

     write_functions(ctl, serial, outfile);
     fflush(outfile);
   }

   write_scopes(ctl, serial, outfile);
   fflush(outfile);
   write_rules(ctl, serial, outfile);
   fflush(outfile);
   write_lines(ctl, serial, outfile);
   fflush(outfile);
   write_rulenodes(ctl, serial, outfile);
   fflush(outfile);
   write_propdescs(ctl, serial, outfile);
   fflush(outfile);
   write_edges(ctl, serial, outfile);
   fflush(outfile);
   write_cexps(ctl, serial, outfile);
   fflush(outfile);
   write_longints(ctl, serial, outfile); 
   fflush(outfile);
}


void write_summary_rule_set(FILE *outfile, char *infilenames[],
         int struct_type_count,   int count) {

   int i;

   fprintf(outfile, "\n\n\n");
   fprintf(outfile, "/****************************************"
                    "****************************************");
   fprintf(outfile, "                        RULE FILE SUMMARY\n");
   fprintf(outfile, "****************************************"
                    "****************************************/\n");
   fflush(outfile);

    
   fprintf(outfile, 
         "\nint struct_type_count = %d;\n\n", struct_type_count); 
   fprintf(outfile, "\nRuleSet RuleSets[] = {");
   for (i = 0; i < count; i++) {

      fprintf(outfile, "\n/*%2d*/ { \"%s\", \n", i, infilenames[i]);
      fprintf(outfile, "        &%s%d[0], &%s%d[0], &%s%d[0], &%s%d[0],\n",
          "Scopes", i, "Rules",     i, "Lines", i, "RuleNodes", i);
      fprintf(outfile, "        &%s%d[0], &%s%d[0], &%s%d[0] },", 
          "Edges",  i, "PropDescs", i, "CExps", i);
   }

   fprintf(outfile,
      "\n/*--*/ { NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL}");
   fprintf(outfile, "\n};\n");

}
