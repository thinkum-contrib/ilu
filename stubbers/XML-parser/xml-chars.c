
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
#include <ctype.h> 
#include "xml-types.h"
#include "xml-func.h"

/* hex values just for verification */
/* ANSI set contains all characters special to xml */
/* Third field indicates translation to internal char */ 
/* when used as character references                   */
/* These are translated to some unused chars, "ESCAPED" */
unsigned char specials [133][3] =  {     
         {0x0,  UNUSED_CHAR,  0x0},
         {0x1,  UNUSED_CHAR,  0x0},
         {0x2,  UNUSED_CHAR,  0x0},
         {0x3,  EOT_CHAR,     0x0},
         {0x4,  UNUSED_CHAR,  0x0},
         {0x5,  UNUSED_CHAR,  0x0},
         {0x6,  UNUSED_CHAR, 0x0},
         {0x7,  UNUSED_CHAR, 0x0},
         {0x8,  UNUSED_CHAR, 0x0},
         {0x9,  TAB_CHAR,      0x0},
          {0xA,  LINEFEED_CHAR, 0x0},
          {0xB,  UNUSED_CHAR, 0x0},
          {0xC,  UNUSED_CHAR, 0x0},
          {0xD,  RETURN_CHAR, 0x0},
          {0xE,  UNUSED_CHAR, 0x0},
          {0xF,  UNUSED_CHAR, 0x0},
          {0x10, UNUSED_CHAR,  0x0}, 
          {0x11, UNUSED_CHAR,  0x0}, 
          {0x12, UNUSED_CHAR,  0x0}, 
          {0x13, UNUSED_CHAR,  0x0},
          {0x14, UNUSED_CHAR,  0x0},
          {0x15, UNUSED_CHAR,  0x0},
          {0x16, UNUSED_CHAR,  0x0},
          {0x17, UNUSED_CHAR,  0x0},
          {0x18, UNUSED_CHAR,  0x0},
          {0x19, UNUSED_CHAR,  0x0},
          {0x1A, UNUSED_CHAR,  0x0},
          {0x1B, UNUSED_CHAR,  0x0},
          {0x1C, UNUSED_CHAR,  0x0},
          {0x1D, UNUSED_CHAR,  0x0},
          {0x1E, UNUSED_CHAR,  0x0},
          {0x1F, UNUSED_CHAR,  0x0},
          {0x20, BLANK_CHAR,        0x0}, 
          {0x21, EXCLAMATION_CHAR,  0x0}, 
          {0x22, DOUBLE_QUOTE_CHAR, 0x80}, 
          {0x23, HASHMARK_CHAR,     0x0},
          {0x24, DOLLAR_CHAR,       0x0},
          {0x25, PERCENT_CHAR,      0x0},
          {0x26, AMPERSAND_CHAR,    0x81},
          {0x27, APOSTROPHE_CHAR,   0x82},
          {0x28, LEFT_PAREN_CHAR,   0x0},
          {0x29, RIGHT_PAREN_CHAR,  0x0},
          {0x2A, STAR_CHAR,         0x0},
          {0x2B, PLUS_CHAR,         0x0},
          {0x2C, COMMA_CHAR,        0x0},
          {0x2D, MINUS_CHAR,        0x0},
          {0x2E, PERIOD_CHAR,       0x0},
          {0x2F, UP_SLASH_CHAR,     0x0},   /* / */
          {0x30, DIGIT_CHAR, 0x0},
          {0x31, DIGIT_CHAR, 0x0},
          {0x32, DIGIT_CHAR, 0x0},
          {0x33, DIGIT_CHAR, 0x0},
          {0x34, DIGIT_CHAR, 0x0},
          {0x35, DIGIT_CHAR, 0x0},
          {0x36, DIGIT_CHAR, 0x0},
          {0x37, DIGIT_CHAR, 0x0},
          {0x38, DIGIT_CHAR, 0x0},
          {0x39, DIGIT_CHAR, 0x0},
          {0x3A, COLON_CHAR, 0x0},
          {0x3B, SEMICOLON_CHAR,    0x0},
          {0x3C, LEFT_ANGLE_CHAR,   0x83},
          {0x3D, EQUAL_CHAR,        0x0},
          {0x3E, RIGHT_ANGLE_CHAR,  MAX_SPECIALS},
          {0x3F, QUESTION_CHAR,     0x0},
          {0x40, SNAIL_CHAR,        0x0},
          {0x41, LETTER_CHAR, 0x0},
          {0x42, LETTER_CHAR, 0x0},
          {0x43, LETTER_CHAR, 0x0},
          {0x44, LETTER_CHAR, 0x0},
          {0x45, LETTER_CHAR, 0x0},
          {0x46, LETTER_CHAR, 0x0},
          {0x47, LETTER_CHAR, 0x0},
          {0x48, LETTER_CHAR, 0x0},
          {0x49, LETTER_CHAR, 0x0},
          {0x4A, LETTER_CHAR, 0x0},
          {0x4B, LETTER_CHAR, 0x0},
          {0x4C, LETTER_CHAR, 0x0},
          {0x4D, LETTER_CHAR, 0x0},
          {0x4E, LETTER_CHAR, 0x0},
          {0x4F, LETTER_CHAR, 0x0},
          {0x50, LETTER_CHAR, 0x0},
          {0x51, LETTER_CHAR, 0x0},
          {0x52, LETTER_CHAR, 0x0},
          {0x53, LETTER_CHAR, 0x0},
          {0x54, LETTER_CHAR, 0x0},
          {0x55, LETTER_CHAR, 0x0},
          {0x56, LETTER_CHAR, 0x0},
          {0x57, LETTER_CHAR, 0x0},
          {0x58, LETTER_CHAR, 0x0},
          {0x59, LETTER_CHAR, 0x0},
          {0x5A, LETTER_CHAR, 0x0},
          {0x5B, LEFT_SQUARE_CHAR,  0x0},
          {0x5C, DOWN_SLASH_CHAR,   0x0},   /* \ */
          {0x5D, RIGHT_SQUARE_CHAR, 0x0},
          {0x5E, UP_CHAR,           0x0},            /* ^ */
          {0x5F, UNDERSCORE_CHAR,   0x0},    /* _ */
          {0x60, DONTKNOW_CHAR,     0x0},   /*?? */
          {0x61, LETTER_CHAR, 0x0},
          {0x62, LETTER_CHAR, 0x0},
          {0x63, LETTER_CHAR, 0x0},
          {0x64, LETTER_CHAR, 0x0},
          {0x65, LETTER_CHAR, 0x0},
          {0x66, LETTER_CHAR, 0x0},
          {0x67, LETTER_CHAR, 0x0},
          {0x68, LETTER_CHAR, 0x0},
          {0x69, LETTER_CHAR, 0x0},
          {0x6A, LETTER_CHAR, 0x0},
          {0x6B, LETTER_CHAR, 0x0},
          {0x6C, LETTER_CHAR, 0x0},
          {0x6D, LETTER_CHAR, 0x0},
          {0x6E, LETTER_CHAR, 0x0},
          {0x6F, LETTER_CHAR, 0x0},
          {0x70, LETTER_CHAR, 0x0},
          {0x71, LETTER_CHAR, 0x0},
          {0x72, LETTER_CHAR, 0x0},
          {0x73, LETTER_CHAR, 0x0},
          {0x74, LETTER_CHAR, 0x0},
          {0x75, LETTER_CHAR, 0x0},
          {0x76, LETTER_CHAR, 0x0},
          {0x77, LETTER_CHAR, 0x0},
          {0x78, LETTER_CHAR, 0x0},
          {0x79, LETTER_CHAR, 0x0},
          {0x7A, LETTER_CHAR, 0x0},
          {0x7B, LEFT_CURLY_CHAR,   0x0},   /* { */
          {0x7C, VERTICAL_BAR_CHAR, 0x0},   /* | */
          {0x7D, RIGHT_CURLY_CHAR,  0x0},   /* } */
          {0x7E, TILDE_CHAR,        0x0},   /* ~ */
          {0x7F, UNUSED_CHAR,       0x0},   /* ~ */
          {0x80,  ESCAPED_DOUBLE_QUOTE, 0x22},
          {0x81,  ESCAPED_AMPERSAND,    0x26},
          {0x82,  ESCAPED_APOSTROPHE,   0x27},
          {0x83,  ESCAPED_LEFT_ANGLE,   0x3c},
          {MAX_SPECIALS,  ESCAPED_RIGHT_ANGLE,  0x3e},
        };
       
void get_unescaped(Ctl *ctl, uchar this_char[]) { 
  uchar  *local;

  local = &this_char[0]; 
  if(ctl->is_double) {
      if( local[0] != 0) return;
      local++;
   }
  /* if it is an escaped character */
  if( (specials[*local][2] != 0)  &&
       *local >= MIN_ESCAPED_CHAR  &&
      *local  <= MAX_ESCAPED_CHAR)
     *local = specials[*local][2];
 }


void get_escaped(Ctl *ctl, uchar this_char[]) { 
  uchar  *local;

  local = &this_char[0]; 
  if(ctl->is_double) {
      if( local[0] != 0) return;
      local++;
  } 
  /* if it is a character that should be escaped rather than */
  /* an escaped character                                    */
  if( (specials[*local][2] != 0 )       
     && specials[*local][2] >= MIN_ESCAPED_CHAR)
     *local = specials[*local][2];
 }

int is_special_p(Ctl *ctl, uchar this_char[], int special) {

  uchar  *local;

  local = &this_char[0]; 
  if(ctl->is_double) {
      if( local[0] != 0) return 0;
      local++;
      } 
   if(local[0] <= MAX_SPECIALS) {
       if(specials[*local][1] == special) return 1;
    }
    return 0;
 }

int is_x(Ctl* ctl, uchar this_char[]) {

  if(ctl->is_double) {
      if( this_char[0] == 0 && this_char[1] == 'x') return 1;
      } 
  else if( this_char[1] == 'x') return 1;
  return 0;
}
 

void get_linefeed(Ctl *ctl, uchar temp_char[]) {
   
  if(!ctl->is_double) 
          temp_char[0] = 0xA;
  else {
          temp_char[0] = 0;
          temp_char[1] = 0xA;
       }
}

/* THIS JUST LOOKS FOR THINGS THAT MIGHT BE SPECIAL CHARS IN XML */
int get_specials_code(Ctl *ctl, uchar this_char[], SpecialEnum *special,
     int tr_special, int tr_quote)
 {
    uchar *local;
    SpecialEnum code;

    local = &this_char[0];
    if(ctl->is_double) { 
       if(this_char[0] != 0) return 0; 
       local++; 
    }
   
    if(local[0] <= MAX_SPECIALS) {
       code = specials[*local][1];
       if(code == LETTER_CHAR || code == DIGIT_CHAR) return 0; 
        /* retranslate escaped characters */
        /* before move to logical buffer  */
       if(tr_special &&  (*local >= MIN_ESCAPED_CHAR && 
              *local <= MAX_ESCAPED_CHAR && specials[*local][2] != 0)){ 
            if( tr_quote || (code != ESCAPED_DOUBLE_QUOTE
                             && code != ESCAPED_APOSTROPHE)) {
               *local = specials[*local][2];
                code = specials[*local][1];
            }
       }
      *special = code;
       return 1;
    }
    return 0;
 }

/* TBD THIS IS JUST APPROXIMATE PLACEHOLDER  */
/* SENSITIVE TO NON_ALPHAS ONLY for ISO-8859-1 */
 int name_begin_char(Ctl *ctl, uchar this_char[]) {
     uchar       *local;
     SpecialEnum code;
 
    local = &this_char[0];
    if(ctl->is_double) { 
       if(this_char[0] != 0) return 1; 
       local++; 
    }
   
    if(local[0] < MAX_SPECIALS) {
        code = specials[*local][1];
        if(code == LETTER_CHAR ||
           code == UNDERSCORE_CHAR ||
           code == COLON_CHAR)
       return 1; 
       else return 0; 
     }
   
    /* iso 8859-1 */
    if( local[0] > 0xC0) {
       if(local[0] != 0xD7 && local[0] != 0xF7) return 1; 
     }
    return 0;
 }

/* TBD THIS IS JUST APPROXIMATE PLACEHOLDER  */
/* SENSITIVE TO NON_ALPHAS ONLY for ISO-8859-1 */
/* AND DOESN"T DISALLOW some ISO-8859 nonalphas */
 int name_char(Ctl *ctl, uchar this_char[]) {
     uchar *local;
     SpecialEnum code;
 
    local = &this_char[0];
    if(ctl->is_double) { 
       if(this_char[0] != 0) return 1; 
       local++; 
    }
   
    if(local[0] <= MAX_SPECIALS) {
        code = specials[*local][1];
        if(code == LETTER_CHAR ||
           code == DIGIT_CHAR ||
           code == PERIOD_CHAR ||
           code == MINUS_CHAR ||
           code == UNDERSCORE_CHAR ||
           code == COLON_CHAR)
       return 1; 
       else return 0; 
     }
   
    /* iso 8859-1 */
    if(local[0] > 0xC0) {
       if(local[0] != 0xD7 && local[0] != 0xF7) return 1; 
     }
    else if(local[0] == 0xB7) return 1; 
    return 0;
 }

/* THIS ALSO IS SENSITIVE ONLY TO ISO-8859 digits */
 int digit_char(Ctl *ctl, uchar this_char[]) {  

    uchar *local;

    local = &this_char[0];
    if(ctl->is_double) { 
       if(this_char[0] != 0) return 0; 
       local++; 
    }

    if(isdigit(local[0])) return 1;
    else return 0;
 }

int signed_begin_char(Ctl *ctl, uchar this_char[]) {
     
    uchar *local;

    local = &this_char[0];
    if(ctl->is_double) { 
       if(this_char[0] != 0) return 0; 
       local++; 
    }

    if(isdigit(local[0])) return 1; 
    if( local[0] == '-' || local[0] == '+') return 1;
    else return 0;
 }
    

void append_xchar(Ctl *ctl, XString *char_group, uchar *this) {

     char_group->loc[char_group->len] = *this;
     if(ctl->is_double) char_group->loc[char_group->len +1] = *(this+1);
     char_group->len += 1 + ctl->is_double;
 }     
       

void append_character(Ctl *ctl, uchar *char_group, int *cur_len, uchar *this){
     char_group[*cur_len] = *this;
     if(ctl->is_double) char_group[(*cur_len) +1] = *(this+1);
    *cur_len += 1 + ctl->is_double;
 }     

void get_blank_surrounded(Ctl *ctl, XString *target, XString *source) { 

    int i;

    target->len = i = 0;

    target->loc = (void *)calloc(1, (source->len) + 6); 
    if(ctl->is_double)
          { target->loc[1] = 0x20 ; i = 2; }
    else 
        {  target->loc[i] = 0x20; i = 1; }

    lclmemcpy(&(target->loc[i]), &(source->loc[0]), source->len); 
    i+= source->len; 
    
    if(ctl->is_double)
        { target->loc[i+1] = 0x20;  i += 2; }
    else 
        { target->loc[i] =  0x20;   i += 1; }

    target->len = i; 

 }
                                 
void escape_contained_quotes (Ctl *ctl, XString *target, XString *source) { 
    int i;

    target->loc = (void *)calloc(1, (source->len)); 

    target->len = source->len;

    lclmemcpy(target->loc, source->loc, source->len);

    if(ctl->is_double) {
      for(i = 0; i < target->len; i= i+2) { 
         if( target->loc[i] == 0 &&   
             (target->loc[i+1] == '\'' || target->loc[i+1] == '\"' ) )
                 get_escaped(ctl, &target->loc[i]);
       }
    }

    else {
      for(i = 0; i < target->len; i++) { 
         if( (target->loc[i] == '\'' || target->loc[i] == '\"' ) )
                 get_escaped(ctl, &target->loc[i]);
       }
    }

}

       
void store_character(Ctl *ctl, uchar buf_char[], uchar *this_char) {

      buf_char[0] = this_char[0];
      if(ctl->is_double) buf_char[1] = this_char[1];
 }     


int single_char_len(Ctl *ctl, int len) {
   if(!ctl->is_double) { 
       if(len == 1) return 1;
   }
   else if(len == 2) return 1; 
   else return 0;

   return 0; /* should never get here */
}


/* ref will have trailing ';'*/ 
/* check if understanding of codepoint correct */ 
int convert_char_parameter(Ctl *ctl, uchar *ref, int is_dec, 
                          uint16 *codepoint) {

   uchar   local[100];
   uchar  *local_ref, *squash; 
   int    i, temp;

   /* if is double, squash and ensure proper parameter char values */
   squash = &local[0];
   i = 0;
  /* if 16 bit rep, squash */
  if(ctl->is_double) { 
     local_ref = ref;
     while(local_ref[i+1] != ';') {
         if(local_ref[i] != 0) return 0;
        *squash = local_ref[i+1];  
         if(is_dec) {
            if(!isdigit(*squash)) return 0;
         }
         else if(!isxdigit(*squash)) return 0;
         squash++;
         i += 2;
         if(i > 98) {
             do_phys_error(ctl, "Overlong character entity", 0);
             return 0;
         }
     }
    *squash = '\0';
   }  

   /* otherwise just ensure proper parameter char values */
   else {
     local_ref = ref;
     while(*local_ref != ';') { 
       *squash = *local_ref;
       if(is_dec) {
            if(!isdigit(*squash)) return 0;
       }
       else if(!isxdigit(*squash)) return 0;
       squash++;
       local_ref++;
       i++;
       if(i > 10) return 0;
     }  
    *squash ='\0';  
   } 

   /* now do actual convert */ 
   squash = &local[0]; 
   if(is_dec) 
        sscanf((char *)squash, "%d", &temp);  
   else sscanf((char *)squash, "%x",&temp);

   *codepoint = temp;

   if( (!ctl->is_double) && *codepoint > MAX_SINGLE_CODEPOINT) return 0;
   if(ctl->is_double && *codepoint > MAX_DOUBLE_CODEPOINT) return 0;

   return 1;

}
/*********************************************************************/
int matches_request(Ctl *ctl, uchar the_char[], 
                    CharacterRequest chartype, SpecialEnum specialreq, 
                    int polarity) { 

  int          is_eq = 0, need_second = 0, spec; 
  SpecialEnum  special;

  spec = get_specials_code(ctl, the_char, &special, 0,0);
  if(spec &&  (special== EOT_CHAR))
          return 0;
  switch(chartype) {
       case CT_Any:     
          is_eq = 1;
          break;
       case CT_NameBegin:
           is_eq = name_begin_char(ctl, the_char);
           break;
       case CT_NameChar: 
           is_eq = name_char(ctl, the_char);
           break;
       default:
         need_second = 1;
  }

  if(need_second) {
    if(get_specials_code(ctl, the_char, &special, 0, 0)) {
  
      switch(chartype) {
         case CT_Specific:
          if(specialreq == special) is_eq = 1; 
          break; 
         case CT_WS:
          if(special == BLANK_CHAR ||
             special == LINEFEED_CHAR ||
             special == RETURN_CHAR || 
             special == TAB_CHAR)  is_eq = 1;
              break;
        case CT_EndCDATA:
          if(special == AMPERSAND_CHAR || 
             special == LEFT_ANGLE_CHAR) is_eq = 1;
          break;
        case CT_Quote:
          if(special == DOUBLE_QUOTE_CHAR || 
             special == APOSTROPHE_CHAR) is_eq = 1; 
             break;
        case CT_EndQuote:
          if(special == DOUBLE_QUOTE_CHAR || 
             special == APOSTROPHE_CHAR ||
             special == AMPERSAND_CHAR ||
             special == LEFT_ANGLE_CHAR) is_eq = 1;
          break;
        case CT_RepeatOp:
          if(special == STAR_CHAR || 
             special == PLUS_CHAR ||
             special == QUESTION_CHAR) is_eq = 1;
             break; 
        case CT_EndElement:
          if(special == RIGHT_ANGLE_CHAR || 
             special == UP_SLASH_CHAR ||
             special == LEFT_ANGLE_CHAR) is_eq = 1;
             break; 

        default: break;      
      }
   }
 }

 return (is_eq == polarity);
         
} 


/***********************************************************************/
void XStringCopy(Ctl *ctl, XString *out, XString *in) {
    int i;
    for(i = 0; i < in->len; i++) out->loc[i] = in->loc[i];
}


   
/***********************************************************************/
int CharsEq(Ctl *ctl, uchar *first, uchar *next)  {
    uchar i;
    for(i = 0; i <= ctl->is_double; i++) {
        if(first[i] != next[i]) return 0;
    }
 return 1;
}
      

/***********************************************************************/
int XStringsEq (Ctl *ctl, XString *s1, XString *s2) {

     int i, len;

     if(s1->len != s2->len) return 0;
     len = s1->len; 
     for(i = 0; i < len; i++) {  
           if(s1->loc[i] != s2->loc[i]) return 0; 
      }
      return 1;
 }

/***********************************************************************/
/* like strcmp but on XStrings */
int XStringsCompare (Ctl *ctl, XString *s1, XString *s2) {
     int i, len, if_equal_value;

     if_equal_value = 0;
     len = s1->len;

     if(s1->len < s2->len) {
                len = s1->len;
                if_equal_value = -1;
               }
     else if(s1->len > s2->len) {
                len = s2->len;
                if_equal_value = 1;
               }

     for(i = 0; i < len; i++) {  
           if(s1->loc[i] < s2->loc[i])  return -1; 
           else if(s1->loc[i] > s2->loc[i]) return 1;
     } 
     
      return if_equal_value;
 }


/***********************************************************************/
int compare_with_special(Ctl *ctl,
                         uchar input[],
                         uchar compare) {


    if(ctl->is_double) {
         if(input[0] != 0x0 || input[1] != compare) return 0;
     }
    else {
         if(input[0] != compare) return 0;
    }
    return 1;
}

