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

#ifndef PREPROC_H
#define PREPROC_H

#include <stddef.h>
#include <assert.h>
#include <setjmp.h> 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "rules.h"  
#include "data.h"


typedef enum {TreeNodeLeaf, TreeNodeGroup, TreeNodeDisj} TreeNodeType;  

struct TreeNode_ {
       TreeNodeType       type;
       int                offset;  /* for TreeNodeLeaf */
       int                ctns_disj; 
       struct TreeNode_  *next_sibling;
       struct TreeNode_  *first_child;
};

typedef struct TreeNode_ TreeNode;

typedef enum {GroupTypeCategory, GroupTypeConstr} GroupType;

typedef enum {cat_type_epsilon, cat_type_grammar_cat,
              cat_type_literal_block, cat_type_literal_line,
              cat_type_literal_string, cat_type_true,
              cat_type_unknown} CategoryType;

typedef struct {
             int   bufno;
             int   bufchar; /* character after last accepted */
            } Pos;

typedef enum {WS_CHAR, PERIOD_CHAR, COMMA_CHAR, EXCL_CHAR,
              VERTICAL_BAR_CHAR, ALPHA_CHAR, NL_CHAR, EQ_CHAR,
              MINUS_CHAR, LEFT_ANGLE_CHAR, RT_ANGLE_CHAR,
              QUOTE_CHAR, F_CHAR, COLON_CHAR,
              UNDERSCORE_CHAR, HASHMARK_CHAR,
              EOF_CHAR, BLANK_EQUIV_CHAR, DIGIT_CHAR, ALPHA_DIG_CHAR,
              NOT_QUOTE_CHAR, NOT_NL_CHAR, NOT_PERIOD_CHAR,
              NOT_WS_CHAR, ANY_CHAR, 
              LEFT_PAREN_CHAR, RT_PAREN_CHAR, 
              LEFT_BRACE_CHAR, RT_BRACE_CHAR, 
              LEFT_SQUARE_CHAR, RT_SQUARE_CHAR} CharType;

typedef enum {lookuptype_rulename, lookuptype_name,
             lookuptype_integer, lookuptype_opndstring} LookupType;

typedef struct {
        int  buffer_number;  
        int  begin_char;  
} InputLine;

typedef struct {
        char  *buffer_loc; 
        int    buffer_size;    
        } BufInfo; 

typedef struct {
        char  *buffer_loc;
        int    chars_used;
        char  *next_char_ptr; 
       }OutBufInfo;
         
typedef struct {
        int   edge; 
        } ExtRuleRef; 


/* provision for handling long integer operands during preprocessing*/ 
typedef char  CLongInt;


#define initial_Line_alloc         2000
#define initial_CExp_alloc         2000           
#define initial_Rule_alloc          500 
#define initial_RuleNode_alloc     3000 
#define initial_PropDesc_alloc     6000
#define initial_Edge_alloc         3000
#define initial_BufInfo_alloc        10 
#define initial_InputLine_alloc    3000 
#define initial_CLongInt_alloc      500 
#define initial_Scope_alloc          50  
#define initial_Function_alloc       50  
#define initial_ExtRuleRef_alloc    500  
#define initial_OutBufInfo_alloc     10  
#define SBUFSIZE                  40000 
#define SBUFTEST                   1000 


#define HASHSIZE 4096
#define INITHASHCHAINALLOC 100
/* indexes to this structure, converted to negatives, are stored in */
/* argument structures, for fast conversion at stub time            */
typedef struct {
          char  structtype[50];
          int   next; 
} HashEnt;
          
typedef struct {
               int        hash_table[HASHSIZE]; 
               HashEnt   *hash_chains_begin;
               int        current_hashchain_alloc;
               int        next_hashchain_ent;  
               int        next_Function_offset; 
               int        allocated_Function_offsets;
               Function  *Functions;
               } HCtl;
           

typedef struct {
               int        error;
               int        next_Line_offset;
               int        allocated_Line_offsets;
               Line      *Lines;
               int        next_CExp_offset;
               int        allocated_CExp_offsets;
               CExp      *CExps;
               int        next_Rule_offset;
               int        allocated_Rule_offsets;
               Rule      *Rules;
               int        next_RuleNode_offset;
               int        allocated_RuleNode_offsets;
               RuleNode  *RuleNodes; 
               int        next_PropDesc_offset;
               int        allocated_PropDesc_offsets;
               PropDesc  *PropDescs; 
               int        next_Edge_offset;
               int        allocated_Edge_offsets;
               Edge      *Edges;
               int        next_BufInfo_offset;
               int        allocated_BufInfo_offsets;
               BufInfo   *BufInfos;
               int        next_InputLine_offset;
               int        allocated_InputLine_offsets;
               InputLine *InputLines;   
               int        next_Scope_offset;
               int        allocated_Scope_offsets;
               Scope     *Scopes;   
               int        next_Function_offset; 
               int        allocated_Function_offsets;
               Function  *Functions;
               int        next_CLongInt_offset;
               int        allocated_CLongInt_offsets;
               CLongInt  *CLongInts;   
               int        next_ExtRuleRef_offset;
               int        allocated_ExtRuleRef_offsets;
               ExtRuleRef *ExtRuleRefs;   
                
               /* string buffer for outputs */
               char      *sbuffer;

               int        next_OutBufInfo_offset;
               int        allocated_OutBufInfo_offsets;
               OutBufInfo *OutBufInfos;   

               int        current_scope_index;
               char      *stringloc;

               char      *input_file_name;
               FILE      *input_file_handle;
               int        reached_end_of_file;
               int        current_input_line;
               int        current_buffer_number;
               int        current_buffer_size;
               char      *current_buffer; 
               OutBufInfo *current_outbufinfo;
               Pos        recover_pos;
               int        recover_rule_offset;
               int        recover_function_offset;
               HCtl      *hc;
             } PCtl;
                

typedef enum {error_warning, error_minor, error_bypass_rule,
              error_bypass_function, error_fatal, error_misc} ErrorType;


char *next_chr(PCtl *ctl, Pos *pos, CharType chartype);
char *next_non_ws(PCtl *ctl, Pos *pos, CharType chartype);
void  skip_ws(PCtl *ctl, Pos *pos);
void  do_error(PCtl *ctl, Pos *pos, ErrorType errortype,
                     const char *message);
char *get_next_input_buffer(PCtl *ctl);
void  do_allocates(PCtl *ctl);
void  write_grammar_decls(PCtl*ctl, int first, int serial, char *infile,
                                 FILE *outfile);
char *lookup_name(PCtl *ctl, LookupType lookuptype, char *name);
void get_scan_linenumber(PCtl *ctl, Pos *pos,
                          int *lineno, int *charno);
void add_char_to_outbuf(PCtl *ctl, Pos *pos, char newchar);
void add_chars_to_outbuf(PCtl *ctl, Pos *pos, char* newchars);
void check_outbuf_size(PCtl *ctl, int size);
char *new_outbuf(PCtl *ctl);

void do_array_allocates(PCtl *ctl, int first);
void do_array_frees(PCtl *ctl);
void free_input_buffers(PCtl *ctl);
void free_output_buffers(PCtl *ctl);

Line       *new_Line(PCtl *ctl);
CExp       *new_CExp(PCtl *ctl);
Rule       *new_Rule(PCtl *ctl);
RuleNode   *new_RuleNode(PCtl *ctl);
Edge       *new_Edge(PCtl *ctl);
BufInfo    *new_BufInfo(PCtl *ctl);
PropDesc   *new_PropDesc(PCtl *ctl);
Scope      *new_Scope(PCtl *ctl);
Function   *new_Function(PCtl *ctl);
ExtRuleRef *new_ExtRuleRef(PCtl *ctl);
OutBufInfo *new_OutBufInfo(PCtl *ctl);

#endif
