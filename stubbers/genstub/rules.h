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

#ifndef RULES_H_
#define RULES_H_

#include "data.h"

#define MAX_ARG_INDICES 10
#define MAX_RULE_SETS 10

typedef enum {op_unify, op_eqp, op_neqp, op_and, op_or,
                 op_name, op_path,
                 op_lit_int, op_lit_longint, op_lit_bool, op_lit_string,
                 op_lit_null,
                 op_fn, op_arg, op_repeat, op_unk} Operator;

/* assume name always char *..  Resolve on load  if bin*/
typedef struct {
       Operator  op;  /* can be literal if simple operand */
       void     *literal;  /* if op_name  or op_lit....*/ 
       int       opnd1;  /* offset in cexp table */
       int       opnd2;   /* offset in cexp table */   
 } CExp;


typedef struct { 
       int       category_index;   /* if for immediate, name null */
       int       first_arg_cexp;   /* in cexp array */
       int       arg_ct;           /* for error reporting */ 
       int       eqs;              /* offset in cexp table */
       int       target_node;      /* offset in rulenode array */
       int       next_edge;        /* next edge (in edge array) in disj */
       int       repeat_exp;       /* if repeat on edge */ 
       char     *category_name;    /* share with index?? use index postcomp*/
} Edge;

typedef struct {
        int     rule;        /* for debug.. containing rule */ 
        int     first_edge;  /* in edge array */
        int     final;
} RuleNode;
        

/* lines represented by format string containing %s for subsitution */ 
/* and list of rule argument indexes.  To be printed by  */ 
/* canned statements differentiated by arg count        */ 
typedef struct { 
       char  *format_string;
       int    arg_ct;
       char   arg_indices[MAX_ARG_INDICES]; /* need ? */
} Line;


typedef enum {ruletype_R, ruletype_L} RuleType;


/* fix parms to discard names, recall just types... common */
/* decide how rules to be indexed */
typedef struct { 
          int           rule_index;  /* this index.. in rule array */  
          int           scope_index;
          int           ext_arg_desc_offset; 
          int           arg_ct;
          int           local_arg_desc_offset; /* grammar rules only */
          int           line_ct; 
          int           first_literal_line;  /* literal rules only */
          int           first_node;  /*in rulenode array = net*/
          int           node_ct;    /* just keeps them together */
          RuleType      ruletype;
          char         *rulename;
 } Rule;

         
typedef struct { 
         int            first_rule;
         int            rule_count; 
         int            first_edge; 
         int            edge_count;
         char          *first_rule_name;
 } Scope;  

typedef struct {
         char     *in_rule_file_name; 
         Scope    *scopes;
         Rule     *rules;
         Line     *lines;  
         RuleNode *rulenodes;
         Edge     *ruleedges;
         PropDesc *propdescs;
         CExp     *cexps;
} RuleSet; 


typedef struct {
        int           function_index;
        void         *(*function_ptr)();
        PropertyKind  returnkind;
        int           arg_desc_offset; 
        int           arg_ct;   
        char*         function_name;
       } Function; 
        
#endif

