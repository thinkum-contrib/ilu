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

#include "parse.h"

/* declare some functions from parse-util.c */
extern int search_structtype_hash(PCtl *ctl, char *in_string, int makenew);
extern void write_summary_rule_set(FILE *outfile, char *infilenames[],
         int struct_type_count,   int count);

/* Possibly TBD-- among others */
/* Modify grammar to state repeats on subsequences     */

jmp_buf NextRuleEnv;
jmp_buf NextFunctionEnv;


struct Arc_ {
             int            reference; 
             struct State_ *target;
             struct  Arc_   *next;
            };
typedef struct Arc_ Arc;

struct State_ {
                Arc          *first_arc;
                int           reference; 
                int           final; 
                struct State_ *next;
               };
typedef struct State_ State;

typedef struct {
                 State *states;
                 State *start_state;
               } Net;

static int parse_rule_parameters(PCtl *ctl, Pos *pos,
                                int islitrule, int islocals);
static int parse_group(PCtl *ctl, Pos *pos, Rule *rule,
                     GroupType grouptype,
                     int first, TreeNode **group_chain, int *group_disj); 
static int parse_invocation_args(PCtl *ctl, Pos *pos, Rule *rule, 
                        int *first_arg_cexp_offset, int *arg_ct);

#define MAX_ARGNAME_LEN 100
#define MAX_QUALIFIERS 6
#define MAX_INT_OPND_LEN 10
#define MAX_STG_OPND_LEN 256
#define MAX_CEXP_LIST 200

/************************************************************************/
/* (Almost) same as intepreter routine of same name                     */
static int find_property(PCtl     *ctl,
                 PropDesc  firstdesc[],
                 char     *name,
                 Ref      *out_ref) {
  int i;

  out_ref->offset = -1;
  /* (MAXPROPERTIES JUST TO AVOID ENDLESS SEARCH ON SYSTEM ERROR */
  for (i = 0;
      (i < MAXPROPERTIES) && firstdesc[i].propertykind != PropLast;
      i++) {

      if(!strcmp(firstdesc[i].propertyname, name)) {
           out_ref->desc = &(firstdesc[i]);
           out_ref->offset = i;
           return 1;
      }
  }
  return 0;
}

/**********************************************************************/
/* Build cexp from component op and operand offsets */
static void store_cexp(PCtl *ctl, Operator op, void *literal,
                int o1_offset, int o2_offset, int *cexp_offset) {

  CExp  *cexp; 

 *cexp_offset =ctl->next_CExp_offset;  
  cexp = new_CExp(ctl);

  cexp->op = op;
  cexp->literal = literal;
  cexp->opnd1 = o1_offset; 
  cexp->opnd2 = o2_offset;
}

/************************************************************************/
/*   Tree and network utilities used in Category and Expression         */ 
/*     sequence processing                                              */
/************************************************************************/


static TreeNode *new_tree_node(PCtl *ctl) {

  return ( (void *)calloc(1, sizeof(TreeNode))); 
}


static Net *new_net() {
       return( (void *)calloc(1, sizeof(Net)));
} 

static Arc *new_arc() {
       return( (void *)calloc(1, sizeof(Arc)));
}


static State *new_state(Net *net) {

    State *state, *temp_state;

    state = (void *)calloc(1, sizeof(State)); 
    if(net->states == NULL) net->states = state;
    else {
         temp_state  = net->states;
         net->states = state;
         state->next = temp_state;
    }
    return state;
}


static void free_network(Net *net) {

    State *state, *next_state;
    Arc   *arc, *next_arc; 
     
    state = net->states;
    next_arc = NULL;
    next_state = NULL;
    for( ; state; state = next_state) {
       arc = state->first_arc;
       for( ; arc; arc = next_arc) {
         next_arc = arc->next;
         free(arc);
       }
       next_state = state->next;
       free(state);
     }
}


/**************************************************************************/
/* Utility for cvt_tree_to_net                                            */
static void add_arc_to_disj_chain(PCtl *ctl, State * state,
                                   Arc *in_arc){
     Arc *arc;
    
     if(state->first_arc == NULL)  
           state->first_arc = in_arc;
     /* must add in order to ensure "first succeeds wins" semantic */ 
     else {
        arc = state->first_arc;
        while(arc->next != NULL)
               arc = arc->next;
        arc->next = in_arc;
     }
}


/************************************************************************/
/* Recursively create network from and/or tree representing             */
/* result of initial parse of either a rule rhs (leaves are categories) */
/* or of initial parse of a constraint expression (leaves are individual */ 
/* constraints)                                                         */
/* For example, given initial parse output for the rhs of a rule      */
/*  C0 --> C1 { C2 | C3 C4 } C5                                       */
/* in the form of a tree such as                                      */ 
/*           T1 -> C1 T2 C5                                           */ 
/*           T2 -> C2 | T3                                            */ 
/*           T3 ->  C3 C4                                             */
/* where the Ti's are the subtrees,                                   */  
/* the procedure creates a network with states Si and arcs Ci         */
/* S1--C1-->S2--------C2-------->S3---C5--->S6                        */ 
/*           |---C3--->S4---C4--->|                                   */
static void cvt_tree_to_net(PCtl *ctl, Pos *pos,
                     Net *net, TreeNode *group_node,
                     State* group_start_state, State* group_end_state) { 

   State    *begin_state, *next_state;
   int       ctns_disj, first, last_is_disjunction;
   Arc      *arc;
   TreeNode *child, *next;

   /*if contains disjunction, all nodes start and end at same rule node*/ 

   ctns_disj = group_node->ctns_disj;
   
   /*Note: begin rn & next_state only change if !ctns_disj */ 
   begin_state = next_state = group_start_state;
   if(ctns_disj) next_state = group_end_state;
   last_is_disjunction = 0;
   first = 1;

   for(child = group_node->first_child; child;child = child->next_sibling){

       if(!ctns_disj) {
         begin_state = next_state; 
         if(child->next_sibling == NULL) next_state = group_end_state;
         else next_state = new_state(net);
       }
      
       switch(child->type) {
          case TreeNodeDisj:
               last_is_disjunction = 1;
               break;
          case TreeNodeGroup: 
               if(ctns_disj && (!first) && !last_is_disjunction)
                   do_error(ctl, pos, error_bypass_rule,
                         "Missing disjunction character");
               last_is_disjunction = 0;
               cvt_tree_to_net(ctl, pos, net, child,begin_state, next_state);
               break; 
          case TreeNodeLeaf:
               if(ctns_disj && (!first) && !last_is_disjunction)
                   do_error(ctl, pos, error_bypass_rule,
                         "Missing disjunction character");
               last_is_disjunction = 0;
               arc = new_arc();
               arc->target = next_state;
               arc->reference = child->offset;
               assert(arc->reference != 0);
               assert(arc->target != NULL);
               add_arc_to_disj_chain(ctl, begin_state, arc);
               break;
         }
     first = 0;
   } /* end for */   
   
  /* now reclaim tree storage */  
   next = NULL;  /* to avoid undef var msg */
   for(child = group_node->first_child; child; child = next){
       next = child->next_sibling;
       free(child);
   }
}

/************************************************************************/
/* if list is singleton exp, just return it                             */
/* Otherwise construct nested expression of specified type              */
/* and return its offset                                                */ 
static int cvt_cexp_list(PCtl *ctl, Operator op,
                       int  exp_ct, int exp_array[]) {

    int   i, opnd1_offset, opnd2_offset, return_offset;

    if(exp_ct == 1) return_offset = exp_array[0];   

    else {
      /* go backwards in conj array to build nested expression */
      /* last exp created is start of conjunction              */ 
      return_offset = exp_array[exp_ct-1];
      for(i = exp_ct-2; i >= 0 ; i--) {  
          opnd2_offset = return_offset;
          opnd1_offset = exp_array[i]; 
          assert(opnd1_offset != 0 && opnd2_offset != 0);
          store_cexp(ctl, op, 0, opnd1_offset, opnd2_offset,
                  &return_offset); 
      } 
   }
   
  return return_offset;
}


/************************************************************************/
/* For each path in net, converts to a disjunction-free               */
/* cexp, and returns array of initial offsets                         */
/* General approach: accumulate paths one by one.  as each completes  */ 
/* emit new top-level cexp, and then backtrack to get next path, etc. */ 
static void cvt_net_to_disj_list (PCtl *ctl, State *state,
               int *disj_ct, int disj_array [],
               int conj_ct,  int conj_array[]) {
   Arc   *arc;
   State *target_state;
   int    new_offset;
   
   
   if(state->final) {
         new_offset =  cvt_cexp_list(ctl, op_and,
                                     conj_ct, conj_array);
         disj_array[*disj_ct] = new_offset;
         assert(*disj_ct < MAX_CEXP_LIST);
         (*disj_ct)++;
   }
   for (arc = state->first_arc; arc; arc = arc->next) {
         assert(conj_ct < MAX_CEXP_LIST);
         conj_array[conj_ct] = arc->reference;
         target_state = arc->target;
         cvt_net_to_disj_list (ctl, target_state,
                          disj_ct, disj_array,
                          conj_ct+1, conj_array);
   }
    return;
}

/***********************************************************************/
/* Utility for cvt_net_to_category_net                                 */
static void add_edge_to_disj_chain(PCtl *ctl, RuleNode *rulenode,
                                   int edge_offset){
     Edge     *edge;

     if(rulenode->first_edge == 0)
           rulenode->first_edge = edge_offset;
     else {
        edge = &(ctl->Edges[rulenode->first_edge]);
        while(edge->next_edge != 0)
               edge = &(ctl->Edges[edge->next_edge]);
        edge->next_edge = edge_offset;
     }
}
                            

/***********************************************************************/
/* RE-express net in terms of rule nodes and edges                     */ 
/* and return index in RuleNodes of initial node                       */

static int cvt_net_to_category_net (PCtl *ctl, Net *net) {              

    RuleNode *rulenode; 
    State    *state; 
    Arc      *arc;
    Edge     *edge;
    int       first_rulenode;;

    for(state = net->states; state; state = state->next) {
        state->reference = ctl->next_RuleNode_offset;
        rulenode =  new_RuleNode(ctl);
        if(state->final)  rulenode->final = 1;
    }
    first_rulenode = net->start_state->reference;
    for(state = net->states; state; state = state->next) {
         rulenode = &(ctl->RuleNodes[state->reference]);
         for(arc = state->first_arc; arc; arc = arc->next) {
             edge =&(ctl->Edges[arc->reference]);
             edge->target_node = arc->target->reference;
             add_edge_to_disj_chain(ctl, rulenode, arc->reference);
          }
     }
                        
     free_network(net);
     return (first_rulenode);
} 


/* END  Tree and network utilities                                   */  
/************************************************************************/

/************************************************************************/
/* Parse single line of literal (L_) rule block                         */
/* or quoted string part of immediate literal rule (L_ or LP)           */
/* Create line structures containing format strings for printing line,  */
/* in which parameter references %xxxxx% are replaced by %s...          */
/* For rule blocks, within lines can escape % and .(at line begin) by   */
/* dummy parms  %percent% %dot%                                         */
/* For immediate literal rules can escape " by %quote%                  */
/* FOr immediate, do no use next_chr                                   */ 

static int parse_literal_rule_line(PCtl *ctl, Pos *pos, int immed,
                  CategoryType cattype /* only for immed */ ,
                  PropDesc *argdesc, int *line) {

   int       line_arg_ct, name_len, in_ref, pending_dot, thislineindex;
   Pos       tpos;
   Line       *thisline; 
   char        subs_name[MAX_ARGNAME_LEN];
   char       *next; 
   char        msg[256]; 
   Ref         ref; 
   OutBufInfo *outbufinfo;

   line_arg_ct = name_len = in_ref = pending_dot = 0;

   lclmemcpy(&tpos, pos, sizeof(Pos));

   if(immed) {
       if(next_non_ws(ctl, &tpos, QUOTE_CHAR) == NULL) 
        do_error(ctl, &tpos, error_bypass_rule,
           "Missing quoted string or other error in literal rule line.");
    }   
       
   /* treat any initial . as block end.. should be escaped */
   /* fix pending dot */ 
   if( (!immed) &&  next_chr(ctl, &tpos, PERIOD_CHAR)) { 
       lclmemcpy(pos, &tpos, sizeof(Pos));
       return 0;
   }

   /* reallocate Lines if necessary */
 *line = thislineindex = ctl->next_Line_offset;
  thisline = new_Line(ctl);

   check_outbuf_size(ctl, 512);  /*allow for max line, inc alloc new buffer*/
   outbufinfo = ctl->current_outbufinfo;
   thisline->format_string = outbufinfo->next_char_ptr;

   if(pending_dot) add_char_to_outbuf(ctl, &tpos, '.');

   while( (immed && (next = next_chr(ctl, &tpos, NOT_QUOTE_CHAR)) ) || 
           (!immed && (next = next_chr(ctl, &tpos, NOT_NL_CHAR)))  ) {

      if(*next == '%') { 
        if(in_ref) {
            subs_name[name_len] = '\0'; 
            in_ref = name_len = 0;

            /* if escaped symbol, substitute symbol */
            if(!strcmp(subs_name, "percent"))
                   add_char_to_outbuf(ctl, &tpos, '%');

            else if(!strcmp(subs_name, "dot"))
                   add_char_to_outbuf(ctl,&tpos, '.');

            else if(!strcmp(subs_name, "quote"))
                    add_char_to_outbuf(ctl, &tpos, '\"');

            /* if in arg list, add index to arg index array */
            else if(find_property(ctl, argdesc, &subs_name[0], &ref)) {
               thisline->arg_indices[thisline->arg_ct] = ref.offset;
               (thisline->arg_ct)++;
               add_chars_to_outbuf(ctl, &tpos,  "%s");
            }  

            /* otherwise issue warning that not in parms and treat as stg*/
             else { 
               sprintf(msg, "Substitution name %s not parameter: "
                            " assuming not substitution", &subs_name[0]);
               do_error(ctl, &tpos, error_misc, msg);
               add_chars_to_outbuf(ctl, &tpos, subs_name); 
               add_char_to_outbuf(ctl, &tpos,  *next);
            }
         } /* end if in_ref*/

       /* beginning of new ref */
       else   in_ref = 1; 
    } /* end if *next = '%' */

    /* next not % */
    else {

       if(in_ref) {
           /* if not too long, add to accumulating ref name */
           if(name_len < MAX_ARGNAME_LEN -1) {
              subs_name[name_len] = *next; 
              name_len++;
           }

          /* otherwise assume not really ref, and print warning */
          else {
            do_error(ctl, &tpos, error_misc, 
               "Substitution name too long, assuming not substitution");
            subs_name[name_len] = '\0'; 
            add_chars_to_outbuf(ctl, &tpos, subs_name); 
            add_char_to_outbuf(ctl, &tpos, *next);
         }

      }

      /* not in ref */
      else  add_char_to_outbuf(ctl,&tpos, *next);

    } /* end next not %s */

  } /* end while not NL or ENDQUOTE (for immed) */

  /* now end of line */
  if(in_ref) {
      do_error(ctl, &tpos, error_warning, 
            "substitution name not ended at NL, assuming not substitution");
      subs_name[name_len] = '\0'; 
      add_chars_to_outbuf(ctl, &tpos, subs_name); 
  }

  /* skip past end of literal */
  next = next_chr(ctl, &tpos, ANY_CHAR);
  /* if not immed, will be newline */
  if( (!immed) || cattype == cat_type_literal_line)  {
   add_char_to_outbuf(ctl, &tpos,'\\'); 
   add_char_to_outbuf(ctl, &tpos,'n'); 
  }
  add_char_to_outbuf(ctl, &tpos, '\0');

  lclmemcpy(pos, &tpos, sizeof(Pos));
  return 1;
}


/*************************************************************************/
static int parse_arrow(PCtl *ctl, Pos *pos) {
  Pos tpos;
  lclmemcpy(&tpos, pos, sizeof(Pos)); 
  if(!next_non_ws(ctl, &tpos, MINUS_CHAR)) return 0; 
  if(!next_chr(ctl, &tpos, MINUS_CHAR)) return 0;
  if(!next_chr(ctl, &tpos, RT_ANGLE_CHAR)) return 0;

  lclmemcpy(pos, &tpos, sizeof(Pos)); 
  return 1;
}

/*************************************************************************/
/*Parse rule of form L_xxxx -->                                          */ 
/* TBD add something to sense ==== in literal block and possible problem */
/* and recovery position                                                 */
static int parse_literal_rule(PCtl *ctl, Pos *pos, Rule *rule) {

   Pos           tpos;
   int           line, firstline, i;
   PropDesc     *argdescs;
   char         *next; 

   lclmemcpy(&tpos, pos, sizeof(Pos));

   rule->ruletype = ruletype_L;

   rule->ext_arg_desc_offset = ctl->next_PropDesc_offset;

   argdescs = &(ctl->PropDescs[rule->ext_arg_desc_offset]); 

   if(!parse_rule_parameters(ctl, &tpos, 1, 0)) {
       /* print message and recover */
       return 0;
   }

   rule->arg_ct = ctl->next_PropDesc_offset - rule->ext_arg_desc_offset -1;

   if(!parse_arrow(ctl, &tpos))   
     do_error(ctl, &tpos, error_bypass_rule, "Rule must have -->");

   /* rest of line must be blank */
   while(next_chr(ctl, &tpos, BLANK_EQUIV_CHAR)) continue;
   next = next_chr(ctl, &tpos, NL_CHAR);
   if(next == NULL) 
       do_error(ctl, &tpos, error_bypass_rule, 
           "In literal rule, characters after --> must be blank");
   
   /* should return 0 when reads . followed by blank*/
   firstline = 1;
   i = 0;

   while(parse_literal_rule_line(ctl, &tpos, 0, 0, argdescs ,&line)) {
      if(firstline) {
         rule->first_literal_line = line;
         firstline = 0; 
      }  
      if(i > 200) 
          do_error(ctl, pos, error_bypass_rule, 
            "In literal rule, more than 200 lines. "
            "Probably problem with quoted string or missing final '.'");
      i++;
  }

  rule->line_ct = i;

  lclmemcpy(pos, &tpos, sizeof(Pos));
  return 1; 
   
}


/*******************************************************************/ 
static int parse_simple_name(PCtl *ctl, Pos *pos,
               LookupType lookuptype, char **name) {
    
   char  tempname[256]; 
   int   namelen = 0;
   Pos   tpos;
   char *next;

   lclmemcpy(&tpos, pos, sizeof(Pos));

   next = next_chr(ctl, &tpos, ALPHA_CHAR);
   if(next == NULL) return 0;
   tempname[0] = *next; 
   namelen = 1;

   while(1) {
      next = next_chr(ctl, &tpos, ALPHA_DIG_CHAR);
      if(next == NULL) break; 
      tempname[namelen] = *next;
      namelen++;
   }

   if(namelen ==0) return 0; 

   tempname[namelen] = '\0';
  
   /* for lookuptypename, use btree. will be lots of repeats */
   *name = lookup_name(ctl, lookuptype, &tempname[0]);

   lclmemcpy(pos, &tpos, sizeof(Pos));
   return 1;
}
    

/***************************************************************/
static int parse_category_name(PCtl *ctl, Pos *pos, char **out_name,
                              CategoryType *category_type) {

   int   namelen;
   Pos   tpos;
   unsigned char *name;

   lclmemcpy(&tpos, pos, sizeof(Pos));
  *category_type = cat_type_unknown;

   if(!parse_simple_name(ctl, &tpos,lookuptype_rulename, out_name))
        return 0;
    
   lclmemcpy(pos, &tpos, sizeof(Pos));

   name = (unsigned char*)*out_name;
   namelen = strlen(*out_name);

   if( (name[0] == 'R') && (name[1] == '_'))
         *category_type = cat_type_grammar_cat;

   else if( (name[0] == 'E') && (name[1] == '_'))  
         *category_type = cat_type_epsilon;
  
   else if( (name[0] == 'L') && (name[1] == '_')) {
         if(strlen( (char*)&name[0]) == 2)
            *category_type = cat_type_literal_line;
         else *category_type = cat_type_literal_block;
   }
 
   else if( (name[0] == 'L') && (name[1] == 'P') && (name[2] == '_')
           && strlen((char *)&name[0]) == 3)
         *category_type = cat_type_literal_string;
  
   else if(!strcmp((char *)&name[0], "TRUE"))  
         *category_type = cat_type_true;
         
   else return 0; 
 
   return 1;

}
    
/***************************************************************************/
/* parse single parameter decl in parameter list                           */
static int parse_parm_decl(PCtl *ctl, Pos *pos, int islitrule) {
  
  char     *parmname,  *parmtype;
  Pos       tpos;
  PropDesc *pdesc;

  lclmemcpy(&tpos, pos, sizeof(Pos));

  skip_ws(ctl, &tpos); 
  if(!parse_simple_name(ctl, &tpos, lookuptype_name, &parmtype))
          return 0; 

  skip_ws(ctl, &tpos); 

  /* if no second parm decl component, assume type is String */
  if(!parse_simple_name(ctl, &tpos,lookuptype_name, &parmname)) { 
         parmname = parmtype;
         parmtype = ctl->stringloc;  /* contains "String"*/
   }

   /* set up property descriptor */ 
   pdesc = new_PropDesc(ctl);

   pdesc->propertyname = parmname;
   if(!strcmp(parmtype, "String"))  pdesc->propertykind = PropString;
   else if(!strcmp(parmtype, "List"))    pdesc->propertykind = PropList;
   else if(!strcmp(parmtype, "Bool"))    pdesc->propertykind = PropBool;
   else if(!strcmp(parmtype, "Int")) pdesc->propertykind = PropInt;
   else if (!strcmp(parmtype, "LongInt")) pdesc->propertykind = PropLongInt;
   else if (!strcmp(parmtype, "Opaque")) pdesc->propertykind = PropOpaque;
   else {
       pdesc->propertykind = PropStruct;
       pdesc->typename = parmtype;
       /* negative because not yet resolved.. currently at stubtime*/
       pdesc->typeindex = -(search_structtype_hash(ctl, parmtype, 1));
   }

   if(islitrule) {
     if(pdesc->propertykind != PropString &&
        pdesc->propertykind != PropInt) {
           do_error(ctl, &tpos, error_minor, 
            "Literal rule arg must be String (can be implicit) or Int");
        return 1;
     }
   }

   lclmemcpy(pos, &tpos, sizeof(Pos)); 
   return 1;

 }
   

/********************************************************************/
/* Convert the arguments of an L_() or LP_() reference to           */
/* a parameter descriptor list for the generated rule               */  
/* TBD? compile-time check for string types?  more general problem? */
static int build_immediate_parm_descriptor(PCtl *ctl, Rule *litrule,
               Pos *pos, int arg_cexp_offset, PropDesc **argdesc) { 
   int       arg_offset;
   CExp     *arg_cexp, *arg_cexp1, *arg_cexp2;
   PropDesc *pdesc, *first_pdesc;

   first_pdesc = &(ctl->PropDescs[ctl->next_PropDesc_offset]);
   arg_offset = arg_cexp_offset;
   while (arg_offset != 0) { 
        arg_cexp = &(ctl->CExps[arg_offset]);

       pdesc= new_PropDesc(ctl);
       pdesc->propertykind = PropString;

       /* args to immediate literal must be simple names */
       if(arg_cexp->op == op_arg) { 
            /* arg itself */
            arg_cexp1 = &(ctl->CExps[arg_cexp->opnd1]); 
            /* arg is local or ext  only, not function or string*/ 
            if(arg_cexp1->op == op_path) {  
                arg_cexp2 =  &(ctl->CExps[arg_cexp1->opnd2]); 
                if(arg_cexp2->op == op_name) 
                    pdesc->propertyname = (char *)arg_cexp2->literal;
             }  
       }
       if(pdesc->propertyname == NULL)  
              do_error(ctl, pos, error_bypass_rule, 
               "Argument to literal rule string must be simple name");

        arg_offset = arg_cexp->opnd2;
   }     

   /* set up last (or only) descriptor */
   pdesc = new_PropDesc(ctl);
   pdesc->propertyname = "Last";
   pdesc->propertykind = PropLast;

   *argdesc = first_pdesc;

   return 1;
}

/***************************************************************************/
/* parse parameter list of form ( (type) name )                            */
static int parse_rule_parameters(PCtl *ctl, Pos *pos,
                                int islitrule, int islocals) {

  Pos       tpos;
  PropDesc *pdesc;
  int       first, no_parms;
  char     *next;
  
  lclmemcpy(&tpos, pos, sizeof(Pos));

  first = 1; 
  no_parms = 0;
  next = next_non_ws(ctl, &tpos, LEFT_PAREN_CHAR);
  if(next == NULL) {
      no_parms = 1; 
      if(!islocals)   do_error(ctl, &tpos, error_warning, 
                     "Rule should have empty parameter list");
  }  

  if(! no_parms) {
     while(1) {  
        if(!parse_parm_decl(ctl, &tpos, islitrule)) {
            if(first) break;  /* no parameters */
            else {
               do_error(ctl, &tpos, error_bypass_rule, 
                 "Parameter list");
           }
        }
        first = 0; 

        if(next_non_ws(ctl, &tpos, COMMA_CHAR)) continue;
        break;
     }
      
     if(next_non_ws(ctl, &tpos, RT_PAREN_CHAR) == NULL) {
         do_error(ctl, &tpos, error_bypass_rule, 
                 "Parameter list: possibly missing right paren");
      }
   }

   /* set up last desc */
   pdesc = new_PropDesc(ctl);
   pdesc->propertykind = PropLast;
   pdesc->propertyname = "Last";

   lclmemcpy(pos, &tpos, sizeof(Pos));

   return 1;
}

/*********************************************************************/
/* Parse data reference in op_path/op_name cexps                     */
/* example results (schmeatic)                                       */
/*  1 name   ( "a")                                                  */     
/*    (oppath                                                        */
/*       (opname arg|local)                                          */  
/*       (opname name))                                              */  
/*  2 names ("a.b")                                                  */  
/*    (oppath                                                        */
/*       (opname arg|local)                                          */  
/*       (oppath                                                     */  
/*          (opname name)                                            */  
/*          (opname name)                                            */  
/*  3 names ("a.b.c")                                                */  
/*    (oppath                                                        */
/*       (opname arg|local)                                          */  
/*       (oppath                                                     */  
/*          (opname name)                                            */  
/*          (oppath                                                  */  
/*             (opname name)                                         */  
/*             (opname name)                                         */  

static int parse_data_reference(PCtl *ctl, Pos *pos,
            Rule *rule,  int *return_offset)  {
    
   int       opnd1_offset, opnd2_offset, next_opnd2_offset, i, qual_ct; 
   char     *name[MAX_QUALIFIERS];
   PropDesc *pdesc;
   Ref        dummyref;
   char       msg[256];
     
   if(!parse_simple_name(ctl, pos, lookuptype_name, &name[1])) return 0; 
 
   if(!strcmp(name[1], "NULL")) { 
           store_cexp(ctl, op_lit_null, 0, 0, 0, return_offset);
           return 1;
   }

   /* see if name is arg or local */
   pdesc = &(ctl->PropDescs[rule->ext_arg_desc_offset]);
   if(find_property(ctl, pdesc, name[1], &dummyref)) 
        name[0] = "arg"; 
   else {
       pdesc = &(ctl->PropDescs[rule->local_arg_desc_offset]);
       if(find_property(ctl, pdesc, name[1], &dummyref))
               name[0] = "local"; 
       else{
          sprintf(msg, "Qualified name %s neither arg nor local data",
                         name[1]); 
          do_error(ctl, pos, error_bypass_rule, msg);
       }
   }
 
   for (i=2; ;i++ ){ 
     if(next_chr(ctl, pos, PERIOD_CHAR)==NULL) break; 
     if(!parse_simple_name(ctl, pos, lookuptype_name, &name[i])) 
         do_error(ctl, pos, error_bypass_rule, "Error in qualified name"); 
   }
           
   qual_ct = i;

   /* store last qualifier */ 
   /* possibly change organization so more like arg list? */
   store_cexp(ctl, op_name, name[qual_ct-1], 0, 0, &next_opnd2_offset); 

   /* process list backwards to get cexp */
   for (i = qual_ct -2 ; i >= 0; i--) {
     opnd2_offset = next_opnd2_offset; 
     store_cexp(ctl, op_name, (char *)name[i], 0, 0, &opnd1_offset ); 
     store_cexp(ctl, op_path, NULL,
                opnd1_offset, opnd2_offset, &next_opnd2_offset);  
   }

   *return_offset = next_opnd2_offset;

   return(1);
}


/*********************************************************************/
int parse_integer_constant(PCtl *ctl, Pos *pos, int *return_offset) {
    
    int    i;
    char   the_integer[MAX_INT_OPND_LEN];
    char  *stringloc, *next; 
    char   message[256];
  
    if( (next =next_non_ws(ctl, pos,DIGIT_CHAR)) == NULL) return 0; 

    the_integer[0] = *next; 

    for(i = 1; i< MAX_INT_OPND_LEN ; i++) { 
        if( (next= next_chr(ctl, pos,DIGIT_CHAR)) == NULL) break; 
        the_integer[i] = *next; 
    }       

    if(i >= MAX_INT_OPND_LEN) {
      sprintf(&message[0],"Integer operand greater than %d digits",
            MAX_INT_OPND_LEN); 
      do_error(ctl, pos, error_bypass_rule, message);
    }
     
    the_integer[i] = '\0'; 
    stringloc = lookup_name(ctl, lookuptype_integer, &the_integer[0]);

    /*TBD: add check for longint and if so, modify op */

    store_cexp(ctl, op_lit_int, (void *)stringloc, 0, 0, return_offset); 
    return(1);
 }
 
/******************************************************************/
/* in string constant, temporarily use \ to escape quote */
int parse_string_constant(PCtl *ctl, Pos *pos, int *return_offset) {
    
    int    i, pending_escape;
    char   the_string[MAX_STG_OPND_LEN];
    char  *stringloc, *next; 
    char   message[256];
  
    if(next_non_ws(ctl, pos,QUOTE_CHAR) == NULL) return 0; 

    pending_escape = 0;
    for(i = 0; i< MAX_STG_OPND_LEN; i++) { 
        if( (next= next_chr(ctl, pos, NOT_QUOTE_CHAR))!= NULL) {  
           if(*next == '\\') pending_escape = 1; 
           else pending_escape = 0;
           the_string[i] = *next; 
           }
        else if (pending_escape) {
              pending_escape = 0;
              next = next_chr(ctl, pos, QUOTE_CHAR);  
              the_string[i] = *next;
          }
        else {
              next_chr(ctl, pos, QUOTE_CHAR);  
              break;
       }
    }       

    if(i >= MAX_STG_OPND_LEN) {
      sprintf(&message[0],"String operand greater than %d chars",
                MAX_STG_OPND_LEN); 
      do_error(ctl, pos, error_bypass_rule, message);
    }
     
    the_string[i] = '\0'; 
    stringloc = lookup_name(ctl, lookuptype_opndstring, &the_string[0]);

    store_cexp(ctl, op_lit_string, (void *)stringloc, 0, 0, return_offset); 
    return(1);
 }

/*************************************************************************/
/* cexp for function ref is (op_fn literal_index (op_arg arg1 ...))  */
static int parse_fn_ref(PCtl* ctl, Pos *pos, Rule *rule, int *return_offset) {

    int       i, fn_index, arg_ct;
    Pos       tpos;
    int      is_fn, first_arg_offset;
    char     *fn_name;
    Function *function_array,*function;

    lclmemcpy(&tpos, pos, sizeof(Pos));
    is_fn = next_non_ws(ctl, &tpos, F_CHAR) &&
            next_chr(ctl, &tpos,UNDERSCORE_CHAR);

    if(!is_fn) return 0;

    lclmemcpy(&tpos, pos, sizeof(Pos)); 

    if(!parse_simple_name(ctl, &tpos, lookuptype_name, &fn_name))  
        do_error(ctl, &tpos, error_bypass_rule, "Error in function_name"); 

    /* find function index -- must be declared before reference */
    fn_index = 0;
    function_array = ctl->Functions;
    for (i = 1; i < ctl->next_Function_offset; i++) {
       function = &(function_array[i]);
       if(!strcmp(fn_name, &function->function_name[0])) {
             fn_index= i;
             break;
       }
    }
    if(fn_index == 0)  
       do_error(ctl, &tpos, error_misc, "Referenced function not declared."); 
       
    /* if no args, first_arg_offset is 0 */
    if(!parse_invocation_args(ctl, &tpos, rule,
        &first_arg_offset, &arg_ct))  
        do_error(ctl, &tpos, error_bypass_rule, "Error in function_args"); 
     if(arg_ct != function->arg_ct)
       do_error(ctl, &tpos, error_bypass_rule,
          "Function reference arg count does not match function decl");

    store_cexp(ctl, op_fn, (void *)fn_index, 
               first_arg_offset, 0, return_offset); 
    lclmemcpy(pos, &tpos, sizeof(Pos));

    return 1;

}
  
     
/******************************************************************/
static int parse_opnd(PCtl *ctl, Pos *pos, Rule *rule, int *opnd_offset) {

   skip_ws(ctl, pos);

   if(parse_string_constant(ctl, pos, opnd_offset)) return 1;

   else if(parse_integer_constant(ctl, pos, opnd_offset)) return 1;

   else if(parse_fn_ref(ctl, pos, rule, opnd_offset)) return 1;

   /* also checks for explicit "NULL" */
   else if(parse_data_reference(ctl, pos, rule, opnd_offset)) return 1;

   else return 0;  
}


/*****************************************************************/
/* Parse args into cexp struct of form                           */
/* op_arg                                                        */ 
/*    arg1_cexp                                                  */
/*      op_arg | 0                                               */ 
/*         arg2_cexp                                             */
/*         op_arg | 0                                            */ 
/*          etc.                                                 */  
/* return offset is 0 if no args.                                */
static int parse_invocation_args(PCtl *ctl, Pos *pos, Rule *rule, 
                        int *first_arg_cexp_offset, int *arg_ct) {

    
   int  opnd2_offset, next_opnd2_offset, i, qual_ct; 
   int  args[MAXFUNARGS];
  

   *first_arg_cexp_offset = 0;
   if(next_non_ws(ctl, pos,LEFT_PAREN_CHAR) == NULL) return 0; 

   /* if non-null arg list */
   if(parse_opnd(ctl, pos, rule, &args[0])) {
      for( i=1 ; ; i++ ){ 
         if(next_non_ws(ctl, pos, COMMA_CHAR) == NULL) break; 
         if(!parse_opnd(ctl, pos, rule, &args[i])) 
         do_error(ctl, pos, error_bypass_rule, "Error in argument list"); 
      }
       qual_ct = i;
   }
   else qual_ct = 0;

   if(next_non_ws(ctl, pos,RT_PAREN_CHAR) == NULL)  
       do_error(ctl, pos, error_bypass_rule,
                "Error in argument or missing ) in argument list"); 

   /* store last qualifier */ 
   next_opnd2_offset = 0;

   /* process list backwards to get cexp */
   for (i = qual_ct - 1; i >= 0; i--) {
     opnd2_offset = next_opnd2_offset; 
     store_cexp(ctl, op_arg, NULL, args[i], opnd2_offset, &next_opnd2_offset); 
   }

   *first_arg_cexp_offset = next_opnd2_offset;
   *arg_ct = qual_ct;

   return 1;

}


/************************************************************************/
/*Parse phrase of form "opnd1 =|!=|== opnd2" into cexp form             */

static int parse_eq_phrase(PCtl *ctl, Pos *pos, Rule *rule, int *cexp_offset) {

 Pos       tpos;
 int       offset1, offset2;
 Operator  op_q;

  lclmemcpy(&tpos, pos, sizeof(Pos));

  /* need rule args info */
  if(!parse_opnd(ctl, &tpos, rule, &offset1)) 
      do_error(ctl, &tpos, error_bypass_rule, 
                 "First operand of constraint expression");

  op_q = op_unk;

  if(next_non_ws(ctl, &tpos, EQ_CHAR)) {  
     if(next_non_ws(ctl, &tpos, EQ_CHAR)) {
         op_q = op_eqp; 
     }
     else op_q = op_unify; 
  }
  else if(next_non_ws(ctl, &tpos, EXCL_CHAR) &&
          next_non_ws(ctl, &tpos, EQ_CHAR) )  
          op_q = op_neqp;

  else  do_error(ctl, &tpos, error_bypass_rule, 
                 "Constraint expression operator");

   
  if(!parse_opnd(ctl, &tpos, rule, &offset2)) 
      do_error(ctl, &tpos, error_bypass_rule, 
                 "Second operand of constraint expression");

   store_cexp(ctl, op_q, 0, offset1, offset2, cexp_offset); 

   lclmemcpy(pos, &tpos, sizeof(Pos)); 

   return 1;
}


/*************************************************************************/
int parse_repeat (PCtl *ctl, Pos *pos, Rule *rule, int *repeat_offset) {

  char *next, *in;
  int  opnd1_offset, opnd2_offset, lit_offset;
  CExp *cexp1, *cexp2, *cexp3;

  next = next_non_ws(ctl, pos, COLON_CHAR); 
  if(next == NULL) return 0;

  if(!parse_opnd(ctl, pos, rule, &opnd1_offset)) 
    do_error(ctl, pos, error_bypass_rule, "Repeat expression");

   /* ensure name is simple local */
    cexp1 = &(ctl->CExps[opnd1_offset]);
    cexp2 = &(ctl->CExps[cexp1->opnd1]); 
    cexp3 = &(ctl->CExps[cexp1->opnd2]); 

    if(cexp2->op != op_name) assert(0); 
    if(strcmp (cexp2->literal, "local") ) 
       do_error(ctl, pos, error_bypass_rule,
          "Iteration var of repeat expression must be simple local var");
     if(cexp3->op != op_name)
        do_error(ctl, pos, error_bypass_rule,
          "Iteration var of repeat expression must be simple local var");

   skip_ws(ctl, pos);

   /* check for index reference */
   lit_offset = 0;
   if(next_non_ws(ctl, pos, LEFT_PAREN_CHAR)) {
    if(!parse_opnd(ctl, pos, rule, &lit_offset)) 
       do_error(ctl, pos, error_bypass_rule, "Repeat expression");

      /* ensure name is simple local */
       cexp1 = &(ctl->CExps[lit_offset]);
       cexp2 = &(ctl->CExps[cexp1->opnd1]); 
       cexp3 = &(ctl->CExps[cexp1->opnd2]); 

       if(cexp2->op != op_name) assert(0); 
        if(strcmp (cexp2->literal, "local") ) 
          do_error(ctl, pos, error_bypass_rule,
            "Index var of repeat expression must be simple local var");
        if(cexp3->op != op_name)
          do_error(ctl, pos, error_bypass_rule,
           "Index var of repeat expression must be simple local var");

       if(next_non_ws(ctl, pos, RT_PAREN_CHAR) == NULL) 
           do_error(ctl, pos, error_bypass_rule, "Repeat expression");
    }
   skip_ws(ctl, pos);
   

   if(!parse_simple_name(ctl, pos, lookuptype_name, &in)) 
          do_error(ctl, pos, error_bypass_rule, "Repeat expression");

   skip_ws(ctl, pos);

   if(strcmp(in, "in"))   
      do_error(ctl, pos, error_bypass_rule, "Repeat expression");

  if(!parse_opnd(ctl, pos, rule, &opnd2_offset)) 
    do_error(ctl, pos, error_bypass_rule, "Repeat expression");
   
    store_cexp(ctl, op_repeat, (void *)lit_offset,
          opnd1_offset, opnd2_offset, repeat_offset); 

   next = next_non_ws(ctl, pos, COLON_CHAR); 
   if(next == NULL) do_error(ctl, pos, error_bypass_rule, "Repeat expression");

   return 1;
}
     
/***************************************************************/
int cvt_tree_to_cexps(PCtl* ctl, Pos *pos, TreeNode *treetop) {

   Net    *net;
   State  *begin_state, *end_state; 
   int     disj_ct, conj_ct;
   int     disj_array[MAX_CEXP_LIST], conj_array[MAX_CEXP_LIST]; 
   

    /* Create set of conjunctive expressions from cexp tree */
    /* first build network                                 */
    net = new_net();
    begin_state = new_state(net);
    end_state = new_state(net);
    end_state->final = 1;
    cvt_tree_to_net(ctl, pos, net, treetop, begin_state, end_state);
 
    /* Then convert net to one or more conjunctive expressions */  
    /* one for each combination of disjunctive alternatives    */
     disj_ct = conj_ct = 0; 
     cvt_net_to_disj_list(ctl, begin_state, 
                       &disj_ct, disj_array,
                        conj_ct, conj_array);
     free_network(net); 

    /* Now have disj_ct disjuncts                               */
    /* Convert it to a disjunctive expression                    */ 

     return( cvt_cexp_list(ctl, op_or, disj_ct, disj_array));

}   


/*************************************************************************/
/* Parse phrase of form "cat :var in set:(args) [constraints]"           */
/* where repeat (:var in set:) and [constraints] are optional            */
static int parse_category_phrase(PCtl *ctl, Pos *pos, Rule *rule,
                                 int *edge_offset) {

  Pos          tpos;
  Edge        *edge;
  char        *catname; 
  CategoryType cat_type;  
  Rule        *litrule;
  PropDesc     *argdesc;
  TreeNode    *tree_top, *child_chain; 
  int          child_disj;

  skip_ws(ctl, pos);

  lclmemcpy(&tpos, pos, sizeof(Pos));

  if(!parse_category_name(ctl, &tpos, &catname, &cat_type))  
       return 0;
  
  *edge_offset = ctl->next_Edge_offset;
   edge = new_Edge(ctl);

   edge->category_name = catname; 

   parse_repeat(ctl, &tpos, rule, &edge->repeat_exp);

   /* treat TRUE as epsilon with no constraints */
   if(cat_type == cat_type_true)  {
           lclmemcpy(pos, &tpos, sizeof(Pos));
           return 1;
   }

   if(cat_type != cat_type_epsilon) {
      /* Parse args into chained arg expressions */
      if(!parse_invocation_args(ctl, &tpos, rule,
               &(edge->first_arg_cexp), &(edge->arg_ct) ))  
           do_error(ctl, &tpos, error_bypass_rule, 
                    "Missing or erroneous category args");
   }
   
   
   /* Parse constraints if any... return 0 is real error  */
    if(next_non_ws(ctl, &tpos, LEFT_SQUARE_CHAR)) { 

      /* create tree for constraint expression */    
       /* If error in constraints, will not return */
      tree_top = new_tree_node(ctl);
      child_chain = NULL;
      parse_group(ctl, &tpos, rule, GroupTypeConstr, 1,
                       &child_chain, &child_disj); 
      tree_top->first_child = child_chain;
      tree_top->ctns_disj = child_disj;
      edge->eqs = cvt_tree_to_cexps(ctl, pos, tree_top); 
    }

    /* if is immediate literal string reference, build a constructed */ 
    /* out of line rule to accomodate                                 */
    if(cat_type == cat_type_literal_line || 
       cat_type == cat_type_literal_string) {
       
       edge->category_index = ctl->next_Rule_offset;

       litrule = new_Rule(ctl); 
       litrule->ruletype = ruletype_L;
       litrule->ext_arg_desc_offset = ctl->next_PropDesc_offset; 
       litrule->arg_ct = edge->arg_ct;

       /* dummy up arg descriptor.. just set up n string descriptors */
       /* and check that args are simple names                      */
       /* (error will be reported in called routine                */
       if(build_immediate_parm_descriptor(ctl, litrule, &tpos,
                   edge->first_arg_cexp, &argdesc)) {

           parse_literal_rule_line(ctl, &tpos, 1, cat_type, argdesc,
                          &litrule->first_literal_line);

           litrule->line_ct = 1;
        }
    }
  
  lclmemcpy(pos, &tpos, sizeof(Pos));
  return 1;
}

 

/**************************************************************************/
/* Recursively parse rhs category-phrase seq of R, or eq_phrase sequence  */
/* of category_phrase into tree.                                          */
/* Leaves represent represent either phrases or disjunctions "|".         */
/* Internal nodes (subtrees) represent {}  or [] groups.                  */
static int parse_group(PCtl *ctl, Pos *pos, Rule *rule, GroupType grouptype,
                       int first, TreeNode **group_chain, int *group_disj ) { 

   Pos      tpos, tpos_save;
   TreeNode *tree_node, *child_chain, *this_chain, *new_node;
   TreeNode *begin_chain, *end_chain, *last_disj;
   int       rslt_offset, disjunction_found, i, child_disj;
   int       group_first, first_disj_here, require_disj; 
   char      groupbeginchar, groupendchar, *next; 

   lclmemcpy(&tpos, pos, sizeof(Pos)); 
  *group_disj = 0;

   if(grouptype == GroupTypeCategory) {
          groupbeginchar = '{';
          groupendchar =   '}';
   }
   else {
         groupbeginchar = '[';
         groupendchar =   ']';
   }  

   /* if any disj between terms, require disj between all terms of group*/
   disjunction_found = 0;
   *group_disj = 0;
   group_first = 1;
   first_disj_here = 0;
   require_disj = 0;

   while(1) { 

      skip_ws(ctl, &tpos); 

      lclmemcpy(&tpos_save, &tpos, sizeof(Pos)); 
      next = next_chr(ctl, &tpos, ANY_CHAR); 
      if(next == NULL)
          do_error(ctl, &tpos, error_bypass_rule, 
                 "Unknown A category or constraint element");


      /*If parsing category seq, end of group shown by } or . */
      if (  ( *next == groupendchar)      ||  
              (first && (grouptype == GroupTypeCategory) && *next =='.') )
                break;

      tree_node = new_tree_node(ctl); 

      if( *next == groupbeginchar) {        
           tree_node->type = TreeNodeGroup; 
           child_chain = NULL;
           
           if(!parse_group(ctl, &tpos, rule, grouptype, 0,
                &child_chain, &child_disj))
                 do_error(ctl, &tpos, error_bypass_rule, 
                     "Category or constraint error");
           tree_node->first_child = child_chain;  
           tree_node->ctns_disj = child_disj;  
           if(group_first) first_disj_here = 1;  
           else first_disj_here = 0;
           group_first = 0;
           if(require_disj) 
                do_error(ctl, &tpos, error_bypass_rule, 
                  "Error in disjunctive phrase");
           if(disjunction_found) require_disj = 1;
       }    
      
      else if(*next == '|') { 
           tree_node->type = TreeNodeDisj;
           if((!disjunction_found) && first_disj_here != 1) 
                do_error(ctl, &tpos, error_bypass_rule, 
                  "Error in disjunctive phrase");
           disjunction_found = 1;
           first_disj_here = 0;
           group_first = 0; 
           require_disj = 0;
      } 

      else if(*next == '#') { 
         /* comment */
         while(next_chr(ctl, &tpos, NOT_NL_CHAR)) {continue;}
         /*next_chr(ctl, &tpos, NL_CHAR); */
         continue;
       }

      else {
           lclmemcpy(&tpos, &tpos_save, sizeof(Pos)); 
           if( (grouptype == GroupTypeCategory &&
           parse_category_phrase(ctl, &tpos, rule,  &rslt_offset)) || 
          (grouptype == GroupTypeConstr &&
           parse_eq_phrase(ctl, &tpos, rule,  &rslt_offset)) )  {
              tree_node->type = TreeNodeLeaf; 
              tree_node->offset = rslt_offset;  
              if(group_first) first_disj_here = 1;
              else first_disj_here = 0;
              group_first = 0;
              if(require_disj) 
                 do_error(ctl, &tpos, error_bypass_rule, 
                  "Error in disjunctive phrase");
             if(disjunction_found) require_disj = 1;
          }    
     
         else do_error(ctl, &tpos, error_bypass_rule, 
                 "Unknown B category or constraint element");
     }


      if(*group_chain == NULL) *group_chain = tree_node; 
      else {
           this_chain = *group_chain;
           while(this_chain->next_sibling != NULL) 
                 this_chain = this_chain->next_sibling;
           this_chain->next_sibling = tree_node;
         }
  } /* end while 1 */

 /* If a disjunction found, extend tree so that multi-node alternatives */ 
 /* are at separate level.  This simplifies subsequent processing       */
 if(disjunction_found) {
    *group_disj = 1;
    first = 1;
    last_disj = NULL;
    this_chain = *group_chain;
    while(1) {  
       for(i= 0, begin_chain= this_chain;
           (this_chain!=NULL)  && (this_chain->type!= TreeNodeDisj); 
            this_chain= this_chain->next_sibling, i++) { 

            end_chain = this_chain;
       } 
      
       /* now this_chain is disjunct or end of group */

       if(i > 1) {
          new_node = new_tree_node(ctl);
          new_node->first_child = begin_chain;
          if(this_chain != NULL) 
              new_node->next_sibling = this_chain;
          else new_node->next_sibling = NULL;
          new_node->ctns_disj = 1;
          end_chain->next_sibling = NULL;

          if(first) {
             *group_chain = new_node;
             first = 0;   
          }  
          else last_disj->next_sibling = new_node;
       }

       last_disj = this_chain;
       if(this_chain != NULL) this_chain = this_chain->next_sibling; 
       else break;

    } /* end while (1) */
 
  }/* end if disjunction found */

  lclmemcpy(pos, &tpos, sizeof(Pos));
  return 1;
}

       
/***********************************************************************/
/* Parse an R_xxx() rule                                               */ 
/* building Rule, category net, and constraint structures              */   
static void parse_grammar_rule(PCtl *ctl, Pos *pos, Rule *rule) {

   Pos      tpos;
   TreeNode *tree_top, *child_chain;
   int       child_disj;
   Net       *net; 
   State    *begin_state, *end_state;
  

   lclmemcpy(&tpos, pos,  sizeof(Pos));

   rule->ruletype = ruletype_R;

   rule->ext_arg_desc_offset = ctl->next_PropDesc_offset;

   /* if no args, will have a PropLast parm */
   if(!parse_rule_parameters(ctl, &tpos, 0, 0)) 
        do_error(ctl, &tpos, error_bypass_rule, 
                 "Missing rule parameters");

   rule->arg_ct = ctl->next_PropDesc_offset - rule->ext_arg_desc_offset - 1; 

   rule->local_arg_desc_offset = ctl->next_PropDesc_offset;
   
   /* Ok if no locals */
   parse_rule_parameters(ctl, &tpos, 0, 1);

   if(!parse_arrow(ctl, &tpos))
          do_error(ctl, &tpos, error_bypass_rule, "Missing -->");

   /* Parse category sequence into tree by recursive procedure.      */
   /* leaf nodes represent either category edges, or disjunctions.   */
   /* internal nodes represent {} groups                             */
   tree_top = new_tree_node(ctl);
   tree_top->type = TreeNodeGroup;
   child_chain = NULL;
   if(!parse_group(ctl, &tpos, rule, GroupTypeCategory, 1,
                  &child_chain, &child_disj)) {  
         do_error(ctl, &tpos, error_bypass_rule, 
                 "Error in category sequence");
   }
   tree_top->first_child = child_chain;
   tree_top->ctns_disj = child_disj;

   /* Create network from category sequence tree */
   net = new_net();
   begin_state = new_state(net);
   net->start_state = begin_state;
   end_state = new_state(net);  
   end_state->final = 1; 

   cvt_tree_to_net(ctl, pos,  net, tree_top, begin_state, end_state);
   rule->first_node = cvt_net_to_category_net(ctl, net);

   lclmemcpy(pos, &tpos, sizeof(Pos));
   
}
  
/*******************************************************/
static int parse_rule(PCtl *ctl, Pos *pos) {

  Pos tpos;
  char  *rulename; 
  int    temp_index;
  Rule  *rule;
  CategoryType cattype;

  lclmemcpy(&tpos, pos, sizeof(Pos));   

  skip_ws(ctl, &tpos); 

  if(!parse_category_name(ctl, &tpos, &rulename, &cattype)) 
         return 0;

  if( (cattype != cat_type_grammar_cat) &&
      cattype != cat_type_literal_block)  {
       do_error(ctl, pos, error_bypass_rule,
            "Rule name must be R_xxxx or L_xxx");
  }
     
  /* TBD: add defintion point for rulename */
  temp_index = ctl->next_Rule_offset;
  rule = new_Rule(ctl);
  rule->scope_index = ctl->current_scope_index;
  rule->rulename = rulename;
  rule->rule_index = temp_index;
  
  if(cattype == cat_type_grammar_cat)
       parse_grammar_rule(ctl, &tpos, rule);

   else if(cattype == cat_type_literal_block)
         parse_literal_rule(ctl, &tpos, rule);
  
   else assert(0);

   lclmemcpy(pos, &tpos, sizeof(Pos));
   return 1;
}
  
/**************************************************************/
/* Can put comment beyond first char                          */
static int parse_comment_or_endscope(PCtl *ctl, Pos *pos, int *end_scope) {

  char *next;
  int  line_number, char_number;
  
 *end_scope = 0; 

  skip_ws(ctl, pos);

  next = next_chr(ctl, pos, NOT_WS_CHAR);

  if(next == NULL) { /* must be end of file */ 
     if(next_chr(ctl, pos, EOF_CHAR)){
          *end_scope = 1;
           return 1;
     }
     assert(0);
    }

  /* comment */
  if(*next == '#') {
     while(next_chr(ctl, pos, NOT_NL_CHAR)) {continue;}
     next_chr(ctl, pos, NL_CHAR);
     return 1;
  }
       
  get_scan_linenumber(ctl, pos, &line_number, &char_number);

 /* might be comment or endscope */
 if( (*next == '=') && char_number == 2) {
      /* end scope if four '='  at line begin */
     if(next_chr(ctl, pos, EQ_CHAR)  &&   
       (next_chr(ctl, pos, EQ_CHAR)) &&   
       (next_chr(ctl, pos, EQ_CHAR)) )  
           *end_scope = 1; 
     while(next_chr(ctl, pos, NOT_NL_CHAR)) {continue;}
     next_chr(ctl, pos, NL_CHAR);
     return 1;
 }

return 0; 
}

/***************************************************************/
/* Look at each edge in scope, resolve local refs. Also,        */
/* zero and record refs to (first rule of) another scope         */
/* TBD... handle generated literal rules better                 */
void resolve_local_rule_refs(PCtl *ctl, Scope *scope) { 

  Rule       *rule;
  Edge       *edge;
  int        i, j, found;
  ExtRuleRef *extref;
  char        msg[256];

  edge = &(ctl->Edges[scope->first_edge]);
    
  for(i = 0; i < scope->edge_count; i++, edge++) {

      found = 0; 
      /* generated literal rule       */
      if(edge->category_index != 0) continue;
          
      if(!strcmp(edge->category_name, "TRUE") || 
         !strcmp(edge->category_name, "E_")) continue;  
 
      rule = &(ctl->Rules[scope->first_rule]);
      for(j = 0; j < scope->rule_count; j++, rule++) {

         if(rule->rulename != NULL  &&
           (!strcmp(edge->category_name, rule->rulename))) {
                 edge->category_index = rule->rule_index;
                 if(edge->arg_ct != rule->arg_ct) {
                   sprintf(msg,
                      "Nonmatching arg count in reference to %s rule",
                       edge->category_name); 
                   do_error(ctl, NULL, error_misc, msg);
                 }
                 found = 1;
                 break;
         }
     } /*end for j */
  
     if(!found) { 
          extref = new_ExtRuleRef(ctl);
          extref->edge = scope->first_edge + i; /* so is offset */
     }

   } /* end for i */
}


/**************************************************************/
void resolve_nonlocal_rule_refs(PCtl* ctl) {

    char        message[256];
    ExtRuleRef *extref;
    int         i,j, found;
    Edge       *edge;
    Scope      *scope;
    Rule       *rule;   

    /* all arrays start at 1 so can identify null refs */ 
    extref = &(ctl->ExtRuleRefs[1]);

    for(i = 1; i < ctl->next_ExtRuleRef_offset; i++, extref++) {

       edge = &(ctl->Edges[extref->edge]);
         
       scope = &(ctl->Scopes[1]);
       found = 0;
       for(j = 1; j < ctl->next_Scope_offset; j++, scope++) { 

            if(!strcmp(edge->category_name, scope->first_rule_name)) {
                edge->category_index = scope->first_rule;
                rule = &(ctl->Rules[scope->first_rule]); 
                if(edge->arg_ct != rule->arg_ct) {
                    sprintf(message,
                        "Nonmatching arg count in reference to %s rule",
                         edge->category_name); 
                    do_error(ctl, NULL, error_misc, message);
                }
                found = 1;
                break;
            }
       } /* end for j */

      /* Improve to add line number for reference */
      if(!found)  {
         sprintf(&message[0], "Undefined rule reference %s", 
                               edge->category_name);
         do_error(ctl, NULL, error_misc, message);
       }
   } /* end for i */  

 }


/**************************************************************/
static int parse_scope(PCtl *ctl, Pos *pos) {

  int    rule_found, end_scope;
  Scope *scope;
  Rule  *rule;

  rule_found = 0;

  if(next_non_ws(ctl, pos, EOF_CHAR))
        return 0; 

  /* TBD: change to have explicit scope name */
  scope = new_Scope(ctl);  
  scope->first_edge = ctl->next_Edge_offset;  
  scope->first_rule = ctl->next_Rule_offset;

   if(setjmp(NextRuleEnv) != 0) {
       lclmemcpy(pos, &ctl->recover_pos, sizeof(Pos));
       ctl->next_Rule_offset = ctl->recover_rule_offset;
   }
   while (1) {
      end_scope = 0; 
      ctl->recover_rule_offset = ctl->next_Rule_offset;
      skip_ws(ctl, pos); 
      if(parse_rule(ctl, pos)) {
          /* if is first rule, indicate scope non empty           */
          /* and record name of rule, for external ref resolution */ 
           if(!rule_found) { 
              rule = &(ctl->Rules[scope->first_rule]); 
              scope->first_rule_name = rule->rulename;
              rule_found = 1;
            }
          continue;
       }

     if(parse_comment_or_endscope(ctl, pos, &end_scope)) {
         if(!end_scope) continue;
         if(end_scope && rule_found) break;
      }

     if(next_non_ws(ctl, pos, EOF_CHAR)) {
           do_error(ctl, pos, error_warning,
            "Missing end_scope marker at end of file");
           /* temporarily return 1 in case need scope finalization */
           if(rule_found) break;
           else return 0;
      }
      else do_error(ctl, pos, error_bypass_rule,
            "Unrecognized material");
    }  

  scope->rule_count = ctl->next_Rule_offset - scope->first_rule;
  scope->edge_count = ctl->next_Edge_offset - scope->first_edge;

  if( (scope->rule_count > 0) && (ctl->error == 0))
      resolve_local_rule_refs(ctl, scope);

  return 1;
}

/**************************************************************/
int parse_function_decl(PCtl *ctl, Pos *pos) {

  Pos       tpos;
  char     *fn_name; 
  int       temp_index;
  Function *function;
  char     *temp; 
  
  lclmemcpy(&tpos, pos, sizeof(Pos));   

  skip_ws(ctl, &tpos); 

  if(!parse_simple_name(ctl, &tpos, lookuptype_name, &temp)) return 0; 

  if( (temp[0] != 'F') || temp[1] != '_') return 0;

  temp_index = ctl->next_Function_offset;
  function = new_Function(ctl);
  /* move function name from input buffer because latter freed */ 
  fn_name = malloc(strlen(temp) + 1); 
  strcpy(fn_name, temp);
  function->function_name = fn_name;
  function->function_index = temp_index;
  
  function->arg_desc_offset = ctl->next_PropDesc_offset;

  /* function parms encoded same way as rule parms */
  /* if none, will have a PropLast parm */
  if(!parse_rule_parameters(ctl, &tpos, 0, 0)) 
        do_error(ctl, &tpos, error_bypass_function, 
                 "Missing function decl arguments: need at least ().");

  function->arg_ct = ctl->next_PropDesc_offset - 
         function->arg_desc_offset - 1; 

  skip_ws(ctl, &tpos); 

  if(!parse_simple_name(ctl, &tpos, lookuptype_name, &temp)) {
        do_error(ctl, &tpos, error_bypass_function, 
             "Function must have return keyword.");
     } 
  if(strcmp(temp, "return"))  
        do_error(ctl, &tpos, error_bypass_function, 
             "Function return keyword must be 'return'.");

  skip_ws(ctl, &tpos); 

  if(!parse_simple_name(ctl, &tpos, lookuptype_name, &temp)) {
        do_error(ctl, &tpos, error_bypass_function, 
             "Function must have return value type.");
  } 
  if(!strcmp(temp, "Int"))  
         function->returnkind = PropInt;
  else if(!strcmp(temp, "LongInt"))  
         function->returnkind = PropLongInt;
  else if(!strcmp(temp, "String"))  
         function->returnkind = PropString;
  else if(!strcmp(temp, "Opaque"))  
         function->returnkind = PropOpaque;
  else if(!strcmp(temp, "List"))  
         function->returnkind = PropList;
  else if(!strcmp(temp, "Any"))  
         function->returnkind = PropStruct;
  else do_error(ctl, &tpos, error_misc, 
          "Function return value type must be Int, String, List,"
              " or Opaque.");

  if(next_non_ws(ctl, &tpos,PERIOD_CHAR) == NULL) 
          do_error(ctl, &tpos, error_bypass_function, 
           "Function decl must end with period.");

  lclmemcpy(pos, &tpos, sizeof(Pos));   
  return 1;
}



/**************************************************************/
void parse_function_decls(PCtl *ctl, Pos *pos) {

  int dummy_end_scope;
  Pos tpos; 

  if(next_non_ws(ctl, pos, EOF_CHAR))
        return; 

   if(setjmp(NextFunctionEnv) != 0) {
       lclmemcpy(pos, &ctl->recover_pos, sizeof(Pos));
       ctl->next_Function_offset = ctl->recover_function_offset;
   }
   while (1) {
      ctl->recover_function_offset = ctl->next_Function_offset;

      /* parse comment doesn't preserve pos */
      lclmemcpy(&tpos, pos, sizeof(Pos));   
      if(parse_comment_or_endscope(ctl, &tpos, &dummy_end_scope)) { 
            lclmemcpy(pos, &tpos, sizeof(Pos));   
            continue;
       }

      if(parse_function_decl(ctl, pos)) { }
   
      else break;
   }

   return;
}

  


/**************************************************************/
/* build outputs for one file                                 */
/* Note: functions must be in first file                     */
int parse_stub_spec(char *infilename, FILE *outfile, HCtl *hc, 
                  int serial /* for output array names */,
                  int first) {

    char       message[200];  
    PCtl      *ctl;
    Pos        pos;
    InputLine *newline;
    int        is_error;

    ctl = (void *)calloc(1, sizeof(PCtl));

    /* allocate arrays rooted in ctl */ 
    /* allocate functions only if first */
    do_array_allocates(ctl, first);

    ctl->input_file_name = infilename;
    ctl->input_file_handle = fopen(ctl->input_file_name, "r");   
    if(ctl->input_file_handle == NULL) {
      sprintf(&message[0], "Cannot open file %s\n", ctl->input_file_name); 
      do_error(ctl, NULL, error_fatal, message);
    }

    if(!get_next_input_buffer(ctl)) { 
      sprintf(&message[0], "Empty file %s\n", ctl->input_file_name); 
      do_error(ctl, NULL, error_fatal, message);
    }

    pos.bufno = 1;  
    pos.bufchar = 0;

    ctl->current_input_line = 1;
    newline = &(ctl->InputLines[1]);
    newline->buffer_number = 1;
    newline->begin_char = 0;

    new_outbuf(ctl);

    ctl->hc = hc; 
    ctl->current_scope_index = 1;
    ctl->stringloc = "String";

   if(first) 
       parse_function_decls(ctl, &pos);
   else {
       ctl->Functions = hc->Functions;
       ctl->next_Function_offset = hc->next_Function_offset;
       ctl->allocated_Function_offsets = hc->allocated_Function_offsets;  
    } 
    while(1) {
       if(!parse_scope(ctl, &pos)) break;
    }

    if(ctl->error == 0)
       resolve_nonlocal_rule_refs(ctl);

    ctl->sbuffer = (void *)malloc(SBUFSIZE);

    if(!ctl->error)
      write_grammar_decls(ctl, first, serial, infilename, outfile);

    free(ctl->sbuffer);

 
   free_input_buffers(ctl);
   free_output_buffers(ctl);

   do_array_frees(ctl);  /* except for functions */
   /* if first */
   if(hc->Functions == NULL) {
       assert(first);
       hc->Functions = ctl->Functions;
       hc->next_Function_offset  = ctl->next_Function_offset;
       hc->allocated_Function_offsets  = ctl->allocated_Function_offsets;
   }
   is_error = ctl->error;
   free(ctl);
   return(!is_error);
}
    


/**************************************************************/

int main(int argc, char *argv[]) {

  int   i;
  FILE *outfile;
  char *infilenames[MAX_RULE_SETS];
  HCtl  *hc;  /* hash chain ctl */

  if(argc < 2) {
     fprintf(stderr, "Missing output filename");
     return 0;
   }

  if(argc < 3) { 
     fprintf(stderr, "Missing input filename");
     return 0;
   }

   hc = (void *)calloc(1, sizeof(HCtl));

   outfile = fopen(argv[1], "w");  

   hc->hash_chains_begin =
            (void *)calloc(1,INITHASHCHAINALLOC * sizeof(HashEnt));
   hc->current_hashchain_alloc = INITHASHCHAINALLOC;
   hc->next_hashchain_ent = 1;

   for(i = 2; i < argc; i++) {
      if(i == 2) { 
          /* indicate first for function scan */    
         if (!parse_stub_spec(argv[i], outfile, hc, i-2,1 )) return 0;
       }
      else { 
          if (!parse_stub_spec(argv[i], outfile, hc, i-2, 0)) return 0;
      }
      infilenames[i-2] = argv[i]; 
   }     

   write_summary_rule_set(outfile, infilenames, hc->next_hashchain_ent,
        i-2);

   fclose(outfile); 

   return 0;
} 
