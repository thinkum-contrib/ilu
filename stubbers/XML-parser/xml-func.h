
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

#ifndef XML_FUNC_H
#define XML_FUNC_H

#include "xml-types.h"

/********************************************************/
/* in errors.c                                          */
/********************************************************/

void do_error(Ctl *ctl, Pos *in_pos, Pos *pos, 
             ErrorKind errorkind, RecoverKind recoverkind,
             char *message);

void do_phys_error(Ctl *ctl, char *message, int fatal);

void do_warning(Ctl *ctl, Pos *position, char *message); 

void valid_error(Ctl *ctl, Pos *pos, char *message);

void do_validity_subst_error(Ctl *ctl, Pos *pos, char *message,
      XString *subst);

void dtd_error(Ctl *ctl, Pos *cur_pos,  Pos *pos, char *message);

void dtd_error_1(Ctl *ctl, Pos *cur_pos,  Pos *pos, char *message);

void do_dtd_subst_error(Ctl *ctl, Pos *cur_pos, Pos *pos, char *message,
      XString *subst);

int end_of_document(Ctl *ctl, Pos *pos);

void xml_get_line_data (Ctl *ctl, Pos *pos,
                   LineIndexEnt **rtn_line_ent, int *rtn_char_num);

/********************************************************/
/* in buffers.c                                         */
/********************************************************/

uchar *next_char(Ctl *ctl, Pos *cur_pos,
                    CharacterRequest   chartype,
                    SpecialEnum        special,
                    int                match);

SrcStack *load_first_buffer(Ctl *ctl, char *ext_name, int initial_call,
                   Pos *pos, Entity *entity);

void get_new_string_buf (Ctl *ctl, int min_len);

int valid_entity_nesting(Ctl *ctl, int first_buf_ind, int final_buf_ind);

void xml_convert_relative_ref(Ctl *ctl, ExternalID * external_id);

/********************************************************/
/* in dtd-parse.c                                       */
/********************************************************/

int parse_doctype(Ctl *ctl, Pos *pos, Node *parent, Node **node); 

int parse_dtd_attlist(Ctl *ctl, Pos *pos, Node *parent, Node **node); 

int parse_dtd_element(Ctl *ctl, Pos *pos, Node *parent, Node **node); 

int parse_dtd_notation(Ctl *ctl, Pos *cur_pos, Node *parent_node,
                      Node **node);

int parse_dtd_entity(Ctl *ctl, Pos *cur_pos, Node *parent_node,
    Node **node);

int parse_conditional(Ctl *ctl, Pos *cur_pos, Node *parent_node,
                      Node **node);

void build_in_entities(Ctl *ctl);

/********************************************************/
/* in xml-parse.c                                       */
/********************************************************/

int parse_name(Ctl *ctl, Pos *pos,
                int name_token_ok, LookupType lookup,
                XString *name, uint16 *index);

int get_keyword(Ctl *ctl, Pos *pos, int count, uchar *keywords[]);

void get_opt_WS(Ctl *ctl, Pos *pos);

int get_WS(Ctl *ctl, Pos *pos);

EncType parse_xml_decl(Ctl* ctl, Pos *pos, int reprocess);

EncType parse_text_decl(Ctl* ctl, Pos *pos, int reprocess);

void add_to_string_buf(Ctl *ctl, XString *invalue, XString *outvalue);

int parse_keyword(Ctl *ctl, Pos *cur_pos, int count, char *keywords[]);

int parse_qstring(Ctl *ctl, Pos *pos, XString *outstring, int convert);

MarkupReturn parse_markup(Ctl *ctl, Pos *cur_pos, Node *parent_node,
                 MarkupType *markup_type,  Node **node);

/********************************************************/
char *xml_get_cstring(char name[], int len, int is_double, char *buffer);

char *xstr2cstr(XString *xstr, int is_double);

void  cstr2xstr(char *cstr, XString *xstr);

/********************************************************/
/* in xml-lookup.c                                      */
/********************************************************/

Node *get_element_def(Ctl *ctl, XString *name);

int btree_search(Ctl* ctl, LookupType lu_type, XString *invalue,
              XString *outvalue, int makenew);

void btree_add_def (Ctl *ctl, LookupType lu_type, Node *node, uint16 index);
 
Node *btree_get_def(Ctl *ctl, LookupType lu_type, XString *name);

Node *btree_get_def_from_index(Ctl *ctl, LookupType lu_type, uint16 index);

ElementSpec *get_element_spec(Ctl *ctl, XString *name);     
      
Entity* get_ref_entity_def(Ctl* ctl, int parameter_entity,
                          XString name, int strip);

uchar *get_element_name(Ctl *ctl, uint16 index );

IdEntry *search_id_hash(Ctl *ctl, XString *value, int make_new);

void *xml_heap_alloc(Ctl *ctl, size_t size);

/********************************************************/
/* In xml-chars.c                                       */
/********************************************************/

void get_unescaped(Ctl *ctl, uchar this_char[]); 

void get_escaped(Ctl *ctl, uchar this_char[]); 

int is_special_p(Ctl *ctl, uchar this_char[], int special);

int is_x(Ctl* ctl, uchar this_char[]);

void get_linefeed(Ctl *ctl, uchar temp_char[]);
   
int get_specials_code(Ctl *ctl, uchar this_char[], SpecialEnum *special,
     int tr_special, int tr_quote);

/* JUST APPROXIMATION, SENSITIVE TO NON_ALPHAS ONLY for ISO-8859-1 */
/* AND DOESN"T DISALLOW some ISO-8859 nonalphas */
int name_char(Ctl *ctl, uchar this_char[]);

void append_xchar(Ctl *ctl, XString *char_group, uchar *this);

void append_character(Ctl *ctl, uchar *char_group, int *cur_len, 
   uchar *this);       

void get_blank_surrounded(Ctl *ctl, XString *target, XString *source); 

void escape_contained_quotes(Ctl *ctl, XString *target, XString *source); 

void store_character(Ctl *ctl, uchar buf_char[], uchar *this_char); 

int single_char_len(Ctl *ctl, int len);

/* ref will have trailing ';'*/ 
int convert_char_parameter(Ctl *ctl, uchar *ref, int is_dec, 
                          uint16 *codepoint);

int matches_request(Ctl *ctl, uchar the_char[], 
                    CharacterRequest chartype, SpecialEnum specialreq, 
                    int polarity);  

void XStringCopy(Ctl *ctl, XString *out, XString *in);

int CharsEq(Ctl *ctl, uchar *first, uchar *next);

int XStringsEq (Ctl *ctl, XString *s1, XString *s2);

/* like strcmp but on XStrings */
int XStringsCompare (Ctl *ctl, XString *s1, XString *s2);

int compare_with_special(Ctl *ctl, uchar input[], uchar compare);

int name_begin_char(Ctl *ctl, uchar this_char[]);

int digit_char(Ctl *ctl, uchar this_char[]);

int signed_begin_char(Ctl *ctl, uchar this_char[]);

/********************************************************/
/* in xml-valid.c                                       */
/********************************************************/

void validate_attributes(Ctl *ctl, Node *node);

void check_attributes(Ctl *ctl, Node *node);

int normalize_and_validate_attribute(
          Ctl      *ctl,         AttrSpec *aspec,
          XString  *aname,       XString  *attr_value,
          Pos      *pos,         int       is_default);


void check_referenced_ids(Ctl *ctl);

void validate_content(Ctl *ctl, Node *element);

/********************************************************/
/* in doctree.c                                         */
/********************************************************/
void print_document_tree( Node *inode, int offset, void *CONTROL);


#endif

