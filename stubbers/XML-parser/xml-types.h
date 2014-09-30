
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

#ifndef XML_TYPES_H_
#define XML_TYPES_H_

#include <stdio.h>
#include "doctree.h"

#define INT_HEAP_SIZE           10000
#define EXTBUFALLOC             10000
#define STRINGBUFALLOC          10000
#define LINELISTALLOC           10000
#define INT_BUF_ALLOC           1000
#define INT_HEAP_ALLOC          10
#define STRING_BUF_LIST_ALLOC   100
#define MAX_SINGLE_CODEPOINT    255 
#define MAX_DOUBLE_CODEPOINT    65535
#define MAX_ENTITY_REF_SIZE     256
#define INIT_BTREE_ALLOC        500
#define FIRST_BTREE_AVAIL       8
#define IDHEAPALLOC             1000     
#define IDHASHSIZE              4096
#define ENTITY_STACK_SIZE       10

/* for element fsms, index in btrees */ 
#define EPSILON_INDEX  0 
#define CDATA_INDEX    1


#define lclmemcpy(a,b,c) (memcpy( (void *)(a), (void *)(b), (c))) 
#define lclmemset(a,b,c) (memset( (void *)(a), (b), (c))) 

/********************************************************************/
/* Error type definitions                                            */ 

typedef enum {dtd_syntax_error, syntax_error,formation_error,
              validation_error,
              compatibility_error, warning_error, fatal_error} ErrorKind; 
typedef enum {recover_before_right_angle,
              recover_after_element, 
              recover_before_end_element, 
              recover_at_right_angle,
              recover_before_left_angle,
              no_recover} RecoverKind;

/***********************************************************************/
/* Character search definitions                                        */

typedef enum {ESCAPED_AMPERSAND,
              AMPERSAND_CHAR,
              SEMICOLON_CHAR,
              COLON_CHAR, 
              RETURN_CHAR, 
              LINEFEED_CHAR,
              HASHMARK_CHAR, 
              BLANK_CHAR,
              TAB_CHAR,
              PERCENT_CHAR,
              MINUS_CHAR,
              PERIOD_CHAR,
              EXCLAMATION_CHAR,
              ESCAPED_DOUBLE_QUOTE,
              DOUBLE_QUOTE_CHAR,
              ESCAPED_APOSTROPHE, 
              APOSTROPHE_CHAR,
              LEFT_PAREN_CHAR,
              RIGHT_PAREN_CHAR,
              STAR_CHAR, 
              PLUS_CHAR,
              UP_SLASH_CHAR,
              ESCAPED_LEFT_ANGLE,
              LEFT_ANGLE_CHAR,
              EQUAL_CHAR, 
              ESCAPED_RIGHT_ANGLE,
              RIGHT_ANGLE_CHAR,
              QUESTION_CHAR,
              SNAIL_CHAR, 
              LETTER_CHAR,
              DIGIT_CHAR, 
              LEFT_SQUARE_CHAR,
              DOWN_SLASH_CHAR, 
              RIGHT_SQUARE_CHAR,
              UP_CHAR,
              UNDERSCORE_CHAR,
              DONTKNOW_CHAR,
              LEFT_CURLY_CHAR,
              VERTICAL_BAR_CHAR,
              RIGHT_CURLY_CHAR,
              TILDE_CHAR,
              UNUSED_CHAR,
              DOLLAR_CHAR,
              COMMA_CHAR,
              EOT_CHAR
            } SpecialEnum; 
              
#define MIN_ESCAPED_CHAR 0x80 
#define MAX_ESCAPED_CHAR 0x84
#define MAX_SPECIALS 0x84

/* special character types used in next_char operations */             
typedef enum {
              CT_Specific,   /* specific code */
              CT_Any,  
              CT_Quote,       /* ' or " */
              CT_EndQuote,   /* ' or " or & or < */
              CT_NameBegin,  /* see spec */
              CT_NameChar,   /* see spec */ 
              CT_WS, 
              CT_EndCDATA,   /* & or < */
              CT_RepeatOp,  /* +, *, ? */
              CT_EndElement  /* / or > */
            } CharacterRequest;   

/***********************************************************************/
/* Parse states relevant to entity substitution handling               */
/* Represent bit positions in Pos->ES_state                            */
/* Set by xml and dtd parsers                                          */

#define ES_internal_dtd  1
#define ES_external_dtd  2
#define ES_PI            4
#define ES_CDATA         8         
#define ES_COMMENT       16
#define ES_attvalue      32 
#define ES_entityvalue   64
#define ES_first_mkup_char 128
/* Other?       */


/***********************************************************************/

/* BTree structures for element, attribute, entity, notation lookup */

/* used in call to parse_name to indicate btree to use for lookup */
/* next temporary */
typedef enum {
             no_lu,
             lu_element,
             lu_attribute,
             lu_pe,          /* parameter entity*/
             lu_ge,          /* general_entity */
             lu_notation
             } LookupType;

typedef struct {
               LookupType      lu_type;
               XString         entry_string;  /* in string buffer */
               uint16          smaller;
               uint16          larger;
               Node           *definition;  /* if element or notation */
                                          /* and is defined      */
             } XmlBTree;

typedef enum {
             ASCII = 0, ISO_8859_1 = 1, UTF_16 = 2, UTF_8 = 3,
             ENCODING_UNSTATED = 4} EncType; 

#define DEFAULT_8_ENCODING ISO_8859_1


/***********************************************************************/
/* hash table and associated hash chains used for element ids and refs  */           
struct IdEntry_ {
        XString          id_string;
        int              defined;
        Pos              undef_pos;  /*one place where not defined*/
        struct IdEntry_ *next;
      };

typedef struct IdEntry_ IdEntry;  


/***********************************************************************/
/* Directed graph structure used to represent valid content */
/* ordering alternatives                                    */
struct State_ { 
       int           is_final;
       struct Arc_   *arc_set;
       struct State_ *next;         /* for debugging only */
       int            seq_no; /* debugging only */ 
      };

typedef struct State_ State;

struct Arc_ {
       int          label;
       State       *destination;
       struct Arc_ *next;
     };

typedef struct Arc_ Arc;

typedef struct {
      State *first_state;
      } Net;

/***********************************************************************/
/* Tree structure used initially to represent content ordering alts */
/* e.g., SEQ( OPT(TYPE(X))) TYPE(Y) PLUS(TYPE(Z)) ALT(        )  ))) */
typedef enum { OpKindType, OpKindSeq, OpKindAlt, OpKindStar,
               OpKindPlus,OpKindOpt, OpKindCdata, OpKindNone} ReOpKind;

struct OpTree_{
             ReOpKind         op;
             struct OpTree_  *first_opnd;
             struct OpTree_  *next;
             XString         value;   /* if op is type */
             int             id;
         };
typedef struct OpTree_ OpTree;
                

/***********************************************************************/
/* DTD content storage                                                 */ 
/* ElementSpec and AttrSpec are eventual representatives of element defs*/ 

typedef enum {AttrKindCDATA = 0,    AttrKindID = 1,
              AttrKindIDREF= 2,     AttrKindIDREFS= 3, 
              AttrKindENTITY= 4,    AttrKindENTITIES = 5,
              AttrKindNMTOKEN = 6,  AttrKindNMTOKENS= 7,
              AttrKindNOTATION = 8, 
              /* the following are not standard XML */ 
              AttrKindINT = 9,     AttrKindUINT = 10,
              AttrKindNAME = 11,    AttrKindNAMES = 12,
              /* but Enum is */
              AttrKindEnum = 13
             } AttrKind;
typedef enum {AttrRequired = 0, AttrImplied = 1,
              AttrFixed = 2,    AttrOrdinary = 3} DefaultKind;

struct ValueList_ {
                 XString           value;
                 struct ValueList_ *next;
                };

typedef struct ValueList_ ValueList;
     
struct AttrSpec_{
                XString         attr_name;
                AttrKind        attr_kind;
                uint16          index;  /* in btree-- non unique */ 
                char            is_duplicate; 
                char            temp_mark;
                ValueList      *attr_alt_list;
                DefaultKind     default_kind; 
                XString         default_value; 
                Pos             pos;
                struct AttrSpec_ *next;
              }; 

typedef struct AttrSpec_ AttrSpec;

struct AttrList_{
                XString        element_name;
                Node          *attr_list_node;  /* for error pos reporting*/
                uint16         index; /* of element in btree */ 
                AttrSpec      *first_attr_spec;
                struct AttrList_ *next;
              };

typedef struct AttrList_ AttrList;

typedef enum {CtnKindEmpty, CtnKindAny, CtnKindMixed, CtnKindGeneralRE,
              CtnKindUnknown} CtnKind; 

struct ElementSpec_{ 
                XString       name; 
                uint16        e_type_index;   
                AttrList     *attrlist;
                CtnKind       content_type;
                OpTree       *top_tree;          /* initial content spec*/
                State        *first_graph_state;  /* xformed content spec*/
                struct ElementSpec_ *next;
               };

typedef struct ElementSpec_ ElementSpec;

typedef struct { 
               int       is_system;   /* else public */ 
               XString   systemid;
               XString   publicid;
               char     *use_id;
              } ExternalID;


typedef struct {
                int           has_external_dtd;  
                ExternalID    external_dtd;
                ElementSpec  *first_element_spec;  /* probably unused */
                AttrList     *first_attr_list;
                XString       root_element_name; 
                uint16        index;  /* of root element in btree*/ 
             } DtdCtl;


/* for all but self defining entities */
struct Entity_{
       XString         name;
       uint16          index;  /* in parameter or general entity btree */
       uint16          notation_index;
       char            parameter_entity;   /* 1/0 */
       char            internal;  /* 1/0. if 1 def is entity_string*/
                                  /* else external_id */
       char            unparsed;  /* is not parsed entity*/
       char            unused;    /* space not used */
       XString         entity_string;
       ExternalID      external_id;
       XString         notation;
    };

typedef struct Entity_ Entity;


/* for notations  defined in <!NOTATION decls */

typedef struct {
       XString         name; 
       XString         notation;
       uint16          index;  /* in btree */
       ExternalID      external_id;
    } Notation;


/* only notations referenced in attribute defs and not */
/* defined at time of reference                        */
struct notation_ref_ {
       int                  index;
       Pos                  refpos;
       struct notation_ref_ *next;
   };
typedef struct notation_ref_ NotationRef;



/**************************************************************************/
typedef enum {MarkupTypeElement, MarkupTypeCDATA,   MarkupTypeEndElement,
              MarkupTypePI,      MarkupTypeComment, MarkupTypeXmlDcl,
              MarkupTypeDocType, MarkupTypeElementDecl, MarkupTypeAttlistDecl,
              MarkupTypeEntityDecl,  MarkupTypeNotationDecl,
              MarkupTypeConditional,
              MarkupTypeUnknown, MarkupTypeError} MarkupType;
   
typedef enum  { NotMarkup, UnknownMarkup, 
                ErrorMarkup, KnownMarkup} MarkupReturn;


/**************************************************************************/
/* Parse control structures                                               */

/* Internal buffers - separate material from different physical sources    */
/* to allow testing of entity nesting and enable full-scale error messages */
/* Parse from internal buffer, so only keep one physical source buffer     */
/* around for each active physical file                                   */
typedef struct {
        uchar  *buffer;  
        int     chars_used;  /* i.e., added */
        Entity *entity;      /* if null external file */
        int     entity_instance;  
        int     is_file;
        int     is_continuation;
        int     line_index;        /* of line entry of first char in buffer*/
        int     first_line_pos;    /* in line of first char                */
        int     parent_intbuf;     /* for error messages                   */
       } IntBufEnt;

typedef struct {
        uchar  *buffer;  
        int    chars_used;  /* i.e., added */
       } IntHeap;

/* One per source line; */
typedef struct  {
        uchar    *file_name;      /* will pt to file */ 
        XString  *entity_name;
        int       line_number;    /* in that file    */ 
        int       line_len;
        int       next_line_index;   /*for this file */ 
       } LineIndexEnt;

/* Storage for individual strings (lengths in entity/att structs) */
typedef struct {
        uchar *buffer;
        } StringBufEnt;


typedef enum { StackTypeExternal, StackTypeInternal,  
                         StackTypeRemainder} StackType;

/* Ctl for buffers for physical file  & internal entities */
struct SrcStack_ {
       Entity           *entity; /*!= 0 unless outer or external or */
                                       /* remainder                */
       int               entity_instance;
       uchar             is_external;
       uchar             is_remainder;  /*scanned after subs */ 
       uchar             is_extra;      /*scanned after subs */ 
       uchar             two_byte;
       uchar             last_was_return;
       uchar             unused;
       int               reached_end_of_file;
       int               cur_line_ent; 
       int               cur_intbuf_ent; 
       uint32            chars_used;
       size_t            char_count;
       FILE             *file_handle;
       uchar            *file_name;   
       uchar            *full_name;
       uchar              scheme[20]; 
       EncType           encoding; 
       uchar            *buffer;
       struct SrcStack_ *prev_stack;
     };
typedef struct SrcStack_ SrcStack;

/* allows for multiple btrees.. but might switch to hash */
typedef struct {
       XmlBTree   *btree;
       int        allocation;
       int        first_unused; 
       } BTreeCtl; 
       
typedef struct {
        DtdCtl        *dtd_ctl; 
        XmlCvtPublic   convert_pubid_proc; 
        Pos           *save_pos;        /*temp*/ 
        BTreeCtl       btree_ctl; 
        int            no_validation;
        SrcStack      *current_src_stack;
        IntBufEnt     *int_bufs;
        int            int_buf_alloc;
        int            int_bufs_used;

        IntHeap        *int_heaps;   
        int            int_heap_alloc;
        int            int_heaps_used;
        int            int_heap_size; 

        LineIndexEnt  *line_list;
        int            line_list_alloc;
        int            line_ents_used;
        Entity        *first_entity;  
        int            string_buf_list_alloc; /*to modularize allocs */
        StringBufEnt  *string_buf_list;      /*to modularize allocs */
        int            string_buf_ents_used;  
        uchar          *string_buf_pos;  /* in current buf */
        uchar          *string_buf_max;
        int            current_id_alloc;  
        int            current_ids_used; 
        IdEntry       *current_id_heap;            
        IdEntry      **id_hash_table;
        NotationRef   *first_notation_ref;  /* in attr spec & undef when ref*/
        int            notation_ref_alloc;
        int            notation_refs_used;
        Node          *top_node;
        Node          *dtd_node;
        Node          *dtd_subord_begin;  /*for include/ignore processing*/
        Node          *dtd_last_subord;   /*for include/ignore processing*/
        int            next_entity_instance;
        uchar          syntax_error;
        uchar          formation_error;
        uchar          validation_error;
        uchar          compatibility_error;
        uchar          fatal_error;
        uchar          interoperability_error; 
        uchar          warning_error; 
        uchar          next_is_newline; 
        uchar          is_double; 
        uchar          no_entity_ref; 
        uchar          standalone; 
        uchar          unused[3];
       } Ctl;

#endif
