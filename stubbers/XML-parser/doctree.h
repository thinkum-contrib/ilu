
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

#ifndef DOCTREE_H
#define DOCTREE_H

/***********************************************************************/
/* Document content information storage                                */
#define uint16        unsigned short
#define uint32        unsigned int
#define uchar         unsigned char

typedef struct {
        uchar         *loc;
        int            len; 
      } XString;

typedef struct {
        uint16         buffer_number;  /* internal buffer */
        uint16         char_number;    /* in internal buffer */
        uint16         ES_state;
       } Pos; 

/* Node is holder of application document structure                 */
/* TBD: modify to use DOCSTRING type                                */
typedef enum {DOCUMENT, TEXT, ATTRIBUTE, ELEMENT, PI, COMMENT,
             /*rest for decls; will point to internal type entries  */
             XMLDECL,  DOCTYPEDECL, ELEMENTDECL,
             ATTLISTDECL, ENTITYDECL, NOTATIONDECL} NodeType;
	
struct Node_{
         NodeType      node_type;   /* attr, elem, CDATA, PI        */
         int           index;       /* for element, attr, doctype, entityef*/
                                    /* index in associated btree      */ 
         XString       name;        /* for elements,attributes, PI  */
                                    /* xxxx in <xxxx, <?, or  xxxx= */
         XString       value;       /* for attrs & CDATA & PI       */
         Pos           position;    /* initial position*/
         char          double_chars;
         char          temp_mark;
         char          built_in;    /* signals built-in entity*/
         char          unused;
         void          *internal_rep; /* for decls                     */
         struct Node_  *parent;       /* for ELEMENTs, CDATA, PI      */
         struct Node_  *first_attr;   /* only for ELEMENTs            */  
         struct Node_  *first_content; /*ELEMENT, CDATA,PI, DOCTYPE   */
         struct Node_  *next;    
      };

typedef struct Node_ Node;
typedef Node   NodeList;
typedef Node   NamedNodeList;

/***********************************************************************/
/* Parse invocation                                                    */
/**********************************************************************/

/* treetop needed to access top of document tree after parse  */ 
/* dctl needed to retrieve line numbers associated with nodes */

typedef char* (*XmlCvtPublic)(char *pub_id_literal);

int parse_xml(/*in*/ char *filename, Node **treetop, void **dctl,
               XmlCvtPublic convert_pubid_proc /* may be null*/); 


/**********************************************************************/
/* Access functions                                                   */
/**********************************************************************/

NodeType xml_getNodeType(Node *node); 

Node *xml_getParentNode(Node *node); 

Node *xml_getFirstChild(Node *node);

NodeList *xml_getChildren(Node *node);

int hasChildren(Node *node);

/* can't use this for attributes */
Node *xml_getPreviousSibling( Node *node);

Node *xml_getNextSibling(Node *node);

Node *nodeListItem(NodeList *nodelist, unsigned long index);
    
unsigned long xml_getLength(NodeList *nodelist);
    
NamedNodeList* xml_attributes(Node *node); 

/* In the following XtoString, if names stored double, return will reduce */ 
/* If buffer supplied (can be NULL), will be used as long as sufficient */
/* for length. Otherwise new storage will be allocated                 */

char *xml_ElementTypetoString(Node *node, char buffer[], int bufsize); 

char *xml_NametoString(Node *node, char buffer[], int bufsize);

char *xml_AttributeValuetoString(Node *node, char buffer[], int bufsize);

char *xml_DatatoString(Node *node, char *buffer, int bufsize);

    
Node *xml_getAttributeNodeByName(NamedNodeList *nodelist, char *name);  

void xml_getNodeLineData(void *ctl, Node *node,
                  char **file_name, int *line_number);

/* offset is offset of initial line for print */ 
void print_document_tree(Node *inode, int offset, void *CONTROL);
    
#endif
