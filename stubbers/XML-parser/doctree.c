
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "doctree.h"

/* These functions are closely related to the similarly named DOM methods */
/* However, all names, values returned as strings                         */
/* And function names extended somewhat to reflect object types           */
/* Also, Only get functions are reflected, and set is not complete        */

/**********************************************************************/
/* Utility functions  - should be moved                               */
/**********************************************************************/

/* Note: is_double distinction only between one and two byte storage */ 
/* two byte chars assumed to have high-order 0                       */
/* Uses given buffer if provided                                     */
char *xml_get_cstring(char name[], int len, int is_double, char *buffer) {
    char   *return_string;
    int     i, j;

    if (buffer != NULL) return_string = buffer;
    else return_string = (void *)malloc(len+1);

    if(is_double) {
       for(j = 0, i = 1; i < len; j++, i+=2) return_string[j] = name[i];
       return_string[j] = '\0';
   }
   else {
       for(i = 0; i < len; i++) return_string[i] =name[i];
       return_string[i] = '\0';
  }
  return(return_string); 
}

char *xstr2cstr(XString *xstr, int is_double) {
     return (xml_get_cstring((char*)xstr->loc, xstr->len, is_double, NULL));
}



void cstr2xstr(char *cstr, XString *xstr) {
    uint32 i;
    uchar *buffer;

    xstr->len = strlen(cstr) * 2;
    buffer = xstr->loc = (void *)malloc(xstr->len);  
    for(i = 0; i<strlen(cstr); i++) {
         buffer[i*2]= '\0';
         buffer[(i*2) +1] = cstr[i];
     }
}


/**********************************************************************/
/* Access functions                                                   */
/**********************************************************************/

NodeType xml_getNodeType(Node *node)  { 
           return(node->node_type);
 }

Node *xml_getParentNode(Node *node) { 
     return (node->parent);
}

Node *xml_getFirstChild(Node *node) {
    return(node->first_content);
}

NodeList *xml_getChildren(Node *node) {
       return(node->first_content); 
}

int hasChildren(Node *node) {
     return(node->first_content != NULL); 
}

/* can't use this for attributes */
Node *xml_getPreviousSibling( Node *node) {

     Node *parent_node, *this_node, *prev_node;

     parent_node = xml_getParentNode(node);
     if(parent_node == NULL) return NULL;

     prev_node = NULL;
    
     for(this_node = node->first_content; this_node; 
           this_node = this_node->next) {
           if(this_node == node) return prev_node;
           prev_node = this_node;
     }  

    return NULL; 
}

Node *xml_getNextSibling(Node *node) {
      return(node->next);
}


/* Ridiculously inefficient for all but short lists */
Node *nodeListItem(NodeList *nodelist, unsigned long index) {
    
    Node *node;
    unsigned long i;

    for(node = nodelist, i = 0; node; node = node->next, i++) {
       if(i == index) break;
    }
   if(node) return node;
   else return NULL;
}
    
unsigned long xml_getLength(NodeList *nodelist) {
    Node *node;
    unsigned long  i;

    for(node = nodelist, i = 0; node; node = node->next, i++) {
    }
   return i;
}

NamedNodeList* xml_attributes(Node *node){ 
    if(node->node_type != ELEMENT) return NULL;
    return(node->first_attr);
 }
    
/* node must be element                        */
/* like DOM tagName or elementType             */
/* but returns simple character string        */
/* In the following XtoString, if names stored double, return will reduce*/
/* If buffer supplied(can be null) will be used as long as sufficient    */ 
/* for length.  Otherwise new storage will be allocated                 */
char *xml_ElementTypetoString(Node *node, char *buffer, int bufsize) {
    char *sendbuf = NULL;

    if(node->node_type != ELEMENT) return NULL;
    if(buffer != NULL && node->name.len < bufsize) sendbuf = buffer;
    return( xml_get_cstring((char*)node->name.loc,
                     node->name.len, node->double_chars, sendbuf));
 }

char *xml_NametoString(Node *node, char *buffer, int bufsize) {
    char *sendbuf = NULL;

    if(buffer != NULL && node->name.len < bufsize) sendbuf = buffer;
    if( (node->node_type != ATTRIBUTE) && node->node_type != PI) return NULL;
    return (xml_get_cstring((char*)node->name.loc, node->name.len,
                 node->double_chars, sendbuf));
 }

/* like DOM toString method on attributes */
char *xml_AttributeValuetoString(Node *node, char *buffer, int bufsize) {
    char *sendbuf = NULL;
    if(buffer != NULL && node->value.len < bufsize) sendbuf = buffer;
    if(node->node_type != ATTRIBUTE) return NULL;
    return (xml_get_cstring((char *)node->value.loc, node->value.len,
            node->double_chars, sendbuf));
 }

char *xml_DatatoString(Node *node, char *buffer, int bufsize) {
    char *sendbuf = NULL;
    if(buffer != NULL && node->value.len < bufsize) sendbuf = buffer;
    if(node->node_type != PI && node->node_type != TEXT) return NULL;
    return (xml_get_cstring((char *)node->value.loc, node->value.len,
            node->double_chars, sendbuf));
 }

    
 Node *xml_getAttributeNodeByName(NamedNodeList *nodelist, char *name) {  
    Node *node;
    char *attname;
    char  stgbuf[100];

    if(nodelist->node_type != ATTRIBUTE) return NULL;

    for(node = nodelist; node; node = node->next) {
       attname = xml_NametoString(node, stgbuf, 100);
       if(!strcmp(attname, name)) {
         return node;
       }
    }
    return NULL;
}

extern void xml_get_line_source(void *ctl, Pos *pos, char **file_name, 
         int *line_number);

void xml_getNodeLineData(void *ctl, Node *node,
                  char **file_name, int *line_number) {
    
     xml_get_line_source(ctl, &node->position, file_name, line_number); 

}
    

    
    
/***********************************************************/
/* Debug prints                                            */

#ifdef PRINT_LINES
static void print_line_number(void *CONTROL, Node *node) {
      char *file_name;     
      int   line_number; 
      xml_getNodeLineData(CONTROL, node, &file_name, &line_number);
      fprintf(stdout, "\nFile %s line %d:  ", file_name, line_number);  
}
#endif

void print_offset(int offset) {
    int i;
    fprintf(stdout, "\n");
    for (i = 0; i < offset; i++) {
       fprintf(stdout, " ");
    }
}


static void print_attributes(Node *node, int offset, void *CONTROL){

   Node *anode; 
   char *print_text;
   char  buffer[500];
   anode = xml_attributes(node);
 
   for(; anode; anode =xml_getNextSibling(anode)) {
       print_offset(offset);
#ifdef PRINT_LINES
            print_line_number(CONTROL, anode);
#endif
       print_text = xml_NametoString(anode, buffer, 500);  
       if(print_text == NULL) { 
                 fprintf(stdout, "%s", "Missing attribute_name");
                 break;
        }
       fprintf(stdout, "attribute name = %-12s", print_text);
       print_text = xml_AttributeValuetoString(anode, buffer, 500);  
       if(print_text == NULL) { 
                 fprintf(stdout, "%s", "Missing attribute value");
                 break;
       }
       fprintf(stdout, "   value = %s", print_text);
   }
}

void print_document_tree( Node *inode, int offset, void *CONTROL) { 

  Node *node;
  NodeType nodetype;
  char  *print_text; 
  char   buffer[500];

  node = xml_getFirstChild(inode);

  for(; node; node=xml_getNextSibling(node)) {

      nodetype = xml_getNodeType(node);

#ifdef DEBUG_LINES
      print_offset(offset);
      
      print_error_source(CONTROL, node->position.buffer_number, 
                       node->position.char_number, 2, 1);
#endif
      switch(nodetype) { 
        case ELEMENT: 
            print_offset(offset);
#ifdef PRINT_LINES
            print_line_number(CONTROL, node);
#endif
            print_text = xml_ElementTypetoString(node, buffer, 500);
            if(print_text == NULL) { 
                 fprintf(stdout, "%s", "Missing element type");
                 break;
            }
            fprintf(stdout, "Element:  type = %s", print_text);
            print_attributes(node, offset+3, CONTROL);
            print_document_tree(node, offset+3, CONTROL); 
            break;
        case TEXT:
            print_offset(offset);
            print_text = xml_DatatoString(node, NULL, 0);  
            if(print_text == NULL) { 
                 fprintf(stdout, "%s", "Missing chardata");
                 break;
            }
            fprintf(stdout, "Data: %s", print_text);
            free(print_text);
            break;
        case PI:
            print_offset(offset);
            print_text = xml_NametoString(node, buffer, 500);  
            if(print_text == NULL) { 
                 fprintf(stdout, "%s", "Missing chardata");
                 break;
            }
            fprintf(stdout, "PI: target = %s", print_text);
            print_text = xml_DatatoString(node, NULL, 0);  
            if(print_text == NULL) { 
                 fprintf(stdout, "%s", "Missing pi data");
                 break;
            }
            fprintf(stdout, " data = %s", print_text);
            free(print_text);
            break;
        default:
            print_offset(offset);
            fprintf(stdout, "%s", "Erroneous element child");
            break;
      }
   
  }
  return;
}

  

  
