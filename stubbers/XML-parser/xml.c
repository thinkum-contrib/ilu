
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
#include <stdlib.h>
#include <assert.h> 
#include "doctree.h"

/***********************************************************************/
int main(int argc, char *argv[]) {

 Node *top_node;
 void *ctl;
 int   rtn;
 
 if(argc < 2) { 
     fprintf(stderr, "Missing filename");
     return 0;
   }

rtn = parse_xml(argv[1], &top_node, &ctl, NULL); 

if(top_node != NULL) { 
  fprintf(stdout, "\nDocument Tree\n"); 
  print_document_tree(top_node, 0, ctl); 
  fprintf(stdout, "\n"); 
}

if(!rtn) return 1; 

 return 0; 
}
