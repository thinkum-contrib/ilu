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
#include <iluptype.h>
#include "data.h"

extern list cvt_interfaces(void *, list ilup_top);
typedef char* NameCvt(char*);
extern void *initialize_convert( NameCvt);
extern void *build_basic_module_structure(Interface);
extern void print_instances(void *, FILE *outfile, list top);
extern void top_fwd_def_sort(void *);


/***********************************************************************/
/* temporary, for basic testing */
int main(int argc, char *argv[]) {

 list         ilup_top, cvt_top, ilup_tops; 
 void         *cvt_ctl;  /*used wholly within convert, for now */   
 void         *top_module;  /*used wholly within convert, for now */   
 listElement  *ptr, *ptr1; 
 int           i; 

 if(argc < 2) { 
     fprintf(stderr, "Missing filename");
     return 0;
   }


 ilup_tops = new_list();
 cvt_ctl = (void *)initialize_convert(NULL); 

 if(cvt_ctl == NULL) {
      fprintf(stderr,"System Error: iluptype convert");
      exit(1);
 }

 for (i = 1; i < argc; i ++) {
    if( (ilup_top = ParseFile(argv[i])) == NULL) {   
      fprintf(stderr,"Couldn't find or parse %s", argv[i]);  
      exit(1);
     }

   /* add new interfaces to collection */
    for(ptr = ilup_top->head; ptr; ptr = ptr->next) {
      for(ptr1 = ilup_tops->head; ptr1; ptr1 = ptr1->next) {
         if(ptr->data == ptr1->data) break;
      }
      if(ptr1 == NULL) list_insert(ilup_tops, (void *)ptr->data);
    }
  }  

  /* build module structures */
  for(ptr = ilup_tops->head; ptr != NULL; ptr = ptr->next) {
       top_module =  build_basic_module_structure((Interface)ptr->data);
       /* sort for forward defs only */
       top_fwd_def_sort(top_module);
  }  

 cvt_top = cvt_interfaces(cvt_ctl, ilup_top); 

 print_instances(cvt_ctl, stdout, cvt_top); 

 exit(0); 
  
}
