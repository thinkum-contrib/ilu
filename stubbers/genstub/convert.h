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

#ifndef CONVERT_H_
#define CONVERT_H_

#include <stdio.h>
#include <assert.h>

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO) */

#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <string.h>
#include <setjmp.h>
#include <stdlib.h>

#include  "data.h"
#include "iluptype.h"


  typedef enum {interface_NonType, module_NonType, 
                 content_elem_NonType, method_NonType,
                 argument_NonType, exception_NonType, 
                 imported_NonType, enumfield_NonType,
                 constant_NonType, 
                 integer_constantval_NonType,
                 boolean_constantval_NonType,
                 real_constantval_NonType,
                 ilucstring_constantval_NonType,
                 enum_constantval_NonType,
                 integerliteral_NonType, arm_NonType, 
                 diminteger_NonType,
                 name_mbr_NonType, statembr_NonType,
                 langname_NonType,
                 invalid_NonType} NonTypeKind;  

  typedef struct { TypeKind typekind; char *ST_name;} MapTypeKind;
  typedef struct { NonTypeKind nontypekind; char *ST_name;} MapNonTypeKind;

  typedef struct {int thisindex; char *ST_name; int ST_index;} StMap; 

  typedef struct { void    *map_input; 
                   void    *map_target;
                   unsigned short int  larger; 
                   unsigned short int  smaller; 
                 } MapBTree;

#define MAX_PRINT_ITEMS 1000
  typedef  char * NameCvt(char *);  
  typedef struct {
                  StMap       type_map[100];
                  StMap       nontype_map[100];
                  unsigned    short int btree_alloc; 
                  unsigned    short int next_btree; 
                  MapBTree   *btree; 
                  int         current_toprint;
                  int         toprint_ct; 
                  int         convert_hyphens; 
                  Instance   *toprint[MAX_PRINT_ITEMS]; 
                  NameCvt    *name_convert_fn; 
                } CCtl;

#define INITIAL_BTREE_ALLOC 1000
typedef enum {cvt_error_system} CvtErrorType;

struct ilu_module_s {
       char   *simple_name;
       struct ilu_module_s  *containing_module; /* its containing module */
       list    scoping;    /* its containing module names */
       list    contained_modules;
       list    contained_types;
       list    contained_constants; 
       list    contained_exceptions;
       list    content_sequence; /* list of ContentElem */
       Type    module_object; /* if is CORBA interface containing other */
                              /* types, the equiv object entry */ 
       int     marked;
};

typedef struct ilu_module_s *Module;

struct ilu_content_elem_s { 
       char *content_type; /* must be fwd_type, type,  or module */ 
       /* only one of following set */
       void *content;
};

typedef struct ilu_content_elem_s *ContentElem;
         

#endif
