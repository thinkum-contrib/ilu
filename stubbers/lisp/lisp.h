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

/*
$Id: lisp.h,v 1.12 1999/08/03 01:50:11 janssen Exp $
*/

#include <stdio.h>
#include <string.h>

#include <iluptype.h>

/* temporary hack to avoid unresolved symbols */

#define AND		&&
#define OR		||
#define NOT		!

#define TRUE		1
#define FALSE		0

typedef struct context_s {
  Interface interface;
  FILE *file;
  Type class;
} * Context;

string c_type_name(Type t);
extern char *lisp_return_type(Type);
extern char *lisp_type_name(Type);
extern void OutputTypeName(Type type, Context context);
extern char *lisp_string(string s);
extern string lisp_argument_name(Argument);
extern string lisp_exception_name(Exception);
extern string lisp_procedure_name(Procedure);
extern string old_lisp_procedure_name(Procedure);
extern string lisp_interface_name(Interface);
extern string lisp_simple_name(Name);
extern string lisp_string(string);

typedef void (*EnumProc) (void *, void *);
typedef boolean (*FindProc) (void *, void *);

#define CLASS(f)	((f)->description->structuredDes.object)

char *lisp_constant_name(Constant);
void GenerateConstants (Interface, FILE *);
