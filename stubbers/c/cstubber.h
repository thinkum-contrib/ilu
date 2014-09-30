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
/* $Id: cstubber.h,v 1.41 1999/08/03 01:50:24 janssen Exp $ */
/* Last edited by Mike Spreitzer March 17, 1997 12:47 pm PST */

#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <ctype.h>

#if (defined(WIN32) && defined(_WINIO))
#include <winiodef.h>
extern int vfprintf_winio(FILE*, const char*, va_list);
/* defunct - extern int g_i_on_win32s; */
#endif /* (defined(WIN32) && defined(_WINIO)) */

#include <string.h>

#include <iluptype.h>

#define True TRUE
#define False FALSE

typedef void (*EnumProc) (void *, void *);
typedef boolean (*FindProc) (void *, void *);

#define Assert( e )	{if ( !( e )) \
	fatal( "Assertion <" # e "> failed in file %s line %d\n",__FILE__, __LINE__);}
#define CAlloc( n, t )	( t * ) iluparser_Malloc( n * sizeof( t ))
#define CRealloc( p, n, t ) ( t * ) iluparser_Realloc( p, n * sizeof( t ))

#define SegFault	*((char *)1)='a'
#define new_list	iluparser_new_list

typedef struct context_s {
    Interface 		interface;
    FILE 		*file;
    Type 		class;
    int			counter;
} *Context;

typedef enum {
  role_In, role_Out, role_InOut, role_Return,
  role_Exn,			/* exception parameter */
  role_Val,			/* plain value */
  role_InpRet			/* _T__Input return type */
}               Role;

extern Role     adRole[3];	/* maps ArgDirection to Role */

string          class_procedure_name(Procedure);
void            dedent(void);
void            emit(char *,...);
void            indent(void);
string          c_interface_name(Interface);
string          c_type_name(Type);
string          c_parameter_type(Type, ArgDirection);
string          c_role_type(Type type, Role role, boolean temp);
string          c_return_type(Type);
string		c_typecode_name(Type);
string          c_argument_name(Argument);
string          c_constant_name(Constant);
string          c_exception_name(Exception);
string          c_simple_name(Name);
string          c_procedure_name(Procedure);
string          c_string(string);
boolean         IsSequenceObject(Type, Interface);
boolean         IsPipeObject(Type, Interface);
boolean         TypeIsStruct(Type);
boolean         TypeIsArray(Type);
boolean         TypeIsString(Type);
boolean         TypeIsWString(Type);
boolean         TypeIsEitherString(Type);
boolean		TypeIsUnbrandedVariant(Type);	/* CORBA any */
boolean         TypeIsNonObjectStruct(Type);
boolean         VariableLength(Type);
boolean         NonStringSequence(Type);
boolean         Sliced(Type type, Role role);
boolean         HasFreeRoutine(Type);	/* has generated free routine */
boolean		NeedsFree(Type);	/* contains malloc'ed storage */
boolean         BlockingCall(Procedure);
boolean		matchString(refany str1, refany str2);
void            fatal(char *,...);
void            error(char *,...);
void            generateCommonCode(Interface, FILE *);
void            generateClientCode(Interface, FILE *);
void            generateServerCode(Interface, boolean, list, FILE *);
void            generateCHeaders(Interface, FILE *);
void            generateNecessaryIncludes(Context);
void            listArgumentTypes(refany arg, refany context);
void            setFile(FILE *);
void            clearMethodList(void);
boolean         methodInList(char *methodName);
void            addMethodToList(char *methodName);
Interface       OriginalInterface(Type);
Type            UltimateType(Type);
boolean         ReadSynonyms(char *filename);
boolean         IsCacheableMethod(Procedure m);
boolean         PassedByRef(Type type);
void            encodeArgument(Argument arg, Context context);

int             methodNdxOf(Procedure p, Type t);

boolean HasAllocRoutine(Type type);

extern cardinal MethodRecordID;
extern void     codeType(Type, char **, Role);
extern char    *interface_header_name(char *pc_interfacename);
extern char    *InterfaceHeaderTranslationFile;

extern void
generateProcDecl(Procedure m, Context c, boolean urname,
		 char *name_prefix, char *name_suffix);
/*
 * Writes the "<return type> <class_name>_<meth_name>(<args>)" part
 * of the C mapping of the given method.  If (urname), the named
 * class is the one that introduced the method; otherwise, it's
 * (c->class).
 */

extern void generateClassMakerDecl(Type class, Context context);
/* Writes "ILU_C_Class <name(class)>__MakeClass(...)". */

extern boolean	TypeIsPointer(Type type);
/*
 * Returns true if the C mapping of the given type is a pointer.
 * Note that the C mapping of an array is an "array object", which
 * is not a pointer (but implicitly converts to one when needed).
 */

extern boolean	TypeIsJustAlias (Type);
/*
 * Returns true if Type is just an alias; that is, if the alias type
 * and the original type have the same type ID.
 */

extern int      Indirectness(Type type, Role role, boolean temp, int ss);
/*
 * How many asterisks appear in this position in table 14.19?  Note
 * that more information than just indirectness is required to fully
 * characterize the array cells.  For slice-based array cells, we
 * count "slice*" as (ss), which can be either 0 or 1.
 */

extern void     MarshallValue(Context c, Type t, string val, int indent);
/*
 * Emit a statement, indented (indent) spaces, that outputs a given
 * value.  (val) is an assignment expression, of type
 * c_parameter_type(t, In).
 */

extern void     UnmarshallValue(Context c, Type t, unsigned long line,
		string lval, int indent);
/*
 * Emit a statement, indented (indent) spaces, that inputs a value
 * of the given type into the given lvalue.  (lval) is a cast
 * expression, of type c_parameter_type(t, InOut).
 */


extern void     SizeValue(Context, Type, string val);
/*
 * Emit an expression for the size computation for a value of the
 * given type.  (val) is an "assignment expression" (in terms of the
 * C grammar), and 0 levels of indirection away from the value; the
 * output is a "multiplicative expression".  The output expression
 * side-effects "_err"
 */

extern void
FreeValue(Type type, string val, Role role, Context context,
	  int indent);
/*
 * Emits a series of statements, with the given indentation, that
 * free the heap-allocated resources held by a given value.  Not the
 * storage for the value itself --- just the storage reached from
 * it.  (val) is a "cast expression" of type c_role_type(type, role,
 * TRUE).
 */

/********************************
When they are generated, the I/O procedures generated for type T
look like this:

void	<c_type_name(t)>__Free(<c_role_type(type, role_Exn, FALSE)> ref);

void	_<c_type_name(t)>__Output(ilu_Call c, <c_parameter_type(t,InOut)> v,
				  ilu_Error *e);

ilu_cardinal
	_<c_type_name(t)>__SizeOf(ilu_Call c, <c_parameter_type(t, InOut)> v,
				  ilu_Error *e);

<c_role_type(t, role_InpRet, FALSE)>
	_<c_type_name(t)>__Input(ilu_Call c,
				 <c_parameter_type(t, InOut)> ref,
				 ilu_Error *e);

The __Input routines are used in three places:

(1) Where a server unmarshalls arguments.  UnmarshallValue emits the code to do this.  That code always uses the parameter, not the return, of the __Input procedure.

(2) Where a client unmarshalls OUT/INOUT parameters and the return value.  This is handled by code in _ILU_C_GenericCall, which is parameterized by a single-letter characterization of the parameter type & mode.  The return of the __Input procedure is used where table 20 uses a double star in the "Out" column or a single star in the "Return" column.

(3) Where a client unmarshalls an exception parameter.  This is done by generic code in _ILU_C_CatchException, which is parameterized by a size (really a size, not a code) and a __Input procedure.  This code always uses the parameter, not the return, of the __Input procedure.

Here is the ILU version of table 20.
The (temp) rows show the type of the variable local to the true stub.
"string" refers to sequences of both short and normal characters.

DataType	role_In		role_InOut	role_Out	Return
--------	----		-----		----		------
scalar		type		type*		type*		type
(temp)		type		type		type		type

enum		type		type*		type*		type
(temp)		type		type		type		type

optional	type		type*		type*		type
(temp)		type		type		type		type

reference	type		type*		type*		type
(temp)		type		type		type		type

object		obtype		obtype*		obtype*		obtype
(temp)		obtype		obtype		obtype		obtype

struct, fixed	struct*		struct*		struct*		struct
(temp)		struct		struct		struct		struct

struct, var	struct*		struct*		struct**	struct*
(temp)		struct		struct		struct*		struct*

union, fixed	union*		union*		union*		union
(temp)		union		union		union		union

union, var	union*		union*		union**		union*
(temp)		union		union		union*		union*

string		string		string*		string*		string
(temp)		string		string		string		string

wstring		wstring		wstring*	wstring*	wstring
(temp)		wstring		wstring		wstring		wstring

ilustring	descr*		descr*		descr**		descr*
(temp)		descr		descr		descr*		descr*

fixedpoint	descr*		descr*		descr**		descr*
(temp)		descr		descr		descr*		descr*

sequence	descr*		descr*		descr**		descr*
(temp)		descr		descr		descr*		descr*

array, fixed	array		array		array		slice*
(temp)		array		array		array		slice*

array, var	array		array		slice**		slice*
(temp)		array		array		slice*		slice*

There are three more ILU-specific columns, for exception parameters, plain values, and InputFn returns:

Datatype	role_Exn	role_Val	role_InpRet
--------	-------		-----		--------
array		array*		array		slice*
string		string*		string		string*
wstring		wstring*	wstring		wstring*
other		type*		type		type*

********************************/


#define SINGLETON(Type)	(type_description(Type)->structuredDes.object->singleton)

#define AND		&&
#define OR		||
#define NOT		!
