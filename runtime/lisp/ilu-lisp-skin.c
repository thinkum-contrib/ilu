/*
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

$Id: ilu-lisp-skin.c,v 1.28 1999/08/30 23:17:39 janssen Exp $
*/
/* Last edited by Mike Spreitzer September 15, 1998 11:14 am PDT */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>

#include <iluxport.h>
#include <iluvector.h>

#if defined(WIN32)

#if defined(ILU_BUILDING_RUNTIME)
/* we're actually building the runtime, so declare things as exported */
#define ILU_RUNTIME_PUBLIC        __declspec(dllexport) extern
#define ILU_RUNTIME_PUBLIC_CLASS  class __declspec(dllexport)

#else
/* we're must be building an app, so declare things as imported */
#define ILU_RUNTIME_PUBLIC       __declspec(dllimport) extern
#define ILU_RUNTIME_PUBLIC_CLASS class __declspec(dllimport)
#endif /* defined(ILU_BUILDING_RUNTIME) */

#else
/* we're not on win32 */
#define ILU_RUNTIME_PUBLIC extern
#define ILU_RUNTIME_PUBLIC_CLASS class

#endif /* defined(WIN32) */


#define OPTCHECK(a)	((opt && !provided)?((ilu_cardinal)0):(a))

typedef struct {
  ilu_bytes data;
  ilu_cardinal len;
} ByteVector;

typedef struct {
  ilu_character *data;
  ilu_cardinal len;
} UnicodeVector;

ILU_RUNTIME_PUBLIC int ilulisp_can_be_null (ilu_Class c)
{
  return (c->cl_optional);
}

ILU_RUNTIME_PUBLIC void ilulisp_bytencopy (unsigned char *to, unsigned char *from,
			unsigned int count)
{
  if (to != NULL && from != NULL && count > 0)
    memmove (to, from, count);
}

ILU_RUNTIME_PUBLIC ilu_string ilulisp_Strdup (ilu_string str)
{
  ilu_string p;

  if (str == NULL)
    return (NULL);
  else if ((p = ilu_must_malloc(strlen(str) + 1)) == NULL)
    return (NULL);
  else
    {
      strcpy(p, str);
      return(p);
    }
}

ILU_RUNTIME_PUBLIC long ilulisp_InputInteger (ilu_Call call, ilu_Error *_err)
{
  ilu_integer i;
  ilu_InputInteger (call, &i, _err);
  return (i);
}

ILU_RUNTIME_PUBLIC int ilulisp_InputBoolean (ilu_Call call, ilu_Error *_err)
{
  ilu_boolean i;
  ilu_InputBoolean (call, &i, _err);
  return (i);
}

ILU_RUNTIME_PUBLIC unsigned long ilulisp_InputCardinal (ilu_Call call, ilu_Error *_err)
{
  ilu_cardinal i;
  ilu_InputCardinal (call, &i, _err);
  return (i);
}

ILU_RUNTIME_PUBLIC short ilulisp_InputShortInteger (ilu_Call call, ilu_Error *_err)
{
  short i;
  ilu_InputShortInteger (call, &i, _err);
  return (i);
}

ILU_RUNTIME_PUBLIC unsigned short ilulisp_InputShortCardinal (ilu_Call call,
						      ilu_Error *_err)
{
  unsigned short i;
  ilu_InputShortCardinal (call, &i, _err);
  return (i);
}

ILU_RUNTIME_PUBLIC unsigned short ilulisp_InputEnumCode (ilu_Call call, ilu_Type the_type, ilu_Error *_err)
{
  unsigned short i;
  ilu_InputEnum (call, &i, the_type, _err);
  return (i);
}

ILU_RUNTIME_PUBLIC unsigned int ilulisp_InputOptional (ilu_Call call, ilu_Type the_type, ilu_Error *_err)
{
  ilu_boolean b;
  ilu_InputOptional (call, &b, the_type, _err);
  return (b ? 1 : 0);
}

ILU_RUNTIME_PUBLIC double ilulisp_InputReal (ilu_Call call, ilu_Error *_err)
{
  double f;
  ilu_InputReal (call, &f, _err);
  return (f);
}

ILU_RUNTIME_PUBLIC ByteVector *ilulisp_InputLongReal (ilu_Call call, ilu_Error *_err)
{
  ilu_longreal f;
  ByteVector *bv = (ByteVector *) ilu_must_malloc(sizeof(ByteVector));

  bv->len = 16;
  bv->data = (ilu_bytes) ilu_must_malloc(bv->len);
  ilu_InputLongReal (call, &f, _err);
  memmove (bv->data, &f, 16);
  return bv;
}

ILU_RUNTIME_PUBLIC int ilulisp_OutputLongReal (ilu_Call call, ByteVector *bv,
				       ilu_Error *_err)
{
  ilu_longreal f;

  memmove (&f, bv->data, 16);
  ilu_OutputLongReal (call, f, _err);
  return 1;
}

ILU_RUNTIME_PUBLIC int ilulisp_SizeOfLongReal (ilu_Call call, ByteVector *bv,
				       ilu_Error *_err)
{
  return (int) ilu_SizeOfLongReal (call, *((ilu_longreal *) (&bv->data)), _err);
}

ILU_RUNTIME_PUBLIC ilu_boolean ilulisp_OutputLongCardinal (ilu_Call call,
						   unsigned long int ms32,
						   unsigned long int ls32,
						   ilu_Error *_err)
{
  ilu_longcardinal l;

#ifdef LONG_CARDINAL_TYPE
  l = (((LONG_CARDINAL_TYPE)ms32)<<32)|ls32;
#else
  l.high = ms32;
  l.low = ls32;
#endif
  ilu_OutputLongCardinal (call, l, _err);
  return 1;
}

ILU_RUNTIME_PUBLIC int ilulisp_SizeOfLongCardinal (ilu_Call call,
					   unsigned long int ms32,
					   unsigned long int ls32,
					   ilu_Error *_err)
{
  ilu_longcardinal l;

#ifdef LONG_CARDINAL_TYPE
  l = (((LONG_CARDINAL_TYPE)ms32)<<32)|ls32;
#else
  l.high = ms32;
  l.low = ls32;
#endif
  return (ilu_SizeOfLongCardinal (call, l, _err));
}

ILU_RUNTIME_PUBLIC ilu_boolean ilulisp_InputLongCardinal (ilu_Call call,
						  unsigned long int *ms32,
						  unsigned long int *ls32,
						  ilu_Error *_err)
{
  ilu_longcardinal l;
  ilu_InputLongCardinal (call, &l, _err);
#ifdef LONG_CARDINAL_TYPE
  *ls32 = l & 0xffffffff;
  *ms32 = (l >> 32);
#else
  *ms32 = l.high;
  *ls32 = l.low;
#endif
  return 1;
}

ILU_RUNTIME_PUBLIC ilu_boolean ilulisp_OutputLongInteger (ilu_Call call, long int ms32,
						  unsigned long int ls32,
						  ilu_Error *_err)
{
  ilu_longinteger l;

#ifdef LONG_INTEGER_TYPE
  l = (((LONG_CARDINAL_TYPE)ms32)<<32)|ls32;
#else
  l.high = ms32;
  l.low = ls32;
#endif
  ilu_OutputLongInteger (call, l, _err);
  return 1;
}

ILU_RUNTIME_PUBLIC int ilulisp_SizeOfLongInteger (ilu_Call call, long int ms32,
					  unsigned long int ls32,
					  ilu_Error *_err)
{
  ilu_longinteger l;

#ifdef LONG_INTEGER_TYPE
  l = (((LONG_CARDINAL_TYPE)ms32)<<32)|ls32;
#else
  l.high = ms32;
  l.low = ls32;
#endif
  return (ilu_SizeOfLongInteger (call, l, _err));
}

ILU_RUNTIME_PUBLIC ilu_boolean ilulisp_InputLongInteger (ilu_Call call, long int *ms32,
						 unsigned long int *ls32,
						 ilu_Error *_err)
{
  ilu_longinteger l;
  ilu_InputLongInteger (call, &l, _err);

#ifdef LONG_INTEGER_TYPE
  *ls32 = l & 0xffffffff;
  *ms32 = (l >> 32);
#else
  *ms32 = l.high;
  *ls32 = l.low;
#endif
  return 1;
}

/* for use if lisp understands ANSI parameter passing */
ILU_RUNTIME_PUBLIC float ilulisp_ANSIInputShortReal (ilu_Call call, ilu_Error *_err)
{
  float f;
  ilu_InputShortReal (call, &f, _err);
  return (f);
}

/* the next 3 for use if lisp doesn't understand ANSI parameter passing */
ILU_RUNTIME_PUBLIC double ilulisp_KandRInputShortReal (ilu_Call call, ilu_Error *_err)
{
  float f;
  ilu_InputShortReal (call, &f, _err);
  return ((double) f);
}

ILU_RUNTIME_PUBLIC void ilulisp_KandROutputShortReal (ilu_Call call, double r,
					      ilu_Error *_err)
{
  float f = (float)r;
  ilu_OutputShortReal (call, f, _err);
}

ILU_RUNTIME_PUBLIC int ilulisp_KandRSizeOfShortReal (ilu_Call call, double r,
					     ilu_Error *_err)
{
  float f = (float)r;
  return (ilu_SizeOfShortReal (call, f, _err));
}

ILU_RUNTIME_PUBLIC unsigned char ilulisp_InputByte (ilu_Call call, ilu_Error *_err)
{
  unsigned char b;
  ilu_InputByte (call, &b, _err);
  return (b);
}

/* short character sequence functions *****************************************/

ILU_RUNTIME_PUBLIC char * ilulisp_InputString (ilu_Call call, ilu_cardinal limit,
				       ilu_Error *_err)
{
  char *p = NULL;
  ilu_cardinal i;

  ilu_InputString (call, &p, &i, limit, _err);
  return (p);
}

ILU_RUNTIME_PUBLIC void ilulisp_OutputString (ilu_Call call, ilu_string string,
				      ilu_cardinal len, ilu_cardinal limit,
				      ilu_boolean opt, ilu_boolean provided,
				      ilu_Error *_err)
{
  ilu_OutputString (call, OPTCHECK(string), OPTCHECK(len), limit, _err);
}

ILU_RUNTIME_PUBLIC ilu_cardinal ilulisp_SizeOfString (ilu_Call call, ilu_string string,
					      ilu_cardinal len,
					      ilu_cardinal limit,
					      ilu_boolean opt,
					      ilu_boolean provided,
					      ilu_Error *_err)
{
  return ilu_SizeOfString (call, OPTCHECK(string), OPTCHECK(len), limit, _err);
}

/* short character vector functions *****************************************/

ILU_RUNTIME_PUBLIC char *ilulisp_InputCharacterVector (ilu_Call call,
					       long unsigned int len,
					       ilu_Error *_err)
{
  char *p;

  ilu_InputStringVec (call, &p, len, _err);
  return (p);
}

ILU_RUNTIME_PUBLIC void ilulisp_OutputStringVec (ilu_Call call, ilu_string string,
					 ilu_cardinal len, ilu_Error *_err)
{
  ilu_OutputStringVec (call, string, len, _err);
}

ILU_RUNTIME_PUBLIC ilu_cardinal ilulisp_SizeOfStringVec (ilu_Call call,
						 ilu_string string,
						 ilu_cardinal len,
						 ilu_Error *_err)
{
  return ilu_SizeOfStringVec (call, string, len, _err);
}

/* byte vector functions *****************************************/

ILU_RUNTIME_PUBLIC ByteVector *ilulisp_InputByteVector (ilu_Call call,
						long unsigned int len,
						ilu_Error *_err)
{
  ByteVector *bv = (ByteVector *) ilu_must_malloc(sizeof(ByteVector));
  bv->data = NULL;
  bv->len = len;
  ilu_InputOpaque (call, &bv->data, len, _err);
  return (bv);
}

ILU_RUNTIME_PUBLIC void ilulisp_OutputByteVector(ilu_Call call, ByteVector *bv,
					 long unsigned int len, ilu_Error *_err)
{
  ilu_OutputOpaque (call, bv->data, len, _err);
}

ILU_RUNTIME_PUBLIC ilu_cardinal ilulisp_SizeOfByteVector (ilu_Call call, ByteVector *bv,
						  long unsigned int len,
						  ilu_Error *_err)
{
  return ilu_SizeOfOpaque (call, bv->data, len, _err);
}

/* byte sequence functions *****************************************/

ILU_RUNTIME_PUBLIC ByteVector *ilulisp_InputByteSequence (ilu_Call call,
						  long unsigned int limit,
						  ilu_Error *_err)
{
  ByteVector *bv = (ByteVector *) ilu_must_malloc(sizeof(ByteVector));
  bv->len = 0;
  bv->data = NULL;
  ilu_InputBytes (call, &bv->data, &bv->len, limit, _err);
  return (bv);
}

ILU_RUNTIME_PUBLIC int ilulisp_OutputByteSequence (ilu_Call call, ByteVector *bv,
					   long unsigned int limit,
					   ilu_Error *_err)
{
  ilu_OutputBytes (call, bv->data, bv->len, limit, _err);
  return 1;
}

ILU_RUNTIME_PUBLIC ilu_cardinal ilulisp_SizeOfByteSequence (ilu_Call call,
						    ByteVector *bv,
						    long unsigned int limit,
						    ilu_Error *_err)
{
  return ilu_SizeOfBytes (call, bv->data, bv->len, limit, _err);
}

/* wide character sequence functions *****************************************/

ILU_RUNTIME_PUBLIC UnicodeVector * ilulisp_InputWString (ilu_Call call,
						 ilu_cardinal limit,
						 ilu_Error *_err)
{
  UnicodeVector *uv = (UnicodeVector *) ilu_must_malloc(sizeof(UnicodeVector));
  uv->data = NULL;
  uv->len = 0;
  ilu_InputWString (call, &uv->data, &uv->len, limit, _err);
  return(uv);
}

ILU_RUNTIME_PUBLIC int ilulisp_OutputWString (ilu_Call call, UnicodeVector *string,
				      ilu_cardinal limit, ilu_Error *_err)
{
  ilu_OutputWString (call, string->data, string->len, limit, _err);
  return 1;
}

ILU_RUNTIME_PUBLIC ilu_cardinal ilulisp_SizeOfWString (ilu_Call call,
					       UnicodeVector *string,
					       ilu_cardinal limit,
					       ilu_Error *_err)
{
  return ilu_SizeOfWString (call, string->data, string->len, limit, _err);
}

/* wide character vector functions *****************************************/

ILU_RUNTIME_PUBLIC UnicodeVector * ilulisp_InputWStringVec (ilu_Call call,
						    ilu_cardinal len,
						    ilu_Error *_err)
{
  UnicodeVector *uv = (UnicodeVector *) ilu_must_malloc(sizeof(UnicodeVector));
  uv->len = len;
  uv->data = (unsigned short *) ilu_must_malloc(len * sizeof(unsigned short));
  ilu_InputWStringVec (call, &uv->data, len, _err);
  return (uv);
}

ILU_RUNTIME_PUBLIC int ilulisp_OutputWStringVec (ilu_Call call, UnicodeVector *uv,
					 ilu_cardinal len, ilu_Error *_err)
{
  ilu_OutputWStringVec (call, uv->data, len, _err);
  return 1;
}

ILU_RUNTIME_PUBLIC ilu_cardinal ilulisp_SizeOfWStringVec (ilu_Call call,
						  UnicodeVector *uv,
						  ilu_cardinal len,
						  ilu_Error *_err)
{
  return ilu_SizeOfWStringVec (call, uv->data, len, _err);
}

/* structured type functions *****************************************/

ILU_RUNTIME_PUBLIC ilu_integer ilulisp_InputSequence (ilu_Call call, ilu_boolean opt,
						      ilu_cardinal limit,
						      ilu_Type the_type,
						      ilu_Error *_err)
{
  ilu_cardinal l;

  ilu_InputSequence (call, &l, limit, the_type, _err);
  return (ilu_integer) l;
}

ILU_RUNTIME_PUBLIC int ilulisp_InputUnion (ilu_Call call, ilu_TypeKind tk,
					   ilu_Type the_type,
					   ilu_Error *_err)
{
  ilu_cardinal d;

  ilu_InputUnion (call, &d, tk, the_type, _err);
  return (int) d;
}

/* Input object ID functions *****************************************/

ILU_RUNTIME_PUBLIC ilu_Object ilulisp_InputObjectID (ilu_Call call,
					     ilu_refany *classrec,
					     ilu_Class pclass, int disc,
					     ilu_Error *_err)
{
  ilu_Object p = ILU_NIL;

  if (disc && pclass->cl_singleton)
    p = ilu_GetCallSingleton(call, _err);
  else
    ilu_InputObjectID (call, &p, disc, pclass, _err);

  if (p == NULL)
    {
      if (disc)
	fprintf (stderr, "ilu-lisp-skin.c:ilulisp_InputObjectID:  ******** Couldn't input discriminator of type %s (%s)\n",
		 pclass->cl_name, pclass->cl_unique_id);
      else
	fprintf (stderr, "ilu-lisp-skin.c:ilulisp_InputObjectID:  ******** Couldn't input object with pclass %s (%s)\n",
		 pclass->cl_name, pclass->cl_unique_id);
    }
  else
    *classrec = ilu_ClassOfObject(p);
  return (p);
}

ILU_RUNTIME_PUBLIC ilu_string ilulisp_IDOfClass (ilu_Class c)
{
  return (c->cl_unique_id);
}

ILU_RUNTIME_PUBLIC ilu_Connection ilulisp_HandleNewConnection (ilu_Port p)
{
  ilu_boolean closed = ilu_FALSE;
  ilu_Connection c;
  ilu_Error lerr;

  c = ilu_HandleNewConnection (p, &closed, &lerr);
  if (ILU_ERRNOK(lerr)) {
    ILU_HANDLED(lerr);
    return NULL;
  } else if (!closed)
    return c;
  else
    return NULL;
}

ILU_RUNTIME_PUBLIC ilu_cardinal ilulisp_IDOfMethod (ilu_Method m)
{
  return (m->me_id);
}


ILU_RUNTIME_PUBLIC ilu_TransportInfo
ilulisp_CreateTransportInfo(unsigned num_strings,
			    unsigned tot_str_len, /* not counting '\0' */
			    ilu_string first_str,
			    ilu_Error *_err)
{
   /*
    * struct t_info_s {
    *   ilu_string array[];   null terminated array of string pointers
    *   ilu_string next_str;  used internally to store start of next string
    *   char strings[];       strings are densely stored here
    * }
    */
   ilu_TransportInfo info =
      ilu_MallocE(sizeof(ilu_string) * (num_strings + 2)
		  + tot_str_len + num_strings, _err);
   if (ILU_ERROK(*_err)) {
      char **ptr_next_str = info + num_strings + 1;
      char *strings = (char *)(ptr_next_str + 1);
      memset(info + 1, 0, sizeof(ilu_TransportInfo) * num_strings);
      info[0] = strings;
      strcpy(strings, first_str);
      *ptr_next_str = strings + strlen(first_str) + 1;
   }
   return info;
}

ILU_RUNTIME_PUBLIC void
ilulisp_AddTransportInfo(ilu_TransportInfo info,
			 unsigned num_strings,
			 unsigned index,
			 ilu_string next_str)
{
   char **ptr_next_str = info + num_strings + 1;
   info[index] = *ptr_next_str;
   strcpy(*ptr_next_str, next_str);
   *ptr_next_str += strlen(next_str) + 1;
}

ILU_RUNTIME_PUBLIC ilu_TransportInfo
  ilulisp_GetDefaultTinfo (ilu_Error *err)
{
  ilu_TransportInfo t2;
  t2 = ilu_CopyTinfo(ilu_DefaultTransportInfo(), err);
  if (ILU_ERROK(*err))
    return t2;
  else
    return ILU_NIL;
}

ILU_RUNTIME_PUBLIC int ilulisp_TinfoLength (ilu_TransportInfo *tinfo)
{
  /* returns tinfo parts as a single string with each piece separated from the next with whitespace */
  int i;
  for (i = 0;  tinfo[i] != ILU_NIL;  i++)
    ;
  return i;
}

ILU_RUNTIME_PUBLIC ilu_string ilulisp_TinfoElement (ilu_TransportInfo tinfo, int n, ilu_Error *err)
{
  int i;
  char *newstring;

  for (i = 0;  tinfo[i] != ILU_NIL;  i++)
    ;
  if (n >= i)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_badTinfoIndex, ILU_NIL);
  newstring = ilu_StrdupE(tinfo[n], err);
  return newstring;
}

ILU_RUNTIME_PUBLIC ilu_Call ilulisp_CreateCallStruct(ilu_Error *_err)
{
  return ilu_MallocE(sizeof(ilu_Call_s), _err);
}

ILU_RUNTIME_PUBLIC void ilulisp_SetFailureActions (int checkfailure, int assertfailure, int memfailure)
{
  ilu_SetCheckFailureAction(checkfailure);
  ilu_SetAssertionFailureAction(assertfailure);
  ilu_SetMemFailureAction(memfailure);
}

ILU_RUNTIME_PUBLIC void ilulisp_FreeCStruct(void *cstruct)
{
   ilu_free(cstruct);
}

/* same as ilu_GetReply, except that retry errors are signalled
   through the retry boolean */
/**Before: Call-Locking(call, OHi);
    After: Call-Invariant(call, err) &&
	   (success => Call-Locking(call, IHi)) &&
	   (transient/retry => Call-Locking(call, OHi))*/
ILU_RUNTIME_PUBLIC      ilu_ProtocolException
ilulisp_GetReply(ilu_Call call, ilu_cardinal * errorStatus,
		 ilu_boolean *retry,
		 ilu_Connection * new_conn,
		 ILU_ERRS((bad_locks, IoErrs)) *err)
{
  ilu_Error lerr;
  ilu_ProtocolException except;

  except = ilu_GetReply (call, errorStatus, new_conn, &lerr);
  if (ILU_ERRNOK(lerr) &&
      (lerr.ilu_type == ILU_ERRTYP(transient)) &&
      (ILU_ERRSEL(transient,lerr).minor == ilu_tm_retry)) {
    ILU_HANDLED(lerr);
    ILU_CLER(*err);
    *retry = ilu_TRUE;
  } else {
    *err = lerr;
    *retry = ilu_FALSE;
  };  
  return except;
}

/* same as ilu_ReceiveRequest but provides method name, type name, and interface name,
   instead of method struct */
ILU_RUNTIME_PUBLIC int
ilulisp_ReceiveRequest(ilu_Call call, ilu_boolean *initted, ilu_Connection conn,
		       ilu_Class *intro_type, ilu_string *method_name, ilu_string *type_name,
		       ilu_cardinal *sn, ilu_Error *_err)
{
  ilu_Method method;
  int stat = ilu_ReceiveRequest(call, initted, conn, intro_type,
				&method, sn, _err);
  if (ILU_ERROK(*_err) && stat == ilu_RcvReqStat_request) {
     *method_name = method->me_name;
     *type_name = (*intro_type)->cl_name;
  }
  else {
     *method_name = 0;
     *type_name = 0;
  }
  return stat;
}

ILU_RUNTIME_PUBLIC ByteVector * ilulisp_ConvertByteVectorToC (ilu_bytes ptr, int len)
{
  ByteVector *p = (ByteVector *) ilu_must_malloc(sizeof(ByteVector));
  p->data = (ilu_bytes) ilu_must_malloc(p->len = len);
  memcpy ((void *) p->data, (const void *) ptr, len);
  return (p);
}

ILU_RUNTIME_PUBLIC void ilulisp_CopyByteVectorToLisp (ByteVector *uv, ilu_bytes lv)
{
  memcpy ((void *) lv, (const void *) uv->data, uv->len);
}

ILU_RUNTIME_PUBLIC unsigned int ilulisp_ByteVectorSize (ByteVector *bv)
{
  if (bv != NULL)
    return (bv->len);
}

ILU_RUNTIME_PUBLIC void ilulisp_FreeByteVector (ByteVector *bv)
{
  if (bv != NULL)
    {
      if (bv->data != NULL)
	ilu_free(bv->data);
      ilu_free(bv);
    }
}

ILU_RUNTIME_PUBLIC UnicodeVector * ilulisp_ConvertUnicodeVectorToC (unsigned short *ptr,
							    int len)
{
  register int i;

  UnicodeVector *p = (UnicodeVector *) ilu_must_malloc(sizeof(UnicodeVector));
  p->data = ilu_must_malloc(len * sizeof(unsigned short int));
  p->len = len;
  for (i = 0;  i < len;  i++)
    p->data[i] = ptr[i];
  return (p);
}

ILU_RUNTIME_PUBLIC void ilulisp_CopyUnicodeVectorToLisp (UnicodeVector *uv,
						 unsigned short int *lv)
{
  register unsigned int i;

  for (i = 0;  i < uv->len;  i++)
    lv[i] = uv->data[i];
}

ILU_RUNTIME_PUBLIC unsigned int ilulisp_UnicodeVectorSize (ByteVector *bv)
{
  if (bv != NULL)
    return (bv->len);
}

ILU_RUNTIME_PUBLIC void ilulisp_FreeUnicodeVector (UnicodeVector *bv)
{
  if (bv != NULL)
    {
      if (bv->data != NULL)
	ilu_free(bv->data);
      ilu_free(bv);
    }
}

ILU_RUNTIME_PUBLIC void * ilulisp_StartSuperclassVector (int nClasses,
						 ilu_string firstClass)
{
  char **tvec = ILU_NIL;
  int i;

  if (nClasses > 0)
    {
      tvec = (char **) ilu_must_malloc(nClasses * sizeof(ilu_string));
      for (i = 0;  i < nClasses;  i++)
	tvec[i] = ILU_NIL;
      if (firstClass != ILU_NIL)
	tvec[0] = firstClass;
    }
  return (void *) tvec;
}

ILU_RUNTIME_PUBLIC void ilulisp_AddSuperclassToVector (char **tvec, int nClasses,
					       int whichClass,
					       ilu_string classID)
{
  if (tvec != ILU_NIL && nClasses > 0 && whichClass >= 0
      && whichClass < nClasses)
    {
      tvec[whichClass] = classID;
    }
}

ILU_RUNTIME_PUBLIC void * ilulisp_ConsExceptionVector (int count)
{
  if (count == 0)
    return (void *) 0;
  else
    return (void *) ilu_must_malloc(sizeof(ilu_Exception) * count);
}

ILU_RUNTIME_PUBLIC void ilulisp_SetExceptionVector (ilu_Exception *vec, int i,
					            char *rep_id)
{
  vec[i] = (ilu_Exception) rep_id;
}

ILU_RUNTIME_PUBLIC ilu_string ilulisp_GetAuthenticationAsString (ilu_Call call)
{
/*
 *  static ilu_AuthenticationScheme SunRPCUnixAS = NULL;
 *
 *  if (SunRPCUnixAS == NULL)
 *    SunRPCUnixAS = ilu_FindAuthenticationSchemeByName ("sunrpc-unix");
 *  if (SunRPCUnixAS != NULL)
 *    {
 *      ilu_Passport passport = ilu_CallerPassportOfCall(call);
 *      if (passport != NULL)
 *	{
 *	  ilu_Principal p;
 *	  ilu_Error err = NULL;
 *	  char idbuffer[1000];
 *
 *	  p = ilu_GetPrincipal(passport, SunRPCUnixAS, &err);
 *	  if (err == NULL)
 *	    {
 *	      ilu_AS_SunRPCUnix_UserID id = NULL;
 *
 *	      id = (ilu_AS_SunRPCUnix_UserID) p.ap_user_id;
 *	      sprintf (idbuffer, "sunrpc-unix:%d,%d@%s", id->au_UID,
 *		       id->au_GID, id->au_hostname);
 *	      free(id);
 *	      return (ilulisp_Strdup(idbuffer));
 *	    }
 *	  else
 *	    {
 *	      fprintf (stderr, "** Attempt to GetPrincipal signalled <%s>\n",
 *		       ILU_ERR_NAME(err));
 *	      ilu_FreeError (err);
 *	    }
 *	}
 *    }
*/
  return (ilulisp_Strdup(""));
}

ILU_RUNTIME_PUBLIC ilu_Class ilulisp_getRootClass ()
{
  return ilu_rootClass;
}

ILU_RUNTIME_PUBLIC ilu_string ilulisp_FormatError (ilu_Error err)
{
  static const char errfmt[] = "<ILU-kernel-error `%s' (%s)>";

  ilu_string newstring =
     (ilu_string) ilu_must_malloc(strlen(ILU_ERR_NAME(err)
					 + strlen(ILU_ERR_DESCRIPTION(err))
					 + sizeof(errfmt) - 4));
  sprintf (newstring, errfmt, ILU_ERR_NAME(err), ILU_ERR_DESCRIPTION(err));
  return (newstring);
}

ILU_RUNTIME_PUBLIC void ilulisp_Acquire_otmu (void)
{
  ilu_AcquireMutex(ilu_GetOTMutex());
}

ILU_RUNTIME_PUBLIC void ilulisp_Release_otmu (void)
{
  ilu_ReleaseMutex(ilu_GetOTMutex());
}

#include <version.h>

ILU_RUNTIME_PUBLIC ilu_string ilulisp_GetLispRuntimeVersion()
{
  return ILU_VERSION_STRING;
}

ILU_RUNTIME_PUBLIC ilu_string ilulisp_GetTypeUID (ilu_string name)
{
  if (strcmp(name, "ilu.CString") == 0)
    return ((ilu_string) &ilu_TypeID_ilu_CString[0]);
  else if (strcmp(name, "short cardinal") == 0)
    return ((ilu_string) &ilu_TypeID_ilu_shortcardinal[0]);
  else if (strcmp(name, "cardinal") == 0)
    return ((ilu_string) &ilu_TypeID_ilu_cardinal[0]);
  else if (strcmp(name, "long cardinal") == 0)
    return ((ilu_string) &ilu_TypeID_ilu_longcardinal[0]);
  else if (strcmp(name, "short integer") == 0)
    return ((ilu_string) &ilu_TypeID_ilu_shortinteger[0]);
  else if (strcmp(name, "integer") == 0)
    return ((ilu_string) &ilu_TypeID_ilu_integer[0]);
  else if (strcmp(name, "long integer") == 0)
    return ((ilu_string) &ilu_TypeID_ilu_longinteger[0]);
  else if (strcmp(name, "short real") == 0)
    return ((ilu_string) &ilu_TypeID_ilu_shortreal[0]);
  else if (strcmp(name, "real") == 0)
    return ((ilu_string) &ilu_TypeID_ilu_real[0]);
  else if (strcmp(name, "long real") == 0)
    return ((ilu_string) &ilu_TypeID_ilu_longreal[0]);
  else if (strcmp(name, "short character") == 0)
    return ((ilu_string) &ilu_TypeID_ilu_shortcharacter[0]);
  else if (strcmp(name, "character") == 0)
    return ((ilu_string) &ilu_TypeID_ilu_character[0]);
  else if (strcmp(name, "byte") == 0)
    return ((ilu_string) &ilu_TypeID_ilu_byte[0]);
  else if (strcmp(name, "boolean") == 0)
    return ((ilu_string) &ilu_TypeID_ilu_boolean[0]);
#ifdef ADD_VARIANT_SUPPORT
  else if (strcmp(name, "pickle") == 0)
    return ((ilu_string) &ilu_TypeID_ilu_pickle[0]);
#endif
  else
    return 0;
}

#ifdef ADD_VARIANT_SUPPORT

ILU_RUNTIME_PUBLIC ilu_Call
  ilulisp_StartPickle (ilu_Error *err)
{
  ilu_Call c = ilu_MallocE(sizeof(*c), err);
  if (ILU_ERRNOK(*err)) return ILU_NIL;
  ilu_StartPickle (c, ILU_NIL, err);
  if (ILU_ERROK(*err)) return c; else return ILU_NIL;
}

ILU_RUNTIME_PUBLIC void
  ilulisp_ReadPickle (ilu_Call call, ByteVector *bytes, ilu_Error *err)
{
  ilu_Pickle p;
  p.pi_len = bytes->len;
  p.pi_bytes = ilu_MallocE(bytes->len, err);
  if (ILU_ERRNOK(*err)) return;
  memcpy (p.pi_bytes, bytes->data, bytes->len);
  (void) ilu_ReadPickle(call, p, err);
}

ILU_RUNTIME_PUBLIC ilu_string
  ilulisp_PickleTypeUID (ByteVector *bv, ilu_Error *err)
{
  ilu_Pickle p;
  ilu_string s;
  p.pi_bytes = bv->data;
  p.pi_len = bv->len;
  s = ilu_PickleType(p, err);
  if (ILU_ERRNOK(*err)) return 0;
  return ilu_StrdupE(s, err);
}

ILU_RUNTIME_PUBLIC ByteVector *
  ilulisp_EndFormPickle (ilu_Call call, ilu_Error *err)
{
  ilu_Pickle p;
  ByteVector *bv = ilu_MallocE(sizeof(ByteVector), err);
  if (ILU_ERRNOK(*err)) return ILU_NIL;
  p.pi_bytes = ILU_NIL;
  p.pi_len = 0;
  if (!ilu_EndPickle(call, &p, err)) {
    ilu_free(bv);
    return ILU_NIL;
  };
  bv->data = p.pi_bytes;
  bv->len = p.pi_len;
  ilu_free(call);
  return bv;
}

ILU_RUNTIME_PUBLIC void
  ilulisp_EndDecodePickle (ilu_Call call, ilu_Error *err)
{
  ilu_EndPickle(call, ILU_NIL, err);
  ilu_free(call);
}

ILU_RUNTIME_PUBLIC void
    ilulisp_OutputPickle (ilu_Call call, ByteVector *bytes, ilu_Error *err)
{
  ilu_Pickle p;
  memset(&p, 0, sizeof(p));
  p.pi_len = bytes->len;
  p.pi_bytes = bytes->data;
  ilu_OutputPickle(call, p, ILU_NIL, err);
}

ILU_RUNTIME_PUBLIC int
    ilulisp_SizeOfPickle (ilu_Call call, ByteVector *bytes, ilu_Error *err)
{
  ilu_Pickle p;
  memset(&p, 0, sizeof(p));
  p.pi_len = bytes->len;
  p.pi_bytes = bytes->data;
  return (ilu_SizeOfPickle(call, p, ILU_NIL, err));
}

ILU_RUNTIME_PUBLIC ByteVector *
    ilulisp_InputPickle (ilu_Call call, ilu_Error *err)
{
  ilu_Pickle p;
  ByteVector *bv;
  bv = ilu_MallocE(sizeof(*bv), err);
  if (ILU_ERRNOK(*err)) return ILU_NIL;
  memset(&p, 0, sizeof(p));
  ilu_InputPickle (call, &p, ILU_NIL, err);
  bv->data = p.pi_bytes;
  bv->len = p.pi_len;
  return bv;
}

#ifdef IIOP_PROTOCOL

ILU_RUNTIME_PUBLIC ilu_cardinal *
  ilulisp_MakeDimVector (int nelements, ilu_Error *err)
{
  ilu_cardinal *p = (ilu_cardinal *) ilu_MallocE(sizeof(ilu_cardinal) * nelements, err);
  return p;
}

ILU_RUNTIME_PUBLIC void
  ilulisp_SetDimVectorElement (ilu_cardinal *dim_vector, int dim, ilu_cardinal value)
{
  dim_vector[dim] = value;
}

ILU_RUNTIME_PUBLIC void
  ilulisp_RegisterUnionArmValue (ilu_UnionArm arm,
				 ilu_cardinal index,
				 int type_int,
				 ilu_cardinal c_val,
				 ilu_integer i_val,
				 ilu_boolean b_val,
				 ilu_string s_val,
				 ilu_Error *err)
{
  ilu_ConstantValue_s cv;

  cv.kind = type_int;
  switch (cv.kind) {
  case ilu_byte_cvk:
    cv.value.byte_val = (ilu_byte) c_val;
    break;
  case ilu_shortcardinal_cvk:
    cv.value.shortcardinal_val = (ilu_shortcardinal) c_val;
    break;
  case ilu_cardinal_cvk:
    cv.value.cardinal_val = c_val;
    break;
  case ilu_shortinteger_cvk:
    cv.value.shortinteger_val = (ilu_shortinteger) i_val;
    break;
  case ilu_integer_cvk:
    cv.value.integer_val = i_val;
    break;
  case ilu_boolean_cvk:
    cv.value.boolean_val = b_val;
    break;
  case ilu_enumeration_cvk:
    cv.value.enumeration_val = s_val;
    break;
  };
  ilu_RegisterUnionArmValue(arm, index, &cv, err);
}

#endif /* IIOP_PROTOCOL */

#endif

/* make sure all modules from libilu.a are included by referencing some routines */

#if !(defined(WIN32))
void ______ilulisp_NeverCalled(void)
{
  ilu_MXAClear(NULL, NULL);		/* alarmux.c */
  ilu_IntroTypeOfCall(NULL);		/* call.c */
  ilu_ClassOfObject(NULL);		/* object.c */
  ilu_IDOfMethod(NULL);			/* method.c */
  ilu_ClosePort (NULL);			/* port.c */
  ilu_hash_PairsInTable(0);		/* hash.c */
  ilu_GetFDBudget();			/* connect.c */
  ilu_SetDebugLevel(0);			/* debug.c */
  ilu_SetLockTech(NULL,NULL);		/* locks.c */
  ilu_CreateTrueServer(0,0,0,0);	/* server.c */
  ilu_VeryInterested(NULL);		/* gc.c */
  _ilu_vector_new (0, NULL);		/* vector.c */
  ilu_malloc(0);			/* memory.c */
  ilu_SetMainLoop (NULL);		/* mainloop.c */
  ilu_FindClassFromID(NULL);		/* type.c */
  ilu_PublishObject(NULL);		/* simpbind.c */
  {
    extern ilu_Error ilu_RegisterTransport (void *, void *, int);
    ilu_RegisterTransport(NULL,NULL,0);	/* ilutransport.c */
  }
  {
    extern ilu_Error ilu_RegisterProtocol (void *, void *, int);
    ilu_RegisterProtocol(NULL,NULL,0);	/* iluprotocol.c */
  }
#ifdef ZLIB_TRANSPORT
  {
    extern void _ilu_zlib_TransportCreator(void *, void *);
    _ilu_zlib_TransportCreator(NULL, NULL);
  }
#endif
#ifdef UDPSOCKET_TRANSPORT
  {
    extern void _ilu_udp_TransportCreator(void *, void *);
    _ilu_udp_TransportCreator(NULL, NULL);
  }
#endif /* UDPSOCKET_TRANSPORT */
#ifdef TCPIP_TRANSPORT
  {
    extern void _ilu_tcp_TransportCreator(void *, void *);
    _ilu_tcp_TransportCreator(NULL, NULL);
  }
#endif /* TCPIP_TRANSPORT */
#ifdef SUNRPCRM_TRANSPORT
  {
    extern void _ilu_sunrpcrm_TransportCreator(void *, void *);
    _ilu_sunrpcrm_TransportCreator(NULL, NULL);
  }
#endif /* SUNRPCRM_TRANSPORT */
#ifdef W3MUX_TRANSPORT
  {
    extern void _ilu_w3mux_TransportCreator(void *, void *);
    _ilu_w3mux_TransportCreator(NULL, NULL);
  }
#endif /* W3MUX_TRANSPORT */
#ifdef SECURE_TRANSPORT
  {
    extern void _ilu_gss_TransportCreator(void *, void *);
    _ilu_gss_TransportCreator(NULL, NULL);
  }
#endif /* SECURE_TRANSPORT */
#ifdef SUNRPC_PROTOCOL
  {
    extern void _ilu_sunrpc_Protocol(void *, void *);
    _ilu_sunrpc_Protocol(NULL, NULL);
  }
#endif /* SUNRPC_PROTOCOL */
#ifdef COURIER_PROTOCOL
  {
    extern void _ilu_courier_Protocol(void *, void *);
    _ilu_courier_Protocol(NULL, NULL);
  }
#endif /* COURIER_PROTOCOL */
#ifdef IIOP_PROTOCOL
  {
    extern void _ilu_IIOP_Protocol(void *, void *);
    _ilu_IIOP_Protocol(NULL, NULL);
  }
#endif /* IIOP_PROTOCOL */
#ifdef W3NG_PROTOCOL
  {
    extern void _ilu_w3ng_Protocol(void *, void *);
    _ilu_w3ng_Protocol(NULL, NULL);
  }
#endif /* W3MUX_PROTOCOL */
#ifdef HTTP_PROTOCOL
  {
    extern void _ilu_http_Protocol(void *, void *);
    _ilu_http_Protocol(NULL, NULL);
  }
#endif /* HTTP_PROTOCOL */
#ifdef W3NG_PROTOCOL
  {
    extern void _ilu_w3ng_Protocol(void *, void *);
    _ilu_w3ng_Protocol(NULL, NULL);
  }
#endif /* HTTP_PROTOCOL */
#ifdef ADD_VARIANT_SUPPORT
  ilu_EndPickle(NULL, NULL, NULL);
#endif
#ifdef ADD_PICKLE3_SUPPORT
  {
    extern void _ilu_pickle3_EndPickle(void *, void *, void *);
    _ilu_pickle3_EndPickle(NULL, NULL, NULL);
  }
#endif
#ifdef ADD_PICKLE2_SUPPORT
  {
    extern void _ilu_pickle2_EndPickle(void *, void *, void *);
    _ilu_pickle2_EndPickle(NULL, NULL, NULL);
  }
#endif
  ilu_AddRegisterersToDefault(NULL, NULL, NULL, NULL, NULL, NULL);	/* bsdmnlp.c */
  ilu_FineTime_Now();			/* bsdutils.c */
}
#endif

