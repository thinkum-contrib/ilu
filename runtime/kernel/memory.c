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
*/
/* $Id: memory.c,v 1.34 1999/08/03 01:53:11 janssen Exp $ */
/* Last edited by Mike Spreitzer November 21, 1997 3:03 pm PST */

#include "iluntrnl.h"

#include "oscalls.h"	/* for OS_SLEEP */

#ifdef USE_BOEHM_GC_MALLOC
#ifdef GC_DEBUG		/* this is the ILU GC_DEBUG... */
#undef GC_DEBUG		/* ...so undefine it so we can use the Boehm GC GC_DEBUG! */
#endif

#ifdef USE_BOEHM_GC_MALLOC_DEBUGGING
#define GC_DEBUG
#endif /* USE_BOEHM_GC_MALLOC_DEBUGGING */

#include BOEHM_GC_MALLOC_HEADER_FILE

#ifdef USE_BOEHM_GC_MALLOC_DEBUGGING
#define REAL_MALLOC(x)		GC_debug_malloc((x), (char *) file, (int) line)
#define REAL_FREE(x)		GC_debug_free(x)
#define REAL_REALLOC(x,s)	GC_debug_realloc((x), (s), (char *) file, (int) line)
#else
#define REAL_MALLOC(x)		GC_malloc(x)
#define REAL_FREE(x)		GC_free(x)
#define REAL_REALLOC(x,s)	GC_realloc((x),(s))
#endif /* USE_BOEHM_GC_MALLOC_DEBUGGING */

#else /* !USE_BOEHM_GC_MALLOC */

#define REAL_MALLOC	malloc
#define REAL_FREE	free
#define REAL_REALLOC	realloc

#endif

/*L1, L2, Main unconstrained*/

typedef struct ilu_FreerCons_s FreerCons, *FreerList;

struct ilu_FreerCons_s {
	FreerList next;
	void (*free)(ilu_cardinal size);
};

static FreerList freers = NIL;

ilu_boolean
ilu_AddFreer(void (*free_rtn) (ilu_cardinal size))
{
  FreerList       fl;
  ILU_NOTE(MALLOC_DEBUG, ("ilu_AddFreer(%p)\n", free_rtn));
  fl = (FreerList) ilu_malloc(sizeof(FreerCons));
  if (fl == NIL)
    return ilu_FALSE;
  fl->free = free_rtn;
  fl->next = freers;
  freers = fl;
  return ilu_TRUE;
}

void           *
ilu_full_malloc(ilu_cardinal size, const char *file, int line)
{
  void           *ans;
  FreerList       fl;
  ans = REAL_MALLOC((SIZE_T) size);
  ILU_NOTE(MALLOC_DEBUG,
	("ilu_malloc(%lu=0x%lx)@%s:%d => %p\n",
	 size, size, file, line, ans));
  if ((ans != NIL) || (size == 0))
    return ans;
  for (fl = freers; fl != NIL; fl = fl->next) {
    ILU_NOTE(MALLOC_DEBUG,
	  ("ilu_malloc: trying freer %p\n", fl->free));
    fl->free(size);
    ans = REAL_MALLOC((SIZE_T) size);
    ILU_NOTE(MALLOC_DEBUG,
	  ("ilu_malloc(%lu=0x%lx)@%s:%d finally => %p\n",
	   size, size, file, line, ans));
    if (ans != NIL)
      return ans;
  }
  ILU_NOTE(MALLOC_DEBUG,
	("ilu_malloc(%lu=0x%lx)@%s:%d fails.\n",
	 size, size, file, line));
  return NIL;
}

void           *
ilu_full_realloc(void *p, ilu_cardinal size, const char *file, int line)
{
  void           *ans;
  FreerList       fl;
  ans = REAL_REALLOC(p, (SIZE_T) size);
  ILU_NOTE(MALLOC_DEBUG,
	("ilu_realloc(%p, %lu=0x%lx)@%s:%d => %p\n",
	 p, size, size, file, line, ans));
  if ((ans != NIL) || (size == 0))
    return ans;
  for (fl = freers; fl != NIL; fl = fl->next) {
    ILU_NOTE(MALLOC_DEBUG,
	  ("ilu_realloc: trying freer %p\n", fl->free));
    fl->free(size);
    ans = REAL_REALLOC(p, (SIZE_T) size);
    ILU_NOTE(MALLOC_DEBUG,
	  ("ilu_realloc(%p, %lu=0x%lx)@%s:%d finally => %p\n",
	   p, size, size, file, line, ans));
    if (ans != NIL)
      return ans;
  }
  ILU_NOTE(MALLOC_DEBUG,
	("ilu_realloc(%p, %lu=0x%lx)@%s:%d fails.\n",
	 p, size, size, file, line));
  return NIL;
}

void ilu_full_free(void *p, const char *file, int line)
{
  ILU_NOTE(MALLOC_DEBUG, ("ilu_free(%p)@%s:%d\n", p,
		       file, line));
  if (p != NIL)
    REAL_FREE(p);
}

static _ilu_FailureHandler theMFC = {_ilu_ConsumeByLoop, ilu_TRUE};

void ilu_SetMemFailureAction(int mfa)
{
  ILU_NOTE(MALLOC_DEBUG,
	("ilu_SetMemFaultAction: to %d.\n", mfa));
  theMFC = _ilu_FailureActionToConsumer(mfa, 0);
  return;
}

void ilu_SetMemFailureConsumer(ilu_FailureConsumer mfc)
{
  _ilu_Assert(mfc != NULLFN, "SetMemFailureConsumer(NIL)");
  ILU_NOTE(MALLOC_DEBUG,
	("ilu_SetMemFailureConsumer: to %p.\n", mfc));
  theMFC.fc = mfc;
  theMFC.printMsg = ilu_FALSE;
  return;
}

void           *
ilu_full_must_malloc(ilu_cardinal size,
		     const char *file, int line)
{
  long unsigned   req = size;
  void           *ans = ilu_full_malloc(size, file, line);
  if (ans != NIL)
    return ans;
  ILU_NOTE(MALLOC_DEBUG,
	("ilu_must_malloc(%lu=0x%lx) failed!\n",
	 size, size));
  if (theMFC.printMsg) {
    ILU_ERRPRINTF(
     "\nILU %s:  unrecoverable failure to allocate dynamic memory",
		  ilu_GetILUVersion());
    ILU_ERRPRINTF(" (%lu=0x%lx bytes requested)", req, req);
    ILU_ERRPRINTF(" at line %d in file %s.\n", line, file);
    ILU_ERRPRINTF("For information on how to debug or report this,");
    ILU_ERRPRINTF(" see the Debugging section of the ILU manual.\n");
  }
  (*theMFC.fc) (file, line);
  ILU_ERRPRINTF("ilu_FailureConsumer %p returned!", theMFC);
  ILU_ERRPRINTF("going into sleep loop!\n");
  _ilu_ConsumeByLoop(__FILE__, __LINE__);
  return (NIL);
}

void           *
ilu_full_MallocE(ilu_cardinal size,
		 ILU_ERRS((no_memory)) * err,
		 const char *file, int line)
{
  void           *ans = ilu_full_malloc(size, file, line);
  if ((ans == NIL) && (size > 0))
    return ILU_ERR_FULLCONS1(no_memory, err, nbytes, size, NIL,
			     file, line);
  return (ILU_CLER(*err), ans);
}

void           *
ilu_full_ReallocE(void *p, ilu_cardinal size,
		  ILU_ERRS((no_memory)) * err,
		  const char *file, int line)
{
  void           *ans = ilu_full_realloc(p, size, file, line);
  if ((ans == NIL) && (size > 0))
    return ILU_ERR_FULLCONS1(no_memory, err, nbytes, size, NIL,
			     file, line);
  return (ILU_CLER(*err), ans);
}

#define BLKSIZE 4

ilu_string
ilu_full_StrdupE(const char *str,
		 ILU_ERRS((no_memory)) * err,
		 const char *file, int line)
{
  ilu_string      p;
  unsigned        len, fullen, i;
  if (str == NIL)
    return (ILU_CLER(*err), NIL);
  len = strlen(str);
  fullen = len + BLKSIZE - (len % BLKSIZE);
  if ((p = ilu_full_MallocE(fullen, err, file, line)) == NIL)
    return (NIL);
  strcpy(p, str);
  for (i = len + 1; i < fullen; i++)
    p[i] = 0;
  return (p);
}

ilu_string
_ilu_full_Strdup(const char *str,
		 const char *file, int line)
{
  ilu_string      p;
  unsigned        len, fullen, i;
  if (str == NIL)
    return (NIL);
  len = strlen(str);
  fullen = len + BLKSIZE - (len % BLKSIZE);
  if ((p = ilu_full_malloc(fullen, file, line)) == NIL)
    return (NIL);
  strcpy(p, str);
  for (i = len + 1; i < fullen; i++)
    p[i] = 0;
  return (p);
}

ilu_string
ilu_Strcat3E(const char *s1, const char *s2,
	     const char *s3, ILU_ERRS((no_memory)) * err)
{
  int             l1 = (s1 == NIL) ? 0 : strlen(s1);
  int             l2 = (s2 == NIL) ? 0 : strlen(s2);
  int             l3 = (s3 == NIL) ? 0 : strlen(s3);
  ilu_string      t = ilu_MallocE(1 + l1 + l2 + l3, err);
  ilu_string      u = t;
  if (t == NIL)
    return t;
  if (s1 != NIL) {
    strcpy(u, s1);
    u += l1;
  }
  if (s2 != NIL) {
    strcpy(u, s2);
    u += l2;
  }
  if (s3 != NIL) {
    strcpy(u, s3);
    u += l3;
  }
  *u = 0;
  return t;
}

ilu_string 
_ilu_Strcat3(const ilu_string s1, const ilu_string s2,
	     const ilu_string s3)
{
  ILU_ERRS((no_memory)) lerr;
  ilu_string      ans = ilu_Strcat3E(s1, s2, s3, &lerr);
  ILU_HANDLED(lerr);
  return ans;
}

ilu_string _ilu_Strcat5(const ilu_string s1, const ilu_string s2,
			const ilu_string s3, const ilu_string s4,
			const ilu_string s5)
{
  ILU_ERRS((no_memory)) lerr;
  ilu_string      ans = ilu_Strcat5E(s1, s2, s3, s4, s5, &lerr);
  ILU_HANDLED(lerr);
  return ans;
}

ilu_string
ilu_Strcat5E(const char *s1, const char *s2,
	     const char *s3, const char *s4,
	     const char *s5, ILU_ERRS((no_memory)) * err)
{
  int             l1 = (s1 == NIL) ? 0 : strlen(s1);
  int             l2 = (s2 == NIL) ? 0 : strlen(s2);
  int             l3 = (s3 == NIL) ? 0 : strlen(s3);
  int             l4 = (s4 == NIL) ? 0 : strlen(s4);
  int             l5 = (s5 == NIL) ? 0 : strlen(s5);
  ilu_string      t = ilu_MallocE(1 + l1 + l2 + l3 + l4 + l5, err);
  ilu_string      u = t;
  if (t == NIL)
    return t;
  if (s1 != NIL) {
    strcpy(u, s1);
    u += l1;
  }
  if (s2 != NIL) {
    strcpy(u, s2);
    u += l2;
  }
  if (s3 != NIL) {
    strcpy(u, s3);
    u += l3;
  }
  if (s4 != NIL) {
    strcpy(u, s4);
    u += l4;
  }
  if (s5 != NIL) {
    strcpy(u, s5);
    u += l5;
  }
  *u = 0;
  return t;
}

ilu_boolean
ilu_CharBufReserve(ilu_CharBuf * s1, ilu_cardinal s2len,
		   ILU_ERRS((no_memory)) * err)
{
  ilu_cardinal    need = s1->icb_len + s2len + 1;
  if (need <= s1->icb_len || need <= s2len)
    return ILU_ERR_CONS1(no_memory, err, nbytes, 0xFFFFFFFF, ilu_FALSE);
  if (s1->icb_size < need) {
    ilu_cardinal    newsize = s1->icb_size + s1->icb_size / 2;
    char           *newbase;
    if (newsize < need)
      newsize = need;
    newbase = (s1->icb_base ? ilu_ReallocE(s1->icb_base, newsize, err)
	       : ilu_MallocE(newsize, err));
    if (!newbase)
      return ilu_FALSE;
    s1->icb_base = newbase;
    s1->icb_size = newsize;
  }
  return ILU_CLER(*err);
}

ilu_boolean
ilu_CharBufAppend(ilu_CharBuf * s1, char *s2, ilu_cardinal s2len,
		  ILU_ERRS((no_memory)) * err)
{
  if (!ilu_CharBufReserve(s1, s2len, err))
    return ilu_FALSE;
  memcpy(s1->icb_base + s1->icb_len, s2, s2len);
  s1->icb_len += s2len;
  s1->icb_base[s1->icb_len] = 0;
  return ilu_TRUE;
}

ilu_CharBuf
ilu_CharBufFromChars(const char *s, ilu_cardinal len,
		     ILU_ERRS((no_memory)) * err)
{
  ilu_CharBuf     ans = {0};
  ans.icb_base = ilu_MallocE(len + 1, err);
  if (ans.icb_base) {
    memcpy(ans.icb_base, s, len);
    ans.icb_len = len;
    ans.icb_size = len + 1;
    ans.icb_base[len] = 0;
  }
  return ans;
}

/*
 * ... and we provide actual procedures, for clients that extract
 * and use procedure pointers.
 */

#undef ilu_malloc
#undef ilu_free
#undef ilu_realloc
#undef ilu_StrdupE
#undef _ilu_Strdup

void           *ilu_malloc(ilu_cardinal size)
{
  return ilu_full_malloc(size, __FILE__, __LINE__);
}

void           *ilu_realloc(void *p, ilu_cardinal size)
{
  return ilu_full_realloc(p, size, __FILE__, __LINE__);
}

void           ilu_free(void *p)
{
  ilu_full_free(p, __FILE__, __LINE__);
}

ilu_string	ilu_StrdupE (const char * s, ilu_Error *err)
{
  return (ilu_full_StrdupE (s, err, __FILE__, __LINE__));
}

ilu_string	_ilu_Strdup(const char *s)
{
  return (_ilu_full_Strdup (s, __FILE__, __LINE__));
}

