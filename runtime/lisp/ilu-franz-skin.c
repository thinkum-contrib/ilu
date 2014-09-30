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

$Id: ilu-franz-skin.c,v 1.22 1999/08/03 01:53:29 janssen Exp $
*/
/* Last edited by Mike Spreitzer September 15, 1998 10:52 am PDT */

#define _BSD_SOURCE		/* to allow SGI to process select() */

#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <string.h>
#include <stdio.h>	/* for sprintf */

#include <iluxport.h>

/* set up technology for blocking on read and write appropriately */

static int (*read_fn) (int fd, unsigned long msec) = NULL;
static int (*write_fn) (int fd, unsigned long msec) = NULL;

#if (LISP_FLAG_BITS==2)
#define FixnumToInt(v)	((v)>>2)
#elif (LISP_FLAG_BITS==3)
#define FixnumToInt(v)	((v)>>3)
#else
#error "Don't know how to convert Fixnum to int for this architecture"
#endif /* figuring out how to set flag bits */

#define MILLISECONDS(x)	(((x)==NULL)?0:((x)->ft_s * 1000 + (((x)->ft_t * 1000) / ilu_FineTimeRate)))

int ilufranz_OutputPossibleP (int fd)
{
  fd_set writefds;
  fd_set exceptfds;
  int width, stat;
  static struct timeval to = { 0, 0 };

  width = fd + 1;
  FD_ZERO(&writefds);
  FD_ZERO(&exceptfds);
  FD_SET(fd, &writefds);
  FD_SET(fd, &exceptfds);
  stat = select (width, NULL, &writefds, &exceptfds, &to);
  if (stat > 0 && (FD_ISSET(fd, &writefds) || FD_ISSET(fd, &exceptfds)))
    return 1;
  else
    return 0;
}

static void internal_read_wait (int fd, int auxfd, ilu_boolean *sure, ilu_FineTime *limit,
				ILU_ERRS((interrupt)) * err)
{
  if (read_fn != NULL)
    *sure = FixnumToInt((*read_fn)(fd, (limit == NULL) ? 0
				   : MILLISECONDS(limit)));
  else
    *sure = 0;
   ILU_CLER(*err);  
}

static void internal_write_wait (int fd, int auxfd, ilu_boolean *sure, ilu_FineTime *limit,
				ILU_ERRS((interrupt)) * err)
{
  if (ilufranz_OutputPossibleP(fd))
    {
      *sure = 1;
    }
  else
    {
      if (write_fn != NULL)
	*sure = FixnumToInt((*write_fn)(fd, (limit == NULL) ? 0
					: MILLISECONDS(limit)));
      else
	*sure = 0;
    }
   ILU_CLER(*err);  
}

void ilufranz_SetWaitTech (int (*read_wait) (int fd, unsigned long timetowait),
			   int (*write_wait) (int fd, unsigned long timetowait))
{
  static ilu_WaitTech wait_tech;

  read_fn = read_wait;
  write_fn = write_wait;
  wait_tech.wt_read_wait = internal_read_wait;
  wait_tech.wt_write_wait = internal_write_wait;
  ilu_SetWaitTech (&wait_tech);
}

/* these functions are used for locking and condition variables */

typedef long (*franzCreateFn) (ilu_string, ilu_string);
typedef void (*franzLockFn) (ilu_private);
typedef void (*franzWaitFn) (ilu_private, ilu_private, ilu_private);

static franzCreateFn internal_mcreate;
static franzLockFn internal_macquire;
static franzLockFn internal_mhold;
static franzLockFn internal_mrelease;
static franzLockFn internal_mdestroy;
static franzCreateFn internal_ccreate;
static franzLockFn internal_cnotify;
static franzLockFn internal_cdestroy;
static franzWaitFn internal_cwait;

typedef struct {
  ilu_private	ilum_lisp_index;	/* Lisp index val */
  ilu_string	ilum_d1;		/* string val */
  ilu_string	ilum_d2;		/* string val */
} FranzMutex;

static char *ilu_must_strdup(char *arg)
{
  char *result = (char *) ilu_must_malloc(strlen(arg) + 1);
  strcpy(result, arg);
  return result;
}

static ilu_private franz_mcreate (ilu_string d1, ilu_string d2)
{
  FranzMutex *m = (FranzMutex *) ilu_must_malloc(sizeof(FranzMutex));
  m->ilum_lisp_index = (ilu_private) FixnumToInt((*internal_mcreate) (d1, d2));
  m->ilum_d1 = ilu_must_strdup(d1);
  m->ilum_d2 = ilu_must_strdup(d2);
  return (ilu_private) m;
}

static void franz_muncons(ilu_private m, ilu_string *d1, ilu_string *d2,
			  ilu_Error *err)
{
  FranzMutex *fm = (FranzMutex *) m;

  if (d1 == 0 || d2 == 0 || fm == 0) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, 0);
    return;
  }
  *d1 = fm->ilum_d1;
  *d2 = fm->ilum_d2;
  ILU_CLER(*err);
  return;
}

static void franz_macquire(ilu_private m, ilu_Error *err)
{
  FranzMutex *fm = (FranzMutex *) m;

  (*internal_macquire) (fm->ilum_lisp_index);
  ILU_CLER(*err);
  return;
}

static void franz_mhold(ilu_private m, ilu_Error *err)
{
  FranzMutex *fm = (FranzMutex *) m;

  (*internal_mhold) (fm->ilum_lisp_index);
  ILU_CLER(*err);
  return;
}

static void franz_mrelease(ilu_private m, ilu_Error *err)
{
  FranzMutex *fm = (FranzMutex *) m;

  (*internal_mrelease) (fm->ilum_lisp_index);
  ILU_CLER(*err);
  return;
}

static void franz_mdestroy (ilu_private m, ilu_Error *err)
{
  FranzMutex *fm = (FranzMutex *) m;

  (*internal_mdestroy) (fm->ilum_lisp_index);
  ilu_free(fm->ilum_d1);
  ilu_free(fm->ilum_d2);
  ilu_free(fm);
  ILU_CLER(*err);
  return;
}

static ilu_private franz_ccreate (ilu_string d1, ilu_string d2)
{
   long retval = (*internal_ccreate) (d1, d2);
   return  (ilu_private)FixnumToInt(retval);
}

static void franz_cuncons(ilu_private c, ilu_string *d1, ilu_string *d2,
			  ilu_Error *err)
{
   if (d1 == 0 || d2 == 0) {
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, 0);
      return;
   }
   ILU_ERR_CONS0(no_implement, err, 0);
   return;
}

static void franz_cnotify(ilu_private c, ilu_Error *err)
{
   (*internal_cnotify) (c);
   ILU_CLER(*err);
   return;
}

static void franz_cdestroy(ilu_private c, ilu_Error *err)
{
   (*internal_cdestroy) (c);
   ILU_CLER(*err);
   return;
}   

static void franz_cwait(ilu_private c, ilu_private m,
			ilu_private m2, const ilu_FineTime * timeout,
			ilu_Error *err)
{
  FranzMutex *fm1, *fm2;
  _ilu_Assert(!timeout,
	      "generic code violates contract wrt CV timeouts"
	      " in ilu-franz-skin:franz_cwait");
  fm1 = (FranzMutex *) m;
  fm2 = (FranzMutex *) m2;

   (*internal_cwait) (c, fm1->ilum_lisp_index, fm2->ilum_lisp_index);
   ILU_CLER(*err);
   return;
}

void ilufranz_SetLockTech (long (*mcreate)(ilu_string d1, ilu_string d2),
			   void (*macquire) (ilu_private m),
			   void (*mhold) (ilu_private m),
			   void (*mrelease) (ilu_private m),
			   void (*mdestroy) (ilu_private m),
			   long (*ccreate)(ilu_string d1, ilu_string d2),
			   void (*cnotify) (ilu_private c),
			   void (*cdestroy) (ilu_private c),
			   void (*cwait) (ilu_private c, ilu_private m,
					  ilu_private m2),
			   ILU_ERRS((bad_param, no_memory)) * _err)
{
  static ilu_LockTech franzLockTech = {0};

  internal_mcreate = mcreate;
  internal_macquire = macquire;
  internal_mhold = mhold;
  internal_mrelease = mrelease;
  internal_mdestroy = mdestroy;
  internal_ccreate = ccreate;
  internal_cnotify = cnotify;
  internal_cdestroy = cdestroy;
  internal_cwait = cwait;

  franzLockTech.lt_mcreate = franz_mcreate;
  franzLockTech.lt_muncons = franz_muncons;
  franzLockTech.lt_acquire = franz_macquire;
  franzLockTech.lt_hold = franz_mhold;
  franzLockTech.lt_release = franz_mrelease;
  franzLockTech.lt_mdestroy = franz_mdestroy;
  franzLockTech.lt_ccreate = franz_ccreate;
  franzLockTech.lt_cuncons = franz_cuncons;
  franzLockTech.lt_notify = franz_cnotify;
  franzLockTech.lt_cdestroy = franz_cdestroy;
  franzLockTech.lt_wait = franz_cwait;

  ilu_SetLockTech (&franzLockTech, _err);
}

void ilufranz_CallAlarmClosure (void (*proc) (ilu_private rock), ilu_private rock)
{
  (*proc)(rock);
}

static int (*internal_create_alarm)();

static ilu_refany franz_create_alarm ()
{
  return ((ilu_refany) (FixnumToInt((*internal_create_alarm)())));
}

static void (*internal_set_alarm) (unsigned long alarm,
				   long time_sec, unsigned long time_msec,
				   unsigned long p1, unsigned long p2);

static void franz_set_alarm (ilu_refany alarm, ilu_FineTime t,
			     void (*proc)(ilu_private rock), ilu_private rock)
{
/*
  printf ("Setting alarm %u to %u.%u (%24.24s)\n", (unsigned long) alarm, t.ft_s, t.ft_t,
	  ctime(&t.ft_s));
*/
  (*internal_set_alarm) ((unsigned long) alarm,
			 t.ft_s, (t.ft_t * 1000) / ilu_FineTimeRate,
			 (unsigned long) proc, (unsigned long) proc);
}

void ilufranz_SetAlarms (int (*create_alarm) (),
			 void (*set_alarm) (unsigned long alarm, long time_sec, unsigned long time_msec,
					    unsigned long p1, unsigned long p2),
			 void (*unset_alarm) (ilu_refany alarm))
{
  static ilu_MainLoop ml;

  internal_create_alarm = create_alarm;
  internal_set_alarm = set_alarm;

  ml.ml_run = NULL;
  ml.ml_exit = NULL;
  ml.ml_register_input = NULL;
  ml.ml_unregister_input = NULL;
  ml.ml_register_output = NULL;
  ml.ml_unregister_output = NULL;

  ml.ml_create_alarm = franz_create_alarm;
  ml.ml_set_alarm = franz_set_alarm;
  ml.ml_unset_alarm = unset_alarm;

  ilu_SetMainLoop (&ml);
}

static int (*internal_fork_proc) (unsigned long proc, unsigned long rock) = 0;

void
  ilufranz_CallThreadProc (unsigned long proc_in, unsigned long rock_in)
{
  ilu_ClosureProc proc;
  void *rock;

  proc = (ilu_ClosureProc) proc_in;
  rock = (void *) rock_in;
  (*proc)(rock);
}

static ilu_boolean
  dummy_fork_proc (ilu_ClosureProc proc, void *rock, ilu_Error *err)
{
  int lispval;
  int retval;

  if (internal_fork_proc == 0) {
    return ILU_ERR_CONS0(no_implement, err, ilu_FALSE);
  } else {
    lispval = (*internal_fork_proc) ((unsigned long) proc, (unsigned long) rock);
    retval = FixnumToInt(lispval);
    if (retval == 0) {
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_threadFork, ilu_FALSE);
    } else {
      ILU_CLER(*err);
      return ilu_TRUE;
    }
  }
}

void ilufranz_SetForkProc (int (*fork_proc) (unsigned long proc, unsigned long rock))
{
  ilu_Error lerr;
  internal_fork_proc = fork_proc;
  ilu_SetForkTech (dummy_fork_proc, &lerr);
  if (ILU_ERRNOK(lerr)) {
    ILU_HANDLED(lerr);
  };
}


static unsigned long (*internal_interest_hook) (unsigned long index, unsigned long interest) = 0;

static ilu_LanguageIndex my_language;

/*Inside(obj's server, obj's type)*/
static ilu_boolean kernel_interest_hook (ilu_Object kobj, int vi)
{
  unsigned long index =
     (unsigned long) ilu_GetLanguageSpecificObject (kobj, my_language);
  unsigned long newindex;
  ilu_Class cl = ilu_ClassOfObject(kobj);
  
  /* printf("Interest:  %s %4u %s\n", (vi == 0) ? "NO  " : "YES", index, ilu_SBHOfObject(kobj)); */

  if (index != 0 &&
      internal_interest_hook != 0 &&
      ((!ilu_TrueInstanceP(kobj)) || (cl->cl_collectible)) &&
      (((vi == 0) && ((index & 0x1) != 0)) ||
       ((vi != 0) && ((index & 0x1) == 0))))
    {
      newindex = FixnumToInt((*internal_interest_hook) (index, vi));
      if (newindex != index)
	{
/*	  printf("           LSPO changed to %u\n", newindex); */
	  ilu_RegisterLanguageSpecificObject (kobj, (void *) newindex,
					      my_language);
	}
    }
  return ilu_TRUE;
}

void ilufranz_SetInterestHook (unsigned long (*hook) (unsigned long,
						      unsigned long),
			       ilu_LanguageIndex language)
{
  internal_interest_hook = hook;
  ilu_SetNoter (&kernel_interest_hook, my_language = language);
}

ILU_PUBLIC ilu_Error * ilufranz_CreateErrorStruct()
{
   return ilu_must_malloc(sizeof(ilu_Error));
}

ILU_PUBLIC void ilufranz_ClearErrorStruct(ilu_Error *_err)
{
   ILU_CLER(*_err);
}

ILU_PUBLIC ilu_boolean ilufranz_ErrorOK(ilu_Error *_err)
{
   return ILU_ERROK(*_err);
}

ILU_PUBLIC ilu_string ilufranz_ErrorDetails(ilu_Error *_err)
{
   const char *err_file = ilu_ErrorFile(_err);
   int         err_line = ilu_ErrorLine(_err);
   const char  format[] = "%s [%s: %d]";
   const char *msg = "no details";
   char *details;
   char  nbuf[1024];

  ILU_ERR_SWITCH(*_err) {
    ILU_SUCCESS_CASE return "SUCCESS";
    ILU_ERR_CASE(no_memory, x) {
      sprintf(nbuf, "attempting to malloc %lu bytes", x->nbytes);
      msg = nbuf;
    }
    ILU_ERR_CASE(interrupted, x) {
      sprintf(nbuf, "interrupt set 0x%x", x->ilu_interruptSet);
      msg = nbuf;
    }
    ILU_ERR_CASE(bad_param, x) msg = ilu_GetMinorDescrFromCodes(_err->ilu_type, x->minor);
    ILU_ERR_CASE(imp_limit, x) msg = ilu_GetMinorDescrFromCodes(_err->ilu_type, x->minor);
    ILU_ERR_CASE(comm_failure, x) msg = ilu_GetMinorDescrFromCodes(_err->ilu_type, x->minor);
    ILU_ERR_CASE(inv_objref, x) msg = ilu_GetMinorDescrFromCodes(_err->ilu_type, x->minor);
    ILU_ERR_CASE(internal, x) msg = ilu_GetMinorDescrFromCodes(_err->ilu_type, x->minor);
    ILU_ERR_CASE(marshal, x) msg = ilu_GetMinorDescrFromCodes(_err->ilu_type, x->minor);
    ILU_ERR_CASE(bad_typecode, x) msg = ilu_GetMinorDescrFromCodes(_err->ilu_type, x->minor);
    ILU_ERR_CASE(bad_operation, x) msg = ilu_GetMinorDescrFromCodes(_err->ilu_type, x->minor);
    ILU_ERR_CASE(no_resources, x) msg = ilu_GetMinorDescrFromCodes(_err->ilu_type, x->minor);
    ILU_ERR_ELSE
      ;
  } ILU_ERR_ENDSWITCH;
   details = ilu_must_malloc(strlen(err_file) + 15 + sizeof(format)
			     + (msg == 0) ? 0 : strlen(msg));
   sprintf(details, format, msg, err_file, err_line);
   return details;
}

ILU_PUBLIC void ilufranz_FreeCStruct(void *ptr)
{
   ilu_free(ptr);
}

static unsigned long (*lisp_object_of_ih) (int, ilu_string) = 0;
static void (*lisp_free_self) (int) = 0;

static ilu_Object GeneralObjectOfIH (ilu_ObjectTable self, ilu_string ih)
{
  if (self != ILU_NIL && lisp_object_of_ih != 0)
    return ((ilu_Object) FixnumToInt((*lisp_object_of_ih) ((int) (self->ot_rock), ih)));
  else
    return 0;
}

static void GeneralFreeSelf (ilu_ObjectTable self)
{
  if (self != ILU_NIL && lisp_free_self != 0)
    {
      (*lisp_free_self) ((int) (self->ot_rock));
      ilu_free(self);
    }
}

ilu_ObjectTable ilufranz_CreateObjectTable (int lot)
{
  ilu_ObjectTable newt = (ilu_ObjectTable) ilu_malloc(sizeof(struct ilu_ObjectTable_struct));
  if (newt == 0)
    return 0;
  else
    {
      newt->ot_object_of_ih = GeneralObjectOfIH;
      newt->ot_free_self = GeneralFreeSelf;
      newt->ot_rock = (ilu_private) lot;
      return (newt);
    }
}

void ilufranz_SetupObjectTables (unsigned long (*p1) (int, ilu_string),
				 void (*p2) (int))
{
  lisp_object_of_ih = p1;
  lisp_free_self = p2;
}
