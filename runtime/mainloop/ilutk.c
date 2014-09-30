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
/* ilutk.c */
/* Last edited by Mike Spreitzer October 19, 1995 11:12 am PDT */

#include <tk.h>
#include <iluxport.h>
#include "ilutk.h"
#include <iluhash.h>
#include <iluntrnl.h>		/* for Assert */
#include <limits.h>		/* for INT_MAX */

#ifdef WIN32
#if (TCL_MAJOR_VERSION <= 7)
/* On Windows, we need to tell Tcl what sort of file we are using.
   tk_CreateFileHandler and tk_DeleteFileHandler assume that they are
   using Unix file descriptors.  We fix this to assume that we are
   always using windows sockets, since internally we always use
   windows sockets.  This won't work if the program calls
   ilu_RegisterInputSource with something that isn't a windows socket,
   but a Tk program is unlikely to do that.  */
#undef Tk_CreateFileHandler
#define Tk_CreateFileHandler(file,mask,proc,data) \
    Tcl_CreateFileHandler(Tcl_GetFile((ClientData) (file), TCL_WIN_SOCKET), \
    (mask), (proc), (data))
#undef Tk_DeleteFileHandler
#define Tk_DeleteFileHandler(file) \
    Tcl_DeleteFileHandler(Tcl_GetFile((ClientData) (file), TCL_WIN_SOCKET))
#else 
/* TCL_MAJOR_VERSION > 7 Tcl_CreateFileHandler and Tcl_DeleteFileHandler no longer exist */
#error "No Tcl_CreateFileHandler and Tcl_DeleteFileHandler available - if you \
        develop a workaround please email ilu-core@parc.xerox.com"
#endif /* TCL_MAJOR_VERSION <= 7 */
#endif /* WIN32 */

static void
Run(int *stop)
{
  *stop = 0;
  while (! (*stop))
    (void) Tk_DoOneEvent(0);
  return;
}

static void
Exit(int *stop)
{
  *stop = 1;
}

typedef struct {
  struct {
    ilu_IOHandler   h;		/* NULLFN when not significant */
    ilu_private     r;
  }               for_in, for_out;
  int             fd;
}              *Handlers;

static HashTable fdh = NIL;

static          Handlers
GetHandlers(int fd)
{
  Handlers        hs;
  if (fdh == NIL) {
    fdh = ilu_hash_MakeNewTable(16, ilu_hash_HashPointer,
				 ilu_hash_PointerCompare);
    if (fdh == NIL)
      return NIL;
  }
  hs = (Handlers) ilu_hash_FindInTable(fdh, (ilu_refany) fd);
  if (hs == NULL) {
    hs = ilu_malloc(sizeof(*hs));
    if (hs == NIL)
      return NIL;
    hs->for_in.h = hs->for_out.h = NULLFN;
    hs->for_in.r = hs->for_out.r = NIL;
    hs->fd = fd;
    _ilu_Assert(ilu_hash_AddToTable(fdh, (ilu_refany) fd, hs),
		"ilutk.c:GetHandlers");
  }
  return hs;
}

static void
CallHandlers(void *clientData, int mask)
{
  Handlers        hs = (Handlers) clientData;
  int             fd = hs->fd;
  while (hs != NIL) {
    if (hs->for_in.h != NULLFN && (mask & (TK_READABLE | TK_EXCEPTION))) {
      mask &= ~(TK_READABLE | TK_EXCEPTION);
      (*hs->for_in.h) (fd, hs->for_in.r);
    } else if (hs->for_out.h != NULLFN && (mask & TK_WRITABLE)) {
      mask &= ~TK_WRITABLE;
      (*hs->for_out.h) (fd, hs->for_out.r);
    } else
      break;
    hs = (Handlers) ilu_hash_FindInTable(fdh, (ilu_refany) fd);
  }
  return;
}

static          ilu_boolean
RegisterInput(int fd, ilu_IOHandler handler, ilu_private rock)
{
  Handlers        hs = GetHandlers(fd);
  int             mask;
  if (hs == NIL)
    return ilu_FALSE;
  if (hs->for_in.h != NULLFN || hs->for_out.h != NULLFN)
    Tk_DeleteFileHandler(fd);
  hs->for_in.h = handler;
  hs->for_in.r = rock;
  mask = (TK_READABLE | TK_EXCEPTION |
	  (hs->for_out.h != NULLFN ? TK_WRITABLE : 0));
  Tk_CreateFileHandler(fd, mask, CallHandlers, hs);
  return ilu_TRUE;
}

static          ilu_boolean
UnregisterInput(int fd, ilu_IOHandler *handler, ilu_private *rock)
{
  Handlers        hs;
  hs = (Handlers) ilu_hash_FindInTable(fdh, (ilu_refany) fd);
  if (hs == NIL)
    return ilu_FALSE;
  *handler = hs->for_in.h;
  *rock = hs->for_in.r;
  Tk_DeleteFileHandler(fd);
  hs->for_in.h = NULLFN;
  hs->for_in.r = NIL;
  if (hs->for_out.h != NULLFN)
    Tk_CreateFileHandler(fd, TK_WRITABLE, CallHandlers, hs);
  else
    _ilu_Assert(ilu_hash_RemoveFromTable(fdh, (ilu_refany) fd) != NIL,
		"ilutk.c:UnregisterInput");
  return ilu_TRUE;
}

static          ilu_boolean
RegisterOutput(int fd, ilu_IOHandler handler, ilu_private rock)
{
  Handlers        hs = GetHandlers(fd);
  int             mask;
  if (hs == NIL)
    return ilu_FALSE;
  if (hs->for_in.h != NULLFN || hs->for_out.h != NULLFN)
    Tk_DeleteFileHandler(fd);
  hs->for_out.h = handler;
  hs->for_out.r = rock;
  mask = (TK_WRITABLE |
      (hs->for_in.h != NULLFN ? (TK_READABLE | TK_EXCEPTION) : 0));
  Tk_CreateFileHandler(fd, mask, CallHandlers, hs);
  return ilu_TRUE;
}

static          ilu_boolean
  UnregisterOutput(int fd, ilu_IOHandler *handler, ilu_private *rock)
{
  Handlers        hs;
  hs = (Handlers) ilu_hash_FindInTable(fdh, (ilu_refany) fd);
  if (hs == NIL)
    return ilu_FALSE;
  *handler = hs->for_out.h;
  *rock = hs->for_in.r;
  Tk_DeleteFileHandler(fd);
  hs->for_out.h = NULLFN;
  hs->for_out.r = NIL;
  if (hs->for_in.h != NULLFN)
    Tk_CreateFileHandler(fd, (TK_READABLE | TK_EXCEPTION),
			 CallHandlers, hs);
  else
    _ilu_Assert(ilu_hash_RemoveFromTable(fdh, (ilu_refany) fd) != NIL,
		"ilutk.c:UnregisterOutput");
  return ilu_TRUE;
}

/* Main Invariant holds; L2 otherwise unconstrained */
typedef void    (*AlarmProc) (ilu_private rock);

typedef struct {
  AlarmProc       proc;
  ilu_private     rock;
  ilu_boolean     isset;
  Tk_TimerToken   tt;		/* significant iff isset */
}              *IluTkAlarm;

static          ilu_refany
CreateAlarm(void)
{
  IluTkAlarm      ita = (IluTkAlarm) ilu_malloc(sizeof(*ita));
  if (ita == NIL)
    return NIL;
  ita->proc = NULLFN;
  ita->rock = NIL;
  ita->isset = ilu_FALSE;
  ita->tt = NIL;
  return ita;
}

static void
CallAlarm(void *clientData)
{
  IluTkAlarm      ita = (IluTkAlarm) clientData;
  _ilu_Assert(ita->isset, "ilutk.c:CallAlarm");
  ita->isset = ilu_FALSE;
  (*ita->proc) (ita->rock);
  return;
}

static void
SetAlarm(ilu_refany alarm, ilu_FineTime t,
	 AlarmProc proc, ilu_private rock)
{
  IluTkAlarm      ita = (IluTkAlarm) alarm;
  static ilu_FineTime zero = {0, 0};
  ilu_cardinal    ms;
  ilu_FineTime    now, dt;
  if (ita->isset)
    Tk_DeleteTimerHandler(ita->tt);
  now = ilu_FineTime_Now();
  dt = ilu_FineTime_Sub(t, now);
  if (ilu_FineTime_Cmp(dt, zero) < 0)
    dt = zero;
  ms = ilu_rescale(dt.ft_t, ilu_FineTimeRate, 1000);
  if (dt.ft_s > (INT_MAX - ms) / 1000)
    ms = INT_MAX;
  else
    ms += dt.ft_s * 1000;
  ita->tt = Tk_CreateTimerHandler(ms, CallAlarm, ita);
  ita->isset = ilu_TRUE;
  ita->proc = proc;
  ita->rock = rock;
  return;
}

static void
UnsetAlarm(ilu_refany alarm)
{
  IluTkAlarm      ita = (IluTkAlarm) alarm;
  if (ita->isset) {
    Tk_DeleteTimerHandler(ita->tt);
    ita->isset = ilu_FALSE;
  }
  return;
}

static void
DestroyAlarm(ilu_refany alarm)
{
  IluTkAlarm ita = (IluTkAlarm) alarm;
  if (ita->isset) {
    Tk_DeleteTimerHandler(ita->tt);
    ita->isset = ilu_FALSE;
  }
  ilu_free(ita);
  return;
}

static ilu_MainLoop synth = {Run, Exit, RegisterInput, UnregisterInput, RegisterOutput, UnregisterOutput, CreateAlarm, SetAlarm, UnsetAlarm, DestroyAlarm};

static ilu_boolean initted = ilu_FALSE;

void
IluTk_Init(void)
{
  if (initted)
    return;
  ilu_SetMainLoop(&synth);
  initted = ilu_TRUE;
  return;
}
