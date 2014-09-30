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

  $Id: iluXt.c,v 1.12 1999/08/03 01:55:47 janssen Exp $


  This code integrates the Xt GUI toolkit's event loop with the ILU
  event loop.  This means that when the application is dormant, waiting
  for input events from the X server, it will also notice and process
  ILU messages, either requests or replies.  Similarly, when the ILU
  system is blocked, waiting, for example, for a reply to a request,
  the system will still notice and process X events.

  The strategy used is to define a complete new event loop, using the
  capability for the user to define new event loops provided by both
  Xt and ILU.  To do this, we need to define 9 functions which provide
  the following functions:

  - call the event processing loop, possibly recursively
  - return from a specific level of the event loop
  - register a function to be called when input is available on an FD
  - unregister an input handler
  - register a function to be called when output is possible on an FD
  - unregister an output handler
  - create an "alarm" - a (time,proc) pair in which "proc" is invoked
  when "time" is reached
  - set the time and proc of an alarm
  - cancel (but not destroy) an alarm

  We then register these with the ILU kernel by a call to
  "ilu_SetMainLoop".

  How to use this:

  You would first call ilu_Xt_Initialize(NULL, NULL) to set things up,
  then do the ILU and Xt stuff, then call ilu_RunMainLoop (int *).
  Otherwise use all Xt calls.  Only dispatches events on the default Xt
  app context; if more app contexts are desired, re-write the default
  "default_ilu_Xt_run" and "default_ilu_Xt_exit" routines from this code
  to handle multiple app contexts, and pass them as arguments to
  ilu_Xt_Initialize.

  */
/* Last edited by Mike Spreitzer June 5, 1996 5:25 pm PDT */

void ilu_Xt_Initialize (void (*user_run_main_loop)(int *), void (*user_exit_main_loop)(int *));

#include <stdio.h>
#include <iluxport.h>		/* the ILU header files */
#include <stdlib.h>		/* for malloc */
#include <Intrinsic.h>		/* the Xt header files */

     /*

       The "struct rock" structure is a C data structure used to carry
       around, in an XView event handler, data that is specific to ILU event
       handler operation.  In the following code, we generally mean by "rock"
       some opaque structure of user data which is carried around and then
       passed back eventually to some domain that understands it.

       We define a set of procedures to allocate, find, create, and
       destroy "struct rock" values.

       */

     typedef enum direction_e { Input, Output } Direction;

     typedef struct rock {
       Direction dir;
       int fd;
       ilu_IOHandler handler;
       ilu_private rock;
       XtInputId id;
       struct rock * next;
     } * Rock;

     static Rock list_of_rocks = NULL;
     static Rock unused_rocks = NULL;

     static Rock FindRock(int fd, Direction dir)
{
  Rock c;
  for (c = list_of_rocks; c != NULL; c = c->next) {
    if (fd == c->fd && dir == c->dir) {
      return c;
    }
  }
  return NULL;
}

static void DeleteRock(int fd, Direction dir)
{
  Rock c, p;
  for (p = NULL, c = list_of_rocks; c != NULL; p = c, c = c->next) {
    if (fd == c->fd && dir == c->dir) {
      if (p != NULL)
	p->next = c->next;
      else
	list_of_rocks = c->next;
      c->handler = NULL;
      c->rock = NULL;
      c->next = unused_rocks;
      unused_rocks = c;
      return; /* Added GWT 08/16/96 */
    }
  }
}

static Rock NewRock (void)
{
  Rock r;

  if (unused_rocks == NULL)
    return (Rock) malloc(sizeof(* r));
  else
    {
      r = unused_rocks;
      unused_rocks = r->next;
      return (r);
    }
}

static Rock AddRock(int fd, ilu_IOHandler handler, ilu_private rock, Direction dir)
{
  Rock r;
  DeleteRock(fd, dir);
  r = NewRock();
  if (r == NULL) {
    fprintf(stderr, "malloc failed in AddRock\n");
    exit(1);
  }
  r->dir = dir;
  r->fd = fd;
  r->handler = handler;
  r->rock = rock;
  r->next = list_of_rocks;
  list_of_rocks = r;
  return r;
}

/*

  Now we define the main event loop procedure, "default_ilu_Xt_run".  It
  uses a "stack frame" structure to keep track of which recursive
  invocations of itself have been returned from.

  */

typedef struct stack_s {
  int *stop;
  struct stack_s *next;
} StackFrame;

static StackFrame *run_stack = NULL;

extern XtAppContext _XtDefaultAppContext(void);

static void
  default_ilu_Xt_run (int *stop)
{
  /*
    If the XtAppNextEvent (), XtDispatchEvent() function pair
    is used instead of XtAppProcessEvent(), alternate input sources are
    not read until an X event occurs.  Bug in Xt? (GWT)
    */

  /*    XEvent event; */
  StackFrame ours;

  XtAppContext app = _XtDefaultAppContext();

  /* push our "frame" onto the "stack" */
  ours.next = run_stack;
  ours.stop = stop;
  run_stack = &ours;

  *stop = 1;

  while (*stop != 0) {
    /*	XtAppNextEvent(app, &event); */
    /*	XtDispatchEvent(&event); */
    XtAppProcessEvent (app, XtIMAll);
  }

  /* pop our "frame" from the stack */
  run_stack = ours.next;

  return;
}

static void
  default_ilu_Xt_exit (int *stop)
{

  StackFrame *p;

  /* Changed by GWT to fix problem across multiple hosts */
  for (p = run_stack;  p != NULL;  p = p->next) {
    if (p->stop == stop) {
      *(p->stop) = 0;
      return;  /* Changed from 'break' */
    }
  }
  ilu_DebugPrintf ("ILU Xt ExitMainLoop:  bad \"stop\" parameter, %p, passed.\n", stop);
#ifdef ORIGINAL_ILU_CODE
  for (p = run_stack;  p != NULL;  p = p->next)
    if (p->stop == stop)
      break;
  if (p == NULL) {	/* that frame isn't on the stack! */
    ilu_DebugPrintf(
	     "ILU Xt ExitMainLoop:  bad \"stop\" parameter, %p, passed.\n",
	     stop);
    return;
  }
  for (p = run_stack;  p != NULL;  p = p->next) {
    /* mark all intervening frames to exit */
    *(p->stop) = 0;
    if (p->stop == stop)
      break;
  }
#endif
  return;
}

/*

  Now we define the four input/output handler functions.  They basically
  map the ILU event loop's notion of a handler callback, to the notion
  of a handler callback that we need to use in our custom event loop.
  They use XtAddInput, etc., because our custom event loop uses the Xt
  conventions for writing custom event loops.

  */

static void
  callback_proc (Rock client_data,
		 int *fd,
		 XtInputId *id)
{
  client_data->handler (client_data->fd, client_data->rock);
}

static ilu_boolean 
  register_input (int fd,
		  ilu_IOHandler handler,
		  ilu_private rock)
{
  Rock r;
  XtAppContext app = _XtDefaultAppContext();

  DeleteRock (fd, Input);
  r = AddRock (fd, handler, rock, Input);
  r->id = XtAppAddInput (app, fd, (XtPointer)XtInputReadMask,
			 (XtInputCallbackProc)callback_proc, r);
  return ((r->id != (XtInputId)NULL) ? ilu_TRUE : ilu_FALSE);
}

static ilu_boolean 
  unregister_input (int fd,
		    ilu_IOHandler *handler,
		    ilu_private *rock)
{
  Rock r = FindRock (fd, Input);
  if (r != NULL)
    {
      *handler = r->handler;
      *rock = r->rock;
      XtRemoveInput (r->id);
      DeleteRock (fd, Input);
      return (ilu_TRUE);
    }
  else
    return (ilu_FALSE);
}

static ilu_boolean 
  register_output (int fd,
		   ilu_IOHandler handler,
		   ilu_private rock)
{
  Rock r;
  XtAppContext app = _XtDefaultAppContext();

  DeleteRock (fd, Output);
  r = AddRock (fd, handler, rock, Output);
  r->id = XtAppAddInput (app, fd, (XtPointer)XtInputWriteMask,
			 (XtInputCallbackProc)callback_proc, r);
  return ((r->id != (XtInputId)NULL) ? ilu_TRUE : ilu_FALSE);
}

static ilu_boolean 
  unregister_output (int fd,
		     ilu_IOHandler *handler,
		     ilu_private *rock)
{
  Rock r = FindRock (fd, Output);
  if (r != NULL)
    {
      *handler = r->handler;
      *rock = r->rock;
      XtRemoveInput (r->id);
      DeleteRock (fd, Output);
      return (ilu_TRUE);
    }
  else
    return (ilu_FALSE);
}

/*

  To implement "alarms", we use the Xt notion of Timeouts.  Here are the
  three functions needed to implement ILU alarms, plus an
  `impedance-matching' helper function "handle_timer" used to map the Xt
  notion of a timer callback to the ILU notion of an alarm callback.

  */

typedef struct ilu_xt_alarm_s {
  XtIntervalId id;
  void (*proc) (ilu_private rock);
  ilu_private rock;
  unsigned long timeout;
} * iluXtAlarm;

static void
  handle_timer (void * client_data, XtIntervalId *id)
{
  iluXtAlarm al = (iluXtAlarm) client_data;
  
  (*(al->proc)) (al->rock);
  al->id = 0;
  al->proc = 0;
  al->rock = 0;
  al->timeout = 0;

  return;
}

static void *
  create_alarm (void)
{
  iluXtAlarm al = (iluXtAlarm) malloc(sizeof(struct ilu_xt_alarm_s));
  al->id = 0;
  al->proc = 0;
  al->rock = 0;
  al->timeout = 0;
  return ((void *) al);
}

static void
  set_alarm (void * alarm,
	     ilu_FineTime t,
	     void (*proc) (ilu_private rock),
	     ilu_private rock)
{
  ilu_FineTime t2;
  iluXtAlarm al = (iluXtAlarm) alarm;
  XtAppContext app = _XtDefaultAppContext();

  if (al->id != 0)	/* alarm already set */
    XtRemoveTimeOut (al->id);	/* clear current setting */
  al->proc = proc;
  al->rock = rock;
  t2 = ilu_FineTime_Sub(t, ilu_FineTime_Now());
  al->timeout = t2.ft_s * 1000 + (t2.ft_t * (1000/ilu_FineTimeRate));
  al->id = XtAppAddTimeOut (app, al->timeout,
			    (XtTimerCallbackProc)handle_timer, (void *) al);

  return;
}

static void
  clear_alarm (void * alarm)
{
  iluXtAlarm al = (iluXtAlarm) alarm;

  if (al->id != 0)
    XtRemoveTimeOut (al->id);
  al->id = 0;
  al->proc = 0;
  al->rock = 0;
  al->timeout = 0;

  return;
}

/*

  Finally, here is the function to call to initialize the event loop.
  It should be called before doing any Xt operations or any ILU
  operations.  It takes two parameters, which should normally be
  specified as NULL, but which can be used to override our custom event
  loop, and allow the user to provide super-custom replacements for
  "default_ilu_Xt_run" and "default_ilu_Xt_exit".

  */

void
  ilu_Xt_Initialize (void (*user_run_main_loop)(int *),
		     void (*user_exit_main_loop)(int *))
{
  static ilu_MainLoop ml;

  ml.ml_run = (user_run_main_loop == NULL || user_exit_main_loop == NULL) ?
    default_ilu_Xt_run : user_run_main_loop;
  ml.ml_exit = (user_run_main_loop == NULL || user_exit_main_loop == NULL) ?
    default_ilu_Xt_exit : user_exit_main_loop;
  ml.ml_register_input = register_input;
  ml.ml_unregister_input = unregister_input;
  ml.ml_register_output = register_output;
  ml.ml_unregister_output = unregister_output;
  ml.ml_create_alarm = create_alarm;
  ml.ml_set_alarm = set_alarm;
  ml.ml_unset_alarm = clear_alarm;

  ilu_SetMainLoop (&ml);

  return;
}
