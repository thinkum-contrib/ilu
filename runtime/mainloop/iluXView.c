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
##
##

$Id: iluXView.c,v 1.7 1999/08/03 01:55:48 janssen Exp $

This code integrates the XView GUI toolkit's event loop with the ILU
event loop.  This means that when the application is dormant, waiting
for input events from the X server, it will also notice and process
ILU messages, either requests or replies.

The XView event loop is non-reentrant; that is, an event handler
cannot call back into the event loop to block until some other event
occurs.  However, the ILU event handling model requires reentrant ILU
event handlers.  Therefore this code uses the XView event loop to
handle XView events and initial ILU events, but uses the ILU event
loop to handle nested recursive ILU events.  This has the unfortunate
side effect of not recognizing XView events while waiting for a nested
ILU event (such as the reply to a request) in the ILU event loop, but
in general behaves in the manner that XView users expect.

The strategy is to use the "ilu_AddRegisterersToDefault" call into the
ILU kernel.  This call tells the ILU kernel to register input and
output handlers for particular file descriptors with the XView event
loop, as well as with the ILU event loop.  Every time ILU then opens a
connection to another process, it will register the file descriptor
for that connection with both its own internal event loop, and with
the external event loop specified.  To call
"ilu_AddRegisterersToDefault", the code must provide six functions
with the signatures specified by "ilu_AddRegisterersToDefault".  These
functions register an input handler with XView for a file descriptor,
unregister an XView input handler, register an output handler with
XView for a file descriptor, unregister an XView output handler,
register an "alarm" -- a procedure to be called at a particular time,
and unregister or cancel the alarm.

*/

#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>

#include <xview/xview.h>

#include <iluxport.h>

/*

The "struct rock" structure is a C data structure used to carry
around, in an XView event handler, data that is specific to ILU event
handler operation.  In the following code, we generally mean by "rock"
some opaque structure of user data which is carried around and then
passed back eventually to some domain that understands it.

We define a set of procedures to allocate, find, create, and
destroy "struct rock" values.

*/

typedef struct rock {
  ilu_boolean input;
  int fd;
  ilu_IOHandler handler;
  ilu_private rock;
  struct rock * next;
} * Rock;

static Rock list_of_rocks = NULL;
static Rock unused_rocks = NULL;

static Rock GetRock(int fd, ilu_boolean input)
{
  Rock c;
  for (c = list_of_rocks; c != NULL; c = c->next) {
    if (fd == c->fd && input == c->input) {
      return c;
    }
  }
  return NULL;
}

static void DeleteRock(int fd, ilu_boolean input) 
{
  Rock c, p;
  for (p = NULL, c = list_of_rocks; c != NULL; p = c, c = c->next) {
    if (fd == c->fd && input == c->input) {
      if (p != NULL)
	p->next = c->next;
      else
	list_of_rocks = c->next;
      c->input = ilu_FALSE;
      c->handler = NULL;
      c->rock = NULL;
      c->next = unused_rocks;
      unused_rocks = c;
      return;
    }
  }
}

static Rock NewRock ()
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

static Rock AddRock (int fd,ilu_IOHandler handler, ilu_private rock, ilu_boolean input)
{
  Rock r;
  DeleteRock(fd, input);
  r = NewRock();
  if (r == NULL) {
    fprintf(stderr, "malloc failed in AddRock\n");
    exit(1);
  }
  r->input = input;
  r->fd = fd;
  r->handler = handler;
  r->rock = rock;
  r->next = list_of_rocks;
  list_of_rocks = r;
  return r;
}

/*

Now we define the six procedures we need to pass to
"ilu_AddRegisterersToDefault".  They are "reg_inp", "can_inp",
"reg_out", "can_out", "set_alarm", and "can_alarm".  In addition, we
need two `impedance-matching' helper functions.  The function "func"
maps the XView notion of an input/output event handler to the ILU
notion of such a handler.  The function "timer_func" maps the XView
notion of a timed function call to the ILU notion of such a call.

*/

static Notify_value func(Notify_client client, int fd) 
{
  (*(((Rock ) client)-> handler))(fd, ((Rock ) client)->rock);
  return NOTIFY_DONE;
}

static ilu_boolean reg_inp(int fd, ilu_IOHandler handler, ilu_private rock)
{
  Rock r = AddRock(fd, handler, rock, ilu_TRUE);
  return ((notify_set_input_func((Notify_client) r, (Notify_func) func, fd) != NULL) ? ilu_TRUE : ilu_FALSE);
}


static ilu_boolean can_inp(int fd)
{
  ilu_boolean b;
  Rock r = GetRock(fd, ilu_TRUE);

  if (r != NULL)
    b = ((notify_set_input_func((Notify_client) r, NULL, fd) != NULL) ? ilu_TRUE : ilu_FALSE);
  else
    b = ilu_TRUE;
  DeleteRock(fd, ilu_TRUE);
  return b;
}

static ilu_boolean reg_out (int fd, ilu_IOHandler handler, ilu_private rock) 
{
  Rock r = AddRock(fd, handler, rock, ilu_FALSE);
  return ((notify_set_output_func((Notify_client) r, (Notify_func) func, fd) == NULL) ? ilu_FALSE : ilu_TRUE);
}

static ilu_boolean can_out(int fd)
{
  ilu_boolean b;
  Rock r = GetRock(fd, ilu_FALSE);

  if (r != NULL)
    b = ((notify_set_output_func((Notify_client) r, NULL, fd) != NULL) ? ilu_TRUE : ilu_FALSE);
  else
    b = ilu_TRUE;
  DeleteRock(fd, ilu_FALSE);
  return b;
}

static void (*ilu_timer_proc) (ilu_FineTime t) = NULL;
static ilu_FineTime ilu_timer_time;
static Notify_func ilu_notify_func;

void timer_func (Notify_client client, int which)
{
  if (client == (Notify_client) &ilu_timer_proc && ilu_timer_proc != NULL)
    {
      (*ilu_timer_proc)(ilu_timer_time);
      ilu_notify_func = NULL;
    }
}

static void set_alarm (ilu_FineTime t,
			/*Main Invariant holds;
			  L2 otherwise unconstrained*/
		       void (*proc) (ilu_FineTime t))
{
  struct itimerval ivalue;
  ilu_FineTime initial_timeout = ilu_FineTime_Sub(t, ilu_FineTime_Now());

  fprintf (stderr, "Warning:  ILU asked XView to set an alarm, and this function isn't implemented!\n");
  ilu_timer_proc = proc;
  ilu_timer_time = t;
  ivalue.it_value.tv_sec = initial_timeout.ft_s;
  ivalue.it_value.tv_usec = initial_timeout.ft_t * (1000000/ilu_FineTimeRate);
  ivalue.it_interval.tv_sec = 0;
  ivalue.it_interval.tv_usec = 0;
  ilu_notify_func = notify_set_itimer_func ((Notify_client) &ilu_timer_proc, (Notify_func) timer_func, ITIMER_REAL,
					    &ivalue, NULL);
}

static void can_alarm (void)
{
  notify_set_itimer_func ((Notify_client) &ilu_timer_proc, NOTIFY_FUNC_NULL, ITIMER_REAL, NULL, NULL);
  ilu_timer_proc = NULL;
}

/*

Finally, we define a routine to call to actually register our
registration functions.  It should be called before any ILU work is
done.

*/

void ilu_xview_Initialize (void)
{
  _ilu_Assert(ilu_AddRegisterersToDefault(reg_inp, can_inp, reg_out, can_out, set_alarm, can_alarm),
	      "ilu_xview_Initialize");
}
