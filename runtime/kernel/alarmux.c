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
/* $Id: alarmux.c,v 1.13 1999/08/03 01:52:57 janssen Exp $ */
/* Last edited by Mike Spreitzer May 22, 1995 9:18 am PDT */

#define _POSIX_SOURCE

#include "iluntrnl.h"


/*L1 >= {mxamu}*/
/*L2, Main unconstrained*/

void ilu_MXASet(ilu_AlarmRep *ar, ilu_Alarmette a, ilu_FineTime t)
{
  ilu_Alarmette cur = NIL;
  ilu_FineTime old_trigger = ar->ar_head->al_next->al_trigger;
  ilu_boolean none = ar->ar_head->al_next == ar->ar_head;
  if (a->al_set == ilu_TRUE) {
      _ilu_Assert(a->al_next->al_prev == a && a->al_prev->al_next == a,
		  "ilu_MXACancel 1");
      if (ilu_FineTime_Eq(t, a->al_trigger))
          return;
      a->al_next->al_prev = a->al_prev;
      a->al_prev->al_next = a->al_next;
      cur = a->al_next;
    }
  else {
      _ilu_Assert(a->al_next == NIL && a->al_prev == NIL,
		  "ilu_MXACancel 2");
      a->al_set = ilu_TRUE;
      cur = ar->ar_head->al_next;
    }
  a->al_trigger = t;
  while ((cur != ar->ar_head) && (ilu_FineTime_Cmp(t, cur->al_trigger) > 0))
      cur = cur->al_next;
  while ((cur->al_prev != ar->ar_head)
	 && (ilu_FineTime_Cmp(t, cur->al_prev->al_trigger) < 0))
      cur = cur->al_prev;
  _ilu_Assert(cur->al_prev->al_next == cur,
	      "ilu_MXACancel 3");
  a->al_next = cur;
  a->al_prev = cur->al_prev;
  a->al_next->al_prev = a;
  a->al_prev->al_next = a;
  if (none || !ilu_FineTime_Eq(ar->ar_head->al_next->al_trigger, old_trigger))
      (*ar->ar_set)(ar->ar_head->al_next->al_trigger);
  return;
}

void ilu_MXAClear(ilu_AlarmRep *ar, ilu_Alarmette a)
{
  ilu_FineTime old_trigger;
  if (a->al_set == ilu_TRUE) {
      _ilu_Assert(a->al_next->al_prev == a && a->al_prev->al_next == a,
		  "ilu_MXACancel");
      old_trigger = ar->ar_head->al_next->al_trigger;
      a->al_set = ilu_FALSE;
      a->al_next->al_prev = a->al_prev;
      a->al_prev->al_next = a->al_next;
      a->al_prev = NIL;
      a->al_next = NIL;
      if (ar->ar_head->al_next == ar->ar_head)
           (*ar->ar_cancel)();
      else if (!ilu_FineTime_Eq(ar->ar_head->al_next->al_trigger, old_trigger))
           (*ar->ar_set   )(ar->ar_head->al_next->al_trigger);
      else {}
    }
  return;
}

/*L1_sup = mxamu*/
void ilu_MXAProc(ilu_FineTime u, ilu_AlarmRep *ar)
{
  ilu_Alarmette cur;
  cur = ar->ar_head->al_next;
  while ((cur != ar->ar_head) && (ilu_FineTime_Cmp(u, cur->al_trigger) >= 0)) {
      while ((cur != ar->ar_head) && (ilu_FineTime_Cmp(u, cur->al_trigger) >= 0)) {
          _ilu_Assert(cur->al_next->al_prev == cur && cur->al_prev->al_next == cur,
		      "ilu_MXAProc");
          cur->al_next->al_prev = cur->al_prev;
          cur->al_prev->al_next = cur->al_next;
          cur->al_set = ilu_FALSE;
          cur->al_next = cur->al_prev = NIL;
          (*ar->ar_invoke)(cur);
          cur = ar->ar_head->al_next;
        }
      u = ilu_FineTime_Now();
    }
  if (ar->ar_head->al_next == ar->ar_head)
       (*ar->ar_cancel)();
  else (*ar->ar_set   )(ar->ar_head->al_next->al_trigger);
  return;
}

