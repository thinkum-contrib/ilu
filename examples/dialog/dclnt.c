/** $Id: dclnt.c,v 1.8 1999/08/03 01:57:25 janssen Exp $
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
/* Last edited by Mike Spreitzer October 8, 1998 10:46 pm PDT */

#include <stdio.h>

#include "Dialog.h"

static ILU_C_InterruptHandle intH = ILU_NIL;

static void
Timeout(ilu_private rock)
{
  ILU_C_InterruptHandleCalls(intH);
  return;
}

int
main(int argc, char *argv[])
{
  Dialog_T        t;
  ILU_C_ENVIRONMENT env;
  ilu_refany      timeout_alarm;
  ilu_FineTime    now, dt, timeout;
  if (argc != 2) {
    fprintf(stderr, "Usage: %s server-sbh\n", argv[0]);
    exit(1);
  }
  Dialog__Initialize();
  intH = ILU_C_NewInterruptHandle();
  timeout_alarm = ilu_CreateAlarm();
  t = Dialog_T__CreateFromSBH(argv[1], &env);
  if (t == NULL) {
    fprintf(stderr, "Unable to create ILU object!\n");
    exit(1);
  }
  ILU_C_SetObjectInterruptHandle(t, intH);
  dt = ilu_FineTime_FromDouble(60);
  for (;;) {
    char            buf[1000], *s1, *s2;
    printf("Next: ");
    s1 = fgets(buf, 1000, stdin);
    if (s1 != buf) {
      fprintf(stderr, "Error reading arg!\n");
      exit(1);
    }
    now = ilu_FineTime_Now();
    timeout = ilu_FineTime_Add(now, dt);
    ilu_SetAlarm(timeout_alarm, timeout, Timeout, ILU_NIL);
    s2 = Dialog_T_M(t, s1, &env);
    if (!ILU_C_SUCCESSFUL(&env)) {
      fprintf(stderr, "M() => %s\n", ILU_C_EXCEPTION_ID(&env));
    } else {
      printf("Answer is '%s'\n", s2);
    }
  }
}
