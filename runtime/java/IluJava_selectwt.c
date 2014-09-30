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
/* IluJava_selectwt.c */
/* Chris Jacobi, December 1, 1998 2:48 pm PST */
/* $Id: IluJava_selectwt.c,v 1.28 1999/08/03 01:54:24 janssen Exp $ */
 
/*
 * WaitTech implementation with select
 * Alternative to IluJava_pollwt.c
 */


#include "IluJava_Includes.h"
#include "IluJava_Common.h"
#include <sys/types.h>
#include <errno.h>

#ifndef WIN32
#include <sys/time.h>
#else
#include <winsock.h>
#endif


static int debug = 0;
static ilu_FineTime _ilujava_z0 = {0, 0};


INTERNALONLY void 
_ilujava_sWaiter(int fd, 
       int auxfd,
       ilu_boolean * sure,
       ilu_FineTime * limit, 
       ilu_boolean input,
       ILU_ERRS((interrupt)) * errs)
{
    fd_set   read_fds; 
    fd_set   write_fds; 
    fd_set   excn_fds;
    struct timeval* timeoutPtr = 0;
    struct timeval  timeout = {0, 0};
    int             res; 
    struct fd_set*  read_fdsPtr = 0;
    struct fd_set*  write_fdsPtr = 0;
    JENV_DECLARE_INITFROMTHINAIR
    {
#ifndef WIN32
        if (fd >= FD_SETSIZE) {
            _ilu_Assert(fd < FD_SETSIZE, "IluJava_selectwt: fd too large");
        }
#endif
        JTRACE(debug, ("$ _ilujava_select: calling fd=%d, input=%d\n", 
                fd, input));
        ILU_CLER(*errs);
        FD_ZERO(&excn_fds);
        FD_SET(fd, &excn_fds);
        if (input) {
            FD_ZERO(&read_fds);
            FD_SET(fd, &read_fds);
            read_fdsPtr = &read_fds;
            if (auxfd>0) {
                FD_SET(auxfd, &excn_fds);
                FD_SET(auxfd, &read_fds);
                if (auxfd > fd) fd = auxfd;
            }
        } else {
            FD_ZERO(&write_fds);
            FD_SET(fd, &write_fds);
            write_fdsPtr = &write_fds;
            if (auxfd>0) {
                FD_SET(auxfd, &excn_fds);
                FD_ZERO(&read_fds);
                FD_SET(auxfd, &read_fds);
                read_fdsPtr = &read_fds;
                if (auxfd > fd) fd = auxfd;
            }
        }
        if (limit) {
             ilu_FineTime    now = ilu_FineTime_Now();
             ilu_FineTime    wait = ilu_FineTime_Sub(*limit, now);
             JTRACE(debug, ("$ _ilujava_select: USES TIMEOUT"));
             if (ilu_FineTime_Cmp(wait, _ilujava_z0) > 0) {
                 timeout.tv_sec = wait.ft_s;
                 timeout.tv_usec = (int) 
                     ilu_rescale(wait.ft_t, ilu_FineTimeRate, 1000000);
                 timeoutPtr = &timeout;
             } 
        }
        res = select(fd+1, read_fdsPtr, write_fdsPtr, &excn_fds, timeoutPtr);
            /* Unlike in IluJava_pollwt.c we can not use _select directly
             * here because _select seems not to exist and is re-written
             * in jdk1.0.2 at least.
             */
        if (res < 0) {
            /* error */
            int theerr = errno;
            if (theerr == EINTR) {
                /* thread has been asked to interrupt */
                *sure = ilu_FALSE;
                ILU_ERR_CONS1(interrupted, errs, ilu_interruptSet, 0, (void) 6);
                return;
            } else {
                /* other error */
                *sure = ilu_TRUE;
                ilu_Check(theerr == EINTR, errs);
                return;
            }
        }
        if (res = 0) {
            /* time limit expired */
            *sure = ilu_FALSE;
            return;
        }
        if (res > 0) {
            /* somefile descriptor is ready */
            *sure = ilu_TRUE;
            return;
        }
    }
}


INTERNALONLY void 
_ilujava_SWaitIn(int fd, int auxfd, ilu_boolean * sure,
        ilu_FineTime * limit,
        ILU_ERRS((interrupt)) * errs)
/* This is an ilu_FDWaitProc */
{
    JTRACE(debug, ("$ _ilujava_poll: (in) %d\n", fd));
    _ilujava_sWaiter(fd, auxfd, sure, limit, ilu_TRUE, errs);
}


INTERNALONLY void 
_ilujava_SWaitOut(int fd, int auxfd, ilu_boolean * sure,
        ilu_FineTime * limit,
        ILU_ERRS((interrupt)) * errs)
/* This is an ilu_FDWaitProc */
{
    JTRACE(debug, ("$ _ilujava_poll: (out) %d\n", fd));
    _ilujava_sWaiter(fd, auxfd, sure, limit, ilu_FALSE, errs);
}


INTERNALONLY ilu_WaitTech 
_ilujava_selectWT = {_ilujava_SWaitIn, _ilujava_SWaitOut};


EXPORTLIMITED void
_ilujava_WaitTechInit()
{
    debug = _ilujava_getIntProp("ilu.debug.poll", 0);
    ilu_SetWaitTech(&_ilujava_selectWT);
}

/* end */
