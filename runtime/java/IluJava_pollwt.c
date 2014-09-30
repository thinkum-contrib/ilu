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
/* IluJava_pollwt.c */
/* Chris Jacobi, November 18, 1998 1:10 pm PST */

/*
 */
 
/* $Id: IluJava_pollwt.c,v 1.26 1999/08/03 01:54:34 janssen Exp $ */
 
/* 
 * WaitTech implementation with poll
 */


#include "IluJava_Includes.h"
#include "IluJava_Common.h"
#include <poll.h>
#include <errno.h>


#define DEBUG debug > 0
static int debug = 0;
static ilu_FineTime _ilujava_z = {0, 0};


#if defined(JAVA_H_NATIVE_THREADS) 
#elif defined(NOT_USED_ANYMORE_introFD)
    
    static int highestFd = -1;

    static void
    introFD(int fd)
    /* Introduce the fd because the java vm wants a call to initialize_monitors.
     * Beats me why it doesn't do that itself.
     * However with native methods it looks like this is neither necessary nor 
     * is the symbol initialize_monitors defined anymore 
     */
    {
        /* we (used to) want this only if ILUJAVA_H_NATIVE_THREADS is not  
         * defined and the version is 1.1, or 1.2beta2 but not in 
         * higher versions
         */
        extern int initialize_monitors(int);
            /* defined in java...iomgr.c 
             * (With different return arg, but we ignore it)
             */
        /* No need to synchronize as initialize_monitors is idempotent. */
        if (fd>highestFd) {
            int i;
            for (i = highestFd+1; i<=fd; i++) {
                initialize_monitors(i);
            } 
            highestFd = fd;
        }
    } /*introFD*/
    
#endif


INTERNALONLY int 
_ilujava_TimoutToMilli(ilu_FineTime * limit)
{
    if (limit) {
        ilu_FineTime now = ilu_FineTime_Now();
        ilu_FineTime wait;
        wait = ilu_FineTime_Sub(*limit, now);
        if (ilu_FineTime_Cmp(wait, _ilujava_z) > 0) {
            if (wait.ft_s < 1000000) {
                return wait.ft_s * 1000 +
                         (int) ilu_rescale(wait.ft_t, ilu_FineTimeRate, 1000);
            } else {
                return 1000000000;
            }
        } else {
            return 0; /* no timeout */
        }
    } else {
        return -1;  /* infinite timeout */
    }
}


INTERNALONLY void 
_ilujava_Waiter(int fd, 
       int auxfd,
       ilu_boolean * sure,
       ilu_FineTime * limit, 
       ilu_boolean input,
       ILU_ERRS((interrupt)) * errs)
{
    extern int _poll(struct pollfd *fds, unsigned long nfds, int timeout);
        /* defined somewhere ... 
         */
    struct pollfd   p[2];
    int             timeout, res; 
    int             theerr = EAGAIN;
    unsigned long   nfds = 1;
    JENV_DECLARE_INITFROMTHINAIR

    #if defined(introFD)
        if (fd > highestFd) {introFD(fd);}
    #endif
    if (auxfd != -1) {
        nfds = 2;
        #if defined(introFD)
            if (auxfd > highestFd) {introFD(auxfd);}
        #endif
    }
        /* 
         * Ignore the result; it is a fatal error if it returns FALSE
         * but the type for FALSE is not exported by java.
         *
         * If somebody knows how to fix the compile time warning correctly
         * please tell me.
         */

    if (JPENDINGEXCEPTION()) {
        JTRACE(DEBUG, ("$ _ilujava_poll: pending java exception\n"));
        *sure = ilu_FALSE;
        ILU_ERR_CONS1(interrupted, errs, ilu_interruptSet, 0, (void)0);
        return;
    }
    
    while (theerr == EAGAIN) {
        p[0].fd = fd;
        p[0].events = (input ? POLLIN : POLLOUT) | POLLPRI;
        p[0].revents = 0;
        if (auxfd != -1) {
            p[1].fd = auxfd;
            p[1].events = POLLIN | POLLPRI;
            p[1].revents = 0;
        }
        timeout = _ilujava_TimoutToMilli(limit);
        
        JTRACE(DEBUG, ("$ _ilujava_poll: timeout %d  fd %d  pending %d\n", 
                timeout, p[0].fd, JPENDINGEXCEPTION()));
                        
        if (timeout == 0) {  
            /* Use the real system call instead the java
             * redefined version.  The java redefined
             * version in jdk1.0.2 blocks the java thread 
             * even if it wouldn't block the unix process.
             */
            res = _poll(p, nfds, 0);
        } else {
            /* Use the java redefined system call as it seems 
             * to work fine and we can't implement blocking 
             * without knowledge of the java scheduler.
             */
            res = poll(p, nfds, timeout);
        }

        JTRACE(DEBUG, ("$ _ilujava_poll: returns %d\n", res));
        
        if (res >= 0) {
            *sure = res > 0;
            ILU_CLER(*errs);
            return;
        }
        theerr = errno;
    }

    if (theerr == EINTR) {
        /* thread has been asked to interrupt its current call */
        *sure = ilu_FALSE;
        ILU_ERR_CONS1(interrupted, errs, ilu_interruptSet, 0, (void)0);
        JTRACE(DEBUG, ("$ _ilujava_poll: errno=EINTR\n"));
    } else {
        /* exceptional condition exists on the FD */
        *sure = ilu_TRUE;
        JTRACE(1, ("$ _ilujava_poll: errno %d\n", theerr));
        perror("_ilujava_poll ERROR IS");
        ilu_Check(0, errs);
    }

    return;
}


INTERNALONLY void 
_ilujava_WaitIn(int fd, int auxfd, ilu_boolean * sure,
        ilu_FineTime * limit,
        ILU_ERRS((interrupt)) * errs)
/* This is an ilu_FDWaitProc */
{
    JTRACE(DEBUG, ("$ _ilujava_poll: (in) %d\n", fd));
    _ilujava_Waiter(fd, auxfd, sure, limit, ilu_TRUE, errs);
}


INTERNALONLY void 
_ilujava_WaitOut(int fd, int auxfd, ilu_boolean * sure,
         ilu_FineTime * limit,
         ILU_ERRS((interrupt)) * errs)
/* This is an ilu_FDWaitProc */
{
    JTRACE(DEBUG, ("$ _ilujava_poll: (out) %d\n", fd));
    _ilujava_Waiter(fd, auxfd, sure, limit, ilu_FALSE, errs);
}


INTERNALONLY ilu_WaitTech 
_ilujava_pollWT = {_ilujava_WaitIn, _ilujava_WaitOut};


EXPORTLIMITED void
_ilujava_WaitTechInit()
{
    /* TEST ???  This used to crash JNI based implementation
       I want to debug more essential things first
    debug = _ilujava_getIntProp("ilu.debug.poll", 0);
    */
    ilu_SetWaitTech(&_ilujava_pollWT);
}


/* end */


