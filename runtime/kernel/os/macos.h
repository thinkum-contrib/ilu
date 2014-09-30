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
 * macos.h -- MacOS specific macro definitions for library and system calls
 *
 * Dan B. Keith    December 1996
 */

#ifndef MACOS_H
#define MACOS_H


/*OLDSTUFF - Probably should be deleted...
 * macos.h -- MacOS specific macro definitions for library and system calls
 *
 * Antony A. Courtney, 17/7/93

#include "support.h"

#define OS_BCOPY		ilu_sup_bcopy
#define OS_GETPID		ilu_sup_getpid
#define OS_SIGVEC		SIGVEC_NOT_SUPPORTED

#ifndef ANSI_STRERROR
#define ANSI_STRERROR(a)	strerror(a)
#endif
 */


#ifdef __cplusplus
extern "C" {
#endif

#include	<unistd.h>

int access(const char *path, int oflag);

int chmod(const char *path, int mode);

unsigned int mac_sleep(unsigned int sleep);

#define OS_SLEEP		mac_sleep	/* memory.c */
#define OS_READ			read	/* mainloop.c */
#define OS_WRITE		write	/* mainloop.c */
#define OS_GETPID		getpid	/* bsdutils.c */
#define OS_UNLINK		unlink	/* simpbind.c */
#define OS_ACCESS		access	/* simpbind.c */
#define OS_CHMOD		chmod	/* simpbind.c */
#define OS_SIGVEC		SIGVEC_NOT_SUPPORTED

#define OS_ACCESS_R_OK		0x04
#define OS_ACCESS_W_OK		0x02

#define OS_CHMOD_S_IWUSR	1	/* simpbind.c */
#define OS_CHMOD_S_IRUSR	0	/* simpbind.c */
#define OS_CHMOD_S_IWGRP	1	/* simpbind.c */
#define OS_CHMOD_S_IRGRP	0	/* simpbind.c */
#define OS_CHMOD_S_IWOTH	1	/* simpbind.c */
#define OS_CHMOD_S_IROTH	0	/* simpbind.c */

#ifndef ANSI_STRERROR
#define ANSI_STRERROR(a)	strerror(a)
#endif


#define OS_SOCKIOCTL(fd,req,arg)	ioctlsocket(fd,req,arg)	/* tcp.c, udp.c */
/* Larner removed casts to int from third arg in his WIN port */

#define OS_ACCEPT(fd,adr,len)	accept(fd,adr,len)	/* tcp.c */

#define OS_SOCKINV(x) ((x) == INVALID_SOCKET)	/* tcp.c, udp.c */
/*
 * Call this on result/errcode to test whether a fd-returning
 * sockets call is raising an error
 */

#define OS_SOCKERR(x) ((x) == SOCKET_ERROR)	/* tcp.c, udp.c */
/*
 * Call this on result/errcode to test whether a non-fd-returning
 * sockets call is raising an error
 */

#define OS_SOCKLOSE(x)	closesocket(x)	/* tcp.c, udp.c */
/* Call this to close an FD for a socket */

#define SOCKERRID(x)	(WSAE##x)
#define sockerrno	WSAGetLastError()

void StartupWinsock();
void ShutdownWinsock();

#ifdef __cplusplus
};
#endif

#endif


