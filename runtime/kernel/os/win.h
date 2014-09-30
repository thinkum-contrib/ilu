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
 * win.h -- Windows definitions for system calls
 */
/* $Id: win.h,v 1.9 1999/08/03 01:52:50 janssen Exp $ */
/* Last edited by Mike Spreitzer October 24, 1995 4:24 pm PDT */

/* dll - <unistd.h> doesn't exist for VC++ */
#include <io.h>
#include <stdio.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <process.h>
#define OS_SLEEP        _sleep  /* memory.c */
#define OS_READ         _read   /* mainloop.c */
#define OS_WRITE        _write  /* mainloop.c */
#define OS_PIPE(fildes)     _pipe(fildes, 16, _O_BINARY)    /* mainloop.c */
#define OS_GETPID       _getpid /* bsdutils.c */
#define OS_UNLINK       _unlink /* simpbind.c */
#define OS_ACCESS       _access /* simpbind.c */
#define OS_CHMOD        _chmod  /* simpbind.c */
#define OS_ACCESS_R_OK      0x04    /* simpbind.c */
#define OS_ACCESS_W_OK      0x02    /* simpbind.c */
#include <sys/types.h>
#include <sys/stat.h>
// xxx - dll - define these as all just read or write for now until
// determine what to do regarding NT ACLs - also, we may be on a FAT
// volume anyway, where there is no file ownership anyhow
#define OS_CHMOD_S_IWUSR    _S_IWRITE   /* simpbind.c */
#define OS_CHMOD_S_IRUSR    _S_IREAD    /* simpbind.c */
#define OS_CHMOD_S_IWGRP    _S_IWRITE   /* simpbind.c */
#define OS_CHMOD_S_IRGRP    _S_IREAD    /* simpbind.c */
#define OS_CHMOD_S_IWOTH    _S_IWRITE   /* simpbind.c */
#define OS_CHMOD_S_IROTH    _S_IREAD    /* simpbind.c */

#define OS_UNAME        uname   /* bsdutils.c */


#define OS_SOCKIOCTL(fd,req,arg)    ioctlsocket(fd,req,arg) /* tcp.c, udp.c */
/* Larner removed casts to int from third arg in his WIN port */

#define OS_ACCEPT(fd,adr,len)   accept(fd,adr,len)  /* tcp.c */

#define OS_SOCKINV(x) ((x) == INVALID_SOCKET)   /* tcp.c, udp.c */
/*
 * Call this on result/errcode to test whether a fd-returning
 * sockets call is raising an error
 */

#define OS_SOCKERR(x) ((x) == SOCKET_ERROR) /* tcp.c, udp.c */
/*
 * Call this on result/errcode to test whether a non-fd-returning
 * sockets call is raising an error
 */

#define OS_SOCKLOSE(x)  closesocket(x)  /* tcp.c, udp.c */
/* Call this to close an FD for a socket */

#define SOCKERRID(x)    (WSAE##x)
#define sockerrno   WSAGetLastError()
