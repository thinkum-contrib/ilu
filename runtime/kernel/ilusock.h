/** $Id: ilusock.h,v 1.11 1999/08/03 01:52:56 janssen Exp $
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
/* Last edited by Mike Spreitzer October 20, 1998 11:09 pm PDT */

/*
  NOTE:

  This file is processed at configure time, to see if it defines
  TCP_NODELAY by the headers it includes.  No ILU types are defined
  at that time, nor is iluconf.h available.  Therefore, any mention
  of ILU types (ilu_string, ilu_cardinal, etc) inside this file
  should be bracketed with #ifndef ILU_CONFIGURE_TIME/#endif.
*/

#ifndef _ILU_SOCKET_HEADERS
#define _ILU_SOCKET_HEADERS

#include <sys/types.h>

#if (defined(WIN32) || defined(WIN16) || defined( macintosh )) 
/* sockets are in winsock.h for WIN32 */
#include <winsock.h>

#if defined( macintosh )
#include	<sys/errno.h>
#endif

#else

#ifndef ILU_CONFIGURE_TIME
#ifndef HAS_RECV_AND_SEND_PROTOTYPES
/* SunOS4.x doesn't have declarations for these in sys/socket.h ! */
int send(/*int sock, char*msg, int len, int flags*/);
int recv(/*int sock, char*buf, int len, int flags*/);
/*
 * Declarations using prototypes would be inappropriate, due to
 * subtle differences in parameter passing.  I wonder if all gcc
 * configurations actually lack declarations of these?
 */
#endif /* not HAS_RECV_AND_SEND_PROTOTYPES */
#endif /* ndef ILU_CONFIGURE_TIME */

#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/ioctl.h>
#include <arpa/inet.h>		/* for inet_addr, inet_ntoa */
#include <netdb.h>
#endif /* not WIN32 */

/* BSD socket network calls used:

   accept, bind, connect, getsockname, listen, setsockopt, socket, getpeername
   gethostbyname, inet_addr, inet_ntoa, ntohs

   We also use the "close" system call to close socket connections.
*/

#ifdef __GNU_LIBRARY__
typedef size_t SOCKET_SIZE_TYPE;
#else /* doesn't define size_t */
typedef int SOCKET_SIZE_TYPE;
#endif

#ifndef ILU_CONFIGURE_TIME
/*L1 >= {trmu}; L2 unconstrained*/
ilu_string
_ilu_CurrentHostIPAddrString(ilu_string * host_out,
			     struct in_addr * addr_out,
			     ILU_ERRS((IoErrs)) * err);
/*
 * Returns an IP address, in string form, for this host.  Result
 * will never be freed.  (host_out) and (addr_out) are OUT
 * parameters, and may independently be NIL to indicate the caller
 * doesn't care to receive the respective value.  If (host_out !=
 * NIL), a statically allocated string will be returned; that string
 * will be a host name if possible, perhaps an address string
 * otherwise.  Sadly, this interface makes the fallacious assumption
 * that the answer will be valid forevermore.
 */
#endif /* ndef ILU_CONFIGURE_TIME */

#endif /* ndef _ILU_SOCKET_HEADERS */
