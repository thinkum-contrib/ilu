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

/* startup and cleanup code for winsock */

#if (defined(WIN32) || defined(WIN16))
#include <stdlib.h>
#include <windows.h>
#include <winsock.h>

#ifndef MAKEWORD
#define MAKEWORD(a, b)      ((WORD)(((BYTE)(a)) | ((WORD)((BYTE)(b))) << 8))
#endif


void _ilu_CleanupWinsock (void) {
/* cleansup winsock */
	WSACleanup();
}

int _ilu_StartupWinsock () {
/* inits winsock for version 1.1 - returns 0 on success, else -1 */

#ifdef WIN16
static int i_cleanup_added_to_atexit = 0;
#endif

WORD wVersionRequested;
WSADATA wsaData;
int err;

wVersionRequested = MAKEWORD( 1, 1 );
err = WSAStartup( wVersionRequested, &wsaData );

if ( err != 0 ) {
	/* report we couldn't find a useable winsock.dll.     */
	return -1;
	}

/* Confirm that the Windows Sockets DLL supports 1.1.*/
/* Note that if the DLL supports versions greater */
/* than 1.1 in addition to 1.1, it will still return */
/* 1.1 in wVersion since that is the version we */
/* requested.     */
if ( LOBYTE( wsaData.wVersion ) != 1 ||
	HIBYTE( wsaData.wVersion ) != 1 ) {

	/* cleanup and report we couldn't find a useable winsock.dll.     */
	WSACleanup( );
	return -1; 
	}
/* The Windows Sockets DLL is acceptable. Proceed. */

#ifdef WIN16
if (i_cleanup_added_to_atexit == 0) {
	atexit(_ilu_CleanupWinsock);
	i_cleanup_added_to_atexit = 1;
}
#endif

return 0;
}

#ifdef WIN16
int ilu_StartupWinsock () {
	return _ilu_StartupWinsock ();
}
#endif

#endif /* WIN32 or WIN16*/
