/** $Id: srvr.h,v 1.7 1999/08/03 01:52:17 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 11:36 am PDT */

#include <iluxport.h>

extern int      doit(char *pinfo, ilu_TransportInfo tinfo,
		     ilu_boolean threadly,
		     ilu_boolean init_credentials,
		     ilu_boolean use_ssl_if_available);
/*
 * Both pointer arguments may be defaulted to NULL; defaulting tinfo
 * means to initialize credentials and use security if available.
 */

extern ilu_TransportInfo DefaultTInfo(ilu_boolean secure, ilu_boolean ssl);
/*
 * Return the default ilu_TransportInfo.  Returns NIL if /secure/
 * but security support isn't configured into ILU.
 */


#if defined (_WINDOWS)
#define OUTPUT	WIN_PRINTF
extern void WIN_PRINTF(char *format, ...);
#else
#define OUTPUT	printf
#endif
