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

/* dll entry function */

#ifdef WIN32
#include <windows.h>

extern void _ilu_CleanupWinsock (void);
extern int _ilu_StartupWinsock ();


BOOL APIENTRY DllMain( HANDLE hModule, 
                        DWORD ul_reason_for_call, 
                        LPVOID lpReserved )
{
    switch( ul_reason_for_call ) {
    case DLL_PROCESS_ATTACH:
   		 _ilu_StartupWinsock();
   		 break;
    case DLL_THREAD_ATTACH:
   		 break;
    case DLL_THREAD_DETACH:
   		 break;
    case DLL_PROCESS_DETACH:
   		 _ilu_CleanupWinsock();
   		 break;
    }
    return TRUE;
}

#endif /* WIN32 */
