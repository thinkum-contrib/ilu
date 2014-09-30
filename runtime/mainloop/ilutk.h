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
/* ilutk.h */
/* Last edited by Mike Spreitzer October 19, 1995 8:01 am PDT */

extern void     IluTk_Init(void);
/*
 * Integrate the ILU and Tk main loops.  After this call, ILU uses
 * Tk's event handling mechanism; Tk_DoOneEvent will dispatch to ILU
 * as well as Tk events (one at a time, of course).  Recall that ILU
 * uses its main loop recursively; thus this integrated main loop
 * can be called while doing an RPC, which means that your
 * application has to be re-entrant.
 * 
 * Call this procedure at least once before (a) using the ILU main loop
 * directly, (b) calling across any ISL-specified interface, or (c)
 * creating any non-local ports for any ILU servers (which is
 * bundled into server creation in some language-specific runtimes).
 */
