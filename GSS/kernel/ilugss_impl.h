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

$Id: ilugss_impl.h,v 1.13 1999/08/03 01:59:30 janssen Exp $
*/
#ifndef GSS_IMPL_H
#define GSS_IMPL_H 1
/*
 * single header file which includes all necessary headers for
 * implementations of GSS security schemes
 *
 * Antony Courtney,	16/6/95
 */

#include "ilugss_debug.h"
#if (defined(WIN32)||defined(_WIN32))
#include "ilugsswin_conf.h"
#else
#include "ilugss_conf.h"
#endif
#include "ilugss_opaque.h"
#include "ilugss_scheme.h"
#include "ilugss_namespace.h"
#include "ilugss_ext.h"
#include "ilugss_util.h"
#include "ilugss_oidtbl.h"

void ilugss_register_scheme (gss_scheme_t *, char *);
void ilugss_register_namespace (gss_namespace_t *, char *);

#endif
