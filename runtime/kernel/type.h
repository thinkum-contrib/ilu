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
/* $Id: type.h,v 1.6 1999/08/03 01:52:53 janssen Exp $ */
/* Last tweaked by Mike Spreitzer September 21, 1993 11:38 am PDT */

#define class_name(c)			((c)->cl_name)
#define class_brand(c)			((c)->cl_brand)
#define class_unique_id(c)		((c)->cl_unique_id)
#define class_singleton(c)		((c)->cl_singleton != (ilu_string) 0)
#define class_authentication(c)		((c)->cl_authentication)
#define class_methods(c)		((c)->cl_methods)
#define class_method_count(c)		((c)->cl_method_count)
#define class_superclass_count(c)	((c)->cl_scls_count)
#define class_superclass(c,index)	((c)->cl_sclses[index])
#define class_superclass_id(c,index)	((c)->cl_scls_ids[index])
#define class_superclasses(c)		((c)->cl_sclses)
#define class_superclass_ids(c)		((c)->cl_scls_ids)
#define class_collectible(c)		((c)->cl_collectible)
