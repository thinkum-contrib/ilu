/*  -*- Mode: C; -*-
 *
 * This code contributed by Siemens Corporate Research, Inc.
 */
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

#ifndef _ILUOBJ_SCM_H_
#define _ILUOBJ_SCM_H_

#include "ilu-type.h"

/* for internal use */
SCM iluguile_object_create_from_registry(ilu_Class c, ilu_Object obj);

/* scheme funcs */
SCM iluguile_object__register_surrogate_creator(SCM _c, SCM _proc);
SCM iluguile_object__create_from_registry(SCM _c, SCM _obj);
SCM iluguile_object__input_object(SCM _call, SCM disc, SCM pclass);
SCM iluguile_object__output_object(SCM _call, SCM _obj, SCM _putative_class);
SCM iluguile_object__size_of_object(SCM _call, SCM obj, SCM _putative_class);
SCM iluguile_object__lookup(SCM _sid, SCM _ih, SCM _pclass);
SCM iluguile_object__register_as_gc_callback(SCM obj);

#endif
