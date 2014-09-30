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

$Id: iluppobject.h,v 1.3 1999/08/03 01:55:40 janssen Exp $
*/
#ifdef __cplusplus
extern "C" {
#endif

/* Representation of an ILU Passport */

typedef struct
{
  PyObject_HEAD
  ilu_Passport passport;
} IluppObject;

extern PyTypeObject	Ilupp_Type;

#define ilupp_Check(op)	((op)->ob_type == &Ilupp_Type)

extern PyObject *	ilupp_FromPassport (ilu_Passport);
extern ilu_Passport	ilupp_AsPassport (IluppObject *);

#ifdef __cplusplus
}
#endif
