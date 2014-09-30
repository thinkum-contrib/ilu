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

$Id: iluplobject.h,v 1.3 1999/08/03 01:55:41 janssen Exp $
*/
#ifdef __cplusplus
extern "C" {
#endif

/* Representation of an ILU Pipeline */

typedef struct
{
  PyObject_HEAD
  ilu_Pipeline pipeline;
} IluplObject;

extern PyTypeObject	Ilupl_Type;

#define ilupl_Check(op)	((op)->ob_type == &Ilupl_Type)

extern PyObject *	ilupl_FromPipeline (ilu_Pipeline);
extern ilu_Pipeline	ilupl_AsPipeline (IluplObject *);

#ifdef __cplusplus
}
#endif