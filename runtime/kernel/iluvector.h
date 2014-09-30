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
/* $Id: iluvector.h,v 1.7 1999/08/03 01:53:21 janssen Exp $ */
/* Last tweaked by Mike Spreitzer April 24, 1998 9:25 am PDT */

#ifndef _ILU_VECTOR_
#define _ILU_VECTOR_

/*L2, Main unconstrained*/

typedef struct ilu_vector_s {
  /*L1 >= {some mutex that protects the vector}*/
  
  ilu_refany *ve_elements;
  ilu_cardinal ve_capacity;
  ilu_cardinal ve_size;
} *ilu_Vector;

/*L1 unconstrained*/
extern ilu_Vector 
_ilu_vector_new(ilu_cardinal capacity,
		ILU_ERRS((no_memory)) *);

/*L1 >= {some mutex that protects the vector}*/

extern void 
_ilu_vector_destroy(ilu_Vector v,
		    void (*f) (ilu_refany));

extern void 
_ilu_vector_add(ilu_Vector v, ilu_refany e,
		ILU_ERRS((no_memory)) *);

extern ilu_boolean 
_ilu_vector_reserve(ilu_Vector v, ilu_cardinal dSize,
		    ILU_ERRS((no_memory)) *);

extern void     _ilu_vector_remove(ilu_Vector v, ilu_refany e);

extern ilu_cardinal _ilu_vector_size(ilu_Vector v);

extern ilu_refany *_ilu_vector_elements(ilu_Vector v);

extern ilu_Vector 
_ilu_vector_copy(ilu_Vector old,
		 ILU_ERRS((no_memory)) *);

extern void
_ilu_vector_assign(ilu_Vector l, ilu_Vector r,
		   ILU_ERRS((no_memory)) *);

extern void 
_ilu_vector_add_if_not_present(ilu_Vector v, ilu_refany e,
			       ILU_ERRS((no_memory)) *);
	
#define VECTOR(a)	((ilu_Vector)(a))

#endif /* _ILU_VECTOR_ */
