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
/* $Id: iluvector.c,v 1.26 1999/08/03 01:53:22 janssen Exp $ */
/* Last edited by Mike Spreitzer April 30, 1998 11:30 am PDT */

#define _POSIX_SOURCE

#include "iluntrnl.h"

/*L1, L2, Main unconstrained*/

ilu_Vector _ilu_vector_new (ilu_cardinal capacity, ILU_ERRS((no_memory)) *err)
{
  ilu_Vector newv;

  newv = (ilu_Vector) ilu_MallocE(sizeof(struct ilu_vector_s), err);
  if (ILU_ERRNOK(*err)) return NIL;
  newv->ve_capacity = (capacity < 1) ? 1 : capacity;
  newv->ve_size = 0;
  newv->ve_elements = (ilu_refany *) ilu_MallocE(capacity * (sizeof(ilu_refany)), err);
  if (ILU_ERRNOK(*err)) { ilu_free(newv); return NIL; };
  return (newv);
}

/*L1 >= {some mutex that protects the vector}*/

void _ilu_vector_destroy (ilu_Vector v, void (*f) (ilu_refany))
{
  register ilu_cardinal i;

  if (f != NULLFN && v->ve_elements != NIL && v->ve_size > 0)
    for (i = 0;  i < v->ve_size;  i += 1)
      (*f)(v->ve_elements[i]);
  if (v->ve_elements != NIL)
    ilu_free(v->ve_elements);
  ilu_free(v);
}

ilu_cardinal _ilu_vector_size (ilu_Vector v)
{
  if (v != NIL)
    return (v->ve_size);
  else
    return (0);
}

ilu_refany * _ilu_vector_elements(ilu_Vector v)
{
  if (v != NIL)
    return (v->ve_elements);
  else
    return (NIL);
}
      
ilu_boolean
_ilu_vector_reserve(ilu_Vector v, ilu_cardinal dSize,
		    ILU_ERRS((no_memory)) * err)
{
  if ((v->ve_capacity < v->ve_size)
      || (dSize < v->ve_capacity - v->ve_size)) {
    ilu_cardinal    newcap = v->ve_capacity * 2, c2 = v->ve_size + dSize;
    if (newcap < c2)
      newcap = c2;
    if (newcap < v->ve_capacity)
      return ILU_ERR_CONS1(no_memory, err, nbytes, 0, ilu_FALSE);
    v->ve_capacity = newcap;
    v->ve_elements = ilu_ReallocE(v->ve_elements, newcap, err);
    if (ILU_ERRNOK(*err))
      return (ilu_FALSE);
    return ilu_TRUE;
  } else
    return ILU_CLER(*err);
}

void _ilu_vector_add (ilu_Vector v, ilu_refany e, ILU_ERRS((no_memory)) *err)
{
  if (v->ve_size >= v->ve_capacity) {
    v->ve_elements = (ilu_refany *) ilu_ReallocE(v->ve_elements, ((v->ve_capacity *= 2) * sizeof(ilu_refany)), err);
    if (ILU_ERRNOK(*err)) return;
  } else {
    ILU_CLER(*err);
  };
  v->ve_elements[v->ve_size] = e;
  v->ve_size += 1;
}

void _ilu_vector_add_if_not_present (ilu_Vector v, ilu_refany e, ILU_ERRS((no_memory)) *err)
{
  register ilu_cardinal i;

  ILU_CLER(*err);
  if (v == NIL)
    return;
  for (i = 0;  i < v->ve_size;  i++)
    if (v->ve_elements[i] == e)
      return;
  _ilu_vector_add (v, e, err);
}

void _ilu_vector_remove (ilu_Vector v, ilu_refany e)
{
  register ilu_cardinal i, j;

  for (i = 0;  i < v->ve_size;  i += 1)
    if (e == v->ve_elements[i])
      {
	for (j = i + 1;  j < v->ve_size;  j += 1)
	  v->ve_elements[j-1] = v->ve_elements[j];
	v->ve_size -= 1;
	i -= 1;
      }
}

ilu_Vector _ilu_vector_copy (ilu_Vector old, ILU_ERRS((no_memory)) *err)
{
  ilu_Vector newv;
  register ilu_cardinal i;

  newv = _ilu_vector_new(old->ve_size, err);
  if (ILU_ERRNOK(*err)) return NIL;
  for (i = 0; i < old->ve_size; i++)
      newv->ve_elements[i] = old->ve_elements[i];
  return newv;
}

void _ilu_vector_assign (ilu_Vector l, ilu_Vector r, ILU_ERRS((no_memory)) *err)
{
  register ilu_cardinal i, m = (l->ve_size < r->ve_size) ? l->ve_size : r->ve_size;
  for (i = 0; i < m; i++)
      l->ve_elements[i] = r->ve_elements[i];
  ILU_CLER(*err);
  for (i = m; i < r->ve_size; i++) {
    _ilu_vector_add(l, r->ve_elements[i], err);
    if (ILU_ERRNOK(*err)) return;
  }
  return;
}
