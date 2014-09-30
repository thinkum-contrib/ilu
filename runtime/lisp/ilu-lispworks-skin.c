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
Copyright (c) 1995 Harlequin Inc.  All Rights Reserved.  

Unlimited use, reproduction, and distribution of this software is
permitted.  Any copy of this software must include both the above
copyright notice of Harlequin Inc. and this paragraph.  Any
distribution of this software must comply with all applicable United
States export control laws.  This software is made available AS IS,
and Harlequin Inc. disclaims all warranties, express or implied,
including without limitation the implied warranties of merchantability
and fitness for a particular purpose, and notwithstanding any other
provision contained herein, any liability for damages resulting from
the software or its use is expressly disclaimed, whether arising in
contract, tort (including negligence) or strict liability, even if
Harlequin Inc. is advised of the possibility of such damages.

*/

#include <unistd.h>

#include <iluxport.h>

#define FixnumToInt(v)	((v)>>2)


static unsigned long (*lisp_object_of_ih) (int, ilu_string) = 0;
static void (*lisp_free_self) (int) = 0;

static ilu_Object GeneralObjectOfIH (ilu_ObjectTable self, ilu_string ih)
{
  if (self != ILU_NIL && lisp_object_of_ih != 0)
    return ((ilu_Object) FixnumToInt((*lisp_object_of_ih) ((int) (self->ot_rock), ih)));
  else
    return 0;
}

static void GeneralFreeSelf (ilu_ObjectTable self)
{
  if (self != ILU_NIL && lisp_free_self != 0)
    {
      (*lisp_free_self) ((int) (self->ot_rock));
      ilu_free(self);
    }
}


ilu_ObjectTable ilulw_CreateObjectTable (int lot)
{
  ilu_ObjectTable newt = (ilu_ObjectTable) ilu_malloc(sizeof(struct ilu_ObjectTable_struct));
  if (newt == 0)
    return 0;
  else
    {
      newt->ot_object_of_ih = GeneralObjectOfIH;
      newt->ot_free_self = GeneralFreeSelf;
      newt->ot_rock = (ilu_private) lot;
      return (newt);
    }
}

void ilulw_SetupObjectTables (unsigned long (*p1) (int, ilu_string),
				 void (*p2) (int))
{
  lisp_object_of_ih = p1;
  lisp_free_self = p2;
}
