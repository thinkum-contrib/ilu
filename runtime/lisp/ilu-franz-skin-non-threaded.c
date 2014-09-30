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

$Id: ilu-franz-skin-non-threaded.c,v 1.4 1999/08/03 01:53:27 janssen Exp $
*/

#define _BSD_SOURCE		/* to allow SGI to process select() */

#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>

#include <iluxport.h>

#if (LISP_FLAG_BITS==2)
#define FixnumToInt(v)	((v)>>2)
#elif (LISP_FLAG_BITS==3)
#define FixnumToInt(v)	((v)>>3)
#else
#error "Don't know how to convert Fixnum to int for this architecture"
#endif /* figuring out how to set flag bits */

#define MILLISECONDS(x)	(((x)==NULL)?0:((x)->ft_s * 1000 + (((x)->ft_t * 1000) / ilu_FineTimeRate)))

int ilufranz_OutputPossibleP (int fd)
{
  static fd_set writefds;
  int width, stat;
  static struct timeval to = { 0, 0 };

  width = fd + 1;
  FD_ZERO(&writefds);
  FD_SET(fd, &writefds);
  stat = select (width, NULL, &writefds, NULL, &to);
  if (stat > 0 && FD_ISSET(fd, &writefds))
    return 1;
  else
    return 0;
}

void *ilufranz_CreateAlarm ()
{
  return ((void *) ilu_CreateAlarm());
}

void ilufranz_SetAlarm (void *alarm, ilu_FineTime *when, void (*what)(void *), void *rock)
{
  ilu_SetAlarm (alarm, *when, what, rock);
  ilu_free(when);
}

ilu_FineTime * ilufranz_Plus5Minutes ()
{
  ilu_FineTime plus5 = { 300, 0 };
  ilu_FineTime *time = (ilu_FineTime *) ilu_must_malloc(sizeof(ilu_FineTime));
  ilu_FineTime tmp;

  tmp = ilu_FineTime_Now();
  *time = ilu_FineTime_Add (tmp, plus5);
  printf ("Plus5Minutes:  now is {%ul,%ul}, *time is {%ul,%ul}\n",
	  tmp.ft_s, tmp.ft_t, time->ft_s, time->ft_t);
  return (time);
}

static unsigned long (*internal_interest_hook) (unsigned long index, unsigned long interest) = 0;

/*Inside(obj's server, obj's type)*/
static void kernel_interest_hook (ilu_Object kobj, int vi)
{
  unsigned long index = (unsigned long) ilu_GetLanguageSpecificObject (kobj);
  unsigned long newindex;
  ilu_Class cl = ilu_ClassOfObject(kobj);
  
  /* printf("Interest:  %s %4u %s\n", (vi == 0) ? "NO  " : "YES", index, ilu_SBHOfObject(kobj)); */

  if (index != 0 &&
      internal_interest_hook != 0 &&
      ((!ilu_TrueInstanceP(kobj)) || (cl->cl_collectible)) &&
      (((vi == 0) && ((index & 0x1) != 0)) ||
       ((vi != 0) && ((index & 0x1) == 0))))
    {
      newindex = FixnumToInt((*internal_interest_hook) (index, vi));
      if (newindex != index)
	{
/*	  printf("           LSPO changed to %u\n", newindex); */
	  ilu_RegisterLanguageSpecificObject (kobj, (void *) newindex);
	}
    }
}

void ilufranz_SetInterestHook (unsigned long (*hook) (unsigned long, unsigned long))
{
  internal_interest_hook = hook;
  ilu_SetNoter (&kernel_interest_hook);
}

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

ilu_ObjectTable ilufranz_CreateObjectTable (int lot)
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

void ilufranz_SetupObjectTables (unsigned long (*p1) (int, ilu_string),
				 void (*p2) (int))
{
  lisp_object_of_ih = p1;
  lisp_free_self = p2;
}

unsigned long *ilufranz_AllocateMainLoopHandle (unsigned long val)
{
  unsigned long *i = (unsigned long *) ilu_must_malloc(sizeof(unsigned long));
  *i = val;
  return (i);
}

void ilufranz_FreeMainLoopHandle (unsigned long *val)
{
  ilu_free(val);
}
