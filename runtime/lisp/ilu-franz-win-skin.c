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

$Id: ilu-franz-win-skin.c,v 1.4 1999/08/03 01:53:31 janssen Exp $
*/

#define _BSD_SOURCE		/* to allow SGI to process select() */

/* #include <unistd.h> */
#include <winsock.h>
#include <sys/types.h>
#include <time.h>
#include <stdio.h>

#include <iluxport.h>
/* #include "../../winio/winio.h" */

#if defined(WIN32)

#if defined(ILU_BUILDING_RUNTIME)
// we're actually building the runtime, so declare things as exported
#define ILU_RUNTIME_PUBLIC        __declspec(dllexport) extern
#define ILU_RUNTIME_PUBLIC_CLASS  class __declspec(dllexport)

#else
// we're must be building an app, so declare things as imported
#define ILU_RUNTIME_PUBLIC       __declspec(dllimport) extern
#define ILU_RUNTIME_PUBLIC_CLASS class __declspec(dllimport)
#endif /* defined(ILU_BUILDING_RUNTIME) */

#else
// we're not on win32
#define ILU_RUNTIME_PUBLIC extern
#define ILU_RUNTIME_PUBLIC_CLASS class

#endif /* defined(WIN32) */



#define FixnumToInt(v)	(v) /* ??? */

ILU_RUNTIME_PUBLIC ilu_Error * ilufranz_CreateErrorStruct()
{
   return ilu_must_malloc(sizeof(ilu_Error));
}

ILU_RUNTIME_PUBLIC void ilufranz_ClearErrorStruct(ilu_Error *_err)
{
   ILU_CLER(*_err);
}

ILU_RUNTIME_PUBLIC ilu_boolean ilufranz_ErrorOK(ilu_Error *_err)
{
   return ILU_ERROK(*_err);
}

ILU_RUNTIME_PUBLIC ilu_string ilufranz_ErrorDetails(ilu_Error *_err)
{
   const char *err_name = ILU_ERR_NAME(*_err);
   const char *err_desc = ILU_ERR_DESCRIPTION(*_err);
   const char *err_file = ilu_ErrorFile(_err);
   int         err_line = ilu_ErrorLine(_err);
   const char  format[] = "%s: %s\nraised on line %d in %s";

   char *details = ilu_must_malloc(strlen(err_name) + strlen(err_desc)
				   + strlen(err_file) + 15 + sizeof(format));
   sprintf(details, format, err_name, err_desc, err_line, err_file);
   return details;
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

ILU_RUNTIME_PUBLIC ilu_ObjectTable ilufranz_CreateObjectTable (int lot)
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

ILU_RUNTIME_PUBLIC void ilufranz_SetupObjectTables (unsigned long (*p1) (int, ilu_string),
				 void (*p2) (int))
{
  lisp_object_of_ih = p1;
  lisp_free_self = p2;
}


ILU_RUNTIME_PUBLIC unsigned long *ilufranz_AllocateMainLoopHandle ()
{
  return (unsigned long *) ilu_must_malloc(sizeof(unsigned long));
}

ILU_RUNTIME_PUBLIC unsigned ilufranz_Strlen(const char *str)
{
   return strlen(str);
}


ILU_RUNTIME_PUBLIC void ilufranz_Memcpy(char *dest, const char *src, unsigned len)
{
   memcpy(dest, src, len);
}


ILU_RUNTIME_PUBLIC void ilufranz_FreeCStruct(void *ptr)
{
   ilu_free(ptr);
}


BOOL FAR WINAPI MyBlockingHook()
{
   MSG msg;
   BOOL ret;

   ret = (BOOL)PeekMessage(&msg, NULL, 0, 0, PM_REMOVE);
   if (ret) {
      TranslateMessage(&msg);
      DispatchMessage(&msg);
   }
   return ret;
}


ILU_RUNTIME_PUBLIC void ilufranz_SetupBlockingHook()
{
   WSASetBlockingHook(MyBlockingHook);
}
