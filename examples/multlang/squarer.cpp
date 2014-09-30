/** $Id: squarer.cpp,v 1.3 1999/08/03 01:57:48 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 10:08 am PDT */

#include <stdio.h>

#include "multlang.hh"

/* an implementation of the Squarer object */

class multlang_T_Squarer_impl : public virtual multlang_T_Squarer {
public:
  multlang_T_Squarer_impl(char *instanceHandle, iluServer *server);

  virtual char * ILUGetInstanceHandle();
  virtual iluServer * ILUGetServer();

  virtual ilu_Cardinal ObtainSquare (multlangStatus *_status, ilu_Cardinal val);

private:
  char *ourInstanceHandle;
  iluServer *ourServer;
};

multlang_T_Squarer_impl::multlang_T_Squarer_impl(char *instanceHandle, iluServer *server)
{
  this->ourInstanceHandle = instanceHandle;
  this->ourServer = server;
}

char * multlang_T_Squarer_impl::ILUGetInstanceHandle()
{
  return this->ourInstanceHandle;
}

iluServer * multlang_T_Squarer_impl::ILUGetServer()
{
  return this->ourServer;
}

ilu_Cardinal multlang_T_Squarer_impl::ObtainSquare (multlangStatus *_status,
						    ilu_Cardinal val)
{
  static multlang_T_Multiplier *theMultiplier = NULL;

  if (theMultiplier == NULL) {
    theMultiplier =
      (multlang_T_Multiplier *) iluObject::Lookup("Server1",
						  "theMultiplierObject",
						  multlang_T_Multiplier::ILUClassRecord);
    if (theMultiplier == NULL)
      return 0;
  }
  return theMultiplier->Multiply (_status, val, val);
}

/* Initialization code */

extern "C" {
  ilu_boolean multlang_StartCPlusPlus (void);
};

ilu_boolean multlang_StartCPlusPlus (void) {

  iluServer s ("Server2", NULL);
  multlang_T_Multiplier *theMultiplier;
  multlang_T_Squarer_impl *theSquarer;

  s.AddPort (NULL, NULL, ilu_TRUE);
  ilu::SetDefaultServer(&s);

  theSquarer = new multlang_T_Squarer_impl("theSquarerObject", &s);
  if ((theSquarer == NULL) || (!theSquarer->ILUPublish())) {
    fprintf (stderr, "Can't create multlang.Squarer object in C++\n");
    return ilu_FALSE;
  };
  return ilu_TRUE;
};
  
