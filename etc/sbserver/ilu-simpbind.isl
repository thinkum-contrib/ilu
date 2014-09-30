(* Simple binding service interface *)

(** $Id: ilu-simpbind.isl,v 1.6 1999/08/03 01:56:16 janssen Exp $
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
*)
(* Note:  changes to this will also require changes to the
	corresponding code in runtime/kernel/sbilu.c *)

INTERFACE ilu-simpbind;

TYPE Server = OBJECT TYPEID "ilu:ilu-simple-binding-version-1"
  DOCUMENTATION "Simple binding name service"
  METHODS
    (* add exceptions *)
    Publish (sbh : StringBindingHandle) : CookieType
	RAISES BadSBH, AlreadyPublished, MallocFailure END
	"returns a cookie if successful or NIL if failed",
    Withdraw (sbh : StringBindingHandle, cookie : CookieType) : BOOLEAN
	RAISES NoTable, NotPublished, BadProof, BadSBH END
	"returns true if successfully withdrawn",
    Lookup (sid : ilu.CString, ih : ilu.CString) : StringBindingHandle
	RAISES NoTable, NotPublished, MallocFailure END
	"returns object's sbh and mst if successful or NIL if failed",
    (* for debugging only *)
    Enumerate (pattern : ilu.CString) : StringBindingHandleList
	"returns list of objects in table, filtered according to pattern"
  END;

TYPE StringBindingHandle = ilu.CString;
TYPE StringBindingHandleList = SEQUENCE OF StringBindingHandle TYPEID "IDL:ilu.parc.xerox.com/ilu_simpbind/StringBindingHandleList:1.0";
TYPE CookieType = ilu.CString;

EXCEPTION BadSBH "Couldn't parse the sbh" ;
EXCEPTION AlreadyPublished "Object's oid is already a key" ;
EXCEPTION NoTable "The SimpleBinding object's internal htable is NIL" ;
EXCEPTION NotPublished "Object's oid is not in htable" ;
EXCEPTION BadProof "Proof does not match htable proof" ;
EXCEPTION MallocFailure "Could not allocate space for published data" ;


