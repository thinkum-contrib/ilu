(** $Id: ngbasic.isl,v 1.3 1999/08/03 01:58:16 janssen Exp $
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
(* Last edited by Mike Spreitzer October 9, 1998 1:57 pm PDT *)

(* ************************************************************************ *)
(*
    Version 1.3 ISL for HTTP-NG, Dan Larner, 3-19-98

    This work is very preliminary - Comments welcomed and encouraged!
    'xxx' marks areas where thought is definitly needed
    Please send comments, suggestions, contributions, etc. to the http-ng
    mailing list, w3c-http-ng@w3.org and/or to larner@parc.xerox.com.

*)

(* ************************************************************************ *)
(* *************************** Basic Types ******************************** *)
(* ************************************************************************ *)

(* Defines a number of basic types used throughout HTTP-NG interfaces.      *)


Interface NgBasic Brand "NG" ;


(* ********************** String & Type related *************************** *)

Type String           = Sequence Of Short Character;

Type OptionalString   = Optional String;

Type StringSequence   = Sequence Of String;

Type ByteSequence     = Sequence Of Byte;

Type URI              = String; (* see RFC1738 and RFC 1808 *)

Type OptionalURI      = Optional URI; 

Type URISequence      = Sequence Of URI;

Type OptionalCardinal = Optional Cardinal;


(* ************************************************************************ *)
(* **************************** Time Related ****************************** *)


(* Represents a number of microseconds elapsed since midnight
   (00:00:00), January 1, 1970, coordinated universal time.
   Note that negative values indicate number of microseconds
   prior to the origin. *)
   
Type AbsoluteTime         = Long Integer;

Type OptionalAbsoluteTime = Optional AbsoluteTime;


(* Represents an relative number of microseconds *)
   
Type RelativeTime         = Long Integer;

Type OptionalRelativeTime = Optional RelativeTime;


(* ************************************************************************ *)
(* **************************** Version Related *************************** *)

Type Version = Record 
    major         :  Short Cardinal,
    minor         :  Short Cardinal
End;



(* ************************************************************************ *)
(* ***************************** Intervals ******************************** *)

(* describes an inclusive range of unsigned numbers               *)

Type UnsignedInclusiveInterval = Record
    startValue    :  Cardinal,
    endValue      :  Cardinal 
End;

Type OptionalUnsignedInclusiveInterval = Optional UnsignedInclusiveInterval;


(* ************************************************************************ *)
(* ************************ Name Value Pairs ****************************** *)

(* simply a named attribute paired with some value *)

Type NameValuePair = Record
    name    :   String,
    value   :   Pickle 
End;

    
Type NameValuePairSequence = Sequence Of NameValuePair;
Type NameSequence          = Sequence Of NgBasic.String;


(* ************************************************************************ *)
(* ***************************** Exceptions ******************************* *)

Type OptionalPickle = Optional Pickle;

Type ExceptionInformation = Record

    (* a human readable description of why the exception occurred *)
    reasonPhrase  : String,           
    
    (* if present, operation specific information on the details of why the 
       operation produced an exception *)
    specificsData : OptionalPickle   
    
End;


(* Certain operations, typically Gets or Puts, may return a WouldBlock  or
   Conflict exception when the operation would block for some reason or is
   somehow in conflict with another operation (perhaps simultaneous Puts
   on a Rendering) or operational semantics (perhaps a Put based on an
   old version as determined through e-tag information for example).
   The value optionally contained in the ExceptionInformation's Pickle is 
   operation specific. For a WouldBlock example, it could contain locking 
   information. Those knowledgeable about locking could attempt to extract 
   and make use of the lock information contained in the pickle. Those not 
   knowledgeable about locking could just disregard the value, but still have 
   knowledge that something is currently going on with the target object that 
   would cause them to block waiting for a result.
   
   There may be situations where an implementation might quickly reify an
   object in an object creation function and later determine that this object
   really doesn't exist.  There's a need to return an object doesn't exist
   exception.  Since this sort of 'system exception' information isn't
   necessarily sent across the wire, it makes sense to create a user
   exception, ObjectNotExist, for this sort of event. This is also useful if
   this were to occur as the result of an async SendRenderingCall, where the
   Report returned in the ReceiveRendering can contain an ObjectNotExist
   exception.
*)



Exception WouldBlock : ExceptionInformation "Operation would have blocked" ;

Exception Conflict   : ExceptionInformation 
                       "Conflict with another operation or semantics" ;

Exception ObjectNotExist : ExceptionInformation 
                           "Discriminator object doesn't exist" ;



(* ************************************************************************ *)
(* ***************************** NgObject ********************************* *)

(* NgObject forms a root class from which all Ng classes inherit *)

Type NgObjectSequence = Sequence Of NgObject;

Type NgObject = Object

Methods

    (* Returns a stringified object reference of an object that supports the 
       Renderable interface.  The denoted object can be asked for a Rendering
       (e.g. text/isl, text/idl, text/midl, etc.) interface in which (the most 
       specific type) the object is defined.  *)
       
    GetInterfaceDefinitionSource () : String

End;


(* ************************************************************************ *)
(* ******************************** END *********************************** *)
(* ************************************************************************ *)
