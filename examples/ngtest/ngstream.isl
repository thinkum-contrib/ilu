(** $Id: ngstream.isl,v 1.3 1999/08/03 01:58:10 janssen Exp $
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
(* Last edited by Mike Spreitzer October 9, 1998 1:58 pm PDT *)

(* ************************************************************************ *)
(*
    Version 1.3 ISL for HTTP-NG, Dan Larner, 3-19-98

    This work is very preliminary - Comments welcomed and encouraged!
    'xxx' marks areas where thought is definitly needed
    Please send comments, suggestions, contributions, etc. to the http-ng
    mailinglist, w3c-http-ng@w3.org and/or to larner@parc.xerox.com.

*)


(* ************************************************************************ *)
(* *********************** DataSource ************************************* *)
(* ************************************************************************ *)

Interface NgStream Brand "NG" Imports 
    NgBasic      From ngbasic.isl
End;

(* This object type may be passed in invocations of operations meant for 
   asynchronous receipt of data (such as ReceiveRendering on a RenderingSink).
   It allows the Sink side some control over the streaming operations. 
   NOTE: Methods invocations are assumed to be processed at the Source in the
   order in which they were sent.  *)

Type OptionalDataSource = Optional DataSource; 

Type DataSource = Object

    Supertypes NgBasic.NgObject End

    Methods
    
    (* Called to abort the streaming that is associated with this object *)
    Asynchronous Abort (),
    
    (* Called to pause the streaming that is associated with this object *)
    Asynchronous Pause (),
    
    (* Called to resume the streaming that is associated with this object *)
    Asynchronous Resume (),
    
    (* Called to have the streaming that is associated with this object resend 
       a range of data *)
    Asynchronous Resend ( repeatRange : NgBasic.UnsignedInclusiveInterval ),
    
    (* Called to suggest the streaming that is associated with this object
       change its chunk size *)
    Asynchronous SuggestChunkSize ( suggestedSize : Cardinal )

End;


(* This object type is used for control over a Sink.
   NOTE: Methods invocations are assumed to be processed at the Sink in the
   order in which they were sent.  *)

Type DataSink = Object

    Supertypes NgBasic.NgObject End

    Methods
    
    (* Called to tell the sink where control information can be sent *)
    Asynchronous RegisterSourceControl (thesource : DataSource),
        
    (* Called to indicate that no more data will be sent *)
    Asynchronous Done ()

End;


(* ************************************************************************ *)
(* ******************************** END *********************************** *)
(* ************************************************************************ *)
