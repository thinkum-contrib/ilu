(** $Id: ngform.isl,v 1.3 1999/08/03 01:58:14 janssen Exp $
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
    mailing list, w3c-http-ng@w3.org and/or to larner@parc.xerox.com.

*)

(* ************************************************************************ *)
(* *************************** FormProcessors ***************************** *)
(* ************************************************************************ *)

(* An Object of type FormProcessor is able to process Form based input, and 
   produce an appropriate rendering as a response.
*)


Interface NgFormProcessor Brand "NG" Imports 
    NgBasic                From ngbasic.isl,
    IANA-Charsets-Registry From ianacharsets.isl,
    NgCache                From ngcache.isl,    
    NgRendering            From ngrendering.isl
End;



(* ************************************************************************ *)
(* ********************* Form entries  ************************************ *)

(* a Form input element is simply a named form field paired with some value *)

Type FormInputElement = NgBasic.NameValuePair;
    
Type FormInputElementSequence = Sequence Of FormInputElement;
Type FormInputElementNames    = NgBasic.NameSequence;



(* ************************************************************************ *)
(* ******************** FormProcessing Exceptions ************************* *)

(* The listed form inputs has invalid values *)
   
Exception InvalidFormEntries : FormInputElementNames;




(* ************************************************************************ *)
(* ********************** ProcessedFormSink  ****************************** *)


(* a FormProblemReport is used to pass information to asynchronous
   callbacks that would have normally been passed back as an exception 
   from the synchronous version of the same sort of call *)
   
Type FormProblemReport = Union
    invalidEntries : FormInputElementNames,
    conflict       : NgBasic.ExceptionInformation,
    objectNotExist : NgBasic.ExceptionInformation
End;

Type OptionalFormProblemReport = Optional FormProblemReport; 


(* A ProcessedFormSink is basically a NgRendering.RenderingSink
   except that there are some more problems that can be reported *)

Type ProcessedFormSink = Object  

    Supertypes NgRendering.RenderingSink End

    Methods        
        
    (* FormProblem is called when a Form related exception would have been
       raised from calling ProcessForm (with the same args as SendForm) on the 
       FormProcessor object.  This is basically to allow exceptions to be 
       passed back as the result of an async method. report is examined 
       for the same information as the exceptions that would have been raised
       from calling ProcessForm on the FormProcessor object. The calls to 
       the sink are considered Done if this method is called. *)
    Asynchronous FormProblem ( report : FormProblemReport )

End;


(* ************************************************************************ *)
(* *********************** FormProcessor Object *************************** *)

Type FormProcessor = Object  

    Supertypes NgBasic.NgObject End

    Methods
        
    (* ProcessForm - The caller supplies a FormInputElementSequence describing
       the form data.  If the supplied formEntries input is unacceptable (e.g.
       wrong type, etc.) the callee raises the InvalidFormEntries exception 
       which contains a sequence of the names of the entries which were 
       unacceptable, else a Rendering is returned.
    *)

    ProcessForm ( formEntries          : FormInputElementSequence, 
                 out responseCacheInfo : NgCache.OptionalResponseCacheControl ) 
                 : NgRendering.Rendering
        Raises InvalidFormEntries, NgBasic.WouldBlock, NgBasic.Conflict, 
               NgBasic.ObjectNotExist End,


    (* SendFormReply is just like ProcessForm only instead of synchronously 
       returning the Rendering, it sends it asynchronously via a call to the 
       ReceiveFormResult method on the supplied ProcessedFormSink *)

    Asynchronous SendFormReply ( formEntries  : FormInputElementSequence, 
                                 formSink     : ProcessedFormSink ),
                            
                            
    (* A Synchronous version of SendForm. The intent is to allow the 
    caller to simply know that the call was received. *)
    SendFormReplySynched( 
        formEntries     : FormInputElementSequence, 
        formSink        : ProcessedFormSink )                            

End;


(* ************************************************************************ *)
(* ******************************** END *********************************** *)
(* ************************************************************************ *)
