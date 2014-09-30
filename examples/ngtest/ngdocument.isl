(** $Id: ngdocument.isl,v 1.4 1999/08/03 01:58:15 janssen Exp $
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
(* *************************** Web Documents ****************************** *)
(* ************************************************************************ *)

(* Defines the concept of Web Documents *)


(* Import iluhttp so that we can offer easy compatability with the existing
   web - that is, anything object inheriting from iluhttp.Resource will
   support Get, Head, Put and Post methods as we know them in the current web
*)

Interface NgDocument Brand "NG" Imports 
    iluhttp         From iluhttp.isl,
    NgRendering     From ngrendering.isl, 
    NgRendering     From ngrendering.isl, 
    NgCache         From ngcache.isl, 
    NgProperty      From ngproperty.isl,
    NgStream        From ngstream.isl
    End;


(* ************************************************************************ *)
(* ******************** RenderingAndPropertiesSink  *********************** *)

(* a RenderingProblemReport is used to pass information to asynchronous
   callbacks that would have normally been passed back as an exception 
   from the synchronous version of the same sort of call *)
   
Type PropertiesProblemReport = Union
    wouldBlock        : NgBasic.ExceptionInformation,
    unknownProperties : NgProperty.PropertyNames
End;
    

Type RenderingAndPropertiesSink = Object  

    Supertypes NgRendering.RenderingSink End

    Methods
    
    (* PropertyProblem is called when a property related exception would 
       have been raised from calling GetRenderingAndProperties (with the 
       same args as SendRenderingAndProperties)  This is basically to 
       allow exceptions to be passed back as the result of an async method. 
       report is examined for the same information as the exceptions that 
       would have been raised from calling GetProperties on the WebDocument 
       object. The calls to the sink are considered Done if this method is 
       called. *)
    Asynchronous PropertiesProblem( report : PropertiesProblemReport),
    
    
     (* ReceiveProperties is called as a result of a
        SendRenderingAndProperties call on a WebDocument. *)

    Asynchronous ReceiveProperties (theproperties : NgProperty.PropertySequence)

End;


(* ************************************************************************ *)
(* ************************** WebDocument Interface *********************** *)

(* A WebDocument is meant to be an NG version of roughly what Web documents 
   are today *)
   
Type WebDocument = Object

    Supertypes  
        NgRendering.Renderable, 
        NgProperty.PropertySet
        End
  

    (* ************
       Properties which may be accessible via the property set interface
       include 

       Authors - a NgBasic.StringSequence,  The authors of this document

       CreationTime - a NgBasic.AbsoluteTime

       Version - a NgBasic.Version,

       Summary a NgBasic.String - some (human readable) summary of what
                                  the document is
    
      Note: The next two are analogous to the lastModified and expires members
            of the ResponseCacheControl Record
       
      LastModificationTime a NgBasic.AbsoluteTime
    
      ExpectedChange an NgBasic.AbsoluteTime - when this document is expected
                                               to change
    
      ***********  *)

Methods

    (* basically a combination of the GetRendering and GetProperties methods *)
    GetRenderingAndProperties (
        renderingPreferences  : NgRendering.RenderingPreferences, 
        requestCacheInfo      : NgCache.OptionalRequestCacheControl,
        out responseCacheInfo : NgCache.OptionalResponseCacheControl,
        propertiesToGet       : NgProperty.PropertyNames, 
        Out theproperties     : NgProperty.PropertySequence
                            ) : NgRendering.Rendering
            Raises NgRendering.NoRenderingMatch,
                   NgProperty.UnknownPropertyNames, 
                   NgBasic.WouldBlock,
                   NgBasic.ObjectNotExist End,

    (* SendRenderingAndProperties is just like GetRenderingAndProperties only 
       instead of synchronusly returning the results, they are sent 
       asynchronously via calls to the ReceiveRenderingAndProperties method 
       on the supplied RenderingAndPropertiesSink. The number of bytes in each 
       Rendering sent is up to the caller of ReceiveRenderingAndProperties, but 
       should be part of the range specified in renderingPreferences. 
       If present, suggestedChunkSize is a suggestion on how large to make 
       the data in the calls to ReceiveRenderingAndProperties. Typically, the 
       implementation of SendRenderingAndProperties would arrange the following 
       sequence of calls on the RenderingAndPropertiesSink.  (Where [] indicates 
       optional and * indicates zero or more.)
       
       When things are successful:
       
           [RegisterSourceControl] 
           [RegisterResponseCacheControl]
           ReceiveProperties
           ReceiveRendering
           ReceiveRenderingChunk* 
           Done
       
       When there's a problem:
       
           RenderingProblem OR PropertiesProblem

       See
       RenderingAndPropertiesSink's ReceiveRenderingAndProperties method.*)
                  
    Asynchronous SendRenderingAndProperties (
        renderingPreferences  : NgRendering.RenderingPreferences, 
        requestCacheInfo      : NgCache.OptionalRequestCacheControl,
        propertiesToGet       : NgProperty.PropertyNames,
        renderPropSink        : RenderingAndPropertiesSink,
        suggestedChunkSize    : NgBasic.OptionalCardinal ),
        
   (* A Synchronous version of SendRenderingAndProperties. The
      intent is to allow the caller to simply know that the
      call was received. *)
    SendRenderingAndPropertiesSynched( 
        renderingPreferences  : NgRendering.RenderingPreferences, 
        requestCacheInfo      : NgCache.OptionalRequestCacheControl,
        propertiesToGet       : NgProperty.PropertyNames,
        renderPropSink        : RenderingAndPropertiesSink,
        suggestedChunkSize    : NgBasic.OptionalCardinal )        
        
End;


(* ************************************************************************ *)
(* *************** WebDocument that you can 'Put' to ********************** *)

Type PutableWebDocument = Object

    Supertypes 
        NgDocument.WebDocument,
        NgRendering.PutableRenderable,
        NgProperty.PutablePropertySet
        End
  
Methods
                     
    (* basically combination of the PutRendering and PutProperties methods *)
    PutRenderingAndProperties (
        renderingInput    : NgRendering.Rendering, 
        propertiesToSet   : NgProperty.PropertyModificationSequence)
            Raises NgRendering.NoRenderingMatch,
                   NgProperty.UnknownPropertyNames, 
                   NgBasic.WouldBlock,
                   NgBasic.Conflict, NgBasic.ObjectNotExist End
        
End;



(* ************************************************************************ *)
(* ************** HTTPCompatibleWebDocument Interface ************ ******** *)

(* A HTTPCompatibleWebDocument is a WebDocument that can also be accessed via 
   HTTP 1.x Get, Head and Post calls. *)
   
Type HTTPCompatibleWebDocument = Object

    Supertypes  
        WebDocument, 
        iluhttp.Resource
        End
;
  

(* ************************************************************************ *)
(* ******************************** END *********************************** *)
(* ************************************************************************ *)
