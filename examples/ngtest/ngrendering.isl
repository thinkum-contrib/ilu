(** $Id: ngrendering.isl,v 1.7 1999/08/03 01:58:11 janssen Exp $
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
(* *************************** Renderings ********************************* *)
(* ************************************************************************ *)

(* An Object of type Renderable is able to produce a rendering.  
   Examples renderings are a text description, a graphic, an html 
   page, an audio, etc.

   An Object of type RenderingSink is able to asynchrounously receive 
   renderings.
*)


Interface NgRendering Brand "NG" Imports 
    NgBasic                From ngbasic.isl,
    IANA-Charsets-Registry From ianacharsets.isl,
    NgCache                From ngcache.isl,
    NgStream               From ngstream.isl
End;


(* ************************************************************************ *)
(* *************************** Rendering Types ***************************** *)

(* Note:  any URIs here which are relative paths, are by default assumed to be
relative to the IANA registry - thus for example one might use "text/html".
xxx Need specification here about what the URI actually denotes *)

Type RenderingType             = NgBasic.URI;
Type RenderingTypeSequence     = NgBasic.URISequence;
Type RenderingEncoding         = NgBasic.URI;
Type RenderingEncodingSequence = NgBasic.URISequence;


(* The requestor of a rendering may supply rendering preferences.  The 
   preferences describe what characteristics a rendering must have.
   For those members which are sequences, an empty sequence indicates no
   particular preference, else the sequence is treated as an ordered
   list of preferences.  *)


(* ************************************************************************ *)
(* ******************** Rendering Preferences ***************************** *)
  
Type RenderingPreferences = Record

    (* analogous to HTTP Accept, Accept-Charset, Accept-Encoding
       Range, and User-Agent headers *)
       
    (* Which content types are actually acceptable is obtained by, taking the 
       set of all types specified by allowContentTypes (some of which may 
       actually indicate a group of types via wildcard or other means), and 
       subtracting from it the set of all types specified by 
       disallowContentTypes.  A zero length sequence for allowContentTypes 
       means all types.  A zero length sequence for disallowContentTypes
       means no types.  Should the result allowContentTypes - 
       disallowContentTypes  be empty, a NoRenderingMatch exception should be
       raised from the receiving method. *)
    allowContentTypes    : RenderingTypeSequence,
    disallowContentTypes : RenderingTypeSequence,
    
    (* Which encodings are actually acceptable is obtained by, taking the set 
       of all types specified by allowEncodings (some of which may actually 
       indicate a group of encodings via wildcard or other means), and 
       subtracting from it the set of all types specified by disallowEncodings.
       A zero length sequence for allowEncodings means all encodings.  A zero 
       length sequence for disallowEncodings means no encodings.  Should the
       result allowEncodings - disallowEncodings be empty, a 
       NoRenderingMatch exception should be raised from the receiving 
       method. *)
    allowEncodings    : RenderingEncodingSequence,
    disallowEncodings : RenderingEncodingSequence,
     
    (* Acceptable Charsets of the rendering bytes before any encoding. A zero 
       length sequence means any charset is acceptable  *)   
    acceptCharsets     : IANA-Charsets-Registry.CharsetMIBEnumValueSequence,

    (* Acceptible Locales of the rendering bytes before any encoding. A zero 
       length sequence means any locale is acceptable  *)   
    acceptLocales     : NgBasic.StringSequence,
    
    (* If not supplied, indicates that the entire rendering is requested.
       If supplied and of non-zero size, it indicates the range of bytes 
       desired from the rendering bytes before any encoding.  If the interval 
       is supplied and is of zero size, effect is similar to Http's Head 
       method. *)
    range              : NgBasic.OptionalUnsignedInclusiveInterval,
    
    (* xxx - Need specification here about what the URI actually denotes *)
    userAgent          : NgBasic.OptionalURI
    
End;


(* ************************************************************************ *)
(* *************************** Renderings ********************************* *)

(* actual bytes of a rendering *)
Type RenderingContentBytes       = Sequence Of Byte;  


Type RenderingChunk = Record
    contentRange    : NgBasic.OptionalUnsignedInclusiveInterval, 
    renderingBytes  : RenderingContentBytes    
End;
        

Type Rendering = Record
    (* analogous to HTTP Content-Type, Content-Range, Content-Language, 
       Content-Encoding and Content-MD5 headers *)
       
    (* Type of the rendering bytes before any encoding. *)
    contentType     : RenderingType,  
    
    (* Represents an ordered sequence of transformations that were applied to 
       the original contentType to arrive at the passed renderingBytes. e.g. 
       a,b,c means a( b( c(originalBytescontent)))  A zero length sequence 
       means no transformations were applied to the original bytes.*)
    contentEncoding : RenderingEncodingSequence,  
    
    (* If rangeEncoded is False, then contentRange is the range of the 
       rendering bytes before any encoding (unsupplied meaning all the bytes).
       If rangeEncoded is True, then contentRange applies to the transformed 
       bytes, e.g. to a( b( c(originalBytescontent))).  This is to allow for 
       the situation where there are intervening caches that have some of the
       encoded bytes available, but have no ability (through design or policy)
       to decode the bytes down to their original content. contentRange is 
       required with one exception: In the case where the RenderingPreferences 
       originally received specified a range of zero size (situation
       treated similarly to HTTP Head), and the actual size is undeterminable
       (e.g. a streaming live audio for example) then contentRange may be
       unsupplied, and in this case, rangeEncoded should be ignored. *)
    contentRange    : NgBasic.OptionalUnsignedInclusiveInterval, 
    rangeEncoded    : Boolean,
    
    (* charset the rendering is in - If not supplied, default is US-ASCII *)
    contentCharSet   : IANA-Charsets-Registry.CharsetMIBEnumValue, 
    
    (* locale rendering is in e.g. en-us, de, etc. If unspecified default en *)
    contentLocale : NgBasic.OptionalString,  
    
    (* encoded bytes of the rendering *)
    renderingBytes   : RenderingContentBytes 
    
End;

Type OptionalRendering = Optional Rendering;


(* ************************************************************************ *)
(* ******************** Rendering Exceptions **************************** *)

(* Can't supply or accept any rendering meeting the preferences or input - 
   contains a RenderingPreferences (with the optional range and userAgent not
   present) describing what is suppliable/acceptable *)
   
Exception NoRenderingMatch : RenderingPreferences;


(* ************************************************************************ *)
(* ********************** RenderingSink Object **************************** *)

(* a RenderingProblemReport is used to pass information to asynchronous
   callbacks that would have normally been passed back as an exception 
   from the synchronous version of the same sort of call *)
   
Type RenderingProblemReport = Union
    noMatch          : RenderingPreferences,
    wouldBlock       : NgBasic.ExceptionInformation,
    objectNotExist   : NgBasic.ExceptionInformation
End;
 

Type RenderingSink = Object

    Supertypes NgStream.DataSink End

    Methods
    
    (* Called to tell the sink information about how to do caching.  If this
       is never called, then the Sink may cache in any manner it wishes, so
       typically this will be called before any Renderings are sent.  
       Any call to this method remains in effect until a subsequent call
       to this method occurs. *)
    Asynchronous RegisterResponseCacheControl 
                 ( responseCacheInfo : NgCache.OptionalResponseCacheControl ),

    (* RenderingProblem is called when an exception would have been raised from 
       calling GetRendering (with the same args as SendRendering) on the 
       Renderable object.  This is basically to allow exceptions to be 
       passed back as the result of an async method. report is examined 
       for the same information as the exceptions that would have been raised
       from calling GetRendering on the Renderable object. The calls to 
       the sink are considered Done if this method is called. *)
    Asynchronous RenderingProblem ( report : RenderingProblemReport ),
        
    (* ReceiveRendering is called as a result of a SendRendering call on a 
       Renderable. The calls to ReceiveRendering and ReceiveRenderingChunk
       are considered complete when the Done method is called. *)
    Asynchronous ReceiveRendering ( therendering : Rendering ),
    
    (* ReceiveRenderingChunk is called to send rendering bytes that
       differ only in contentRange and renderingBytes from the 
       Rendering in the last call to ReceiveRendering *)
    Asynchronous ReceiveRenderingChunk ( thechunk : RenderingChunk ),
    
    (* Called by the DataSource when something happens to its state such 
       that it needs to know what was last received by the Sink.  If the 
       OptionalCardinal return value is supplied, it indicates (w.r.t
       rangeEncoded) the next byte expected by the sink.  If it is not 
       supplied, it indicates that the sink has not yet received any 
       rendering chunks.
       *)
    Resynchronize ( ) : NgBasic.OptionalCardinal


End;



(* ************************************************************************ *)
(* ************************* Renderable Object **************************** *)

Type Renderable = Object  

    Supertypes NgBasic.NgObject End

    Methods

    (* find out what types of renderings are available.  The returned 
    RenderingPreferences describes what can be supplied. *)
    GetAvailableRenderings () : RenderingPreferences 
        Raises NgBasic.WouldBlock, NgBasic.ObjectNotExist End, 


    (* GetRendering - The caller supplies a RenderingPreferences record
       to specify the desired characteristics of the rendering. 
       The callee's method returns the appropriate range of bytes for the 
       best match it has for the rendering Preferences.  If no match is 
       possible, the callee raises the NoRenderingMatch exception which 
       contains a  RenderingPreferences describing what can be supplied.
    *)

    GetRendering ( renderingPreferences  : RenderingPreferences, 
                   requestCacheInfo      : NgCache.OptionalRequestCacheControl,
                   out responseCacheInfo : NgCache.OptionalResponseCacheControl ) 
                 : Rendering
        Raises NoRenderingMatch, NgBasic.WouldBlock, NgBasic.ObjectNotExist End,


    (* SendRendering is just like GetRendering only instead of synchronously
       returning the Rendering, it sends it asynchronously via calls to the 
       ReceiveRendering method on the supplied RenderingSink, The number of 
       bytes in each Rendering sent is up to the caller of ReceiveRendering, 
       but should be part of the range specified in renderingPreferences.
       If present, suggestedChunkSize is a suggestion on how large to make 
       the data in the calls to ReceiveRendering.  Typically, the implementation
       of SendRendering would arrange the following sequence of calls on the
       RenderingSink.  (Where [] indicates optional and * indicates zero or more.)
       
       When things are successful:
       
           [RegisterSourceControl] 
           [RegisterResponseCacheControl]
           ReceiveRendering
           ReceiveRenderingChunk*
           Done
       
       When there's a problem:
       
           RenderingProblem
       
       See RenderingSink's ReceiveRendering method.*)

    Asynchronous SendRendering( 
        renderingPreferences : RenderingPreferences,
        requestCacheInfo     : NgCache.OptionalRequestCacheControl,
        renderSink           : RenderingSink,
        suggestedChunkSize   : NgBasic.OptionalCardinal ),

    (* A Synchronous version of SendRendering. The
      intent is to allow the caller to simply know that the
      call was received. *)
    SendRenderingSynched( 
        renderingPreferences : RenderingPreferences,
        requestCacheInfo     : NgCache.OptionalRequestCacheControl,
        renderSink           : RenderingSink,
        suggestedChunkSize   : NgBasic.OptionalCardinal )

End;


(* ************************************************************************ *)
(* ***************** Renderable that you can 'Put' to ********************* *)

Type PutableRenderable = Object  

    Supertypes NgRendering.Renderable End

    Methods
        
    (* PutRendering - The caller supplies a Rendering record describing
       the write operation to take place.  If the supplied rendering input
       is unacceptable (e.g. wrong type, etc.) the callee raises the 
       NoRenderingMatch exception which contains a RenderingPreferences 
       describing what is acceptable.
    *)

    PutRendering ( renderingInput    : Rendering )
        Raises NoRenderingMatch, NgBasic.WouldBlock, NgBasic.Conflict, 
               NgBasic.ObjectNotExist End

End;


(* ************************************************************************ *)
(* ******************************** END *********************************** *)
(* ************************************************************************ *)
