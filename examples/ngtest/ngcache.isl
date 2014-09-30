(** $Id: ngcache.isl,v 1.3 1999/08/03 01:58:16 janssen Exp $
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

(* ************************************************************************ *)
(* *************************** Caching Control **************************** *)
(* ************************************************************************ *)

(* Caching control provides cache and proxy relevant information that
   may be sent along with requests and responses.  This mostly mimics
   the cache related headers and Cache-Control header values found in 
   HTTP 1.1.  See that specification for the semantics. 
*)



Interface NgCache Brand "NG" Imports 

    NgBasic From ngbasic.isl
    
End;


(* ************************************************************************ *)
(* *************************** Entity tags ******************************** *)

Type EntityTag             = NgBasic.String;
Type OptionalEntityTag     = Optional NgBasic.String;

Type EntityTagSequence     = Sequence Of EntityTag;

Type EntityTagOrDate = Union
    etag   : EntityTag,
    ifdate : NgBasic.AbsoluteTime
End;    


(* ************************************************************************ *)
(* ***************** Request Cache Control ******************************** *)

Type RequestCacheControl = Record

    (* analogous to HTTP Cache-Control header values *)
    noCache         : Boolean,
    noStore         : Boolean,
    noTransform     : Boolean,
    onlyIfCached    : Boolean,
    
    (* analogous to HTTP If-ModifiedSince, If-Match, If-None-Match and
       If-Range headers *)                                      
    ifModifiedSince : NgBasic.OptionalAbsoluteTime,
    ifMatch         : EntityTagSequence,
    ifNoneMatch     : EntityTagSequence,
    ifRange         : EntityTagOrDate

End;

Type OptionalRequestCacheControl = Optional RequestCacheControl;  


(* ************************************************************************ *)
(* ***************** Response Cache Control ******************************* *)

Type ResponseCacheControl = Record
   
    (* analogous to HTTP  Cache-Control header values*)
    okPublic        : Boolean,
    isPrivate       : Boolean,
    noCache         : Boolean,
    noStore         : Boolean,
    noTransform     : Boolean,
    mustRevalidate  : Boolean,
    proxyRevalidate : Boolean,
    maxAge          : NgBasic.OptionalRelativeTime,
    sMaxAge         : NgBasic.OptionalRelativeTime,
    
    (* analogous to HTTP Age, Vary, Etag, Last-Modified and Expires headers *)
    age             : NgBasic.OptionalRelativeTime,
    vary            : NgBasic.StringSequence,
    entityTag       : OptionalEntityTag,
    lastModified    : NgBasic.OptionalAbsoluteTime,
    expires         : NgBasic.OptionalAbsoluteTime
   
End;
   
Type OptionalResponseCacheControl = Optional ResponseCacheControl;


(* ************************************************************************ *)
(* ******************************** END *********************************** *)
(* ************************************************************************ *)
