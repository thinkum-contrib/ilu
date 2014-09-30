(*
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

(* $Id: iluhttp.isl,v 1.7 1999/08/03 01:50:17 janssen Exp $ *)


(* 
   ILU Http Ilu_Http_1_0_resource_object ISL

	This is a modification of the Proposed ILU-Requester interface 
	 (a CGI alternative) by Digital Creations info@digicool.com
	 Reference: http://www.w3.org/pub/WWW/Protocols/HTTP1.0/draft-ietf-http-spec.html
			
*)

INTERFACE iluhttp;


(* -------------------- Header related Types ------------------------- *)

TYPE field-name = ilu.CString;
TYPE field-value = ilu.CString;
TYPE optional-field-value = OPTIONAL field-value;
TYPE Header = RECORD			(* message header *)
  name  : field-name,
  value : optional-field-value
END;
TYPE HTTPHeader = Header;
TYPE HTTPHeaders = SEQUENCE of HTTPHeader;

(* -------------------- Entity Body related Types -------------------- *)

TYPE EntityBody = SEQUENCE of BYTE;
TYPE OptionalEntityBody = OPTIONAL EntityBody;

(* -------------------- Request URI related Types -------------------- *)

TYPE RequestURI = ilu.CString;

(* -------------------- Full Request Types --------------------------- *)

TYPE Request = RECORD		(* 'mostly' a http full request *)

  URI     : RequestURI,	(* This can be the absoluteURI or abs_path uri - including params, 
			   queries, etc. (if it's the full absoluteURI or abs_path, then the 
			   scheme, netpath, and path portion of this should be http:, 
			   the netpath should agree with the server id, and the path the 
			   same as the object ID although this isn't checked), 
			   OR more commonly it can be just the params, queries, e.g. ;foo;bar?zap *)

  headers : HTTPHeaders,(* The general, request and entity headers NOTE: if the user 
			   didn't supply a Content-Length header, ilu's http 
			   will automatically put in a Content-Length header if an
			   Entity body is supplied. Note that when responding to a 
			   HEAD method then (since there is no body) the user should 
			   supply a Content-Length header. *)

  body    : OptionalEntityBody
			(* may or may not be some body in a request *)
  END;



(* -------------------- Response related Types ----------------------- *)

TYPE StatusCode = ENUMERATION	(* some possible status return codes *)
	OK = 200,
	Created = 201,
	Accepted = 202,
	NoContent = 204,
	MovedPermanently = 301,
	MovedTemporarily = 302,
	NotModified = 304,
	BadRequest = 400,
	Unauthorized = 401,
	Forbidden = 403,
	NotFound = 404,
	InternalError = 500,
	NotImplemented = 501,
	BadGateway = 502,
	ServiceUnavailable = 503
  END;

TYPE Response = RECORD		(* a http full response *)

  status  : StatusCode,		(* status of servicing the request *)

  headers : HTTPHeaders,	(* the general, response and entity headers *)

  body    : OptionalEntityBody	(* may or may not be some body in a response *)
END;


(* -------------------- Resource Object ------------------------------ *)

TYPE Resource = OBJECT		(* the object that knows the standard http methods *)

  (* NOTE the following TYPEID MUST AGREE with the definition
     of HTTP_RESOURCE_OBJECT_TYPE_ID in the file
     src/runtime/kernel/httpprot.h *)
  TYPEID "ilu:Ilu_Http_1_0_resource_object"

  METHODS	(* the standard http 1.0 methods *)

    GET  (request: Request) : Response,	
    HEAD (request: Request) : Response,  
    POST (request: Request) : Response

  END;


(* ------------------------------------------------------------------- *)
(* -------------------- end of file ---------------------------------- *)
(* ------------------------------------------------------------------- *)

