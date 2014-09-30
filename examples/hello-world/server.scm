#!/usr/bin/env iluguile
!#
;; $Id: server.scm,v 1.3 1999/08/03 01:59:05 janssen Exp $
;; BeginILUCopyright

;; Copyright (c) 1991-1999 Xerox Corporation.  All Rights Reserved.

;; Unlimited use, reproduction, modification, and distribution of this
;; software and modified versions thereof is permitted.  Permission is
;; granted to make derivative works from this software or a modified
;; version thereof.  Any copy of this software, a modified version
;; thereof, or a derivative work must include both the above copyright
;; notice of Xerox Corporation and this paragraph.  Any distribution of
;; this software, a modified version thereof, or a derivative work must
;; comply with all applicable United States export control laws.  This
;; software is made available AS IS, and XEROX CORPORATION DISCLAIMS ALL
;; WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
;; LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
;; EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
;; NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGES.

;; EndILUCopyright

(require "hello-world")
(require "hello-world-server-stubs")

(define (make-hello-world:service-impl server)
  (ilu-object-with-ancestors
   ((hello-world:service (make-hello-world:service)))
   ((get-server this) server)
   ((hello-world hello-world:service-impl . status-only)
    (ilu-bind-arguments
     (status-only status)
     (set-return-code status hello-world:reply-success)
     "\"Hello, World!\" from Guile Scheme"))))

(define (main argv)
  (let ((theServer #f)
	(theObject #f))

    (ilu:init)

    (set! theServer (ilu-server:create #f #f))
    (ilu-server:add-port theServer #f #f #t)

    (let ((theObject (make-hello-world:service-impl theServer)))
      (if theObject
	  (begin
	    (format #t "hello world server is ~a~&" (string-binding-handle theObject))
	    (ilu:run-main-loop (ilu:make-main-loop-id)))
	  (error "could not create object"))
      )))

(main (command-line))
