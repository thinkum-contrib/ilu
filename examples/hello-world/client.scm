#!/usr/bin/env iluguile
!#
;; $Id: client.scm,v 1.3 1999/08/03 01:59:06 janssen Exp $
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

(define (main argv)
  (ilu:init)
  (let ((sbh (list-ref argv 2))
	(class hello-world:service:class-record))
    (let ((theObject (ilu:sbh-to-object sbh class))
	  (status-param (make-hello-world:status)))
      (if (not theObject)
	  (error "Unable to import hello-world service with SBH <~a>~%" sbh))
      (let ((response (hello-world theObject status-param)))
	(if (get-return-code status-param)
	    (format #t "(hello-world theObject) fails with exception <~a>~%"
		    (get-return-code status-param))
	    (format #t "~a~%" response))))))

(main (command-line))
