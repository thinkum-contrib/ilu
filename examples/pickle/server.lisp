;;;-*- Package: PICKLE-TEST; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-
;;; Last edited by Mike Spreitzer October 9, 1998 10:45 am PDT
#|
 $Id: server.lisp,v 1.5 1999/08/03 01:58:45 janssen Exp $
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
|#

(cl:defpackage :pickle-test
  (:use :common-lisp :ilu)
  (:export #:start-server))

(cl:in-package :pickle-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  pickle-test
;;;
(defclass pickle-test (test2:o1.impl) ())

(defmethod test2:o1.bounce ((self pickle-test) v)
  (format t "type:  ~s     value:  ~s~%" (pickle-type v) (pickle-value
  v))
  v)

(defun start-server ()
  (let* ((ks (make-instance 'ilu:kernel-server :id "pickleServer"))
	 (sv (make-instance 'pickle-test
			    :ilu-kernel-server ks
			    :ilu-instance-handle "pickleObj")))
    (ilu:publish sv)
    (format t "SBH:  ~a~%" (ilu:string-binding-handle sv))
    sv))
