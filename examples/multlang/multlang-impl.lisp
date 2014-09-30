;;;-*- Package: USER; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-
;;; Last edited by Mike Spreitzer October 9, 1998 10:05 am PDT
#|
 $Id: multlang-impl.lisp,v 1.3 1999/08/03 01:57:50 janssen Exp $
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

(cl:in-package :user)

;;;
;;; first, provide an implementation of the Multiplier object in Lisp
;;;
(defclass my-multiplier (multlang:multiplier.impl) ())
(defmethod multlang:multiply ((self my-multiplier) val1 val2)
  (* val1 val2))

;;;
;;; provide a hook to call to initialize the C++ side of the world
;;;
(ilu::define-c-function initialize-c++-code
    "Start the C++ side of the world running, and create the necessary objects"
  "multlang_StartCPlusPlus"
  () :boolean)

;;;
;;; provide a function which initializes the Lisp side of the world
;;;
(defun initialize-lisp-code ()
  (let* ((server (make-instance 'ilu:kernel-server :id "Server1"))
	    (multiplier (and server
			     (make-instance 'multlang:multiplier
			       :ilu-kernel-server server
			       :ilu-instance-handle "theMultiplierObject"))))
    (if (and server multiplier)
	(progn
	  (format t "Created Multiplier object <~a>~%"
		  (ilu:string-binding-handle multiplier))
	  (ilu:publish multiplier)
	  t))))

;;;
;;; finally, a main program
;;;
(defun main ()

  ;; first, initialize
  (when (and (initialize-lisp-code)
	     (initialize-c++-code))
    
    ;; bind service objects
    
    (let ((squarer (ilu:lookup "Server2" "theSquarerObject" 'multlang:squarer)))

      ;; now do the calculations
      
      (let ((val (multlang:obtain-square squarer 21)))
	(format t "square of 21 is ~a.~%" val))
      (let ((val (multlang:obtain-square squarer #xFFFFFFF3)))
	(format t "square of #xFFFFFFF3 is ~a.~%" val))
      (let ((val (multlang:obtain-square squarer #xFFF3)))
	(format t "square of #xFFF3 is ~a.~%" val))
      (format t "All calls behaved as expected.~%")
      )))
