;;;-*- Package: TEST1-SERVER; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-
;;; Last edited by Mike Spreitzer October 9, 1998 11:35 am PDT
#|
 $Id: server.lisp,v 1.15 1999/08/03 01:52:14 janssen Exp $
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

(cl:defpackage :test1-server
  (:use :common-lisp :ilu)
  (:export #:start-server #:start-server-iiop))

(cl:in-package :test1-server)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Test1.O1
;;;
(defclass test1-o1 (test1:the-o1.impl) ())

(defmethod test1:the-o1.u-css-to-u ((self test1-o1) u css)
  (format t "test1:the-o1.u-css-to-u (~s ~s ~s) => ~s~%" self u css u)
  u)

(defmethod test1:the-o1.f-css-to-ro ((self test1-o1) css)
  (let ((ro (test1:make-the-r
	     :i 9
	     :css (list "just" "strings")
	     :a (make-array 3 :element-type 'test1:a0
			    :initial-contents '("just" "three" "strings")))))
    (format t "test1:the-o1.f-css-to-ro (~s ~s) => ~s~%" self css ro)
    ro))

(defmethod test1:the-o1.r-sc-s-to-f ((self test1-o1) r s)
  (let ((f 39.7))
    (format t "test1:the-o1.r-sc-s-to-f (~s ~s ~s) => ~s~%" self r s f)
    f))

(defmethod test1:the-o1.a-ro ((self test1-o1) ro)
  (format t "test1:the-o1.a-ro (~s ~s)~%" self ro)
  )

(defvar *my-o2* nil)

(defmethod test1:the-o1.get-o2 ((self test1-o1))
  (format t "test1:the-o1.get-o2 (~s) => ~s~%" self *my-o2*)
  *my-o2*)

(defvar *one* 0)

(defmethod test1:the-o1.get-o3 ((self test1-o1) b)
  (let ((o3 (if b (make-instance 'test3-o)
	      (if (= *one* 0)
		  (progn
		    (setq *one* 1)
		    (make-instance 'test1-o3))
		(progn
		  (setq *one* 0)
		  (make-instance 'test1-o4))))))
    (format t "test1:the-o1.get-o3 (~s ~s) => ~s~%" self b o3)
    o3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Test1.O2
;;;
(defclass test1-o2 (test1:o2.impl) ())

(defmethod test1:o2.oo-a0-to-css ((self test1-o2) o a)
  (format t "test1:o2.oo-a0-to-css (~s ~s ~s) => " self o a)
  (unless o
    (signal 'test1:e2 :exception-value 7))
  (let ((css nil))
    (format t "~s~%" css)
    css))

(defmethod test1:o2.r-i-a1-to-i-a0 ((self test1-o2) r i a)
  (let ((a2 (make-array 8 :element-type '(unsigned-byte 8))))
    (format t "test1:o2.r-i-a1-to-i-a0 (~s ~s ~s ~s) => ~s ~s~%" self r i
	    a a2 i)
    (values a2 i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Test1.O3
;;;
(defclass test1-o3 (test1:o3.impl) ())

(defmethod test1:o3.rs-r-to-r-is ((self test1-o3) r)
  (let ((r2 (test1:make-the-r
	     :i 3
	     :css (list "another" "css" "seq" "of" "strings")
	     :a (make-array 3 :element-type 'test1:Sc-S
			    :initial-contents '("array" "of" "strings"))))
	(is nil))
    (format t "test1:o3.rs-r-to-r-is (~s ~s) => ~s ~s~%" self r is r2)
    (values is r2)))

(defmethod test1:o3.o1-u-to-u ((self test1-o3) o u)
  (format t "test1:o3.o1-u-to-u (~s ~s ~s) => ~s~%" self o u o)
  (cons 3 o))

(defmethod test1:o3.bs-to-i ((self test1-o3) b)
  (let ((len (* (length b) (length b))))
    (format t "test1:o3.bs-to-i (~s ~s) => ~s~%" self b len)
    len))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Test1.P
;;;
(defclass test1-p  (test1-o3 test1:p.impl) ())

(defmethod test1:o3.rs-r-to-r-is ((self test1-p) r)
  (let ((r2 (test1:make-the-r :i 25719 :css nil :a (make-array 3 :element-type
							       'test1:Sc-S)))
	(is nil))
    (format t "test1:o3.rs-r-to-r-is (~s ~s) => ~s ~s~%" self r is r2)
    (values is r2)))

(defmethod test1:o3.bs-to-i ((self test1-p) b)
  (format t "test1:o3.bs-to-i (~s ~s) => ~s~%" self b (length b))
  (length b))

(defmethod test1:p.m2 ((self test1-p) j)
  (let ((rval (list j (* j j))))
    (format t "test1:p.m2 (~s ~s) => ~s~%" self j rval)
    rval))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Test1.O4
;;;
(defclass test1-o4 (test1-o3 test1:o4.impl) ())

(defmethod test1:o4.r-to-r ((self test1-o4) r)
  (let ((r2 1020304.05060708D0))
    (format t "test1:o4.r-to-r (~s ~s) => ~s~%" self r r2)
    r2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Test2.P
;;;
(defclass test2-p  (test2:p.impl) ())

(defmethod test2:p.sr-to-i ((self test2-p) sr)
  (format t "test2:p.sr-to-i (~s ~s) => ~s~%" self sr (round sr))
  (round sr)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Test3.O
;;;
(defclass test3-o  (test1-o3 test2-p test3:o.impl) ())

;; the inheritance here is so hairy, we'll just direct ILU to the
;; right class record explicitly
(defmethod ilu::find-ilu-class-record ((self test3-o))
  (ilu::ilu-class-record 'test3:o))

(defmethod test3:o.i-to-test1-u ((self test3-o) i)
  (format t "test3:o.i-to-test1-u (~s ~s) => ~s~%" self i t)
  (cons 5 t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Start the server by creating an instance of Test1.O1
;;;

#+ilu-iiop
(defun start-server-iiop ()
  (let* ((sks (make-instance 'ilu:kernel-server
			    :protocol "sunrpc"
			    :transport '("sunrpcrm" "tcp_0_0")
			    ))
	 (ks (make-instance 'ilu:kernel-server
			    :id "Test1-Server"
			    :protocol "iiop_1_0_1"
			    :transport '("tcp_0_0")
			    ))
	 (sv (make-instance 'test1-o1
			    :ilu-kernel-server ks
			    :ilu-instance-handle "Test1_Initial_Object")))
    (format t "SBH:  ~a~%" (ilu:string-binding-handle sv))
    (ilu:publish sv)
    (setf *my-o2* (make-instance 'test1-o2
				 :ilu-kernel-server sks
				 :ilu-instance-handle "the-02"))
    sv))

(defun start-server ()
  (let* ((ks (make-instance 'ilu:kernel-server :id "Test1-Server"))
	 (sks (make-instance 'ilu:kernel-server
		:protocol "sunrpc"
		:transport '("sunrpcrm" "tcp_0_0")
		:default-server nil))
	 (sv (make-instance 'test1-o1
	       :ilu-kernel-server ks
	       :ilu-instance-handle "Test1_Initial_Object")))
    (format t "SBH:  ~a~%" (ilu:string-binding-handle sv))
    (ilu:publish sv)
    (setf *my-o2* (make-instance 'test1-o2
				 :ilu-kernel-server sks
				 :ilu-instance-handle "the-02"))
    sv))
