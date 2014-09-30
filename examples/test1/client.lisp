;;;-*- Package: USER; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-
;;; Last edited by Mike Spreitzer October 9, 1998 11:25 am PDT
#|
 $Id: client.lisp,v 1.8 1999/08/03 01:52:11 janssen Exp $
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

(defun test-server ()
  (let ((server (ilu:lookup "Test1-Server" "Test1_Initial_Object" 'test1:the-o1)))
    (if (not server)
	(error "Couldn't bind server.~%")
      (progn
	(let* ((u1 '(5 . t))
	       (css '("just" "some" "strings"))
	       (u2 (test1:the-o1.u-css-to-u server u1 css)))
	  (format t "(the-o1.u-css-to-u ~s ~s) => ~s~%" u1 css u2))
	(let* ((css '("just" "some" "strings"))
	       (r (test1:the-o1.f-css-to-ro server css)))
	  (format t "(the-o1.f-css-to-r0 ~s) => ~s~%" css r))
	(let* ((r (test1:make-the-r :i 238 :css '("more" "strings") :a #("test1" "test2" "bletch")))
	       (s "just a string")
	       (f (test1:the-o1.r-sc-s-to-f server r s)))
	  (format t "(the-o1.r-sc-s-to-f ~s ~s) => ~s~%" r s f))
	(let* ((ro nil))
	  (test1:the-o1.a-ro server ro)
	  (format t "(the-o1.a-ro ~s)~%" ro))
	(let* ((o2 (test1:the-o1.get-o2 server)))
	  (format t "(the-o1.get-o2) => ~s~%" o2)
	  (if (not o2)
	      (error "Can't get instance of O2~%")
	    (progn
	      (let* ((o server)
		     (a (make-array 8 :element-type '(unsigned-byte 8)))
		     (css (test1:o2.oo-a0-to-css o2 o a)))
		(format t "(test1:o2.oo-a0-to-css ~s ~s) => ~s~%" o a css))
	      (let ((r (test1:make-the-r :i 13 :css '("another" "list" "of" "strings") :a #("and" "an" "array")))
		    (i 3289)
		    (a1 (make-array 3 :element-type 'simple-string)))
		(setf (aref a1 0) "string 1")
		(setf (aref a1 1) "string 2")
		(setf (aref a1 2) "string 3")
		(multiple-value-bind (i2 a0)
		    (test1:o2.r-i-a1-to-i-a0 o2 r i a1)
		  (format t "(o2.r-i-a1-to-i-a0 ~s ~s ~s) => ~s ~s~%" r i a1 i2 a0)))
	      )))
	(let* ((o3 (test1:the-o1.get-o3 server nil)))
	  (format t "(the-o1.get-o3 nil) => ~s~%" o3)
	  (if (null o3)
	      (error "Couldn't construct o3 from ~s~%" server)
	    (progn
	      (let ((r (list (test1:make-the-r :i 2349 :css '("more") :a #("one" "two" "three")))))
		(multiple-value-bind (r2 is)
		    (test1:o3.rs-r-to-r-is o3 r)
		  (format t "(o3.rs-r-to-r-is ~s) => ~s ~s~%" r r2 is)))
	      (let* ((o server)
		     (u '(5 . t))
		     (u2 (test1:o3.o1-u-to-u o3 o u)))
		(format t "(o3.o1-u-to-u ~s ~s) => ~s~%" o u u2))
	      )))
	))))
