#!/usr/bin/env iluguile
!#
;; $Id: server.scm,v 1.7 1999/08/03 01:52:25 janssen Exp $
;;
;; Support for Guile Scheme has been contributed by Siemens Corporate Research, Inc.
;;
;; Copyright (c) 1991-1999 Xerox Corporation.  All Rights Reserved.  
;; All rights reserved.
;;
;; BeginILUCopyright

;; Copyright (c) 1991-1998 Xerox Corporation.  All Rights Reserved.

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

(require "Test1")
(require "Test2")
(require "Test3")
(require "Test1-server-stubs")
(require "Test2-server-stubs")
(require "Test3-server-stubs")

;;///////////////////// Test1_T_O1_impl methods /////////////////////

(define Test1:O1-impl-uc #f)
(define Test1:O1-impl-one 0)

(define (make-Test1:O1-impl ih server)
  (let* ((ourInstanceHandle ih)
	 (ourServer server)
	 (obj
	  (ilu-object-with-ancestors
	   (
	    (Test1:O1 (make-Test1:O1))
	   )

	   ((get-instance-handle this)
	    ourInstanceHandle)

	   ((get-server this)
	    ourServer)

	   ((U-CSS-to-U Test1:O1-impl . status-and-u-and-css)
	    (ilu-bind-arguments
	     (status-and-u-and-css status u css)
	     (let ((res #f))
	       (format #t "Test1.O1.U-CSS-to-U~&")
	       (set! res u)
	       (set-return-code status Test1:reply-success)
	       res)
	     )
	    )

	   ((f-CSS-to-RO Test1:O1-impl . status-and-css)
	    (ilu-bind-arguments
	     (status-and-css status css)
	     (let ((x (make-Test1:R)))
	       (format #t "Test1.O1.f-CSS-to-R0~&")
	       (set-i x 9)
	       (set-css x (Test1:CSS (list "aaa")))
	       (set-a x (make-vector 3))
	       (vector-set! (get-a x) 0 "hi1")
	       (vector-set! (get-a x) 1 "hi2")
	       (vector-set! (get-a x) 2 "hi3")
	       (set-return-code status Test1:reply-success)
	       x)
	     )
	    )

	   ((R-ScS-to-F Test1:O1-impl . status-and-r-and-scs)
	    (ilu-bind-arguments
	     (status-and-r-and-scs status r scs)
	     (format #t "Test1.O1.R-ScS-to-F~&")
	     (set-return-code status Test1:reply-success)
	     39.7)
	    )

	   ((a-RO Test1:O1-impl . status-and-ro)
	    (ilu-bind-arguments
	     (status-and-ro status ro)
	     (format #t "Test1.O1.a-RO~&")
	     (set-return-code status Test1:reply-success)
	     #t)
	    )

	   ((get-O2 Test1:O1-impl . status)
	    (format #t "Test1.O1.get-O2~&")
	    (if (not Test1:O1-impl-uc)
		(set! Test1:O1-impl-uc (make-Test1:O2-impl)))
	    (if (not Test1:O1-impl-uc)
		(set-return-code (car status) Test1:CantCreate)
		(set-return-code (car status) Test1:reply-success))
	    Test1:O1-impl-uc)

	   ((get-O3 Test1:O1-impl . status-and-subclass)
	    (ilu-bind-arguments
	     (status-and-subclass status subclass)
	     (let ((uc #f))
	       (format #t "Test1:O1.get-03~&")
	       (if subclass
		   (set! uc (make-Test3:O-impl))
		   (begin
		     (if (= Test1:O1-impl-one 0)
			 (begin
			   (set! Test1:O1-impl-one 1)
			   (format #t "making O3...~&")
			   (set! uc (make-Test1:O3-impl)))
			 (begin
			   (set! Test1:O1-impl-one 0)
			   (format #t "making O4...~&")
			   (set! uc (make-Test1:O4-impl)))
			 )))
	       (if (not uc)
		   (set-return-code status Test1:CantCreate)
		   (set-return-code status Test1:reply-success))
	       uc)
	     )
	    )
	   )
	  )
	 )
    obj)
  )



;;///////////////////// Test1_T_O2_impl methods /////////////////////

(define (make-Test1:O2-impl)
  (ilu-object-with-ancestors
   (
    (Test1:O2 (make-Test1:O2))
   )

   ((OO-A0-to-CSS Test1:O2-impl . status-and-o-and-a0)
    (ilu-bind-arguments
     (status-and-o-and-a0 status o a0)
     (format #t "Test1.O2.OO-A0-to-CSS~&")
     (if (not o)
	 (begin
	   (set-return-code status Test1:E2-exception)
	   (set-status-value status 7)
	   (list)
	   )
	 (begin
	   (set-return-code status Test1:reply-success)
	   (Test1:CSS (list "xxx"))
	   )
	 )
     )
    )

   ((R-I-A1-to-I-A0 Test1:O2-impl . status-and-r-and-i-and-a1)
    (ilu-bind-arguments
     (status-and-r-and-i-and-a1 status r i a1)
     (format #t "Test1.O2.R-I-A1-to-I-A0~&")
     (set-return-code status Test1:reply-success)
     (list i (make-vector 8 #\space))
     )
    )
   )
  )

;; ///////////////////// Test1_T_O3_impl methods /////////////////////

(define (make-Test1:O3-impl)
  (ilu-object-with-ancestors
   (
    (Test1:O3 (make-Test1:O3))
   )

   ((RS-R-to-R-IS Test1:O3-impl . status-and-rs-and-r2)
    (ilu-bind-arguments
     (status-and-rs-and-r2 status rs r2)
     (let ((is (list)))
       (format #t "Test1.O3.RS-R-to-R-IS~&")
       (set! r2 (make-Test1:R))
       (set-i r2 3)
       (set-css r2 (Test1:CSS (list "xxx")))
       (set-a r2 (make-vector 3))
       (vector-set! (get-a r2) 0 "just")
       (vector-set! (get-a r2) 1 "a")
       (vector-set! (get-a r2) 2 "string")
       (list r2 is)
      )
     )
    )

   ((O1-U-to-U Test1:O3-impl . status-and-o-and-u)
    (ilu-bind-arguments
     (status-and-o-and-u  status o u)
     (format #t "Test1.O3.O1-U-to-U~&")
     (set-car! u 3)
     (set-cdr! u o)
     (set-return-code status Test1:reply-success)
     (list u #t)
     )
    )

   ((BS-to-I Test1:O3-impl . status-and-b)
    (ilu-bind-arguments
     (status-and-b status b)
     (set-return-code status Test1:reply-success)
     (* (length b) (length b))
     )
    )
   )
  )

;; ///////////////////// Test1_T_P_impl methods /////////////////////

(define (make-Test1:P-impl)
  (ilu-object-with-ancestors
   (
    (Test1:P (make-Test1:P))
   )

   ((RS-R-to-R-IS Test1:P-impl . status-and-rs-and-r2)
    (ilu-bind-arguments
     (status-and-rs-and-r2 status rs r2)
     (let ((is (list)))
       (format #t "Test1.P.RS-R-to-R-IS~&")
       (set! r2 (make-Test1:R))
       (set-i r2 25179)
       (set-css r2 (Test1:CSS (list "xxx" "yyy")))
       (set-a r2 (make-vector 3))
       (vector-set! (get-a r2) 0 "from")
       (vector-set! (get-a r2) 1 "P")
       (vector-set! (get-a r2) 2 "string")
       (set-return-code status Test1:reply-success)
       (list r2 is)
       )
     )
    )
    
   ((O1-U-to-U Test1:P-impl . status-and-o-and-u)
    (ilu-bind-arguments
     (status-and-o-and-u status o u)
     (format #t "Test1.P.O1-U-to-U~&")
     (set-car! u 3)
     (set-cdr! u o)
     (set-return-code status Test1:reply-success)
     (list u #t)
     )
    )

   ((BS-to-I Test1:P-impl . status-and-b)
    (ilu-bind-arguments
     (status-and-b status b)
     (set-return-code status Test1:reply-success)
     (length b)
     )
    )

   ((m2 Test1:P-impl . status-and-j)
    (ilu-bind-arguments
     (status-and-j status j)
     (set-return-code status Test1:reply-success)
     (list j (* j j))
     )
    )

   )
  )

;; ///////////////////// Test1_T_O4_impl methods /////////////////////

(define (getb l i)
  (if (>= i (length l)) 0 (list-ref l i)))

(define (make-Test1:O4-impl)
  (ilu-object-with-ancestors
   (
    (Test1:O4 (make-Test1:O4))
   )

   ((RS-R-to-R-IS Test1:O4-impl . status-and-rs-and-r2)
    (ilu-bind-arguments
     (status-and-rs-and-r2 status rs r2)
     (let ((is (list)))
       (format #t "Test1.O4.RS-R-to-R-IS~&")
       (set! r2 (make-Test1:R))
       (set-i r2 25179)
       (set-css r2 (Test1:CSS (list "xxx")))
       (set-a r2 (make-vector 3))
       (vector-set! (get-a r2) 0 "from")
       (vector-set! (get-a r2) 1 "P")
       (vector-set! (get-a r2) 2 "string")
       (set-return-code status Test1:reply-success)
       (list r2 is)
       )
     )
    )

   ((O1-U-to-U Test1:O4-impl . status-and-o-and-r2)
    (ilu-bind-arguments
     (status-and-o-and-r2 status o r2)
     (let ((u (#f . #f)))
       (format #t "Test1.O4.O1-U-to-U~&")
       (set-car! u 3)
       (set-cdr! u o)
       (set-return-code status Test1:reply-success)
       (list u #t)
       )
     )
    )

   ((BS-to-I Test1:O4-impl . status-and-b)
    (ilu-bind-arguments
     (status-and-b status b)
     (format
      #t "Test1.O4.BS_to_I (~d: ~d ~d ~d ~d ~d ~d ~d ~d ~d ~d ~d ...) => ~d~&"
      (length b) (getb b 0) (getb b 1) (getb b 2) (getb b 3) (getb b 4)
      (getb b 5) (getb b 6) (getb b 7) (getb b 8) (getb b 9) (getb b 10)
      (length b))
     (set-return-code status Test1:reply-success)
     (length b)
     )
    )

   ((R-to-R Test1:O4-impl . status-and-r)
    (ilu-bind-arguments
     (status-and-r status r)
     (format #t "Test1.O4.R_to_R (~f) => 1020304.05060708~&" r)
     (set-return-code status Test1:reply-success)
     1020304.05060708)
    )
   )
  )


;; ///////////////////// Test3_T_O_impl methods /////////////////////

(define (make-Test3:O-impl)
  (ilu-object-with-ancestors
   (
    (Test3:O (make-Test3:O))
   ) 

   ((RS-R-to-R-IS Test3:O-impl . status-and-rs-and-r2)
    (ilu-bind-arguments
     (status-and-rs-and-r2 status rs r2)
     (let ((is (list)))
       (format #t "Test3.O.RS-R-to-R-IS~&")
       (set! r2 (make-Test1:R))
       (set-i r2 3)
       (set-css r2 (Test1:CSS (list "xxx")))
       (set-a r2 (make-vector 3))
       (vector-set! (get-a r2) 0 "just")
       (vector-set! (get-a r2) 1 "a")
       (vector-set! (get-a r2) 2 "string")
       (set-return-code status Test1:reply-success)
       (list r2 is)
       )
     )
    )

   ((O1-U-to-U Test3:O-impl . status-and-o-and-u)
    (ilu-bind-arguments
     (status-and-o-and-u status o u)
     (format #t "Test3.O.O1-U-to-U(0x~x, {~d})~&" (object-address o) (car u))
     (set-car! u 3)
     (set-cdr! u o)
     (set-return-code status Test1:reply-success)
     (list u #t)
     )
    )

   ((BS-to-I Test3:O-impl . status-and-b)
    (ilu-bind-arguments
     (status-and-b status b)
     (set-return-code status Test1:reply-success)
     (* (length b) (length b))
     )
    )

   ((SR-to-I Test3:O-impl . status-and-i)
    (ilu-bind-arguments
     (status-and-i status i)
     (set-return-code status Test1:reply-success)
     (format #t "Test3.O.SR-to-I(~f)~&" r)
     (truncate i)
     )
    )

   ((I-to-Test1U Test3:O-impl . status-and-i)
    (ilu-bind-arguments
     (status-and-i status i)
     (let ((u (cons 5 #t)))
       (format #t "Test3.O.I-to-Test1U(~d)~&" i)
       (set-return-code status Test3:reply-success)
       u)
     )
    )
   )
  )

(define (main argv)
  (let ((s #f) (uc #f) (uc2 #f))

    (ilu:init)

    (set! s (ilu-server:create "Test1-Server" #f))
    (ilu-server:add-port s #f #f #t)
    (ilu:set-default-server s)

    (set! uc (make-Test1:O1-impl "Test1_Initial_Object" s))
    (if (not (publish uc))
	(error "*** Error, could not publish object"))

    ;; test the publish and lookup a bit
    (set! uc2 (ilu-object:lookup "Test1-Server"
				 "Test1_Initial_Object"
				 Test1:TheO1:class-record))

    (if (not (equal? uc2 uc))
	(error "*** Error, lookup returns wrong object"))

    (if (and uc2 (not (publish uc2)))
	(error "*** Error, second publish failed"))

    (if uc
	(begin
	  (format #t "exported ~a~&" (string-binding-handle uc))
	  (ilu:run-main-loop (ilu:make-main-loop-id))
	  )
	(error "could not create object"))
    ))

(main (command-line))
