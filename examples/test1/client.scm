#!/usr/bin/env iluguile
!#
;; $Id: client.scm,v 1.5 1999/08/03 01:52:24 janssen Exp $
;;
;; This code donated by Siemens Corporate Research, Inc.
;;
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

(require "Test1")
(require "Test2")
(require "Test3")

(define (printU prefix varName u)
  (begin
    (format #t "~a~a.discriminator = ~d" prefix varName (car u))
    (cond ((= 3 (car u))
	   (format #t " ~a.value.O1 = 0x~x~&" varName (object-address (cdr u))))
	  ((= 5 (car u))
	   (format #t " ~a.value.boolean = ~s~&" varName (cdr u)))
	  (#t
	   (format #t " (unexpected value)~&"))
	  )
    ))

(define (main argv)
  (ilu:init)
  (let ((s1 (make-Test1:status))
	(handle #f) (o2 #f) (o3 #f) (f #f) (u #f)
	(css #f) (scs #f) (ro #f) (r #f))

    (set! handle (ilu-object:lookup "Test1-Server"
				    "Test1_Initial_Object"
				    Test1:TheO1:class-record))

    (if (not handle)
	(error "Unable to import server!"))

    (set! u (cons 5 #t))
    (set! scs (vector "hello world" "hello mars" ""))

    (set! css (Test1:CSS (vector->list scs)))

    (set! u (U-CSS-to-U handle s1 u css))

    (if (get-return-code s1)
	(error "U-CSS-to-U => exn " (get-return-code s1)))

    (printU "" "u" u)

    (set! ro (f-CSS-to-RO handle s1 css))
    (if (get-return-code s1)
	(error "f-CSS-to-RO => exn " (get-return-code s1)))
    (format #t "ro->i=~s~&" (get-i ro))
    (format #t "ro->css=~s~&" (get-css ro))
    (format #t "ro->a=~s~&" (get-a ro))

    (set! f (R-ScS-to-F handle s1 ro (vector-ref scs 0)))
    (if (get-return-code s1)
	(error "R-ScS-to-F => exn " (get-return-code s1)))
    (format #t "f=~f~&" f);

    (a-RO handle s1 ro)

    (if (get-return-code s1)
	(error "a-RO => exn " (get-return-code s1)))

    (set! o2 (get-O2 handle s1))

    (if (equal? (get-return-code s1) Test1:reply-success)
	(begin
	  (let ((a (make-vector 8 #\space))
		(ap #f)
		(a1 #f)
		(i 0)
		(css2 #f))

	    (format #t "got O2, sbh = ~a~&" (string-binding-handle o2))

	    (set! css2 (OO-A0-to-CSS o2 s1 handle a))

	    (if (not (eq? (get-return-code s1) Test2:reply-success))
		(format
		 #t "exception on Test1:O2:OO-A0-to-CSS, exception is ~a~&"
		 (get-return-code s1))
		)

	    (set! r (make-Test1:TheR))
	    (set-css r (Test1:CSS (list "xxx")))
	    (set-i r 12)
	    (set-a r (make-vector 3))
	    (vector-set! (get-a r) 0 "this is")
	    (vector-set! (get-a r) 1 "data")
	    (vector-set! (get-a r) 2 "initialization")
	    (set! a1 (vector "but this" "is" "fun"));
	    (set! ap (R-I-A1-to-I-A0 o2 s1 r i a1))
	    (set! i (car ap))
	    (set! ap (cadr ap))
	    ))
	(format #t "couldn't get an instance of O2.  Exception is ~a~&"
	       (get-return-code s1))
	)

    (set! o3 (get-O3 handle s1 #f))

    (if (eq? (get-return-code s1) Test1:reply-success)
	(begin
	  (let ((rs (Test1:TheRS (list))) (i2 #f))

	    (format #t "got O3, sbh = ~a, type = ~a~&"
		    (string-binding-handle o3) 
		    (class-name o3))

	    (if (not (equal? (get-instance-class-record o3)
			     (ilu:find-class-from-type-name "Test1.O3")))
		(format #t "instance of class ~a received!~&" (class-name o3))
		(begin
		  (set! i2 (RS-R-to-R-IS o3 s1 rs r))
		  (set! rs (car i2))
		  (set! i2 (cadr i2))
		  (set! u (car (O1-U-to-U o3 s1 handle u)))
		  (printU "" "u" u)
		  )
		)
	    ))
	(format #t "couldn't get an instance of O3.  Exception is ~a~&"
		(get-return-code s1))
	)

    (set! o3 (get-O3 handle s1 #t))

    (if (equal? (get-return-code s1) Test1:reply-success)
	(let ((rs (Test1:TheRS (list)))
	      (i2 #f))

	  (format #t "got O3, sbh = ~a, type = ~a~&"
		  (string-binding-handle o3)
		  (class-name o3))
	  
	  (set! i2 (car (RS-R-to-R-IS o3 s1 rs r)))
	  (set! u (car (O1-U-to-U o3 s1 handle u)))

	  (printU "" "u" u)

	  (if (equal? (get-instance-class-record o3)
		      (ilu:find-class-from-type-name "Test3.O"))
	      (let ((o #f) (s3 (make-Test3:status)) (u2 #f))
		(set! o o3)
		(set! u2 (I-to-Test1U o s3 397))

		(if (not (equal? (get-return-code s3)
			      Test3:reply-success))
		    (format
		     #t "exception on Test3_O::I_to_Test1U, exception is ~a~&"
		     (get-return-code s3))
		    (printU "Test3_O::I_to_Test1U:  " "u2" u2))
		)))
	  (format #t "couldn't get an instance of O3.  Exception is ~a~&"
		 (get-return-code s1)))

    ;; this next call should return an instance of Test1.O4 
    (set! o3 (get-O3 handle s1 #f))

    (if (equal? (get-return-code s1) Test1:reply-success)
	(begin
	  (format #t "got O3, sbh = ~a, type = ~a~&"
		  (string-binding-handle o3)
		  (class-name o3))

	  (if (equal? (get-instance-class-record o3)
		      (ilu:find-class-from-type-name "Test1.O4"))
	      (let ((o4 #f) (r1 0.0) (r2 0.0))
		(set! o4 o3)
		(set! r1 12345.6789)
		(set! r2 (R-to-R o4 s1 r1))
		(if (not (equal? (get-return-code s1)
				 Test1:reply-success))
		    (format #t "exception on R_to_R, exception is ~a~&"
			    (get-return-code s1))
		    (format #t "doubles:  r1 is ~f, r2 is ~f~&" r1 r2)))
	      )
	  )
	(format #t "couldn't get an instance of O3.  Exception is ~a~&"
		(get-return-code s1))
	)
    ))

(main (command-line))
