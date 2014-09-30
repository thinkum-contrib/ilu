;;;-*- Package: USER; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-
;;; Last edited by Mike Spreitzer October 9, 1998 10:40 am PDT
#|
 $Id: client.lisp,v 1.4 1999/08/03 01:58:59 janssen Exp $
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

(defun test-bank (&key ior (bname "sample-bank") (account-id "sample-id") (initial-deposit 333.56))
  (let ((factory (if ior
		     (ilu:sbh->instance 'bank-app:Factory ior)
		   (ilu:lookup "bank3.examples.ORBPlus.hp.com" "Factory" 'bank-app:Factory))))
    (if (not factory)
	(error "Couldn't bind server.~%")
      (progn
	(format t "Requesting factory create a bank with name ~s...~%" bname)
	(let ((bank (bank-app:factory.create-bank factory bname)))
	  (unless bank
	    (error "Received NIL bank"))
	  (format t "Retrieving the name of bank ~s...~%" bank)
	  (let ((name (bank-app:bank.name bank)))
	    (unless (string-equal name bname)
	      (error (format nil "Received name ~s doesn't match specified name ~s!" name bname)))
	    (format t "Requesting bank create an account (~a, ~a)...~%" account-id initial-deposit)
	    (let ((account (bank-app:bank.create-account bank account-id initial-deposit)))
	      (unless account
		(error "Received NIL account"))
	      (format t "Checking the account ID of ~S...~%" account)
	      (let ((id (bank-app:account.ID account)))
		(unless (string-equal id account-id)
		  (error (format nil "Received ID ~s and specified ID ~s don't match!" id account-id)))
		(format t "Retrieving the balance...")
		(let ((balance (bank-app:account.balance account)))
		  (unless (< (abs (- balance initial-deposit)) 0.01)
		    (error (format nil "Received balance ~a different from specified balance ~a!" balance initial-deposit)))
		  (format t "Client completed successfully!~%")
		  (values))))))))))
