;;  -*- Mode: Scheme; -*-
;;
;; Copyright (c) 1993 Siemens Corporate Research, Inc.
;; All rights reserved.
;;
;; $Author: janssen $
;; $Date: 1999/08/03 01:55:57 $
;; $Source: /var/tmp/tape/RCS/ilu-object.scm,v $
;; $Revision: 1.4 $
;;
;; BeginILUCopyright
;; 
;; Copyright (c) 1991-1999 Xerox Corporation.  All Rights Reserved.
;; 
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
;; 
;; EndILUCopyright

(require "ilu-yasos")

(ilu-define-operation (string-binding-handle obj))
(ilu-define-operation (publish obj))
(ilu-define-operation (withdraw obj))
(ilu-define-operation (class-name obj))
(ilu-define-operation (class-id obj))
(ilu-define-operation (get-server obj))
(ilu-define-operation (get-instance-handle obj))
(ilu-define-operation (set-instance-handle obj . ih))
(ilu-define-operation (get-rpc-object obj))
(ilu-define-operation (set-rpc-object obj . robj))
(ilu-define-operation (get-kernel-server obj))
(ilu-define-operation (set-kernel-server obj . server))
(ilu-define-operation (ensure-kernel-object obj))
(ilu-define-operation (ensure-kernel-server obj))
(ilu-define-operation (get-instance-class-record obj))
(ilu-define-operation (set-instance-class-record obj . cr))
(ilu-define-operation (destroy obj))

(define ilu-object:id-counter 0)

(define (make-ilu:object)
  (let ((instance-class-record #f)
	(rpc-object #f)
	(rpc-server #f)
	(publish-proof #f)
	(instance-handle #f))
    (ilu-object
     ((string-binding-handle this)
      (ilu:sbh-of-object (get-rpc-object this)))

     ((publish this)
      (let* ((obj (get-rpc-object this))
	     (proof (ilu:publish-object obj)))
	(set! publish-proof proof)
	proof)
      )

     ((withdraw this)
      (let* ((obj (get-rpc-object this))
	     (status (ilu:withdraw-object obj publish-proof)))
	(set! publish-proof #f)
	status))

     ((class-name this)
      (ilu-class:name instance-class-record))

     ((class-id this)
      (ilu-class:unique-id instance-class-record))

     ((get-instance-handle this)
      instance-handle)

     ((set-instance-handle this . ih)
      (set! instance-handle (car ih)))

     ((get-rpc-object this)
      (if (not (ensure-kernel-server this))
	  #f
	  (begin
	    (ilu:enter-server rpc-server instance-class-record)
	    (ensure-kernel-object this)
	    (if (not rpc-object)
		(begin
		  (ilu:exit-server rpc-server intance-class-record)
		  #f
		  )
		rpc-object)
	    )))

     ((set-rpc-object this . obj)
      (set! rpc-server (ilu:server-of-object (car obj)))
      (set! rpc-object (car obj)))

     ((get-server this)
      (ilu:get-default-server))

     ((get-kernel-server this)
      rpc-server)

     ((set-kernel-server this . server)
      (set! rpc-server (car server)))

     ((ensure-kernel-object this)
      (cond ((not (ensure-kernel-server this))
	     #f)
	    ((not rpc-object)
	     (let ((id (get-instance-handle this)))
	       (if (not id)
		   (begin
		     (set! ilu-object:id-counter (+ ilu-object:id-counter 1))
		     (set! id (number->string ilu-object:id-counter))
		     (set! instance-handle id)))
	       (set! rpc-object
		     (ilu:create-true-kernel-object id
						    rpc-server
						    instance-class-record
						    this))
	       )
	     rpc-object)))
     
     ((ensure-kernel-server this)
      (if (not rpc-server)
	  (set! rpc-server (get-server this))
	  rpc-server))

     ((get-instance-class-record this)
      instance-class-record)

     ((set-instance-class-record this . rec)
      (set! instance-class-record (car rec)))

     ;; would be nice to hook this into GC somehow
     ((destroy this)
      (if publish-proof
	  (withdraw this)
	  (if rpc-server
	      (begin
		(ilu:enter-server rpc-server instance-class-record)
		(if rpc-object
		    (ilu:set-language-specific-object rpc-object #f))
		(set! rpc-object #f)
		(ilu:exit-server rpc-server instance-class-record)))))

     )))

;; helper fns for C code
(define (ilu-object:rpc-server obj)
  (get-kernel-server obj))

(define (ilu-object:instance-class-record obj)
  (get-instance-class-record obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ilu:CORBA-Object implementation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (not (defined? 'ilu:CORBA-Object:class-record))
    (define ilu:CORBA-Object:class-record #f)
    )

(define (make-ilu:CORBA-Object)
  (let ((obj (ilu-object-with-ancestors
	      ((ilu-parent-object (make-ilu:object)))
	      )
	     )
	)
    (set-instance-class-record obj ilu:CORBA-Object:class-record)
    obj))

(define (ilu:CORBA-Object:create obj)
  (let ((nobj (make-ilu:CORBA-Object)))
    (set-rpc-object nobj obj)
    (ilu:set-language-specific-object obj nobj)
    nobj))

(if (not ilu:CORBA-Object:class-record)
    (begin
      (ilu:enter-ot-mu)
      (set! ilu:CORBA-Object:class-record
	    (ilu:define-object-type
	     "ilu.CORBA-Object"  ;; ILU name
	     ""                  ;; Brand
	     (ilu:get-ilu-corba-object-type-id)  ;; Id
	     #f                  ;; singleton
	     #t                  ;; optional
	     #f                  ;; collectable
	     #f                  ;; authentication
	     0                   ;; number of methods
	     0                   ;; number of superclasses
	     #f))                ;; superclass ids
      (ilu-object:register-surrogate-creator ilu:CORBA-Object:class-record ilu:CORBA-Object:create)
      (ilu:object-type-defined ilu:CORBA-Object:class-record)
      (ilu:exit-ot-mu)
      )
    )

