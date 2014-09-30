;; -*- Mode: Scheme; -*-
;;
;; Copyright (c) 1997 Siemens Corporate Research, Inc.
;; All rights reserved.
;;
;; $Author: janssen $
;; $Date: 1999/08/03 01:56:01 $
;; $Source: /var/tmp/tape/RCS/ilu.scm,v $
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

(require "ilu-object")
(require "ilu-server")

;; some standard ilu errors
(define ilu:success #t)
(define ilu:protocol-error "ilu.ProtocolError")

;; ilu protocol exception types
(define ilu:protocol-exception-success                 0)
(define ilu:protocol-exception-no-such-class-at-server 1)
(define ilu:protocol-exception-class-version-mismatch  2)
(define ilu:protocol-exception-no-such-method-on-class 3)
(define ilu:protocol-exception-garbage-arguments       4)
(define ilu:protocol-exception-unknown                 5)
(define ilu:protocol-exception-lost-connection         6)
(define ilu:protocol-exception-request-rejected        7)
(define ilu:protocol-exception-request-timeout         8)
(define ilu:protocol-exception-not                  1000)

;; internal types for ipc
(define ilu:rcv-req-stat-noop    0)
(define ilu:rcv-req-stat-quit    1)
(define ilu:rcv-req-stat-request 2)

;; should generics be in ilu package? or in any package?
(ilu-define-operation (run obj . stop))
(ilu-define-operation (exit obj . stop))
(ilu-define-operation (register-input-handler obj . fd-and-handler))
(ilu-define-operation (register-output-handler obj . fd-and-handler))
(ilu-define-operation (unregister-input-handler obj . fd))
(ilu-define-operation (unregister-output-handler obj . fd))
(ilu-define-operation (create-alarm obj))
(ilu-define-operation (set-alarm obj . alarm-and-time-and-proc))
(ilu-define-operation (clear-alarm obj . alarm))

;; main loop class
(define (make-ilu:main-loop)
  (ilu-object
   ;; run main loop
   ((run this . stop)
    (error "ilu:main-loop is an abstract class"))

   ;; exit main loop
   ((exit this . stop)
    (error "ilu:main-loop is an abstract class"))

   ;; register input handle for a file descriptor
   ((register-input-handler this . fd-and-handler)
    (error "ilu:main-loop is an abstract class"))

   ;; register output handle for a file descriptor
   ((register-output-handler this . fd-and-handler)
    (error "ilu:main-loop is an abstract class"))

   ;; unregister input handler for file descriptor
   ((unregister-input-handler this . fd)
    (error "ilu:main-loop is an abstract class"))

   ;; unregister output handler for file descriptor
   ((register-output-handler this . fd)
    (error "ilu:main-loop is an abstract class"))

   ;; create an alarm object
   ((create-alarm this)
    (error "ilu:main-loop is an abstract class"))

   ;; set an alarm object with a time and a handler
   ;; time is value in seconds represented by a double
   ;; proc can be any proc, lambda, closure, etc.
   ((set-alarm this . alarm-and-time-and-proc)
    (error "ilu:main-loop is an abstract class"))

   ;; reset an alarm to do nothing
   ((clear-alarm this . alarm)
    (error "ilu:main-loop is an abstract class"))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; default ok return code
(define ilu:reply-success #f)

(ilu-define-operation (get-return-code obj))
(ilu-define-operation (set-return-code obj . code))

(ilu-define-operation (get-caller-passport obj))
(ilu-define-operation (set-caller-passport obj . passp))

(ilu-define-operation (get-status-value obj))
(ilu-define-operation (set-status-value obj . value))

(define (make-ilu:status args)
  (let ((return-code (if args (list-ref args 0) #f))
	(caller-passport (if args (list-ref args 1) #f))
	(value (if args (list-ref args 2) #f)))
    (ilu-object
     ((get-return-code this) return-code)
     ((set-return-code this . rc) (set! return-code (car rc)))

     ((get-caller-passport this) caller-passport)
     ((set-caller-passport this . cp) (set! caller-passport (car cp)))

     ((get-status-value this) value)
     ((set-status-value this . v) (set! value (car v)))
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this seems to be ignored in ilu library code so for now...
(define ilu:size-of-discriminator 4)

;; map recursively to nested lists or vectors
;; sequences are lists
;; arrays are vectors
(define (ilu:map-rec proc seq)
  (if (and seq (not (null? seq)))
      (if (list? seq)
	  (if (list? (car seq))
	      (ilu:map-rec proc (car seq))
	      (map proc seq))
	  (if (and (> (length seq) 0) (vector? (vector-ref seq 0)))
	      (ilu:map-rec proc (vector-ref seq 0))
	      (map proc (vector->list seq)))
	  )
      #f)
  )

;; sequences are nested lists
(define (ilu:input-sequence-rec proc diml)
  (if (and diml (not (null? diml)))
      (do ((i 0 (+ i 1))
	   (l (list) (append l (list (ilu:input-sequence-rec proc (cdr diml))))))
	  ((= i (car diml)) l))
      (apply proc (list))
      )
  )

;; arrays are nested vectors
(define (ilu:input-array-rec proc diml)
  (if (and diml (not (null? diml)))
      (do ((i 0 (+ i 1))
	   (v (make-vector (car diml)) v))
	  ((= i (car diml)) v)
	(vector-set! v i (ilu:input-array-rec proc (cdr diml))))
      (apply proc (list))
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GC callback object
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (not (defined? 'ilu:did-init))
    (define ilu:did-init #f))

(define ilu:gc-callback-object #f)
(define ilu-gc-callback:class-record #f)

(define (make-ilu-gc-callback arg)
  (let* ((our-server arg)
	 (obj (ilu-object-with-ancestors
	       ((parent (make-ilu:object)))
	       ((get-server this) our-server)
;;	       ((set-server this . server) (set! our-server (car server)))
	       )))
    (set-instance-class-record obj (ilu:get-gc-callback-class))
    obj))

(define (ilu-gc-callback:create obj)
  (let ((nobj (make-ilu-gc-callback #f)))
    (set-rpc-object nobj obj)
    (ilu:set-language-specific-object obj nobj)
    nobj))

(define (ilu:init)
  (if (and (not ilu:did-init) (not (ilu:is-gc-client-set)))
      (let ((server #f))
	(set! ilu:did-init #t)
	(set! ilu-gc-callback:class-record (ilu:get-gc-callback-class))
	(ilu-object:register-surrogate-creator
	 ilu-gc-callback:class-record ilu-gc-callback:create)
	(set! server (ilu-server:create #f #f))
	(ilu-server:add-port server #f #f #t)
	(set! ilu:gc-callback-object (make-ilu-gc-callback server))
	(ilu-object:register-as-gc-callback ilu:gc-callback-object)
	)
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ILU stand-alone-repl (input loop for interactive use)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ilu:init-stand-alone-repl argc argv)
  (ilu:register-input-handler *stdin* ilu:synthetic-repl)
  )

;; clean this up (use SLIB repl:repl for this)
(define (ilu:synthetic-repl)
  (let ((repl (lambda ()
		(pretty-print (eval (read)))
		(format #t "~!ilu-guile> ")
		)))
    (with-dynamic-root
     (lambda () (with-input-from-port *stdin* repl))
     (lambda (errcode)
       (with-input-from-port *stdin*
	 (lambda ()
	   (cond
	    ((= errcode repl-quit) #t)
	    (#t (repl)))))))))
