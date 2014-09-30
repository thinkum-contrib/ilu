;; -*- Mode: Scheme; -*-
;;
;; Copyright (c) 1997 Siemens Corporate Research, Inc.
;; All rights reserved.
;;
;; $Author: janssen $
;; $Date: 1999/08/03 01:56:01 $
;; $Source: /var/tmp/tape/RCS/ilu-yasos.scm,v $
;; $Revision: 1.4 $
;;
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

; "ILU-YASOS.scm" ILU Object System
; Modified from YASOS.scm by bnell@scr.siemens.com
;
; "YASOS.scm" Yet Another Scheme Object System
; COPYRIGHT (c) Kenneth Dickey 1992
;
;		This software may be used for any purpose whatever
;               without warrantee of any kind.
; DATE		1992 March 1
; LAST UPDATED	1992 September 1 -- misc optimizations
;		1992 May 22  -- added SET and SETTER

;; REQUIRES     R^4RS Syntax System

;; NOTES: A simple object system for Scheme based on the paper by
;; Norman Adams and Jonathan Rees: "Object Oriented Programming in
;; Scheme", Proceedings of the 1988 ACM Conference on LISP and Functional
;; Programming, July 1988 [ACM #552880].
;
;; Setters use space for speed {extra conses for O(1) lookup}.


;;
;; INTERFACE:
;;
;; (ilu-define-operation (opname self . args) default-body)
;;
;; (ilu-define-predicate opname)
;;
;; (ilu-object ((name self . args) body) ... )
;;
;; (ilu-object-with-ancestors ( (ancestor1 init1) ...) operation ...)
;;
;; in an operation {a.k.a. send-to-super}
;;   (ilu-operate-as component operation self arg ...)
;;

;; (ilu-set var new-vale) or (ilu-set (access-proc index ...) new-value)
;;
;; (ilu-setter access-proc) -> setter-proc
;; (ilu-define-access-operation getter-name) -> operation
;; (ilu-add-setter getter setter) ;; setter is a Scheme proc
;; (ilu-remove-setter-for getter)
;;

(require 'format)
(require 'macro-by-example)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; IMPLEMENTATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INSTANCES

; (ilu-define-predicate instance?)
; (define (make-instance dispatcher)
;    (ilu-object
;	((ilu-instance?  self) #t)
;       ((ilu-instance-dispatcher self) dispatcher)
; )  )

(define ilu-yasos:make-instance 'bogus)  ;; defined below
(define ilu-yasos:instance?     'bogus)

;; alias so compiler can inline for speed
(define-syntax ilu-yasos:instance-dispatcher
   (syntax-rules () ((ilu-yasos:instance-dispatcher inst) (cdr inst)))
)

(let ((instance-tag "ilu-instance"));; Make a unique tag within a local scope.
				    ;; No other data object is EQ? to this tag.
  (set! ilu-yasos:make-instance
     (lambda (dispatcher) (cons instance-tag dispatcher)))

  (set! ilu-yasos:instance?
     (lambda (obj) (and (pair? obj) (eq? (car obj) instance-tag))))
)

;; ilu-define-operation

(define-syntax ilu-define-operation
  (syntax-rules ()
    ((ilu-define-operation (<name> <inst> . <args>) <exp1> <exp2> ...)
     ;;=>
     (define <name>
       (letrec ((former-inst #f) ;; simple caching -- for loops
		(former-method #f)
		(self
		 (lambda (<inst> . <args>)
		   (cond
		    ((eq? <inst> former-inst) ; check cache
		     (apply former-method <inst> <args>)
		     )
		    ((and (ilu-yasos:instance? <inst>)
			  ((ilu-yasos:instance-dispatcher <inst>) self))
		     => (lambda (method)
			  (set! former-inst <inst>)
			  (set! former-method method)
			  (apply method <inst> <args>)
			  )
		     )
		    (else <exp1> <exp2> ...)
		    )
		   )
		 )
		)
	 self)
       )
     )
  ((ilu-define-operation (<name> <inst> . <args>) ) ;; no body
   ;;=>
   (ilu-define-operation (<name> <inst> . <args>)
     (slib:error "Operation not handled"
		 '<name>
		 (format #f
			 (if (ilu-yasos:instance? <inst>)
			     "#<ilu-instance>" "~s")
			 <inst>)))
   )
  )
)




;; ilu-define-predicate

(define-syntax ilu-define-predicate
  (syntax-rules ()
    ((ilu-define-predicate <name>)
     ;;=>
     (ilu-define-operation (<name> obj) #f)
    )
  )
)


;; ilu-object

(define-syntax ilu-object
  (syntax-rules ()
    ((ilu-object ((<name> <self> . <args>) <exp1> <exp2> ...) ...)
    ;;=>
     (let ((table
	    (list (cons <name> (lambda (<self> . <args>) <exp1> <exp2> ...))
		  ...
		  )
	    )
	   )
      (ilu-yasos:make-instance
	(lambda (op)
	  (cond
	    ((assq op table) => cdr)
	    (else #f)
	    )
	  )
	)
      )
     )
    )
)


;; ilu-object with MULTIPLE INHERITANCE  {First Found Rule}

(define-syntax ilu-object-with-ancestors
  (syntax-rules ()
    ((ilu-object-with-ancestors ((<ancestor1> <init1>) ...) <operation> ...)
    ;;=>
     (let ((<ancestor1> <init1>) ...)
      (let ((child (ilu-object <operation> ...)))
       (ilu-yasos:make-instance
	 (lambda (op)
	    (or ((ilu-yasos:instance-dispatcher child) op)
		((ilu-yasos:instance-dispatcher <ancestor1>) op) ...
		)
	    )
	 )
       )
      )
     )
    )
)


;; ilu-operate-as {a.k.a. send-to-super}

; used in operations/methods

(define-syntax ilu-operate-as
  (syntax-rules ()
   ((ilu-operate-as <component> <op> <composit> . <args>)
   ;;=>
    (apply ((ilu-yasos:instance-dispatcher <component>) <op>)
	   <composit> <args>)
    )
   )
)


;; ilu-bind-arguments

(define-syntax ilu-bind-arguments
  (syntax-rules ()
    ((ilu-bind-arguments (<alist> <arg1> <arg2> ...) <exp1> <exp2> ...)
     ;;=>
     (let ((<arg1> (let ((v (car <alist>))) (set! <alist> (cdr <alist>)) v))
	   (<arg2> (let ((v (car <alist>))) (set! <alist> (cdr <alist>)) v))
	   ...)
;;       (if (> (length <alist>) 0)
;;	   (slib:error "Too many arguments passed to method")
;;	   )
       <exp1>
       <exp2>
       ...)
     )
    )
  )


;; ilu-set & ilu-setter

(define-syntax ilu-set
  (syntax-rules ()
    ((ilu-set (<access> <index> ...) <newval>)
     ((ilu-yasos:setter <access>) <index> ... <newval>)
    )
    ((ilu-set <var> <newval>)
     (set! <var> <newval>)
    )
  )
)


(define ilu-yasos:add-setter	'bogus)
(define ilu-yasos:remove-setter-for 'bogus)

(define ilu-yasos:setter
  (let ((known-setters (list (cons car set-car!)
			     (cons cdr set-cdr!)
			     (cons vector-ref vector-set!)
			     (cons string-ref string-set!))
		       )
	(added-setters '())
       )

    (set! ilu-yasos:add-setter
      (lambda (getter setter)
	(set! added-setters (cons (cons getter setter) added-setters)))
    )
    (set! ilu-yasos:remove-setter-for
      (lambda (getter)
	(cond
	  ((null? added-setters)
	   (slib:error "remove-setter-for: Unknown getter" getter)
	  )
	  ((eq? getter (caar added-setters))
	   (set! added-setters (cdr added-setters))
	  )
	  (else
	    (let loop ((x added-setters) (y (cdr added-setters)))
	      (cond
		((null? y) (slib:error "remove-setter-for: Unknown getter"
				       getter))
		((eq? getter (caar y)) (set-cdr! x (cdr y)))
		(else (loop (cdr x) (cdr y)))
	  ) ) )
     ) ) )

    (letrec ( (self
		 (lambda (proc-or-operation)
		   (cond ((assq proc-or-operation known-setters) => cdr)
			 ((assq proc-or-operation added-setters) => cdr)
			 (else (proc-or-operation self))) )
	    ) )
      self)
) )



(define (ilu-yasos:make-access-operation <name>)
  (letrec ( (setter-dispatch
	       (lambda (inst . args)
		   (cond
		     ((and (ilu-yasos:instance? inst)
			   ((ilu-yasos:instance-dispatcher inst) setter-dispatch))
		       => (lambda (method) (apply method inst args))
		     )
		     (else #f)))
	    )
	    (self
	       (lambda (inst . args)
		  (cond
		     ((eq? inst ilu-yasos:setter) setter-dispatch) ; for (setter self)
		     ((and (ilu-yasos:instance? inst)
			   ((ilu-yasos:instance-dispatcher inst) self))
		      => (lambda (method) (apply method inst args))
		     )
		     (else (slib:error "Operation not handled" <name> inst))
		)  )
	    )
	  )

	  self
) )


(define-syntax ilu-define-access-operation
  (syntax-rules ()
    ((ilu-define-access-operation <name>)
     ;=>
     (define <name> (ilu-yasos:make-access-operation '<name>))
) ) )


;;---------------------
;; general operations
;;---------------------

(ilu-define-operation (ilu-yasos:print obj . port)
  (format (car port)
	  ;; if an instance does not have a PRINT operation..
	  (if (ilu-yasos:instance? obj) "#<ilu-instance>" "~s")
	  obj
) )

(ilu-define-operation (ilu-yasos:size obj)
  ;; default behavior
  (cond
    ((vector? obj) (vector-length obj))
    ((list?   obj) (length obj))
    ((pair?   obj) 2)
    ((string? obj) (string-length obj))
    ((char?   obj) 1)
    (else
      (slib:error "Operation not supported: size" obj))
) )

