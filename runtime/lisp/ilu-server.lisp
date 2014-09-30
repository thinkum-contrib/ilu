#|
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
#|

$Id: ilu-server.lisp,v 1.39 1999/08/03 01:53:30 janssen Exp $
|#

(in-package :ilu)		; in this package

(eval-when (compile load eval)
  (export '(signal-exception
	    kernel-server
	    server-c-server
	    server-unix-port
	    server-id
	    define-server-class
	    )))
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Actual code for dealing with server objects
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *default-server* nil)

(defclass kernel-server ()
  ((unix-port :reader server-unix-port :initarg :port :initform 0)
   (c-server :initform nil :reader server-c-server)
   (object-table :initarg :object-table
		 :accessor server-object-table :initform nil)
   (id :initarg :id :initform nil :reader server-id)
   (protocol :initarg :protocol :initform nil :reader protocol)
   (transport :initarg :transport :initform nil :reader transport))
  )

(defclass stateless-kernel-server (kernel-server) ())

(defmethod has-state ((self kernel-server))
  t)

(defmethod has-state ((self stateless-kernel-server))
  nil)

(defmethod add-port ((self kernel-server) protocol transport
		     &key (private nil) (default nil) (passport nil) &allow-other-keys)
  (let ((server (server-c-server self)))
    (if (c-null-pointer-p server)
	(error "Can't open port on nil server~%"))
    (let ((ti (make-transport-info transport))
	  (pinfo (or protocol (get-default-protocol-info))))      
      ;; allocated transport info, ensure it is freed
      (unwind-protect
	  (let* ((pp (or passport (c-null-pointer))) ; should be passport!
		 (s-port (create-port server pinfo ti pp (not private))))
	    (if (c-null-pointer-p s-port)
		(error
		 "Can't open port for server ~s.~%" (id-of-kernel-server server))
	      (progn
		(when default
		  (set-server-default-port server s-port))
		(setup-new-connection-handler #'handle-new-connection server s-port)
		s-port)))
	(ilulisp_free-c-struct ti)))))

(defmethod add-cinfo ((self kernel-server) pinfo tinfo)
  (let ((ti (make-transport-info tinfo)))
    (ilu_add_cinfo_to_server (server-c-server self) pinfo ti)))

(defmethod native-cinfo ((self kernel-server) &optional (private-p nil))
  (multiple-value-bind (status pinfo ti)
      (ilu_get_cinfo_of_server (server-c-server self) (not private-p))
    (if (not status)
	(values nil nil)
      (let ((n (ilulisp_tinfo-length ti))
	    (tinfo (list)))
	(values pinfo
		(do ((i 0 (+ i 1)))
		    ((= i n) tinfo)
		    (setq tinfo (append tinfo (list (ilulisp_tinfo-element ti i))))))))))

(defun create-true-c-server (self server-id object-table-parms)
  (ensure-initialized)
  (let* ((k-object-table
	  (if object-table-parms
	      (create-object-table (first object-table-parms)
				   (second object-table-parms)
				   self)
	    (c-null-pointer)))
	 (c-server (ilu_create-true-server
		    server-id k-object-table (my-language-index)))
	 (selfref (register-lisp-object
		   self :reftype :strong	; XXX should be weak ref
		   )))
    (if (c-null-pointer-p c-server)
       (error
       "Can't open ILU kernel server with server-id ~s and object table ~s (~s)"
        server-id object-table-parms k-object-table)
      (progn
	(ilu_set-lss c-server selfref (my-language-index))
	(ilu_exit-server c-server (ilulisp_get-root-class))
	c-server))))

(defmethod initialize-instance ((self kernel-server) &key (noport nil) (default-server t) &allow-other-keys)
  (call-next-method)
  (unless (server-id self) (setf (slot-value self 'id) (generate-server-id)))
  (setf (slot-value self 'c-server)
    (create-true-c-server
     self (server-id self) (server-object-table self)))
  (unless noport
    (add-port self (protocol self) (transport self) :default t))
  (if default-server (setf *default-server* self)))

(defmethod print-object ((self ilu:kernel-server) stream)
  (format stream "#<~s ~s>"
	  (type-of self)
	  (server-id self)))

(defun ensure-server ()
  (unless *default-server*
    (setf *default-server* (make-instance 'kernel-server :port 0)))
  *default-server*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Code for dealing with true objects
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *servers-inside* nil)

;; before: Main invariant holds
;; after:  Inside(server,pclass)
(defun find-or-create-true-kernel-object (ih server pclass lspo)
  (declare (special *servers-inside*))
  (let ((kserver (server-c-server server)))
    (unless (and (boundp '*servers-inside*) (find kserver *servers-inside*))
      (ilu_enter-server kserver pclass))
    (ilu_find-or-create-true-object
     ih kserver pclass
     (if lspo (register-lisp-object lspo :reftype :strong)
       0))))

(defclass ilu-true-object (ilu-object) ()); only appears in servers

(defparameter *object-id-counter* 1)

(defun create-default-id ()
  (setf *object-id-counter* (1+ *object-id-counter*))
  (format nil "~a" *object-id-counter*))

;; Inside(kernel-object->ob_server, kernel-object->ob_class)
(defun setup-true-object-links (lisp-obj kernel-object pclass server)
  (setf (ilu-server lisp-obj) (server-c-server server))
  (setf (ilu-class lisp-obj) (ilu_ilu-class kernel-object))
  (setf (ilu-cached-kernel-obj lisp-obj) kernel-object)
  (unless (and (boundp '*servers-inside*) *servers-inside*)
    (ilu_exit-server (ilu-server lisp-obj) pclass)))

(defmethod initial-pointer-reftype ((self ilu-true-object))
  :strong)

(defmethod initialize-instance :after ((self ilu-true-object)
				       &key
				       (ilu-kernel-server nil)
				       (ilu-instance-handle nil)
				       &allow-other-keys)
  (let ((server (or ilu-kernel-server (ensure-server)))
	(class-record (find-ilu-class-record self)))
    (setup-true-object-links
     self
     (find-or-create-true-kernel-object
      (or ilu-instance-handle (create-default-id))
      server class-record self)
     class-record server)))

(defmethod object-write-prep (call (self ilu-true-object) pclass)
  (declare (ignore pclass call))
  (when (null (ilu-cached-kernel-obj self))
    (let ((server (or (ilu-server self) (ensure-server)))
	  (class-record (find-ilu-class-record self)))
      (setup-true-object-links
       self
       (find-or-create-true-kernel-object
	(or (slot-value 'ilu-instance-handle self) (create-default-id))
	server class-record self)
       class-record server))))

(defgeneric call-server-stub-for-method-name (call method-id obj))

(defvar *gc-callback-object* nil)	;; hold onto this obj to prevent GC

(defclass ilu-gc-callback-class (ilu-true-object) ())

(defmethod find-ilu-class-record ((self ilu-gc-callback-class))
  (ilu_gc-callback-class))

(defun initialize-gc-callback ()
  (unless *gc-callback-object*
    (setq *gc-callback-object* (make-instance 'ilu-gc-callback-class))
    (let ((gc-callback-kobj (kernel-obj *gc-callback-object*)))
      (ilu_set-gc-callback gc-callback-kobj)
      (ilu_exit-server (ilu_ilu-server gc-callback-kobj)
		       (ilu_ilu-class gc-callback-kobj))
      (values))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Server functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *debug-uncaught-conditions* nil
  "If T, causes break to debugger.  Default to NIL.")

(defvar *trace-calls* nil
  "If T, causes message to be printed on each request serviced.  Default NIL.")

(defun handle-uncaught-condition (call cond lspo conn method-name sn)
  (declare (ignore conn))
  (unwind-protect
      (if *debug-uncaught-conditions*
	  (invoke-debugger cond)
	(warn
	 "Uncaught condition in call ~D: method ~A on ~A. Condition: ~A (~A)"
	 sn method-name lspo (type-of cond) cond))
    ; send "unknown error"
    (begin-exception call (- +protocol-exception-unknown+) 0)
    (finish-exception call)))

(defun get-discriminator (call pclass)
  (multiple-value-bind (ilu-obj class-record)
	    (ilulisp_input-object-id call pclass 1)
    (if (and ilu-obj (not (c-null-pointer-p ilu-obj)))
	(ilu-object->instance
	 (gethash (cpointer->key class-record) *ilu-to-lisp-class-table*
		  (gethash (cpointer->key pclass) *ilu-to-lisp-class-table*))
	 ilu-obj pclass)
      (error "Couldn't read kernel object~%"))))

(defvar *caller-identity* nil)

;; return t to keep the connection alive, nil otherwise
(defun handle-input-from-connection (conn server)
  (declare (ignore server))
  (let ((call (obtain-ilu-call-struct)))
    ;; we have taken a call struct,
    ;; use unwind-protect to ensure that it is returned
    (unwind-protect
      (multiple-value-bind (status initted intro-type method-name method-class sn)
		(ilulisp_receive-request call conn)
	(if (= status 0)
	    ;; ilu_RcvStat_noop
	    (progn
	      (if initted (finish-call call))
	      t)
	  (if (= status 1)
	      ;; ilu_RcvStat_quit
	      (progn
		(if initted
		    (finish-call call))
		nil)
	    ;; ilu_RcvStat_request
	    ;; ensure that finish-call will be called
	    (unwind-protect
	      (let ((lspo (get-discriminator call intro-type))
		    (*caller-identity* (caller-identity call)))
		(when *trace-calls*
		      (format t "~d:  ~s, ~a~%" sn lspo method-name))
		(handler-bind ((serious-condition
				#'(lambda (cond)
				    (handle-uncaught-condition
				     call cond lspo conn method-name sn)
				    (return-from
				     handle-input-from-connection t))))
		;(format
		; t "calling method ~s (~s) on object ~s with precedence list ~s.~%"
		; method-name (intern (concatenate 'string method-class "." method-name) :keyword)
		; lspo (clos:class-precedence-list (class-of lspo)))
		(call-server-stub-for-method-name
		 call (intern (concatenate 'string method-class "." method-name) :keyword) lspo))
		t)
	      (finish-call call)))))
      (return-ilu-call-struct call))))
  
(defun handle-new-connection (server port)
  (let ((conn (handle-ilu-connection port)))
    (unless (c-null-pointer-p conn)
      (setup-watch-connection #'handle-input-from-connection
			      conn server))))
