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
;;; -*- Mode:Lisp; Package: ILU; Syntax:COMMON-LISP; Base:10 -*-
#|

$Id: ilu.lisp,v 1.119 1999/08/03 01:53:30 janssen Exp $
|#

(cl:in-package :ilu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Constants
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;; packet types ;;;;;;;;;;;;;;;;;;;;

(defconstant +packet-type-request+	0	"type value for request packet")
(defconstant +packet-type-reply+	1	"type value for reply packet")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Objects
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *language-id* nil "will be set to id for LISP language mapping")

(defvar *types-to-be-registered* nil "Set to a list of closures to be
called by the initialization function to register types that have
already been loaded")

(defun my-language-index () *language-id*)

(defun class-or-class-name->type (c-or-c-n)
  (if (or (stringp c-or-c-n)
	  (symbolp c-or-c-n))
      (find-class c-or-c-n)
    c-or-c-n))

(defun class-or-class-name->name (c-or-c-n)
  (if (or (stringp c-or-c-n)
	  (symbolp c-or-c-n))
      c-or-c-n
    (class-name c-or-c-n)))

(defmethod ilu-class-id (something)
  (declare (ignore something))
  nil)

(defmethod ilu-class-name (classname)
  (declare (ignore classname))
  nil)

(defmethod ilu-class-record (classname)
  (declare (ignore classname))
  nil)

(defgeneric ilu-class-info (classname &optional info-type))

(defun get-class-name (class)
  (ilu-class-name (class-or-class-name->name class)))

(defclass ilu-object ()
  ((ilu-cached-kernel-obj :initform nil :accessor ilu-cached-kernel-obj)	; C-runtime handle
   (ilu-server     	:initform nil :accessor ilu-server)
   (ilu-class	   	:initform nil :accessor ilu-class)
   (ilu-publish-proof	:initform nil :accessor ilu-publish-proof)
   (ilu-instance-id	:initform nil)
   (ilu-server-id	:initform nil)
   (ilu-string-binding-handle :initform nil)))

(defmethod initialize-instance :after ((self ilu-object)
				       &key &allow-other-keys)
  (optional-finalization-hook self))

;; before:  NOT Inside (server, class)
;; after:  if result non-nil, Inside(server, class) of result
(defmethod kernel-obj ((self ilu-object))
  (ilu_enter-server (ilu-server self) (ilu-class self))
  (unless (ilu-cached-kernel-obj self)
    (ilu_exit-server (ilu-server self) (ilu-class self)))
  (ilu-cached-kernel-obj self))

(defmethod instance-id ((self ilu-object))
  (or (slot-value self 'ilu-instance-id)
      (if (null (ilu-cached-kernel-obj self))
	  "(no kernel object)"
	(let ((kernel-object (kernel-obj self)))
	  (if (null kernel-object)
	      nil
	    ;; now Inside(SERVER,CLASS)
	    (let ((id-string (ilu_instance-id kernel-object)))
	      (ilu_exit-server (ilu-server self) (ilu-class self))
	      ;; now out of Server
	      (setf (slot-value self 'ilu-instance-id) id-string)
	      id-string))))))

(defmethod server-id ((self ilu-object))
  (or (slot-value self 'ilu-server-id)
      (if (ilu-server self)
	  (setf (slot-value self 'ilu-server-id) (id-of-kernel-server
						  (ilu-server self)))
	"(no kernel server)")))

(defmethod print-object ((self ilu-object) stream)
  (format stream "#<~s ~a@~a>"
	  (type-of self)
	  (instance-id self)
	  (server-id self)))

(defclass ilu-singleton-object (ilu-object) ())

(defmethod ilu-class-name ((self ilu-object))
  (ilu-class-info self :name))

(defmethod find-ilu-class-name (object)
  (declare (ignore object))
  nil)

(defmethod find-ilu-class-name ((object ilu-object))
  (dolist (class
	   #+ACLPC (allegro:class-precedence-list (class-of object))
	   #-ACLPC (clos:class-precedence-list (class-of object))
	   )
    (let ((name (ilu-class-name (class-name class))))
      (when name
	(return name)))))

(defmethod find-ilu-class-id ((object ilu-object))
  (dolist (class
	   #+ACLPC (allegro:class-precedence-list (class-of object))
	   #-ACLPC (clos:class-precedence-list (class-of object))
	   )
    (let ((id-pointer (ilu-class-id class)))
      (when id-pointer
	(return id-pointer)))))

(defmethod find-ilu-class-record ((object ilu-object))
  (dolist (class
	   #+ACLPC (allegro:class-precedence-list (class-of object))
	   #-ACLPC (clos:class-precedence-list (class-of object))
	   )
    (let ((record (ilu-class-record (class-name class))))
      (when (and record (not (c-null-pointer-p record)))
	(return record)))))

(defmethod rpc-method ((self ilu-object) putative-class method-id)
  (find-method-by-id putative-class method-id))

(defmethod ilu-class-id ((self ilu-object))
  (ilu-class-info self :id))

(defvar *ilu-to-lisp-class-table* (make-hash-table) "KEY is kernel class record, VALUE is CLOS class")
(defvar *lisp-to-ilu-class-table* (make-hash-table) "KEY is CLOS class, VALUE is ILU kernel class record")
(defvar *exception-repository-ids* (make-hash-table :test 'equal) "KEY is CORBA repository ID string, VALUE is lisp condition name")
(defvar *custom-surrogate-table* (make-hash-table) "KEY is standard CLOS class, VALUE is custom CLOS class")

(defun register-custom-surrogate (standard-class custom-class)
  (setf (gethash (find-class standard-class) *custom-surrogate-table*) custom-class))

(defvar *ilu-initialized* nil "t if the ILU kernel has been initialized")

(defvar *version* nil "set to string determining the current version of ILU")

(defun make-exception-vector (exceptions)
  (let ((vec (ilulisp_cons-exception-vector (length exceptions))))
    (do* ((i 0 (1+ i))
	  (e (nth i exceptions) (nth i exceptions)))
	((null e) nil)
      (ilulisp_set-exception-vector
       vec i (ilu_define-exception nil (car e) (cadr e))))
    vec))

(defun make-ilu-class-record (class-info)
  (let* ((superclasses (dig-out-class-info class-info :superclass-ids))
	 (superclass-vec (ilulisp_start-superclass-vector
			  (length superclasses) (car superclasses)))
	 (superclass-index 0))
    (dolist (superclass (cdr superclasses))
      (incf superclass-index)
      (ilulisp_add-superclass-to-vector
       superclass-vec
       (length superclasses)
       superclass-index
       superclass))
    (let ((class-record (ilu_define-object-type
			 (dig-out-class-info class-info :name)
			 (dig-out-class-info class-info :brand)
			 (dig-out-class-info class-info :id)
			 (dig-out-class-info class-info :singleton)
			 (dig-out-class-info class-info :optional-p)
			 (dig-out-class-info class-info :collectible-p)
			 (dig-out-class-info class-info :authentication)
			 (length (dig-out-class-info class-info :methods))
			 (length superclasses)
			 superclass-vec))
	  (methods (dig-out-class-info class-info :methods))
	  (method-index 0))
      (dolist (method methods)
	(let ((method-rec
	       (ilu_define-method
		class-record
		method-index
		(dig-out-method-info method :c-name)
		(dig-out-method-info method :index)
		(dig-out-method-info method :functional-p)
		(dig-out-method-info method :asynchronous-p)
		(length (dig-out-method-info method :exceptions))
		(make-exception-vector (dig-out-method-info method :exceptions))
		(length (nth 6 method))	; number of args
		(car (nth 5 method)))))	; return type
	  (do ((arg-index 0 (1+ arg-index))
	       (args (nth 6 method) (cdr args))
	       (arg nil))
	      ((null args))
	    (setf arg (car args))
	    (ilu_define-method-arg
	     method-rec arg-index (symbol-name (car arg))
	     (nth 4 arg) (arg-direction->fixnum (nth 2 arg))
	     (nth 3 arg)))
	  ;; XXX should call define-method-arg here...
	  (incf method-index)))
      (unless (ilu_object-type-defined class-record)
	(error "ilu_object-type-defined failed"))
      class-record)))

(defun initialize-ilu-class (class-name class-info)
  (let ((class (find-class class-name)))
    (if *ilu-initialized*
	(let ((record (make-ilu-class-record class-info)))
	  (setf (gethash class *lisp-to-ilu-class-table*) record)
	  (setf (gethash (cpointer->key record) *ilu-to-lisp-class-table*) class)
	  #+ilu-type-info
	  (with-type-mutex
	      (ilu_register-object-type (dig-out-class-info class-info :isl-name)
					(dig-out-class-info class-info :interface-name)
					(dig-out-class-info class-info :interface-brand)
					(dig-out-class-info class-info :id)
					record))
	  )      
      (setf (gethash class *lisp-to-ilu-class-table*) (c-null-pointer)))))

(defun initialize-ilu ()
  ;; set up LockTech and WaitTech
  (unless *ilu-initialized*
    (setf *ilu-initialized* t)
    (setf *version* (get-ilu-version))
    (setf *language-id* (ilu_register-language "LISP"))
    (when (not (string-equal *version* (get-lisp-runtime-version)))
      (error "Lisp runtime compiled against ILU version ~s, ILU kernel library compiled against ILU version ~s"
	     (get-lisp-runtime-version) *version*))
    ;; set up CheckFailure, AssertFailure, and MemFailure
    (set-failure-actions
     -3	;; check failure -- return error back to caller
     -2 ;; assert failure -- print message and dump core
     -2 ;; mem failure -- print message and dump core
     )
    ;;
    (initialize-locking)	;; defined in "ilu-<implementation>"
    ;;
    (maphash #'(lambda (v k)
		 (let ((classname (class-name v))
		       (r (ilu_find-class-from-id (ilu-class-info (class-name v) :id))))
		   (unless
		       (string-equal *version*
				     (ilu-class-info classname :ilu-version))
		     (warn "Stubs for ~s compiled with ILU stubber for version ~s, but current kernel is at ILU version ~s"
			   v (ilu-class-info classname :ilu-version)
			   *version*))
		   (cond
		    ((c-null-pointer-p r)
		     (initialize-ilu-class classname (ilu-class-info classname)))
		    ((not (= (cpointer->key k) (cpointer->key r)))
		     (setf (gethash v *lisp-to-ilu-class-table*) r)
		     (setf (gethash (cpointer->key r) *ilu-to-lisp-class-table*)
			   v)))))
	     *lisp-to-ilu-class-table*)
    ;;
    (initialize-gc-callback)	;; in ilu-server.lisp
    ;;
    #+(or ilu-iiop ilu-pickle)	;; register types
    (dolist (registration-closure *types-to-be-registered*)
      (funcall registration-closure)
      (delete registration-closure *types-to-be-registered* :test #'eq))
    ))

(defmacro ensure-initialized ()
    `(unless *ilu-initialized* (initialize-ilu)))

(defmethod singleton-p ((self ilu-object))
  nil)

(defmethod singleton-p ((self ilu-singleton-object))
  t)

(defmethod initial-pointer-reftype ((self ilu-object))
  :weak)

;; Inside(server,class)
(defun language-specific-object (obj)
  (let ((index (get-language-specific-object obj (my-language-index))))
    (unless (zerop index)
      (lookup-registered-lisp-object index))))

;; Inside(server,class)
(defsetf language-specific-object (obj) (lspo)
  `(progn
     (register-language-specific-object
      ,obj
      (register-lisp-object ,lspo :reftype (initial-pointer-reftype ,lspo))
      (my-language-index))
     ,lspo))

;; before: Inside(server,class)
;; after:  Main invariant holds
(defun ilu-object->instance (class kernel-obj class-record)
  (let ((lisp-obj (language-specific-object kernel-obj))
	(class-to-use (or (gethash class *custom-surrogate-table*) class)))
    (unless lisp-obj
      (setq lisp-obj (make-instance class-to-use))
      (setf (language-specific-object kernel-obj) lisp-obj)
      (setf (ilu-server lisp-obj) (ilu_ilu-server kernel-obj))
      (setf (ilu-class lisp-obj) (ilu_ilu-class kernel-obj))
      (setf (ilu-cached-kernel-obj lisp-obj) kernel-obj))
    (ilu_exit-server (ilu-server lisp-obj) class-record)
    lisp-obj))

;; L1_sup < smu; L2, Main unconstrained
(defun consider-sbh (newsbh)
  (multiple-value-bind (result new-server errorstruct)
      (ilu_consider-sbh newsbh)
    (declare (ignore new-server))
    (ecase result
      (0 (error "ILU signals error ~s" errorstruct))
      (1 (warn "non-reified server ~s" newsbh))
      (2 t)
      (3 (warn "~s is a true instance"))
      (4 t)
      (5 (progn
	   (format t "the server has been switched to the new contact info~%") t))
      )))

;; Main invariant holds
(defun sbh->instance (class-name sbh &key (mstid nil) (new-ih nil) (new-contact-info nil))
  "Given the CLASS-NAME and STRING-BINDING-HANDLE, return an ilu-object"
  (declare (simple-string sbh mstid) (ignore mstid))
  (ensure-initialized)
  (let ((the-sbh (if new-ih
		     (concatenate 'string new-ih (subseq sbh (position #\@ sbh)))
		   sbh)))
    (when new-contact-info
      (consider-sbh the-sbh))
    (let ((ilu-obj (ilu_object-of-sbh
		    the-sbh
		    (ilu-class-record class-name))))
      (if (c-null-pointer-p ilu-obj)
	  nil
	;; now Inside (server(ilu-obj), class(ilu-obj))
	(ilu-object->instance class-name ilu-obj (ilu_ilu-class ilu-obj))
	;; now Main holds
	))))

;; Main invariant holds
(defmethod string-binding-handle ((self ilu-object))
  (or (slot-value self 'ilu-string-binding-handle)
      (let ((kobj (kernel-obj self)))
	(when kobj
	  (setf (slot-value self 'ilu-string-binding-handle)
	    (ilu_sbh-of-object kobj))
	  (ilu_exit-server (ilu-server self) (ilu-class self))
	  (slot-value self 'ilu-string-binding-handle)))))

#+ilu-iiop
;; Main invariant holds
(defmethod interoperable-object-reference ((self ilu-object))
  (let ((ior nil)
	(kobj (kernel-obj self)))
    (when kobj
      (setf ior (ilu_ior-of-object kobj)))
    (ilu_exit-server (ilu-server self) (ilu-class self))
    ior))

(defun object-read (class call static-type-name)
  (multiple-value-bind (ilu-obj class-record)
      (ilulisp_input-object-id call (ilu-class-record
				     static-type-name) 0)
    (if (and ilu-obj (not (c-null-pointer-p ilu-obj)))
	(let ((lisp-obj (ilu-object->instance
			 (gethash
			  (cpointer->key class-record)
			  *ilu-to-lisp-class-table* class)
			 ilu-obj
			 (ilu-class-record static-type-name)
			 )))
	  lisp-obj)
      (unless (ilu-class-info static-type-name :optional-p)
	(error "Couldn't read kernel object of class ~s with pclass ~s~%" class static-type-name)))))

(defmethod object-write-prep (call self pclass)
  (declare (ignore call self pclass))
  (values))

(defun object-write (call self pclass)
  (unless self
    (unless (ilu-class-info pclass :optional-p)
      (error "NIL object of non-collectible class ~s passed" pclass)))
  (object-write-prep call self pclass)
  (object-id-write call
		   (if self (kernel-obj self) (c-null-pointer))
		   0 (ilu-class-record pclass))
  self)

(defun object-id-size (call kobj discriminator-p static-type exit-server-p)
  (let ((size (ilu_object-id-size call kobj discriminator-p static-type)))
    (when (and (not (c-null-pointer-p kobj)) exit-server-p)
      (ilu_exit-server (car exit-server-p) (cdr exit-server-p)))
    size))

(defun object-size (call self pclass)
  (unless self
    (unless (ilu-class-info pclass :optional-p)
      (error "NIL object of non-collectible class ~s passed" pclass)))
  (object-write-prep call self pclass)
  (object-id-size call (if self (kernel-obj self) (c-null-pointer))
		  0 (ilu-class-record pclass)
        (when self (cons (ilu-server self)
                 (ilu-class self)))))

(defmethod publish ((self ilu-object))
  (let ((kobj (kernel-obj self)))
    (when kobj
      (setf (ilu-publish-proof self) (ilu_publish-object kobj)))))

(defmethod withdraw ((self ilu-object))
  (when (stringp (ilu-publish-proof self))
    (let ((kobj (kernel-obj self)))
      (when kobj
	(let ((stat (ilu_withdraw-published-object kobj (ilu-publish-proof self))))
	  (if stat
	    (setf (ilu-publish-proof self) nil))
	  stat)))))

(defun lookup (sid ih type)
  (ensure-initialized)
  (let* ((class-record (ilu-class-record (class-or-class-name->name type)))
	 (kobj (ilu_lookup-object-by-oid sid ih class-record)))
    (when (not (c-null-pointer-p kobj))
      (ilu-object->instance (class-or-class-name->type type) kobj class-record))))

(defmethod ping ((self ilu-object))
  (let ((kobj (ilu-cached-kernel-obj self)))
    (when kobj
      (multiple-value-bind (retval new-conn)
	  (ilu_ping-object kobj)
	(unless (c-null-pointer-p new-conn)
	  (watch-outgoing-connection new-conn))
	retval))))

(defun form-string-binding-handle (sid ih type pinfo tinfo_vec)
  (flet ((encode-string (s)
	   (reduce #'(lambda (str ch)
		       (concatenate
			   'string
			 str
			 (if (find ch "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ._0123456789")
			     (string ch)
			   (format nil "%~02x" (char-code ch)))))
		   s :initial-value "")))
    (let ((type-id (typecode-uid (typecode type)))
	  (pinfo2 (reduce #'(lambda (res new)
			      (concatenate
				  'string
				res (if (zerop (length res)) "" "_")
				(encode-string new)))
			  pinfo))
	  (tinfo2 (reduce #'(lambda (res new)
			      (concatenate
				  'string
				res (if (zerop (length res)) "" "=")
				(reduce #'(lambda (res2 new2)
					    (concatenate
						'string
					      res2
					      (if (zerop (length res2))  "" "_")
					      (encode-string
					       (format nil "~a" new2))))
					new :initial-value "")))
			  tinfo_vec :initial-value "")))
      (concatenate 'string "ilu:"
		   (encode-string sid)
		   "/"
		   (encode-string ih)
		   ";"
		   (encode-string type-id)
		   ";"
		   pinfo2 "@" tinfo2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation of typecodes for Lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *typecode-by-uid* (make-hash-table :test #'equal)
  "Hash table mapping type UID strings to type manipulation
 functions (size, write, read)")

(defvar *typecode-by-name* (make-hash-table :test #'eq)
  "Hash table mapping type names to type manipulation
 functions (size, write, read)")

(defun add-type (uid name size-fn write-fn read-fn)
  (let ((typecode (list uid name size-fn write-fn read-fn)))
    (setf (gethash uid *typecode-by-uid*) typecode)
    (setf (gethash name *typecode-by-name*) typecode)))

(defun add-alias (uid name real-type)
  (declare (ignore uid name real-type)))

(defmethod typecode (selector)
  (cond ((stringp selector)
	 (gethash selector *typecode-by-uid*))
	((symbolp selector)
	 (gethash selector *typecode-by-name*))))  

(defun typecode-uid (tc)
  (nth 0 tc))

(add-type (ilulisp_get-primitive-type-uid "short cardinal")
	  'shortcardinal
	  #'(lambda (call value)
	      (short-cardinal-size call value))
	  #'(lambda (call value)
	      (short-cardinal-write call value))
	  #'(lambda (call)
	      (short-cardinal-read call)))
(add-type (ilulisp_get-primitive-type-uid "cardinal")
	  'cardinal
	  #'(lambda (call value)
	      (cardinal-size call value))
	  #'(lambda (call value)
	      (cardinal-write call value))
	  #'(lambda (call)
	      (cardinal-read call)))
(add-type (ilulisp_get-primitive-type-uid "long cardinal")
	  'longcardinal
	  #'(lambda (call value)
	      (long-cardinal-size call value))
	  #'(lambda (call value)
	      (long-cardinal-write call value))
	  #'(lambda (call)
	      (long-cardinal-read call)))

(add-type (ilulisp_get-primitive-type-uid "short integer")
	  'shortinteger
	  #'(lambda (call value)
	      (short-integer-size call value))
	  #'(lambda (call value)
	      (short-integer-write call value))
	  #'(lambda (call)
	      (short-integer-read call)))
(add-type (ilulisp_get-primitive-type-uid "integer")
	  'integer
	  #'(lambda (call value)
	      (integer-size call value))
	  #'(lambda (call value)
	      (integer-write call value))
	  #'(lambda (call)
	      (integer-read call)))
(add-type (ilulisp_get-primitive-type-uid "long integer")
	  'longinteger
	  #'(lambda (call value)
	      (long-integer-size call value))
	  #'(lambda (call value)
	      (long-integer-write call value))
	  #'(lambda (call)
	      (long-integer-read call)))

(add-type (ilulisp_get-primitive-type-uid "short real")
	  'shortreal
	  #'(lambda (call value)
	      (short-real-size call value))
	  #'(lambda (call value)
	      (short-real-write call value))
	  #'(lambda (call)
	      (short-real-read call)))
(add-type (ilulisp_get-primitive-type-uid "real")
	  'real
	  #'(lambda (call value)
	      (real-size call value))
	  #'(lambda (call value)
	      (real-write call value))
	  #'(lambda (call)
	      (real-read call)))
(add-type (ilulisp_get-primitive-type-uid "long real")
	  'longreal
	  #'(lambda (call value)
	      (long-real-size call value))
	  #'(lambda (call value)
	      (long-real-write call value))
	  #'(lambda (call)
	      (long-real-read call)))

(add-type (ilulisp_get-primitive-type-uid "short character")
	  'shortcharacter
	  #'(lambda (call value)
	      (short-character-size call value))
	  #'(lambda (call value)
	      (short-character-write call value))
	  #'(lambda (call)
	      (short-character-read call)))
(add-type (ilulisp_get-primitive-type-uid "character")
	  'character
	  #'(lambda (call value)
	      (short-cardinal-size call value))
	  #'(lambda (call value)
	      (short-cardinal-write call value))
	  #'(lambda (call)
	      (short-cardinal-read call)))

(add-type (ilulisp_get-primitive-type-uid "boolean")
	  'boolean
	  #'(lambda (call value)
	      (boolean-size call value))
	  #'(lambda (call value)
	      (boolean-write call value))
	  #'(lambda (call)
	      (boolean-read call)))

(add-type (ilulisp_get-primitive-type-uid "byte")
	  'byte
	  #'(lambda (call value)
	      (byte-size call value))
	  #'(lambda (call value)
	      (byte-write call value))
	  #'(lambda (call)
	      (byte-read call)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation of pickles for Lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+ilu-pickle
(defclass pickle ()
  ((bytes :reader pickle-bytes :initform nil)
   (type :reader pickle-type :initform nil)
  ))

#+ilu-pickle
(defmethod initialize-instance ((self pickle)
				&key
				(type nil)
				(value nil)
				(bytes nil)
				&allow-other-keys)
  (cond (type
	 (let ((tc (typecode type)))
	   (when (and tc (listp tc)
		      (= (length tc) 5))
	     (let ((size-fn (nth 2 tc))
		   (write-fn (nth 3 tc))
		   (uid (nth 0 tc))
		   (call (ilu_start-pickle)))
	       (let ((kernel-pickle
		      (let ((size (funcall size-fn call value)))
			(ilu_write-pickle call size uid)
			(funcall write-fn call value)
			(ilu_end-pickle-formation call))))
		 (setf (slot-value self 'bytes) kernel-pickle)
		 (setf (slot-value self 'type) (nth 1 tc)))))))
	(bytes
	 (let* ((uid (ilu_pickle-type-uid bytes))
		(tc (typecode uid))
		(tname (if tc (nth 1 tc) :unknown)))
	   (setf (slot-value self 'bytes) bytes)
	   (setf (slot-value self 'type) tname)))
	))

#+ilu-pickle
(defmethod print-object ((self pickle) stream)
  (format stream "#<~s ~s ~d>"
	  (type-of self)
	  (pickle-type self)
	  (length (or (pickle-bytes self) '()))))

#+ilu-pickle
(defmethod pickle-value ((self pickle))
  (let ((bytes (pickle-bytes self))
	(tc (typecode (pickle-type self))))
    (when (and bytes tc)
      (let ((call (ilu_start-pickle)))
	(ilu_read-pickle call bytes)
	(let ((value (funcall (nth 4 tc) call)))
	  (ilu_end-pickle-decode call)
	  value)))))

#+ilu-pickle
(defun pickle-size (call value)
  (ilulisp_sizeof-pickle call (pickle-bytes value)))

#+ilu-pickle
(defun pickle-write (call value)
  (ilulisp_output-pickle call (pickle-bytes value)))

#+ilu-pickle
(defun pickle-read (call)
  (let ((bytes (ilulisp_input-pickle call)))
    (make-instance 'pickle :bytes bytes)))

#+ilu-pickle
(add-type (ilulisp_get-primitive-type-uid "pickle")
	  'pickle #'pickle-size #'pickle-write #'pickle-read)

#+(and excl svr4)
(clos:finalize-inheritance (find-class 'pickle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Exceptions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (eval load compile)
   (define-condition rpc-exception (serious-condition)
     ((exception-value :accessor exception-value :initarg :exception-value)
      (exception-value-description :initform "(no description)"
				   :accessor exception-value-description
				   :initarg :exception-value-description))
     (:documentation
      "All exceptions signalled by RPC servers are subtypes of this exception")
     (:report (lambda (c s)
		(format s "#<~a -- ~a>"
			; (or (documentation (type-of c) 'type) (type-of c))
			(type-of c)
			(exception-value-description c))))
     ))

(defmethod exception-value-read (call (exception rpc-exception))
  (declare (ignore call)))

(defgeneric exception-value-size (call exception))
(defmethod exception-value-size (call (exception rpc-exception))
  (declare (ignore call))
  0)

(defgeneric exception-value-write (call exception))
(defmethod exception-value-write (call (exception rpc-exception))
  (declare (ignore call)))

(defmethod initialize-instance :after ((exception rpc-exception)
				       &key (call nil call-p)
				       &allow-other-keys)
  (when call-p
    (setf (exception-value exception)
      (exception-value-read call exception))))

(defun signal-exception (call position e)
  (begin-exception call position
		   (+ (begin-sizing-exception call position)
		      (exception-value-size call e)))
  (exception-value-write call e)
  (finish-exception call))

(define-condition protocol-error (rpc-exception) ()
  (:documentation "RPC Error"))

;;;;;;;;;;;;;;;;;;;; protocol error values ;;;;;;;;;;;;;;;;;;;;

(defconstant +protocol-exception-success+           0
  "value returned when RPC protocol has succeeded")
(defconstant +protocol-exception-no-such-class+     1
  "value for bad class ID at server")
(defconstant +protocol-exception-version-mismatch+  2
  "value for bad version at server")
(defconstant +protocol-exception-no-such-method+    3
  "value for bad method ID at server")
(defconstant +protocol-exception-bad-arguments+     4
  "value for invalid argument formatting")
(defconstant +protocol-exception-unknown+           5
  "catchall for protocol errors")
(defconstant +protocol-exception-no-connection+     6
  "value for bad or lost connection")
(defconstant +protocol-exception-request-rejected+  7
  "value for server refusing to look at packets")
(defconstant +protocol-exception-request-timeout+   8
  "no answer within timeout period")

;;;;;;;;;;;; CORBA system exceptions ;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +completed-yes+   0)
(defconstant +completed-no+    1)
(defconstant +completed-maybe+ 2)

(defun default-completion-status () +completed-maybe+)

(define-condition corba-system-exception (rpc-exception)
  ((completion-status :accessor completion-status :initarg :completion-status))
  (:documentation "Standard CORBA system exception"))

(define-condition corba-unknown (corba-system-exception) ()
  (:documentation "the unknown exception"))

(define-condition corba-bad-param (corba-system-exception) ()
  (:documentation "an invalid parameter was passed"))

(define-condition corba-no-memory (corba-system-exception) ()
  (:documentation "dynamic memory allocation failure"))

(define-condition corba-imp-limit (corba-system-exception) ()
  (:documentation "violated implementation limit"))

(define-condition corba-comm-failure (corba-system-exception) ()
  (:documentation "communication failure"))

(define-condition corba-inv-objref (corba-system-exception) ()
  (:documentation "invalid object reference"))

(define-condition corba-no-permission (corba-system-exception) ()
  (:documentation "no permission for attempted operation"))

(define-condition corba-internal (corba-system-exception) ()
  (:documentation "ORB internal error"))

(define-condition corba-marshal (corba-system-exception) ()
  (:documentation "error marshalling parameter/result"))

(define-condition corba-initialize (corba-system-exception) ()
  (:documentation "ORB initialisation failure"))

(define-condition corba-no-implement (corba-system-exception) ()
  (:documentation "operation implementation unavailable"))

(define-condition corba-bad-typecode (corba-system-exception) ()
  (:documentation "bad typecode"))

(define-condition corba-bad-operation (corba-system-exception) ()
  (:documentation "invalid operation"))

(define-condition corba-no-resources (corba-system-exception) ()
  (:documentation "insufficient resources for request"))

(define-condition corba-no-response (corba-system-exception) ()
  (:documentation "response to request not yet available"))

(define-condition corba-persist-store (corba-system-exception) ()
  (:documentation "persistent storage failure"))

(define-condition corba-bad-inv-order (corba-system-exception) ()
  (:documentation "routine invocations out of order"))

(define-condition corba-transient (corba-system-exception) ()
  (:documentation "transient failure - reissue request"))

(define-condition corba-free-mem (corba-system-exception) ()
  (:documentation "cannot free memory"))

(define-condition corba-inv-ident (corba-system-exception) ()
  (:documentation "invalid identifier syntax"))

(define-condition corba-inv-flag (corba-system-exception) ()
  (:documentation "invalid flag was specified"))

(define-condition corba-intf-repos (corba-system-exception) ()
  (:documentation "error accessing interface repository"))

(define-condition corba-context (corba-system-exception) ()
  (:documentation "error processing context object"))

(define-condition corba-obj-adapter (corba-system-exception) ()
  (:documentation "failure detected by object adapter"))

(define-condition corba-data-conversion (corba-system-exception) ()
  (:documentation "data conversion error"))

(define-condition corba-object-not-exist (corba-system-exception) ()
  (:documentation "non-existent object - delete reference"))
