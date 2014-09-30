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

$Id: ilu-franz.lisp,v 1.41 1999/08/03 01:53:35 janssen Exp $
|#

(cl:in-package :ilu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Franz Allegro Common Lisp - specific ILU code
;;;
;;;  Any function, var, macro, etc., with "franz" in its name,
;;;  defined in this file, is part of the Franz ACL implementation,
;;;  and might not appear in implementations for other CLs.  Any
;;;  function, etc., without "franz" in its name, is a require
;;;  function or macro which the generic ILU lisp implementation
;;;  uses, and must be provided by any implementation.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Technology for mapping back and forth between Lisp and C
;;;
;;;  (register-lisp-object VAL &key (reftype (or :WEAK :STRONG))) =>
;;;                                                    TAG (of type fixnum)
;;;
;;;  (lookup-registered-lisp-object TAG) => VAL
;;;
;;;  (unregister-lisp-object TAG) => <void>
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  We provide both weak and strong references in this Franz impl,
;;;  as Franz ACL provides both weak refs and finalization on GC,
;;;  thus allowing us to hook the Lisp GC into the network GC.
;;;  Lisps without weak refs would ignore the :reftype keyword on
;;;  register-lisp-value, and would not have to implement the
;;;  weak registry shown here.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *franz-weak-array* (excl:weak-vector 100))
(defvar *franz-weak-array-next* 1)

(defun franz-weak-register-value (val)
  (let ((index
	 (ilu-process:without-scheduling
	   (let ((len (length *franz-weak-array*))
		 (zero-index (1- *franz-weak-array-next*)))
	     (if (< zero-index len)
		 (progn 
		   (setf (aref *franz-weak-array* zero-index) val)
		   (incf *franz-weak-array-next*)
		   zero-index)
	       ;; no empty slots on end of list, look for gaps
	       ;; from objects that have been deregistered
	       (let ((found-index (position nil *franz-weak-array*)))
		 (if found-index
		     ;; found a gap, use it
		     (progn
		       (setf (aref *franz-weak-array* found-index) val)
		       found-index)
		   ;; no gaps -- increase the size of the weak-vector
		   (let ((new-weak-vector (excl:weak-vector (* 2 len))))
		     (dotimes (i len)
		       (setf (aref new-weak-vector i)
			 (aref *franz-weak-array* i)))
		     (setf *franz-weak-array* new-weak-vector)
		     (setf (aref *franz-weak-array* len) val)
		     (incf *franz-weak-array-next*)
		     len))))))))
    ; (format t "~s weak-registered with index ~d.~%" val index)
    (1+ index)))

(defun franz-weak-value (index)
  (ilu-process:without-scheduling
    (when (and (> index 0) (<= (length *franz-weak-array*)))
      (aref *franz-weak-array* (1- index)))))

(defun franz-set-weak-value (index val)
  (ilu-process:without-scheduling
    (when (and (> index 0) (<= (length *franz-weak-array*)))
      (setf (aref *franz-weak-array* (1- index)) val))))

(defsetf franz-weak-value franz-set-weak-value)

(defun register-lisp-object (obj &key (reftype :strong))
  (let ((value (ecase reftype
		 (:strong
		  (1+ (* (ff:register-value obj) 2)))
		 (:weak
		  (* (franz-weak-register-value obj) 2))))
	)
;    (if (or (typep obj 'cl:standard-object) (evenp value))
;	(format t "Code for ~s is ~d~%" obj value))
    value))
	
(defun lookup-registered-lisp-object (index)
  (let ((obj (case (mod index 2)
	       (0	;; weak ref
		(franz-weak-value (truncate index 2)))
	       (1	;; strong ref
		(ff:lisp-value (truncate index 2))))))
    ;; (format t "Value for ~d is ~s~%" index obj)
    (if (null obj)
	(format t
	   "~%lookup-registered-lisp-object:  No value found for index ~d~%"
	   index))
    obj))

(defmacro unregister-lisp-object (index)
  `(case (mod ,index 2)
     (0	;; weak ref
      (setf (franz-weak-value (truncate ,index 2)) nil))
     (1  ;; strong ref
      (setf (ff:lisp-value (truncate ,index 2)) nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Foreign function utilities:
;;;
;;;  (define-c-function LISP-NAME C-NAME ARGS RETURN-TYPE &key INLINE)
;;;
;;;  (c-null-pointer-p VALL)
;;;  (c-null-pointer)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro c-null-pointer-p (val) `(zerop ,val))
(defmacro c-null-pointer () 0)

(eval-when (compile eval load)
  #+(or svr4 dlfcn)
  (push :lisp-understands-ansi-c-parameter-passing cl:*features*))

(defvar *franz-debug* nil "when non-NIL, enables debugging messages")

;(defmacro franz-c-pointer-arg ()
;  `'ff:foreign-address)

;; allowable return types from C function
;;
;; define-c-function	Lisp type			Franz ff package type
;; -----------------    ---------			----------------
;;
;; :short-cardinal	(unsigned-byte 16)		:fixnum
;; :cardinal		(unsigned-byte 32)		:integer
;;
;; :short-integer	(signed-byte 16)		:fixnum
;; :integer		(signed-byte 32)		:integer
;;
;; :short-real		single-float			:single-float
;; :real		double-float			:double-float
;;
;; :byte		(unsigned-byte 8)		:fixnum
;; :boolean		t or nil			:fixnum (1 for true, 0 for false)
;; :fixnum		fixnum				:fixnum
;;
;; :string		simple-string			:integer (C ptr to vector of C chars)
;; :constant-string	simple-string			:integer (C ptr to vector of C chars)	; can't be freed
;; :bytes		vector of (unsigned-byte 8)	:integer (C ptr to vector of C bytes)
;; :unicode		vector of (unsigned-byte 16)	:integer (C ptr to vector of C 16-bit vals)
;;
;; :ilu-call		(unsigned-byte 32)		:integer
;; :ilu-object		(unsigned-byte 32)		:integer
;; :ilu-class		(unsigned-byte 32)		:integer
;; :ilu-server		(unsigned-byte 32)		:integer
;; :char*		(unsigned-byte 32)		:integer
;; :pointer		(unsigned-byte 32)		:integer
;;

(eval-when (compile load eval)
   (defun franz-defforeign-return-type (type)
     (ecase type
	((:short-cardinal :short-integer :byte :boolean :fixnum)
	 :fixnum)
	((:cardinal :integer)
	 :integer)
	((:string :constant-string :bytes :unicode :ilu-call :ilu-object
	  :ilu-class :ilu-server :char* :pointer)
	 :integer)
	(:short-real :single-float)
	(:real :double-float)
	(:void :void)
	)))

;; allowable argument types to C function
;;
;; define-c-function	Lisp type			Franz ff package type
;; -----------------    ---------			----------------
;;
;; :short-cardinal	(unsigned-byte 16)		fixnum
;; :cardinal		(unsigned-byte 32)		integer
;;
;; :short-integer	(signed-byte 16)		fixnum
;; :integer		(signed-byte 32)		integer
;;
;; :short-real		single-float			single-float
;; :real		double-float			double-float
;;
;; :byte		fixnum				fixnum
;; :boolean		t or nil			fixnum (1 for true, 0 for false)
;; :fixnum		fixnum				fixnum
;;
;; :string		simple-string			ff:foreign-address (ptr to vector of C chars)
;; :constant-string	simple-string			ff:foreign-address (ptr to vector of C chars) ; should be copied
;;
;; :bytes		vector of (unsigned-byte 8)	ff:foreign-address (ptr to vector of C bytes)
;; :unicode		vector of (unsigned-byte 16)	ff:foreign-address (ptr to vector of C 16-bit vals)
;;
;; :ilu-call		(unsigned-byte 32)		ff:foreign-address
;; :ilu-object		(unsigned-byte 32)		ff:foreign-address
;; :ilu-class		(unsigned-byte 32)		ff:foreign-address
;; :ilu-server		(unsigned-byte 32)		ff:foreign-address
;; :char*		(unsigned-byte 32)		ff:foreign-address
;; :pointer		(unsigned-byte 32)		ff:foreign-address

(eval-when (compile load eval)
  (defun franz-defforeign-argument-type (arg)
    (let ((type (if (consp arg) (second arg) arg))
	     (direction (and (consp arg) (first arg))))
      (let ((basic-type
	     (ecase type
	       (:short-cardinal 'fixnum)
	       (:cardinal 'integer)
	       (:short-integer 'fixnum)
	       (:integer 'integer)
	       (:short-real 'single-float)
	       (:real 'double-float)
	       (:byte 'fixnum)
	       (:boolean 'fixnum)
	       (:fixnum 'fixnum)
	       ((:string :constant-string :bytes :unicode :ilu-call
		 :ilu-object :ilu-class :ilu-server :char* :pointer :ilu-kerr)
		'integer))))
	(if (and direction (member direction '(:out :inout)))
	    (list 'simple-array (if (equal basic-type 'integer)
				    '(unsigned-byte 32)
				  basic-type) 1)
	  basic-type)))))

(eval-when (compile load eval)
  (defconstant +franz-defforeign-allowable-inline-arg-types+
      #+(or svr4 dlfcn)
      '(:LISP :FIXNUM :INTEGER
	:SINGLE-FLOAT :DOUBLE-FLOAT :SIMPLE-STRING :CHARACTER)
      #-(or svr4 dlfcn)
      '(:FIXNUM :SINGLE-FLOAT :DOUBLE-FLOAT :SIMPLE-STRING :CHARACTER)
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  In some ports C pointers don't work as hash keys, therefore,
;;;  instead of using a C pointer "ptr" directly we use
;;;  (cpointer->key ptr) as a hash key. Here we map pointers to ints
;;;  so we can use them directly as hash keys.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cpointer->key (ptr)
  `,ptr)

;; we define a resource list of malloc'ed arrays, that is,
;; arrays allocated in C space via "excl:make-static-array" and never
;; GC'd.  That way we re-use the ones we make.

(defvar *franz-malloced-arrays*
    (make-hash-table :rehash-size 2.0 :test #'equal))
(defvar *franz-malloced-array-lock* (ilu-process:make-process-lock))

(defun franz-obtain-static-array (type &optional (initial-element nil
						  initial-element-p))
  (ilu-process:with-process-lock (*franz-malloced-array-lock*)
    (let ((premade-arrays (gethash type *franz-malloced-arrays*))
	  (actual-type (if (member type '(integer ff:foreign-address))
			   '(unsigned-byte 32)
			 type)))
      (if (not premade-arrays)
	  (if initial-element-p
	      #+(and allegro-version>= (version>= 4 3))
	      (make-array 1 :allocation :static :element-type actual-type
			  :initial-element initial-element)
	      #-(and allegro-version>= (version>= 4 3))
	      (excl:make-static-array 1 :element-type actual-type
				      :initial-element initial-element)
	    #+(and allegro-version>= (version>= 4 3))
	    (make-array 1 :allocation :static :element-type actual-type)
	    #-(and allegro-version>= (version>= 4 3))
	    (excl:make-static-array 1 :element-type actual-type))
	(let ((array (pop premade-arrays)))
	  (setf (gethash type *franz-malloced-arrays*) premade-arrays)
	  (if initial-element-p
	      (setf (aref array 0) initial-element))
	  array)))))

(defun franz-return-static-array (val type)
  (ilu-process:with-process-lock (*franz-malloced-array-lock*)
    (push val (gethash type *franz-malloced-arrays*))
    (values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  transform argument values as necessary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ff:defforeign 'ilulisp_convert-byte-vector-to-c
    :entry-point (ff:convert-to-lang "ilulisp_ConvertByteVectorToC"
				     :language :c)
    :arguments '((simple-array (unsigned-byte 8) *) fixnum)
    :return-type :integer	; in C, ByteVector *
    :callback nil
    :call-direct nil)

(ff:defforeign 'ilulisp_convert-unicode-vector-to-c
    :entry-point (ff:convert-to-lang "ilulisp_ConvertUnicodeVectorToC"
				     :language :c)
    :arguments '((simple-array (unsigned-byte 16) *) fixnum)
    :return-type :integer	; in C, UnicodeVector *
    :callback nil
    :call-direct nil)

(ff:defforeign 'ilulisp_byte-vector-size
    :entry-point (ff:convert-to-lang "ilulisp_ByteVectorSize" :language :c)
    :arguments `(integer)
    :return-type :fixnum
    :callback nil
    :call-direct #+(or svr4 dlfcn) t #-(or svr4 dlfcn) nil)

(ff:defforeign 'ilulisp_unicode-vector-size
    :entry-point (ff:convert-to-lang "ilulisp_UnicodeVectorSize" :language :c)
    :arguments `(integer)
    :callback nil
    :call-direct #+(or svr4 dlfcn) t #-(or svr4 dlfcn) nil)

(ff:defforeign 'ilulisp_free-byte-vector
    :entry-point (ff:convert-to-lang "ilulisp_FreeByteVector" :language :c)
    :arguments `(integer)
    :return-type :fixnum
    :callback nil
    :call-direct #+(or svr4 dlfcn) t #-(or svr4 dlfcn) nil)

(ff:defforeign 'ilulisp_free-unicode-vector
    :entry-point (ff:convert-to-lang "ilulisp_FreeUnicodeVector" :language :c)
    :arguments `(integer)
    :callback nil
    :call-direct #+(or svr4 dlfcn) t #-(or svr4 dlfcn) nil)

(ff:defforeign 'ilulisp_copy-byte-vector
    :entry-point (ff:convert-to-lang "ilulisp_CopyByteVectorToLisp"
				     :language :c)
    :arguments `(integer (simple-array (unsigned-byte 8) *))
    :callback nil
    :call-direct #+(or svr4 dlfcn) t #-(or svr4 dlfcn) nil)

(ff:defforeign 'ilulisp_copy-unicode-vector
    :entry-point (ff:convert-to-lang "ilulisp_CopyUnicodeVectorToLisp"
				     :language :c)
    :arguments `(integer (simple-array (unsigned-byte 16) *))
    :callback nil
    :call-direct #+(or svr4 dlfcn) t #-(or svr4 dlfcn) nil)

(defun franz-convert-byte-vector-from-c (bv)
  (let ((lisp-value (make-array (ilulisp_byte-vector-size bv)
				:element-type '(unsigned-byte 8))))
    (ilulisp_copy-byte-vector bv lisp-value)
    lisp-value))

(defun franz-convert-unicode-vector-from-c (bv)
  (let ((lisp-value (make-array (ilulisp_unicode-vector-size bv)
				:element-type '(unsigned-byte 16))))
    (ilulisp_copy-unicode-vector bv lisp-value)
    lisp-value))

(defmacro franz-transform-lisp-arg-value-to-defforeign-arg-value (type value)
  (let ((tvalue (gensym)))
    (case type
      (:boolean `(if ,value 1 0))
      ((:string :constant-string) `(if ,value (ff:string-to-char* ,value)
				     (c-null-pointer)))
      (:bytes `(let ((,tvalue ,value)) (ilulisp_convert-byte-vector-to-c
					,tvalue (length ,tvalue))))
      (:unicode `(let ((,tvalue ,value)) (ilulisp_convert-unicode-vector-to-c
					  ,tvalue (length ,tvalue))))
      (otherwise value))))

(defmacro franz-transform-lisp-arg-to-defforeign-arg (type dir value)
  (ecase dir
    (:in `(franz-transform-lisp-arg-value-to-defforeign-arg-value ,type ,value))
    (:inout
     `(franz-obtain-static-array ',(franz-defforeign-argument-type type)
	 (franz-transform-lisp-arg-value-to-defforeign-arg-value ,type ,value)))
    (:out
     `(franz-obtain-static-array ',(franz-defforeign-argument-type type)))))

(defmacro franz-transform-defforeign-value-to-lisp-value (type value)
  (case type
    ((:cardinal :ilu-call :ilu-object :ilu-class :ilu-server :char* :pointer)
     `(ldb (byte 32 0) ,value))
    (:boolean `(not (zerop ,value)))
    (:string `(unless (c-null-pointer-p ,value)
		(prog1
		    (ff:char*-to-string ,value)
		  (ff:free-cstruct ,value))))
    (:constant-string `(unless (c-null-pointer-p ,value) (ff:char*-to-string
							  ,value)))
    (:bytes `(prog1
		 (franz-convert-byte-vector-from-c ,value)
	       (ilulisp_free-byte-vector ,value)))
    (:unicode `(prog1
		   (franz-convert-unicode-vector-from-c ,value)
		 (ilulisp_free-unicode-vector ,value)))
    (otherwise `,value)))

(defmacro franz-transform-defforeign-arg-value-to-lisp-value (type value)
  ;; only called in cases where value was :out or :inout, so always has
  ;; associate array
  `(let ((interior-value (aref ,value 0)))
     (franz-return-static-array ,value ',(franz-defforeign-argument-type type))
     (franz-transform-defforeign-value-to-lisp-value ,type interior-value)))

(defmacro franz-maybe-free-defforeign-value (value type)
  (case type
    (:string `(ff:free-cstruct ,value))
    (:bytes `(ilulisp_free-byte-vector ,value))
    (:unicode `(ilulisp_free-unicode-vector ,value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; we define a resource list of malloc'ed ilu_Error structs,
;;; that way we re-use the ones we make.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *franz-malloced-kerrs* nil)

(ff:defforeign 'ilufranz_create-error-struct
    :entry-point (ff:convert-to-lang "ilufranz_CreateErrorStruct" :language :c)
    :arguments ()
    :return-type :integer)

(ff:defforeign 'ilufranz_clear-error-struct
    :entry-point (ff:convert-to-lang "ilufranz_ClearErrorStruct" :language :c)
    :arguments `(integer)
    :return-type :void)

(ff:defforeign 'ilufranz_error-ok
    :entry-point (ff:convert-to-lang "ilufranz_ErrorOK" :language :c)
    :arguments `(integer)
    :return-type :fixnum)

(ff:defforeign 'ilufranz_error-details
    :entry-point (ff:convert-to-lang "ilufranz_ErrorDetails" :language :c)
    :arguments `(integer)
    :return-type :integer)

(defun franz-obtain-kerr-struct ()
  (let ((kerr (ilu-process:without-scheduling
		(if *franz-malloced-kerrs*
		    (pop *franz-malloced-kerrs*)
		  (ilufranz_create-error-struct)))))
    (ilufranz_clear-error-struct kerr)
    kerr))

(defmacro franz-return-kerr-struct (val)
  `(ilu-process:without-scheduling
     (push ,val *franz-malloced-kerrs*)))

(ff:defforeign 'ilu_kerr->corba-error
    :entry-point (ff:convert-to-lang "ilu_CORBAizeSystemErr" :language :c)
    :arguments `(integer (simple-array (unsigned-byte 32) 1))
    :return-type :integer)

(defun franz-make-corba-exception (kerr-pointer)
  (let ((major (franz-obtain-static-array 'integer)))
    (let ((minor (ilu_kerr->corba-error kerr-pointer major))
	  (minor-description
	   (ff:char*-to-string (ilufranz_error-details kerr-pointer))))
      (make-condition
	  (case (let ((return-value (aref major 0)))
		  (franz-return-static-array major 'integer)
		  return-value)
	    (0 'corba-unknown)
	    (1 'corba-bad-param)
	    (2 'corba-no-memory)
	    (3 'corba-imp-limit)
	    (4 'corba-comm-failure)
	    (5 'corba-inv-objref)
	    (6 'corba-no-permission)
	    (7 'corba-internal)
	    (8 'corba-marshal)
	    (9 'corba-initialize)
	    (10 'corba-no-implement)
	    (11 'corba-bad-typecode)
	    (12 'corba-bad-operation)
	    (13 'corba-no-resources)
	    (14 'corba-no-response)
	    (15 'corba-persist-store)
	    (16 'corba-bad-inv-order)
	    (17 'corba-transient)
	    (18 'corba-free-mem)
	    (19 'corba-inv-ident)
	    (20 'corba-inv-flag)
	    (21 'corba-intf-repos)
	    (22 'corba-context)
	    (23 'corba-obj-adapter)
	    (24 'corba-data-conversion)
	    (25 'corba-object-not-exist)
	    (otherwise 'corba-unknown))
	:exception-value minor
	:exception-value-description minor-description
	:completion-status (default-completion-status)))))

;; this monster takes a description of a C function, of the form
;; (define-c-function LISP-NAME DOC-STRING C-NAME ARGS RETURN-TYPE
;;   &key INLINE)
;; where LISP-NAME is a symbol, C-NAME is a string,
;; ARGS is a list, each member of which is either a TYPE (implicitly
;; of direction :IN), or a 2-ple (DIRECTION TYPE),
;; where DIRECTION is one of (:IN :OUT :INOUT) and TYPE is a type keyword.
;; RETURN-TYPE may be specified as :VOID.
;;
;; This constructs a function called LISP-NAME whose arguments are
;; all the :IN and :INOUT arguments specified, in order, and which
;; returns multiple values, consisting of the specified RETURN-TYPE,
;; if any, followed by the :INOUT and :OUT arguments specified, in
;; the order specified.  Thus,
;;
;; (define-c-function FOO "doc" "foo"
;;   (:fixnum (:inout :cardinal) (:in :double-float) (:out :pointer))
;;   :fixnum)
;;
;; will produce a function FOO with three arguments of types
;; (FIXNUM :CARDINAL :DOUBLE-FLOAT) that returns three values
;; (FIXNUM :CARDINAL :POINTER).

(defmacro define-c-function (lisp-name doc-string c-name args return-type
			     &key inline)
  (declare (ignore doc-string))
  (format t "; Defining ~s => ~s~%" lisp-name c-name)
  (let* ((arg-directions
	  (mapcar #'(lambda (arg)
		      (if (consp arg) (first arg) :in))
		  (remove :ilu-kerr args)))
	 (arg-types
	  (mapcar #'(lambda (arg) (if (consp arg) (second arg) arg)) args))
	 (needs-wrapper-p
	  (or (member :out arg-directions)
	      (member :inout arg-directions)
	      (member :ilu-kerr arg-types)
	      (member return-type '(:cardinal :ilu-call :ilu-object :ilu-class
				    :ilu-server :char* :pointer))
	      (member :boolean arg-types) (eq :boolean return-type)
	      (member :string arg-types) (eq :string return-type)
	      (member :constant-string arg-types)
	      (eq :constant-string return-type)
	      (member :bytes arg-types) (eq :bytes return-type)
	      (member :unicode arg-types) (eq :unicode return-type)))
	 (lisp-function-name (if needs-wrapper-p (gensym) lisp-name)))
    `(progn

       ;; first define the foreign function
       (ff:defforeign
	   ',lisp-function-name
	   :arguments ',(mapcar #'franz-defforeign-argument-type args)
	   :entry-point ,(ff:convert-to-lang c-name :language :c)
	   :return-type ,(franz-defforeign-return-type return-type)
	   #+(or svr4 dlfcn)
	   ,@(when (or (find :short-real args)
		       (eq return-type :short-real))
	       '(:prototype t))
	   ,@(when (and
		    inline args
		    (every #'(lambda (arg)
			       (member
				(franz-defforeign-argument-type arg)
				+franz-defforeign-allowable-inline-arg-types+))
			   args))
	       '(:arg-checking nil :call-direct t)))

       ;; then add the wrapper, if one is needed to do either GC safety
       ;; (for strings and byte vectors), or type conversion,
       ;; or array allocation (for :inout or :out args)

       ,(if needs-wrapper-p
	    (let ((return-value-name (gensym))
		  (err (gensym))
		  (err-p (member :ilu-kerr args))
		  (internal-arg-names
		   (mapcar #'(lambda (arg)
			       (declare (ignore arg)) (gensym))
			   (remove :ilu-kerr args)))
		  (wrapper-args
		   (mapcar #'(lambda (dir)
			       (unless (eq :out dir)
				 (gensym)))
			   arg-directions)))
	      `(defun ,lisp-name ,(remove nil wrapper-args)
		 (let (,@(append
			  (mapcar
			   #'(lambda (name type dir wrapper-arg)
			       `(,name
				 (franz-transform-lisp-arg-to-defforeign-arg
				  ,type ,dir ,wrapper-arg)))
			   internal-arg-names arg-types
			   arg-directions wrapper-args)
			  (when err-p
			    `((,err (franz-obtain-kerr-struct))))))
		   (let ((,return-value-name
			  (,lisp-function-name
			   ,@(if err-p
				 (append
				  internal-arg-names (list err))
			       internal-arg-names))))
		     ,@(if (eq return-type :void)
			   `((declare (ignore ,return-value-name))))
		     ,@(remove
			nil
			(mapcar #'(lambda (value-name type dir)
				    (when (eq dir :in)
				      `(franz-maybe-free-defforeign-value
					,value-name ,type)))
				internal-arg-names arg-types arg-directions))
		     ,(when err-p
			  `(unless (/= 0 (ilufranz_error-ok ,err))
			     (let ((ex (franz-make-corba-exception ,err)))
			       (franz-return-kerr-struct ,err)
			       (error ex))))
		     ,(when err-p
			  `(franz-return-kerr-struct ,err))
		     (values
		      ,@(unless (eq return-type :void)
			  (list
			   `(franz-transform-defforeign-value-to-lisp-value
			     ,return-type ,return-value-name)))
		      ,@(mapcan
			 #'(lambda (value-name type dir)
			     (unless (eq dir :in)
			       (list
			   `(franz-transform-defforeign-arg-value-to-lisp-value
			     ,type ,value-name))))
			 internal-arg-names arg-types arg-directions))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Technology for hooking the Network GC together with the Lisp GC
;;;
;;;  (optional-finalization-hook ILU-OBJECT)
;;;
;;;     Sets up finalization, if available.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The approach here is to keep track of whether the ILU kernel
;;;  is "very interested" in an object.  If so, this means that either
;;;  the kernel has some low-level use for it, or that some other
;;;  external address space has some use for it.  In either case, the
;;;  Lisp object should not be GC'ed.  On the other hand, if the
;;;  the kernel is not very interested, and Lisp is not interested
;;;  in the object, it can be GC'ed.  So we register a callback with
;;;  the kernel, which is invoked by the kernel if its level of
;;;  interest in the object changes.  The callback moves our pointer
;;;  to the Lisp object to be either a strong ref (low-order bit
;;;  of index == 1) or a weak ref (low-order bit == 0).
;;;  
;;;  To actually finalize the object, we need to disassociate the
;;;  pointers between the Lisp object and the kernel object.  We use
;;;  ACL's finalization to do this.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *franz-shutdown-lock* (ilu-process:make-process-lock
			       :name "ILU Network GC Lock"))
(defvar *franz-shutdown-list* nil)
(defvar *franz-shutdown-verbose* nil)

(ff:defun-c-callable franz-interest-change ((tag :unsigned-long)
					    (interest :unsigned-long))
  (if (or (and (oddp tag) (zerop interest))
	  (and (evenp tag) (not (zerop interest))))
      (let ((lobj (lookup-registered-lisp-object tag)))
	(if lobj
	    (progn
	      (unregister-lisp-object tag)
	      (register-lisp-object lobj
			  :reftype (if (zerop interest) :weak :strong)))
	  0))
    tag))

(defun franz-shutdown-ilu-object (self)
  (when *franz-shutdown-verbose*
    (format t "ILU GC'ing ~s~%" self))
  (let ((kobj (kernel-obj self)))
    (when kobj
      (let ((tag (get-language-specific-object kobj (my-language-index))))
	(register-language-specific-object kobj 0 (my-language-index))
	(setf (ilu-cached-kernel-obj self) nil)
	(ilu_exit-server (ilu-server self) (ilu-class self))
	(unregister-lisp-object tag)))))

(defun franz-mark-for-shutdown (obj)
  (ilu-process:process-lock *franz-shutdown-lock*)
  (push obj *franz-shutdown-list*)
  (ilu-process:process-unlock *franz-shutdown-lock*))

;; executed in the scheduler ; implicitly within a without-scheduling block
(defun franz-test-for-shutdowns-available ()
  *franz-shutdown-list*)

(defun franz-shutdown-proc ()
  (loop
    (ilu-process:process-wait "Waiting for garbage"
			      #'franz-test-for-shutdowns-available)
    (let ((the-obj nil))
      (ilu-process:process-lock *franz-shutdown-lock*)
      (when *franz-shutdown-list*
	(setf the-obj (car *franz-shutdown-list*))
	(setf *franz-shutdown-list* (cdr *franz-shutdown-list*)))
      (ilu-process:process-unlock *franz-shutdown-lock*)
      (when the-obj
	(franz-shutdown-ilu-object the-obj)))))

(defun franz-start-shutdown-thread ()
  (ilu-process:fork-process '(:name "ILU Network GC Thread")
			    #'franz-shutdown-proc))

(defmacro optional-finalization-hook (self)
  `(excl:schedule-finalization ,self #'franz-mark-for-shutdown))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Technology for managing outgoing connections
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-c-function ilu_outgoing-connection-thread-proc
    "Monitors an outgoing connection"
  "ilu_OutgoingConnectionThreadProc"
  (:pointer :ilu-kerr) :boolean)

(defun watch-outgoing-connection (conn)
  (ilu-process:fork-process
   (format nil "ILU Outgoing Connection Watcher (connection 0x~x)" conn)
   #'ilu_outgoing-connection-thread-proc conn))

(define-c-function ilu_other-new-connection
    "Ask kernel whether it created another outgoing connection"
  "ilu_OtherNewConnection"
  (:ilu-kerr) :pointer)

(define-c-function ilu_new-connection-getter-forked
    "Tell kernel that we have forked the connection getter"
  "ilu_NewConnectionGetterForked"
  (:ilu-kerr) :boolean)

(defun connection-getter-thread-proc ()
  (loop
    (let ((new-conn (ilu_other-new-connection)))
      (if (= 0 new-conn)
	  (mp:process-sleep 0.3 "wait before calling ilu_other-new-connection")
	(watch-outgoing-connection new-conn)))))
	 
(defun fork-connection-getter ()
  (ilu-process:fork-process
   (format nil "ILU Outgoing Connection Collector")
   #'connection-getter-thread-proc)
  (ilu_new-connection-getter-forked))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Technology for managing incoming connections
;;;
;;;  (setup-new-connection-handler FN SERVER PORT SERVER-ID)
;;;
;;;   This is called when a kernel server creates a new port, SERVER
;;;   is implemented in this address space.  It should arrange to apply
;;;   FN to (list SERVER PORT) when an incoming connection arrives,
;;;   the function FN is assumed to return NIL if no request handler
;;;   could be established, non-NIL otherwise.  SERVER is the C address
;;;   of an ILU kernel ilu_Server, port is the C address of an ILU kernel
;;;   ilu_Port.
;;;
;;;  (setup-watch-connection FN CONN SERVER)
;;;
;;;   This should be called when a new incoming connection is setup.
;;;   It should arrange things so that FN is applied to
;;;   (list CONN SERVER) when input is available on CONN, and FN 
;;;   will return non-NIL if the input was successfully handled,
;;;   NIL otherwise.  If FN ever returns NIL, the connection-watcher
;;;   should be demolished.  CONN is the C address of an ILU kernel
;;;   ilu_Connection, and SERVER is the C address of an ILU kernel
;;;   ilu_Server.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-c-function ilu_blocking-wait-for-input-on-connection
    "Returns when input from a client is available"
  "ilu_BlockingWaitForInputOnConnection"
  (:pointer :pointer) :boolean)

(defun franz-watch-connection (fn conn server)
  (loop
    (if (ilu_blocking-wait-for-input-on-connection conn 0)
	(unless (funcall fn conn server)
	  (return))
      (return)))
  (when *franz-debug*
    (format t
       "~&ILU request processing thread for conn 0x~x terminated~%" conn)))

(defun setup-watch-connection (fn conn server)
  ;; we fork a new thread to handle requests from each new connection.
  (ilu-process:fork-process
   (format nil "ILU Incoming Connection Watcher (connection 0x~x to <~a>)"
	   conn (id-of-kernel-server server))
   #'franz-watch-connection fn conn server))

(define-c-function ilu_wait-for-port-connection-request
    "Returns when a client connects"
  "ilu_WaitForPortConnectionRequest"
  (:pointer :ilu-kerr) :boolean)

(defun franz-watch-for-connections (fn server port)
  (loop
    (if (ilu_wait-for-port-connection-request port)
	(funcall fn server port)
      (return)))
  (when *franz-debug*
    (format t "~&ILU Listener thread for connections to <~a> terminated~%"
	    (id-of-kernel-server server))))
      
(defun setup-new-connection-handler (fn server port)
  ;; we fork a thread to handle listening for connections, on each port
  (ilu-process:fork-process
   (format nil "ILU Incoming Connection Listener (server <~a>)"
	   (id-of-kernel-server server))
   #'franz-watch-for-connections fn server port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Technology for using UNICODE and ISO Latin-1 character strings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro construct-unicode-character-from-code (code)
  #+excl `(if (and (>= ,code 0) (< ,code 128)) (code-char ,code)
	    (progn
	      (warn "No Lisp character for Unicode value ~a." ,code)
	      (code-char 127)))
  #-excl `(declare (ignore code))
  #-excl `(error "Unable to construct Unicode characters in this lisp"))

(defmacro construct-iso-latin-1-character-from-code (code)
  #+excl `(code-char ,code)
  #-excl `(declare (ignore ,code))
  #-excl `(error "Unable to construct Unicode characters in this lisp"))

(defun ascii-to-string (character-values)
  ;; maps list of ASCII character codes to string
  (map 'string #'(lambda (v)
		   (construct-iso-latin-1-character-from-code v))
       character-values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Technology for using `server object tables'
;;;
;;;  Object tables are a scheme in ILU to allow lazy construction
;;;  of objects.  When the ILU kernel receives a method invocation on
;;;  an object which does not exist in the current address space, if
;;;  the kernel server designated by the server-ID portion of the
;;;  object's object-ID has an `object table' associated with it,
;;;  the ILU kernel will call the `ot_object_of_ih' method on the
;;;  object table with the instance handle of the desired object.
;;;  This method will call back into Lisp (at least for the Franz
;;;  implementation), where a new Lisp true object will be cons'ed
;;;  up, based on the information in the instance handle passed as
;;;  a parameter.
;;;
;;;  (create-object-table OBJECT-OF-IH-FN FREE-SELF-FN) => C pointer
;;;
;;;  When called, this should return a C pointer value, of the
;;;  C type "ilu_ObjectTable", or the value (c-null-pointer), if no such object
;;;  table can be produced.  The argument OBJECT-OF-IH-FN is a function
;;;  with the signature
;;;
;;;     ;; Locking: L1 >= {server}; L1 >= {gcmu} if result is true and collectible
;;;     ;; L2, Main unconstrained
;;;     (object-of-ih-fn ILU-INSTANCE-HANDLE) => ilu:ilu-true-object
;;;
;;;  Given an ILU instance handle (the knowledge of the server-ID is
;;;  supposed to be encoded into the routine), it will return an instance
;;;  of the class ilu:ilu-true-object.
;;;
;;;  The argument FREE-SELF-FN has the signature
;;;
;;;     ;; Locking: L1 >= {server}; L2, Main unconstrained.
;;;     (free-self-fn)
;;;
;;;  Should free any resources used by this `object table'.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-c-function ilufranz_create-object-table
    "Create an object table struct for ILU"
  "ilufranz_CreateObjectTable"
  (:fixnum) :pointer)

(defun create-object-table (object-of-ih-fn free-self-fn server)
  (let ((oti (register-lisp-object (list object-of-ih-fn free-self-fn server))))
    (ilufranz_create-object-table oti)))

(ff:defun-c-callable franz-ot-object-of-ih ((ot :fixnum)
					    (cstring :unsigned-long))
  (declare (special *servers-inside*))
  (let ((oti (lookup-registered-lisp-object ot))
	(ih (ff:char*-to-string cstring)))
    (if (or (null oti) (null ih))
	(c-null-pointer)
      (progn
	(push (server-c-server (third oti)) *servers-inside*)
	(unwind-protect
	    (let ((lisp-obj (funcall (car oti) ih)))
	      (if lisp-obj
		  (ilu-cached-kernel-obj lisp-obj)
		(c-null-pointer)))
	  (pop *servers-inside*))))))

(ff:defun-c-callable franz-ot-free-self ((ot :fixnum))
  (let ((oti (lookup-registered-lisp-object ot)))
    (when oti
      (unregister-lisp-object ot)
      (funcall (second oti))))
  0)

(define-c-function ilufranz_setup-object-tables
    "Initialize the C part of the Franz object table system"
  "ilufranz_SetupObjectTables"
  (:pointer :pointer) :void)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Technology for coordinating blocking reads & writes
;;;
;;;    These functions have to be callable from C.  Franz provides
;;;    support for registering lisp functions, then calling them
;;;    via an index from C.  There is also Franz-specific code in
;;;    ilu-franz-skin.c to support this.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ff:defun-c-callable franz-wait-for-input-on-fd ((fd :fixnum)
						 (time :unsigned-long))
  (when *franz-debug*
    (format t
       "(Franz:  Waiting for input on fd ~d with timeout ~d (milliseconds).)~%"
       fd time))
  (if (= 0 time)
      (progn (mp:wait-for-input-available fd) 1)
    (if (mp:wait-for-input-available fd :timeout (/ (double-float time) 1000.0))
	1
      0)))

;; can't wait for output stream in Allegro, so try workaround

(define-c-function ilufranz_output-possible-p
    "Returns 1 if output is possible"
  "ilufranz_OutputPossibleP" (:fixnum) :fixnum)

(ff:defun-c-callable franz-wait-for-output-on-fd ((fd :fixnum)
						  (time :unsigned-long))
  (when *franz-debug*
    (format t
      "(Franz:  Waiting for output on fd ~d with timeout ~d (milliseconds).)~%"
      fd time))
  (if (= time 0)
      (loop
	(if (= 1 (ilufranz_output-possible-p fd))
	    (return 1)
	  (mp:process-sleep 0.2 "waiting to do output")))
    (mp:with-timeout ((/ (double-float time) 1000.0) . (0))
      (loop
	(if (= 1 (ilufranz_output-possible-p fd))
	    (return 1)
	  (mp:process-sleep 0.2 "waiting to do output"))))))

(define-c-function ilufranz_set-wait-tech
    "Register I/O wait functions with kernel"
    "ilufranz_SetWaitTech" (:pointer :pointer) :void)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  An implementation of ILU alarms.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(defstruct franz-ilu-alarm
  (cname)
  (cvar)
  (process)
  (time)
  (closure))

(defmethod print-object ((self franz-ilu-alarm) stream)
  (format stream "#<ILU Alarm ~d ~s>"
	  (franz-ilu-alarm-cname self)
	  (franz-pretty-time (franz-ilu-alarm-time self))))

(defun franz-pretty-time (univ-time)
  (if univ-time
      (multiple-value-bind (second minute hour day month year
			    day-of-week ds-p tz)
	  (decode-universal-time (+ (car univ-time) (round (cdr univ-time))))
	(declare (ignore day-of-week ds-p tz))
	(format nil "~d.~d.~d ~d:~d:~d" day month year hour minute second))
    "(unset)"))

(defvar *franz-alarm-thread-counter* 0 "counter to identify different alarms")
(defvar *franz-alarms* nil)	;; list of alarms, for debugging

(define-c-function ilufranz_call-alarm-closure
    "Call the C closure represented by the alarm"
  "ilufranz_CallAlarmClosure" (:pointer :pointer) :void :inline t)

;; each alarm has a process associated with it.  The process runs this loop,
;; forever.  The loop waits for someone to set the alarm (and in the process
;; notify the condition variable), then sleeps until it's time to signal the
;; alarm.  It then checks the alarm time again (someone may have unset the
;; alarm in the meantime), and then calls the indicated C function, indirectly
;; through ilufranz_call-alarm-closure.  It then goes back to wait for someone
;; to set the alarm again.
(defun franz-do-franz-ilu-alarm (alarm-struct)
  (loop
      (unless (ilu-process:without-scheduling
		(and (franz-ilu-alarm-time alarm-struct)
		     (franz-ilu-alarm-closure alarm-struct)))
	(ilu-process:condition-variable-wait
	 (franz-ilu-alarm-cvar alarm-struct)))
    (when (franz-ilu-alarm-time alarm-struct)
	(mp:process-sleep (+ (- (car (franz-ilu-alarm-time alarm-struct))
				(get-universal-time))
			     (cdr (franz-ilu-alarm-time alarm-struct)))))
      (when (ilu-process:without-scheduling
	      (and (franz-ilu-alarm-time alarm-struct)
		   (franz-ilu-alarm-closure alarm-struct)))
	(ilufranz_call-alarm-closure
	 (car (franz-ilu-alarm-closure alarm-struct))
	 (cdr (franz-ilu-alarm-closure alarm-struct))))))

(ff:defun-c-callable franz-create-alarm ()
  (when *franz-debug*
    (format t "(Franz:  Creating alarm...)~%"))
  (let ((alarm-struct (make-franz-ilu-alarm :cvar
			 (ilu-process:make-condition-variable))))
    (setf (franz-ilu-alarm-process alarm-struct)
      (ilu-process:fork-process
       (format nil "ILU Alarm Thread ~d" (incf *franz-alarm-thread-counter*))
       #'franz-do-franz-ilu-alarm alarm-struct))
    (push alarm-struct *franz-alarms*)
    (setf (franz-ilu-alarm-cname alarm-struct)
      (register-lisp-object alarm-struct))))

(defun franz-internal-unset-alarm (alarm-struct)
    (ilu-process:without-scheduling
      (setf (franz-ilu-alarm-time alarm-struct) nil)
      (setf (franz-ilu-alarm-closure alarm-struct) nil))
    (mp:process-reset (franz-ilu-alarm-process alarm-struct)))

(ff:defun-c-callable franz-set-alarm ((alarm :unsigned-long)
				      (sec :signed-long)
				      (msec :unsigned-long)
				      (p1 :unsigned-long)
				      (p2 :unsigned-long))
  (let ((alarm-struct (lookup-registered-lisp-object alarm))
	(univ-time (cons (+ +unix-epoch+ sec) (/ msec 1000.0)))
	was-set)
;    (format t "~s => ~s (~a)~%"
;    alarm-struct univ-time (franz-pretty-time univ-time))
    (ilu-process:without-scheduling
      (setf was-set (and (franz-ilu-alarm-time alarm-struct)
			 (franz-ilu-alarm-closure alarm-struct)))
      (setf (franz-ilu-alarm-time alarm-struct) univ-time)
      (setf (franz-ilu-alarm-closure alarm-struct) (cons p1 p2)))
    (if was-set
	(mp:process-reset (franz-ilu-alarm-process alarm-struct))
      (ilu-process:condition-variable-notify
       (franz-ilu-alarm-cvar alarm-struct)))))

(ff:defun-c-callable franz-unset-alarm ((alarm :unsigned-long))
  (let ((alarm-struct (lookup-registered-lisp-object alarm)))
;    (format t "Unsetting ~s~%" alarm-struct)
    (franz-internal-unset-alarm alarm-struct)))

(define-c-function ilufranz_register-alarms
    "Initialize all the alarm mechanism"
  "ilufranz_SetAlarms" (:pointer :pointer :pointer) :void)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Technology for building and using locks.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Although they have "franz" in their names,
;;; these 9 functions are pretty generic.  They can be used with
;;; any lisp for which the ilu-process package works.

(ff:defun-c-callable franz-create-mutex ((d1 :unsigned-long)
					 (d2 :unsigned-long))
  (let ((s1 (ff:char*-to-string d1))
	(s2 (ff:char*-to-string d2)))
    (when *franz-debug*
      (format t "(Franz:  Creating mutex '~a~a'...)~%" s1 s2))
    (register-lisp-object (ilu-process:make-process-lock
			   :name (format nil "ILU mutex '~a~a'" s1 s2)))))

(ff:defun-c-callable franz-acquire-mutex ((m :fixnum))
  (when *franz-debug*
    (format t "(Franz:  Acquiring mutex ~s)~%"
	    (lookup-registered-lisp-object m)))
  (let ((mutex (lookup-registered-lisp-object m)))
    (unless (eq (ilu-process:current-process)
		(ilu-process:process-lock-locker mutex))
      (ilu-process:process-lock mutex)))
  (values))

(ff:defun-c-callable franz-hold-mutex ((m :fixnum))
  (when *franz-debug*
    (format t "(Franz:  Hold mutex ~s?)~%" (lookup-registered-lisp-object m)))
  (let ((lock (lookup-registered-lisp-object m)))
    (unless (eq (ilu-process:current-process)
		(ilu-process:process-lock-locker lock))
      (error "Mutex ~s not held by current process!" lock))))	

(ff:defun-c-callable franz-release-mutex ((m :fixnum))
  (when *franz-debug*
    (format t "(Franz:  Releasing mutex ~s)~%"
	    (lookup-registered-lisp-object m)))
  (ilu-process:process-unlock (lookup-registered-lisp-object m))
  (values))

(ff:defun-c-callable franz-destroy-mutex ((m :fixnum))
  (when *franz-debug*
    (format t "(Franz:  Destroying mutex ~s)~%"
	    (lookup-registered-lisp-object m)))
  (let ((mutex (lookup-registered-lisp-object m)))
    (if mutex
	(unregister-lisp-object m)))
  (values))

(ff:defun-c-callable franz-cvar-create ((d1 :unsigned-long)(d2 :unsigned-long))
  (let ((s1 (ff:char*-to-string d1))
	(s2 (ff:char*-to-string d2)))
    (when *franz-debug*
      (format t "(Franz:  Creating cvar '~a~a'...)~%" s1 s2))
    (register-lisp-object
     (ilu-process:make-condition-variable
      :name (format nil "ILU cvar '~a~a'" s1 s2)))))

(ff:defun-c-callable franz-cvar-notify ((v :fixnum))
  (let ((var (lookup-registered-lisp-object v)))
    (if var
	(ilu-process:condition-variable-notify var)))
  (values))

(ff:defun-c-callable franz-cvar-wait ((v :fixnum) (m :fixnum) (m2 :fixnum))
  (let ((var (lookup-registered-lisp-object v))
	(mutex (lookup-registered-lisp-object m))
	(mutex2 (lookup-registered-lisp-object m2)))
    (when *franz-debug*
      (format t "(Franz:  Prepare to wait for ~a)~%"
	      (ilu-process:condition-variable-name var)))
    (when (and var mutex)
      (unless (equal mutex mutex2)
	(when *franz-debug*
	    (format t "(Franz:  Releasing mutex ~s)~%" mutex2))
	(ilu-process:process-unlock mutex2))
      (ilu-process:without-scheduling
	(when *franz-debug*
	  (format t "(Franz:  Releasing mutex ~s)~%" mutex))
	(ilu-process:process-unlock mutex)
	(when *franz-debug*
	  (format t "(Franz:  Wait for cvar ~s)~%" var))
	(ilu-process:condition-variable-wait var))
      (unless (equal mutex mutex2)
	(when *franz-debug*
	  (format t "(Franz:  Acquiring mutex ~s)~%" mutex2))
	(ilu-process:process-lock mutex2))
      (when *franz-debug*
	(format t "(Franz:  Acquiring mutex ~s)~%" mutex))
      (ilu-process:process-lock mutex))
    (when *franz-debug*
      (format t "(Franz:  Finished waiting for ~a)~%"
	      (ilu-process:condition-variable-name var))))
  (values))

(ff:defun-c-callable franz-cvar-destroy ((v :fixnum))
  (let ((var (lookup-registered-lisp-object v)))
    (if var
	(unregister-lisp-object v)))
  (values))
  
(define-c-function ilufranz_register-lock-tech
    "Register Franz locking with kernel"
  "ilufranz_SetLockTech"
  (:pointer :pointer :pointer :pointer :pointer	;mutex create, acquire, hold, release, destroy
   :pointer :pointer :pointer :pointer		; condition create, notify, destroy, wait
   :ilu-kerr)
  :void)

(define-c-function ilufranz_set-network-gc-hook
    "Register Franz locking with kernel"
  "ilufranz_SetInterestHook"
  (:pointer :fixnum)	;; object noter
  :void)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Allow C code to fork a new thread with this call
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-c-function ilufranz_call-thread-proc
    "Invoke a C procedure with a specified argument"
  "ilufranz_CallThreadProc"
  (:cardinal :cardinal) :boolean)

(ff:defun-c-callable franz-fork-thread ((proc :unsigned-long) (rock :unsigned-long))
  (when *franz-debug*
    (format t "(Franz:  Forking thread ~a ~a...)~%" proc rock))
  (ilu-process:fork-process
   (format nil "ILU kernel thread ~a ~a" proc rock)
   #'ilufranz_call-thread-proc proc rock))

(define-c-function ilufranz_set-fork-proc
    "Register a Franz function which will create a new thread"
  "ilufranz_SetForkProc"
  (:pointer) :void)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; we define a resource list of malloc'ed ilu_Call_s structs,
;; that way we re-use the ones we make.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *malloced-ilu-call-structs* nil)

(define-c-function ilulisp_create-call-struct
  "allocate an ilu_Call_s structure"
  "ilulisp_CreateCallStruct"
  (:ilu-kerr) :ilu-call);

(defun obtain-ilu-call-struct ()
  (ilu-process:without-scheduling
    (if *malloced-ilu-call-structs*
	(pop *malloced-ilu-call-structs*)
      (ilulisp_create-call-struct))))

(defmacro return-ilu-call-struct (val)
  `(ilu-process:without-scheduling
     (push ,val *malloced-ilu-call-structs*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-locking ()
  (unless mp:*all-processes*
    (mp:start-scheduler))
  (ilufranz_set-network-gc-hook
   (ff:register-function 'franz-interest-change)
   (my-language-index))
  (franz-start-shutdown-thread)
  (ilufranz_set-wait-tech
   (ff:register-function 'franz-wait-for-input-on-fd)
   (ff:register-function 'franz-wait-for-output-on-fd))
  (ilufranz_register-lock-tech
   (ff:register-function 'franz-create-mutex)
   (ff:register-function 'franz-acquire-mutex)
   (ff:register-function 'franz-hold-mutex)
   (ff:register-function 'franz-release-mutex)
   (ff:register-function 'franz-destroy-mutex)
   (ff:register-function 'franz-cvar-create)
   (ff:register-function 'franz-cvar-notify)
   (ff:register-function 'franz-cvar-destroy)
   (ff:register-function 'franz-cvar-wait))
  (ilufranz_register-alarms
   (ff:register-function 'franz-create-alarm)
   (ff:register-function 'franz-set-alarm)
   (ff:register-function 'franz-unset-alarm))
  (ilufranz_set-fork-proc
   (ff:register-function 'franz-fork-thread))
  (ilufranz_setup-object-tables
   (ff:register-function 'franz-ot-object-of-ih)
   (ff:register-function 'franz-ot-free-self))
  (fork-connection-getter)
  )
