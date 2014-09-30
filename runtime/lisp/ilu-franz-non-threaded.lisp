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

$Id: ilu-franz-non-threaded.lisp,v 1.5 1999/08/03 01:53:34 janssen Exp $
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
;;;  (register-lisp-object VAL &key (reftype (or :WEAK :STRONG))) => TAG (of type fixnum)
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
	 (let ((len (length *franz-weak-array*)))
	   (if (< *franz-weak-array-next* len)
	       (let ((index *franz-weak-array-next*))
		 (setf (aref *franz-weak-array* (1- index)) val)
		 (setf *franz-weak-array-next* (1+ index))
		 index)
	     ;; no empty slots on end of list, look for gaps
	     ;; from objects that have been deregistered
	     (let ((found-index (do ((i 0 (1+ i)))
				    ((or (>= i len)
					 (null (aref *franz-weak-array* i)))
				     (if (null (aref *franz-weak-array* i))
					 (1+ i)
				       nil)))))
	       (if found-index
		   ;; found a gap, use it
		   (progn
		     (setf (aref *franz-weak-array* (1- found-index)) val)
		     found-index)
		 ;; no gaps -- increase the size of the weak-vector
		 (let ((new-weak-vector (excl:weak-vector (* 2 len)))
		       (index *franz-weak-array-next*))
		   (dotimes (i len)
		     (setf (aref new-weak-vector i) (aref *franz-weak-array* i)))
		   (setf *franz-weak-array* new-weak-vector)
		   (setf (aref *franz-weak-array* (1- index)) val)
		   (setf *franz-weak-array-next* (1+ index))
		   index)
		 ))))))
;    (format t "~s weak-registered with index ~d.~%" val index)
    index))

(defun franz-weak-value (index)
  (when (and (> index 0) (<= (length *franz-weak-array*)))
    (aref *franz-weak-array* (1- index))))

(defun franz-set-weak-value (index val)
  (when (and (> index 0) (<= (length *franz-weak-array*)))
    (setf (aref *franz-weak-array* (1- index)) val)))

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
	(format t "lookup-registered-lisp-object:  No value found for index ~d~%" index))
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
  #+svr4
  (push :lisp-understands-ansi-c-parameter-passing cl:*features*)

(defvar *franz-debug* nil "when non-NIL, enables debugging messages")

(defmacro franz-c-pointer-arg ()
  `'ff:foreign-address)

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

(defun franz-defforeign-return-type (type)
  (ecase type
    ((:short-cardinal :short-integer :byte :boolean :fixnum)
     :fixnum)
    ((:cardinal :integer)
     :integer)
    ((:string :constant-string :bytes :unicode :ilu-call :ilu-object :ilu-class :ilu-server :char* :pointer)
     :integer)
    (:short-real :single-float)
    (:real :double-float)
    (:void :void)
    ))

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
	     ((:string :constant-string :bytes :unicode :ilu-call :ilu-object :ilu-class :ilu-server :char* :pointer)
	      (franz-c-pointer-arg)))))
      (if (and direction (member direction '(:out :inout)))
	  (list 'simple-array (if (member basic-type '(ff:foreign-address integer)) '(unsigned-byte 32) basic-type) 1)
	basic-type))))

#.(defconstant +franz-defforeign-allowable-inline-arg-types+
      #+svr4
      '(FOREIGN-FUNCTIONS:FOREIGN-ADDRESS :LISP :FIXNUM :INTEGER :SINGLE-FLOAT :DOUBLE-FLOAT :SIMPLE-STRING :CHARACTER)
      #-svr4
      '(:FIXNUM :SINGLE-FLOAT :DOUBLE-FLOAT :SIMPLE-STRING :CHARACTER)
      )

)

;; we define a resource list of malloc'ed arrays, that is,
;; arrays allocated in C space via "excl:make-static-array" and never
;; GC'd.  That way we re-use the ones we make.

(defvar *franz-malloced-arrays* (make-hash-table :rehash-size 2.0 :test #'equal))

(defun franz-obtain-static-array (type &optional (initial-element nil initial-element-p))
  (let ((premade-arrays (gethash type *franz-malloced-arrays*))
	(actual-type (if (member type '(integer ff:foreign-address))
			 '(unsigned-byte 32)
		       type)))
    (if (not premade-arrays)
	(if initial-element-p
	    (excl:make-static-array 1 :element-type actual-type :initial-element initial-element)
	  (excl:make-static-array 1 :element-type actual-type))
      (let ((array (pop premade-arrays)))
	(setf (gethash type *franz-malloced-arrays*) premade-arrays)
	(if initial-element-p
	    (setf (aref array 0) initial-element))
	array))))

(defun franz-return-static-array (val type)
  (push val (gethash type *franz-malloced-arrays*))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  transform argument values as necessary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ff:defforeign 'ilulisp_convert-byte-vector-to-c
    :entry-point (ff:convert-to-lang "ilulisp_ConvertByteVectorToC" :language :c)
    :arguments '((simple-array (unsigned-byte 8) *) fixnum)
    :return-type :integer	; in C, ByteVector *
    :callback nil
    :call-direct nil)

(ff:defforeign 'ilulisp_convert-unicode-vector-to-c
    :entry-point (ff:convert-to-lang "ilulisp_ConvertUnicodeVectorToC" :language :c)
    :arguments '((simple-array (unsigned-byte 16) *) fixnum)
    :return-type :integer	; in C, UnicodeVector *
    :callback nil
    :call-direct nil)

(ff:defforeign 'ilulisp_byte-vector-size
    :entry-point (ff:convert-to-lang "ilulisp_ByteVectorSize" :language :c)
    :arguments `(,(franz-c-pointer-arg))
    :return-type :fixnum
    :callback nil
    :call-direct #+svr4 t #-svr4 nil)

(ff:defforeign 'ilulisp_unicode-vector-size
    :entry-point (ff:convert-to-lang "ilulisp_UnicodeVectorSize" :language :c)
    :arguments `(,(franz-c-pointer-arg))
    :callback nil
    :call-direct #+svr4 t #-svr4 nil)

(ff:defforeign 'ilulisp_free-byte-vector
    :entry-point (ff:convert-to-lang "ilulisp_FreeByteVector" :language :c)
    :arguments `(,(franz-c-pointer-arg))
    :return-type :fixnum
    :callback nil
    :call-direct #+svr4 t #-svr4 nil)

(ff:defforeign 'ilulisp_free-unicode-vector
    :entry-point (ff:convert-to-lang "ilulisp_FreeUnicodeVector" :language :c)
    :arguments `(,(franz-c-pointer-arg))
    :callback nil
    :call-direct #+svr4 t #-svr4 nil)

(ff:defforeign 'ilulisp_copy-byte-vector
    :entry-point (ff:convert-to-lang "ilulisp_CopyByteVectorToLisp" :language :c)
    :arguments `(,(franz-c-pointer-arg) (simple-array (unsigned-byte 8) *))
    :callback nil
    :call-direct #+svr4 t #-svr4 nil)

(ff:defforeign 'ilulisp_copy-unicode-vector
    :entry-point (ff:convert-to-lang "ilulisp_CopyUnicodeVectorToLisp" :language :c)
    :arguments `(,(franz-c-pointer-arg) (simple-array (unsigned-byte 16) *))
    :callback nil
    :call-direct #+svr4 t #-svr4 nil)

(defun franz-convert-byte-vector-from-c (bv)
  (let ((lisp-value (make-array (ilulisp_byte-vector-size bv) :element-type '(unsigned-byte 8))))
    (ilulisp_copy-byte-vector bv lisp-value)
    lisp-value))

(defun franz-convert-unicode-vector-from-c (bv)
  (let ((lisp-value (make-array (ilulisp_unicode-vector-size bv) :element-type '(unsigned-byte 16))))
    (ilulisp_copy-unicode-vector bv lisp-value)
    lisp-value))

(defmacro franz-transform-lisp-arg-value-to-defforeign-arg-value (type value)
  (let ((tvalue (gensym)))
    (case type
      (:boolean `(if ,value 1 0))
      ((:string :constant-string) `(if ,value (ff:string-to-char* ,value) (c-null-pointer)))
      (:bytes `(let ((,tvalue ,value)) (ilulisp_convert-byte-vector-to-c ,tvalue (length ,tvalue))))
      (:unicode `(let ((,tvalue ,value)) (ilulisp_convert-unicode-vector-to-c ,tvalue (length ,tvalue))))
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
    (:constant-string `(unless (c-null-pointer-p ,value) (ff:char*-to-string ,value)))
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
     (franz-return-static-array ,type ,value)
     (franz-transform-defforeign-value-to-lisp-value ,type interior-value)))

(defmacro franz-maybe-free-defforeign-value (value type)
  (case type
    (:string `(ff:free-cstruct ,value))
    (:bytes `(ilulisp_free-byte-vector ,value))
    (:unicode `(ilulisp_free-unicode-vector ,value))))

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
  (let* ((arg-directions (mapcar #'(lambda (arg) (if (consp arg) (first arg) :in)) args))
	 (arg-types (mapcar #'(lambda (arg) (if (consp arg) (second arg) arg)) args))
	 (needs-wrapper-p
	  (or (member :out arg-directions)
	      (member :inout arg-directions)
	      (member return-type '(:cardinal :ilu-call :ilu-object :ilu-class :ilu-server :char* :pointer))
	      (member :boolean arg-types) (eq :boolean return-type)
	      (member :string arg-types) (eq :string return-type)
	      (member :constant-string arg-types) (eq :constant-string return-type)
	      (member :bytes arg-types) (eq :bytes return-type)
	      (member :unicode arg-types) (eq :unicode return-type)))
	 (lisp-function-name (if needs-wrapper-p (gensym) lisp-name))
	 )
    `(progn

       ;; first define the foreign function
       
       (ff:defforeign ',lisp-function-name
	   :arguments ',(mapcar #'franz-defforeign-argument-type args)
	   :return-type ,(franz-defforeign-return-type return-type)
	   :entry-point ,(ff:convert-to-lang c-name :language :c)
	   #+svr4
	   ,@(when (or (find :short-real args)
		       (eq return-type :short-real))
	       '(:prototype t))
	   ,@(when (and inline args
			(every #'(lambda (arg)
				   (member
				    (franz-defforeign-argument-type arg)
				    +franz-defforeign-allowable-inline-arg-types+))
			       args))
	       '(:arg-checking nil :call-direct t))
	   )

       ;; then add the wrapper, if one is needed to do either GC safety
       ;; (for strings and byte vectors), or type conversion,
       ;; or array allocation (for :inout or :out args)

       ,(if needs-wrapper-p
	    (let ((return-value-name (gensym))
		  (internal-arg-names (mapcar #'(lambda (arg) (declare (ignore arg)) (gensym)) args))
		  (wrapper-args (mapcar #'(lambda (dir)
					    (unless (eq :out dir)
					      (gensym)))
					arg-directions))
		  )
	    `(defun ,lisp-name ,(remove nil wrapper-args)	;; define formal parameters

	       (let (,@(mapcar #'(lambda (name type dir wrapper-arg)
				   `(,name (franz-transform-lisp-arg-to-defforeign-arg ,type ,dir ,wrapper-arg)))
			       internal-arg-names arg-types arg-directions wrapper-args))
		 (let ((,return-value-name (,lisp-function-name ,@internal-arg-names)))
		   ,@(if (eq return-type :void) `((declare (ignore ,return-value-name))))
		   ,@(remove nil (mapcar #'(lambda (value-name type dir)
					     (when (eq dir :in)
					       `(franz-maybe-free-defforeign-value ,value-name ,type)))
					 internal-arg-names arg-types arg-directions))
		   (values ,@(unless (eq return-type :void)
			       (list `(franz-transform-defforeign-value-to-lisp-value
				       ,return-type ,return-value-name)))
			   ,@(mapcan #'(lambda (value-name type dir)
					 (unless (eq dir :in)
					   (list `(franz-transform-defforeign-arg-value-to-lisp-value ,type ,value-name))))
				     internal-arg-names arg-types arg-directions))
		   )))))
       )))

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
      (let ((tag (get-language-specific-object kobj)))
	(register-language-specific-object kobj 0)
	(setf (ilu-cached-kernel-obj self) nil)
	(ilu_exit-server (ilu-server self) (ilu-class self))
	(unregister-lisp-object tag)
	))))

(defun franz-mark-for-shutdown (obj)
  (push obj *franz-shutdown-list*))

(defmacro optional-finalization-hook (self)
  `(excl:schedule-finalization ,self #'franz-mark-for-shutdown))

;; executed in the scheduler ; implicitly within a without-scheduling block
(defun franz-test-for-shutdowns-available ()
  *franz-shutdown-list*)

(defvar *franz-shutdown-proc* nil)

;;
;; We need ILU alarms at this point, so import them from the ILU kernel...
;;

(define-c-function ilufranz_create-alarm
    "Return an ILU alarm object"
  "ilufranz_CreateAlarm" () :pointer)

(define-c-function ilufranz_set-alarm
    "Set an alarm object.  Takes 4 args:  the alarm,
a pointer to an ilu_FineTime struct, which specifies the 'alarm time'
at which to call the function, a pointer to a C-callable function of type (void) (*)(void *),
and the void * argument to call the function with."
  "ilufranz_CreateAlarm" (:pointer :pointer :pointer :pointer) :pointer)

(define-c-function ilufranz_now-plus-5-minutes
    "Return a pointer to an ilu_FineTime struct for *now* plus 5 minutes."
  "ilufranz_Plus5Minutes" () :pointer)

;;
;; now that we have the alarms, use them to establish
;; a periodic cleanup routine, that walks down the list of
;; "uninteresting" (from the ILU kernel viewpoint) objects,
;; and releases the kernel's hold on the LSPO
;;

(ff:defun-c-callable franz-shutdown-proc (alarm)
  (dolist (obj *franz-shutdown-list*)
    (franz-shutdown-ilu-object obj))
  (setf *franz-shutdown-list* nil)
  (ilufranz_set-alarm alarm (ilufranz_now-plus-5-minutes)
		 *franz-shutdown-proc* alarm))

(defun franz-start-shutdown-alarm ()
  (let ((gc-cleanup-alarm (ilufranz_create-alarm)))
    (setf *franz-shutdown-proc* (ff:register-function 'franz-shutdown-proc))
    (ilufranz_set-alarm gc-cleanup-alarm (ilufranz_now-plus-5-minutes)
		   *franz-shutdown-proc* gc-cleanup-alarm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Technology for managing connections
;;;
;;;  (setup-new-connection-handler FN SERVER PORT SERVER-ID)
;;;
;;;   This is called when a client connects to a kernel server, SERVER,
;;;   implemented in this address space.  It should arrange to apply
;;;   FN to (list SERVER PORT), which should return NIL if no handler
;;;   could be established, non-NIL otherwise.  SERVER is the C address
;;;   of an ILU kernel ilu_Server, port is the C address of an ILU kernel
;;;   ilu_Port.
;;;
;;;  (setup-connection-watcher FN CONN SERVER)
;;;
;;;   This should be called when a new connection is setup.  It should
;;;   arrange things so that FN is applied to (list CONN SERVER) when
;;;   when input is available on CONN, and FN should return non-NIL if
;;;   the input was successfully handled, NIL otherwise.  If FN ever
;;;   returns NIL, the connection-watcher should be demolished.  CONN
;;;   is the C address of an ILU kernel ilu_Connection, and SERVER is
;;;   the C address of an ILU kernel ilu_Server.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-c-function ilu_register-input-source
    "Establishes a callback handler.  When the ILU kernel notices that
the specified file descriptor has input, it calls the specified
function with the specified argument."
  "ilu_RegisterInputSource" (:fixnum :pointer :cardinal) :boolean)

(define-c-function ilu_unregister-input-source
    "Removes a callback handler."
  "ilu_UnregisterInputSource" (:fixnum) :boolean)

(defvar *callback-calling-function* nil)

(ff:defun-c-callable callback-calling-function ((fd :fixnum)
						(index :unsigned-long))
  (ilu_unregister-input-source fd)
  (let* ((callback-closure (lookup-registered-lisp-object index))
	 (status (apply (car callback-closure)
			(cdr callback-closure))))
    (if status
	(ilu_register-input-source
	 fd *callback-calling-function* index)
      (unregister-lisp-object index))
    (if status 1 0)))

(defun init-io-callbacks ()
  (setf *callback-calling-function*
    (ff:register-function 'callback-calling-function)))

(defun setup-watch-connection (fn conn server)

  ;; when a request comes in on a connection, we want to call
  ;; FN to read and dispatch the request

  (let ((fd (file-descriptor-of-connection conn)))
    (ilu_register-input-source
     fd *callback-calling-function*
     (register-lisp-object (list fn conn server)))))

(defun setup-new-connection-handler (fn server port)

  ;; when input is available on the file descriptor of the mooring
  ;; of PORT, call FN, passing FN SERVER and PORT as args.
  ;; Uses an ILU kernel I/O callback for this.
  
  (let ((fd (file-descriptor-of-mooring-of-port port)))
    (ilu_register-input-source
     fd *callback-calling-function*
     (register-lisp-object (list fn server port)))))

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

(ff:defun-c-callable franz-ot-object-of-ih ((ot :fixnum) (cstring :unsigned-long))
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

(define-c-function ilufranz_set-network-gc-hook
    "Register Franz locking with kernel"
  "ilufranz_SetInterestHook"
  (:pointer)	;; object noter
  :void)

(define-c-function ilu_run-main-loop
    "Call the ILU main loop and do event dispatching"
  "ilu_RunMainLoop" (:pointer) :void)

(define-c-function ilufranz_allocate-c-handle
    "Create and return a C handle on a main loop frame"
  "ilufranz_AllocateMainLoopHandle" (:cardinal) :pointer)

(define-c-function ilufranz_free-c-handle
    "Free the C handle"
  "ilufranz_FreeMainLoopHandle" (:pointer) :void)

(defun run-main-loop (&optional user-handle)
  (let ((handle (or user-handle (gensym))))
    (let ((c-handle (ilufranz_allocate-c-handle
		     (register-lisp-object handle))))
      (set handle c-handle)
      (ilu_run-main-loop c-handle))))

(defun exit-main-loop (handle)
  (let ((c-handle (symbol-value handle)))
    (ilu_exit-main-loop c-handle)
    (ilufranz_free-c-handle c-handle)
    (unregister-lisp-object handle)))

(defun initialize-locking ()
  (init-io-callbacks)
  (ilufranz_set-network-gc-hook
   (ff:register-function 'franz-interest-change))
  (franz-start-shutdown-alarm)
  (ilufranz_setup-object-tables
   (ff:register-function 'franz-ot-object-of-ih)
   (ff:register-function 'franz-ot-free-self))
  )
