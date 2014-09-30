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
Copyright (c) 1996 Mercury Scheduling Systems Inc. All Rights Reserved.


$Id: ilu-franz-win.lisp,v 1.10 1999/08/03 01:53:32 janssen Exp $
|#

(cl:in-package :ilu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Franz Allegro Common Lisp for Windows - specific ILU code
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; name of DLL for functions with names starting with ilulisp_ or ilufranz_
(defparameter franz-lisp-dll "ilulisp.dll")

;; name of DLL for functions with names not starting with ilulisp_ or ilufranz_ 
(defparameter franz-kernel-dll "ilu32.dll")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; we define a resource list of malloc'ed ilu_Error structs,
;;; that way we re-use the ones we make.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *franz-malloced-kerrs* nil)

(ct:defun-dll ilufranz_create-error-struct
	      ()
	      :entry-name "ilufranz_CreateErrorStruct"
	      :library-name franz-lisp-dll
	      :return-type (:void *))

(ct:defun-dll ilufranz_clear-error-struct
	      ((kerr (:void *)))
	      :entry-name "ilufranz_ClearErrorStruct"
	      :library-name franz-lisp-dll
	      :return-type :void)

;(ct:defun-dll ilufranz_error-ok
;	      ((kerr (:void *)))
;	      :entry-name "ilufranz_ErrorOK"
;	      :library-name franz-lisp-dll
;	      :return-type :short-bool)
;
; avoid a foreign call by checking ourselves (may stop working
; when the ilu_Error struct layout changes!):
(defun ilufranz_error-ok (kerr)
  (= 0 (ct:cref (:long 3) kerr 2)))

(ct:defun-dll ilufranz_error-details
	      ((kerr (:void *)))
	      :entry-name "ilufranz_ErrorDetails"
	      :library-name franz-lisp-dll
	      :return-type (:char *))

(defun franz-obtain-kerr-struct ()
  (if *franz-malloced-kerrs*
      (pop *franz-malloced-kerrs*)
    (ilufranz_create-error-struct)))

(defmacro franz-return-kerr-struct (val)
  `(push ,val *franz-malloced-kerrs*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Technology for mapping back and forth between Lisp and C
;;;
;;;  (register-lisp-object VAL &key (reftype (or :WEAK :STRONG)))
;;;       => TAG (of type fixnum)
;;;
;;;  (lookup-registered-lisp-object TAG) => VAL
;;;
;;;  (unregister-lisp-object TAG) => <void>
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  We provide only strong references in this Franz impl,
;;;  as Franz ACL for Windows does not support weak refs,
;;;  We ignore the :reftype keyword on register-lisp-object.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *franz-array* (cl:make-array 100))
(defvar *franz-array-next* 1)

(defun register-lisp-object (val &key reftype)
  (declare (ignore reftype))
  (let ((index
	 (let ((len (length *franz-array*))
	       (zero-index (1- *franz-array-next*)))
	   (if (< zero-index len)
	       (progn 
		 (setf (aref *franz-array* zero-index) val)
		 (incf *franz-array-next*)
		 zero-index)
	     ;; no empty slots on end of list, look for gaps
	     ;; from objects that have been deregistered
	     (let ((found-index (position nil *franz-array*)))
	       (if found-index
		   ;; found a gap, use it
		   (progn
		     (setf (aref *franz-array* found-index) val)
		     found-index)
		 ;; no gaps -- increase the size of the vector
		 (let ((new-vector (make-array (* 2 len)))
		       (index zero-index))
		   (dotimes (i len)
			    (setf (aref new-vector i)
				  (aref *franz-array* i)))
		   (setf *franz-array* new-vector)
		   (setf (aref *franz-array* len) val)
		   (incf *franz-array-next*)
		   len)))))))
    ;(format t "~&~s registered with index ~d.~%" val (1+ index))
    (1+ index)))

(defun franz-value (index)
  (when (and (> index 0) (<= (length *franz-array*)))
    (aref *franz-array* (1- index))))

(defun franz-set-value (index val)
  (when (and (> index 0) (<= (length *franz-array*)))
    (setf (aref *franz-array* (1- index)) val)))

(defsetf franz-value franz-set-value)

(defun lookup-registered-lisp-object (index)
  (let ((obj (franz-value index)))
    ;(format t "~&Value for ~d is ~s~%" index obj)
    (if (null obj)
	(format t "~&lookup-registered-lisp-object:  Invalid index ~d~%" index))
    obj))

(defmacro unregister-lisp-object (index)
  `(progn
     ;(format t "~&unregistered ~d~%" ,index)
     (setf (franz-value ,index) nil)))

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

(defmacro c-null-pointer-p (val) `(ct:null-cpointer-p ,val ))
(defmacro c-null-pointer () `ct:hnull)

(eval-when (compile eval load)
  (push :lisp-understands-ansi-c-parameter-passing cl:*features*))


;; allowable return types from C function
;;
;; define-c-function	Lisp type			Franz ct package type
;; -----------------    ---------			----------------
;;
;; :short-cardinal	(unsigned-byte 16)		:unsigned-short
;; :cardinal		(unsigned-byte 32)		:unsigned-long
;;
;; :short-integer	(signed-byte 16)		:short
;; :integer		(signed-byte 32)		:long
;;
;; :short-real		single-float			:single-float
;; :real		double-float			:double-float
;;
;; :byte		(unsigned-byte 8)		:unsigned-char
;; :boolean		t or nil			:long
;; :fixnum		fixnum				:short
;;
;; The names for the following two string types are a little counter-intuitive
;; when used to describe argument types. Basically :string should be used
;; when the caller (Lisp) retains ownership, i.e. the called C function
;; must not store or delete the passed-in char pointer. :const-string
;; should be used when ownership is passed into the function, i.e. Lisp
;; must not delete the passed-in pointer after the function returns.
;; 
;; :string		simple-string			(:char *)
;; :constant-string	simple-string			(:char *)
;;
;; :bytes		vector of (unsigned-byte 8)	(:unsigned-char *)
;; :unicode		vector of (unsigned-byte 16)	(:unsigned-short *)
;;
;; :ilu-call		(unsigned-byte 32)		(:void *)
;; :ilu-object		(unsigned-byte 32)		(:void *)
;; :ilu-class		(unsigned-byte 32)		(:void *)
;; :ilu-server		(unsigned-byte 32)		(:void *)
;; :char*		(unsigned-byte 32)		(:void *)
;; :pointer		(unsigned-byte 32)		(:void *)
;;

(eval-when (:compile-toplevel :load-top-level :execute)
   (defun franz-defforeign-return-type (type)
     (ecase type
	(:short-cardinal :unsigned-short)
	(:cardinal :unsigned-long)
	((:ilu-call :ilu-object :ilu-class
	  :ilu-server :char* :pointer :ilu-kerr) '(:void *))
	((:short-integer :fixnum) :short)
	((:integer :boolean) :long)
	(:short-real :single-float)
	(:real :double-float)
	(:byte :unsigned-char)
	((:string :constant-string) '(:char *))
	(:bytes '(:unsigned-char *))
	(:unicode '(:unsigned-short *))
	(:void :void))))

(eval-when (:compile-toplevel :load-top-level :execute)
   (defun franz-defforeign-argument-type (arg)
     (let ((type (if (consp arg) (second arg) arg))
	   (direction (if (consp arg) (first arg) :in)))
       (let ((basic-type (franz-defforeign-return-type type)))
	 (if (member direction '(:out :inout))
	     (list basic-type '*)
	   basic-type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  C pointers don't work as hash keys, therefore, instead of using a
;;;  C pointer "ptr" directly we use (cpointer->key ptr) as a hash key 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cpointer->key (ptr)
  `(ct:cpointer-value ,ptr))

;; we define a resource list of malloc'ed arrays, that is,
;; arrays allocated in C space via ilu_malloc and never
;; GC'd.  That way we re-use the ones we make.

(defvar *franz-malloced-arrays*
    (make-hash-table :rehash-size 2.0 :test #'equal))

;; ACL/WIN doesn't have anything equivalent to "static arrays",
;; so we must allocate this in C

(ct:defun-dll ilu_malloc
     ((size :unsigned-long))
     :entry-name "ilu_malloc"
     :library-name franz-kernel-dll
     :return-type (:char *))

(defun franz-raw-static-array-by-size (size)
  (let ((premade-arrays (gethash size *franz-malloced-arrays*)))
    (if premade-arrays
	(prog1
	    (pop premade-arrays)
	  (setf (gethash size *franz-malloced-arrays*) premade-arrays))
      (ilu_malloc size))))

(eval-when (compile load eval)
   (defun franz-c-type-size (type)
     (case type
	((:unsigned-char) 1)
	((:short :unsigned-short) 2)
	(:double-float 8)
	(otherwise 4))))

(defmacro franz-obtain-raw-static-array (type)
  `(franz-raw-static-array-by-size ,(franz-c-type-size type)))

(defmacro franz-obtain-static-array (type &optional (initial-element nil
						     initial-element-p))
  (if initial-element-p
      `(let ((array (franz-obtain-raw-static-array ,type)))
	 (ct:cset (,type 1) array 0 ,initial-element))
    `(franz-obtain-raw-static-array ,type)))

(defmacro franz-return-static-array (val type)
  `(push ,val (gethash ,(franz-c-type-size type) *franz-malloced-arrays*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  transform argument values as necessary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ct:defun-dll ilulisp_convert-byte-vector-to-c
	      ((bs (:unsigned-char *)) (len :unsigned-long))
	      :entry-name "ilulisp_ConvertByteVectorToC"
	      :library-name franz-lisp-dll
	      :return-type (:void *))

(ct:defun-dll ilulisp_convert-unicode-vector-to-c
	      ((ucs (:unsigned-short *)) (len :unsigned-long))
	      :entry-name "ilulisp_ConvertUnicodeVectorToC"
	      :library-name franz-lisp-dll
	      :return-type (:void *))

(ct:defun-dll ilulisp_byte-vector-size
	      ((bv (:void *)))
	      :entry-name "ilulisp_ByteVectorSize"
	      :library-name franz-lisp-dll
	      :return-type :unsigned-long)

(ct:defun-dll ilulisp_unicode-vector-size
	      ((uv (:void *)))
	      :entry-name "ilulisp_UnicodeVectorSize"
	      :library-name franz-lisp-dll
	      :return-type :unsigned-long)

(ct:defun-dll ilulisp_free-byte-vector
	      ((bv (:void *)))
	      :entry-name "ilulisp_FreeByteVector"
	      :library-name franz-lisp-dll
	      :return-type :void)

(ct:defun-dll ilulisp_free-unicode-vector
	      ((bv (:void *)))
	      :entry-name "ilulisp_FreeUnicodeVector"
	      :library-name franz-lisp-dll
	      :return-type :void)

(ct:defun-dll ilulisp_copy-byte-vector
	      ((bv (:void *)) (bs (:unsigned-char *)))
	      :entry-name "ilulisp_CopyByteVectorToLisp"
	      :library-name franz-lisp-dll
	      :return-type :void)

(ct:defun-dll ilulisp_copy-unicode-vector
	      ((bv (:void *)) (ucs (:unsigned-short *)))
	      :entry-name "ilulisp_CopyUnicodeVectorToLisp"
	      :library-name franz-lisp-dll
	      :return-type :void)

(ct:defun-dll ilufranz_strlen
	      ((str (:char *)))
	      :entry-name "ilufranz_Strlen"
	      :library-name franz-lisp-dll
	      :return-type :unsigned-long)

(ct:defun-dll ilufranz_memcpy
	      ((dest (:char *)) (src (:char *)) (len :unsigned-long))
	      :entry-name "ilufranz_Memcpy"
	      :library-name franz-lisp-dll
	      :return-type :void)

(ct:defun-dll ilufranz_free-cstruct
	      ((str (:void *)))
	      :entry-name "ilufranz_FreeCStruct"
	      :library-name franz-lisp-dll
	      :return-type :void)

(ct:defun-dll ilu_kerr->corba-error
	      ((kerr (:void *)) (major (:long *)))
	      :entry-name "ilu_CORBAizeSystemErr"
	      :library-name franz-kernel-dll
	      :return-type :unsigned-long)

(defun franz-make-corba-exception (kerr-pointer)
  (let ((major (franz-obtain-static-array :long)))
    (let ((minor (ilu_kerr->corba-error kerr-pointer major)))
      (make-condition
       (case (let ((return-value (ct:cref (:long 1) major 0)))
	       (franz-return-static-array major :long)
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
       :completion-status (default-completion-status)))))

;; converts a char* to a Lisp string, the passed-in pointer is not freed
(defun franz-char*->string (ptr)
  (let* ((len (ilufranz_strlen ptr))
	 (tmp (ct:callocate (:char *) :size len)))
    (ilufranz_memcpy tmp ptr len)
    (subseq tmp 0 len)))

;; converts a Lisp string to a char*, the char* points to a copy of
;; the string allocated on the C heap
(ct:defun-dll franz-string->char*
	      ((str (:char *)))
	      :entry-name "ilulisp_Strdup"
	      :library-name franz-lisp-dll
	      :return-type (:char *))

(defun franz-convert-byte-vector-from-c (bv)
  (let ((lisp-value
	 (make-array
	  (ilulisp_byte-vector-size bv) :element-type '(unsigned-byte 8))))
    (ilulisp_copy-byte-vector bv lisp-value)
    lisp-value))

(defun franz-convert-unicode-vector-from-c (bv)
  (let ((lisp-value
	 (make-array
	  (ilulisp_unicode-vector-size bv) :element-type '(unsigned-byte 16))))
    (ilulisp_copy-unicode-vector bv lisp-value)
    lisp-value))

(defmacro franz-transform-lisp-arg-value-to-defforeign-arg-value (type value)
  (let ((tvalue (gensym)))
    (case type
      (:boolean `(if ,value 1 0))
      (:string `(if ,value ,value (c-null-pointer)))
      ;; for a :constant-string we must create a copy because the C
      ;; function wants to own the string.
      (:constant-string `(if ,value
			     (franz-string->char* ,value)
			   (c-null-pointer)))
      (:bytes `(let ((,tvalue ,value))
		 (ilulisp_convert-byte-vector-to-c ,tvalue (length ,tvalue))))
      (:unicode `(let ((,tvalue ,value))
		   (ilulisp_convert-unicode-vector-to-c
		    ,tvalue (length ,tvalue))))
      (otherwise value))))

(defmacro franz-transform-lisp-arg-to-defforeign-arg (type dir value)
  (ecase dir
    (:in `(franz-transform-lisp-arg-value-to-defforeign-arg-value ,type ,value))
    (:inout
     `(franz-obtain-static-array ,(franz-defforeign-argument-type type)
	 (franz-transform-lisp-arg-value-to-defforeign-arg-value ,type ,value)))
    (:out
     `(franz-obtain-static-array ,(franz-defforeign-argument-type type)))))

(defmacro franz-transform-defforeign-value-to-lisp-value (type value)
  (case type
    (:boolean `(unless (zerop ,value) t))
    (:string `(unless (c-null-pointer-p ,value)
		(prog1
		    (franz-char*->string ,value)
		  (ilufranz_free-cstruct ,value))))
    (:constant-string `(unless (c-null-pointer-p ,value)
			 (franz-char*->string ,value)))
    (:bytes `(prog1
		 (franz-convert-byte-vector-from-c ,value)
	       (ilulisp_free-byte-vector ,value)))
    (:unicode `(prog1
		   (franz-convert-unicode-vector-from-c ,value)
		 (ilulisp_free-unicode-vector ,value)))
    (otherwise `,value)))

(defmacro franz-transform-defforeign-arg-value-to-lisp-value (type value)
  ;; only called in cases where value was :out or :inout
  `(let ((interior-value (ct:cref
			  (,(franz-defforeign-argument-type type) 1)
			  ,value
			  0)))
     (franz-return-static-array ,value ,(franz-defforeign-argument-type type))
     (franz-transform-defforeign-value-to-lisp-value ,type interior-value)))

(defmacro franz-maybe-free-defforeign-value (value type)
  (case type
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
;;
;; The argument type :ilu-kerr is allowed to appear as the last argument
;; in the argument list, if it appears it must appear without a
;; direction specifier. :ilu-kerr designates a C argument of type
;; (ilu_Error *). The define-c-macro generates code that handles
;; the (ilu_Error *) argument transparently, initializing it before
;; calling the C function, and checking for error on return. In case
;; of an error it signals a lisp condition. This argument is not
;; present in the generated lisp wrapper, meaning the lisp user
;; never sees it.

(defmacro define-c-function (lisp-name doc-string c-name args return-type
			     &key inline)
  (declare (ignore doc-string inline))
  (format t "~&; Defining ~s => ~s~%" lisp-name c-name)
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

       (ct:defun-dll
	,lisp-function-name
	,(mapcar #'(lambda (arg)
		     (list (gensym) (franz-defforeign-argument-type arg)))
		 args)
	:entry-name ,c-name
	:library-name ,(if (or (string= (subseq c-name 0 8) "ilulisp_")
			       (string= (subseq c-name 0 9) "ilufranz_"))
			   `franz-lisp-dll
			 `franz-kernel-dll)
	:return-type ,(franz-defforeign-return-type return-type))

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
			  `(unless (ilufranz_error-ok ,err)
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
;;;  Not available, so do nothing.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun optional-finalization-hook (x)
  (declare (ignore x)))

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

(define-c-function ilu_set-connection-request-handler
  "server then waits for connection requests to show up on the port"
  "ilu_SetConnectionRequestHandler"
  (:pointer :pointer :cardinal :ilu-kerr) :boolean)

(defvar *connection-callback-calling-function* nil)

(ct:defun-c-callback connection-callback-calling-function
		     ((index :unsigned-long))
  (let* ((callback-closure (lookup-registered-lisp-object index))
	 (fn (first callback-closure))
	 (args (cdr callback-closure))
	 (server (first args))
	 (port (second args)))
    (let ((status (apply fn args)))
      (if status
	  1
	(progn
	  ; (remove-connection-request-handler port index)
	  0)))))

(defun setup-new-connection-handler (fn server port)

  ;; when input is available on the PORT, call FN, passing FN SERVER
  ;; and PORT as args.
  (let ((index (register-lisp-object (list fn server port))))
    ; (format t "~%new-connection handler with index ~d registered~%" index)
    (ilu_set-connection-request-handler
     port *connection-callback-calling-function* index)))

(defun remove-connection-request-handler (port index)
  (unregister-lisp-object index)
  (ilu_set-connection-request-handler port (c-null-pointer) 0))

(define-c-function ilu_set-connection-input-handler
  "call this to set the input handler for the connection"
  "ilu_SetConnectionInputHandler"
  (:pointer :pointer :cardinal :ilu-kerr) :boolean)

(define-c-function ilu_clear-connection-input-handler
  "remove a connection handler"
  "ilu_ClearConnectionInputHandler"
  (:pointer :ilu-kerr) :boolean)

(defvar *input-callback-calling-function* nil)

(defvar *conn-index-alist* nil)
(defvar *index-closure-alist* nil)
(defvar *next-index* 1)

(defun register-handler (fn conn server)
  (let* ((old-index (cdr (assoc conn *conn-index-alist*)))
	 (index (if old-index old-index *next-index*)))
    (unless old-index
	    (setf *conn-index-alist* (acons conn index *conn-index-alist*))
	    (setf *next-index* (1+ *next-index*)))
    (setf *index-closure-alist*
	  (acons index (list fn conn server) *index-closure-alist*))
    index))

(defun unregister-handler (index)
  (setf *index-closure-alist*
	(remove (assoc index *index-closure-alist*) *index-closure-alist*)))

(defun lookup-handler (index)
  (cdr (assoc index *index-closure-alist*)))


(ct:defun-c-callback input-callback-calling-function
		     ((index :unsigned-long))
  (let ((callback-closure (lookup-handler index)))
    (if callback-closure
	(let* ((fn (first callback-closure))
	       (args (cdr callback-closure))
	       (conn (first args))
	       (server (second args)))
	  (remove-connection-input-handler conn index)
	  (let ((status (apply fn args)))
	    (if status
		(progn
		  (setup-watch-connection fn conn server)
		  1)
	      (progn
		;(remove-connection-input-handler conn index)
		0))))
      (error "input-callback-calling-function: invalid index~%"))))

(defun setup-watch-connection (fn conn server)
  (let ((index (register-handler fn conn server)))
    (ilu_set-connection-input-handler
     conn *input-callback-calling-function* index)))

(defun remove-connection-input-handler (conn index)
  (unregister-handler index)
  (ilu_set-connection-input-handler conn (c-null-pointer) 0))

;(defun setup-watch-connection (fn conn server)
;  ;; when a request comes in on a connection, we want to call
;  ;; FN to read and dispatch the request
;  (let ((index (register-lisp-object (list fn conn server))))
;    (ilu_set-connection-input-handler
;     conn *input-callback-calling-function* index)))

;(defun remove-connection-input-handler (conn index)
;  (unregister-lisp-object index)
;  (ilu_set-connection-input-handler conn (c-null-pointer) 0))

(defun init-io-callbacks ()
  (setf *connection-callback-calling-function*
    (ct:get-callback-procinst 'connection-callback-calling-function))
  (setf *input-callback-calling-function*
    (ct:get-callback-procinst 'input-callback-calling-function)))

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
  #+(or excl aclpc) `(code-char ,code)
  #-(or excl aclpc) `(declare (ignore ,code))
  #-(or excl aclpc) `(error "Unable to construct Unicode characters in this lisp"))

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
;;;   ;; Locking: L1 >= {server}; L1 >= {gcmu} if result is true and collectible
;;;   ;; L2, Main unconstrained
;;;   (object-of-ih-fn ILU-INSTANCE-HANDLE) => ilu:ilu-true-object
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

(ct:defun-c-callback franz-ot-object-of-ih
	   ((ot :unsigned-short) (cstring :unsigned-long))
  (declare (special *servers-inside*))
  (let ((oti (lookup-registered-lisp-object ot))
	(ih (franz-char*->string cstring)))
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

(ct:defun-c-callback franz-ot-free-self ((ot :unsigned-short))
  (let ((oti (lookup-registered-lisp-object ot)))
    (when oti
      (unregister-lisp-object ot)
      (funcall (second oti))))
  0)

(define-c-function ilufranz_setup-object-tables
    "Initialize the C part of the Franz object table system"
  "ilufranz_SetupObjectTables"
  (:pointer :pointer) :void)

;; ACL for Windows does not permit double return values, therefore,
;; use the kernel function directly, which uses an out parameter

(define-c-function ilu_input-real
  "unmarshal a double"
  "ilu_InputReal" (:ilu-call (:out :real) :ilu-kerr) :void)

(defmacro real-read (call)
  `(ilu_input-real ,call))

(define-c-function ilu_input-short-real
  "unmarshal a float"
  "ilu_InputShortReal" (:ilu-call (:out :short-real) :ilu-kerr) :void)

(defmacro short-real-read (call)
  `(ilu_input-short-real ,call))

(define-c-function ilufranz_allocate-c-handle
    "Create and return a C handle on a main loop frame"
  "ilufranz_AllocateMainLoopHandle" () :pointer)

(define-c-function ilu_run-main-loop
    "Call the ILU main loop and do event dispatching"
  "ilu_RunMainLoop" (:pointer) :void)

(define-c-function exit-main-loop
    "Call the ILU main loop and do event dispatching"
  "ilu_ExitMainLoop" (:pointer) :void)

(defun create-main-loop-handle ()
  (ilufranz_allocate-c-handle))

(defun run-main-loop (&optional user-handle)
  (let ((c-handle (or user-handle (ilufranz_allocate-c-handle))))
    (ilu_run-main-loop c-handle)))

;; a mechanism that empties the Windows event loop while blocked in select
(define-c-function ilufranz_setup-blocking-hook
    "Set up a blocking hook for Winsock"
  "ilufranz_SetupBlockingHook"
  () :void)

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
  (if *malloced-ilu-call-structs*
      (pop *malloced-ilu-call-structs*)
    (ilulisp_create-call-struct)))

(defmacro return-ilu-call-struct (val)
  `(push ,val *malloced-ilu-call-structs*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-locking ()
  (init-io-callbacks)
  (ilufranz_setup-object-tables
   (ct:get-callback-procinst 'franz-ot-object-of-ih)
   (ct:get-callback-procinst 'franz-ot-free-self))
  (ilufranz_setup-blocking-hook))
