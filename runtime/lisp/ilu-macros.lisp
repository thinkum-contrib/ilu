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

$Id: ilu-macros.lisp,v 1.84 1999/08/03 01:53:33 janssen Exp $
|#

(cl:in-package :ilu)

;;;; first, some generic code generators for operations on ILU types

(defmacro remove-nils (&rest body)
  `(remove nil ,@body :test #'eq))

(defun make-primitive-name (format-string type-name)
  (intern (funcall #'format nil format-string type-name)
	  (find-package "ILU")))

(defun make-constructed-name (format-string type-name)
  (let* ((package (symbol-package type-name))
	 (name
	  (intern (funcall #'format nil format-string type-name) package)))
    (unless (eq (symbol-package name) package)
      (warn "~S and ~S are not in the same package." type-name name))
    name))


(defun make-read-form (call type)
  (destructuring-bind (meta-type name) type
    (ecase meta-type
      (:primitive
       `(,(make-primitive-name "~A-READ" name) ,call))
      (:constructed
       `(,(make-constructed-name "~A-read" name) ,call))
      (:object
       `(object-read (find-class ',name) ,call ',name)))))

(defun make-write-form (call type obj)
  (destructuring-bind (meta-type name) type
    (ecase meta-type
      (:primitive
       `(,(make-primitive-name "~A-WRITE" name) ,call ,obj))
      (:constructed
       `(,(make-constructed-name "~A-write" name) ,call ,obj))
      (:object
       `(object-write ,call ,obj ',name)))))

(defun make-size-form (call type obj)
  (destructuring-bind (meta-type name) type
    (ecase meta-type
      (:primitive
       `(,(make-primitive-name "~A-SIZE" name) ,call ,obj))
      (:constructed
       `(,(make-constructed-name "~A-size" name) ,call ,obj))
      (:object
       `(object-size ,call ,obj ',name)))))



(defvar *primitive-types*
    ;;<ILU name>     <Lisp name>              <prototype form>	<C name>
    '((:null          null                    nil		"null")
      (:character     character               #\space		"character")
      (:short-character character             #\space		"short character")
      (:cardinal      (unsigned-byte 32)      0			"cardinal")
      (:short-cardinal (unsigned-byte 16)     0			"short cardinal")
      (:long-cardinal (unsigned-byte 64)      0			"long cardinal")
      (:integer       (signed-byte 32)        0			"integer")
      (:short-integer (signed-byte 16)        0			"short integer")
      (:long-integer  (signed-byte 64)	      0			"long integer")
      (:boolean	      (or nil t)	      't		"boolean")
      #+ilu-pickle
      (:pickle	      pickle
       #-aclpc		(clos:class-prototype (find-class 'pickle))
       #+aclpc		(allegro:class-prototype (find-class 'pickle))
								"pickle")
      (:real          double-float            0.0D0		"real")
      (:short-real    single-float            0.0		"short real")
      (:long-real     double-float	      0.0D0		"long real")
      (:byte          (unsigned-byte 8)       0			"byte")
      ))
                 
(defun type-prototype-form (type)
  (destructuring-bind (meta-type name) type
    (case meta-type
      (:primitive
       (third (assoc name *primitive-types*)))
      (:constructed
       `(get ',name 'prototype))
      (:object
       `(make-instance ',name)))))

(defun type-lisp-type (type)
  (destructuring-bind (meta-type name) type
    (case meta-type
      (:primitive (second (assoc name *primitive-types*)))
      ((:constructed :object) (second type)))))

(defun type-type-id (type)
  (ecase (car type)
    (:primitive
     (let ((typerec (assoc (cadr type) *primitive-types*)))
       (when typerec
	 (ilulisp_get-primitive-type-uid (fourth typerec)))))
    (:constructed
     (typecode-uid (typecode (cadr type))))
    (:object
     (typecode-uid (typecode (cadr type))))))

;;;; DEFINE-TYPE: emits a DEFTYPE

(defmacro define-primitive-type (name type)
  `(deftype ,name () ',(type-lisp-type type)))

;;;; constructed types

;;;; DEFINE-RECORD: emits a DEFSTRUCT and appropriate read, write & size code

(defun make-name (format-string &rest format-args)
  (let ((name (apply #'format nil format-string format-args)))
    (shadow (make-symbol name))		; shadow before intern at macroexpand
    (intern name)))			; time to avoid any conflicts

(defun export-names (names)
  `(eval-when (compile eval load)	; shadow before intern at load time too
     (shadow ',(mapcar #'(lambda (s) (make-symbol (symbol-name s))) names))
     (export ',names)))

(defmacro define-record-type (name &rest slots)
  (let* ((name-string (symbol-name name))
	 (slot-names (mapcar #'first slots))
	 (slot-types (mapcar #'second slots))
	 (accessors
	  (mapcar #'(lambda (name) (make-name "~A-~A" name-string name))
		  slot-names))
	 (constructor (make-name "MAKE-~A" name-string))
	 (predicate (make-name "~A-P" name-string)))
    `(progn
       ,(export-names (list* constructor predicate accessors))
       (defstruct ,name
	 ,@(mapcar
	    #'(lambda (name type)
		`(,name ,(type-prototype-form type)
			; :type ,(type-lisp-type type)
			))
	    slot-names slot-types)))))

(defmacro define-record (name uid isl-name ifc-name ifc-brand extensible-p supertype-uid &rest slots)
  #-ilu-type-info
  (declare (ignore isl-name ifc-name ifc-brand))
  (let* ((name-string (symbol-name name))
	 (slot-names (mapcar #'first slots))
	 (slot-keys (mapcar
		     #'(lambda (name)
			 (intern (symbol-name name) (find-package :keyword)))
		     slot-names))
	 (slot-types (mapcar #'second slots))
	 (accessors
	  (mapcar #'(lambda (name) (make-name "~A-~A" name-string name))
		  slot-names))
	 (constructor (make-name "MAKE-~A" name-string))
	 (read-fn (intern (format nil "~A-read" name-string)))
	 (write-fn (intern (format nil "~A-write" name-string)))
	 (size-fn (intern (format nil "~A-size" name-string))))
    `(progn
       (setf (get ',name 'prototype) (,constructor))
       (setf (get ',name 'type-id) ,uid)
       (defun ,read-fn (call)
	 (prog1 (when (record-read call)
		  (,constructor
		   ,@(mapcan #'(lambda (key type)
			  `(,key ,(make-read-form 'call type)))
			     slot-keys slot-types)))
	   (record-end call)))
       (defun ,write-fn (call record)
	 (record-write call record)
	 ,@(mapcar
	    #'(lambda (type accessor)
		(make-write-form 'call type `(,accessor record)))
	    slot-types accessors)
	 (record-end call)
	 record)
       (defun ,size-fn (call record)
	 (prog1 (+ (record-size call record)
		   (+ ,@(mapcar
			 #'(lambda (type accessor)
			     (make-size-form 'call type `(,accessor record)))
			 slot-types accessors)))
	   (record-end call)))
       (add-type ,uid ',name #',size-fn #',write-fn #',read-fn)
       #+ilu-type-info
       (let ((fn #'(lambda()
		     (with-type-mutex
			 (multiple-value-bind (record newp)
			     (ilu_register-record-type
			      ,isl-name ,ifc-name ,ifc-brand ,uid ,(length slots)
			      ,extensible-p ,supertype-uid)
			   (when newp
			     (let ((index 0))
			       (dolist (field ',slots)
				 (ilu_register-record-field
				  record index (third field) (fourth field))
				 (incf index)))))))))
	 (if *ilu-initialized*
	     (funcall fn)
	   (push fn *types-to-be-registered*)))
       )))


;;;; DEFINE-ENUMERATION: enumerations are symbols, mapped to numbers on wire

(defmacro define-enumeration-type (name &rest specs)
  (let* ((name-string (symbol-name name))
	 (names (mapcar #'first specs))
	 (constant (intern (format nil "+~A-ENUMERATION+" name-string))))
    `(progn
       (defconstant ,constant ',specs)
       (deftype ,name () '(member ,@names))
       (setf (get ',name 'prototype) ',(first names)))))

(defmacro define-enumeration (name uid isl-name ifc-name ifc-brand &rest specs)
  #-ilu-type-info
  (declare (ignore isl-name ifc-name ifc-brand specs))
  (let* ((name-string (symbol-name name))
	 (constant (intern (format nil "+~A-ENUMERATION+" name-string)))
	 (read-fn (intern (format nil "~A-read" name-string)))
	 (write-fn (intern (format nil "~A-write" name-string)))
	 (size-fn (intern (format nil "~A-size" name-string))))
    `(progn
       (setf (get ',name 'type-id) ,uid)
       (defun ,read-fn (call)
	 (let ((key (enumeration-entry-read call)))
	   (when key
	     (first (find key ,constant :test #'= :key #'second)))))
       (defun ,write-fn (call symbol)
	 (enumeration-entry-write
	  call
	  (let ((entry (assoc symbol ,constant)))
	    (if entry
		(second entry)
	      (error "~A not a valid ~A value" symbol ',name))))
	 symbol)
       (defun ,size-fn (call symbol)
	 (enumeration-entry-size
	  call
	  (let ((entry (assoc symbol ,constant)))
	    (if entry
		(second entry)
	      (error "~A not a valid ~A value" symbol ',name)))))
       (add-type ,uid ',name #',size-fn #',write-fn #',read-fn)
       #+ilu-type-info
       (let ((fn #'(lambda()
		     (with-type-mutex
			 (multiple-value-bind (type newtype)
			     (ilu_register-enumeration-type
			      ,isl-name ,ifc-name ,ifc-brand ,uid
			      ,(length specs))
			   (when newtype
			     (let ((index 0))
			       (dolist (spec ',specs)
				 (ilu_register-enumeration-type-field
				  type index (caddr spec) (cadr spec))
				 (incf index)))))))))
	 (if *ilu-initialized*
	     (funcall fn)
	   (push fn *types-to-be-registered*))))))


;;;; DEFINE-SEQUENCE: sequences are represented by lists

(defmacro define-sequence-type (name type limit)
  (declare (ignore limit))
  `(progn
     ,@(cond 
	((and (eq (first type) :primitive)
	      (eq (second type) :short-character))
	 `((deftype ,name () 'simple-string)
	   (setf (get ',name 'prototype) "")))
	((and (eq (first type) :primitive)
	      (eq (second type) :character))
	 `((deftype ,name () 'simple-string)
	   (setf (get ',name 'prototype) "")))
	((and (eq (first type) :primitive) (eq (second type) :byte))
	 `((deftype ,name () '(simple-array (unsigned-byte 8) (*)))
	   (setf (get ',name 'prototype)
	     (make-array 0 :element-type '(unsigned-byte 8)))))
	(t `((deftype ,name () 'list)
	     (setf (get ',name 'prototype) '()))))))

(defmacro define-sequence (name uid type limit isl-name interface-name interface-brand content-type-uid)
  #-ilu-type-info
  (declare (ignore isl-name interface-name interface-brand content-type-uid))
  (let* ((name-string (symbol-name name))
	 (read-fn (intern (format nil "~A-read" name-string)))
	 (write-fn (intern (format nil "~A-write" name-string)))
	 (size-fn (intern (format nil "~A-size" name-string))))
    `(progn
       (setf (get ',name 'type-id) ,uid)
       ,@(cond 
	  ((and (eq (first type) :primitive)
		(eq (second type) :short-character))
	   `((defun ,read-fn (call)
	       (string-read call ,limit))
	     (defun ,write-fn (call s)
	       (string-write call s ,limit))
	     (defun ,size-fn (call s)
	       (string-size call s ,limit))
	     (add-type ,uid ',name #',size-fn #',write-fn #',read-fn)
	     ))
	  ((and (eq (first type) :primitive)
		(eq (second type) :character))
	   `((defun ,read-fn (call)
	       (unicode-string-read call ,limit))
	     (defun ,write-fn (call s)
	       (unicode-string-write call s ,limit))
	     (defun ,size-fn (call s)
	       (unicode-string-size call s ,limit ))
	     (add-type ,uid ',name #',size-fn #',write-fn #',read-fn)
	     ))
	  ((and (eq (first type) :primitive) (eq (second type) :byte))
	   `((defun ,read-fn (call)
	       (byte-sequence-read call ,limit))
	     (defun ,write-fn (call v)
	       (byte-sequence-write call v ,limit))
	     (defun ,size-fn (call v)
	       (byte-sequence-size call v ,limit))
	     (add-type ,uid ',name #',size-fn #',write-fn #',read-fn)
	     ))
	  (t `((defun ,read-fn (call)
		 (let ((value '())
		       (count (sequence-read call ,limit)))
		   (prog1 (when count
			    (dotimes (i count (nreverse value))
			      (push ,(make-read-form 'call type) value)))
		     (sequence-end call))))
	       (defun ,write-fn (call list)
		 (sequence-write call list ,limit)
		 (dolist (entry list)
		   ,(make-write-form 'call type 'entry))
		 (sequence-end call)
		 list)
	       (defun ,size-fn (call list)
		 (let ((size (sequence-size call list ,limit)))
		   (prog1 (dolist (entry list size)
			    (incf size ,(make-size-form 'call type 'entry)))
		     (sequence-end call))))
	       (add-type ,uid ',name #',size-fn #',write-fn #',read-fn)
	       )))
       #+ilu-type-info
       (if *ilu-initialized*
	   (with-type-mutex
	       (ilu_register-sequence-type ,isl-name ,interface-name ,interface-brand ,uid
					   ,content-type-uid ,limit))
	 (push #'(lambda ()
		   (with-type-mutex
		       (ilu_register-sequence-type
			,isl-name ,interface-name ,interface-brand ,uid
			,content-type-uid ,limit)))
	       *types-to-be-registered*))		   
       )))

;;;; DEFINE-ARRAY: vectors of characters are represented as strings, and
;;;; vectors of bytes are represented as arrays.  Other arrays are arrays.

(defmacro define-array-type (name type &rest dimensions)
  `(progn
     ,@(cond 
	((and (eq (first type) :primitive)
	      (eq (second type) :short-character)
	      (= (length dimensions) 1))
	 `((deftype ,name () '(simple-string ,(car dimensions)))
	   (setf (get ',name 'prototype) (make-string ,(car dimensions)))))
	((and (eq (first type) :primitive)
	      (eq (second type) :character)
	      (= (length dimensions) 1))
	 `((deftype ,name () '(simple-string ,(car dimensions)))
	   (setf (get ',name 'prototype) (make-string ,(car dimensions)))))
	((and (eq (first type) :primitive)
	      (eq (second type) :byte)
	      (= (length dimensions) 1))
	 `((deftype ,name ()
	     '(simple-array (unsigned-byte 8) (,(car dimensions))))
	   (setf (get ',name 'prototype)
	     (make-array ,(car dimensions)
			 :element-type '(unsigned-byte 8)))))
	(t
	 `((deftype ,name ()
	     '(simple-array ,(type-lisp-type type) ,dimensions)))))))

(defmacro define-array (name uid type 
			isl-name interface-name interface-brand base-type-uid
			&rest dimensions)
  #-ilu-type-info
  (declare (ignore isl-name interface-name interface-brand base-type-uid))
  (let* ((name-string (symbol-name name))
	 (read-fn (intern (format nil "~A-read" name-string)))
	 (write-fn (intern (format nil "~A-write" name-string)))
	 (size-fn (intern (format nil "~A-size" name-string))))
    `(progn
       (setf (get ',name 'type-id) ,uid)
       ,@(cond 
	  ((and (eq (first type) :primitive)
		(eq (second type) :short-character)
		(= (length dimensions) 1))
	   `((defun ,read-fn (call)
	       (char-vector-read call ,(car dimensions)))
	     (defun ,write-fn (call s)
	       (char-vector-write call s ,(car dimensions)))
	     (defun ,size-fn (call s)
	       (char-vector-size call s ,(car dimensions)))
	     (add-type ,uid ',name #',size-fn #',write-fn #',read-fn)
	     ))
	  ((and (eq (first type) :primitive)
		(eq (second type) :character)
		(= (length dimensions) 1))
	   `((defun ,read-fn (call)
	       (unicode-char-vector-read
		call ,(car dimensions)))
	     (defun ,write-fn (call s)
	       (unicode-char-vector-write
		call s ,(car dimensions)))
	     (defun ,size-fn (call s)
	       (unicode-char-vector-size
		call s ,(car dimensions)))
	     (add-type ,uid ',name #',size-fn #',write-fn #',read-fn)
	     ))
	  ((and (eq (first type) :primitive)
		(eq (second type) :byte)
		(= (length dimensions) 1))
	   `((defun ,read-fn (call)
	       (byte-vector-read call ,(car dimensions)))
	     (defun ,write-fn (call v)
	       (byte-vector-write call v ,(car dimensions)))
	     (defun ,size-fn (call v)
	       (byte-vector-size call v ,(car dimensions)))
	     (add-type ,uid ',name #',size-fn #',write-fn #',read-fn)
	     ))
	  ((and (eq (first type) :primitive)
		(member (second type) '(:short-character :byte :character)))
	   `((defun ,read-fn (call)
	       (when (array-read call)
		 (let ((value
			(make-array ',dimensions
				    :element-type ',(type-lisp-type type))))
		   ,(labels ((reader (vars dims)
			       (if (null (cdr dims))
				   (ecase (second type)
				     (:short-character
				      `(ilu-lisp_char-vector-read
					call value (+ ,@(cdr vars)) ,(car dims)))
				     (:byte
				      `(ilu-lisp_byte-vector-read
					call value (+ ,@(cdr vars)) ,(car dims)))
				     (:character
				      `(ilu-lisp_unicode-char-vector-read
					call value (+ ,@(cdr vars)) ,(car dims))))
				 `(dotimes (,(cadar vars) ,(car dims))
				    ,(reader
				      (cons (list '* (gensym) (cadr dims))
					    vars)
				      (cdr dims))))))
		      (reader (list (list '* (gensym) (car dimensions)))
			      dimensions))
		   (array-end call)
		   value)))
	     (defun ,write-fn (call array)
	       (array-write call array)
	       ,(labels ((writer (vars dims)
			   (if (null (cdr dims))
			       (ecase (second type)
				 (:short-character
				  `(ilu-lisp_char-vector-write
				    call array (+ ,@(cdr vars)) ,(car dims)))
				 (:byte
				  `(ilu-lisp_byte-vector-write
				    call array (+ ,@(cdr vars)) ,(car dims)))
				 (:character
				  `(ilu-lisp_unicode-char-vector-write
				    call array (+ ,@(cdr vars)) ,(car dims))))
			     `(dotimes (,(cadar vars) ,(car dims))
				,(writer
				  (cons (list '* (gensym) (cadr dims)) vars)
				  (cdr dims))))))
		  (writer (list (list '* (gensym) (car dimensions)))
			  dimensions))
	       (array-end call)
	       array)
	     (defun ,size-fn (call)
	       (let ((size (array-size call array)))
		 ,(labels ((sizer (vars dims)
			     (if (null (cdr dims))
				 `(incf size
					,(ecase (second type)
					   (:short-character
					    `(ilu-lisp_char-vector-size
					      call array (+ ,@(cdr vars))
					      ,(car dims)))
					   (:byte
					    `(ilu-lisp_byte-vector-size
					      call array (+ ,@(cdr vars))
					      ,(car dims)))
					   (:character
					    `(ilu-lisp_unicode-char-vector-size
					      call array (+ ,@(cdr vars))
					      ,(car dims)))))
			       `(dotimes (,(cadar vars) ,(car dims))
				  ,(sizer
				    (cons (list '* (gensym) (cadr dims)) vars)
				    (cdr dims))))))
		    (sizer (list (list '* (gensym) (car dimensions)))
			   dimensions))
		 (array-end call)
		 size))
	     (add-type ,uid ',name #',size-fn #',write-fn #',read-fn)
	     ))
	  (t
	   `((setf (get ',name 'prototype)
	       (make-array ',dimensions
			   :element-type ',(type-lisp-type type)))
	     (defun ,read-fn (call)
	       (prog1 (when (array-read call)
			(let ((value
			       (make-array
				',dimensions
				:element-type ',(type-lisp-type type))))
			  ,(labels
			       ((reader (vars dims)
				  `(dotimes (,(car vars) ,(car dims))
				     ,(if (null (cdr dims))
					  `(setf (aref value ,@(reverse vars))
					     ,(make-read-form 'call type))
					(reader (cons (gensym) vars)
						(cdr dims))))))
			     (reader (list (gensym)) dimensions))
			  value))
		 (array-end call)))
	     (defun ,write-fn (call array)
	       (array-write call array)
	       ,(labels ((writer (vars dims)
			   `(dotimes (,(car vars) ,(car dims))
			      ,(if (null (cdr dims))
				   (make-write-form
				    'call type `(aref array
						      ,@(reverse vars)))
				 (writer (cons (gensym) vars)
					 (cdr dims))))))
		  (writer (list (gensym)) dimensions))
	       (array-end call)
	       array)
	     (defun ,size-fn (call array)
	       (let ((size (array-size call array)))
		 ,(labels ((sizer (vars dims)
			     `(dotimes (,(car vars) ,(car dims))
				,(if (null (cdr dims))
				     `(incf
				       size
				       ,(make-size-form
					 'call type
					 `(aref array ,@(reverse vars))))
				   (sizer (cons (gensym) vars)
					  (cdr dims))))))
		    (sizer (list (gensym)) dimensions))
		 (array-end call)
		 size))
	     (add-type ,uid ',name #',size-fn #',write-fn #',read-fn)
	     )))
       #+ilu-type-info
       (if *ilu-initialized*
	   (register-array-type ,isl-name ,interface-name ,interface-brand
				,uid ,base-type-uid ',dimensions)
	 (push #'(lambda ()
		   (register-array-type
		    ,isl-name ,interface-name ,interface-brand
		    ,uid ,base-type-uid ',dimensions))
	       *types-to-be-registered*))
       )))


;;;; DEFINE-UNION: use lisp's run-time typing to distinguish union types

(defmacro do-clauses ((index item list) &body body)
  (let ((tail (gensym)) (result (gensym)))
    `(do ((,result '() (let ((,item (car ,tail))) (cons ,@body ,result)))
	  (,index 0 (1+ ,index))
	  (,tail ,list (cdr ,tail)))
	 ((null ,tail) (nreverse ,result)))))

(defmacro define-union-type (name disc-type bogus-values-allowed-p
			     default-type &rest types)
  (declare (ignore bogus-values-allowed-p disc-type default-type types))
  (let ((check-fn-name
	 (intern (concatenate 'string (symbol-name `,name) "-P"))))
    `(deftype ,name () '(satisfies ,check-fn-name))))

(defun int-to-union-discriminator (dtype val)
  (cond
   ((and (eq (first dtype) :primitive)
	 (member (second dtype)
		 '(:cardinal :integer :short-cardinal :short-integer
		   :byte)))
    val)
   ((and (eq (first dtype) :primitive)
	 (eq (second dtype) :boolean))
    (not (= 0 val)))
   ((eq (first dtype) :constructed)
    ;; must be enumeration
    (let* ((values-list
	    (symbol-value
	     (intern
	      (format nil "+~a-ENUMERATION+"
		      (symbol-name (second dtype)))		      
	      (find-package (package-name (symbol-package (second
							   dtype))))
	      )))
	   (value (find val values-list :test #'= :key #'second)))
      (first value)))
   (t
    (error "Invalid discriminant type for union passed:  ~s." dtype))))

(defun union-discriminator-to-int (dtype val)
  (cond
   ((eq (first dtype) :primitive)
    (ecase (second dtype)
      ((:cardinal :integer :short-cardinal :short-integer :byte)
       (coerce val 'fixnum))
      (:boolean
       (if val 1 0))))
   ((eq (first dtype) :constructed)
    ;; must be enumerated type, so val->int function exists
    (let* ((values-list
	    (symbol-value
	     (intern
	      (format nil "+~a-ENUMERATION+"
		      (symbol-name (second dtype)))		      
	      (find-package (package-name (symbol-package (second
							   dtype))))
	      )))
	   (value (find val values-list :test #'eq :key #'first)))
      (second value)))
   (t
    (error "Invalid discriminant type for union passed:  ~s." dtype))))

(defmacro define-union (name uid disc-type bogus-values-allowed-p
			default-type isl-name ifc-name ifc-brand
			discriminant-type-uid discriminant-type-kind
			default-arm-index &rest arms)
  #-ilu-type-info
  (declare (ignore isl-name ifc-name ifc-brand discriminant-type-uid
		   default-arm-index))
  (let* ((name-string (symbol-name name))
	 (read-fn (intern (format nil "~A-read" name-string)))
	 (write-fn (intern (format nil "~A-write" name-string)))
	 (size-fn (intern (format nil "~A-size" name-string)))
	 (check-fn-name
	  (intern (concatenate 'string name-string "-P")))
	 (types (mapcar #'second arms)))
    (when default-type
      (push default-type types))
    `(progn
       (setf (get ',name 'prototype) ,(type-prototype-form (first types)))
       (setf (get ',name 'type-id) ,uid)
       (defun ,check-fn-name (x)
	 (and (consp x)
	      (typep (car x) ',(type-lisp-type disc-type))
	      (case ,(if (equal (second disc-type) :boolean)
			 `(if (car x) t nil)
		       `(car x))
		,@(do-clauses (i type arms)
		    `(,(first type) (typep (cdr x)
					   ',(type-lisp-type (second
							     type)))))
		,@(if default-type
		      `((t (typep (cdr x) ',(type-lisp-type default-type))))
		    (if bogus-values-allowed-p
			'((t t))
		      `((t nil)))))))
       (defun ,read-fn (call)
	 (let ((_disc-val (int-to-union-discriminator ',disc-type
			   (ilulisp_input-union call
			    ,discriminant-type-kind (c-null-pointer)))))
	   (prog1
	       (cons _disc-val
		     (,(if (or default-type bogus-values-allowed-p)
			   'case
			 'ecase) _disc-val
			 ,@(do-clauses (i type arms)
			     `(,(first type) ,(make-read-form 'call (second
								     type))))
		       ,@(if default-type
			     `((t ,(make-read-form 'call default-type)))
			   (if bogus-values-allowed-p
			       '((t (values)))
			     (values)))
		       ))
	     (union-end call))))
       (defun ,write-fn (call value)
	 (progn
	   (ilu_union-write call
	      (union-discriminator-to-int ',disc-type (car value))
	      ,discriminant-type-kind (c-null-pointer))
	   (,(if (or default-type bogus-values-allowed-p) 'case 'ecase)
	    ,(if (equal (second disc-type) :boolean)
		 `(if (car value) t nil)
	       `(car value))
	    ,@(do-clauses (i type arms)
			  `(,(first type)
			    ,(make-write-form 'call (second type) '(cdr
								    value))))
	    ,@(if default-type
		  `((t ,(make-write-form 'call default-type '(cdr value))))
		(values)))
	   (union-end call))
	 value)
       (defun ,size-fn (call value)
	 (prog1
	     (+ (ilu_union-size call
		  (union-discriminator-to-int ',disc-type (car value))
		  ,discriminant-type-kind (c-null-pointer))
		(,(if (or default-type bogus-values-allowed-p)
		      'case 'ecase)
		 ,(if (equal (second disc-type) :boolean)
		      `(if (car value) t nil)
		    `(car value))
		  ,@(do-clauses (i type arms)
				`(,(first type)
				  ,(make-size-form 'call (second type)
						   '(cdr value))))
		  ,@(if default-type
			`((t ,(make-size-form 'call
					      default-type
					      '(cdr value))))
			(values))
		  ,@(if bogus-values-allowed-p
			`((t 0))
			(values))))
	   (union-end call)))
       (add-type ,uid ',name #',size-fn #',write-fn #',read-fn)
       #+ilu-type-info
       (if *ilu-initialized*
	   (register-union-type ,isl-name ,ifc-name ,ifc-brand ,uid
				,discriminant-type-uid
				,discriminant-type-kind ,default-arm-index
				,bogus-values-allowed-p ',arms)
	 (push #'(lambda ()
		   (register-union-type ,isl-name ,ifc-name ,ifc-brand ,uid
					,discriminant-type-uid
					,discriminant-type-kind ,default-arm-index
					,bogus-values-allowed-p ',arms))
	       *types-to-be-registered*))
       )))


;;;; DEFINE-OPTIONAL

(defmacro define-optional-type (name type)
  `(deftype ,name () '(or nil ,(type-lisp-type type))))

(defmacro define-optional (name uid type isl-name ifc-name ifc-brand base-type-uid)
  (let* ((name-string (symbol-name name))
	 (read-fn (intern (format nil "~A-read" name-string)))
	 (write-fn (intern (format nil "~A-write" name-string)))
	 (size-fn (intern (format nil "~A-size" name-string))))
    `(setf (get ',name 'prototype) ,(type-prototype-form type))
    `(setf (get ',name 'type-id) ,uid)
    `(progn
       (defun ,read-fn (call)
	 (when (optional-read call (c-null-pointer))
	   ,(make-read-form 'call type)))
       (defun ,write-fn (call value)
	 (optional-write call (not (null value)) (c-null-pointer))
	 (when value
	   ,(make-write-form 'call type 'value))
	 value)
       (defun ,size-fn (call value)
	 (if value
	     (+ (optional-size call t (c-null-pointer))
		,(make-size-form 'call type 'value))
	   (optional-size call nil (c-null-pointer))))
       (add-type ,uid ',name #',size-fn #',write-fn #',read-fn)
       #+ilu-type-info
       (if *ilu-initialized*
	   (with-type-mutex
	       (ilu_register-optional-type ,isl-name ,ifc-name ,ifc-brand ,uid ,base-type-uid))
	 (push #'(lambda ()
		   (with-type-mutex
		       (ilu_register-optional-type ,isl-name ,ifc-name ,ifc-brand ,uid ,base-type-uid)))
	       *types-to-be-registered*)))
    ))


;;;; DEFINE-ALIAS and DEFINE-ALIAS-TYPE

(defmacro define-alias-type (name true-type)
  (declare (ignore uid))
  `(deftype ,name () '(,(type-lisp-type true-type))))

(defmacro define-alias (name uid true-type isl-name ifc-name ifc-brand base-type-uid)
  `(progn
     (setf (get ',name 'type-id) ,uid)
     (add-alias ,uid ',name ',true-type)
     #+ilu-type-info
     (if *ilu-initialized*
	 (with-type-mutex
	     (ilu_register-alias-type ,isl-name ,ifc-name ,ifc-brand ,uid ,base-type-uid))
       (push #'(lambda()
		 (with-type-mutex
		     (ilu_register-alias-type ,isl-name ,ifc-name ,ifc-brand ,uid ,base-type-uid)))
	     *types-to-be-registered*))))


(defstruct (method-spec (:type list) (:conc-name ms-))
  c-name name id functional-p asynchronous-p return-type args exceptions)

(defun dig-out-class-info (info &optional (info-type :all))
  (if (eq info-type :all)
      info
    (let ((the-info-cell (assoc info-type info)))
      (if the-info-cell
	  (cdr the-info-cell)
	(error "Invalid ILU class info item ~s" info-type)))))

(defun dig-out-method-info (info &optional (info-type :all))
  (if (eq info-type :all)
      info
    (ecase info-type
      (:c-name (ms-c-name info))
      (:lisp-name (ms-name info))
      (:index (ms-id info))
      (:functional-p (ms-functional-p info))
      (:asynchronous-p (ms-asynchronous-p info))
      (:return-type (ms-return-type info))
      (:arguments (ms-args info))
      (:exceptions (ms-exceptions info)))))

(defmacro define-class-type (name supers superclass-ids singleton-info
			     authentication brand optional-p collectible-p
			     methods ilu-version isl-name ifc-name ifc-brand
			     type-uid doc-string)
  (let ((the-info
	 `'((:brand . ,brand)
	    (:ilu-version . ,ilu-version)
	    (:superclasses . ,supers)
	    (:superclass-ids . ,superclass-ids)
	    (:authentication . ,authentication)
	    (:optional-p . ,optional-p)
	    (:collectible-p . ,collectible-p)
	    (:singleton . ,singleton-info)
	    (:methods . ,methods)
	    (:name . ,(format nil "~a.~a" ifc-name isl-name))
	    (:id . ,type-uid)
	    (:isl-name . ,isl-name)
	    (:interface-name . ,ifc-name)
	    (:interface-brand . ,ifc-brand)
	    (:doc-string . ,doc-string))))
    `(progn
       (setf (get ',name 'type-id) ,type-uid)
       (defclass ,name
	   ,(or supers
	     (if singleton-info '(ilu-singleton-object) '(ilu-object)))
	   (,@(mapcan #'(lambda (method)
			  (when (and (ms-functional-p method)
				     (ms-return-type method)
				     (null (ms-args method)))
			    `((,(ms-name method)))))
		      methods))
	 ,@(when (> (length doc-string) 0)
	     `((:documentation ,doc-string)))
	 )
       (defmethod ilu-class-info
	      ((classname (eql ',name)) &optional (info-type :all))
	 (dig-out-class-info ,the-info info-type))
       (defmethod ilu-class-info ((self ,name) &optional (info-type :all))
	 (dig-out-class-info ,the-info info-type))
       (add-type ,type-uid ',name
		 #'(lambda (call value)
		     (object-size call value ',name))
		 #'(lambda (call value)
		     (object-write call value ',name))
		 #'(lambda (call)
		     (object-read ',name call ',name)))
       )))

(defmacro define-class (name &optional c-class-get-fn)
  (declare (ignore c-class-get-fn))
  `(progn

     (defmethod ilu-class-record ((classname (eql ',name)))
       (gethash (find-class ',name) *lisp-to-ilu-class-table*))
     (defmethod ilu-class-name ((classname (eql ',name)))
       (ilu-class-info ',name :name))

     #+(and excl svr4)
     (clos:finalize-inheritance (clos::find-class ',name))

     (defmethod ilu-class-id ((class ,name))
       (ilu-class-info ',name :id))

     (initialize-ilu-class ',name (ilu-class-info ',name))))



;;;; DEFINE-EXCEPTION: expands to DEFINE-CONDITION plus

(defmacro define-exception-type (name repository-id fields docstring)
  `(progn
     (define-condition
       ,(intern (format nil "~A" name))
       (rpc-exception)
       (,@(if fields
	      (mapcar #'(lambda
			    (field) `(,(car field)
				      :type ,(type-lisp-type (cadr
							      field))))
		      fields)
	    ))
       (:documentation ,docstring))
     (setf (gethash ,repository-id *exception-repository-ids*)
       ',(intern (format nil "~A" name)))
     ))

(defmacro define-exception (name fields)
  (when fields
    `(progn
       (defmethod exception-value-read (call (self ,(intern (format nil
							     "~A" name))))
	 (progn
	   ,@(mapcar #'(lambda (field)
			 `(setf (slot-value self ',(car field))
			    ,(make-read-form 'call (cadr field))))
		     fields)))
       (defmethod exception-value-size (call (self ,(intern (format nil
								    "~A" name))))
	 (+ ,@(mapcar #'(lambda (field)
			  (make-size-form 'call (cadr field)
					  `(slot-value self
						       ',(car field))))
		      fields)))
       (defmethod exception-value-write (call (self ,(intern (format nil
								     "~A" name))))
	 (progn
	   ,@(mapcar #'(lambda (field)
			 (make-write-form 'call (cadr field)
					  `(slot-value self
						       ',(car field))))
		     fields)))
	 )))


;;;; DEFINE-METHOD: expands into a defmethod

(defmacro define-method (name old-name id singleton-p functional-p asynchronous-p
			 args exceptions return-type)
  (destructuring-bind ((self self-type) &rest other-args) args
    (declare (ignore self))
    #-ilu-old-method-names (declare (ignore old-name))
    (let ((other-arg-names (mapcar #'first other-args))
	  (other-arg-types (mapcar #'second other-args))
	  (other-arg-dirs (mapcar #'third other-args))
	  (other-arg-siblings (mapcar #'fifth other-args))
	  (has-return-values (or return-type
				 (dolist (dir (mapcar #'third other-args) nil)
				   (when (or (eq dir :inout) (eq dir :out))
				     (return t)))))
	  (self (gensym))
	  (class-record (gensym))
	  (call (gensym))
	  (status (gensym))
	  (exception-index (gensym))
	  (retry (gensym))
	  (new-conn (gensym)))
      `(progn
	 (defmethod ,name
	     ,(if (symbolp name)
		  ;; normal method
		  `((,self ,(second self-type))
		    ,@(remove-nils
		       (mapcar #'(lambda (dir name)
				   (unless (eq dir :out) name))
			       other-arg-dirs other-arg-names)))
		;; setf method, must reverse arguments
		`(,(first other-arg-names)
		  (,self ,(second self-type))))
	     ,(when (and functional-p return-type (null other-args))
		`(when (slot-boundp ,self ',name)
		   (return-from ,name (apply #'values (slot-value ,self ',name)))))
	   ,@(remove-nils
	      (mapcar #'(lambda (dir name sibling-p)
			  (if (and sibling-p (or (eq dir :in) (eq dir :inout)))
			      `(assert (string= (server-id ,self) (server-id ,name))
				   (,self ,name)
				 "Argument ~a must be a SIBLING of ~a~%" ,name ,self)))
		      other-arg-dirs other-arg-names other-arg-siblings))
	   (let ((,call (obtain-ilu-call-struct))
		 (,class-record (ilu-class-record ',(second self-type))))
	     ;; we have taken a call struct,
	     ;; use unwind-protect to ensure that it is returned
	     (unwind-protect
		 (multiple-value-bind (,status ,new-conn)
		     (ilu_start-call
		      ,call (ilu-server ,self) ,class-record
		      (rpc-method ,self ,class-record ,id)
		      (my-language-index) (c-null-pointer) ;really passport
		      )
		   ;; ilu_start-call has succeeded, use unwind-protect
		   ;; to ensure that finish-call will be called
		   (declare (ignore ,status))
		   (unwind-protect
		       (multiple-value-bind (,status ,exception-index)
			   (loop
			     (unless (c-null-pointer-p ,new-conn)
			       (watch-outgoing-connection ,new-conn))
			     (ilu_start-request
			      ,call
			      (+ ,(if singleton-p
				      0
				    `(object-id-size
				      ,call (kernel-obj ,self) 1 ,class-record
				      (cons (ilu-server ,self) (ilu-class ,self))
				      ))
				 ,@(remove-nils
				    (mapcar #'(lambda (dir type name)
						(if (or (eq dir :in) (eq dir :inout))
						    (make-size-form call type name)))
					    other-arg-dirs other-arg-types
					    other-arg-names))
				 ))
			     ,@(unless singleton-p
				 `((object-id-write
				    ,call (kernel-obj ,self) 1 ,class-record)))
			     ;; note that object-id-write releases the server mutex,
			     ;; so now not Inside the server
			     ,@(remove-nils
				(mapcar #'(lambda (dir type name)
					    (if (or (eq dir :in) (eq dir :inout))
						(make-write-form call type name)))
					other-arg-dirs other-arg-types other-arg-names))
			     (finish-request ,call)
			     ,(if (not asynchronous-p)
				  `(multiple-value-bind (,status ,exception-index ,retry ,new-conn)
				       (wait-for-reply ,call)
				     (if (not ,retry)
					 (return (values ,status ,exception-index))))
				`(return (values nil nil))))
			 ,(if asynchronous-p
			      `(values nil t)
			    `(let (,@(if has-return-values
					`((return-value
					   (if (and
						(= ,status
						   +protocol-exception-success+)
						(= ,exception-index 0))
					       (list
						,@(remove-nils
						   (cons
						    (when return-type
						      (make-read-form
						       call return-type))
						    (remove-nils
						     (mapcar
						      #'(lambda (dir type)
							  (when (or
								 (eq dir :inout)
								 (eq dir :out))
							    (make-read-form
							     call type)))
						      other-arg-dirs
						      other-arg-types))))
						t)))))
				  ,@(when exceptions
				      `((exception
					 (when (and
						(= ,status
						   +protocol-exception-success+)
						(/= ,exception-index 0))
					   (make-instance
					       (gethash 
						(car (nth
						      (1- ,exception-index)
						      ',exceptions))
						*exception-repository-ids*)
					     :call ,call))))))
			      (ilu_reply-read ,call)
			      (if (= ,status +protocol-exception-success+)
				  (if (= ,exception-index 0)
				      ,(if has-return-values
					   (if (and functional-p
						    return-type
						    (null other-args))
					       `(apply #'values
						       (setf
							   (slot-value ,self ',name)
							 return-value))
					     `(apply #'values return-value))
					 (if (symbolp name)
					     ;; normal method
					     `(values nil t)
					   ;; setf method, return new value
					   `(values ,(first other-arg-names) t)))
				    ,(and exceptions `(error exception)))
				(error (make-instance
					   'protocol-error
					 :exception-value ,status))))))
		   (finish-call ,call)))
	       (return-ilu-call-struct ,call))))
	 #+ilu-old-method-names
	 (defmethod ,old-name
	     ,(if (symbolp name)
		  ;; normal method
		  `((,self ,(second self-type))
		    ,@(remove-nils
		       (mapcar #'(lambda (dir name)
				   (unless (eq dir :out) name))
			       other-arg-dirs other-arg-names)))
		;; setf method, must reverse arguments
		`(,(first other-arg-names)
		  (,self ,(second self-type))))
	     ;; now call the real method to implement this one
	     ,(if (symbolp name)
		  ;; normal method
		  `(,name ,self
			  ,@(remove-nils
			     (mapcar #'(lambda (dir name)
					 (unless (eq dir :out) name))
				     other-arg-dirs other-arg-names)))
		;; setf method, must reverse arguments
;;		`(setf (,name ,self)
;;		   ,(first other-arg-names))))
		;; 
		`(setf (,(second name) ,self)
		   ,(first other-arg-names))))
		))))

;;; Moved DEFINE-SERVER-CLASS and DEFINE-METHOD-SERVER-STUB to ilu-server.lisp

