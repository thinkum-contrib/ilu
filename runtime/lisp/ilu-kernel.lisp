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

$Id: ilu-kernel.lisp,v 1.48 1999/08/30 22:35:10 janssen Exp $
|#

(cl:in-package :ilu)

;;;;;;;;;;;;;;; random ILU kernel functions ;;;;;;;;;;;;;;;

;; locking unconstrained

(define-c-function ilu_set-debug-level
    "set the kernel debugging level.  The argument is a bitmask, each
     bit selecting debug messages on some functional unit."
    "ilu_SetDebugLevel" (:cardinal) :cardinal)

(define-c-function ilu_set-debug-level-via-string
    "set the kernel debugging level.  The argument is a string with
     colon-separated fields"
    "ilu_SetDebugLevelViaString" (:string) :cardinal)

(define-c-function ilu_send-debug-output-to-file
     "redirect debug output to a file"
     "ilu_SendDebugOutputToFile" (:string) :void)

(defun set-debug-level (arg)
  (etypecase arg
    (integer
     (ilu_set-debug-level arg))
    (simple-string
     (ilu_set-debug-level-via-string arg))))

(define-c-function generate-server-id
  "Generate universally unique string from host's IP address, time, pid, etc."
  "ilu_InventID" () :string)

(define-c-function get-ilu-version
    "Return char * which defines the ILU version"
  "ilu_GetILUVersion" () :constant-string :inline t)

(define-c-function get-lisp-runtime-version
    "Return char * which defines the ILU version"
  "ilulisp_GetLispRuntimeVersion" () :constant-string :inline t)

(define-c-function get-fd-budget
    "Return number of file descriptors that ILU will limit itself to"
  "ilu_GetFDBudget" () :cardinal :inline t)

(define-c-function set-fd-budget
    "Set number of file descriptors that ILU will limit itself to.  Returns value actually established."
  "ilu_SetFDBudget" (:cardinal) :cardinal)

(define-c-function set-failure-actions
    "Set CheckFailureAction, AssertFailureAction, and MemFailureAction"
  "ilulisp_SetFailureActions"
  (:fixnum :fixnum :fixnum) :void :inline t)

;;;;;;;;;;;;;;; ILU kernel object manipulation ;;;;;;;;;;;;;;;

;; L1 >= {obj's server}; L2, main unconstrained
(define-c-function get-language-specific-object
    "Return Lisp object associated with kernel object (arg 1)"
  "ilu_GetLanguageSpecificObject" (:ilu-object :cardinal) :cardinal :inline t)

;; Inside (obj's server, obj's type)
(define-c-function register-language-specific-object
    "Set Lisp object of kernel object (arg 1) to be (arg 2).  
Arg 2 is index returned by ilu:register-lisp-value."
  "ilu_RegisterLanguageSpecificObject" (:ilu-object :cardinal :cardinal)
  :void :inline t)

;;  before: L1 = {};
;;  after:  result!=NULL => Inside(result's server, static_type);
;;  after:  result==NULL => L1 = {};
;;  forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});
;;  Main otherwise unconstrained
(define-c-function ilu_object-of-sbh
    "Return kernel object associated with SBH (arg 1).  Arg 2 is ilu_Class."
  "ilu_ObjectOfSBH" (:string :ilu-class :ilu-kerr) :ilu-object)

;typedef enum {
;  ilucsr_err,			/* see *err */
;  ilucsr_notReified,		/* sbh identifies a non-reified
;				 * server */
;  ilucsr_noProblem,		/* no problem has been detected
;				 * with the server's current
;				 * contact info */
;  ilucsr_isTrue,		/* the server isnt a surrogate */
;  ilucsr_noNews,		/* sbh doesn't contain new contact
;				 * info */
;  ilucsr_changed		/* the identified surrogate server
;				 * has been switched to the contact
;				 * info in sbh */
;}               ilu_ConsiderSbhResult;
;
;; L1_sup < smu; L2, Main unconstrained
(define-c-function ilu_consider-sbh
    "Reconsider the sbh"
  "ilu_ConsiderSBH"
  (:string (:out :pointer) :ilu-kerr) :fixnum)

;; L1 >= {obj's server}; L1_sup < prmu
(define-c-function ilu_sbh-of-object
    "Returns the kernel object's string binding handle"
  "ilu_SBHOfObject" (:ilu-object) :constant-string :inline t)

#+ilu-iiop
;; L1 >= {obj's server}; L1_sup < prmu
(define-c-function ilu_ior-of-object
    "Returns the kernel object's string binding handle"
  "ilu_IOROfObject" (:ilu-object :ilu-kerr) :constant-string :inline t)

;;  before: 				       L1 disjoint {cmu, server};
;;  before: cl collectible		    => L1  not >=  {gcmu};
;;  before: cl collectible & server surrogate => Main Invariant holds;
;;  after:  Inside(server, cl)
(define-c-function ilu_enter-server
    "go to Inside(SERVER, CLASS)"
  "ilu_EnterServer" (:ilu-server :ilu-class) :void :inline t)

;;  before: Inside(server, cl);
;;  after:				      L1 disjoint {cmu, server};
;;  after: cl collectible			   => L1  not >=  {gcmu};
;;  after: cl collectible & server surrogate => Main Invariant holds
(define-c-function ilu_exit-server
    "Exit from Inside(SERVER, CLASS)"
  "ilu_ExitServer" (:ilu-server :ilu-class) :void :inline t)

;; L1, L2, Main unconstrained
;; (But be careful about holding directly onto an ilu_Object)

(define-c-function ilu_instance-id
    "Return instance ID of object"
  "ilu_IhOfObject" (:ilu-object) :constant-string)

(define-c-function ilu_ilu-class
    "ilu_Class of kernel object"
  "ilu_ClassOfObject" (:ilu-object) :ilu-class :inline t)

(define-c-function ilu_ilu-server
    "ilu_Server of kernel object"
  "ilu_ServerOfObject" (:ilu-object) :ilu-server :inline t)

(define-c-function ilu_ping-object
    "Returns ilu_TRUE if the true object exists, and the process
 serving it can be contacted; ilu_FALSE otherwise.  May return a
 new outgoing connection to monitor (a la ilu_StartCall)."
  "ilu_PingObject" (:ilu-object (:out :pointer)) :boolean)

;;;;;;;;;;;;;;; ILU kernel method manipulation ;;;;;;;;;;;;;;;

;; L1, L2, Main unconstrained

(define-c-function find-method-by-id
    "Return ilu_Method on object (arg 1) indicated by index (arg 2)"
  "ilu_FindMethodByID" (:ilu-class :cardinal) :pointer :inline t)

(define-c-function id-of-method
    "Return the ID if the ilu_Method"
  "ilulisp_IDOfMethod" (:pointer) :cardinal :inline t)

;;;;;;;;;;;;;;; type registration for IIOP ;;;;;;;;;;;;;;;

(define-c-function ilulisp_acquire-otmu
    "Acquire the lock on the type DB"
  "ilulisp_Acquire_otmu"
  () :void)

(define-c-function ilulisp_release-otmu
    "Release the lock on the type DB"
  "ilulisp_Release_otmu"
  () :void)

(defmacro with-type-mutex (&body body)
  `(progn
     (ilulisp_acquire-otmu)
     ,@body
     (ilulisp_release-otmu)))  

#+ilu-type-info
(define-c-function ilu_find-type-by-UID
    "Given a UID, return the kernel data structure for that type"
  "ilu_FindTypeByUID"
  (:constant-string			; uid
   :ilu-kerr) :pointer)

#+ilu-type-info
(define-c-function ilu_register-sequence-type
    "Register a sequence type with the ILU kernel"
  "ilu_RegisterSequenceType"
  (:constant-string			; name
   :constant-string			; interface name
   :constant-string			; interface brand
   :constant-string			; uid
   :constant-string			; base type
   :cardinal				; limit (0 if none)
   (:out :boolean)			; t if new
   :ilu-kerr) :pointer)			; returns ilu_Type

#+ilu-type-info
(define-c-function ilu_register-array-type
    "Begin to register an array type with the ILU kernel"
  "ilu_RegisterArrayType"
  (:constant-string			; name
   :constant-string			; interface name
   :constant-string			; interface brand
   :constant-string			; uid
   :constant-string			; base type
   :cardinal				; ndims
   :pointer				; vector of dims
   (:out :boolean)			; true if new type
   :ilu-kerr) :pointer)			; returns ilu_Type

#+ilu-type-info
(define-c-function ilulisp_make-dim-vector
    "Malloc a vector of N ilu_cardinals, and return pointer"
  "ilulisp_MakeDimVector"
  (:cardinal				; number of elements
   :ilu-kerr) :pointer)			; returns vector

#+ilu-type-info
(define-c-function ilulisp_set-dim-vector-element
    "Set the dimension DIM of dimension N"
  "ilulisp_SetDimVectorElement"
  (:pointer				; dim vector
   :fixnum				; index
   :cardinal				; dimension value
   ) :void)

#+ilu-type-info
(defun register-array-type (name ifc-name ifc-brand uid
			    base-type-uid dims)
  (let ((ilutype (ilu_find-type-by-UID uid)))
    (when (c-null-pointer-p ilutype)
      (with-type-mutex
	  (let ((index 0)
		(dim-vector (ilulisp_make-dim-vector (length dims))))
	    (dolist (dim dims)
	      (ilulisp_set-dim-vector-element dim-vector index dim)
	      (incf index))
	    (ilu_register-array-type name ifc-name ifc-brand uid
				     base-type-uid (length dims)
				     dim-vector)
	    (ilulisp_free-c-struct dim-vector))))))

#+ilu-type-info
(define-c-function ilu_register-optional-type
    "Register an optional type with the ILU kernel"
  "ilu_RegisterOptionalType"
  (:constant-string			; name
   :constant-string			; interface name
   :constant-string			; interface brand
   :constant-string			; uid
   :constant-string			; base type
   (:out :boolean)			; t if new
   :ilu-kerr) :pointer)			; returns ilu_Type

#+ilu-type-info
(define-c-function ilu_register-alias-type
    "Register an alias type with the ILU kernel"
  "ilu_RegisterAliasType"
  (:constant-string			; name
   :constant-string			; interface name
   :constant-string			; interface brand
   :constant-string			; uid
   :constant-string			; base type
   (:out :boolean)			; t if new
   :ilu-kerr) :pointer)			; returns ilu_Type

#+ilu-type-info
(define-c-function ilu_register-object-type
    "Register an object type with the ILU kernel"
  "ilu_RegisterObjectType"
  (:constant-string			; name
   :constant-string			; interface name
   :constant-string			; interface brand
   :constant-string			; uid
   :pointer				; ilu_Class record
   (:out :boolean)			; t if new
   :ilu-kerr) :pointer)			; returns ilu_Type

#+ilu-type-info
(define-c-function ilu_register-enumeration-type
    "Register an enumeration type with the ILU kernel"
  "ilu_RegisterEnumerationType"
  (:constant-string			; name
   :constant-string			; interface name
   :constant-string			; interface brand
   :constant-string			; uid
   :fixnum				; number of elements
   (:out :boolean)			; t if new
   :ilu-kerr) :pointer)			; returns ilu_Type

#+ilu-type-info
(define-c-function ilu_register-enumeration-type-field
    "Register a field of an enumeration type with the ILU kernel"
  "ilu_RegisterEnumerationElement"
  (:pointer				; enum type record
   :fixnum				; element index
   :string				; element name
   :cardinal				; element value
   :ilu-kerr) :boolean)

#+ilu-type-info
(define-c-function ilu_register-record-type
    "Register a record type with the ILU kernel"
  "ilu_RegisterRecordType"
  (:constant-string			; name
   :constant-string			; interface name
   :constant-string			; interface brand
   :constant-string			; uid
   :fixnum				; number of fields
   :boolean				; extensible?
   :constant-string			; supertype-uid or NIL
   (:out :boolean)			; t if new
   :ilu-kerr) :pointer)			; returns ilu_Type

#+ilu-type-info
(define-c-function ilu_register-record-field
    "Register a field of a record type with the kernel"
  "ilu_RegisterRecordField"
  (:pointer				; the record type struct
   :fixnum				; field index
   :string				; field name
   :string				; UID of field type
   :ilu-kerr) :boolean)

#+ilu-type-info
(define-c-function ilu_register-union-type
    "Register a union type with the ILU kernel"
  "ilu_RegisterUnionType"
  (:constant-string			; name
   :constant-string			; interface name
   :constant-string			; interface brand
   :constant-string			; uid
   :constant-string			; discriminant type uid
   :cardinal				; number of arms
   :cardinal				; default arm index
   :boolean				; bogus disc values allowed?
   (:out :boolean)			; t if new
   :ilu-kerr) :pointer)			; returns ilu_Type

#+ilu-type-info
(define-c-function ilu_register-union-arm
    "Register a particular arm of a union type with the ILU kernel"
  "ilu_RegisterUnionArm"
  (:pointer				; type record
   :cardinal				; index
   :string				; arm name
   :string				; arm type uid
   :fixnum				; number of selectors
   :ilu-kerr) :pointer)			; returns ilu_UnionArm

#+ilu-type-info
(define-c-function ilulisp_register-union-arm-value
    "Register a particular selector value for an arm of a union"
  "ilulisp_RegisterUnionArmValue"
  (:pointer				; the arm
   :cardinal				; value index
   :fixnum				; value type
   ;; only one of the following is valid on each call
   :cardinal
   :integer
   :boolean
   :string				; used for enum types
   :ilu-kerr) :void)

#+ilu-type-info
(defconstant *byte-cvk*			0)
(defconstant *short-integer-cvk*	1)
(defconstant *integer-cvk*		2)
(defconstant *short-cardinal-cvk*	3)
(defconstant *cardinal-cvk*		4)
(defconstant *short-real-cvk*		5)
(defconstant *real-cvk*			6)
(defconstant *boolean-cvk*		7)
(defconstant *enumeration-cvk*		8)
(defconstant *string-cvk*		9)

(defun register-union-type (isl-name ifc-name ifc-brand uid
			    disc-type-uid disc-kind default-arm-index
			    bogus-values-allowed-p arms)
  (with-type-mutex
      (multiple-value-bind (type-record newp)
	  (ilu_register-union-type isl-name ifc-name ifc-brand
				   uid disc-type-uid (length arms)
				   default-arm-index bogus-values-allowed-p)
	(when newp
	  (let ((arm-index 0))
	    (dolist (arm arms)
	      (let ((arm-record (ilu_register-union-arm
				 type-record arm-index (third arm)
				 (fourth arm) (length (first arm))))
		    (selector-index 0)
		    (values (fifth arm)))
		(dolist (selector values)
		  (ecase disc-kind	;; disc-kind is ilu_TypeKind
		    (0			; byte
		     (ilulisp_register-union-arm-value
			arm-record selector-index *byte-cvk* selector 0 nil nil))
		    (4			; short integer
		     (ilulisp_register-union-arm-value
			arm-record selector-index *short-integer-cvk* 0 selector nil nil))
		    (5			; integer
		     (ilulisp_register-union-arm-value
			arm-record selector-index *integer-cvk* 0 selector nil nil))
		    (7			; short cardinal
		     (ilulisp_register-union-arm-value
			arm-record selector-index *short-cardinal-cvk* selector 0 nil nil))
		    (8			; cardinal
		     (ilulisp_register-union-arm-value
			arm-record selector-index *cardinal-cvk* selector 0 nil nil))
		    (1			; boolean
		     (ilulisp_register-union-arm-value
			arm-record selector-index *boolean-cvk* 0 0 (if selector t nil) nil))
					; enumeration
		    (21 (ilulisp_register-union-arm-value
			 arm-record selector-index *enumeration-cvk* 0 0 nil selector))
		    )
		  (incf selector-index))
		(incf arm-index))))))))

;;;;;;;;;;;;;;; ILU kernel class manipulation ;;;;;;;;;;;;;;;

;; L1, L2, Main unconstrained
(define-c-function ilulisp_id-of-class
    "Return unique_id field of class (arg 1)"
  "ilulisp_IDOfClass" (:ilu-class) :constant-string :inline t)

;; L1_sup < otmu
;; L2, Main unconstrained

(define-c-function ilu_find-class-from-id
    "Return pointer to class record, given pointer to class UID"
  "ilu_FindClassFromID" (:string) :ilu-class :inline t)

(define-c-function ilulisp_start-superclass-vector
    "Start building a vector of superclass types"
  "ilulisp_StartSuperclassVector" (:integer :constant-string) :pointer
  :inline t)

(define-c-function ilulisp_add-superclass-to-vector
    "Add a superclass to a superclass vector"
  "ilulisp_AddSuperclassToVector" (:pointer :integer :integer :constant-string)
  :void :inline t)

(define-c-function ilu_define-object-type
  "adds a (partially constructed) new object type to the kernel or checks
   the arguments against an object type already known to the kernel"
  "ilu_DefineObjectType"
  (:string			; name
   :string			; brand
   :string			; unique ID
   :string			; singleton
   :boolean			; optional
   :boolean			; collectible
   :string			; doc string
   :cardinal			; method count
   :cardinal			; superclass count
   :pointer			; superclass IDs
   :ilu-kerr
   ) :ilu-class)

(define-c-function ilu_define-method
  "defines the i'th method of the class"
  "ilu_DefineMethod"
  (:ilu-class				; class
   :cardinal				; i
   :string				; name
   :cardinal				; index
   :boolean				; functional
   :boolean				; asynchronous
   :cardinal				; exception count
   :pointer				; exception vector
   :fixnum				; number of args
   :string				; type ID of return type, or NULL
   :ilu-kerr
   ) :pointer)

(define-c-function ilu_define-method-arg
    "defines the i'th argument of the method"
  "ilu_DefineMethodArg"
  (:pointer				; method (returned from ilu_define-method)
   :fixnum				; index of arg
   :constant-string			; name of arg
   :boolean				; sibling?
   :fixnum				; `direction' (in=1,out=2,inout=3)
   :constant-string			; type ID
   :ilu-kerr)
  :boolean)

(defmacro arg-direction->fixnum (arg-direction)
  `(ecase ,arg-direction
     (:in 1)
     (:out 2)
     (:inout 3)))

(define-c-function ilu_define-exception
  "returns the representation of an exception"
  "ilu_DefineException"
  (:string			; interface
   :string			; name
   :string			; type ID of value type, or NULL
   :ilu-kerr
   ) :pointer)

(define-c-function ilu_object-type-defined
  "end definition of an object type"
  "ilu_ObjectTypeDefined"
  (:ilu-class :ilu-kerr) :boolean)

(define-c-function ilulisp_cons-exception-vector
    "allocate some memory for exception pointers"
  "ilulisp_ConsExceptionVector" (:integer) :pointer)

(define-c-function ilulisp_set-exception-vector
    "Allocate a string in C space and assign it to a particular cell
in an exception vector."
  "ilulisp_SetExceptionVector" (:pointer :integer :pointer) :void)

(define-c-function ilulisp_get-root-class
    "Return an ilu_Class for built-in ILU root class."
  "ilulisp_getRootClass"
  () :ilu-class)

(define-c-function ilulisp_get-primitive-type-uid
    "Return the UID for the specified primitive type"
  "ilulisp_GetTypeUID"
  (:string) :constant-string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Pickle API
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+ilu-pickle
(define-c-function ilu_start-pickle
    "initialize an ilu_Call structure for use in pickling or unpickling"
  "ilulisp_StartPickle"
  (:ilu-kerr) :ilu-call)

#+ilu-pickle
(define-c-function ilu_write-pickle
    "write the type tag to the pickle, and allocate space"
  "ilu_WritePickle"
  (:ilu-call :cardinal :string :ilu-kerr) :void)

#+ilu-pickle
(define-c-function ilu_read-pickle
    "begin parsing an existing pickle"
  "ilulisp_ReadPickle"
  (:ilu-call :bytes :ilu-kerr) :void)

#+ilu-pickle
(define-c-function ilu_end-pickle-formation
    "Finish marshalling a pickle"
  "ilulisp_EndFormPickle"
  (:ilu-call :ilu-kerr) :bytes)

#+ilu-pickle
(define-c-function ilu_end-pickle-decode
    "Finish unmarshalling a pickle"
  "ilulisp_EndDecodePickle"
  (:ilu-call :ilu-kerr) :void)

#+ilu-pickle
(define-c-function ilu_pickle-type-uid
    "Find the type UID of a pickle from its bytes"
  "ilulisp_PickleTypeUID"
  (:bytes :ilu-kerr) :string)

#+ilu-pickle
(define-c-function ilulisp_output-pickle
  "Output a pickle"
  "ilulisp_OutputPickle"
  (:ilu-call :bytes :ilu-kerr) :void)

#+ilu-pickle
(define-c-function ilulisp_sizeof-pickle
  "Calculate size needed for a pickle"
  "ilulisp_SizeOfPickle"
  (:ilu-call :bytes :ilu-kerr) :fixnum)

#+ilu-pickle
(define-c-function ilulisp_input-pickle
  "Calculate size needed for a pickle"
  "ilulisp_InputPickle"
  (:ilu-call :ilu-kerr) :bytes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  build a Transport Info
;;;
;;;  no error checking is done to verify arguments
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-c-function ilulisp_create-transport-info
  "Create a Transport Info structure"
  "ilulisp_CreateTransportInfo"
  (:cardinal		; total number of strings
   :cardinal		; total string length (not counting terminating zeros)
   :string		; the first string
   :ilu-kerr
   ) :pointer)

(define-c-function ilulisp_add-transport-info
  "Add a string to a partially initialized Transport Info structure"
  "ilulisp_AddTransportInfo"
  (:pointer		; transport info structure
   :cardinal		; total number of strings
   :cardinal		; index of this string
   :string		; this string
   ) :void)

(define-c-function ilulisp_default-transport-info
  "Return a copy of the default transport info"
  "ilulisp_GetDefaultTinfo" (:ilu-kerr) :pointer)

(defun make-transport-info (strings)
  (if strings
      (let* ((num-strings (length strings))
	     (tot-length (loop for s in strings sum (length s)))
	     (tinfo
	      (ilulisp_create-transport-info num-strings tot-length (car strings))))
	(do ((index 1 (1+ index))
	     (strings (cdr strings) (cdr strings)))
	    ((null strings) tinfo)
	    (ilulisp_add-transport-info tinfo num-strings index (car strings))))
    (ilulisp_default-transport-info)))  

;; transport info structs can be freed with this function:
(define-c-function ilulisp_free-c-struct
  "free a C-allocated structure"
  "ilulisp_FreeCStruct"
  (:pointer) :void)

(define-c-function ilu_add_cinfo_to_server
  "Add the specified PINFO and TINFO to the specified server"
  "ilu_AddCInfo"
  (:ilu-server :string :pointer :ilu-kerr) :boolean)

(define-c-function ilu_get_cinfo_of_server
  "Return the 'native' cinfo for the server as PINFO and TINFO"
  "ilu_ServerCInfo"
  (:ilu-server :boolean (:out :constant-string) (:out :pointer) :ilu-kerr) :boolean)

(define-c-function ilulisp_tinfo-length
  "Returns the number of elements in the tinfo"
  "ilulisp_TinfoLength"
  (:pointer) :fixnum)

(define-c-function ilulisp_tinfo-element
  "Returns the Nth number of elements in the TINFO"
  "ilulisp_TinfoElement"
  (:pointer :fixnum :ilu-kerr) :string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Call primitive functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-c-function ilu_start-call
  "called by stub to initiate call"
  "ilu_StartCall"
  (:ilu-call :ilu-server :ilu-class :pointer :cardinal
   :pointer (:out :pointer) :ilu-kerr) :boolean)

(define-c-function ilu_start-request
  "called by stub to introduce the arguments"
  "ilu_StartRequest"
  (:ilu-call :cardinal :ilu-kerr) :boolean)

(define-c-function call-needs-sizing 
  "tell if call needs sizing"
  "ilu_CallNeedsSizing"
  (:ilu-call) :boolean)

;; Main Invariant holds
(define-c-function begin-sizing-reply
  "return initial size for reply"
  "ilu_BeginSizingReply"
  (:ilu-call :boolean :ilu-kerr) :cardinal)


;; Main Invariant holds
(define-c-function finish-request
    "Finish the request (flush the message to the server)"
  "ilu_FinishRequest" (:ilu-call :ilu-kerr) :boolean :inline t)

;;  before: L2 not >=   {call's conn's iomu},
;;	  L2     >=   {call's conn's callmu} iff protocol not concurrent;
;;   after: L2     >=   {call's conn's callmu, iomu} if result is true,
;;	  L2 disjoint {call's conn's callmu, iomu} if result is false.
(define-c-function begin-reply
    "Start a reply"
  "ilu_BeginReply"  (:ilu-call :boolean :cardinal :ilu-kerr)
  :boolean :inline t)

;; Main Invariant holds
;; L2    >=    {call's conn's callmu, iomu} before,
;; L2 disjoint {call's conn's callmu, iomu} after
(define-c-function finish-reply
    "Finish the reply (flush the message to the caller)"
  "ilu_FinishReply" (:ilu-call :ilu-kerr) :boolean :inline t)

;; before: L2 not >=   {call's conn's iomu},
;;         L2     >=   {call's conn's callmu} iff protocol not concurrent;
;; after:  L2 disjoint {call's conn's callmu, iomu}
(define-c-function ilu_no-reply
    "Used by server to end asynchronous call arg unmarshalling"
  "ilu_NoReply" (:ilu-call :ilu-kerr) :boolean :inline t)

;; Main Invariant holds, L2 otherwise unconstrained
(define-c-function begin-sizing-exception
    "Start an exception"
  "ilu_BeginSizingException" (:ilu-call :integer :ilu-kerr)
  :cardinal :inline t)

;;  before: L2 not >=   {call's conn's iomu},
;;	  L2     >=   {call's conn's callmu} iff protocol not concurrent;
;;  after: L2     >=   {call's conn's callmu, iomu} if result is true,
;;	  L2 disjoint {call's conn's callmu, iomu} if result is false.
(define-c-function begin-exception
    "Start an exception"
  "ilu_BeginException" (:ilu-call :integer :cardinal :ilu-kerr)
  :boolean :inline t)

;; Main Invariant holds
;; L2    >=    {call's conn's callmu, iomu} before,
;; L2 disjoint {call's conn's callmu, iomu} after
(define-c-function finish-exception
    "Finish the exception"
  "ilu_FinishException" (:ilu-call :ilu-kerr) :boolean :inline t)

;; L2    >=    {call's conn's callmu, iomu} before,
;; L2 disjoint {call's conn's callmu, iomu} after
(define-c-function finish-call
    "end the scope of the call"
  "ilu_FinishCall" (:ilu-call :ilu-kerr) :void :inline t)

;; Main Invariant holds
(define-c-function wait-for-reply
    "Wait for the reply"
  "ilulisp_GetReply" (:ilu-call (:out :cardinal) (:out :boolean) (:out :pointer) :ilu-kerr) :fixnum)

(define-c-function ilu_reply-read
    "Finished reading the reply"
  "ilu_ReplyRead" (:ilu-call :ilu-kerr) :boolean)

(defun caller-identity (call-struct)
  (declare (ignore call-struct))
  "unknown identity")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Server functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; L1, L2, Main unconstrained
(define-c-function id-of-kernel-server
    "Return string ID of kernel server"
  "ilu_IDOfServer" (:ilu-server) :constant-string)

(define-c-function ilu_register-language
  "register a language mapping"
  "ilu_RegisterLanguage"
  (:constant-string) :cardinal)

;;L2 unconstrained;
;;   before: L1.sup < gcmu;
;;   after:  result => Inside(result, ilu_rootClass);
;;   after: !result => L1.sup < gcmu
(define-c-function ilu_create-true-server
  "Takes string ID of server, and optional OBJECT-TABLE, returns pointer to server"
  "ilu_CreateTrueServer" (:constant-string :pointer :fixnum :ilu-kerr) :ilu-server)

;; L1 >= {s}; L2 unconstrained
(define-c-function ilu_get-lss
    "Get the language-specific server for a given kernel server"
  "ilu_GetLSS"
  (:ilu-server				; the server
   :fixnum				; language index for desired
					; LSS
   ) :cardinal)				; the LSS

;; L1 >= {s}; L2 unconstrained
(define-c-function ilu_set-lss
    "Makes a link from the given kernel server to the given
 Language-Specific Server; removes such a link if (lss==NIL).
 If (lss==NIL), this may provoke the kernel to destroy and free
 the kernel server."
  "ilu_SetLSS"
  (:ilu-server				; kernel server
   :cardinal				; LSS
   :fixnum				; language index
   :ilu-kerr) :boolean)

;; L1 >= {s}; L2 unconstrained
(define-c-function ilu_delta-server-holds
    "This function can be used to hold onto a kernel server when no
LSS has yet been registered for it."
  "ilu_DeltaServerHolds"
  (:ilu-server				; the server
   :fixnum				; the change in the ref count
   :ilu-kerr) :boolean)

;; L1_sup < s
(define-c-function set-server-default-port
    "Sets the default port of SERVER to be PORT"
  "ilu_SetServerDefaultPort" (:ilu-server :pointer) :void)

;; locking unconstrained
(define-c-function get-default-protocol-info
  "Returns the default protocol info, as a string"
  "ilu_DefaultProtocolInfo" () :constant-string)

;; locking unconstrained
(define-c-function get-default-transport-info
  "Returns the default protocol info, as an ilu_TransportInfo struct"
  "ilu_DefaultTransportInfo" () :pointer)

;; Main Invariant holds
(define-c-function create-port
    "Creates an ilu port on SERVER with protocol as specified by PROTOCOL \
     and transport as specified by TRANSPORT, with the specified PASSPORT, \
     making it public if PUBLIC is True, private otherwise"
  "ilu_FullCreatePort" (:ilu-server :string :pointer :pointer :boolean :ilu-kerr) :pointer)

;; L1_sup < cmu
;; Main Invariant holds
(define-c-function handle-ilu-connection
    "Called by server to create a connection on PORT"
    "ilulisp_HandleNewConnection"
    (:pointer) :pointer)

(define-c-function ilulisp_receive-request
    "Called by server to begin the input of a request"
  "ilulisp_ReceiveRequest"
  (:ilu-call (:out :boolean) :pointer (:out :ilu-class)
   (:out :constant-string) (:out :constant-string) (:out :cardinal) :ilu-kerr) :fixnum)

(define-c-function ilu_request-read
    "Called by the server to indicate that it has finished reading the arguments of a request"
  "ilu_RequestRead"
  (:ilu-call :ilu-kerr) :boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Binding functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  before: Inside(s, cl)
;;  after:				 L1 disjoint {cmu, s};
;;  after: cl collectible		      => L1  not >=  {gcmu};
;;  after: cl collectible & s surrogate => Main Invariant holds;
;;  where s = obj's server and cl = obj's type.
;;  (We don't really need to hold cmu for surrogate or non-collectible
;;   objects, but this is convenient because ilu_Enter/ExitServer can
;;   be used.)
(define-c-function ilu_publish-object
    "Called to register object's OID with domain binding service"
  "ilu_PublishObject"
  (:ilu-object) :string)
    
;;  before: Inside(s, cl)
;;  after:				 L1 disjoint {cmu, s};
;;  after: cl collectible		      => L1  not >=  {gcmu};
;;  after: cl collectible & s surrogate => Main Invariant holds;
;;  where s = obj's server and cl = obj's type.
;;  (We don't really need to hold cmu for surrogate or non-collectible
;;   objects, but this is convenient because ilu_Enter/ExitServer can
;;   be used.)
(define-c-function ilu_withdraw-published-object
    "Called to withdraw registration"
  "ilu_WithdrawObject"
  (:ilu-object :constant-string) :boolean)

;;  before: L1 = {};
;;  after:  result!=NULL => Inside(result's server, pclass);
;;  after:  result==NULL => L1 = {};
;;  forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});
;;  Main otherwise unconstrained */
(define-c-function ilu_lookup-object-by-oid
    "Given an oid, return the kernel object associated with it"
  "ilu_LookupObject"
  (:string :string :ilu-class) :ilu-object)

;;  before: L1 = {};
;;  after:  result!=NULL => Inside(result's server, pclass);
;;  after:  result==NULL => L1 = {};
;;  forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});
;;  Main otherwise unconstrained */
(define-c-function ilu_re-lookup-object-by-oid
    "Given an oid, return the kernel object associated with it"
  "ilu_ReLookupObject"
  (:string :string :ilu-class (:out :ilu-object)) :boolean)

;;  L1 >= {the object's server}
;;  L1 >= {gcmu} if cl collectible
(define-c-function ilu_find-or-create-true-object
    "Create true object if not found"
  "ilu_FindOrCreateTrueObject"
  (:string :ilu-server :ilu-class :cardinal) :ilu-object)

;;  L1, L2 unconstrained
(define-c-function ilu_set-gc-callback
    "A client of GCed objects calls this --- once, before RegisterLanguageSpecificObject on any GCed object."
  "ilu_SetGcClient"
  (:ilu-object) :void)

;;  L1_sup < otmu
;;  L2, Main unconstrained
(define-c-function ilu_gc-callback-class
    "Returns the class record for the GC callback class"
  "ilu_GetGcCallbackClass"
  () :ilu-class :inline t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Error handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-c-function ilulisp_format-error
    "Given an ILU C error struct, reports the error as best it can"
  "ilulisp_FormatError"
  (:pointer) :string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  This is *only* for debugging.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-c-function ilu_set-assertion-failure-action
    "0 causes not to SEGV on assert failure, 1 causes SEGV"
  "ilu_SetAssertionFailureAction"
  (:fixnum) :void)
