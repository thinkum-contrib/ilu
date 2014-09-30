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

$Id: ilu-marshalling.lisp,v 1.23 1999/08/03 01:53:28 janssen Exp $
|#

(cl:in-package :ilu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; General locking rules (see src/runtime/kernel/locking.txt):
;;
;;   Size-computing routines:  L1, L2, Main unconstrained
;;   Marshalling and un-marshalling routines:
;;                    L2 >= {call's connection's callmu, iomu}
;;                    L1, Main unconstrained
;;
;;   (Objects are special.  See end of file.)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Primitive output functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun null-write (call n)
  (declare (ignore call n))
  ;; do nothing
  )

(defmacro define-primitive-writer (writer type c-fn)
  `(progn
     (define-c-function ,(intern c-fn) "primitive writer fn" ,c-fn
       (:ilu-call ,type :ilu-kerr) :fixnum :inline t)
     (defmacro ,writer (call object)
       `(let ((call ,call)		; Have to bind all these so that we can
	      (object ,object))		; ignore one & preserve order of eval.
	  (,',(intern c-fn) call object)))))

(define-primitive-writer integer-write :integer "ilu_OutputInteger")

(define-primitive-writer short-integer-write :fixnum "ilu_OutputShortInteger")

(define-primitive-writer cardinal-write :cardinal "ilu_OutputCardinal")

(define-primitive-writer short-cardinal-write
    :fixnum "ilu_OutputShortCardinal")

(define-c-function ilu_long-cardinal-write
    "Write a 64-bit unsigned value"
  "ilulisp_OutputLongCardinal"
  (:ilu-call :cardinal :cardinal :ilu-kerr) :fixnum :inline t)

(defmacro long-cardinal-write (call value)
  `(let ((call ,call)
	 (val ,value))
     (ilu_long-cardinal-write call (logand (ash val -32) #xFFFFFFFF) (logand value #xFFFFFFFF))))

(define-c-function ilu_long-integer-write
    "Write a 64 bit signed value"
  "ilulisp_OutputLongInteger"
  (:ilu-call :integer :cardinal :ilu-kerr) :fixnum :inline t)

(defmacro long-integer-write (call value)
  `(let ((call ,call)
	 (val ,value))
     (ilu_long-integer-write call (logand (ash val -32) #xFFFFFFFF) (logand val #xFFFFFFFF))))

(define-c-function ilu_enumeration-entry-write
    "Output an enumeration code"
  "ilu_OutputEnum"
  (:ilu-call :fixnum :pointer :ilu-kerr) :fixnum :inline t)

(defmacro enumeration-entry-write (call value)
  `(let ((call ,call)
	 (value ,value))
     (ilu_enumeration-entry-write call value (c-null-pointer))))

(define-primitive-writer real-write :real "ilu_OutputReal")

(define-c-function ilu_long-real-write
    "Write a 128-bit real number"
  "ilulisp_OutputLongReal"
  (:ilu-call :bytes :ilu-kerr) :fixnum :inline t)

(defmacro long-real-write (call r)
  `(let ((call ,call)
	 (r, r))
     (ilu_long-real-write call r)))

#+lisp-understands-ansi-c-parameter-passing
(define-primitive-writer short-real-write :short-real "ilu_OutputShortReal")

#-lisp-understands-ansi-c-parameter-passing
(progn
  (define-c-function ilulisp_short-real-write "Output a 32-bit real number"
    "ilulisp_KandROutputShortReal" (:ilu-call :short-real :ilu-kerr)
    :void :inline t)
  (defmacro short-real-write (call value)
    `(ilulisp_short-real-write ,call ,value)))

(define-primitive-writer byte-write :fixnum "ilu_OutputByte")

(define-c-function ilu_boolean-write
    "Write a boolean value"
  "ilu_OutputBoolean"
  (:ilu-call :fixnum :ilu-kerr) :void :inline t)

(defmacro boolean-write (call c)
  `(let ((call ,call)
	 (c ,c))
     (ilu_boolean-write call (if c 1 0))))

(define-c-function ilu_character-write 
    "Write a character (16 bits)"
  "ilu_OutputCharacter"
  (:ilu-call :short-cardinal :ilu-kerr) :void :inline t)

(defmacro character-write (call c)
  `(let ((call ,call)
 	 (c ,c))
     (ilu_character-write call (char-code c))))

(defmacro short-character-write (call c)
  `(let ((call ,call)
 	 (c ,c))
     (byte-write call (char-code c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Primitive input functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-primitive-reader (reader type c-fn)
  `(progn
     (define-c-function ,(intern c-fn) "primitive reader fn" ,c-fn
       (:ilu-call :ilu-kerr) ,type :inline t)
     (defmacro ,reader (call)
       `(,',(intern c-fn) ,call))))

(defun null-read (call)
  (declare (ignore call))
  nil)

(defmacro character-read (call)
  `(construct-unicode-character-from-code
    (short-cardinal-read ,call)))

(defmacro short-character-read (call)
  `(construct-iso-latin-1-character-from-code (byte-read ,call)))

(define-primitive-reader integer-read :integer "ilulisp_InputInteger")

(define-c-function ilulisp_cardinal-read
    "Read a 32-bit value"
  "ilulisp_InputCardinal"
  (:ilu-call :ilu-kerr) :cardinal :inline t)

(defun cardinal-read (call)	; hack around franz unsigned bug
  (let ((cardinal (ilulisp_cardinal-read call)))
    (if (minusp cardinal)
	(+ cardinal (expt 2 32))
      cardinal)))

(define-c-function ilu_long-cardinal-read
    "Read a 64 bit unsigned value"
    "ilulisp_InputLongCardinal"
  (:ilu-call (:out :cardinal) (:out :cardinal) :ilu-kerr) :fixnum :inline t)

(defun long-cardinal-read (call)
  (multiple-value-bind (result ms32 ls32)
      (ilu_long-cardinal-read call)
    (declare (ignore result))
    (+ (* ms32 4294967296) ls32)))

(define-c-function ilu_long-integer-read
    "Read a 64 bit integer"
    "ilulisp_InputLongInteger"
  (:ilu-call (:out :integer) (:out :cardinal) :ilu-kerr) :fixnum :inline t)

(defun long-integer-read (call)
  (multiple-value-bind (result ms32 ls32)
      (ilu_long-integer-read call)
    (declare (ignore result))
    (+ (* ms32 4294967296) ls32)))

(define-c-function ilulisp_boolean-read
    "Read a boolean value"
    "ilulisp_InputBoolean"
  (:ilu-call :ilu-kerr) :boolean :inline t)

(defmacro boolean-read (call)
  `(ilulisp_boolean-read ,call))

(define-primitive-reader short-integer-read
    :short-integer "ilulisp_InputShortInteger")

(define-primitive-reader short-cardinal-read
    :short-cardinal "ilulisp_InputShortCardinal")

(define-c-function ilu_enumeration-entry-read
    "Read an enum code"
  "ilulisp_InputEnumCode"
  (:ilu-call :pointer :ilu-kerr) :fixnum :inline t)

(defmacro enumeration-entry-read (call)
  `(ilu_enumeration-entry-read ,call (c-null-pointer)))

#-aclpc
(define-primitive-reader real-read :real "ilulisp_InputReal")

(define-c-function ilu_long-real-read
    "Read a 128-bit real number"
  "ilulisp_InputLongReal"
  (:ilu-call :ilu-kerr) :bytes :inline t)

(defmacro long-real-read (call)
  `(ilu_long-real-read ,call))

#+(and lisp-understands-ansi-c-parameter-passing (not aclpc))
(define-primitive-reader short-real-read
    :short-real "ilulisp_ANSIInputShortReal")

#-lisp-understands-ansi-c-parameter-passing
(define-primitive-reader short-real-read
    :short-real "ilulisp_KandRInputShortReal")

(define-primitive-reader byte-read :byte "ilulisp_InputByte")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Primitive size functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-primitive-sizer (sizer type c-fn)
  `(progn
     (define-c-function ,(intern c-fn) "primitive sizer fn" ,c-fn
       (:ilu-call ,type :ilu-kerr) :fixnum)
     (defmacro ,sizer (call object)
       `(let ((call ,call)		; Have to bind all these so that we can
	      (object ,object))		; ignore one & preserve order of eval.
	  (,',(intern c-fn) call object)))))

(defun null-size (call n)
  (declare (ignore call n))
  0)

(define-primitive-sizer integer-size :integer "ilu_SizeOfInteger")

(define-primitive-sizer short-integer-size :short-integer "ilu_SizeOfShortInteger")

(define-primitive-sizer cardinal-size :cardinal "ilu_SizeOfCardinal")

(define-primitive-sizer short-cardinal-size :short-cardinal "ilu_SizeOfShortCardinal")

(define-c-function ilu_enumeration-entry-size
    "Size an enum code"
  "ilu_SizeOfEnum"
  (:ilu-call :fixnum :pointer :ilu-kerr) :fixnum :inline t)

(defmacro enumeration-entry-size (call v)
    `(let ((call ,call)
	   (v ,v))
       (ilu_enumeration-entry-size call v (c-null-pointer))))

(define-primitive-sizer real-size :real "ilu_SizeOfReal")

(define-c-function ilu_long-real-size
    "Size a 128-bit real number"
  "ilulisp_SizeOfLongReal"
  (:ilu-call :bytes :ilu-kerr) :fixnum :inline t)

(defmacro long-real-size (call r)
  `(let ((call ,call)
	 (r, r))
     (ilu_long-real-size call r)))

#+lisp-understands-ansi-c-parameter-passing
(define-primitive-sizer short-real-size :short-real "ilu_SizeOfShortReal")

#-lisp-understands-ansi-c-parameter-passing
(progn
  (define-c-function ilulisp_short-real-size "SizeOf a 32-bit real number"
    "ilulisp_KandRSizeOfShortReal" (:ilu-call :short-real :ilu-kerr)
    :fixnum :inline t)
  (defmacro short-real-size (call value)
    `(ilulisp_short-real-size ,call ,value)))

(define-primitive-sizer byte-size :byte "ilu_SizeOfByte")


(define-c-function ilu_short-character-size
    "Take the size of a short character"
  "ilu_SizeOfShortCharacter"
  (:ilu-call :byte :ilu-kerr) :fixnum :inline t)

(defmacro short-character-size (call ch)
  `(let ((call ,call)
	 (ch ,ch))
     (ilu_short-character-size call (char-code ch))))

(define-c-function ilu_boolean-size
    "Take the size of a boolean"
  "ilu_SizeOfBoolean"
  (:ilu-call :boolean :ilu-kerr) :fixnum :inline t)

(defmacro boolean-size (call b)
  `(let ((call ,call)
	 (b ,b))
     (ilu_boolean-size call b)))

(define-c-function ilu_long-integer-size
    "Take the size of a 64 bit int"
  "ilulisp_SizeOfLongInteger"
  (:ilu-call :integer :cardinal :ilu-kerr) :fixnum :inline t)

(defmacro long-integer-size (call value)
  `(let ((call ,call)
	 (val ,value))
     (ilu_long-integer-size call (logand (ash val -32) #xFFFFFFFF) (logand val #xFFFFFFFF))))

(define-c-function ilu_long-cardinal-size
    "Take the size of a 64 bit int"
  "ilulisp_SizeOfLongCardinal"
  (:ilu-call :cardinal :cardinal :ilu-kerr) :fixnum :inline t)

(defmacro long-cardinal-size (call value)
  `(let ((call ,call)
	 (val ,value))
     (ilu_long-cardinal-size call (logand (ash val -32) #xFFFFFFFF) (logand val #xFFFFFFFF))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string (short character sequence) functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-c-function ilu_output-string
    "Output a string"
    "ilu_OutputString"
  (:ilu-call :string :cardinal :cardinal :ilu-kerr)
  :void :inline t)

(defun string-write (call s limit)
  (ilu_output-string call s (length s) limit))

(define-c-function string-read
    "Input a string"
  "ilulisp_InputString"
  (:ilu-call :cardinal :ilu-kerr) :string :inline t)

(define-c-function ilu_size-of-string
    "Take the size of a string"
  "ilu_SizeOfString"
  (:ilu-call :string :cardinal :cardinal :ilu-kerr) :fixnum)

(defmacro string-size (call s limit)
  `(let ((call ,call)
	 (s ,s))
     (declare (simple-string s))
     (ilu_size-of-string call s (length s) ,limit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; short character vector functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-c-function char-vector-write
    "Output a vector of short character"
    "ilulisp_OutputStringVec"
  (:ilu-call :string :cardinal :ilu-kerr) :fixnum :inline t)

(define-c-function char-vector-read
    "Input a vector of short character"
    "ilulisp_InputCharacterVector"
  (:ilu-call :cardinal :ilu-kerr) :string :inline t)

(define-c-function char-vector-size
    "Size a vector of short character"
    "ilulisp_SizeOfStringVec"
  (:ilu-call :string :cardinal :ilu-kerr) :fixnum :inline t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; character sequence functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-c-function ilu_output-wide-string
    "Output UNICODE string"
  "ilulisp_OutputWString" (:ilu-call :unicode :cardinal :ilu-kerr)
  :fixnum :inline t)

(defmacro unicode-string-write (call s limit)
  `(ilu_output-wide-string ,call ,s (length ,s) ,limit))

(define-c-function unicode-string-read
    "Input unicode string"
  "ilulisp_InputWString" (:ilu-call :cardinal :ilu-kerr) :unicode :inline t)

(define-c-function ilu_sizeof-wide-string
    "Size UNICODE string"
    "ilulisp_SizeOfWString"
  (:ilu-call :unicode :cardinal :ilu-kerr) :fixnum :inline t)

(defmacro unicode-string-size (call s limit)
  `(ilu_sizeof-wide-string ,call ,s (length ,s) ,limit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; character vector functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-c-function unicode-vector-write
    "Output unicode vector"
    "ilulisp_OutputWStringVec"
  (:ilu-call :unicode :fixnum :ilu-kerr) :fixnum)

(define-c-function unicode-vector-read
    "Input Unicode vector"
    "ilulisp_InputWStringVec"
  (:ilu-call :fixnum :ilu-kerr) :unicode :inline t)

(define-c-function unicode-vector-size
    "Size unicode vector"
    "ilulisp_SizeOfWStringVec"
  (:ilu-call :unicode :fixnum :ilu-kerr) :fixnum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; byte sequence functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-c-function ilulisp_byte-sequence-write
    "Output byte sequence"
    "ilulisp_OutputByteSequence"
  (:ilu-call :bytes :cardinal :ilu-kerr) :fixnum :inline t)

(defmacro byte-sequence-write (call v limit)
  `(ilulisp_byte-sequence-write ,call ,v ,limit))

(define-c-function ilulisp_input-byte-sequence "" "ilulisp_InputByteSequence"
  (:ilu-call :cardinal :ilu-kerr) :bytes :inline t)

(defmacro byte-sequence-read (call limit)
  `(ilulisp_input-byte-sequence ,call ,limit))

(define-c-function ilulisp_size-of-bytes "" "ilulisp_SizeOfByteSequence"
  (:ilu-call :bytes :cardinal :ilu-kerr) :fixnum)

(defmacro byte-sequence-size (call v limit)
  `(ilulisp_size-of-bytes ,call ,v ,limit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; byte vector functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-c-function byte-vector-write "" "ilulisp_OutputByteVector"
  (:ilu-call :bytes :cardinal :ilu-kerr) :fixnum :inline t)

(define-c-function byte-vector-read "" "ilulisp_InputByteVector"
  (:ilu-call :cardinal :ilu-kerr) :bytes :inline t)

(define-c-function byte-vector-size "" "ilulisp_SizeOfByteVector"
  (:ilu-call :bytes :cardinal :ilu-kerr) :fixnum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sequence functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-c-function ilu_sequence-write "" "ilu_OutputSequence"
  (:ilu-call :cardinal :cardinal :pointer :ilu-kerr) :void :inline t)

(defmacro sequence-write (call value limit)
  (let ((val (gensym)))
    `(let ((,val ,value))
       (ilu_sequence-write ,call (length ,val) ,limit (c-null-pointer)))))

(define-c-function ilulisp_input-sequence "" "ilulisp_InputSequence"
  (:ilu-call :cardinal :fixnum :pointer :ilu-kerr) :integer :inline t)

(defmacro sequence-read (call limit)
  (let ((i (gensym)))
    `(let ((,i (ilulisp_input-sequence ,call ,limit 0 (c-null-pointer))))
       (when (>= ,i 0)
	 ,i))))

(define-c-function ilu_sequence-size "" "ilu_SizeOfSequence"
  (:ilu-call :cardinal :cardinal :pointer :ilu-kerr) :fixnum)

(defmacro sequence-size (call value limit)
  (let ((val (gensym)))
    `(let ((,val ,value))
       (ilu_sequence-size ,call (length ,val) ,limit (c-null-pointer)))))

(define-c-function sequence-end "" "ilu_EndSequence"
  (:ilu-call :ilu-kerr) :fixnum :inline t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; union functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-c-function ilu_union-write "" "ilu_OutputUnion"
  (:ilu-call :integer :cardinal :pointer :ilu-kerr) :void :inline t)

(define-c-function ilulisp_input-union "" "ilulisp_InputUnion"
  (:ilu-call :cardinal :pointer :ilu-kerr) :integer :inline t)

(define-c-function ilu_union-size "" "ilu_SizeOfUnion"
  (:ilu-call :integer :cardinal :pointer :ilu-kerr) :cardinal)

(define-c-function union-end "" "ilu_EndUnion"
  (:ilu-call :ilu-kerr) :fixnum :inline t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; optional functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-c-function optional-write "" "ilu_OutputOptional"
  (:ilu-call :boolean :pointer :ilu-kerr) :void :inline t)

(define-c-function optional-read "" "ilulisp_InputOptional"
  (:ilu-call :pointer :ilu-kerr) :boolean :inline t)

(define-c-function optional-size "" "ilu_SizeOfOptional"
  (:ilu-call :boolean :pointer :ilu-kerr) :fixnum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; record functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-c-function ilu_record-write "" "ilu_OutputRecord"
  (:ilu-call :pointer :ilu-kerr) :void :inline t)

(defmacro record-write (call record)
  (declare (ignore record))
  `(ilu_record-write ,call (c-null-pointer)))

(define-c-function ilu_record-read "" "ilu_InputRecord"
  (:ilu-call :pointer :ilu-kerr) :void :inline t)

(defmacro record-read (call)
  `(progn
     (ilu_record-read ,call (c-null-pointer))
     t))

(define-c-function ilu_record-size "" "ilu_SizeOfRecord"
  (:ilu-call :pointer :ilu-kerr) :fixnum)

(defmacro record-size (call record)
  (declare (ignore record))
  `(ilu_record-size ,call (c-null-pointer)))

(define-c-function record-end "" "ilu_EndRecord"
  (:ilu-call :ilu-kerr) :fixnum :inline t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; array functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-c-function ilu_array-write "" "ilu_OutputArray"
  (:ilu-call :cardinal :pointer :ilu-kerr) :void :inline t)

(defmacro array-write (call array)
  `(ilu_array-write ,call (length ,array) (c-null-pointer)))

(define-c-function ilu_array-read "" "ilu_InputArray"
  (:ilu-call :pointer :ilu-kerr) :void :inline t)

(defmacro array-read (call)
  `(progn
     (ilu_array-read ,call (c-null-pointer))
     t))

(define-c-function ilu_array-size "" "ilu_SizeOfArray"
  (:ilu-call :cardinal :pointer :ilu-kerr) :fixnum)

(defmacro array-size (call array)
  `(ilu_array-size ,call (length ,array) (c-null-pointer)))

(define-c-function array-end "" "ilu_EndArray"
  (:ilu-call :ilu-kerr) :fixnum :inline t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; object I/O functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  before: L1 = {},
;;  after:  *o!=NULL => Inside(*o's server, static_type);
;;  after:  *o==NULL => L1 = {};
;;  forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});
;;  L2 >= {call's connection's callmu, iomu};
;;  Main otherwise unconstrained
(define-c-function ilulisp_input-object-id "" "ilulisp_InputObjectID"
  (:ilu-call (:out :ilu-class) :pointer :fixnum :ilu-kerr)
  :ilu-object :inline t)

;;  before: Inside(s, cl);
;;  after:				   L1 disjoint {cmu, s};
;;  after: cl collectible	        => L1  not >=  {gcmu};
;;  after: cl collectible & s surrogate => Main Invariant holds;
;;  where s = h's server and cl = h's type.
;;  (We don't really need to hold cmu for surrogate or non-collectible
;;   objects, but this is convenient because ilu_Enter/ExitServer can
;;   be used.)
(define-c-function object-id-write "" "ilu_OutputObjectID"
  (:ilu-call :ilu-object :fixnum :pointer :ilu-kerr) :void :inline t)

;; L1 >= {obj's server}
(define-c-function ilu_object-id-size "" "ilu_SizeOfObjectID"
  (:ilu-call :ilu-object :fixnum :pointer :ilu-kerr) :cardinal :inline t)
