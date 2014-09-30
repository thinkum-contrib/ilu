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

$Id: ilu-def-package.lisp,v 1.30 1999/08/03 01:53:34 janssen Exp $
|#

(cl:in-package :user)

(cl:defpackage :ilu
  (:use :common-lisp)
  (:shadow cl:byte-size)
  (:export

   :initialize-ilu
   :*version*
   :ilu_set-debug-level-via-string
   :ilu_send-debug-output-to-file
   :get-fd-budget
   :set-fd-budget
   
   ;; type constructors

   :define-primitive-type
   :define-record-type
   :define-enumeration-type
   :define-sequence-type
   :define-array-type
   :define-union-type
   :define-class-type
   :define-method-type
   :define-optional-type
   :define-alias-type

   :define-primitive
   :define-record
   :define-enumeration
   :define-sequence
   :define-array
   :define-union
   :define-class
   :define-method
   :define-optional
   :define-alias

   :define-method-server-stub

   :define-exception-type
   :define-exception

   :setup-rpc-stubs

   :ilu-class-name
   :ilu-class-record
   :ilu-class-id
   :ilu-class-info
   :get-class-name
   :find-ilu-class-name
   :find-ilu-class-id
   :*caller-identity*
   :register-custom-surrogate
   
   :language-specific-object	;; (ilu-object) => lisp-object (setf-able)
   :object-of-sbh		;; (string-binding-handle) => ilu-object
   :ilu-object->instance	;; (class ilu-object) => obj
   :sbh->instance		;; (class sbh) => obj
   :destroy-instance		;; (obj) =>
   :string-binding-handle	;; (obj) => string-binding-handle
   #+ilu-iiop
   :interoperable-object-reference	;; OMG-style string-binding-handle
   :form-string-binding-handle	;; (sid ih type pinfo tinfo) => string-binding-handle

   ;; base class for objects

   :ilu-object
   :ilu-singleton-object
   :ilu-true-object

   ;; server manipulation
   
   :create-object-table
   :kernel-server
   :native-cinfo	; method on kernel-server
   :add-cinfo		; method on kernel-server
   :add-port		; method on kernel-server
   :*default-server*

   ;; methods on ilu-object

   :rpc-method			;; (method-id) => C method descriptor
   :rpc-handle
   :instance-id
   :string-binding-handle
   :object-write
   :object-size
   :publish
   :withdraw
   :lookup
   :ping
   :find-type

   ;; base class for exceptions

   :rpc-exception
   :exception-value
   :signal-exception
   :set-debug-level

   :*debug-uncaught-conditions*

   ;; some pre-defined exceptions and values

   :cstring
   :corba-object
   :cardinal
   :shortcardinal
   :longcardinal
   :integer
   :shortinteger
   :longinteger
   :boolean
   :byte
   :real
   :shortreal
   :longreal
   
   )

  #+ilu-pickle
  (:export
   :pickle
   :pickle-value
   :pickle-type
   :pickle-bytes
   )
  
  ;; export some additional symbols for ACL Win
#+aclpc (:export
   :create-main-loop-handle
   :run-main-loop
   :exit-main-loop
   )

  )

