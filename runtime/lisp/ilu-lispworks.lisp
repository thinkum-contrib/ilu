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
Copyright (c) 1995 Harlequin Inc.  All Rights Reserved.  

Unlimited use, reproduction, and distribution of this software is
permitted.  Any copy of this software must include both the above
copyright notice of Harlequin Inc. and this paragraph.  Any
distribution of this software must comply with all applicable United
States export control laws.  This software is made available AS IS,
and Harlequin Inc. disclaims all warranties, express or implied,
including without limitation the implied warranties of merchantability
and fitness for a particular purpose, and notwithstanding any other
provision contained herein, any liability for damages resulting from
the software or its use is expressly disclaimed, whether arising in
contract, tort (including negligence) or strict liability, even if
Harlequin Inc. is advised of the possibility of such damages.

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Harlequin Lispworks Common Lisp - specific ILU code
;;;
;;;  Any function, var, macro, etc., with "lispworks" in its name,
;;;  defined in this file, is part of the Harlequin Lispworks implementation,
;;;  and might not appear in implementations for other CLs.  Any
;;;  function, etc., without "lispworks" in its name, is a required
;;;  function or macro which the generic ILU lisp implementation
;;;  uses, and must be provided by any implementation.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Foreign function utilities:
;;;
;;;  (define-c-function LISP-NAME C-NAME ARGS RETURN-TYPE &key INLINE)
;;;
;;;  (register-lisp-object VAL) => TAG (of type fixnum)
;;;
;;;  (lookup-registered-lisp-object TAG) => VAL
;;;
;;;  (char*-to-string C-POINTER) => STRING
;;;
;;;  (string-to-char* STRING) => C-POINTER
;;;
;;;  (initialize-locking)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package ilu)

(ffi:define-foreign-type :unicode (:array :unsigned-short))

(defmacro get-void-pointer ()
  '(ffi:make-pointer-alien :void))

;(defmacro get-void-pointer ()
;  '(ffi::make-alien-object 0 :void))


;;hm, we can't force the guy to get rid of this, so we just cons, icky.
(defun c-null-pointer ()
  (ffi::make-alien-object 0 :void))

;;we have one of these lying about for times when we're sure it's not going to
;;get modified.  Maybe we should *always* just have one?  Hm.
(defvar *c-null-pointer* (c-null-pointer))

;;;This is lame, trying to make up for our lack of ability to cope with NILs
;;;in foreign function args---we have to make up a bunch of null typed
;;;objects.  This seems really losing.
(defvar *empty-byte-vector* (make-array 0 :element-type '(unsigned-byte 8)))

(defun c-null-pointer-p (xxx) 
  (if (typep xxx 'number)
      (progn (cerror "return value from zerop" "null-pointer got a number ~A" xxx)
	     (zerop xxx))
    (ffi:ffi-null xxx)))

(eval-when (compile eval load)
(defun lispworks-return-type (type)
  (ecase type
    ((:short-cardinal :short-integer :byte :fixnum)
     :fixnum)
    (:boolean
     :boolean)
    ((:cardinal :integer)
     :integer)
    ((:bytes :ilu-call :ilu-object :ilu-class :ilu-server :pointer)
     '(:alien :void))
    ((:string :constant-string :char*)
     :string)
    (:short-real :single-float)
    (:real :double-float)
    (:void '(:alien :void))
    (:unicode :unicode)
    ))


(defun lispworks-argument-type (arg)
  (ecase arg
    (:pointer                '(:alien :void))
    (:cardinal                :unsigned-integer)
    (:short-cardinal          :unsigned-short)
    (:long-cardinal          '(:array :integer 2))
    (:integer                 :integer)
    (:short-integer           :short)
    (:long-integer           '(:array :integer 2))
    (:string                  :string)
    (:constant-string         :string) ; probably this should be fancier.
;    (:wide-string            '(:array :unsigned-short))
    (:byte                    :character)
    (:boolean                 :boolean)
    (:short-real              :single-float)
    (:real                    :double-float)
    (:long-float              :double-float)
    (:bytes                  '(:array :unsigned-byte))

    (:ilu-object              '(:alien :void))
    (:ilu-class               '(:alien :void))
    (:ilu-server              '(:alien :void))
    (:ilu-call                '(:alien :void))

    (:fixnum                  :fixnum)

    (:unicode                 :unicode)
;    (:cardinal-pointer       '(:array :integer))
;    (:shortcardinal-pointer  '(:array :fixnum))
;    (:integer-pointer        '(:array :integer))
;    (:shortinteger-pointer   '(:array :fixnum))
;    (:byte-pointer           '(:array :char))
;    (:real-pointer           '(:array :double-float))
;    (:shortreal-pointer      '(:array :single-float))
;    (:string-pointer         '(:array :string))
    ;; Someone will have to do some work here
;    (:ilu-object-pointer     '(:array :integer))
    
    (:true-pointer           '(:pointer :void))))

(defun lispworks-argument-value (arg)
  (ecase arg
    (:pointer                '*c-null-pointer*)
    (:cardinal                0)
    (:short-cardinal          0)
    (:long-cardinal          '(:array :integer 2))
    (:integer                 0)
    (:short-integer           0)
    (:long-integer           '(:array :integer 2))
    (:string                  nil)
    (:constant-string         NIL) ; probably this should be fancier.
;    (:wide-string            '(:array :unsigned-short))
    (:byte                    0)
    (:boolean                 nil) ;this is kind of a kluge, but.
    (:short-real              0.0)
    (:real                    0.0)
    (:long-float              0.0)
    (:bytes                  '*empty-byte-vector*)

    (:ilu-object              '*c-null-pointer*)
    (:ilu-class               '*c-null-pointer*)
    (:ilu-server              '*c-null-pointer*)
    (:ilu-call                '*c-null-pointer*)

    (:fixnum                  0)

    (:unicode                 :unicode)
;    (:cardinal-pointer       '(:array :integer))
;    (:shortcardinal-pointer  '(:array :fixnum))
;    (:integer-pointer        '(:array :integer))
;    (:shortinteger-pointer   '(:array :fixnum))
;    (:byte-pointer           '(:array :char))
;    (:real-pointer           '(:array :double-float))
;    (:shortreal-pointer      '(:array :single-float))
;    (:string-pointer         '(:array :string))
    ;; Someone will have to do some work here
;    (:ilu-object-pointer     '(:array :integer))
    
    (:true-pointer           (get-void-pointer))))

;;;The point of this kluge is to prevent the GC from trashing strings which 
;;;we are passing in to C.  Turns out even when we statically allocate them,
;;;the GC will sometimes decide they're garbage and while it will not *move*
;;;anything in static area, it may re-use the memory.  So, fill up this fine 
;;;variable with pointers into static space so that GC keeps its paws off.
(defvar *lispworks-landfill* nil)

(defun lispworks-sanitize-arg-type (arg-name arg-type)
  (let ((default-value (lispworks-argument-value arg-type)))
    ;;special kluge for strings: we must not permit them to get GC'd.
    (if (eq arg-type :constant-string)
	`(and ,arg-name
	      (let ((len (length ,arg-name)))
		(setq ,arg-name (lw:make-static-string len ,arg-name))
		(push ,arg-name *lispworks-landfill*)))
      `(setq ,arg-name (or ,arg-name ,default-value)))))
) ;lave-nehw

(defmacro define-c-function (lisp-name doc-string c-name args return-type
			     &key inline)
  (declare (ignore doc-string inline))
  (let* ((arg-directions (mapcar #'(lambda (arg) (if (consp arg) (first arg) :in)) args))
	 (arg-types (mapcar #'(lambda (arg) (if (consp arg) (second arg) arg)) args))
	 (internal-name (intern (concatenate 'string (string lisp-name) "-INTERNAL")))
	 
	 macro-args dff-args macro-call defun-args mv-ret setq-stmt int-mv-ret
	 )
    ;;;for c fn foo with :in :out :inout, do:
    ;;;(define-foreign-function foo-internal (in (out :reference-return)
    ;;;                                      (inout :reference)))
    ;;;(defmacro foo (in inout) (foo-internal in anything inout))
    ;;;collect three things: args to the macro, args to the define-foreign-fn,
    ;;;and args to be *passed* to the foreign function (this last goes in the
    ;;;macro body).  In cases where :inout and :out are not specified, we don't
    ;;;actually need all the work of the macro, it just passes its args, but
    ;;;at present I'm lazy.
    (loop for dir in arg-directions as type in arg-types
	  as next from 1 as lw-type = (lispworks-argument-type type)
	  as nextarg = (intern (format nil "ARG-~D" next))
	  as nextret = (intern (format nil "RET-~D" next))
	  if (and (eq dir :out) (consp lw-type) (eq (car lw-type) :alien))
	  collect (list nextarg '(get-void-pointer)) into defun
	  and collect `(,nextarg (:pointer ,lw-type)) into dff
	  and collect nextarg into call
	  and collect `(ffi:make-alien-from-pointer ,nextarg) into mv
	  else
	  if (eq dir :out)
	  collect (list nextarg lw-type :reference-return) into dff
	  and collect t into call
	  and collect nextret into mv
	  and collect nextret into int-mv
	  else 
	  if (eq dir :inout)
	  collect (list nextarg lw-type :reference) into dff
	  and collect nextarg into macro
	  and collect nextarg into call
	  and collect nextret into mv
	  and collect nextret into int-mv
	  and collect (lispworks-sanitize-arg-type nextarg type) into one-setq
	  else 
	  collect (list nextarg lw-type) into dff
	  and collect nextarg into macro
	  and collect nextarg into call
	  and collect (lispworks-sanitize-arg-type nextarg type) into one-setq
	  finally
	  (setq macro-args macro dff-args dff macro-call call defun-args defun mv-ret mv setq-stmt one-setq int-mv-ret int-mv))

;;;So: now deal properly with return results.  Multiple-value-bind, dingdong.
    `(progn
       (ffi:define-foreign-function
	(,internal-name ,c-name :source)
	,dff-args
	,@(if (not (eq return-type :void))
	      (list :result-type (lispworks-return-type return-type))))
       (defun ,lisp-name ,macro-args
	 (let (,@defun-args)
	   ,@setq-stmt
	   (multiple-value-bind (result ,@int-mv-ret)
	       (,internal-name ,@macro-call)
	     (values result ,@mv-ret))
	   )))))


(defvar *lisp-object-registry-index* 0)
(defvar *lisp-object-registry* nil)

(defmacro register-lisp-object (obj &key reftype)
  ;;When we have weak hashtables we can differentiate reftype :weak and :strong
  ;;for now, we are permitted to ignore it.
  (declare (ignore reftype))
  `(ilu-process:without-scheduling
    (let ((index (incf *lisp-object-registry-index*)))
      (setf *lisp-object-registry* (acons index ,obj *lisp-object-registry*))
      index)))

(defmacro unregister-lisp-object (index)
  `(ilu-process:without-scheduling
    (let ((item (assoc ,index *lisp-object-registry*)))
      (when item
	(delete item *lisp-object-registry*))
      (cdr item))))

(defmacro lookup-registered-lisp-object (index)
  `(ilu-process:without-scheduling
    (let ((item (assoc ,index *lisp-object-registry*)))
      (when item
	(cdr item)))))


;;;When we get weak hash tables, this issue should be understood and revisited.
;;;See the comments in the franz file for a discussion of what it's supposed
;;;to do.
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

(defun setup-watch-connection (fn conn server)
  (ilu-process:fork-process 
   (format nil "ILU request processing thread for conn 0x~x (fd ~d) to <~a>"
	   conn (file-descriptor-of-connection conn)
	   (id-of-kernel-server server))
   #'lispworks-watch-connection fn conn server))

(defun lispworks-watch-connection (fn conn server)
  (let ((fd (file-descriptor-of-connection conn)))
    (mp:notice-fd fd)
    (loop named watch do
	  (mp:process-wait "waiting for input on fd" 
#-full-debug              #'input-available-p
#+full-debug               'input-available-p
			   fd)
	  (unless (funcall fn conn server)
	    (return-from watch nil)))))

(defun setup-new-connection-handler (fn server port)
  ;; we fork a thread to handle listening for connections, on each port
  (ilu-process:fork-process
   (format nil "ILU Listener thread for connections to <~a> (fd ~d)"
	   (id-of-kernel-server server)
	   (file-descriptor-of-mooring-of-port port))
   #'lispworks-watch-for-connections fn server port))

(defun lispworks-watch-for-connections (fn server port)
  (let ((fd (file-descriptor-of-mooring-of-port port)))
    (mp:notice-fd fd)
    (loop named watch-for do
	  (mp:process-wait "waiting for input on fd" 
#-full-debug              #'input-available-p
#+full-debug               'input-available-p
			   fd)
	  (unless (funcall fn server port)
	    (error "Mooring 0x~x, fd ~d, on server ~s, closed.~%"
		   port (file-descriptor-of-mooring-of-port port)
		   (id-of-kernel-server server))))))


(defun input-available-p (fd)
  (let ((value (sys::check-input-or-exception fd)))
    (if (eq value :exception)
	nil
      value)))

#|

;;Well despite the comments saying this needs to be defined, it isn't
;;actually *used* anywhere, so we're taking it out.

(defmacro char*-to-string (pointer)
  `(sys::make-string-from-c ,pointer))

(defmacro string-to-char* (string)
  ;;Hm, I'm not sure about this...  is real-malloc the right thing??
  (let ((alienstr (ffi::real-malloc (* 4 (1+ (length string))))))
    (sys::lisp-string-to-c alienstr string)
    alienstr))		 

|#
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
;;;  This method will call back into Lisp (at least for the lw
;;;  implementation), where a new Lisp true object will be cons'ed
;;;  up, based on the information in the instance handle passed as
;;;  a parameter.
;;;
;;;  (create-object-table OBJECT-OF-IH-FN FREE-SELF-FN) => C pointer
;;;
;;;  When called, this should return a C pointer value, of the
;;;  C type "ilu_ObjectTable", or the value 0, if no such object
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

(define-c-function ilulw_create-object-table
    "Create an object table struct for ILU"
  "ilulw_CreateObjectTable"
  (:fixnum) :pointer)

(defun create-object-table (object-of-ih-fn free-self-fn server)
  (let ((oti (register-lisp-object (list object-of-ih-fn free-self-fn server))))
    (ilulw_create-object-table oti)))



;;;The following is for locking support.  Mostly cribbed from franz impl.
(eval-when (compile load eval)
  (ffi:define-foreign-type function-pointer (:pointer :integer))

  (ffi:define-foreign-type
   locktech
   (:structure
    (lt_mcreate function-pointer)
    (lt_acquire function-pointer)
    (lt_hold function-pointer)
    (lt_release function-pointer)
    (lt_mdestroy function-pointer)
    (lt_ccreate function-pointer)
    (lt_notify function-pointer)
    (lt_cdestroy function-pointer)
    (lt_wait function-pointer)))
)

(defun create-function-pointer (foreign-callable-symbol)
  (ffi::make-alien-object
   (ffi:in-foreign-symbol-address foreign-callable-symbol)
   'function-pointer))

(defvar *alien-ptr-list* nil)

(defvar *lispworks-debug* t)

(defun lispworks-create-mutex (s1 s2)
  (when *lispworks-debug* 
    (format t "(Lispworks:  Creating mutex '~a~a'...)~%" s1 s2))
  (register-lisp-object (ilu-process:make-process-lock :name (format nil "ILU mutex '~a~a'" s1 s2))))

(ffi:foreign-callable lispworks-create-mutex (:string :string))

(defun lispworks-acquire-mutex (m)
  (when *lispworks-debug* 
    (format t "(Lispworks:  Acquiring mutex ~s)~%" (lookup-registered-lisp-object m)))
  (let ((mutex (lookup-registered-lisp-object m)))
    (unless (eq (ilu-process:current-process) (ilu-process:process-lock-locker mutex))
      (ilu-process:process-lock mutex)))
  (values))

(ffi:foreign-callable lispworks-acquire-mutex (:fixnum))

(defun lispworks-hold-mutex (m)
  (when *lispworks-debug* 
    (format t "(Lispworks:  Hold mutex ~s?)~%" (lookup-registered-lisp-object m)))
  (let ((lock (lookup-registered-lisp-object m)))
    (unless (eq (ilu-process:current-process) (ilu-process:process-lock-locker lock))
      (error "Mutex ~s not held by current process!" lock))))

(ffi:foreign-callable lispworks-hold-mutex (:fixnum))

(defun lispworks-release-mutex (m)
  (when *lispworks-debug* 
    (format t "(Lispworks:  Releasing mutex ~s)~%" (lookup-registered-lisp-object m)))
  (ilu-process:process-unlock (lookup-registered-lisp-object m))
  (values))

(ffi:foreign-callable lispworks-release-mutex (:fixnum))

(defun lispworks-destroy-mutex (m)
  (lispworks-release-mutex m)
  (unregister-lisp-object m)
  (values))

(ffi:foreign-callable lispworks-destroy-mutex (:fixnum))

(defun lispworks-cvar-create (s1 s2)
  (when *lispworks-debug* 
    (format t "(Lispworks:  Creating cvar '~a~a'...)~%" s1 s2))
  (register-lisp-object
   (ilu-process:make-condition-variable
    :name (format nil "ILU cvar '~a~a'" s1 s2))))

(ffi:foreign-callable lispworks-cvar-create (:string :string))

(defun lispworks-cvar-notify (v)
  (let ((var (lookup-registered-lisp-object v)))
    (if var
	(ilu-process:condition-variable-notify var)))
  (values))

(ffi:foreign-callable lispworks-cvar-notify (:fixnum))

(defun lispworks-cvar-wait (v m)
  (let ((var (lookup-registered-lisp-object v))
	(mutex (lookup-registered-lisp-object m)))
    (if (and var mutex)
	(ilu-process:without-scheduling
	  (when *lispworks-debug*
	    (format t "(Lispworks:  Releasing mutex ~s)~%" mutex))
	  (ilu-process:process-unlock mutex)
	  (ilu-process:condition-variable-wait var))))
  (values))

(ffi:foreign-callable lispworks-cvar-wait (:fixnum :fixnum))

(defun lispworks-cvar-destroy (v)
  (let ((var (lookup-registered-lisp-object v)))
    (if var
	(ilu-process:condition-variable-notify var)))
  (values))

(ffi:foreign-callable lispworks-cvar-destroy (:fixnum))

(ffi:define-foreign-function (ilu_set-lock-tech "ilu_SetLockTech" :source)
			     ((lt locktech)))			     

(defun initialize-locking ()
  (unless mp:*processes*
    (warn "Initializing multiprocessing.  Any code beyond this point was not executed.")
    (warn "(You may have wished to have initialized multiprocessing first.)")
    (mp:initialize-multiprocessing))
  (let ((locktech (make-locktech)))
    (setf (locktech->lt_mcreate locktech) (create-function-pointer 'lispworks-create-mutex))
    (setf (locktech->lt_acquire locktech) (create-function-pointer 'lispworks-acquire-mutex))
    (setf (locktech->lt_hold locktech) (create-function-pointer 'lispworks-hold-mutex))
    (setf (locktech->lt_release locktech) (create-function-pointer 'lispworks-release-mutex))
    (setf (locktech->lt_mdestroy locktech) (create-function-pointer 'lispworks-destroy-mutex))
    (setf (locktech->lt_ccreate locktech) (create-function-pointer 'lispworks-cvar-create))
    (setf (locktech->lt_notify locktech) (create-function-pointer 'lispworks-cvar-notify))
    (setf (locktech->lt_cdestroy locktech) (create-function-pointer 'lispworks-cvar-destroy))
    (setf (locktech->lt_wait locktech) (create-function-pointer 'lispworks-cvar-wait))
    (ilu_set-lock-tech locktech)))
