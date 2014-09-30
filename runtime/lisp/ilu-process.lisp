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
;;; -*- Mode:Lisp; Package: ILU-PROCESS; Syntax:COMMON-LISP; Base:10 -*-
#|

$Id: ilu-process.lisp,v 1.10 1999/08/03 01:53:29 janssen Exp $
|#

(cl:in-package :user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  ILU Process macros
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+excl
(eval-when (compile eval load)
  #+(version>= 4 1 47) (require :process)
  #-(version>= 4 1 47) (cltl1:require :process))

(cl:defpackage :ilu-process
  (:use :common-lisp)

;;; EXCL implementation -- almost everything is directly
;;; imported from the multiprocessing package

  #+excl
  (:import-from :mp
                ; from process-functions
		mp:process
		mp:process-add-arrest-reason
		mp:process-add-run-reason
		mp:process-allow-schedule
		mp:process-arrest-reasons
		mp:process-disable
		mp:process-enable
		mp:process-initial-form
		mp:process-interrupt
		mp:process-kill
		mp:process-name
		mp:process-priority
		mp:process-quantum
		mp:process-revoke-arrest-reason
		mp:process-revoke-run-reason
		mp:process-run-function
		mp:process-run-reasons
		mp:process-wait
		mp:process-wait-args
		mp:process-wait-function
		mp:process-whostate
		; from process-locks
		mp::process-lock-name
		mp:make-process-lock
		mp:process-lock
		mp:process-lock-locker
		mp:with-process-lock
		mp:without-scheduling
	        mp:process-unlock
		)

;;; LUCID implementation

  #+lucid
  (:import-from :lcl
		lcl:process-active-p
		lcl:process-allow-schedule
		lcl:process-name
		lcl:process-wait
		lcl:process-wait-function
		lcl:process-whostate
		lcl:processp
		lcl:process
		)

;;;LispWorks implementation

  #+lispworks
  (:import-from :mp
		mp:process-name
		mp:process-lock
		mp:process-unlock
		mp:process-run-reasons
		mp:process-priority
		mp:process-wait-function
		mp:process-arrest-reasons
		mp::process-active-p
		mp::process-initial-form
		mp::process-whostate)
		

;;; Export the standard process interface

  (:export
   active-processes
   all-processes
   current-process
   find-process
   fork-process
   process
   process-active-p
   process-add-arrest-reason
   process-add-run-reason
   process-alive-p
   process-allow-schedule
   process-arrest-reasons
   process-disable
   process-enable
   process-initial-form
   process-interrupt
   process-kill
   process-name
   process-priority
   process-quantum
   process-revoke-arrest-reason
   process-revoke-run-reason
   process-run-function
   process-run-reasons
   process-wait
   process-wait-args
   process-wait-function
   process-whostate
   processp
   without-scheduling
   ;; from process-locks
   make-process-lock
   process-lock
   process-lock-locker
   process-lock-name
   process-lock-p
   process-unlock
   with-process-lock
   ;; from process-utilities
   show-all-processes
   show-process
   ;; from condition-variables
   make-condition-variable
   condition-variable-name
   condition-variable-waiting-processes
   condition-variable-notify
   condition-variable-wait
   ))

(in-package :ilu-process)

#+excl
(defmacro ALL-PROCESSES ()
  "Return a list of all non-killed processes"
  'mp:*all-processes*)

#+excl
(defun PROCESS-ACTIVE-P (PROCESS)
  "Is process active, i.e., have no arrest reasons and at least one run reason"
  (check-for-process PROCESS)
  (without-scheduling
   (and (null (mp:process-arrest-reasons PROCESS))
	(mp:process-run-reasons PROCESS)
	t)
   ))

#+excl
(defmacro ACTIVE-PROCESSES ()
  "Return a list of all active processes"
  '(remove-if-not #'process-active-p mp:*all-processes*)
  )

#+excl
(defmacro CURRENT-PROCESS ()
  "Return the current process"
  'mp:*current-process*)

#+excl
(defun PROCESSP (OBJECT)
  "Is object a PROCESS?"
  (typep OBJECT 'process)
  )

#+excl
(defun FORK-PROCESS-CORE (NAME PRIORITY QUANTUM STACK-SIZE RUN-REASONS
       ARREST-REASONS BINDINGS FUNCTION ARGS)
  "Actually create the process as specified to fork-process.  If there
   are no arrest reasons and at least one run reason, then start the process.
   Otherwise, just leave the process disbaled."
  (declare (ignore STACK-SIZE))
  (let (process)
    (without-scheduling
     (setq process
	   (mp:make-process
	    :name NAME
	    :priority PRIORITY
	    :quantum QUANTUM
	    :run-reasons RUN-REASONS
	    :arrest-reasons (cons :**initialization** ARREST-REASONS)))
     (mp:process-preset process 
			#'process-initialization-function
			bindings FUNCTION ARGS)
     ;;; kludge due to bug in EXCL whereby preset incorrectly calls enable!!!!
     (mp:process-revoke-run-reason process :enable)
     (dolist (reason ARREST-REASONS)
       (mp:process-add-arrest-reason process reason))
     (dolist (reason RUN-REASONS)
       (mp:process-add-run-reason process reason))
     ;;; end kludge
     (mp:process-revoke-arrest-reason process :**initialization**)
     )
    process
    ))

#+lispworks
(defmacro WITHOUT-SCHEDULING (&body BODY)
  "do BODY with scheduling turned off"
  `(mp:without-interrupts ,@BODY)
  )

#+lispworks
(defmacro ALL-PROCESSES ()
  "Return a list of all non-killed processes"
  '(mp::all-processes))

#+lispworks
(defmacro ACTIVE-PROCESSES ()
  "Return a list of all active processes"
  (ALL-PROCESSES))

#+lispworks
(defmacro CURRENT-PROCESS ()
  "Return the current process"
  'mp:*current-process*)

#+lispworks
(defmacro PROCESSP (x)
  `(mp::process-p ,x))

#+lispworks 
(defmacro PROCESS-WAIT-ARGS (x)
  `(mp::process-wait-function-arguments ,x))

#+lispworks
;;not sure if this is correct, like it's zero for some processes, is that bad?
(defmacro PROCESS-QUANTUM (x)
  `(mp::process-time-slice ,x))

#+lispworks
(defun PROCESS-ADD-ARREST-REASON (PROCESS REASON)
  (setf (mp:process-arrest-reasons process)
	(cons reason (mp:process-arrest-reasons process))))

#+lispworks
(defun PROCESS-REVOKE-ARREST-REASON (PROCESS REASON)
  (setf (mp:process-arrest-reasons process)
	(delete reason (mp:process-arrest-reasons process))))

#+lispworks
(defun FORK-PROCESS-CORE (NAME PRIORITY QUANTUM STACK-SIZE RUN-REASONS
       ARREST-REASONS BINDINGS FUNCTION ARGS)
  "Actually create the process as specified to fork-process.  If there
   are no arrest reasons and at least one run reason, then start the process.
   Otherwise, just leave the process disbaled."
  ;;lispworks official interface is process-run-function.  Because of
  ;;fussing with arrest reasons, should use create-process,
  ;;unfortunately, despite undocumented nature.
  (declare (ignore quantum stack-size)
	   (special mp:*process-initial-bindings*))
  (let ((mp:*process-initial-bindings* mp:*process-initial-bindings*)
	process)
    (loop for (symbol value) in bindings
	  do (push (cons symbol value) mp:*process-initial-bindings*))
    (setq process
	  (mp::create-process name
			:priority (or priority mp::*default-process-priority*)
			:run-reasons run-reasons
			:arrest-reasons arrest-reasons
			:function function
			:arguments args))
    (apply 'mp::process-preset process function args)
    (if (not arrest-reasons)
	(mp:process-enable process))))



#+lucid
(defmacro ALL-PROCESSES ()
  "Return a list of all non-killed processes"
  'lcl:*all-processes*)

#+lucid
(defmacro ACTIVE-PROCESSES ()
  "Return a list of all active processes"
  'lucid::*active-processes*)

#+lucid
(defmacro CURRENT-PROCESS ()
  "Return the current process"
  'lcl:*current-process*)

#+lucid
(progn
  (defvar *process-scheduler-reasons-table* (make-hash-table :test #'eq))
  
  (defstruct (process-scheduler-reasons (:conc-name nil))
    run-reasons
    arrest-reasons
    )
  )

#+lucid
(defun PROCESS-ENABLE (PROCESS)
  "Make a process active by removing all its run and arrest reasons
   and then giving it the single run reason :enable"
  (declare (special *process-scheduler-reasons-table*))
  (check-for-process PROCESS)
  (lcl:with-scheduling-inhibited
   (let ((reasons (gethash PROCESS *process-scheduler-reasons-table*)))
     (setf (run-reasons reasons) (list :enable))
     (setf (arrest-reasons reasons) nil)
     (lcl:activate-process PROCESS))
   ))

#+lucid
(defun PROCESS-DISABLE (PROCESS)
  "Make a process inactive by revoking all of its run and arrest reasons"
  (declare (special *process-scheduler-reasons-table*))
  (check-for-process PROCESS)
  (lcl:with-scheduling-inhibited
   (let ((reasons (gethash PROCESS *process-scheduler-reasons-table*)))
     (setf (run-reasons reasons) nil)
     (setf (arrest-reasons reasons) nil)
     (lcl:deactivate-process PROCESS))
   ))
  
#+lucid
(defun PROCESS-KILL (PROCESS)
  "Unwind process and then remove it from the scheduler's tables"
  (declare (special *process-scheduler-reasons-table*))
  (check-for-process PROCESS)
  (unwind-protect
      (lcl:kill-process PROCESS)
    (remhash PROCESS *process-scheduler-reasons-table*)
    )
  )

  
#+lucid
(defun PROCESS-INTERRRUPT (PROCESS FUNCTION &rest ARGS)
  "Apply function to args in context of process"
  (check-for-process PROCESS)
  (if (null ARGS)
      (lcl:interrupt-process PROCESS FUNCTION)
    (apply #'lcl:interrupt-process PROCESS FUNCTION ARGS)
    ))

#+lucid
(defun PROCESS-INITIAL-FORM (PROCESS)
  "Return a CONS of the initial function and the initial arguments"
  (check-for-process PROCESS)
  (cons
   (lcl:process-initial-function PROCESS)
   (lcl:process-initial-arguments PROCESS))
  )

#+lucid
(defun PROCESS-WAIT-ARGS (PROCESS)
  "Return the arguments passed to the wait function"
  (lcl:process-wait-arguments PROCESS)
  )

#+lucid
(defun PROCESS-RUN-REASONS (PROCESS)
  "Return the list of run reasons for this process"
  (declare (special *process-scheduler-reasons-table*))
  (check-for-process PROCESS)
  (let ((reasons (gethash PROCESS *process-scheduler-reasons-table*)))
    (run-reasons reasons))
  )

#+lucid
(defun PROCESS-ARREST-REASONS (PROCESS)
  "Return the list of arrest reasons for this process"
  (declare (special *process-scheduler-reasons-table*))
  (check-for-process PROCESS)
  (let ((reasons (gethash PROCESS *process-scheduler-reasons-table*)))
    (arrest-reasons reasons))
  )

#+lucid
(defun REACTIVATE-PROCESS-IF-NECESSARY (PROCESS)
  "If there are no arrest reasons and at least one run reason, activate
   process."
  (declare (special *process-scheduler-reasons-table*))
  ;Assume scheduling inhibited by calling function!
  (let ((reasons (gethash PROCESS *process-scheduler-reasons-table*)))
    (if (and (null (arrest-reasons reasons))
	     (run-reasons reasons))
	(lcl:activate-process PROCESS))
    ))

#+lucid
(defun DEACTIVATE-PROCESS-IF-NECESSARY (PROCESS)
  "If there are no run reasons or at least one arrest reason,
   deactivate this PROCESS"
  (declare (special *process-scheduler-reasons-table*))
  ;Assume scheduling inhibited by calling function!
  (let ((reasons (gethash PROCESS *process-scheduler-reasons-table*)))
    (if (or  (arrest-reasons reasons)
	     (null (run-reasons reasons)))
	(lcl:deactivate-process PROCESS))
    ))
  

#+lucid
(defun PROCESS-ADD-RUN-REASON (PROCESS OBJECT)
  "Add a reason to the list of run reasons for this process"
  (declare (special *process-scheduler-reasons-table*))
  (check-for-process PROCESS)
  (lcl:with-scheduling-inhibited
   (let ((reasons (gethash PROCESS *process-scheduler-reasons-table*)))
     (pushnew OBJECT (run-reasons reasons) :test #'eq)
     (reactivate-process-if-necessary PROCESS)
    ))
  )

#+lucid
(defun PROCESS-ADD-ARREST-REASON (PROCESS OBJECT)
  "Add a reason to the list of arrest reasons for this process"
  (declare (special *process-scheduler-reasons-table*))
  (check-for-process PROCESS)
  (lcl:with-scheduling-inhibited
   (let ((reasons (gethash PROCESS *process-scheduler-reasons-table*)))
     (pushnew OBJECT (arrest-reasons reasons) :test #'eq)
     (deactivate-process-if-necessary PROCESS)
    ))
  )

#+lucid
(defun PROCESS-REVOKE-RUN-REASON (PROCESS OBJECT)
  "Remove a reason from the list of run reasons for this process"
  (declare (special *process-scheduler-reasons-table*))
  (check-for-process PROCESS)
  (lcl:with-scheduling-inhibited
   (let ((reasons (gethash PROCESS *process-scheduler-reasons-table*)))
     (setf (run-reasons reasons)
	   (delete OBJECT (run-reasons reasons) :test #'eq))
     (deactivate-process-if-necessary PROCESS)
    ))
  )

#+lucid
(defun PROCESS-REVOKE-ARREST-REASON (PROCESS OBJECT)
  "Remove a reason from the list of arrest reasons for this process"
  (declare (special *process-scheduler-reasons-table*))
  (check-for-process PROCESS)
  (lcl:with-scheduling-inhibited
   (let ((reasons (gethash PROCESS *process-scheduler-reasons-table*)))
     (setf (arrest-reasons reasons)
	   (delete OBJECT (arrest-reasons reasons) :test #'eq))
     (reactivate-process-if-necessary PROCESS)
    ))
  )


#+lucid
(defun PROCESS-PRIORITY (PROCESS)
  "Supposed to return the scheduling priority for the PROCESS.
   Can't figure out how to do that, so just return 0"
  (declare (ignore PROCESS))
  0
  )

#+lucid
(defun PROCESS-SET-PRIORITY (PROCESS NEW-PRIORITY)
  "Supposed to set the scheduling priority for the PROCESS.
   Can't figure out how to do that, so just return the new priority
   as required by setf"
  (declare (ignore PROCESS))
  NEW-PRIORITY
  )

#+lucid
(defsetf PROCESS-PRIORITY PROCESS-SET-PRIORITY
  "Doesn't really do anything!")


#+lucid
(defun PROCESS-QUANTUM (PROCESS)
  "Supposed to return the scheduling quantum for the PROCESS.
   Can't figure out how to do that, so just return the default quantum"
  (declare (ignore PROCESS))
  lcl:*scheduling-quantum*
  )

#+lucid
(defun PROCESS-SET-QUANTUM (PROCESS NEW-QUANTUM)
  "Supposed to set the scheduling quantum for the PROCESS.
   Can't figure out how to do that, so just return the new quantum
   as required by setf"
  (declare (ignore PROCESS))
  NEW-QUANTUM
  )

#+lucid
(defsetf PROCESS-QUANTUM PROCESS-SET-QUANTUM "Doesn't really do anything!")


#+lucid
(defmacro WITHOUT-SCHEDULING (&body BODY)
  "do BODY with scheduling turned off"
  `(lcl:with-scheduling-inhibited ,@BODY)
  )

#+lucid
(defmacro process-run-function (name function &rest args)
  "Run the given function in its own process.
   name - the process name must be a string or a plist '(:name \"foo\")
   function - the function to run
   args - the initial arguments
  "
   `(lcl:make-process
     ,(if (listp name)
	  (getf name :name "Anonymous")
	  name)
     :function ,function :args ,args))
  
#+lucid
(defun FORK-PROCESS-CORE (NAME PRIORITY QUANTUM STACK-SIZE RUN-REASONS
       ARREST-REASONS BINDINGS FUNCTION ARGS)
  "Actually create the process as specified to fork-process.  If there
   are no arrest reasons and at least one run reason, then start the process.
   Otherwise, just leave the process disbaled."
  (declare (ignore PRIORITY QUANTUM))
  (let (process)
    (lcl:with-scheduling-inhibited
     (setq process
	   (lcl:make-process
	    :name name 
	    :function #'process-initialization-function
	    :args (list bindings FUNCTION ARGS)
	    :stack-size stack-size))
     (setf (gethash process *process-scheduler-reasons-table*) 
	   (make-process-scheduler-reasons
	    :run-reasons run-reasons
	    :arrest-reasons arrest-reasons))
     (deactivate-process-if-necessary process)
     )
    process
    )
  )
			 
;;;
;;;
;;;    Common implementations
;;;
;;;

(defun PROCESS-INITIALIZATION-FUNCTION (BINDINGS FUNCTION ARGS)
  "Inital function for all processes.  BINDINGS is a let-like binding
   list.  Do the bindings and then apply function to the args."
  (let (
	(symbols
	 (mapcar
	  #'(lambda (binding) (if (listp binding)(car binding) binding))
	  BINDINGS))
	(values
	 (mapcar
	  #'(lambda (binding) (if (listp binding)(eval (cadr binding)) nil))
	  BINDINGS))
	)
    (progv symbols values (apply FUNCTION ARGS))
    ))
	   

(defvar *default-process-bindings* nil)

(defun FORK-PROCESS
  (NAME-OR-PROP-LIST FUNCTION &rest ARGS)
  "Fork a new process in which FUNCTION is applied to ARGS.
   NAME-OR-PROP-LIST can be a string to serve as the name of the process
   or it can be a prop-list used to set various parameters of the process.
   This proplist can contain the following key-property pairs:
      :NAME -- a string to be used as the name of the process
      :PRIORITY -- sets the priority of the process to the given value
      :QUANTUM -- sets the quantum of the process to the given value
      :STACK-SIZE -- sets the stack-size of the process (if possible in
                     this implementation)
      :RUN-REASONS -- sets the run reasons of this process to the given
                      list.  Unless run reasons is non-nil, the forked
                      process will not run until an process-add-run-reasons is
                      done.  Defaults to '(:start).
      :ARREST-REASONS -- sets the arrest reasons of this process to the given
                      list.  If arrest-reasons is non-nil,  the forked
                      process will not run until a
                      process-revoke-arrest-reasons is done. 
                      Defaults to nil.
      :BINDINGS -- is a list of bindings (as in let) that will be done
                   in the context of the forked process before the function is
                   run.  Defaults to *default-process-bindings*.
   "
#+lucid
  (declare (special *process-scheduler-reasons-table*))
  (if (null (listp NAME-OR-PROP-LIST))
      (setq NAME-OR-PROP-LIST (list :name NAME-OR-PROP-LIST)))
  (let(
       (name (getf NAME-OR-PROP-LIST :name (symbol-name (gensym))))
       (priority (getf NAME-OR-PROP-LIST :priority))
       (quantum (getf NAME-OR-PROP-LIST :quantum))
       (stack-size (getf NAME-OR-PROP-LIST :stack-size 5000))
       (run-reasons (getf NAME-OR-PROP-LIST :run-reasons '(:start)))
       (arrest-reasons (getf NAME-OR-PROP-LIST :arrest-reasons))
       (bindings
	(getf NAME-OR-PROP-LIST :bindings *default-process-bindings*))
       )
    (if (not (stringp name))
	(error "Process name not stringp: ~S" name))
    (if (and priority (not (numberp priority)))
	(error "Process priority not numberp: ~S" priority))
    (if (and quantum (not (numberp quantum)))
	(error "Process quantum not numberp: ~S" quantum))
    (if (and stack-size (not (numberp stack-size)))
	(error "Process stack-size not numberp: ~S" stack-size))
    (fork-process-core name
		       priority
		       (or quantum 1)
		       stack-size
		       run-reasons
		       arrest-reasons
		       bindings
		       FUNCTION
		       ARGS)
    ))


(defun FIND-PROCESS (NAME)
  "Return the process named NAME (a string), if any"
  (dolist (Process (ALL-PROCESSES))
    (if (string= NAME (process-name Process))
	(return Process))
    ))


(defun CHECK-FOR-PROCESS (PROCESS)
  "Make sure that process is a process object"
  (if (not (processp PROCESS))
      (error "~S is not a process!" PROCESS))
  )

(defun PROCESS-ALIVE-P (PROCESS)
  "Return non-nil if process is alive -- i.e., hasn't been killed"
  (if (member PROCESS (all-processes) :test #'eq)
      t nil)
  )

;;; MAKE-PROCESS-LOCK
;;;  "builds a shared resource process lock object
;;;   ARGUMENTS:
;;;    &key name
;;;   RETURNS:
;;;    the process lock object
;;;   SIDE-EFFECTS:
;;;    conses up the lock object
;;;  "

;;; excl make-process-lock imported from mp:


#+lucid
(defstruct (process-lock (:predicate nil))
  name
  value)

#+lucid
(defun PROCESS-LOCK-P (OBJECT)
   "Return non-nil if OBJECT is a process-lock"
  (typep OBJECT 'process-lock))

#+lispworks
(defmacro make-process-lock (&rest keywords)
  `(mp::make-lock ,@keywords))

#+lispworks
(defmacro process-lock-p (object)
  `(mp::lock-p ,object))

#+lispworks
(defmacro with-process-lock ((lock &rest keys) &body body)
  `(mp:with-lock (,lock ,@keys)
     ,@body))

#+lispworks
(defmacro process-lock-locker (object)
  `(mp:lock-owner ,object))

;;;
;;;
;;;    WITH-PROCESS-LOCK macro
;;;
;;;

#+lucid
(defmacro with-process-lock ((lock &key norecursive) &body body)
  "evaluates its body with the shared resource process lock LOCK locked.
   ARGUMENTS:
    LOCK - the process lock for the resource in question
    NORECURSIVE - don't allow recursive access within the same process
    BODY - the forms to be locked
   RETURNS:
    don't care
   SIDE-EFFECTS:
    no other process may access the locked resource while this body is
    executed.
  "
  `(let ((locked nil))
     (unwind-protect
	 (progn
	   (lcl:with-scheduling-inhibited
	     ,(if norecursive
		  `(cond
		     ((eq (process-lock-locker ,lock) lcl:*current-process*)
		      (error "Recursive attempt to size lock ~S by ~S."
			     ,lock lcl:*current-process*))
		     (t
		      (loop 
			(when
			  (eq (process-lock-locker ,lock) nil)
			  (setf (process-lock-value ,lock)
				(or lcl:*current-process* t))
			  (setq locked t)
			  (return))
			(lcl:with-scheduling-allowed
			 (process-wait
			  "lock"
			  #'(lambda ()
			      (null (process-lock-value ,lock))))))))
		  `(cond
		     ((eq (process-lock-value ,lock) lcl:*current-process*))
		     (t
		       (loop 
			(when
			  (eq (process-lock-value ,lock) nil)
			  (setf (process-lock-value ,lock)
				(or lcl:*current-process* t))
			  (setq locked t)
			  (return))
			(lcl:with-scheduling-allowed
			(process-wait
			 "lock"
			 #'(lambda ()
			     (null (process-lock-value ,lock))))))))))
	   ,@body)
       (when locked
	 (lcl:with-scheduling-inhibited
	   (if (eq (process-lock-value ,lock) (or lcl:*current-process* t))
	       (setf (process-lock-value ,lock) nil)
	       (error "Process ~S tried to unlock ~S"
		      (or lcl:*current-process* t) ,lock)))))))



;;;
;;;
;;;    PROCESS-LOCK, PROCESS-UNLOCK
;;;
;;;

;;; excl process-lock, process-unlock imported directly from mp: package
;;; except for process-unlock which is temporarily redefined here becuase
;;; of the bug with mp::*sequence-break-pending* not being reset in the scheduler

;#+excl
;(defun process-unlock (lock &optional (lock-value mp:*current-process*))
;  (unless (eq lock-value (process-lock-locker lock))
;    (error "~s attempted to unlock ~s with incorrect value ~s instead of ~s"
;	   mp:*current-process* lock lock-value (process-lock-locker lock)))
;;;;  (without-scheduling ;)
;  (mp::without-scheduling-internal ;this is the change
;   (setf (process-lock-locker lock) nil)
;   (when (mp::process-lock-waiting lock)
;     ;; The next line is potentially efficient, but loses if the next process
;     ;; is disabled, and even worse if the process has been killed.
;     ;;   (delete lock (mp::process-arrest-reasons
;     ;;                  (pop (mp::process-lock-waiting lock))))
;     ;; So we have to wake up everybody, and let them fight over it.
;     (dolist (p (mp::process-lock-waiting lock))
;       (mp::process-revoke-arrest-reason p lock))
;     (setf (mp::process-lock-waiting lock) nil))))


#+lucid
(defun PROCESS-LOCK
  (LOCK &optional (LOCK-VALUE lcl:*current-process*) (WHOSTATE "Lock"))
  "Wait for lock to become free and then seize the lock, putting
   the current process in as the value of the lock"
  (declare (ignore whostate))
  (lcl:process-lock (process-lock-value lock) lock-value))


#+lucid
(defun PROCESS-UNLOCK
  (LOCK &optional (LOCK-VALUE lcl:*current-process*) (ERROR-P t))
  "release the lock.  If lock-value (default current-process) does not 
   match the lock's value signal an error if error-p is non-nil (default)"
  (declare (ignore error-p))
  (lcl:process-unlock (process-lock-value lock) lock-value))
			    
;;;
;;;
;;;     PROCESS-LOCK-LOCKER
;;;
;;;

;;; excl process-lock-locker imported directly from mp: package

#+lucid
(defun PROCESS-LOCK-LOCKER (LOCK)
  "Return the process that is currently locking this lock"
   (process-lock-value LOCK)
  )

(defun show-process (&optional (process (current-process))
                               (stream *standard-output*)
                               (verbose t))
  "
  Display information about a process, PROCESS may be a process object, or
  its name (as a string or a symbol, symbols will be downcased to get the name.
  Output will be to STREAM, if VERBOSE is nil then only non-nil fields will be
  displayed and ILU-PROCESS:PROCESS-INITIAL-FORM will not be shown.
  "
  (cond
   ((stringp process)
    (setq process (find-process process)))
   ((symbolp process)
    (setq process (find-process (string-downcase (symbol-name process)))))
   )
  (unless (processp process)
    (error 
     "Arg to ~S (which was ~S) is not a process, or the name of a process"
     'show-process
     process))
  (let (
        (*standard-output* stream)
        )
    (format t "Process: ~S~%" (process-name process))
    (when (or verbose (process-alive-p process))
      (format t "  Process-alive-p: ~S~%" (process-alive-p process)))
    (when (or verbose (process-active-p process))
      (format t "  Process-active-p: ~S~%" (process-active-p process)))
    ;;(format t "  Process-name: ~S~%" (process-name process))
    (when (or verbose (process-whostate process))
      (format t "  Process-whostate: ~S~%" (process-whostate process)))
    (when  verbose 
      (format t "  Process-initial-form: ~S~%"
              (process-initial-form process)))
    (when (or verbose (process-wait-function process))
      (format t "  Process-wait-function: ~S~%"
            (process-wait-function process)))
    (when (or verbose (process-wait-args process))
      (format t "  Process-wait-args: ~S~%" (process-wait-args process)))
    (when (or verbose (process-quantum process))
      (format t "  Process-quantum: ~S~%" (process-quantum process)))
    (when (or verbose (process-priority process))
      (format t "  Process-priority: ~S~%" (process-priority process)))
    (when (or verbose (process-run-reasons process))
      (format t 
	      "  Process-run-reasons: ~S~%" 
	      (process-run-reasons process)))
    (when (or verbose (process-arrest-reasons process))
      (format t "  Process-arrest-reasons: ~S~%"
            (process-arrest-reasons process)))
    (finish-output)
    ))

(defun show-all-processes (&optional (stream *standard-output*)
                                   (verbose nil))
  "
  Use show-process to display info about all processes returned by
  all-processes. Output to STREAM, show only non-nil field unless
  VERBOSE is non-nil.
  "
  (format stream "-------------Data on all processes follows---------~%")
  (dolist (process (all-processes))
    (show-process process stream verbose)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Condition variables (portable implementation)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct condition-variable
  (name)		;; name for debugging
  (waiting-processes))	;; processes waiting on this var

(defmacro condition-variable-wait (v)
  "Wait for someone to \"notify\" this condition variable.
   The current process is arrested until this occurs."
  `(without-scheduling
     (let ((p (current-process)))
       (push p (condition-variable-waiting-processes ,v))
       (process-add-arrest-reason p ,v))))
   
(defmacro condition-variable-notify (v)
  "\"Notify\" all processes waiting on this condition variable."
  `(without-scheduling
     (dolist (p (condition-variable-waiting-processes ,v))
       (process-revoke-arrest-reason p ,v))
     (setf (condition-variable-waiting-processes ,v) nil)))
