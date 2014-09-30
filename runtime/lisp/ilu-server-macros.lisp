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

$Id: ilu-server-macros.lisp,v 1.23 1999/08/03 01:53:32 janssen Exp $
|#

(cl:in-package :ilu)

;;;; DEFINE-SERVER-CLASS:  expands into server

(defmacro define-server-class (class superclass)
  `(progn
     (eval-when (compile eval load) (export '(,class)))
     (defclass ,class (ilu-true-object ,superclass) ())))

;;;; DEFINE-METHOD-SERVER-STUB: expands into a defun which calls a method

(defmacro define-method-server-stub
   (name old-name id singleton-p functional-p asynchronous-p args exceptions return-type)
  (declare (ignore singleton-p functional-p))
  #-ilu-old-method-names (declare (ignore old-name))
  (destructuring-bind ((self self-type) &rest other-args) args
    #-ilu-old-method-names
    (declare (ignore self))
    (let ((other-arg-types (mapcar #'second other-args))
	  #+ilu-old-method-names
	  (other-arg-names (mapcar #'first other-args))
	  (other-arg-dirs (mapcar #'third other-args))
	  (input-arg-types (mapcar #'second
				   (remove-if #'(lambda (arg)
						  (eq (third arg)
						      :out)) other-args)))
	  (has-return-values
	   (and (not asynchronous-p)
		(remove-nils
		 (cons return-type
		       (mapcar #'(lambda (dir)
				   (or (eq dir :out) (eq dir :inout)))
			       (mapcar #'third other-args)))))))
      `(progn
	 (defmethod call-server-stub-for-method-name
	     (call (method-id (eql ,(intern (string id) :keyword)))
	      (self ,self-type))
	   ,(let ((the-method-stub-form
		   `(let ((return-value
			   (,(if (< 1 (length has-return-values))
				 'cl:multiple-value-list 'cl:list)
			       ,(if (symbolp name)
				    ;; normal method
				    `(apply #',name self
					    (let ((args
						   (list
						    ,@(mapcar
						       #'(lambda (type)
							   (make-read-form `call type))
						       input-arg-types))))
					      (ilu_request-read call)
					      args))
				  ;; setf method, must reverse arguments
				  `(funcall #',name
					    (let ((arg ,(make-read-form
							 `call (first input-arg-types))))
					      (ilu_request-read call)
					      arg)
					    self)))))
		      ,@(remove-nils
			 (list
			  (unless has-return-values
			    `(declare (ignore return-value)))
			  (if asynchronous-p
			      `(ilu_no-reply call)
			    `(progn
			       (begin-reply
				call ,(if exceptions 't 'nil)
				(+ (if (call-needs-sizing call)
				       (begin-sizing-reply
					call
					,(if exceptions 't 'nil))
				     0)
				   ,@(remove-nils
				      (let ((index -1))
					(cons
					 (if return-type
					     (make-size-form
					      `call return-type
					      `(nth ,(incf index) return-value)))
					 (mapcar
					  #'(lambda (dir type)
					      (unless (eq dir :in)
						(make-size-form
						 `call type
						 `(nth
						   ,(incf index) return-value))))
					  other-arg-dirs other-arg-types))))))
			       ,@(remove-nils
				  (let ((index -1))
				    (cons
				     (if return-type
					 (make-write-form
					  `call return-type `(nth ,(incf index)
								  return-value)))
				     (mapcar
				      #'(lambda (dir type)
					  (unless (eq dir :in)
					    (make-write-form
					     `call type `(nth ,(incf index)
							      return-value))))
				      other-arg-dirs other-arg-types))))
			       (finish-reply call))))))))
	      (if exceptions
		  `(handler-bind
		       ,@(remove-nils 
			  (list
			   (mapcar
			    #'(lambda (exception)
				`(,exception
				  #'(lambda (e)
				      (signal-exception
				       call ,(1+ (position exception exceptions)) e)
				      (return-from
					  call-server-stub-for-method-name))))
			    exceptions)))
		       ,the-method-stub-form)
		the-method-stub-form)
	      )
	   )
	 #+ilu-old-method-names
	 (defmethod ,old-name
	     ,(if (symbolp old-name)
		  ;; normal method
		  `((,self ,self-type)
		    ,@(remove-nils
		       (mapcar #'(lambda (dir name)
				   (unless (eq dir :out) name))
			       other-arg-dirs other-arg-names)))
		;; setf method, must reverse arguments
		`(,(first other-arg-names)
		  ;;(,self ,(second self-type))))
		  (,self ,self-type)))
	 ;;    (,name
	   ;;  ,@
	   ,(if (symbolp old-name)
		    ;; normal method
		    `(,name ,self
		      ,@(remove-nils
			 (mapcar #'(lambda (dir name)
				     (unless (eq dir :out) name))
				 other-arg-dirs other-arg-names)))
		  ;; setf method, must reverse arguments
	     `(setf (,(second name) ,self)
		,(first other-arg-names))))
	 ))))
