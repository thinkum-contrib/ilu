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
;;; $Id: compile-files.lisp,v 1.11 1999/08/03 01:53:30 janssen Exp $
;;;
;;;

(in-package :user)

(let (#+aclpc (*default-pathname-defaults* *load-pathname*))
  (load #+aclpc "pdefsys.lisp" #-aclpc "./pdefsys.lisp"))

(let (#+aclpc (*default-pathname-defaults* *load-pathname*))

  (defun file-needs-compilation (file)
    (let ((source-filename (make-pathname :name file :type "lisp"))
	  (object-filename (make-pathname
			    :name file :type (cadr pdefsys::lisp-file-types))))
      (or (not (probe-file object-filename))
	  (> (file-write-date source-filename)
	     (file-write-date object-filename)))))

  (if (file-needs-compilation "pdefsys")
      (compile-file #+aclpc "pdefsys.lisp" #-aclpc "./pdefsys.lisp"))

  (load "ilu-sysdcl")

  (let ((*ilu-library-build-location* "../kernel"))
    (handler-bind ((excl:compiler-undefined-functions-called-warning
		   #'(lambda (c) (declare (ignore c)) (muffle-warning))))
     (pdefsys:compile-system :ilu)))

  #-aclpc (exit))
