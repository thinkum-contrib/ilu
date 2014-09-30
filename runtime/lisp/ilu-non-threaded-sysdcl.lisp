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

$Id: ilu-non-threaded-sysdcl.lisp,v 1.4 1999/08/03 01:53:27 janssen Exp $
|#

(in-package :user)

(eval-when (compile load eval)
  (defparameter *ilu-c-include-location*
      (let ((build (pdefsys:get-environment-variable "ILU_BUILD_ENV"))
	    (iluhome (pdefsys:get-environment-variable "ILUHOME")))
	(concatenate 'string "-I"
		     (if build build
		       (if iluhome (concatenate 'string iluhome "/include")
			 "../kernel"))))))

(eval-when (compile load eval)
  (defparameter *ilu-library*
      (let ((build (pdefsys:get-environment-variable "ILU_BUILD_ENV"))
	    (iluhome (pdefsys:get-environment-variable "ILUHOME")))
	(if build
	    (concatenate 'string build "/libilu.a")
	  (if iluhome (concatenate 'string iluhome "/lib/libilu.a")
	    "../kernel/libilu.a")))))

(pdefsys:defsystem :ilu
  (:default-optimizations ((speed 3) (safety #+excl 1 #-excl 0)))
  ("ilu-def-package" :load-before-compile t)	;; build package :ilu and export symbols from it
  ("ilu-lisp-skin" :language :ansi-c		;; Import C ILU runtime kernel, plus C-to-CL shim
   :optimizations ( #.*ilu-c-include-location* #+PARC "-g" )
   :pathname "ilu-lisp-skin.c"
   :libraries (#.*ilu-library*))

  #+excl
  ("ilu-franz-skin-non-threaded" :language :ansi-c		;; specific Franz EXCL C glue
   :optimizations (#.*ilu-c-include-location* #+PARC "-g" #+allegro-v4.2 "-DALLEGRO_4_2")
   :pathname "ilu-franz-skin-non-threaded.c")

  ("ilu-process" :load-before-compile t)	;; implement generic lightweight process model

  #+excl ("ilu-franz-non-threaded" :load-before-compile t)	;; define Franz-EXCL-specific items
  #-excl ("lose-badly")

  ("ilu-kernel" :load-before-compile t)		;; define C functions imported from ILU C kernel
  ("ilu-marshalling" :load-before-compile t)	;; define marshalling primitives
  ("ilu" :load-before-compile t)		;; implementation of ILU CL runtime
  ("ilu-macros" :load-before-compile t)		;; macros for implementing ISL types
  ("ilu-types" :load-before-compile t)		;; define ilu.CString type from ilu.isl
  ("ilu-server-macros" :load-before-compile t)	;; define macros for server subtypes of ISL types
  ("ilu-server" :load-before-compile t)		;; implementation of ILU CL Server runtime
  )
