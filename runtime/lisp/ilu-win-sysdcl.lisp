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

$Id: ilu-win-sysdcl.lisp,v 1.3 1999/08/03 01:53:35 janssen Exp $
|#

(in-package :user)

(pushnew :ilu-pickle cl:*features*)

(pushnew :ilu-type-info cl:*features*)

(pushnew :ilu-iiop cl:*features*)

;; (pushnew :ilu-old-method-names cl:*features*)

(pdefsys:defsystem :ilu

  ;; these are also the Allegro defaults
  (:default-optimizations ((speed 3) (safety 2) (space 0)))

  ;; build package :ilu and export symbols from it
  ("ilu-def-package" :load-before-compile t)

  ;; implement generic lightweight process model
  ;; ("ilu-process" :load-before-compile t)

  ;; define Franz-Windows-specific items
  ("ilu-franz-win" :load-before-compile t)

  ;; define C functions imported from ILU C kernel
  ("ilu-kernel" :load-before-compile t)

  ;; define marshalling primitives
  ("ilu-marshalling" :load-before-compile t)

  ;; implementation of ILU CL runtime
  ("ilu" :load-before-compile t)

  ;; macros for implementing ISL types
  ("ilu-macros" :load-before-compile t)

  ;; define ilu.CString type from ilu.isl
  ("ilu-types" :load-before-compile t)

  ;; define macros for server subtypes of ISL types
  ("ilu-server-macros" :load-before-compile t)

  ;; implementation of ILU CL Server runtime
  ("ilu-server" :load-before-compile t))
