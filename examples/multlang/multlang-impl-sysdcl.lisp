;;;-*- Package: USER; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-
;; $Id: multlang-impl-sysdcl.lisp,v 1.3 1999/08/03 01:57:50 janssen Exp $
;; BeginILUCopyright
;; Last edited by Mike Spreitzer October 9, 1998 10:05 am PDT

;; Copyright (c) 1991-1999 Xerox Corporation.  All Rights Reserved.

;; Unlimited use, reproduction, modification, and distribution of this
;; software and modified versions thereof is permitted.  Permission is
;; granted to make derivative works from this software or a modified
;; version thereof.  Any copy of this software, a modified version
;; thereof, or a derivative work must include both the above copyright
;; notice of Xerox Corporation and this paragraph.  Any distribution of
;; this software, a modified version thereof, or a derivative work must
;; comply with all applicable United States export control laws.  This
;; software is made available AS IS, and XEROX CORPORATION DISCLAIMS ALL
;; WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
;; LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
;; EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
;; NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGES.

;; EndILUCopyright


(in-package :user)

(eval-when (load eval)
  (pdefsys:load-system :ilu))

(pdefsys:defsystem :multlangimpl
	(:load-before-compile (:multlang)
	 :needed-systems (:ilu :multlang))
  ("multlang-c++-side" :language :library :binary-only t :binary-pathname "multlang-c++-side.so")
  ("multlang-impl" :load-before-compile t))

