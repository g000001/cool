;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         co-defsys.l
; RCS:          $Revision: 1.1 $
; SCCS:         %A% %G% %U%
; Description:  System Definition for CommonObjects
; Author:       James Kempf, HP/DCC
; Created:      11-Mar-87
; Modified:     11-Mar-87 22:08:34 (James Kempf)
; Language:     Lisp
; Package:      COMMON-OBJECTS
; Status:       Distribution
;
; (c) Copyright 1987, HP Labs, all rights reserved.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 1987 Hewlett-Packard Corporation. All rights reserved.
;
; Use and copying of this software and preparation of derivative works based
; upon this software are permitted.  Any distribution of this software or
; derivative works must comply with all applicable United States export
; control laws.
; 
; This software is made available AS IS, and Hewlett-Packard Corporation makes
; no warranty about the software, its performance or its conformity to any
; specification.
;
; Suggestions, comments and requests for improvement may be mailed to
; aiws@hplabs.HP.COM

;;;-*-Mode:LISP; Package:(PCL LISP 1000); Base:10; Syntax:Common-lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987 Xerox Corporation.  All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their
;;; name and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox Artifical Intelligence Systems
;;;   2400 Hanover St.
;;;   Palo Alto, CA 94303
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *************************************************************************
;;;

(provide "co-defsys")

(in-package 'common-objects :nicknames '(co) :use '(lisp pcl walker))

(export '(compile-co 
	  load-co
	  run-tests
    ))
    
(require "pcl")   		;  Portable CommonLoops

(defvar *co-system-date* "3/10/87")

(defvar *co-pathname-defaults*
		(pathname "/net/hplfs2/users/kempf/public/cool/")
    
)

(defvar *co-files*
  (let ((xxx-low (or #+KCL       'kcl-low  ; placeholder
		     #+HP        'hp-low
		     nil)))
    ;; file         load           compile         files which force
    ;;              environment    environment     recompilations of
    ;;                                             this file
    `(
      (pcl-patches  nil             nil            nil)
      (co-macros    t               (pcl-patches
					(co-macros :source))  (pcl-patches))
      (co-dmeth     t               (co-macros
				     pcl-patches)
	                                           (co-macros pcl-patches))
      (co-meta      t               (co-macros
				     pcl-patches
				    (co-meta :source))
						   (co-macros pcl-patches))
      (co-dtype     t               (co-macros
				     pcl-patches)  (co-macros pcl-patches))
      (co-sfun      t               (co-macros
				     pcl-patches)  (co-macros))
    )))

(defmacro wrong-pcl-version? () 
  '(not (string-equal "2/24/87" pcl::*pcl-system-date*)))

(defmacro error-wrong-pcl ()
  '(error 
"This version of CommonObjects will only run with
Portable CommonLoops Version 'System Date 2/24/87'.
This version of PCL may be obtained by sending mail
to commonobjects-request@hplabs.hp.com"))

(defun load-co (&optional (sources-p nil))
  (when (wrong-pcl-version?) (error-wrong-pcl))
  (pcl::load-system
    (if sources-p :sources :load) *co-files* *co-pathname-defaults*)
  (provide "co"))

(defun compile-co (&optional (force-p nil))
  (when (wrong-pcl-version?) (error-wrong-pcl))
  (pcl::load-system 
      (if force-p ':force ':compile) *co-files* *co-pathname-defaults*))

(defun run-tests ()
  (load "co-test.l")
  (load "co-regress.l")
)

;;; end of co-defsys.l ;;;;;

