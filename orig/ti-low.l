;;; -*- Mode:LISP; Package:(PCL Lisp 1000); Base:10.; Syntax:Common-lisp; Patch-File: Yes -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985 Xerox Corporation.  All rights reserved.
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
;;; This is the 3600 version of the file portable-low.
;;;

(in-package 'pcl)

(defmacro without-interrupts (&body body)
  `(zl:without-interrupts ,.body))

  ;;   
;;;;;; Cache No's
  ;;  

(defmacro symbol-cache-no (symbol mask)
  `(logand (si::%pointer ,symbol) ,mask))

(defmacro object-cache-no (object mask)
  `(logand (si::%pointer ,object) ,mask))

  ;;   
;;;;;; printing-random-thing-internal
  ;;
(defun printing-random-thing-internal (thing stream)
  (format stream "~O" (si:%pointer thing)))

(eval-when (compile load eval)             ;There seems to be some bug with
  (setq si::inhibit-displacing-flag t))	   ;macrolet'd macros or something.
					   ;This gets around it but its not
					   ;really the right fix.

