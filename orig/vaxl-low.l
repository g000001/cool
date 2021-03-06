;;; -*- Mode: LISP; Syntax: Common-lisp; Package: (PCL Lisp 1000); Base: 10. -*-
;;;
;;; *******************************************************************************
;;; Copyright (c) 1985 Xerox Corporation.  All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works based upon
;;; this software are permitted.  Any distribution of this software or derivative
;;; works must comply with all applicable United States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no warranty
;;; about the software, its performance or its conformity to any specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their name
;;; and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox Artifical Intelligence Systems
;;;   2400 Hanover St.
;;;   Palo Alto, CA 94303
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *******************************************************************************
;;;
;;; The version of low for VAXLisp
(in-package 'pcl)

  ;;   
;;;;;; Load Time Eval
  ;;
(defmacro load-time-eval (form)
  `(progn ,form))

  ;;   
;;;;;; Generating CACHE numbers
  ;;
;;; How are symbols in VAXLisp actually arranged in memory?
;;; Should we be shifting the address?
;;; Are they relocated?
;;; etc.

(defmacro symbol-cache-no (symbol mask)
  `(logand (the fixnum (system::%sp-pointer->fixnum ,symbol)) ,mask))

(defmacro object-cache-no (object mask)
  `(logand (the fixnum (system::%sp-pointer->fixnum ,object)) ,mask))

  ;;   
;;;;;; printing-random-thing-internal
  ;;
(defun printing-random-thing-internal (thing stream)
  (format stream "~O" (system::%sp-pointer->fixnum thing)))


(defun function-arglist (fn)
  (system::function-lambda-vars (symbol-function fn)))

