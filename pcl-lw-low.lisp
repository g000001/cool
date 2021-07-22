;;; -*- Mode: LISP; Syntax: Common-lisp; Package: (PCL Lisp 1000); Base: 10. -*-
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
;;; The version of low for Kyoto Common Lisp (KCL)
(in-package :cool.pcl)

  ;;   
;;;;;; Load Time Eval
  ;;
;;; 

;;; This doesn't work because it looks at a global variable to see if it is
;;; in the compiler rather than looking at the macroexpansion environment.
;;; 
;;; The result is that if in the process of compiling a file, we evaluate a
;;; form that has a call to load-time-eval, we will get faked into thinking
;;; that we are compiling that form.
;;;
;;; THIS NEEDS TO BE DONE RIGHT!!!
;;; 
;(defmacro load-time-eval (form)
;  ;; In KCL there is no compile-to-core case.  For things that we are 
;  ;; "compiling to core" we just expand the same way as if were are
;  ;; compiling a file since the form will be evaluated in just a little
;  ;; bit when gazonk.o is loaded.
;  (if (and (boundp 'compiler::*compiler-input*)  ;Hack to see of we are
;	   compiler::*compiler-input*)		  ;in the compiler!
;      `'(si:|#,| . ,form)
;      `(progn ,form)))


  ;;   
;;;;;; The %instance datastructure.
  ;;   


  ;;   
;;;;;; Generating CACHE numbers
  ;;
;;; This needs more work to be sure it is going as fast as possible.
;;;   -  The calls to si:address should be open-coded.
;;;   -  The logand should be open coded.
;;;   

(defmacro symbol-cache-no (symbol mask)
  (if (and (constantp symbol)
	   (constantp mask))
      `(load-time-value  
        (logand (ash (system:OBJECT-ADDRESS ,symbol) -2) ,mask))
      `(logand (ash (the fixnum (system:OBJECT-ADDRESS ,symbol)) -2) ,mask)))

(defmacro object-cache-no (object mask)
  `(logand (the fixnum (system:OBJECT-ADDRESS ,object)) ,mask))

  ;;   
;;;;;; printing-random-thing-internal
  ;;
(defun printing-random-thing-internal (thing stream)
  (format stream "~O" (system:OBJECT-ADDRESS thing)))




