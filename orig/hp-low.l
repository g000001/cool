;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         new-hp-low.l
; SCCS:         %A% %G% %U%
; Description:  Revised hp-low.l
; Author:       James Kempf, HP/DCC
; Created:      16-Jul-86
; Modified:     26-Feb-87 13:35:43 (James Kempf)
; Language:     Lisp
; Package:      USER
; Status:       Experimental (Do Not Distribute)
;
; (c) Copyright 1986, James Kempf, all rights reserved.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; -*- Mode:LISP; Package:(PCL LISP 1000); Base:10; Syntax:Common-lisp -*-
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
;;; This is the HP Common Lisp version of the file low.
;;;
;;; 

(in-package 'pcl)

  ;;   
;;;;;; Load Time Eval
  ;;
;;;
;;; #, is woefully inadequate.  You can't use it inside of a macro and have
;;; the expansion of part of the macro be evaluated at load-time its kind of
;;; a joke.  load-time-eval is used to provide an interface to implementation
;;; dependent implementation of load time evaluation.
;;;
;;; A compiled call to load-time-eval:
;;;   should evaluated the form at load time,
;;;   but if it is being compiled-to-core evaluate it at compile time
;;; Interpreted calls to load-time-eval:
;;;   should just evaluate form at run-time.
;;; 
;;; The portable implementation just evaluates it every time, and PCL knows
;;; this.  PCL is careful to only use load-time-eval in places where (except
;;; for performance penalty) it is OK to evaluate the form every time.
;;; 
;;(defmacro load-time-eval (form)
;;  `(progn ,form))
;;(defmacro load-time-eval (form)
;;   `(impl::loadtime ,form))

(defmacro load-time-eval (form)
  `(eval-when (load eval) ,form))  


(setq *class-of*
	'(lambda (x) 
	   (cond ((%instancep x)
		  (%instance-class-of x))
		 ;; Ports of PCL should define the rest of class-of
		 ;; more meaningfully.  Because of the underspecification
                 ;; of type-of this is the best that I can do.
		 ((null x)
                  (class-named 'null))
                 ((stringp x)
                  (class-named 'string))
		 ((characterp x)
		  (class-named 'character))
		 (t
		  (or (class-named (type-of x) t)
		      (error "Can't determine class of ~S." x)
		  )
		)
            )
        )
)

(eval-when (load eval)
  (recompile-class-of)
)
  ;;   
;;;;;; Cache No's
  ;;  

;;; Grab the top 29 bits
;;;
(defmacro symbol-cache-no (symbol mask)
;`(logand (prim:@inf ,symbol) ,mask)			;	33% hit rate
  `(logand (ash (prim:@inf ,symbol) -5) ,mask))		;	83% hit rate
;   `(the extn::index (logand (prim::@>> ,symbol 4) ,mask)))  ; 75% hit rate

(defmacro object-cache-no (symbol mask)
  `(logand (ash (prim:@inf ,symbol) -5) ,mask))

  ;;   
;;;;;; printing-random-thing-internal
  ;;
(defun printing-random-thing-internal (thing stream)
  (format stream "~O" (prim:@inf thing)))


