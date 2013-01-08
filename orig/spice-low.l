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
;;; This is the Spice Lisp version of the file portable-low.
;;;
;;; History:
;;;    7-Dec-86
;;;       Rick Busdiecker (rfb) at Carnegie-Mellon University
;;;          Added suggested change from Gregor Kiczales @ Parc
;;;    ??-???-??
;;;	  CMU:     David B. McDonald (412)268-8860
;;;	     Modified.
;;;    ??-???-??
;;;	  Skef Wholey at Carnegie-Mellon University
;;;	     Created.
;;;
;;;
;;; 

(in-package 'pcl)

  ;;   
;;;;;; Cache No's
  ;;  

;;; Abuse the type declaration, but it generates great code.

(defun symbol-cache-no (symbol mask)
  (logand (the fixnum (%primitive lisp::make-immediate-type
				  symbol
				  system::%+-fixnum-type))
	  (the fixnum mask)))

(clc::deftransform symbol-cache-no symbol-cache-no-transform (symbol mask)
  `(logand (the fixnum (%primitive lisp::make-immediate-type
				   ,symbol
				   system::%+-fixnum-type))
	   (the fixnum ,mask)))

(defun object-cache-no (symbol mask)
  (logand (the fixnum (%primitive lisp::make-immediate-type
				  symbol
				  system::%+-fixnum-type))
	  (the fixnum mask)))

(clc::deftransform object-cache-no object-cache-no-transform (symbol mask)
  `(logand (the fixnum (%primitive make-immediate-type
				   ,symbol
				   system::%+-fixnum-type))
	   (the fixnum ,mask)))



(eval-when (load)
  (setq *class-of*		
	'(lambda (x) 
	   (or (and (%instancep x)
		    (%instance-class-of x))
	      ;(%funcallable-instance-p x)

	       (and (null object) (class-named 'nil))
	       (and (stringp object) (class-named 'string))
	       (and (ratiop object) (class-named 'rational))
	       (and (streamp object) (class-named 'stream))
	       
	       (class-named (type-of x) t)
	       (error "Can't determine class of ~S" x)))))

