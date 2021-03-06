;;;-*-Mode:LISP; Package: PCL; Base:10; Syntax:Common-lisp; -*-
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

(in-package :cool.pcl)

(defmacro run-super () '(call-next-method))


(defun convert-with-first-arg (first-arg use-slot-value)
  (iterate ((opc in first-arg))
    (or (listp opc) (setq opc (list opc)))
    (collect
      ;; Can't use the obvious backquote in Genera!
      (let ((entry ()))
	(when use-slot-value
	  (push t entry)
	  (push :use-slot-value entry))
	(when (cddr opc)
	  (push (caddr opc) entry)
	  (push :class entry))
	(when (cadr opc)
	  (push (cadr opc) entry)
	  (push :prefix entry))
	(cons (car opc) entry)))))

(defmacro with (objects-prefixes-and-classes &body body)
  `(with-slots ,(convert-with-first-arg objects-prefixes-and-classes nil)
     . ,body))

(defmacro with* (objects-prefixes-and-classes &body body)
  `(with-slots ,(convert-with-first-arg objects-prefixes-and-classes t)
     . ,body))

