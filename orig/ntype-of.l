;;;-*- Mode:LISP; Package: (ntype-of lisp); Base:10; Syntax:Common-lisp -*-
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

(in-package 'ntype-of)

(defvar *portable-types*
  `(number
    (ratio 1/2)
    (complex #c(1 2) complexp)
    ((integer fixnum bignum) 1 integerp)
    ((float short-float single-float double-float long-float) 1.1 floatp)
    (null () null)
    ((character standard-char string-char) #\a characterp)
    (simple-bit-vector #*101 simple-bit-vector-p)
    (bit-vector ,(make-array 3 :element-type 'bit) bit-vector-p)
    (simple-array ,(make-array 10))
    (string ,(make-string 3) stringp)
    (simple-vector #(1 2 3))
    (array (make-array 3 :displaced-to (make-array 3)) arrayp)
    ))

(defvar *portable-types*
  `(t
    (array (make-array 3 :displaced-to (make-array 3)) arrayp)
    (simple-bit-vector #*101 simple-bit-vector-p)
    (bit-vector ,(make-array 3 :element-type 'bit) bit-vector-p)
    (simple-array ,(make-array 10))
    ))

(defvar *portable-type-lattice*)

(defstruct (node (:conc-name node-)
		 (:constructor make-node (type entry))
		 (:print-function
		   (lambda (node stream d)
		     (declare (ignore d))
		     (format stream "#<node ~S ~:S ~:S>"
			     (node-type node)
			     (mapcar #'node-type (node-supers node))
			     (mapcar #'node-type (node-subs node))))))
  type
  supers
  subs
  entry)

(defun make-type-lattice ()
  (macrolet ((memq (x l) `(member ,x ,l :test #'eq))
	     (delq (x l) `(delete ,x ,l :test #'eq)))
    (flet ((entry-type (entry)			        ;type of an element 
	     (cond ((symbolp entry) entry)	        ;of *portable-types*
		   ((symbolp (car entry)) (car entry))	
		   (t (caar entry))))
	   (add-super (node super)
	     (setf (node-supers node) (cons super (node-supers node))
		   (node-subs super) (cons node (node-subs super))))	 
	   (remove-super (node super)
	     (setf (node-supers node) (delq super (node-supers node))
		   (node-subs super) (delq node (node-subs super)))))
      (let ((nodes (mapcar #'(lambda (entry)
			       (make-node (entry-type entry) entry))
			   *portable-types*)))
	(setq *portable-type-lattice* (find 't nodes :key #'node-type))
	(dolist (n1 nodes)
	  (dolist (n2 (cdr (memq n1 nodes)))
	    (cond ((subtypep (node-type n1) (node-type n2))
		   (add-super n1 n2))
		  ((subtypep (node-type n2) (node-type n1))
		   (add-super n2 n1)))))
	(dolist (node nodes)
	  (dolist (super1 (node-supers node))
	    (dolist (super2 (cdr (node-supers node)))
	      (unless (eq super1 super2)
		(when (subtypep (node-type super1) (node-type super2))
		  (remove-super node super2))))))
	nodes))))

(defun prune-type-lattice (lattice subs)
  (cond ((null subs) nil)
	(

	 )))

