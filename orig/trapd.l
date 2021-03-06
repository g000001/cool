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
;;; Trapped discriminators.
;;;
;;; These allow someone to declare that for a given selector, the methods
;;; should actually be defined on some other selector, the so-called trap-
;;; selector.
;;;
;;; An example of its use is:
;;;   (make-primitive-specializable 'car 'car-trap)
;;;

(in-package 'pcl)

(ndefstruct (trapped-discriminator-mixin
	      (:class class)
	      (:include discriminator)
	      (:conc-name trapped-discriminator-))
  (trap-discriminator ()))

(defmeth trapped-discriminator-selector ((self trapped-discriminator-mixin))
  (let ((td (trapped-discriminator-trap-discriminator self)))
    (and td (discriminator-name td))))	

(defmeth add-method-internal ((self trapped-discriminator-mixin)
			      (method basic-method))
  (with (self) (add-method-internal trap-discriminator method)))

(ndefstruct (trapped-discriminator
	      (:class class)
	      (:include (trapped-discriminator-mixin discriminator))))

(defun make-primitive-specializable (name trap-selector &rest options)
  (let ((trap-discriminator
	  (apply #'make-specializable trap-selector arglist)))
    (setf (discriminator-named name)
	  (make 'trapped-discriminator
		:name name
		:trap-discriminator trap-discriminator))))


