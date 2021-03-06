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
;;; This is the Lucid lisp version of the file portable-low.
;;;
;;; Lucid:               (415)329-8400
;;; Sun:     Steve Gadol (415)960-1300
;;; 

(in-package 'pcl)

  ;;   
;;;;;; Memory Block primitives.
  ;;   

(defmacro make-memory-block (size &optional area)
  (ignore area)
  `(make-array ,size))

;;;
;;; Reimplementation OF %INSTANCE
;;;
;;; We take advantage of the fact that Lucid defstruct doesn't depend on
;;; the fact that Common Lisp defstructs are fixed length.  This allows us to
;;; use defstruct to define a new type, but use internal structure allocation
;;; code to make structure of that type of any length we like.
;;;
;;; In our %instance datatype, the array look like
;;;
;;;  structure type: The symbol %INSTANCE, this tells the system what kind
;;;                  of structure this is.
;;;  element 0:      The meta-class of this %INSTANCE
;;;  element 1:      This is used to store the value of %instance-ref slot 0.
;;;  element 2:      This is used to store the value of %instance-ref slot 1.
;;;     .                                .
;;;     .                                .
;;;
(defstruct (%instance (:print-function print-instance)
		      (:constructor nil)
		      (:predicate %instancep))
  meta-class)

(defmacro %make-instance (meta-class size)
  (let ((instance-var (gensym)))
    `(let ((,instance-var (lucid::new-structure (1+ ,size) '%instance)))
       (setf (lucid::structure-ref ,instance-var 0 '%instance) ,meta-class)
       ,instance-var)))

(defmacro %instance-ref (instance index)
  `(lucid::structure-ref ,instance (1+ ,index) '%instance))


  ;;   
;;;;;; Cache No's
  ;;  

;;; Grab the top 29 bits
;;;
(lucid::defsubst symbol-cache-no (symbol mask)
  (logand (lucid::%field symbol 3 29) mask))

;;; Same here
;;;
(lucid::defsubst object-cache-no (object mask)
  (logand (lucid::%field object 3 29) mask))

  ;;   
;;;;;; printing-random-thing-internal
  ;;
(defun printing-random-thing-internal (thing stream)
  (format stream "~O" (lucid::%pointer thing)))


(in-package 'lucid)

(defun output-structure (struct currlevel)
  (let ((type (structure-type struct)))
    (multiple-value-bind (length struct-type constructor print-function)
	(defstruct-info type)
      (declare (ignore struct-type constructor))
      (if (not *print-structure*)
	  (output-terse-object struct
			       (if (streamp struct) "Stream" "Structure")
			       type)
	  (funcall (if print-function
		       (symbol-function print-function)
		       #'default-structure-print)
		   struct *print-output* currlevel)))))

