;;;-*-Mode:LISP; Package:(PCL (LISP WALKER)); Base:10; Syntax:Common-lisp -*-
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
;;; Non-Bootstrap stuff
;;;

(in-package :cool.pcl)


(ndefstruct (obsolete-class (:class class)
                            (:include (class))))


(defmeth get-slot-using-class ((class obsolete-class)
			       object slot-name
			       dont-call-slot-missing-p
			       default)
  (change-class object
		(cadr (get-slot class 'class-precedence-list)))
  (get-slot-using-class
    (class-of object) object slot-name dont-call-slot-missing-p default))


  ;;   
;;;;;; 
  ;;   


(defmeth describe-class (class-or-class-name
			  &optional (stream *standard-output*))
  (flet ((pretty-class (class) (or (class-name class) class)))
    (if (symbolp class-or-class-name)
	(describe-class (class-named class-or-class-name) stream)
	(let ((class class-or-class-name))
	  (format stream
		  "~&The class ~S is an instance of class ~S."
		  class
		  (class-of class))
	  (format stream "~&Name:~23T~S~%~
			    Class-Precedence-List:~23T~S~%~
                            Local-Supers:~23T~S~%~
                            Direct-Subclasses:~23T~S"
		  (class-name class)
		  (mapcar #'pretty-class (class-class-precedence-list class))
		  (mapcar #'pretty-class (class-local-supers class))
		  (mapcar #'pretty-class (class-direct-subclasses class)))
	  class))))

(defun describe-instance (object &optional (stream t))
  (let* ((class (class-of object))
         (instance-slots (class-instance-slots class))
         (non-instance-slots (class-non-instance-slots class))
         (dynamic-slots (iwmc-class-dynamic-slots object))
	 (max-slot-name-length 0))
    (macrolet ((adjust-slot-name-length (name)
		 `(setq max-slot-name-length
			(max max-slot-name-length
			     (length (the string (symbol-name ,name))))))
	       (describe-slot (name value &optional (allocation () alloc-p))
		 (if alloc-p
		     `(format stream
			      "~% ~A ~S ~VT  ~S"
			      ,name ,allocation (+ max-slot-name-length 7)
			      ,value)
		     `(format stream
			      "~% ~A~VT  ~S"
			      ,name max-slot-name-length ,value))))
      ;; Figure out a good width for the slot-name column.
      (iterate ((slotd in instance-slots))
	(adjust-slot-name-length (slotd-name slotd)))      
      (iterate ((slotd in non-instance-slots))
	(adjust-slot-name-length (slotd-name slotd)))
      (iterate ((name in dynamic-slots by cddr))
	(adjust-slot-name-length name))
      (setq max-slot-name-length  (min (+ max-slot-name-length 3) 30))
      (format stream "~%~S is an instance of class ~S:" object class)
      (format stream "~% The following slots are allocated in the instance ~
                         (:INSTANCE allocation):")
      (iterate ((slotd in instance-slots))
	(let ((name (slotd-name slotd)))
	  (describe-slot name (get-slot object name))))
      (when (or dynamic-slots
		(iterate ((slotd in non-instance-slots))
		  (when (neq (slotd-allocation slotd) :dynamic) (return t))))
	(format stream
		"~%The following slots have special allocations as shown:")
	(iterate ((slotd in non-instance-slots))
	  (unless (eq (slotd-allocation slotd) :dynamic)
	    (describe-slot (slotd-name slotd)
			   (get-slot object (slotd-name slotd))
			   (slotd-allocation slotd))))
	(iterate ((name in dynamic-slots by cddr)
		  (val in (cdr dynamic-slots) by cddr))
	  (describe-slot name val :dynamic)))))
  object)


  ;;   
;;;;;; 
  ;;   

(ndefstruct (structure-metaclass (:class class)
				 (:include class)
				 (:constructor nil)))

(defmeth expand-defstruct ((class structure-metaclass)
			   name-and-options doc slot-descriptions)
  (declare (ignore class doc))
  (let ((class-argument (iterate ((option in (cdr name-and-options)))
				 (when (and (listp option)
					    (eq (car option) ':class))
				   (return option)))))
    `(defstruct ,(remove class-argument name-and-options)
       . ,slot-descriptions)))


  ;;   
;;;;;; 
  ;;   

(cltl1-eval-when (compile load eval)
(ndefstruct (built-in (:class class)
		      (:include (class))))

(ndefstruct (built-in-with-fast-type-predicate (:class class)
					       (:include (built-in))))

(defmacro define-built-in-class (name includes &optional fast-type-predicate)
  `(ndefstruct (,name (:class ,(if fast-type-predicate
				   'built-in-with-fast-type-predicate
				   'built-in))
		      (:include ,includes))
     (fast-type-predicate ',fast-type-predicate)  ;;;

     ))

(defmeth parse-defstruct-options ((class built-in) name options)
  (let ((ds-options (call-next-method)))
    (or (ds-options-includes ds-options)
	(setf (ds-options-includes ds-options) (list 'object)))
    ds-options))

(defmeth expand-defstruct-make-definitions ((class built-in)
					    name ds-options slotds)
  (declare (ignore class name ds-options slotds))
  ())

(defmeth make-instance ((class built-in))
  (error
    "Attempt to make an instance of the built-in class ~S.~%~
     Currently it is not possible to make instance of built-in classes with~
     make.~%~
     A design for this exists, because of metaclasses it is easy to do,~%~
     it just has to be done."
    class))

(defmeth compatible-meta-class-change-p
	 ((from built-in)
	  (to built-in-with-fast-type-predicate))
  (declare (ignore from to))
  t)

(defmeth check-super-metaclass-compatibility ((built-in built-in)
					       (new-super class))
  (or (eq new-super (class-named 't))
      (error "~S cannot have ~S as a super.~%~
              The only meta-class CLASS class that a built-in class can~%~
              have as a super is the class T."
	     built-in new-super)))



(defmeth check-super-metaclass-compatibility
	 ((class built-in)
	  (new-local-super built-in))
  (declare (ignore class new-local-super))
  t)

;(defmeth check-super-metaclass-compatibility
;	 ((class built-in-with-fast-type-predicate)
;	  (new-local-super built-in))
;  (ignore class new-local-super)
;  t)

(defmeth compute-class-precedence-list ((class built-in))
  ;; Compute the class-precedence list just like we do for CLASS except that
  ;; a built-in class cannot inherit COMMON from another built-in class.  But
  ;; it does inherit the things that it would have inherited had it inherited
  ;; common.
  (let ((val (call-next-method))
	(common-class nil))
    (if (not (memq (setq common-class (class-named 'common t))
		   (class-local-supers class)))
	(remove common-class val)
	val)))


)

  ;;   
;;;;;; The built in types 
  ;;   

(define-built-in-class common (t))

(define-built-in-class pathname (common) pathnamep)

(define-built-in-class stream (common) streamp)

(define-built-in-class sequence (t))
(define-built-in-class list (sequence) listp)
(define-built-in-class cons (list common) consp)
(define-built-in-class symbol (common) symbolp)
(define-built-in-class null (list symbol) null)

(define-built-in-class keyword (symbol common) keywordp)

(define-built-in-class array (common) arrayp)
(define-built-in-class vector (sequence array) vectorp)
(define-built-in-class simple-array (array))

(define-built-in-class string (vector common) stringp)
(define-built-in-class bit-vector (vector) bit-vector-p)
;(vector t) should go here

(define-built-in-class simple-string (string simple-array) simple-string-p)
(define-built-in-class simple-bit-vector (bit-vector simple-array)
					 simple-bit-vector-p)
(define-built-in-class simple-vector (vector simple-array) simple-vector-p)

(define-built-in-class function (t))

(define-built-in-class character (t) characterp)
(define-built-in-class string-char (character) string-char-p)
(define-built-in-class standard-char (string-char common) standard-char-p)

(define-built-in-class structure (common))

(define-built-in-class number (t) numberp)

(define-built-in-class rational (number) rationalp)
(define-built-in-class float (number) floatp)
(define-built-in-class complex (number common) complexp)

(define-built-in-class integer (rational))
(define-built-in-class ratio   (rational common))

(define-built-in-class fixnum (integer common))
(define-built-in-class bignum (integer common))

(define-built-in-class short-float  (float common))
(define-built-in-class single-float (float common))
(define-built-in-class double-float (float common))
(define-built-in-class long-float   (float common))

(define-built-in-class hash-table (common) hash-table-p)
(define-built-in-class readtable (common) readtablep)
(define-built-in-class package (common) packagep)
(define-built-in-class random-state (common) random-state-p)


(cltl1-eval-when (load)
  (setq *error-when-defining-method-on-existing-function* t))

