;;;-*-Mode:LISP; Package: PCL; Base:10; Syntax:Common-lisp -*-
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



  ;;   
;;;;;; New New Minglewood Blues
  ;;   the new "legendary macro itself"
;;;
(defmacro ndefstruct (name-and-options &rest slot-descriptions)
  ;;
  ;; The defstruct macro does some pre-processing on name-and-options and
  ;; slot-descriptions before it passes them on to expand-defstruct. It
  ;; also pulls out the documentation string (if there is one) and passes
  ;; it to expand defstruct as a separate argument.
  ;;
  ;; The main reason for doing this is that it imposes more uniformity in
  ;; the syntax of defstructs for different metaclasses, and it puts some
  ;; useful error checking for that syntax in one central place.
  ;; 
  (let ((documentation (and (stringp (car slot-descriptions))
			    (pop slot-descriptions))))
    (or (listp name-and-options) (setq name-and-options (list name-and-options)))
    (setq slot-descriptions
          (iterate ((sd in slot-descriptions))
            (collect
              (cond ((not (listp sd)) (list sd nil))
                    (t (unless (evenp (length sd))
                         (error "While parsing the defstruct ~S, the slot-description: ~S~%~
                                 has an odd number of elements."
                                (car name-and-options) sd))
                       sd)))))
    (keyword-parse ((class 'structure))
                   (cdr name-and-options)
      (let ((class-object (class-named class t)))
        (if class-object
            (expand-defstruct
             (class-prototype class-object) name-and-options documentation slot-descriptions)
            (error "The argument to defstruct's :class option was ~S;~%~
                    but there is no class named ~S."
                   class class))))))

(defmacro defclass (name includes slots &rest options)
  (keyword-parse ((metaclass 'class)) options
    (let ((metaclass-object (class-named metaclass t)))
      (or metaclass-object 
	  (error "The class option to defclass was ~S,~%~
                  but there is no class with that name."
		 metaclass))
      (or (subclassp metaclass-object 'class)
	  (error
	    "The class specified in the :metaclass option to defclass, ~S,~%~
            is not a subclass of the class class."
	    metaclass))
      (expand-defclass metaclass-object name includes slots options))))

(defmethod expand-defclass ((metaclass class) name includes slots options)
  (keyword-parse ((accessor-prefix nil accessor-prefix-p)) options
    (when (and accessor-prefix-p
	       (not (or (null accessor-prefix)
			(symbolp accessor-prefix))))
      (error "The :accessor-prefix option, when specified must have either~%~
              have an argument which is a symbol, or no argument at all."))
    (setq slots (iterate ((slot in slots))
		  (collect
		    (cond ((and (listp slot)
				(cddr slot))
			   (let ((initform
				   (if (memq :initform (cdr slot))
				       (cadr (memq :initform (cdr slot)))
				       *slotd-unsupplied*)))
			     (list* (car slot) initform (cdr slot))))
			  ((listp slot) slot)
			  (t (list slot *slotd-unsupplied*))))))
    `(ndefstruct (,name (:class ,(class-name metaclass))
			(:include ,includes)
			,@(and accessor-prefix-p
			       `((:conc-name ,accessor-prefix)))
			(:generate-accessors ,(and accessor-prefix-p
						   'method))
			,@options)
     ,@slots)))

(defmeth expand-defstruct ((class basic-class) name-and-options documentation slot-descriptions)
  (declare (ignore documentation))
  (let* ((name (car name-and-options))
         (ds-options (parse-defstruct-options class name (cdr name-and-options)))
         (slotds (parse-slot-descriptions class ds-options slot-descriptions)))
    `(progn
       (cltl1-eval-when (load eval)	 
	 (record-definition ',name 'ndefstruct))
       ;; Start by calling add-named-class which will actually define the new
       ;; class, updating the class lattice obsoleting old instances etc.
       (cltl1-eval-when (compile load eval)
         (add-named-class
	   (class-prototype (class-named ',(class-name (class-of class))))
	   ',name
	   ',(or (ds-options-includes ds-options)
		 (class-default-includes class))
	   ',slotds
	   ',ds-options))
       ,@(expand-defstruct-make-definitions class name ds-options slotds)
       ',name)))

(defmeth expand-defstruct-make-definitions ((class basic-class)
					     name ds-options slotds)
  (append (make-accessor-definitions class name ds-options slotds)
          (make-constructor-definitions class name ds-options slotds)
          (make-copier-definitions class name ds-options slotds)
          (make-predicate-definitions class name ds-options slotds)
          (make-print-function-definitions class name ds-options slotds)))

(define-function-template iwmc-class-accessor () '(slot-name)
  `(function (lambda (iwmc-class) (get-slot--class iwmc-class slot-name))))

(cltl1-eval-when (load)
  (pre-make-templated-function-constructor iwmc-class-accessor))

(define-function-template iwmc-class-accessor-setf (read-only-p) '(slot-name)
  (if read-only-p
      `(function
        (lambda (iwmc-class new-value)
         (declare (ignore iwmc-class new-value))
	   (error "~S is a read only slot." slot-name)))
      `(function
         (lambda (iwmc-class new-value)
	   (put-slot--class iwmc-class slot-name new-value)))))


(cltl1-eval-when (load)
  (pre-make-templated-function-constructor iwmc-class-accessor-setf nil)
  (pre-make-templated-function-constructor iwmc-class-accessor-setf t))

(defmethod make-iwmc-class-accessor ((ignore class) slotd)
  (declare (ignore ignore))
  (funcall (get-templated-function-constructor 'iwmc-class-accessor)
	   (slotd-name slotd)))

(defmethod make-iwmc-class-accessor-setf ((ignore class) slotd)
  (declare (ignore ignore))
  (funcall
    (get-templated-function-constructor 'iwmc-class-accessor-setf
					(slotd-read-only slotd))
    (slotd-name slotd)))

(defun add-named-method-early (discriminator-name
			       arglist
			       argument-specifiers
			       function)
  (if (null *real-methods-exist-p*)
      (unless (memq discriminator-name *protected-early-selectors*)
	(setf (symbol-function discriminator-name) function))
      (add-named-method (class-prototype (class-named 'discriminator))
			(class-prototype (class-named 'method))
			discriminator-name
			arglist
			argument-specifiers
			()
			function)))
  
(defmeth make-accessor-definitions
	 ((class basic-class) name ds-options slotds)
  (declare (ignore class ds-options))
  (cons `(do-accessor-definitions ',name ',slotds)
	(iterate ((slotd in slotds))
	  (let ((accessor (slotd-accessor slotd))
		setf-discriminator-name)
	    (when accessor
	      (setq setf-discriminator-name
		    (make-setf-discriminator-name accessor))
	      (compile-time-define 'defun accessor)
	      (compile-time-define 'defun setf-discriminator-name)
	      (compile-time-define 'defsetf accessor setf-discriminator-name)
	      (collect `(defsetf ,accessor ,setf-discriminator-name)))))))

(defun do-accessor-definitions (name slotds)
  (let ((class (class-named name))
	(accessor nil)
	(setf-discriminator-name nil))
    (dolist (slotd slotds)
      (when (setq accessor (slotd-accessor slotd))
	(setq setf-discriminator-name
	      (make-setf-discriminator-name accessor))
	(unless *real-methods-exist-p*
	  (record-early-discriminator accessor)
	  (record-early-discriminator setf-discriminator-name))
	(add-named-method-early accessor
				`(,name)
				`(,class)
				(or (slotd-get-function slotd)
				    (make-iwmc-class-accessor class slotd)))
	(add-named-method-early setf-discriminator-name
				`(,name new-value)
				`(,class)
				(or (slotd-put-function slotd)
				    (make-iwmc-class-accessor-setf class
								   slotd)))))
    (unless *real-methods-exist-p*
      (record-early-method-fixup
	`(let ((*real-methods-exist-p* t))
	   (do-accessor-definitions ',name ',slotds))))))

(defmeth make-constructor-definitions ((class basic-class) name ds-options slotds)
  (declare (ignore class slotds))
  (let ((constructors (ds-options-constructors ds-options)))
    (iterate ((constructor in constructors))
      (when (car constructor)
        (collect
          (if (cdr constructor)
              `(defun ,(car constructor) ,(cadr constructor)
                 (make ',name ,@(iterate ((slot-name in (cadr constructor)))
                                         (unless (memq slot-name
                                                       '(&optional &rest &aux))
                                           (collect `',(make-keyword slot-name))
                                           (collect slot-name)))))
              `(defun ,(car constructor) (&rest init-plist)
                 (apply #'make ',name init-plist))))))))

(define-function-template copier--class () ()
  `(function
     (lambda (iwmc-class)
       (let* ((class (class-of iwmc-class))
              (to (make-instance (class-of iwmc-class)))
              (from-static (iwmc-class-static-slots iwmc-class))        
              (to-static (iwmc-class-static-slots to))
              (static-slots (class-instance-slots class)))
         (do ((i 0 (+ i 1))
	      (index nil index)		 
              (x static-slots (cdr x)))
             ((null x))
	   (setq index (%convert-slotd-position-to-slot-index i))
           (setf (%static-slot-storage-get-slot--class to-static index)
                 (%static-slot-storage-get-slot--class from-static index)))
         (setf (iwmc-class-dynamic-slots to)
               (copy-list (iwmc-class-dynamic-slots iwmc-class)))
         to))))

(cltl1-eval-when (load)
  (pre-make-templated-function-constructor copier--class))

(defmeth make-copier-definitions ((class basic-class) name ds-options slotds)
  (declare (ignore class slotds))
  (let ((copier (ds-options-copier ds-options)))    
    (when copier
      (compile-time-define 'defun copier)
      `((do-copier-definition ',name ',copier)))))

(defun do-copier-definition (class-name copier-name)
  (unless *real-methods-exist-p*
    (record-early-discriminator copier-name)
    (record-early-method-fixup
      `(let ((*real-methods-exist-p* t))
	 (do-copier-definition ',class-name ',copier-name))))
  (add-named-method-early copier-name
			  `(,class-name)
			  `(,(class-named class-name))
			  (funcall
			    (get-templated-function-constructor
			      'copier--class))))

(define-function-template iwmc-class-predicate () '(class-name)
  `(function (lambda (x)
	       (and (iwmc-class-p x)
		    (typep--class x class-name)))))

(cltl1-eval-when (load)
  (pre-make-templated-function-constructor iwmc-class-predicate))

(defmeth make-predicate-definitions ((class basic-class)
				     name ds-options slotds)
  (declare (ignore class slotds))
  (let ((predicate (or (ds-options-predicate ds-options)
                       (make-symbol (string-append name " Predicate")))))
    (compile-time-define 'defun predicate)
    `((do-predicate-definition ',name ',predicate)
      (deftype ,name () '(satisfies ,predicate)))))

(defun do-predicate-definition (class-name predicate-name)
  (setf (symbol-function predicate-name)
	(funcall (get-templated-function-constructor 'iwmc-class-predicate)
		 class-name)))

(defun make-print-function-definitions
	  (class name ds-options slotds)
  (declare (ignore class slotds))
  (let* ((print-function (ds-options-print-function ds-options))
	 (arglist ())
	 (defun ())
	 (defun-name ()))
    (when print-function
      (cond ((symbolp print-function)
	     (setq arglist '(object stream depth)))
	    ((and (listp print-function) (eq (car print-function) 'lambda))
	     (setq arglist (cadr print-function)
		   defun-name (intern 
				(string-append (symbol-name name)
					       " Print Function"))
		   defun `(defun ,defun-name ,arglist
			    ,@(cddr print-function))
		   print-function defun-name))
	    (t
	     (error "Internal error, make-print-function-definitions can't~%~
                     understand the contents of the print-function slot of~%~
                     the ds-options.")))
      `(,defun
	(do-print-function-definitions ',name ',arglist ',print-function)))))

(defun do-print-function-definitions (name arglist print-function)
  (unless *real-methods-exist-p*
    (record-early-method-fixup
      `(let ((*real-methods-exist-p* t))
	 (do-print-function-definitions ',name ',arglist ',print-function))))
  (add-named-method-early 'print-instance
			  arglist
			  (list (class-named name))
			  print-function))

