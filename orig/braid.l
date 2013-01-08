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
;;; The meta-braid and defstruct.
;;;
;;; NOTE: This file must be loaded before it can be compiled.

#| *** TO DO ***

|#
(in-package 'pcl)

  ;;   
;;;;;; Medium-level support for the class CLASS.
  ;;   
;;; The low-level macros are defined by the file portable-low (or a special
;;; version) of that file if there is one for this implementation.  This is
;;; the lowest-level completely portable code which operates on instances
;;; with meta-class class.

(defmacro get-static-slot--class (iwmc-class slot-index)
  `(%static-slot-storage-get-slot--class
     (iwmc-class-static-slots ,iwmc-class)
     ,slot-index))

(defmacro get-dynamic-slot--class (iwmc-class slot-name default)
  `(%dynamic-slot-storage-get-slot--class
     (iwmc-class-dynamic-slots ,iwmc-class)
     ,slot-name
     ,default))

(defmacro remove-dynamic-slot--class (iwmc-class slot-name)
  `(%dynamic-slot-storage-remove-slot--class
     (iwmc-class-dynamic-slots ,iwmc-class)
     ,slot-name))


  ;;
;;;;;; defmeth  -- defining methods
  ;;
;;; We need to be able to define something like methods before we really have
;;; real method functionality available.
;;;
;;; defmeth expands by calling expand-defmeth, this means that we can define
;;; an early version of defmeth just by defining an early version of expand-
;;; defmeth.
;;;
(defmacro defmethod (&rest args)
 ;(declare (zl:arglist name qualifier* arglist &body body))
  (let ((name (pop args))
	(qualifiers ())
	(arglist ())
	(body nil))
    (multiple-value-setq (qualifiers args) (defmethod-qualifiers args))
    (setq arglist (pop args)
	  body args)
    `(defmeth (,name . ,qualifiers) ,arglist . ,body)))

(defmacro defmethod-setf (&rest args)
  (let ((name (pop args))
	(qualifiers ())
	(arglist ())
	(new-value-arglist ())
	(body nil))
    (multiple-value-setq (qualifiers args) (defmethod-qualifiers args))
    (setq arglist (pop args)
	  new-value-arglist (pop args)
	  body args)
    `(defmeth (,name (:setf ,new-value-arglist) ,.qualifiers) ,arglist
       ,@body)))

(defun defmethod-qualifiers (args)
  (declare (values qualifiers arglist-and-body))
  (let ((qualifiers ()))
    (loop (if (and (car args) (listp (car args)))
	      (return (values (nreverse qualifiers) args))
	      (push (pop args) qualifiers)))))

(defun defmethod-argument-specializers (arglist)
  (let ((arg (car arglist)))
    (cond ((null arglist) nil)
	  ((memq arg '(&optional &rest &key &aux)) nil) ;Don't allow any
                                                        ;argument specializers
	                                                ;after one of these.
	  ((memq arg lambda-list-keywords)	        ;Or one of these!!
	   (warn "Unrecognized lambda-list keyword ~S in arglist.~%~
                  Assuming that no argument specializers appear after it."
		 arg)
	   nil)
	  (t
	   (let ((tail (defmethod-argument-specializers (cdr arglist)))
		 (specializer (and (listp arg) (cadr arg))))
	     (or (and tail (cons (or specializer 't) tail))
		 (and specializer (cons specializer ()))))))))


(defmacro defmeth (name&options arglist &body body)
  (expand-defmeth name&options arglist body))

(eval-when (compile load eval)
  ;; Make sure we call bootstrap-expand-defmeth during bootstrapping.
  ;;  - Can't say (setf (symbol-fu ..) #'bootstrap-expand-defmeth because
  ;;    bootstrap-expand-defmeth isn't defined yet and that isn't legal
  ;;    in Common Lisp.
  ;;  - Can't say (setf (symbol-fu ..) 'bootstrap-expand-defmeth because
  ;;    not all Common Lisps like having symbols in the function cell.
  (setf (symbol-function 'expand-defmeth)
	#'(lambda (name&options arglist body)
	    (bootstrap-expand-defmeth name&options arglist body)))
  )

  ;;   
;;;;;; Early methods
  ;;   

(defvar *real-methods-exist-p*)
(eval-when (compile load eval)
  (setq *real-methods-exist-p* nil))

(eval-when (load)  
  (setq *error-when-defining-method-on-existing-function* 'bootstrapping))

(defvar *protected-early-selectors* '(print-instance))

(defparameter *early-defmeths* ())

(defmacro simple-type-specs (arglist)
  `(let ((type-specs
	   (iterate ((arg in ,arglist))
		    (until (memq arg '(&optional &rest &key &aux)))
		    (collect (if (listp arg) (cadr arg) 't)))))
     (setq type-specs (nreverse type-specs))
     (iterate ((type-spec in type-specs))
	      (until (neq type-spec 't))
	      (pop type-specs))
     (nreverse type-specs)))

(defmacro simple-without-type-specs (arglist)
  `(iterate ((loc on ,arglist))
	    (cond ((memq (car loc) '(&optional &rest &key &aux))
		   (join loc) (until t))
		  (t
		   (collect (if (listp (car loc))
				(caar loc)
				(car loc)))))))
(defmacro simple-args (arglist)
  `(iterate ((arg in ,arglist))
	    (until (eq arg '&aux))
	    (unless (memq arg '(&optional &rest &key))
	      (collect (if (listp arg) (car arg) arg)))))

(defun bootstrap-expand-defmeth (name&options arglist body)
  ;; Some SIMPLE local macros for getting the type-specifiers out of the
  ;; argument list.  Unfortunately, it is important that these simple
  ;; macros and the methods which come along later and do this job better
  ;; be compatible.  This will become less of an issue once methods don't
  ;; have names anymore.
; (macrolet ()             
    (multiple-value-bind (documentation declares body)
        (extract-declarations body)
      (or (listp name&options) (setq name&options (list name&options)))
      (keyword-parse ((setf () setfp))
                     (cdr name&options)
        (let* ((name (car name&options))
               (discriminator-name (if setfp
				       (make-setf-discriminator-name name)
				       name))
               (method-name (if setfp
                                (make-setf-method-name
				  name
				  (simple-type-specs setf)
				  (simple-type-specs arglist))
                                (make-method-name
				  name (simple-type-specs arglist))))
               (method-arglist (simple-without-type-specs
                                 (if setfp
                                     (cons (car arglist)
					   (append setf (cdr arglist)))
                                     arglist))))
          `(progn
             ;; Record this early defmeth so that fixup-early-defmeths will
             ;; know to fix it up later.
             (eval-when (compile load eval)
               (record-early-defmeth
		 ',discriminator-name ',name&options ',arglist ',body))
	     (record-definition ',discriminator-name 'method)
             (defun ,method-name ,method-arglist
               ,@(and documentation (list documentation))
               ,@declares
;              #+Symbolics(declare (sys:function-parent ,name defmeth))
               . ,body)	     
	     ,(unless (memq discriminator-name *protected-early-selectors*)
		`(eval-when (load eval)
		   (setf (symbol-function ',discriminator-name)
			 (symbol-function ',method-name))))
             ,@(and setfp
		    (not (memq discriminator-name *protected-early-selectors*))
                    (let ((args (simple-without-type-specs arglist))
                          (setf-args (simple-without-type-specs setf)))
                      `((defsetf ,name ,args ,setf-args
                          (list ',discriminator-name
                                ,(car args)
                                ,@(simple-args setf)
                                ,@(simple-args (cdr args))))))))))))
;)

(defun record-early-defmeth (discriminator-name name&options arglist body)
  (pushnew (list* 'defmeth discriminator-name name&options arglist body)
	   *early-defmeths*
	   :test #'equal))

(defun record-early-discriminator (discriminator-name)
  (pushnew (list 'clear discriminator-name) *early-defmeths* :test #'equal))

(defun record-early-method-fixup (form)
  (pushnew (list 'eval form) *early-defmeths* :test #'equal))

(defmacro fix-early-defmeths ()
  (let ((resets ())
	(evals ()))
    (dolist (entry *early-defmeths*)
      (ecase (car entry)
	(defmeth (push (cons 'defmeth (cddr entry)) evals)
		 (push (cadr entry) resets))
	(clear   (push (cadr entry) resets))
	(eval    (push (cadr entry) evals))))    
    `(progn
       ;; The first thing to do is go through and get rid of all the old
       ;; discriminators.  This only needs to happen when we are being
       ;; loaded into the same VMem we were compiled in.  The WHEN is
       ;; making that optimization.
       (defun fix-early-defmeths-1 ()	 
	 (when (discriminator-named ',(car resets))	   
	   (dolist (x ',resets) (setf (discriminator-named x) nil))))
       (fix-early-defmeths-1)
       ,@evals)))

#| This is useful for debugging.
(defmacro unfix-early-defmeths ()
  `(progn
     (do-symbols (x)
       (remprop x 'discriminator)
       (remprop x 'setf-discriminator))
     . ,(mapcar '(lambda (x) (cons 'defmeth x)) (reverse *early-defmeths*))))

(unfix-early-defmeths)
|#

(defun make-setf-discriminator-name (name)
  (intern (string-append name " :SETF-discriminator")
	  (symbol-package name)))

(defun make-method-name (selector type-specifiers)
  (intern (apply #'string-append
                      (list* "Method "
                             selector
                             " "
                             (make-method-name-internal type-specifiers)))
	  (symbol-package selector)))

(defun make-setf-method-name (selector setf-type-specifiers type-specifiers)
  (intern (apply #'string-append
                      (list* "Method "
                             selector
                             " ("
                             (apply #'string-append
                                    ":SETF "
                                    (make-method-name-internal setf-type-specifiers))
                             ") "
                             (make-method-name-internal type-specifiers)))
	  (symbol-package selector)))

(defun make-method-name-internal (type-specifiers)
  (if type-specifiers
      (iterate ((type-spec on type-specifiers))
        (collect (string (car type-spec)))
        (when (cdr type-spec) (collect " ")))
      '("Default")))
  


  ;;
;;;;;; SLOTDS and DS-OPTIONS
  ;;
;;;
;;; A slot-description is the thing which appears in a defstruct.  A SLOTD is
;;; an internal description of a slot.
;;;
;;; The SLOTD structure corresponds to the kind of slot the structure-class
;;; meta-class creates (the kind of slot that appears in Steele Edition 1).
;;; Other metaclasses which need to have more elaborate slot options and
;;; slotds, they :include that class in their slotds.
;;;
;;; slotds are :type list for 2 important reasons:
;;;   - so that looking up a slotd in a list of lists will compile
;;;     into a call to assq
;;;   - PCL assumes only the existence of the simplest of defstructs
;;;     this allows PCL to be used to implement a real defstruct.
;;;     
(defstruct (essential-slotd (:type list)
			    (:constructor make-slotd--essential-class))
  name)

;;;
;;; Slotd-position is used to find the position of a slot with a particular
;;; name in a list of slotds.  Specifically it is used in the case of a
;;; get-slot cache miss to find this slot index.  That means it is used in
;;; about 2% of the total slot accesses so it should be fast.
;;; 
(defmacro slotd-position (slotd-name slotds)
  `(let ((slotd-name ,slotd-name))
     (do ((pos 0 (+ pos 1))
	  (slotds ,slotds (cdr slotds)))
	 ((null slotds) nil)
       (declare (type integer pos) (type list slotds))
       (and (eq slotd-name (slotd-name (car slotds)))
	    (return pos)))))

(defmacro slotd-member (slotd-name slotds)	              ;I wonder how
  `(member ,slotd-name ,slotds :test #'eq :key #'slotd-name)) ;many compilers
						              ;are really
						              ;smart enough.
(defmacro slotd-assoc (slotd-name slotds)	
  `(assq ,slotd-name ,slotds))

;;;
;;; Once defstruct-options are defaulted and parsed, they are stored in a
;;; ds-options (defstruct-options) structure.  This modularity makes it
;;; easier to build the meta-braid which has to do some slot and option
;;; parsing long before the real new defstruct exists.  More importantly,
;;; this allows new meta-classes to inherit the option parsing code 
;;; from other metaclasses.
;;;
(defstruct (ds-options (:constructor make-ds-options--class))
  name
  constructors             ;The constructor argument, a list whose car is the
			   ;name of the constructor and whose cadr if present
                           ;is the argument-list for the constructor.
  copier                   ;(defaulted) value of the :copier option.
  predicate                ;ditto for :predicate
  print-function           ;ditto for :print-function
  generate-accessors       ;ditto for :generate-accessors
  conc-name                ;ditto for :conc-name 
  includes                 ;The included structures (car of :include)
  slot-includes            ;The included slot modifications (cdr of :include)
  initial-offset           ;(defaulted) value of the :initial-offset option.
  )

  

  ;;
;;;;;; The beginnings of the meta-class CLASS (parsing the defstruct)
  ;;   

(defmeth make-ds-options ((class basic-class) name)
  (ignore class)
  (make-ds-options--class :name name))

(defmeth parse-defstruct-options ((class basic-class) name options)
  (parse-defstruct-options-internal
    class name options
    (default-ds-options class name (make-ds-options class name))))

(defmeth default-ds-options ((class basic-class) name ds-options)
  (ignore class)
  (setf
    (ds-options-constructors ds-options)       `((,(symbol-append "MAKE-"
								  name)))
    (ds-options-copier ds-options)             (symbol-append "COPY-" name)
    (ds-options-predicate ds-options)          (symbol-append name "-P")
    (ds-options-print-function ds-options)     nil
    (ds-options-generate-accessors ds-options) 'method
    (ds-options-conc-name ds-options)          (symbol-append name "-")
    (ds-options-includes ds-options)           ()
    (ds-options-slot-includes ds-options)      ()
    (ds-options-initial-offset ds-options)     0)
  ds-options)

(defmeth parse-defstruct-options-internal ((class basic-class)
					    name options ds-options)
  (ignore class name)
  (keyword-parse ((conc-name (ds-options-conc-name ds-options))
                  (constructor () constructor-p :allowed :multiple
						:return-cdr t)
                  (copier (ds-options-copier ds-options))
                  (predicate (ds-options-predicate ds-options))
                  (include () include-p :return-cdr t)
                  (print-function () print-function-p)
                  (initial-offset (ds-options-initial-offset ds-options))
                  (generate-accessors (ds-options-generate-accessors
					ds-options)))
                 options
    (setf (ds-options-conc-name ds-options) conc-name)
    (when constructor-p
      (setf (ds-options-constructors ds-options) constructor))
    (setf (ds-options-copier ds-options) copier)
    (setf (ds-options-predicate ds-options) predicate)
    (when include-p
      (destructuring-bind (includes . slot-includes) include
	(setf (ds-options-includes ds-options) (if (listp includes)
						   includes
						   (list includes))
	      (ds-options-slot-includes ds-options) slot-includes)))
    (when print-function-p
      (setf (ds-options-print-function ds-options)
	    (cond ((null print-function) nil)
		  ((symbolp print-function) print-function)
		  ((and (listp print-function)
			(eq (car print-function) 'lambda)
			(listp (cadr print-function)))
		   print-function)
		  (t
		   (error "The :PRINT-FUNCTION option, ~S~%~
                           is not either nil or a function suitable for the~
                           function special form."
			   print-function)))))
    (setf (ds-options-initial-offset ds-options) initial-offset)
    (setf (ds-options-generate-accessors ds-options) generate-accessors)
    ds-options))

;;;
;;;

(defvar *slotd-unsupplied* (list nil))

(defstruct (class-slotd (:include essential-slotd)
			(:type list)
			(:conc-name slotd-)
			(:constructor make-slotd--class)
			(:copier copy-slotd))
  keyword
  (default *slotd-unsupplied*)
  (type *slotd-unsupplied*)
  (read-only *slotd-unsupplied*)
  (accessor *slotd-unsupplied*)
  (allocation *slotd-unsupplied*)
  get-function   ;NIL if no :get(put)-function argument was supplied.
  put-function   ;Otherwise, a function of two (three)arguments, the
                 ;object, the name of the slot (and the new-value).
  )

(defmeth make-slotd ((class basic-class) &rest keywords-and-options)
  (ignore class)
  (apply #'make-slotd--class keywords-and-options))

(defmeth parse-slot-descriptions ((class basic-class) ds-options slot-descriptions)
  (iterate ((slot-description in slot-descriptions))
    (collect (parse-slot-description class ds-options slot-description))))

(defmeth parse-slot-description ((class basic-class) ds-options slot-description)
  (parse-slot-description-internal
    class ds-options slot-description (make-slotd class)))

(defmeth parse-slot-description-internal ((class basic-class) ds-options slot-description slotd)
  (ignore class)
  (let ((conc-name (ds-options-conc-name ds-options))
        (generate-accessors (ds-options-generate-accessors ds-options)))
    #+Lucid (declare (special conc-name generate-accessors))
    (destructuring-bind (name default . args)
                        slot-description
      (keyword-bind ((type nil)
                     (read-only nil)
                     (generate-accessor generate-accessors)
                     (allocation :instance)
                     (get-function nil)
                     (put-function nil)

		     (accessor nil accessor-p)
		     (initform nil)		;ignore
		     )
                    args
        #+Lucid(declare (special type read-only generate-accessor allocation
                                 get-function put-function))
        (check-member allocation '(:class :instance :dynamic)
                      :test #'eq
                      :pretty-name "the :allocation option")
        (setf (slotd-name slotd)         name
              (slotd-keyword slotd)      (make-keyword name)
              (slotd-default slotd)      default
              (slotd-type slotd)         type
              (slotd-read-only slotd)    read-only
              (slotd-accessor slotd)     (if accessor-p
					     accessor
					     (and generate-accessor
						  (if conc-name
						     (symbol-append conc-name
								    name)
						     name)))
              (slotd-allocation slotd)   allocation
              (slotd-get-function slotd) (and get-function
                                              (if (and (consp get-function)
                                                       (eq (car get-function) 'function))
                                                  get-function
                                                  (list 'function get-function)))
              (slotd-put-function slotd) (and put-function
                                              (if (and (consp put-function)
                                                       (eq (car put-function) 'function))
                                                  put-function
                                                  (list 'function put-function))))
        slotd))))

;;;
;;; Take two lists of slotds and return t if they describe an set of slots of
;;; the same shape.  Otherwise return nil.  Sets of slots are have the same
;;; same shape if they have they both have the same :allocation :instance
;;; slots and if those slots appear in the same order.
;;; 
(defun same-shape-slots-p (old-slotds new-slotds)
  (do ()
      ((and (null old-slotds) (null new-slotds)) t)
    (let* ((old (pop old-slotds))
	   (new (pop new-slotds))
	   (old-allocation (and old (slotd-allocation old)))
	   (new-allocation (and new (slotd-allocation new))))
      ;; For the old and new slotd check all the possible reasons
      ;; why they might not match.
      ;;   - One or the other is null means that a slot either
      ;;     disappeared or got added.
      ;;   - The names are different means that a slot moved
      ;;     disappared or go added.
      ;;   - If the allocations are different, and one of them
      ;;     is :instance then a slot either became or ceased
      ;;     to be :allocation :instance.
      (when (or (null old)
		(null new)
		(neq (slotd-name old) (slotd-name new))
		(and (neq old-allocation new-allocation)
		     (or (eq old-allocation :instance)
			 (eq new-allocation :instance))))
	(return nil)))))

(defmeth slots-with-allocation ((class basic-class) slotds allocation)
  (ignore class)
  (iterate ((slotd in slotds))
    (when (eq (slotd-allocation slotd) allocation)
      (collect slotd))))

(defmeth slots-with-allocation-not ((class basic-class) slotds allocation)
  (ignore class)
  (iterate ((slotd in slotds))
    (unless (eq (slotd-allocation slotd) allocation)
      (collect slotd))))

  ;;   
;;;;;; GET-SLOT and PUT-SLOT
  ;;
;;; Its still too early to fully define get-slot and put-slot since they need
;;; the meta-braid to work.
;;;
;;; But its nice if as part of defining the meta-braid we can define and compile
;;; code which does get-slots and setfs of get-slots and in order to do this we
;;; need to have get-slot around.  Actually we could do with just the defsetf of
;;; get-slot but might as well put all 3 here.
;;;
;;; The code bootstrap meta-braid defines with get-slot in it is all done with
;;; defmeth, so these get-slots will all get recompiled once the optimizers
;;; exist don't worry.
(defun get-slot (object slot-name)
  (get-slot-using-class (class-of object) object slot-name))

(defun put-slot (object slot-name new-value)
  (put-slot-using-class (class-of object) object slot-name new-value))

(defun setf-of-get-slot (new-value object slot-name)
  (put-slot-using-class (class-of object) object slot-name new-value))

(defsetf get-slot (object slot-name &rest extra-args) (new-value)
  `(setf-of-get-slot ,new-value ,object ,slot-name . ,extra-args))

(defun get-slot-always (object slot-name &optional default)
  (get-slot-using-class (class-of object) object slot-name t default))

(defun put-slot-always (object slot-name new-value)
  (put-slot-using-class (class-of object) object slot-name new-value t))

(defsetf get-slot-always (object slot-name &optional default) (new-value)
  `(put-slot-always ,object ,slot-name ,new-value))

(defun remove-dynamic-slot (object slot-name)
  (remove-dynamic-slot-using-class (class-of object) object slot-name))




  ;;   
;;;;;; Actually bootstrapping the meta-braid
  ;;
;;;
;;; *meta-braid* is the list from which the initial meta-classes are created.
;;; The elements look sort of like defstructs.  The car of each element is
;;; the name of the class;  the cadr is the defstruct options;  the caddr is
;;; the slot-descriptions.
;;;
(defvar *meta-braid*
        '((t
            ((:include ()))
            ())
          (object
            ((:include (t)))
            ())
          (essential-class
            ((:include (object))
             (:conc-name class-))
            ((name nil)                    ;A symbol, the name of the class.
             (class-precedence-list ())    ;The class's class-precedence-list
					   ;see compute-class-precedence-list
             (local-supers ())		   ;This class's direct superclasses.
	     (local-slots ())
             (direct-subclasses ())	   ;All the classes which have this
					   ;class on their local-supers.
	     (direct-methods ())
	     ))
          (basic-class
            ((:include (essential-class))
	     (:conc-name class-))
            ((no-of-instance-slots 0)      ;The # of slots with :allocation :instance
                                           ;in an instance of this class.
             (instance-slots ())           ;The slotds of those slots.
             (non-instance-slots ())       ;The declared slots with :allocation other
                                           ;than :instance.  instance-slots + non-
                                           ;instance-slots = all-slots.
             (wrapper nil)                 ;The class-wrapper which instances of
                                           ;this class point to.
	     (direct-discriminators ())
	     (discriminators-which-combine-methods ())
             (prototype nil :get-function (lambda (c slot-name)
                                            (ignore slot-name)
                                            (or (get-slot c 'prototype)
                                                (setf (get-slot c 'prototype)
                                                      (make c)))))      
             (ds-options ())))
	  (class
	    ((:include (basic-class)))
	    ())))

;;;
;;; *bootstrap-slots* is a list of the slotds corresponding to the slots of class
;;; class with :allocation :instance.  It is used by bootstrap-get-slot during the
;;; bootstrapping of the meta-braid.
;;;
(defvar *bootstrap-slots*)

(defmacro bootstrap-get-slot (iwmc-class slot-name)
  `(get-static-slot--class ,iwmc-class
        (%convert-slotd-position-to-slot-index 
          (slotd-position ,slot-name *bootstrap-slots*))))

(defun bootstrap-initialize (iwmc-class name includes local-slots
                                        prototype wrapper ds-options)
  (let ((cpl ())
        (all-slots ())
        (instance-slots ()))
    (setf (bootstrap-get-slot iwmc-class 'name) name)
    (setf (bootstrap-get-slot iwmc-class 'local-supers)
          (iterate ((i in includes)) (collect (class-named i))))
    (setf (bootstrap-get-slot iwmc-class 'class-precedence-list)
          (setq cpl (bootstrap-compute-class-precedence-list iwmc-class)))
    (setq all-slots (append (iterate ((super in (reverse (cdr cpl))))
                              (join (bootstrap-get-slot super 'local-slots)))
                            local-slots))
    (setf (bootstrap-get-slot iwmc-class 'instance-slots)
          (setq instance-slots (slots-with-allocation () all-slots :instance)))
    (setf (bootstrap-get-slot iwmc-class 'non-instance-slots)
          (slots-with-allocation-not () all-slots :instance))
    (setf (bootstrap-get-slot iwmc-class 'no-of-instance-slots)
          (length instance-slots))
    (setf (bootstrap-get-slot iwmc-class 'local-slots) local-slots)
    (setf (bootstrap-get-slot iwmc-class 'direct-discriminators) ())
    (setf (bootstrap-get-slot iwmc-class 'direct-methods) ())
    (setf (bootstrap-get-slot iwmc-class 'prototype) prototype)
    (setf (bootstrap-get-slot iwmc-class 'wrapper) wrapper)
    (setf (bootstrap-get-slot iwmc-class 'ds-options) ds-options)))

(defun bootstrap-compute-class-precedence-list (class)
  ;; Used by define-meta-braid to compute the class-precedence-list of a class.
  (let ((local-supers (bootstrap-get-slot class 'local-supers)))
    (cons class
          (and local-supers
               (iterate ((ls in local-supers))
                 (join (bootstrap-compute-class-precedence-list ls)))))))

;;; bootstrap-meta-braid sets *bootstrap-slots* and builds the meta-braid.
;;; Note that while it is somewhat general-purpose and driven off of *meta-braid*,
;;; it has several important built-in assumptions about the meta-braid.
;;; Namely:
;;;  - The class of every class in the meta-braid is class.
;;;  - The class class inherits its slots from every other class in the
;;;    meta-braid.  Put another way, bootstrap-meta-braid figures out the
;;;    slots of class by appending the slots of all the other classes
;;;    in the meta-braid.
;;;   
(defmacro bootstrap-meta-braid ()
  ;; Parse *meta-braid* and setup *bootstrap-slots* so that we can call
  ;; bootstrap-get-slot to fill in the slotds of the classes we create.
  (let* ((meta-braid
           (iterate ((classd in *meta-braid*))
             (let* ((name (car classd))
                    (ds-options (parse-defstruct-options ()
							 name
							 (cadr classd)))
                    (slotds (parse-slot-descriptions ()
						     ds-options
						     (caddr classd))))
               (collect (list name ds-options slotds)))))
         (all-slots-of-class-class
           (iterate ((classd in meta-braid))
             (join (caddr classd)))))
    (setq *bootstrap-slots* (slots-with-allocation ()
                                                   all-slots-of-class-class
                                                   :instance))
    `(progn      
       (setq *bootstrap-slots* ',*bootstrap-slots*)
       ;; First make the class class.  It is the class of all the classes in
       ;; the metabraid so we need it and a wrapper of it so that we can set
       ;; the wrapped class field of the other metaclasses as we make them.
       (let* ((class-class
		(%allocate-class-class ,(length *bootstrap-slots*)))
              (wrapper-of-class-class (make-class-wrapper class-class)))
         ,@(iterate ((classd in meta-braid))
             (collect
               (destructuring-bind (met-name met-ds-options met-slotds)
				   classd
                 (let ((met-includes (ds-options-includes met-ds-options)))
                   `(let* ((name ',met-name)
                           (includes ',met-includes)
                           (ds-options ',met-ds-options)
                           (slotds ',met-slotds)
                           (class ,(if (eq met-name 'class)
                                       'class-class
                                       `(%allocate-instance--class
                                          ,(length *bootstrap-slots*)
					  (class-named 'class))))
                           (class-wrapper ,(if (eq met-name 'class)
                                               'wrapper-of-class-class
                                               '(make-class-wrapper class))))
                      (setf (iwmc-class-class-wrapper class)
			    wrapper-of-class-class)
                      (setf (class-named name) class)
                      (bootstrap-initialize class
                                            name
                                            includes
                                            slotds
                                            (if (eq class class-class)
						class
						())
                                            class-wrapper
                                            ds-options))))))
         (let ((class-cpl (bootstrap-get-slot class-class
					      'class-precedence-list)))
           (iterate ((sub in class-cpl)
                     (sup in (cdr class-cpl)))
             (push sub (bootstrap-get-slot sup 'direct-subclasses)))))
       ;; CLASS-INSTANCE-SLOTS has to be defined specially!
       ;; It cannot be defined in terms of get-slot since it is the method
       ;; that the get-slot mechanism (actually get-slot-using-class) appeals
       ;; to to find out what slots are in an instance of a particular class.
       ;;
       ;; The fact that class-instance-slots is defined specially this way
       ;; means that any change to the class class which changes the location
       ;; of the instance-slots slot must redefine and recompile
       ;; class-instance-slots.
       (defun class-instance-slots (class)
         (get-static-slot--class class
           ,(%convert-slotd-position-to-slot-index
              (slotd-position 'instance-slots *bootstrap-slots*))))
       (defun class-non-instance-slots (class)
         (get-static-slot--class class
           ,(%convert-slotd-position-to-slot-index
              (slotd-position 'non-instance-slots *bootstrap-slots*))))
       ;; Now define the other accessors and :setf methods for those
       ;; accessors.
       ,@(iterate ((classd in meta-braid))
           (destructuring-bind (name () slotds) classd
             (join
               (iterate ((slotd in slotds))
                 (let* ((slot-name (slotd-name slotd))
                        (accessor-name (slotd-accessor slotd)))
                   (unless (memq slot-name '(instance-slots
					     non-instance-slots))
                     (collect
                       `(defmeth ,accessor-name ((,name ,name))
                          (funcall ,(or (slotd-get-function slotd)
					''get-slot)
                                   ,name
                                   ',(slotd-name slotd)))))
                   (collect
                     `(defmeth (,accessor-name (:setf (.new_value.)))
				((,name ,name))
                        (funcall ,(or (slotd-put-function slotd) ''put-slot)
                                 ,name
                                 ',(slotd-name slotd)
                                 .new_value.))))))))
       t)))


(eval-when (eval load)
  (clrhash *class-name-hash-table*)
  (bootstrap-meta-braid)
  (recompile-class-of))

(defmeth class-slots ((class class))
  (append (class-non-instance-slots class)
	  (class-instance-slots class)))

(defmeth (class-direct-methods (:setf (nv))) ((class class))
  (setf (get-slot class 'direct-methods) nv)
  (dolist (m nv) (pushnew (method-discriminator m)
			  (get-slot class 'direct-discriminators))))

