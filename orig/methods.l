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

(in-package 'pcl)

  ;;   
;;;;;; Methods
  ;;   

(ndefstruct (essential-method
	      (:class class)
	      (:conc-name method-))
  (discriminator nil)
  (arglist ())
  (type-specifiers ())
  (function nil))

(ndefstruct (combinable-method-mixin (:class class)))

(ndefstruct (basic-method
	      (:class class)
	      (:include (essential-method))
	      (:constructor make-method-1)
	      (:conc-name method-))
  (function nil)
  (discriminator nil)
  (type-specifiers ())
  (arglist ())
  (options () :allocation :dynamic))

(ndefstruct (method (:class class)
		    (:include (combinable-method-mixin
			       basic-method))))


(ndefstruct (essential-discriminator
	      (:class class)
	      (:conc-name discriminator-))
  (name nil)
  (methods ())
  (discriminating-function ())
  (classical-method-table nil :allocation :dynamic)
  (cache ()))

(ndefstruct (method-combination-mixin (:class class)
				      (:conc-name nil))
  (method-combination-type :daemon)
  (method-combination-parameters ())
  (methods-combine-p ())
  )

(ndefstruct (basic-discriminator
	      (:class class)
	      (:include (essential-discriminator))
	      (:constructor make-discriminator-1)
	      (:conc-name discriminator-))

  (dispatch-order :default)  
  (inactive-methods () :allocation :dynamic))

(ndefstruct (discriminator (:class class)
			   (:include (method-combination-mixin
				      basic-discriminator)))
  )

;;;
;;; This is really just for bootstrapping, of course this isn't all
;;; worked out yet.  But this SHOULD really just be for bootstrapping.
;;; 
(defmeth method-causes-combination-p ((method basic-method))
  (ignore method)
  ())

  ;;   
;;;;;; 
  ;;   


(defun real-expand-defmeth (name&options arglist body)
  (unless (listp name&options) (setq name&options (list name&options)))
  (keyword-parse ((discriminator-class 'discriminator)
                  (method-class 'method))
                 (cdr name&options)
    (dolist (x '(:discriminator-class :method-class))
      (delete x name&options :test #'(lambda (x y)
				       (and (listp y) (eq (car y) x)))))
    (let ((discriminator-class-object (class-named discriminator-class t))
          (method-class-object (class-named method-class t)))
      (or discriminator-class-object		;
          (error
	    "The :DISCRIMINATOR-CLASS option to defmeth was used to specify~
             that the class~%of the discriminator should be ~S;~%~
             but there is no class named ~S."
	    discriminator-class
	    discriminator-class))
      (or method-class-object
          (error "The :METHOD-CLASS option to defmeth was used to specify~%~
                  that the class of the method should be ~S;~%~
                  but there is no class named ~S."
                 method-class
                 method-class))
      (expand-defmeth-internal (class-prototype discriminator-class-object)
			       (class-prototype method-class-object)
			       name&options
			       arglist
			       body))))

(defvar *method-being-defined*)

(defmeth expand-defmeth-internal ((proto-discriminator basic-discriminator)
				  (proto-method basic-method)
				  name&options arglist body)
  (keyword-parse ((setf () setf-specified-p))
                 (cdr name&options)
    (let* ((discriminator-class-name (class-name
				       (class-of proto-discriminator)))
           (method-class-name (class-name (class-of proto-method)))
           (name (car name&options))
           (merged-arglist (cons (car arglist) (append setf (cdr arglist))))
           (merged-args (arglist-without-type-specifiers proto-discriminator
                                                         proto-method
                                                         merged-arglist))
           (merged-type-specifiers
	     (defmethod-argument-specializers arglist))
           discriminator-name
           method-name
	   (defmethod-uid (gensym))
	   (load-method-1 ())
	   (documentation ())
	   (declarations ()))
      (if setf-specified-p
	  (setq discriminator-name (make-setf-discriminator-name name)
		method-name (make-setf-method-name name
						   (arglist-type-specifiers
						     proto-discriminator
						     proto-method
						     setf)
						   merged-type-specifiers))
	  (setq discriminator-name name
		method-name (make-method-name name
					      merged-type-specifiers)))
      (multiple-value-setq (documentation declarations body)
	(extract-declarations body))
      (setq load-method-1 `(,discriminator-class-name
			    ,method-class-name
			    ,discriminator-name
			    ,merged-type-specifiers
			    ,merged-args
			    ,(cdr name&options)))
      ;;
      ;; There are 4 cases:
      ;;   - evaluated
      ;;   - compiled to core
      ;;   - compiled to file
      ;;   - loading the compiled file
      ;;
      ;; When loading a method which has a run-super in it, there is no way
      ;; to know which of two events will happen first:
      ;;   1. the load-time-eval form in the run super will be
      ;;      evaluated first, or
      ;;   2. the function to install the loaded method (defmethod-uid)
      ;;      will be evaluated first.
      ;; consequently, both the special function (defmethod-uid) and the
      ;; expansion of run-super must check to see if the other has already
      ;; run and set the value of defmethod-uid to the method involved.
      ;; This is what causes the boundp checks of defmethod-uid each time
      ;; before it is set.
      ;; 
      `(progn
	 
	 (eval-when (eval load)
	   
	   (defun ,defmethod-uid ()
	     (declare (special ,defmethod-uid))
	     (unless (boundp ',defmethod-uid)
	       (setq ,defmethod-uid (apply #'load-method-1
					   ',load-method-1)))
	     ,@(and *real-methods-exist-p*
		    `((record-definition
			',discriminator-name 'method
			',merged-type-specifiers ',(cdr name&options))
		      (setf (symbol-function ',method-name)
			    #'(lambda ,merged-args
				,@documentation
				,@declarations
				(declare (method-function-name ,method-name))
				,(wrap-method-body
				   proto-discriminator
				   (apply 'compile-method-1 load-method-1)
				   discriminator-name
				   defmethod-uid
				   load-method-1
				   body)
				))))
	     
	     (setf (method-function ,defmethod-uid)
		   (symbol-function ',method-name))
	     
	     (add-method (discriminator-named ',discriminator-name)
			 ,defmethod-uid
			 ()))
	   
	   (,defmethod-uid))
	 
	 (eval-when (compile load eval)
	   
	   ,@(and setf-specified-p
		  `((record-definition
		      ',name 'defsetf ',discriminator-name 'defmeth)
		    (defsetf ,name
			     ,(arglist-without-type-specifiers
				proto-discriminator proto-method arglist)
			     ,(arglist-without-type-specifiers
				proto-discriminator proto-method setf)
		      (list ',discriminator-name ,@(arglist-args
						     proto-discriminator
						     proto-method
						     merged-args)))))
	   
	   ',discriminator-name)))))

(defmethod wrap-method-body ((mex-generic-function discriminator)
			     (mex-method method)
			     generic-function-name
			     method-uid
			     load-method-1-args
			     body)
  (let ((macroexpand-time-information (list mex-generic-function
					    mex-method
					    generic-function-name
					    method-uid
					    load-method-1-args)))
    `(macrolet ,(iterate (((name arglist params fn) in *method-body-macros*))
		  (collect `(,name ,arglist
			       (funcall (function ,fn)
					',macroexpand-time-information
					,@params))))
       (block ,generic-function-name
	 . ,body))))

(defun macroexpand-time-generic-function (mti) (nth 0 mti))

(defun macroexpand-time-method (mti) (nth 1 mti))

(defun macroexpand-time-generic-function-name (mti) (nth 2 mti))

(defun macroexpand-time-method-uid (mti) (nth 3 mti))

(defun macroexpand-time-load-method-1-args (mti) (nth 4 mti))


(defun load-method-1 (discriminator-class-name
		       method-class-name
		       discriminator-name
		       method-type-specifiers
		      method-arglist
		      options)
  (let* ((discriminator
	   (ensure-selector-specializable
	     (class-prototype (class-named discriminator-class-name))
	     discriminator-name
	     method-arglist))
	 (method
	   (or (find-method discriminator method-type-specifiers options t)
	       (make method-class-name))))
    (setf (method-arglist method) method-arglist)
    (setf (method-type-specifiers method)
	  (parse-type-specifiers
	    discriminator method method-type-specifiers))
    (setf (method-options method) options)
    method))

(defun compile-method-1 (discriminator-class-name
			 method-class-name
			 discriminator-name
			 method-type-specifiers
			 method-arglist
			 options)
  (ignore discriminator-name)
  (let ((method (make method-class-name)))
    (setf (method-arglist method) method-arglist)
    (setf (method-type-specifiers method)
          (parse-type-specifiers
	    (class-prototype (class-named discriminator-class-name))
	    method
	    method-type-specifiers))
    (setf (method-options method) options)
    method))



(defmeth add-named-method ((proto-discriminator essential-discriminator)
			   (proto-method essential-method)
			   discriminator-name
			   arglist
			   type-specs
			   extra
			   function)
  ;; What about changing the class of the discriminator if there is
  ;; one.  Whose job is that anyways.  Do we need something kind of
  ;; like class-for-redefinition?
  (let* ((discriminator
	   ;; Modulo bootstrapping hair, this is just:
	   ;;   (or (discriminator-named ..)
	   ;;       (make-specializable))
	   (ensure-selector-specializable proto-discriminator
					  discriminator-name
					  arglist))
	 (existing (find-method discriminator type-specs extra t))
	 (method (or existing
		     (make (class-of proto-method)))))
    (when existing (change-class method (class-of proto-method)))
    (setf (method-arglist method) arglist)
    (setf (method-function method) function)
    (setf (method-type-specifiers method) type-specs)
    (add-method discriminator method extra)))

(defmeth add-method ((discriminator essential-discriminator)
		     (method essential-method)
		     extra)
  (ignore extra)
  (let ((type-specs (method-type-specifiers method))
       ;(options (method-options method))
       ;(methods (discriminator-methods discriminator))
	)
    (setf (method-discriminator method) discriminator)
;    ;; Put the new method where it belongs, either:
;    ;;  - The same (EQ) method object is already on discriminator-methods
;    ;;    of the discriminator so we don't need to do anything to put the
;    ;;    new methods where it belongs.
;    ;;  - There is an method on discriminator-methods which is equal to
;    ;;    the new method (according to METHOD-EQUAL).  In this case, we
;    ;;    replace the existing method with the new one.
;    ;;  - We just add the new method to discriminator-methods by pushing
;    ;;    it onto that list.
;    (unless (memq method methods)
;      (do* ((tail (discriminator-methods discriminator) (cdr tail))
;	    (existing-method (car tail) (car tail)))
;	   ((cond ((null existing-method)		 
;		   (push method (discriminator-methods discriminator)))
;		  ((method-equal existing-method type-specs options)
;		   (remove-method discriminator existing-method)
;		   (return (add-method discriminator method))))
;	    
;	    (when (method-causes-combination-p method)	         ;NOT part of
;	      (pushnew method (methods-combine-p discriminator)));standard
;						                 ;protocol.
;	    (dolist (argument-specifier type-specs)
;	      (add-method-on-argument-specifier discriminator
;						method
;						argument-specifier)))
;	()))
    (pushnew method (discriminator-methods discriminator))
    (dolist (argument-specifier type-specs)
      (add-method-on-argument-specifier discriminator
					method
					argument-specifier)))
    (discriminator-changed discriminator method t)
    (update-pretty-arglist discriminator method)	;NOT part of
						        ;standard protocol.
    ())


(defmeth remove-named-method (discriminator-name
			      argument-specifiers
			      &optional extra)
  (let ((discriminator ())
	(method ()))
    (cond ((null (setq discriminator (discriminator-named
				       discriminator-name)))
	   (error "There is no discriminator named ~S." discriminator-name))
	  ((null (setq method (find-method discriminator
					   argument-specifiers 
					   extra
					   t)))
	   (error "There is no method for the discriminator ~S~%~
                   which matches the argument-specifiers ~S."
		  discriminator
		  argument-specifiers))
	  (t
	   (remove-method discriminator method)))))

(defmeth remove-method ((discriminator basic-discriminator) method)
  (setf (method-discriminator method) nil)
  (setf (discriminator-methods discriminator)
	(delq method (discriminator-methods discriminator)))
  (dolist (type-spec (method-type-specifiers method))
    (remove-method-on-argument-specifier discriminator method type-spec))
  (discriminator-changed discriminator method nil)
  discriminator)



(defmeth add-method-on-argument-specifier
	 ((discriminator essential-discriminator)
	  (method essential-method)
	  argument-specifier)
  (ignore method)
  (when (classp argument-specifier)
    (pushnew method
	     (class-direct-methods argument-specifier))
    ;; This is a bug.  This needs to be split up into a method on
    ;; essential class and a method on class or something.
    (when (methods-combine-p discriminator)
      (pushnew discriminator
	       (class-discriminators-which-combine-methods
		 argument-specifier)))))

(defmeth remove-method-on-argument-specifier
	 ((discriminator essential-discriminator)
	  (method essential-method)
	  argument-specifier)
  (ignore method)
  (when (classp argument-specifier)
    (setf (class-direct-methods argument-specifier)
	  (delq method
		(class-direct-methods argument-specifier)))
    (when (methods-combine-p discriminator)
      (setf (class-discriminators-which-combine-methods
	      argument-specifier)
	    (delq discriminator
		  (class-discriminators-which-combine-methods
		    argument-specifier))))))


(defun make-specializable (function-name &rest options)
  (when options (setq options (list* ':allow-other-keys t options)))
  (keyword-bind ((arglist nil arglist-specified-p)
		 (discriminator-class 'discriminator)
		 (dispatch nil dispatch-p))
		options
    (cond ((not (null arglist-specified-p)))
	  ((fboundp 'function-arglist)
	   ;; function-arglist exists, get the arglist from it.
	   ;; Note: the funcall of 'function-arglist prevents
	   ;;       compiler warnings at least in some lisps.
	   (setq arglist (funcall 'function-arglist function-name)))
	  ((fboundp function-name)
	   (error
	     "The :arglist argument to make-specializable was not supplied~%~
              and there is no version of FUNCTION-ARGLIST defined for this~%~
              port of Portable CommonLoops.~%~
              You must either define a version of FUNCTION-ARGLIST (which~%~
              should be easy), and send it off to the Portable CommonLoops~%~
              people or you should call make-specializable again with the~%~
              function's arglist as its second argument.")))
    (setq dispatch
	  (if dispatch-p
	      (iterate ((disp in dispatch))
		(unless (memq disp arglist)
		  (error "There is a symbol in the :dispatch argument (~S)~%~
                          which isn't in the arglist."))
		(collect (position disp arglist)))
	      :default))
    (let ((discriminator-class-object
	    (if (classp discriminator-class)
		discriminator-class
		(class-named discriminator-class t)))
	  (discriminator nil))
      (if (null discriminator-class-object)
	  (error
	    "The :DISCRIMINATOR-CLASS argument to make-specializable is ~S~%~
             but there is no class by that name."
	    discriminator-class)
	  (setq discriminator             
		(apply #'make discriminator-class-object
		       :name function-name
		       :dispatch-order dispatch
		       options)))
;     (setf (function-pretty-arglist function-name) arglist)
      (if arglist-specified-p
	  (put-slot-always discriminator 'pretty-arglist arglist)
	  (remove-dynamic-slot discriminator 'pretty-arglist))
      (setf (discriminator-named function-name) discriminator)
      (when (fboundp function-name)
	(add-named-method (class-prototype (class-named 'discriminator))
			  (class-prototype (class-named 'method))
			  function-name
			  arglist
			  ()
			  ()
			  (symbol-function function-name)))
      discriminator)))





(defun update-pretty-arglist (discriminator method)
  (setf (function-pretty-arglist
	  (or (discriminator-name discriminator)
	      (discriminator-discriminating-function discriminator)))
	(or (get-slot-using-class (class-of discriminator) discriminator
				  'pretty-arglist t ())
	    (method-arglist method))))

(defmeth discriminator-pretty-arglist ((discriminator basic-discriminator))
  (or (get-slot-using-class (class-of discriminator) discriminator
			    'pretty-arglist t ())
      (let ((method (or (discriminator-default-method discriminator)
			(car (discriminator-methods discriminator)))))
	(and method (method-arglist method)))))

(defmeth ensure-selector-specializable ((proto-discriminator
					   essential-discriminator)
					 selector arglist)
  (let ((discriminator (discriminator-named selector)))
    (cond ((not (null discriminator)) discriminator)
          ((or (not (fboundp selector))
               (eq *error-when-defining-method-on-existing-function*
		   'bootstrapping))
           (setf (discriminator-named selector)
                 (make (class-of proto-discriminator) :name selector)))
          ((null *error-when-defining-method-on-existing-function*)
           (make-specializable selector
			       :arglist arglist
			       :discriminator-class (class-of
						      proto-discriminator))
           (discriminator-named selector))
          (t
           (error "Attempt to add a method to the lisp function ~S without~%~
                   first calling make-specializable.  Before attempting to~
                   define a method on ~S~% you should evaluate the form:~%~
                   (~S '~S)"
                  selector selector 'make-specializable selector)))))

(defmeth find-method (discriminator type-specifiers options &optional parse)
  (iterate ((method in (discriminator-methods discriminator)))
    (when (method-equal method
			(if parse
			    (parse-type-specifiers discriminator
						   method
						   type-specifiers)
			    type-specifiers)
			options)
      (return method))))

(defmeth method-equal ((method basic-method) argument-specifiers options)
  (and (equal options (method-options method))
       (equal argument-specifiers (method-type-specifiers method))))


(defmeth discriminator-default-method ((discriminator essential-discriminator))
  (find-method discriminator () ()))

(defmeth install-discriminating-function ((discriminator
					    essential-discriminator)
					  where
					  function
					  &optional inhibit-compile-p)
  (ignore discriminator)
  (check-type where symbol "a symbol other than NIL")
  (check-type function function "a funcallable object")
  
  (when (and (listp function)
	     (eq (car function) 'lambda)
	     (null inhibit-compile-p))
    (setq function (compile nil function)))

  (if where
      (setf (symbol-function where) function)
      (setf (discriminator-discriminating-function discriminator) function)))


  ;;   
;;;;;; Discriminator-Based caching.
  ;;
;;; Methods are cached in a discriminator-based cache.  The cache is an N-key
;;; cache based on the number of specialized arguments the discriminator has.
;;; As yet the size of the cache does not change statically or dynamically.
;;; Because of this I allow myself the freedom of computing the mask at
;;; compile time and not even storing it in the discriminator.

(defvar *default-discriminator-cache-size* 8)

(defun make-discriminator-cache (&optional
				  (size *default-discriminator-cache-size*))
  (make-memory-block size))

(defun make-discriminator-cache-mask (discriminator-cache
				      no-of-specialized-args)
  (make-memory-block-mask (memory-block-size discriminator-cache)
                          (+ no-of-specialized-args 1)))

(defmeth flush-discriminator-caches ((discriminator essential-discriminator))
  (let ((cache (discriminator-cache discriminator)))
    (when cache (clear-memory-block (discriminator-cache discriminator) 0))))

(defmeth initialize-discriminator-cache ((self essential-discriminator)
                                            no-of-specialized-args)
  (ignore no-of-specialized-args)
  (unless (discriminator-cache self)
    (setf (discriminator-cache self) (make-discriminator-cache))))

(defmacro discriminator-cache-offset (mask &rest classes)
  `(logand ,mask
           ,@(iterate ((class in classes))
	       (collect `(object-cache-no ,class ,mask)))))

(defmacro discriminator-cache-entry (cache offset offset-from-offset)
  `(memory-block-ref ,cache (+ ,offset ,offset-from-offset)))

(defmacro cache-method (cache mask method-function &rest classes)
  `(let* ((.offset. (discriminator-cache-offset ,mask ,@classes)))
     ;; Once again, we have to endure a little brain damage because we can't
     ;; count on having without-interrupts.  I suppose the speed loss isn't
     ;; too significant since this is only when we get a cache miss.
     (setf (discriminator-cache-entry ,cache .offset. 0) nil)
     ,@(iterate ((class in (cdr classes)) (key-no from 1))
         (collect `(setf (discriminator-cache-entry ,cache .offset. ,key-no)
			 ,class)))
     (prog1
       (setf (discriminator-cache-entry ,cache .offset. ,(length classes))
	     ,method-function)
       (setf (discriminator-cache-entry ,cache .offset. 0) ,(car classes)))))

(defmacro cached-method (var cache mask &rest classes)
  `(let ((.offset. (discriminator-cache-offset ,mask . ,classes)))
     (and ,@(iterate ((class in classes) (key-no from 0))
              (collect
                `(eq (discriminator-cache-entry ,cache .offset. ,key-no)
		     ,class)))
          (setq ,var (discriminator-cache-entry ,cache
						.offset.
						,(length classes)))
          t)))

(defmeth make-caching-discriminating-function (discriminator lookup-function
							      cache
							      mask)
  (multiple-value-bind (required restp specialized-positions)
      (compute-discriminating-function-arglist-info discriminator)
    (funcall (get-templated-function-constructor
	       'caching-discriminating-function
	       required
	       restp
	       specialized-positions
	       lookup-function)
             discriminator cache mask)))

(defun make-checking-discriminating-function (discriminator method-function
                                                            type-specs
							    default-function)
  (multiple-value-bind (required restp)
      (compute-discriminating-function-arglist-info discriminator)
    (let ((check-positions
	    (iterate ((type-spec in type-specs)
		      (pos from 0))
	      (collect (and (neq type-spec 't) pos)))))
      (apply (get-templated-function-constructor
	       'checking-discriminating-function
	       required
	       restp
	       (if default-function t nil)
	       check-positions)
             discriminator method-function default-function type-specs))))


  ;;   
;;;;;; 
  ;;   

(defvar *always-remake-discriminating-function* nil)

(defmeth make-discriminating-function ((discriminator
					 essential-discriminator))
  (let ((default (discriminator-default-method discriminator))
        (methods (discriminator-methods discriminator)))
    (cond ((null methods)
	   (make-no-methods-discriminating-function discriminator))
	  ((and default (null (cdr methods)))
           (make-default-method-only-discriminating-function discriminator))
          ((or (and default (null (cddr methods)))
	       (and (null default) (null (cdr methods))))
           (make-single-method-only-discriminating-function discriminator))
          ((every #'(lambda (m)
                      (classical-type-specifiers-p
			(method-type-specifiers m)))
                  methods)
           (make-classical-methods-only-discriminating-function
	     discriminator))
          (t
           (make-multi-method-discriminating-function discriminator)))))

(defmeth make-no-methods-discriminating-function (discriminator)
  (install-discriminating-function
    discriminator
    (discriminator-name discriminator)
    #'(lambda (&rest ignore)
	(error "There are no methods on the discriminator ~S,~%~
                so it is an error to call it."
	       discriminator))))

(defmeth make-default-method-only-discriminating-function
	 ((self essential-discriminator))
  (install-discriminating-function
    self
    (discriminator-name self)
    (method-function (discriminator-default-method self))))

(defmeth make-single-method-only-discriminating-function
	  ((self essential-discriminator))
  (let* ((methods (discriminator-methods self))
	 (default (discriminator-default-method self))
	 (method (if (eq (car methods) default)
		     (cadr methods)
		     (car methods)))
         (method-type-specifiers (method-type-specifiers method))
         (method-function (method-function method)))
    (install-discriminating-function
      self
      (discriminator-name self)
      (make-checking-discriminating-function
	self
	method-function
	method-type-specifiers
	(and default (method-function default))))))

(defmeth make-classical-methods-only-discriminating-function
	  ((self essential-discriminator))
  (initialize-discriminator-cache self 1)
  (let ((default-method (discriminator-default-method self))
	(methods (discriminator-methods self)))
    (setf (discriminator-classical-method-table self)
	  (cons (and default-method (method-function default-method))
		(iterate ((method in methods))
		  (unless (eq method default-method)
		    (collect (cons (car (method-type-specifiers method))
				   (method-function method))))))))
  (let* ((cache (discriminator-cache self))
	 (mask (make-discriminator-cache-mask cache 1)))
    (install-discriminating-function
      self
      (discriminator-name self)
      (make-caching-discriminating-function
	self 'lookup-classical-method cache mask))))

(defun lookup-classical-method (discriminator class)
  ;; There really should be some sort of more sophisticated protocol going
  ;; on here.  Compare type-specifiers and all that.
  (let* ((classical-method-table
	   (get-slot--class discriminator 'classical-method-table)))
    (or (iterate ((super in (get-slot--class class 'class-precedence-list)))
          (let ((hit (assq super (cdr classical-method-table))))
            (when hit (return (cdr hit)))))
	(car classical-method-table))))

(defmeth make-multi-method-discriminating-function
	  ((self essential-discriminator))
  (multiple-value-bind (required restp specialized)
      (compute-discriminating-function-arglist-info self)
    (ignore required restp)
    (initialize-discriminator-cache self (length specialized))
    (let* ((cache (discriminator-cache self))
	   (mask (make-discriminator-cache-mask cache (length specialized))))
      (install-discriminating-function
	self
	(discriminator-name self)
	(make-caching-discriminating-function
	  self 'lookup-multi-method cache mask)))))

(defvar *lookup-multi-method-internal*
	(make-array (min 256. call-arguments-limit)))

(defun lookup-multi-method-internal (discriminator classes)
  (let* ((methods (discriminator-methods discriminator))
	 (cpls *lookup-multi-method-internal*)
	 (order (get-slot--class discriminator 'dispatch-order))
         (most-specific-method nil)
         (most-specific-type-specs ())
	 (type-specs ()))
    ;; Put all the class-precedence-lists in a place where we can save
    ;; them as we look through all the methods.
    (without-interrupts
      (iterate ((class in classes)
		(i from 0))
	(setf (svref cpls i) (get-slot--class class 'class-precedence-list)))
      (dolist (method methods)
	(setq type-specs (get-slot--class method 'type-specifiers))
	(when (iterate ((type-spec in  type-specs)
			(i from 0))
		(or (eq type-spec 't)
		    (memq type-spec (svref cpls i))
		    (return nil))
		(finally (return t)))
	  (if (null most-specific-method)
	      (setq most-specific-method method
		    most-specific-type-specs type-specs)
	      (case (compare-type-specifier-lists
		      most-specific-type-specs type-specs nil
		      () classes order)
		(2 (setq most-specific-method method
			 most-specific-type-specs type-specs))
		(1))))))
    (or most-specific-method
	(discriminator-default-method discriminator))))

(defun lookup-multi-method (discriminator &rest classes)
  (declare (inline lookup-multi-method-internal))
  (let ((method (lookup-multi-method-internal discriminator classes)))
    (and method (method-function method))))

(defun lookup-method (discriminator &rest classes)
  (declare (inline lookup-multi-method-internal))
  (lookup-multi-method-internal discriminator classes))

  ;;   
;;;;;; Code for parsing arglists (in the usual case).
  ;;   (when discriminator is class DISCRIMINATOR and method is class METHOD)
;;;
;;; arglist-type-specifiers
;;; Given an arglist this returns its type-specifiers.  Trailing T's (both
;;; implicit and explicit) are dropped.  The type specifiers are returned as
;;; they are found in the arglist, they are not parsed into internal
;;; type-specs.
;;;
(defmeth arglist-type-specifiers ((proto-disc basic-discriminator)
				  (proto-meth basic-method)
				  arglist)
  (let ((arg (car arglist)))
    (and arglist
         (not (memq arg '(&optional &rest &key &aux)))  ;Don't allow any
                                                        ;type-specifiers
	                                                ;after one of these.
         (let ((tail (arglist-type-specifiers proto-disc
					      proto-meth
					      (cdr arglist)))
               (type-spec (and (listp arg) (cadr arg))))
           (or (and tail (cons (or type-spec 't) tail))
               (and type-spec (cons type-spec ())))))))

;;; arglist-without-type-specifiers
;;; Given an arglist remove the type specifiers.
;;; 
(defmeth arglist-without-type-specifiers ((proto-disc basic-discriminator)
					  (proto-meth basic-method)
					  arglist)
  (let ((arg (car arglist)))
    (and arglist
         (if (memq arg '(&optional &rest &key &aux))    ;don't allow any
                                                        ;type-specifiers
                                                        ;after one of these.
             arglist
             (cons (if (listp arg) (car arg) arg)
                   (arglist-without-type-specifiers proto-disc
						    proto-meth
						    (cdr arglist)))))))

(defmeth arglist-args ((discriminator-class basic-discriminator)
		       (method-class basic-method)
		       arglist)
  (and arglist
       (cond ((eq (car arglist) '&aux) ())
             ((memq (car arglist) '(&optional &rest &key))
              (arglist-args discriminator-class method-class (cdr arglist)))
             (t
              ;; This plays on the fact that no type specifiers are allowed
	      ;; on arguments that can have default values.
              (cons (if (listp (car arglist)) (caar arglist) (car arglist))
                    (arglist-args discriminator-class
				  method-class
				  (cdr arglist)))))))

(defmeth parse-type-specifiers ((proto-discriminator basic-discriminator)
				(proto-method basic-method)
				type-specifiers)
  (iterate ((type-specifier in type-specifiers))
    (collect (parse-type-specifier proto-discriminator
				   proto-method
				   type-specifier))))

(defmeth parse-type-specifier ((proto-discriminator basic-discriminator)
                                (proto-method basic-method)
                                type-specifier)
  (ignore proto-discriminator proto-method)
  (cond ((eq type-specifier 't) 't)
        ((symbolp type-specifier)
         (or (class-named type-specifier nil)
             (error
	       "~S used as a type-specifier, but is not the name of a class."
	       type-specifier)))
        ((classp type-specifier) type-specifier)
        (t (error "~S is not a legal type-specifier." type-specifier))))

(defmeth unparse-type-specifiers ((method essential-method))
  (iterate ((parsed-type-spec in (method-type-specifiers method)))
    (collect (unparse-type-specifier method parsed-type-spec))))

(defmeth unparse-type-specifier ((method essential-method) type-spec)
  (ignore method)
  (if (classp type-spec)
      (class-name type-spec)
      type-spec))

(defun classical-type-specifiers-p (typespecs)
  (or (null typespecs)
      (and (classp (car typespecs))
           (null (cdr typespecs)))))

;;;
;;; Compute various information about a discriminator's arglist by looking at
;;; the argument lists of the methods.  The hair for trying not to use &rest
;;; arguments lives here.
;;;  The values returned are:
;;;    number-of-required-arguments
;;;       the number of required arguments to this discrimator's
;;;       discriminating function
;;;    &rest-argument-p
;;;       whether or not this discriminator's discriminating
;;;       function takes an &rest argument.
;;;    specialized-argument-positions
;;;       a list of the positions of the arguments this discriminator
;;;       specializes (e.g. for a classical discrimator this is the
;;;       list: (1)).
;;;
;;; As usual, it is legitimate to specialize the -internal function that is
;;; why I put it there, since I certainly could have written this more
;;; efficiently if I didn't want to provide that extensibility.
;;; 
(defmeth compute-discriminating-function-arglist-info
	 ((discriminator essential-discriminator)
	  &optional (methods () methods-p))
  (declare (values number-of-required-arguments
                   &rest-argument-p
                   specialized-argument-postions))
  (unless methods-p
    (setq methods (discriminator-methods discriminator)))
  (let ((number-required nil)
        (restp nil)
        (specialized-positions ()))
    (iterate ((method in methods))
      (multiple-value-setq (number-required restp specialized-positions)
        (compute-discriminating-function-arglist-info-internal
	  discriminator method number-required restp specialized-positions)))
    (values number-required restp (sort specialized-positions #'<))))

(defmeth compute-discriminating-function-arglist-info-internal
	 ((discriminator essential-discriminator)
	  (method essential-method)
	  number-of-requireds restp specialized-argument-positions)
  (ignore discriminator)
  (let ((requireds 0))
    ;; Go through this methods arguments seeing how many are required,
    ;; and whether there is an &rest argument.
    (iterate ((arg in (method-arglist method)))
      (cond ((eq arg '&aux) (return))
            ((memq arg '(&optional &rest &key))
             (return (setq restp t)))
	    ((memq arg lambda-list-keywords))
            (t (incf requireds))))
    ;; Now go through this method's type specifiers to see which
    ;; argument positions are type specified.  Treat T specially
    ;; in the usual sort of way.  For efficiency don't bother to
    ;; keep specialized-argument-positions sorted, rather depend
    ;; on our caller to do that.
    (iterate ((type-spec in (method-type-specifiers method))
              (pos from 0))
      (unless (eq type-spec 't)
	(pushnew pos specialized-argument-positions)))
    ;; Finally merge the values for this method into the values
    ;; for the exisiting methods and return them.  Note that if
    ;; num-of-requireds is NIL it means this is the first method
    ;; and we depend on that.
    (values (min (or number-of-requireds requireds) requireds)
            (or restp
		(and number-of-requireds (/= number-of-requireds requireds)))
            specialized-argument-positions)))

(defun make-discriminating-function-arglist (number-required-arguments restp)
  (iterate ((i from 0 below number-required-arguments))
    (collect (intern (format nil "Discriminating Function Arg ~D" i)))
    (finally (when restp
               (collect '&rest)
               (collect (intern "Discriminating Function &rest Arg"))))))

(defmeth compare-methods (discriminator method-1 method-2)
  (ignore discriminator)
  (let ((compare ()))
    (iterate ((ts-1 in (method-type-specifiers method-1))
	      (ts-2 in (method-type-specifiers method-2)))
      (cond ((eq ts-1 ts-2) (setq compare '=))
	    ((eq ts-1 't)   (setq compare method-2))
	    ((eq ts-2 't)   (setq compare method-1))	    
	    ((memq ts-1 (class-class-precedence-list ts-2))
	     (setq compare method-2))
	    ((memq ts-2 (class-class-precedence-list ts-1))
	     (setq compare method-1))
	    (t (return nil)))
      (finally (return compare)))))

  ;;   
;;;;;; Comparing type-specifiers, statically or wrt an object.
  ;;
;;; compare-type-specifier-lists compares two lists of type specifiers
;;; compare-type-specifiers compare two type specifiers
;;; If static-p it t the comparison is done statically, otherwise it is
;;; done with respect to object(s).  The value returned is:
;;;    1    if type-spec-1 is more specific
;;;    2    if type-spec-2 is more specific
;;;    =    if they are equal
;;;    NIL  if they cannot be disambiguated
;;;
(defun compare-type-specifier-lists (type-spec-list-1
				     type-spec-list-2
				     staticp
				     args
				     classes
				     order)
  (when (or type-spec-list-1 type-spec-list-2)
    (ecase (compare-type-specifiers (or (car type-spec-list-1) t)
                                    (or (car type-spec-list-2) t)
                                    staticp
                                    (car args)
                                    (car classes))
      (1 '1)
      (2 '2)
      (= (if (eq order :default)
	     (compare-type-specifier-lists (cdr type-spec-list-1)
					   (cdr type-spec-list-2)
					   staticp
					   (cdr args)
					   (cdr classes)
					   order)
	     (compare-type-specifier-lists (nth (car order) type-spec-list-1)
					   (nth (car order) type-spec-list-2)
					   staticp
					   (cdr args)
					   (cdr classes)
					   (cdr order))))
	    
      (nil
        (unless staticp
          (error "The type specifiers ~S and ~S can not be disambiguated~
                  with respect to the argument: ~S"
                 (or (car type-spec-list-1) t)
                 (or (car type-spec-list-2) t)
                 (car args)
                 (car classes)))))))

(defun compare-type-specifiers (type-spec-1 type-spec-2 staticp arg class)
  (cond ((equal type-spec-1 type-spec-2) '=)
        ((eq type-spec-2 t) '1)
        ((eq type-spec-1 t) '2)
        ((and (classp type-spec-1) (classp type-spec-2))
;        (if staticp
;            (if (common-subs type-spec-1 type-spec-2)
;                nil
;                (let ((supers (common-supers type-spec-1 type-spec-2)))
;                  (cond ((cdr supers) nil)
;                        ((eq (car supers) type-spec-1) '2)
;                        ((eq (car supers) type-spec-2) '1)
;                        (t 'disjoint))))
             (iterate ((super in (class-class-precedence-list (or class (class-of arg)))))
               (cond ((eq super type-spec-1)
                      (return '1))
                     ((eq super type-spec-2)
                      (return '2)))))
;)
        (t
         (compare-complex-type-specifiers type-spec-1 type-spec-2 staticp arg class))))

(defun compare-complex-type-specifiers (type-spec-1 type-spec-2 static-p arg class)
  (ignore type-spec-1 type-spec-2 static-p arg class)
  (error "Complex type specifiers are not yet supported."))

(defmeth no-matching-method (discriminator)
  (let ((class-of-discriminator (class-of discriminator)))
    (if (eq (class-of class-of-discriminator) (class-named 'class))
        ;; The meta-class of the discriminator is class, we can get at
        ;; it's name slot without doing any method lookup.
        (let ((name (get-slot--class discriminator 'name)))
          (if (and name (symbolp name))
              (error "No matching method for: ~S." name)
              (error "No matching method for the anonymous discriminator: ~S."
                     discriminator)))
        (error "No matching method for the discriminator: ~S." discriminator))))
  ;;   
;;;;;; Optimizing GET-SLOT
  ;;   

(defmeth method-argument-class ((method basic-method) argument)
  (let* ((arglist (method-arglist method))
         (position (position argument arglist)))
    (and position (nth position (method-type-specifiers method)))))


(defmeth optimize-get-slot ((class basic-class)
			    form)
  (declare (ignore class))
  (cons 'get-slot--class (cdr form)))

(defmeth optimize-setf-of-get-slot ((class basic-class)
				    form)
  (declare (ignore class))
  (cons 'put-slot--class (cdr form)))
