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
;;; User-defined method combination.  A first try.
;;;
;;; For compatibility with New Flavors, the following functions macros and
;;; variables have the same meaning.
;;;   define-simple-method-combination
;;;   define-method-combination
;;;   call-component-method
;;;   call-component-methods
;;;   *combined-method-arguments*
;;;   *combined-method-apply*
;;;
;;; In define-method-combination the arguments have the following meanings:
;;;
;;;   name             the name of this method combination type (symbol)
;;;   parameters       like a defmacro lambda list, it is matched with
;;;                    the value specified by the :method-combination
;;;                    option to make-specializable
;;;   method-patterns  a list of method-patterns specifications that are
;;;                    used to select some subset of the methods defined
;;;                    on the discriminator.  Each method pattern specifies
;;;                    a variable which is bound to a list of the methods
;;;                    it selects.
;;;   body             forms evaluated with the variables specified by
;;;                    the method patterns bound to produce the body of
;;;                    the combined method.  (see call-component-methods).
;;;
;;;  Body can be preceded by any number of options which take the form:
;;;    (<option-name> . <option-args>)
;;;
;;;  Currently, the defined options are:
;;;
;;;   :causes-combination-predicate
;;;       The only argument, should be a function of one argument.  It
;;;       will be called on a method (of the discriminator) and should
;;;       return T if that method causes the discriminator to combine
;;;       its methods.
;;;
;;; A method-patterns looks like:
;;;                    
;;;   (<var> <printer> <filter> <order> <pattern-1> <pattern-2> ..)
;;;
;;;      <var>      is the variable to which the selected methods
;;;                 are bound
;;;      <printer>  is ignored
;;;      <filter>   one of :every, :first, :last or :remove-duplicates
;;;      <order>    :most-specific-first or :most-specific-last
;;;
;;;      Methods matching any of the patterns are selected.  The patterns
;;;      are matched against the method-combination-options of the method
;;;      as specified in the defmeth.
;;; 

(in-package 'pcl)

;;;
;;; The method combination type of a particular method combination is stored
;;; as a symbol (the name of the method-combination) in the discriminator (in
;;; the method-combination-type slot).  Information about that particular
;;; method-combination-type is stored on the property list of the type symbol
;;; 
(defun get-method-combination-info (type &optional no-error-p)
  (or (get type 'method-combination)
      (if no-error-p () (error "No method combination named ~S." type))))

(defun set-method-combination-info (type combiner predicate)
  (setf (get type 'method-combination) (list type combiner predicate)))

(defmeth method-combiner ((discriminator method-combination-mixin))
  (cadr (get-method-combination-info
          (method-combination-type discriminator))))

(defmeth method-causes-combination-predicate
         ((discriminator method-combination-mixin))
  (caddr (get-method-combination-info
           (method-combination-type discriminator))))




  ;;   
;;;;;; COMBINED-METHOD class
  ;;   

(ndefstruct (combined-method (:class class)
                             (:include (method)))
  (deactivated-methods ()))

(defmeth automatically-defined-p ((m combined-method)) (ignore m) t)

(defmeth method-options ((m combined-method)) (ignore m) '(:combined))
                                                
(defmeth method-causes-combination-p ((m combined-method)) (ignore m) nil)

(defmacro define-simple-method-combination (name operator
                                            &optional single-arg-is-value
                                                      (pretty-name
                                                        (string-downcase
                                                          name)))
  `(define-method-combination ,name
                              (&optional (order :most-specific-first))
             ((methods ,pretty-name :every order () (,name) :default))
     `(call-component-methods ,methods
                              :operator ,',operator
                              :single-arg-is-value ,',single-arg-is-value)))

(defmacro define-method-combination (name parameters method-patterns
                                     &body body)
  (check-type parameters list)
  (check-type method-patterns (and list (not null)))
  (make-method-combination name parameters method-patterns body))


(defvar *combined-method-arguments*)
(defvar *combined-method-apply*)
(defvar *combined-method-template*)

;;;
;;; Generate a form that calls a single method.
;;; With no keyword arguments, uses the value of *combined-methods-arguments*
;;; as the arguments to the call;
;;; With :ARGLIST, uses that instead;
;;; With :ARGLIST and :APPLY T, uses APPLY instead of FUNCALL
;;; With just :APPLY, it is the single argument to apply to.
;;;
;;; When called with *combined-method-template* bound, generates calls to
;;; the value of variables gotten from *combined-method-template* instead
;;; of to the actual methods themselves.  This is used to build templates
;;; for combined methods.
;;;
(defmacro call-component-method
          (method &key (apply nil apply-p)
                       (arglist 
                         (if apply-p
                             (prog1 (list apply) (setq apply t))
                             (prog1 *combined-method-arguments*
                                    (setq apply *combined-method-apply*)))))
  (call-component-method-internal method apply arglist))

(defmacro call-component-methods (methods &key (operator 'progn)
                                               (single-arg-is-value nil))
  (call-component-methods-internal methods operator single-arg-is-value))

(defmeth call-component-method-internal
         (method &optional (apply *combined-method-apply*)
                           (arglist *combined-method-arguments*))
  (when method
    `(,(if apply 'apply 'funcall)
      ,(if (boundp '*combined-method-template*)
	   (let ((gensym (cdr (assq method *combined-method-template*))))
	     (if gensym
		 `(the function ,gensym)
		 (error "*combined-method-template* out of sync??")))
	   `',(method-function method))
      ,@arglist)))
  
(defmeth call-component-methods-internal (methods
					  operator single-arg-is-value)
  (when methods
    (if (and single-arg-is-value (null (cdr methods)))
	(call-component-method-internal (car methods))
	`(,operator
	  ,@(iterate ((method in methods))
	      (collect (call-component-method-internal method)))))))

(defmeth call-component-method-equal (discriminator call-1 call-2)
  ;; If the options are the same (the part that the macros control the
  ;; processing of); and the individual calls are the same the part the
  ;; methods themselves control the processing of.
  (and (equal (cddr call-1) (cddr call-2))
       (if (eq (car call-1) 'call-component-method)
	   (cond ((null (cadr call-1)) (null (cadr call-2)))
		 ((null (cadr call-2)) (null (cadr call-1)))
		 (t
		  (call-component-method-equal-internal
		    discriminator (cadr call-1) (cadr call-2))))
           (iterate ((meth-1 on (cadr call-1))
                     (meth-2 on (cadr call-2)))
	     (when (or (and (cdr meth-1) (null (cdr meth-2)))
		       (and (cdr meth-2) (null (cdr meth-1)))
		       (null (call-component-method-equal-internal
			       discriminator (car meth-1) (car meth-2))))
	       (return nil))))))

(defmeth call-component-method-equal-internal (discriminator meth-1 meth-2)
  (ignore discriminator meth-1 meth-2)
  t)



(defvar *method-combination-filters*
        '(:every :first :last :remove-duplicates))

(defvar *method-combination-orders*
        '(:most-specific-first :most-specific-last))

(defun make-method-combination (name parameters method-patterns body)
  (let ((causes-combination-predicate 'true)
        (combiner (make-symbol (string-append name " Method Combiner"))))
    ;; Error check and canonicalize the arguments.
    (unless (symbolp name)
      (error "The name of a method combination type must be a symbol, but ~S~
            was specified."
             name))
    ;; Check the various sub-parts of each method-patterns.  Canonicalize
    ;; each method-pattern by adding the () pattern to it if it has no
    ;; other patterns.
    (iterate ((method-patterns-loc on method-patterns))
      (destructuring-bind (var printer filter order . patterns)
                          (car method-patterns-loc)
        (check-symbol-variability var "bind (in a method-patterns)")
        (or (null (keywordp filter))
            (memq filter *method-combination-filters*)
            (error "A method-patterns filter must be one of: ~S~%not ~S."
                   *method-combination-filters* filter))
        (or (null (keywordp order))
            (memq order *method-combination-orders*)
            (error "A method-patterns order must be one of: ~S~%not ~S."
                   *method-combination-orders* filter))
        (if (null patterns)
            (setf (car method-patterns-loc)
                  (append (car method-patterns-loc) (list nil)))
            (iterate ((pattern in patterns))
              (or (listp pattern)
                  (eq pattern ':default)
                  (error "A method-pattern must be a list.~%~
                         In the method-patterns ~S, ~S is an invalid pattern."
                         (car method-patterns-loc) pattern))))))
    (iterate ()
      (while (and body (listp (car body))))
      (case (caar body)
        (:causes-combination-predicate
          (setq causes-combination-predicate (cadr (pop body))))
        (otherwise (return))))

    `(progn 
       ,(make-combiner-definer
          combiner name parameters method-patterns body)
       (setf (get ',name 'combined-method-templates) ())
       (set-method-combination-info ',name
                                    ',combiner
                                    ',causes-combination-predicate))))

(defun make-combiner-definer
       (combiner name parameters method-patterns body)
  (ignore name)
  `(defun ,combiner (.discriminator. .methods. .params.)
     .discriminator.
     (apply
       #'(lambda ,parameters
           (let ,(iterate (((var) in method-patterns)) (collect `(,var nil)))
             (do ((.method. (pop .methods.) (pop .methods.)))
                 ((null .method.))
               (cond 
                 ,@(iterate (((var nil fil ord . pats) in method-patterns))
                     (collect
		       `((and ,(ecase fil
				 (:first
				   `(if (eq ,ord :most-specific-first)
					(null ,var)
					't))
				 (:last
				   `(if (eq ,ord :most-specific-first)
					t
					(null ,var)))
				 (:every
				   't))
			      (method-matches-patterns-p .method. ',pats))
                         (push .method. ,var))))))
	     ,@(iterate (((var nil fil ord) in method-patterns))
		 (cond ((memq fil '(:first :last))
			(collect `(setq ,var (car ,var))))
		       ((eq ord ':most-specific-first)
			(collect `(setq ,var (nreverse ,var))))))
             ,@body))
       .params.)))


(defmeth method-matches-patterns-p (method patterns)
  (iterate ((pattern in patterns))
    (when (method-matches-pattern-p method pattern)
      (return t))))

(defmeth method-matches-pattern-p (method pattern)
  (iterate ((pats = pattern (cdr pats))
            (opts = (method-options method) (cdr opts)))
    (if (symbolp pats)
        ;; Special case this because it means we have to blow out of
        ;; iterate.  Should iterate should know about dotted lists.
        (return (or (eq pats '*) (eq pats opts)))
        (unless (or (eq (car pats) '*)
                    (equal (car pats) (car opts)))
          (return nil)))    
    (finally (return t))))

(defun patterns-keywords (patterns)
  (let ((keywords ()))
    (iterate ((pattern in patterns))
      (iterate ((elem in pattern))
        (when (keywordp elem) (push elem keywords))))
    keywords))

(defun check-symbol-variability (symbol verb)
  (cond ((not (symbolp symbol))
         (error "Attempt to ~A ~S which is not a symbol" verb symbol))
        ((or (null symbol) (eq symbol 't))
         (error "Attempt to ~A ~S" verb symbol))
        ((eq (symbol-package symbol) (find-package 'keyword))
         (error "Attempt to ~A ~S, which is a keyword" verb symbol))
        ((constantp symbol)
         (error "Attempt to ~A ~S, which is a constant" verb symbol))))

(defun cpl-filter-= (cpl1 cpl2 discriminator)
  (macrolet ((has-method-on-discriminator-p (class)
	       `(memq discriminator (class-direct-discriminators ,class))))
    (prog ()
       restart
          (cond ((null cpl1)
		 (if (null cpl2)
		     (return t)
		     (return nil)))
                ((null cpl2)
                 (return nil)))
          (unless (has-method-on-discriminator-p (car cpl1))
            (pop cpl1)
            (go restart))
          (unless (has-method-on-discriminator-p (car cpl2))
            (pop cpl2)
            (go restart))
          (if (neq (pop cpl1) (pop cpl2))
              (return nil)
	      (go restart)))))


;;;   class-discriminators-which-combine-methods
;;;   discriminator-methods-combine-p

(defmeth combine-methods ((class class) &optional discriminators)
  (let ((cpl (class-class-precedence-list class))
        (method nil)
        (method-cpl nil)
        (combined-method nil))
  
    (iterate ((disc in discriminators))
      (setq method (lookup-method disc class)
	    method-cpl (and method
			    (not (combined-method-p method))
			    (class-class-precedence-list
			      (car (method-type-specifiers method)))))
      (unless (cpl-filter-= cpl method-cpl disc)
	(dolist (other-method (discriminator-methods disc))
	  (when (and (combined-method-p other-method)
		     (eq (car (method-type-specifiers other-method))
			 class))
	    (remove-method disc other-method)))
	(multiple-value-bind (arguments apply-p body)
	    (combine-methods-internal class disc cpl)
	  (setq combined-method 
		(make 'combined-method
		      :function (compile-combined-method
				  disc arguments apply-p body)
		      :arglist arguments
		      :type-specifiers (cons class
					     (cdr (method-type-specifiers
						    method)))))
	  (add-method disc combined-method nil))))))

(defmeth combine-methods-internal (class discriminator cpl)
  (ignore class)
  (let ((methods (iterate ((c in cpl))
                   (join
		     (iterate ((m in (discriminator-methods discriminator)))
		       (when (and (eq (car (method-type-specifiers m)) c)
				  (not (combined-method-p m)))
			 (collect m)))))))
    (multiple-value-bind (required restp)
        (compute-discriminating-function-arglist-info discriminator)
      (let ((*combined-method-arguments*
              (make-discriminating-function-arglist required restp))
            (*combined-method-apply* restp))
        (values *combined-method-arguments*
                *combined-method-apply*
                (funcall (method-combiner discriminator)
                         discriminator methods ()))))))


  ;;   
;;;;;; COMPILE-COMBINED-METHOD
  ;;   

(defmeth compile-combined-method ((discriminator method-combination-mixin)
                                  *combined-method-arguments*
                                  *combined-method-apply*
                                  body)
  (multiple-value-bind (constructor methods-called)
      (compile-combined-method-internal discriminator body)
    (apply constructor (mapcar #'method-function methods-called))))

(defmeth compile-combined-method-internal (discriminator body)
  (let* ((combination-type (method-combination-type discriminator))
         (templates (get combination-type 'combined-method-templates))
         (methods-called ())
         (walked-body 
           (walk-form body
             :walk-function
             #'(lambda (form context &aux temp)
                 (ignore context)
                 (values form
                         (and (eq context 'eval)
                              (listp form)
                              (setq temp (car form))
                              (cond ((eq temp 'call-component-method)
                                     (push (cadr form) methods-called))
                                    ((eq temp 'call-component-methods)
                                     (setq methods-called
                                           (append (cadr form)
                                                   methods-called))))))))))
    (setq methods-called (remove nil methods-called))
    (iterate ((entry in templates))
      (when (combined-method-equal discriminator (car entry) walked-body)
        (return (values (cdr entry) methods-called)))
      (finally	
        (let* ((*combined-method-template*
                 (iterate ((method in methods-called))
                   (collect (cons method (gensym)))))
               (new-constructor
                 (compile ()
                          `(lambda
                             ,(mapcar #'cdr *combined-method-template*)
                             #'(lambda ,*combined-method-arguments*
                                 ,(walk-form walked-body))))))
          (push (cons walked-body new-constructor)
                (get combination-type 'combined-method-templates))
          (return (values new-constructor methods-called)))))))
  
(defmeth combined-method-equal (discriminator comb-meth-1 comb-meth-2)
  (cond ((atom comb-meth-1) (eq comb-meth-1 comb-meth-2))
        ((memq (car comb-meth-1)
               '(call-component-method call-component-methods))
         (and (eq (car comb-meth-1) (car comb-meth-2))
              (call-component-method-equal
                discriminator comb-meth-1 comb-meth-2)))
        (t
         (and (combined-method-equal
                discriminator (car comb-meth-1) (car comb-meth-2))
              (combined-method-equal
                discriminator (cdr comb-meth-1) (cdr comb-meth-2))))))



(defmeth discriminator-changed ((discriminator method-combination-mixin)
				(method combined-method)
				added-p)
  (ignore discriminator method added-p))

(defmeth discriminator-changed ((discriminator method-combination-mixin)
				method
				added-p)
  (when (methods-combine-p discriminator)
    (let ((class (car (method-type-specifiers method))))
      (when (classp class)
	(labels ((walk-tree (class)
		   (combine-methods class (list discriminator))
		   (dolist (subclass (class-direct-subclasses class))
		     (walk-tree subclass))))
	  (walk-tree class)))))
  (run-super))


