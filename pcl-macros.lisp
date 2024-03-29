;;;-*-Mode:LISP; Package:COOL.PCL; Base:10; Syntax:Common-lisp -*-
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
;;; Macros global variable definitions, and other random support stuff used
;;; by the rest of the system.
;;;
;;; For simplicity (not having to use eval-when a lot), this file must be
;;; loaded before it can be compiled.
;;;
(in-package :cool.pcl)

(declaim (declaration #-ansi-cl values 		;I use this so that Zwei can
						;remind me what values a
						;function returns.
			
			method-function-name	;This is used so that some
						;systems can print the name
						;of the method when I am in
						;the debugger.
                        ))

;;; Age old functions which CommonLisp cleaned-up away.  They probably exist
;;; in other packages in all CommonLisp implementations, but I will leave it
;;; to the compiler to optimize into calls to them.
;;;
;;; Common Lisp BUG:
;;;    Some Common Lisps define these in the Lisp package which causes
;;;    all sorts of lossage.  Common Lisp should explictly specify which
;;;    symbols appear in the Lisp package.
;;;    
(defmacro memq (item list) `(member ,item ,list :test #'eq))
(defmacro assq (item list) `(assoc ,item ,list :test #'eq))
(defmacro rassq (item list) `(rassoc ,item ,list :test #'eq))
(defmacro delq (item list) `(delete ,item ,list :test #'eq))
(defmacro neq (x y) `(not (eq ,x ,y)))

(defun make-caxr (n form)
  (if (< n 4)
      `(,(nth n '(car cadr caddr cadddr)) ,form)
      (make-caxr (- n 4) `(cddddr ,form))))

(defun make-cdxr (n form)
  (cond ((zerop n) form)
	((< n 5) `(,(nth n '(identity cdr cddr cdddr cddddr)) ,form))
	(t (make-cdxr (- n 4) `(cddddr ,form)))))

#-ansi-cl
(defmacro ignore (&rest vars)
  #+Symbolics `(progn ,.(remove 'ignore vars))
  #-Symbolics `(progn ,@vars))

(defun true (&rest ignore) (declare (ignore ignore)) t)
(defun false (&rest ignore) (declare (ignore ignore)) nil)

;;; ONCE-ONLY does the same thing as it does in zetalisp.  I should have just
;;; lifted it from there but I am honest.  Not only that but this one is
;;; written in Common Lisp.  I feel a lot like bootstrapping, or maybe more
;;; like rebuilding Rome.
(defmacro once-only (vars &body body)
  (let ((gensym-var (gensym))
        (run-time-vars (gensym))
        (run-time-vals (gensym))
        (expand-time-val-forms ()))
    (dolist (var vars)
      (push `(if (or (symbolp ,var)
                     (numberp ,var)
                     (and (listp ,var)
			  (member (car ,var) '(quote function))))
                 ,var
                 (let ((,gensym-var (gensym)))
                   (push ,gensym-var ,run-time-vars)
                   (push ,var ,run-time-vals)
                   ,gensym-var))
            expand-time-val-forms))    
    `(let* (,run-time-vars
            ,run-time-vals
            (wrapped-body
              ((lambda ,vars . ,body) . ,(reverse expand-time-val-forms))))
       `((lambda ,(nreverse ,run-time-vars)  ,wrapped-body)
         . ,(nreverse ,run-time-vals)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun extract-declarations (body &optional environment)
   (declare (cl:ignore environment))
   ;; (declare (values documentation declares body))
   (let (documentation declares form temp)
     (declare (cl:ignore temp))
     (when (stringp (car body)) (setq documentation (pop body)))
     (loop
       (when (null body) (return))
       (setq form (car body))
       (cond ((and (listp form) (eq (car form) 'declare))
              (push (pop body) declares))
                                        ;	    ((and (neq (setq temp (macroexpand form environment)) form)
                                        ;		  (listp temp)
                                        ;		  (eq (car temp) 'declare))
                                        ;	     (pop body)
                                        ;	     (push temp declares))
             (t (return))))
     (values documentation declares body))))

  ;;   
;;;;;; FAST-NCONC Lists
  ;;
;;; These are based on Interlisp's TCONC function.  They are slighlty
;;; generalized to take either the item to nconc onto the end of the list or
;;; a cons to add to the end of a list. In addition there is a constructor to
;;; make fast-nconc-lists and an accessor to get at a fast-nconc-list's real
;;; list.
(defmacro make-fast-nconc-list ()
  `(let ((fast-nconc-list (cons () (list ()))))
     (rplaca fast-nconc-list (cdr fast-nconc-list))
     fast-nconc-list))

(defmacro fast-nconc-list-real-list (fast-nconc-list)
  `(cddr ,fast-nconc-list))

(defmacro fast-nconc-cons (fast-nconc-list cons)
  (once-only (fast-nconc-list)
    `(progn (rplacd (car ,fast-nconc-list) ,cons)
            (rplaca ,fast-nconc-list (cdar ,fast-nconc-list)))))

(defmacro fast-nconc-item (fast-nconc-list item)
  `(fast-nconc-cons ,fast-nconc-list (cons ,item nil)))

#+Lucid
(cltl1-eval-when (compile load eval)
  (eval `(defstruct ,(intern "FASLESCAPE" (find-package 'lucid)))))

; rds 3/8 added -HP and +HP for make-keyword:
#-HP
(defun make-keyword (symbol)
  (intern (symbol-name symbol) `,(load-time-value (find-package 'keyword))))

#+HP
(defun make-keyword (symbol)
   (intern (symbol-name symbol) (find-package 'keyword)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun string-append (&rest strings)
   (setq strings (copy-list strings))   ;The explorer can't even
                                        ;rplaca an &rest arg?
   (do ((string-loc strings (cdr string-loc)))
       ((null string-loc)
        (apply #'concatenate 'string strings))
     (rplaca string-loc (string (car string-loc)))))

  (defun symbol-append (sym1 sym2 &optional (package *package*))
    (intern (string-append sym1 sym2) package))
  
  (defmacro check-member (place list &key (test #'eql) (pretty-name place))
    (once-only (place list)
      `(or (member ,place ,list :test ,test)
           (error "The value of ~A, ~S is not one of ~S."
                  ',pretty-name ,place ,list)))))



;;; A simple version of destructuring-bind.

;;; This does no more error checking than CAR and CDR themselves do.  Some
;;; attempt is made to be smart about preserving intermediate values.  It
;;; could be better, although the only remaining case should be easy for
;;; the compiler to spot since it compiles to PUSH POP.
;;;
;;; Common Lisp BUG:
;;;    Common Lisp should have destructuring-bind.
;;;    
#-ansi-cl
(defmacro destructuring-bind (pattern form &body body)
  (multiple-value-bind (ignore declares body)
      (extract-declarations body)
    (declare (ignore ignore))
    (multiple-value-bind (setqs binds)
	(destructure pattern form)
      `(let ,binds
	 ,@declares
	 ,@setqs
	 . ,body))))

(defun destructure (pattern form)
  ;; (declare (values setqs binds))
  (let ((*destructure-vars* ())
	(setqs ()))
    (declare (special *destructure-vars*))
    (when (not (symbolp form))
      (setq *destructure-vars* '(.destructure-form.)
	    setqs (list `(setq .destructure-form. ,form)))
      (setq form '.destructure-form.))
    (values (nconc setqs (nreverse (destructure-internal pattern form)))
	    (delete nil *destructure-vars*))))

(defun destructure-internal (pattern form)
  ;; When we are called, pattern must be a list.  Form should be a symbol
  ;; which we are free to setq containing the value to be destructured.
  ;; Optimizations are performed for the last element of pattern cases.
  ;; we assume that the compiler is smart about gensyms which are bound
  ;; but only for a short period of time.
  (declare (special *destructure-vars*))
  (let ((gensym (gensym))
	(pending-pops 0)
	(var nil)
	(setqs ()))
    (labels
        ((make-pop (var form pop-into)
	   (prog1 
	     (cond ((zerop pending-pops)
		    `(progn ,(and var `(setq ,var (car ,form)))
			    ,(and pop-into `(setq ,pop-into (cdr ,form)))))
		   ((null pop-into)
		    (and var `(setq ,var ,(make-caxr pending-pops form))))
		   (t
		    `(progn (setq ,pop-into ,(make-cdxr pending-pops form))
			    ,(and var `(setq ,var (pop ,pop-into))))))
	     (setq pending-pops 0))))
      (do ((pat pattern (cdr pat)))
	  ((null pat) ())
	(if (symbolp (setq var (car pat)))
	    (progn
	      (push var *destructure-vars*)
	      (cond ((null (cdr pat))
		     (push (make-pop var form ()) setqs))
		    ((symbolp (cdr pat))
		     (push (make-pop var form (cdr pat)) setqs)
		     (push (cdr pat) *destructure-vars*)
		     (return ()))
		    ((memq var '(nil ignore)) (incf pending-pops))
		    ((memq (cadr pat) '(nil ignore))
		     (push (make-pop var form ()) setqs)
		     (incf pending-pops 1))
		    (t
		     (push (make-pop var form form) setqs))))
	    (progn
	      (push `(let ((,gensym ()))
		       ,(make-pop gensym form (if (symbolp (cdr pat)) (cdr pat) form))
		       ,@(nreverse
                          #-(or sbcl lispworks) 
                          (destructure-internal (if (consp pat) (car pat) pat)
						 gensym)
                          #+(or sbcl lispworks)
                          (destructure-internal (car pat) gensym)))
		    setqs)
	      (when (symbolp (cdr pat))
		(push (cdr pat) *destructure-vars*)
		(return)))))
      setqs)))

;;; Iterate is a simple iteration macro.  If CommonLisp had a standard Loop
;;; macro I wouldn't need this wretched crock.  But what the hell, it seems
;;; to do most of what I need.  It looks like:
;;;   (iterate (<control-clause-1> <control-clause-2> ...)
;;;      . <body>)
;;;
;;;  a control clause can be one of:
;;;   (<var> in <list-form>)  | (<var> in <list-form> by <function>)
;;;   (<var> on <list-form>)  | (<var> on <list-form> by <function>)
;;;   (<var> from <initial> to <final>)
;;;   (<var> from <initial> below <final>)
;;;   (<var> from <initial> to <final> by <function> | <increment>)
;;;   (<var> from <initial> below <final> by <function> | <increment>)
;;;   (<var> = <form>)   <form> is evaluated each time through
;;;   (<var> = <initial> <subsequent>)
;;;   
;;;  inside <body> you are allowed to use:
;;;    collect
;;;    join
;;;    sum

(defvar *iterate-result-types* ())

(defmacro define-iterate-result-type (name arglist &body body)
  (let ((fn-name
	  (if (and (null (cdr body)) (symbolp (car body)))
	      (car body)
	      (make-symbol (string-append (symbol-name name) " iterate result type")))))
    `(progn
       (let ((existing (assq ',name  *iterate-result-types*)))
	 (if existing
	     (rplacd existing ',fn-name)
	     (push ',(cons name fn-name) *iterate-result-types*)))
       ,(and (not (and (null (cdr body)) (symbolp (car body))))
	     `(defun ,fn-name ,arglist . ,body)))))

(defmacro iterate (controls &body body &environment env)
  #+Xerox (setq body (copy-tree body))
  (let (binds var-init-steps
	pre-end-tests post-end-tests
	pre-bodies post-bodies
	(result-type ()))
    (mapc #'(lambda (control)
	      (let ((var (car control))
		    (type (cadr control))
		    (initial (caddr control))
		    (args (cdddr control)))
		(ecase type
		  ((in on)
		   (let* ((gensym (if (or (eq type 'in) (consp var)) (gensym) var))
			  (step `(,(if args (cadr args) 'cdr) ,gensym)))
		     (push `(,gensym ,initial ,step) var-init-steps)
		     (push `(null ,gensym) pre-end-tests)
		     (cond ((listp var)
			    (multiple-value-bind (setqs dbinds)
				(destructure var (if (eq type 'in) `(car ,gensym) gensym))
			      (setq binds (nconc dbinds binds))
			      (setq pre-bodies (nconc pre-bodies (nreverse setqs)))))
			   ((eq type 'in)
			    (push var binds)
			    (push `(setq ,var (car ,gensym)) pre-bodies)))))
		  (from
		    (let ((gensym (gensym))
			  (final
			    (and (memq (car args) '(to below))
				 (if (eq (car args) 'to)
				     (cadr args)
				     `(- ,(cadr args) 1))))
			  (step
			    (progn (setq args (member 'by args))
				   (cond ((null args)
					  `(1+ ,var))
					 ((numberp (cadr args))
					  `(+ ,var ,(cadr args)))
					 (t (cadr args))))))
		      (push `(,var ,initial ,step) var-init-steps)
		      (and final (push `(,gensym ,final) binds))
		      (and final (push `(> , var ,gensym) pre-end-tests))))
		  (=
		    (push `(,var ,initial ,(or (car args) initial))
			  var-init-steps))
		  )))
	  controls)
    (setq body
	  (walk-form (cons 'progn body)
                     :environment env
		     :walk-function 
		     #'(lambda (form context env cl:&aux aux)
			 (declare (ignore context))
                         
			 (or (and (listp form)
				  (setq aux (assq (car form) *iterate-result-types*))
				  (setq result-type
					(if (null result-type)
					    (funcall (cdr aux)
						     form nil 'create-result-type)
					    (funcall (cdr aux)
						     form result-type 'check-result-type)))
				  (funcall (cdr aux) form result-type 'macroexpand))
			     form)))
          )
    (let* ((initially (cons 'progn
			    (dolist (tlf body)
			      (when (and (consp tlf) (eq (car tlf) 'initially))
				(return (prog1 (cdr tlf)
					       (setf (car tlf) 'progn
						     (cdr tlf) ())))))))
	   (finally (cons 'progn
			  (dolist (tlf body)
			    (when (and (consp tlf) (eq (car tlf) 'finally))
			      (return (prog1 (cdr tlf)
					     (setf (car tlf) 'progn
						   (cdr tlf) ()))))))))
      `(let (,@binds . ,(caddr result-type))
	 (iterate-macrolets
	   (prog ,(mapcar #'(lambda (x) (list (car x) (cadr x)))
			  var-init-steps)
		 ,initially
	      restart
		 (and (or . ,(reverse pre-end-tests))
		      (go .iterate_return.))
		 (progn . ,(reverse pre-bodies))
		 ,body
		 (progn . ,(reverse post-bodies))
		 (or ,@post-end-tests
		     (progn ,@(mapcar #'(lambda (x)
					  (and (cddr x)
					       `(setq ,(car x)
						      ,(caddr x))))
				      var-init-steps)
			    (go restart)))
	      .iterate_return.
		 ,finally
		 (return ,(cadddr result-type))))))))

(define-iterate-result-type collect (form result-type op)
  iterate-collect-join)

(define-iterate-result-type join (form result-type op)
  iterate-collect-join)

(defun iterate-collect-join (form result-type op)
  (ecase op
    (create-result-type
      (let ((gensym (gensym)))
	`(,(car form) ,gensym ((,gensym ())) (nreverse ,gensym))))
    (check-result-type
      (if (memq (car result-type) '(collect join))
	  result-type
	  (error "Using ~S inside an iterate in which you already used ~S."
		 (car form) (car result-type))))
    (macroexpand
      (if (eq (car form) 'collect)
	  `(push ,(cadr form) ,(cadr result-type))
	  `(setq ,(cadr result-type)
		 (append (reverse ,(cadr form)) ,(cadr result-type)))))))

(define-iterate-result-type sum (form result-type op)
  (ecase op
    (create-result-type
      (let ((gensym (gensym)))
	`(,(car form) ,gensym ((,gensym 0)) ,gensym)))
    (check-result-type
      (eq (car result-type) 'sum))
    (macroexpand
      `(incf ,(cadr result-type) ,(cadr form)))))

(defmacro iterate-macrolets (&body body)
  `(macrolet
     ((until (test)
        `(when ,test (go .iterate_return.)))
      (while (test)
	`(until (not ,test)))
      (initially (&body body)
        (declare (ignore body))
	(error
	  "It is an error for FINALLY to appear other than at top-level~%~
	   inside an iterate."))
      (finally (&body ignore)
        (declare (ignore ignore))
	(error
	  "It is an error for INITIALLY to appear other than at top-level~%~
           inside an iterate."))
      )
     . ,body))
  
;;;
;;; Two macros useful for parsing defstructs.
;;; The first parses slot-description (or lambda-list) style keyword-value
;;; pairs.  The second, more complicated one, parses defstruct option style
;;; keyword-value pairs.
;;;
(defmacro keyword-bind (keywords form &body body)
  `(apply (function (lambda (&key . ,keywords) . ,body)) ,form))

;;;
;;;   (keyword-parse (<keyword-spec-1> <keyword-spec-2> ..)
;;;                  form
;;;      . body)
;;;
;;; Where form is a form which will be evaluated and should return the list
;;; of keywords and values which keyword-parse will parse.  Body will be
;;; evaluated with the variables specified by the keyword-specs bound.
;;; Keyword specs look like:
;;;        <var>
;;;        (<var> <default>)
;;;        (<var> <default> <suppliedp var>)
;;;        (<var> <default> <suppliedp var> <option-1> <val-1> ...)
;;;
;;;    The options can be:
;;;       :allowed     ---  :required   :multiple
;;;       :return-cdr  ---  t           nil
;;;       
(defmacro keyword-parse (keywords form &body body)
  ;; This makes an effort to resemble keyword-bind in that the vars are bound
  ;; one at a time so that a default value form can look at the value of a
  ;; previous argument. This is probably more hair than its worth, but what
  ;; the hell, programming is fun.
  (let* ((lambda-list ())
         (supplied-p-gensyms ())
         (value-forms ())
         (entry-var (gensym)))
    (dolist (kw keywords)
      (unless (listp kw) (setq kw (list kw)))      
      (destructuring-bind (var default &optional supplied-p-var &rest options) kw
        (keyword-bind (presence (allowed ':required) return-cdr) options
          (push var lambda-list)
          (when supplied-p-var
            (push supplied-p-var lambda-list)
            (push (gensym) supplied-p-gensyms))
          (push `(let ((,entry-var (keyword-parse-assq ',(make-keyword var)
						       ,form
						       ',allowed)))
                   (if (null ,entry-var)
                       ,default
                       ;; Insert appropriate error-checking based on the
                       ;; allowed argument.
                       (progn
                       ,(when (null allowed)
                          `(unless (nlistp (car ,entry-var))
                             (error "The ~S keyword was supplied with an ~
                                    argument, it is not allowed to have one."
                                    ',(make-keyword var))))
                       ,(when (eq allowed ':required)
                          `(unless (listp (car ,entry-var))
                             (error
			       "The ~S keyword was supplied without an ~
                                argument~%when present, this keyword must ~
                                have an argument."
                               ',(make-keyword var))))
                       (cond ((listp (car ,entry-var))
                              ,(and supplied-p-var
                                    `(setq ,(car supplied-p-gensyms) 't))
                              ,(if return-cdr
				   (if (eq allowed ':multiple)
				       `(mapcar #'cdr ,entry-var)
				       `(cdar ,entry-var))
				   (if (eq allowed ':multiple)
				       `(mapcar #'cadr ,entry-var)
				       `(cadar ,entry-var))))
                             (t
                              ,(and supplied-p-var
                                    `(setq ,(car supplied-p-gensyms)
					   ':presence))
                              ,presence)))))
                value-forms)
          (when supplied-p-var
            (push (car supplied-p-gensyms) value-forms)))))
    `(let ,supplied-p-gensyms
       ((lambda ,(reverse lambda-list) . ,body) . ,(reverse value-forms)))))


(defun keyword-parse-assq (symbol list allowed)
  (do ((result nil result)
       (tail list (cdr tail)))
      ((null tail) (nreverse result))
    (if (eq (if (symbolp (car tail)) (car tail) (caar tail)) symbol)
	(if (neq allowed ':multiple)
	    (return tail)
	    (push (car tail) result)))))

  ;;   
;;;;;; printing-random-thing
  ;;
;;; Similar to printing-random-object in the lisp machine but much simpler
;;; and machine independent.
(defmacro printing-random-thing ((thing stream) &body body)
  (once-only (stream)
  `(let ((*print-level* (and (numberp *print-level*) (- *print-level* 1))))
     (progn (princ "#<" ,stream)
            ,@body
	    (princ " " ,stream)
	    (printing-random-thing-internal ,thing ,stream)
	    (princ ">" ,stream)))))

(defun printing-random-thing-internal (thing stream)
  (declare (ignore thing stream))
  nil)

  ;;   
;;;;;; 
  ;;

(defun capitalize-words (string)
  (let ((string (copy-seq (string string))))
    (declare (string string))
    (do* ((flag t flag)
	  (length (length string) length)
	  (char nil char)
	  (i 0 (+ i 1)))
	 ((= i length) string)
      (setq char (elt string i))
      (cond ((both-case-p char)
	     (if flag
		 (and (setq flag (lower-case-p char))
		      (setf (elt string i) (char-upcase char)))
		 (and (not flag) (setf (elt string i) (char-downcase char))))
	     (setq flag nil))
	    ((char-equal char #\-)
	     (setq flag t))
	    (t (setq flag nil))))))

  ;;
;;;;;; CLASS-NAMED  naming classes.
  ;;
;;;
;;; (CLASS-NAMED <name>) returns the class named <name>.  setf can be used
;;; with class-named to set the class named <name>.  These are "extrinsic"
;;; names.  Neither class-named nor setf of class-named do anything with the
;;; name slot of the class, they only lookup and change the association from
;;; name to class.
;;; 

(defvar *class-name-hash-table* (make-hash-table :test #'eq))

(defun class-named (name &optional no-error-p)
  (or (gethash name *class-name-hash-table*)
      (if no-error-p () (error "No class named: ~S." name))))

(defsetf class-named (name &optional ignore-damnit) (class)
  (declare (cl:ignore ignore-damnit))
  `(setf (gethash ,name *class-name-hash-table*) ,class))


(defvar *discriminator-name-hash-table* (make-hash-table :test #'eq
							 :size 1000))

(defun discriminator-named (name)		        ;This a function for
  (gethash name *discriminator-name-hash-table*))	;the benefit of
						        ;compile-time-define?

(defun set-discriminator-named (name new-value)
  (setf (gethash name *discriminator-name-hash-table*) new-value))

(defsetf discriminator-named set-discriminator-named)

;;;
;;; To define a macro which is only applicable in the body of a defmethod,
;;; use define-method-body-macro.  This macro takes two arguments the name
;;; of the macro that should be defined in the body of the method and the
;;; function which should be called to expand calls to that macro.
;;; 
;;; Expander-function will be called with 3 arguments:
;;; 
;;;   the entire macro form (gotten with &whole)
;;;   the macroexpand-time-information
;;;   the environment
;;;   

(defvar *method-body-macros* ())

(defmacro define-method-body-macro (name arglist &key global method)
  (when (eq global :error)
    (setq global
	  `(progn (warn "~S used outside the body of a method." ',name)
		  '(error "~S used outside the body of a method." ',name))))
  (or method
      (error "Have to provide a value for the method-body definition of~%~
              a macro defined with define-method-body-macro."))
  #+KCL (when (memq '&environment arglist)
	  ;; In KCL, move &environment to the beginning of the
	  ;; arglist since they require that it be there.
	  (unless (eq (car arglist) '&environment)
	    (do ((loc arglist (cdr loc)))
		((eq (cadr loc) '&environment)
		 (setq arglist (list* (cadr loc) (caddr loc) arglist))
		 (setf (cdr loc) (cdddr loc))))))
  (let ((body-expander-function (gensym))
	(parameters (remove lambda-list-keywords arglist
			    :test #'(lambda (x y) (member y x)))))
    `(cltl1-eval-when (compile load eval)
       ,(and global `(defmacro ,name ,arglist ,global))
       (defun ,body-expander-function
	      (macroexpand-time-environment ,@parameters)
	 ,method)
	 
       (let ((entry (or (assq ',name *method-body-macros*)
			(progn (push (list ',name) *method-body-macros*)
			       (car *method-body-macros*)))))
	 (setf (cdr entry) (list ',arglist
				 ',parameters
				 ',body-expander-function))))))

  ;;   
;;;;;; Special variable definitions.
  ;;
;;; Gets set to its right value once early-defmeths are fixed.
;;; 
(defvar *error-when-defining-method-on-existing-function* 'bootstrapping
  "If this variable is non-null (the default) defmethod signals an error when
   a method is defined on an existing lisp-function without first calling
   make-specializable on that function.")

