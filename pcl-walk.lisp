;;;-*- Mode:LISP; Package:(WALKER LISP 1000); Base:10; Syntax:Common-lisp -*-
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
;;; A simple code walker, based IN PART on: (roll the credits)
;;;   Larry Masinter's Masterscope
;;;   Moon's Common Lisp code walker
;;;   Gary Drescher's code walker
;;;   Larry Masinter's simple code walker
;;;   .
;;;   .
;;;   boy, thats fair (I hope).
;;;
;;; For now at least, this code walker really only does what PCL needs it to
;;; do.  Maybe it will grow up someday.
;;;

(in-package :cool.pcl.walker)


;;; *walk-function* is the function being called on each sub-form as we walk.
;;; Normally it is supplied using the :walk-function keyword argument to
;;; walk-form, but it is OK to bind it around a call to walk-form-internal.
(defvar *walk-function*)

;;; *walk-form* is used by the IF template.  When the first argument to the
;;; if template is a list it will be evaluated with *walk-form* bound to the 
;;; form currently being walked.
(defvar *walk-form*)

;;; *declarations* is a list of the declarations currently in effect.
(defvar *declarations*)
	
;;; *lexical-variables* is a list of the variables bound in the current
;;; contour. In *lexical-variables* the cons whose car is the variable is
;;; meaningful in the sense that the cons whose car is the variable can be
;;; used to keep track of which contour the variable is bound in.
;;;
;;; Now isn't that just the cats pajamas.
;;;
(defvar *lexical-variables*)

;;; An environment of the kind that macroexpand-1 gets as its second
;;; argument.  In fact, that is exactly where it comes from.  This is kind of
;;; kludgy since Common Lisp is somewhat screwed up in this respect.
;;; Hopefully Common Lisp will fix this soon.  For more info see:
;;; MAKE-LEXICAL-ENVIRONMENT
(defvar *environment*)

;;;
;;; With new contour is used to enter a new lexical binding contour which
;;; inherits from the exisiting one.  I admit that using with-new-contour is
;;; often overkill.  It would suffice for the the walker to rebind
;;; *lexical-variables* and *declarations* when walking LET and rebind
;;; *environment* and *declarations* when walking MACROLET etc.
;;; WITH-NEW-CONTOUR is much more convenient and just as correct.
;;; 
(defmacro with-new-contour (&body body)
  `(let ((*declarations* ())			;If Common Lisp got an
						;unspecial declaration
						;this would need to be
						;re-worked.
         (*lexical-variables* *lexical-variables*)
         (*environment* *environment*))
     . ,body))

(defmacro note-lexical-binding (thing)
  `(push ,thing *lexical-variables*))

(defmacro note-declaration (declaration)
  `(push ,declaration *declarations*))


(defun variable-lexically-boundp (var)
  (if (not (boundp '*walk-function*))
      :unsure
      (values (member var *lexical-variables* :test (function eq))
	      (variable-special-p var) 't)))

(defun variable-lexical-p (var)
  (if (not (boundp '*walk-function*))
      :unsure
      (and (not (eq (variable-special-p var) 't))
	   (member var *lexical-variables* :test (function eq)))))

(defun variable-special-p (var)
  (if (not (boundp '*walk-function*))
      (or (variable-globally-special-p var) :unsure)
      (or (dolist (decl *declarations*)
	    (and (eq (car decl) 'special)
		 (member var (cdr decl) :test #'eq)
		 (return t)))
	  (variable-globally-special-p var))))

;;;
;;; VARIABLE-GLOBALLY-SPECIAL-P is used to ask if a variable has been
;;; declared globally special.  Any particular CommonLisp implementation
;;; should customize this function accordingly and send their customization
;;; back.
;;;
;;; The default version of variable-globally-special-p is probably pretty
;;; slow, so it uses *globally-special-variables* as a cache to remember
;;; variables that it has already figured out are globally special.
;;;
;;; This would need to be reworked if an unspecial declaration got added to
;;; Common Lisp.
;;;
;;; Common Lisp nit:
;;;   variable-globally-special-p should be defined in Common Lisp.
;;;
#-(or Symbolics Xerox TI VaxLisp KCL LMI excl)
(defvar *globally-special-variables* ())

(defun variable-globally-special-p (symbol)
  #+Symbolics                   (si:special-variable-p symbol)
  #+(or Lucid TI LMI)           (get symbol 'special)
  #+Xerox                       (il:variable-globally-special-p symbol)
  #+VaxLisp                     (get symbol 'system::globally-special)
  #+KCL			        (si:specialp symbol)
  #+excl                        (get symbol 'excl::.globally-special.)
  #+HP                          (member (get symbol 'impl:vartype)
					'(impl:fluid impl:global)
					:test #'eq)
  #+sbcl                        (sb-walker:var-globally-special-p symbol)
  #-(or Symbolics Lucid TI LMI Xerox VaxLisp KCL excl HP sbcl)
  (or (not (null (member symbol *globally-special-variables* :test #'eq)))
      (when (eval `(flet ((ref () ,symbol))
                     #+sbcl
                     (handler-case 
                         (let ((,symbol ',(load-time-value (list nil))))
                           (and (boundp ',symbol) (eq ,symbol (ref))))
                       (type-error () T)
                       (error () nil))
                     #-sbcl (let ((,symbol ',(load-time-value (list nil))))
                              (and (boundp ',symbol) (eq ,symbol (ref))))))
	(push symbol *globally-special-variables*)
	t)))



  ;;   
;;;;;; Handling of special forms (the infamous 24).
  ;;
;;;
;;; and I quote...
;;; 
;;;     The set of special forms is purposely kept very small because
;;;     any program analyzing program (read code walker) must have
;;;     special knowledge about every type of special form. Such a
;;;     program needs no special knowledge about macros...
;;;
;;; So all we have to do here is a define a way to store and retrieve
;;; templates which describe how to walk the 24 special forms and we are all
;;; set...
;;;
;;; Well, its a nice concept, and I have to admit to being naive enough that
;;; I believed it for a while, but not everyone takes having only 24 special
;;; forms as seriously as might be nice.  There are (at least) 3 ways to
;;; lose:
;;
;;;   1 - Implementation x implements a Common Lisp special form as a macro
;;;       which expands into a special form which:
;;;         - Is a common lisp special form (not likely)
;;;         - Is not a common lisp special form (on the 3600 IF --> COND).
;;;
;;;     * We can safe ourselves from this case (second subcase really) by
;;;       checking to see if there is a template defined for something
;;;       before we check to see if we we can macroexpand it.
;;;
;;;   2 - Implementation x implements a Common Lisp macro as a special form.
;;;
;;;     * This is a screw, but not so bad, we save ourselves from it by
;;;       defining extra templates for the macros which are *likely* to
;;;       be implemented as special forms.  (DO, DO* ...)
;;;
;;;   3 - Implementation x has a special form which is not on the list of
;;;       Common Lisp special forms.
;;;
;;;     * This is a bad sort of a screw and happens more than I would like
;;;       to think, especially in the implementations which provide more
;;;       than just Common Lisp (3600, Xerox etc.).
;;;       The fix is not terribly staisfactory, but will have to do for
;;;       now.  There is a hook in get walker-template which can get a
;;;       template from the implementation's own walker.  That template
;;;       has to be converted, and so it may be that the right way to do
;;;       this would actually be for that implementation to provide an
;;;       interface to its walker which looks like the interface to this
;;;       walker.
;;;
(defmacro get-walker-template-internal (x)
  `(get ,x 'walker-template))

(defun get-walker-template (x)
  (cond ((symbolp x)
	 (or (get-walker-template-internal x)
	     (get-implementation-dependent-walker-template x)))
	((and (listp x) (eq (car x) 'lambda))
	 '(lambda repeat (eval)))
	((and (listp x) (eq (car x) 'lambda))
	 '(call repeat (eval)))))

(defun get-implementation-dependent-walker-template (x)
  (declare (ignore x))
  ())

(cltl1-eval-when (compile load eval)
(defmacro define-walker-template (name template)
  `(cltl1-eval-when (load eval)
     (setf (get-walker-template-internal ',name) ',template)))
)


  ;;   
;;;;;; The actual templates
  ;;   

(define-walker-template BLOCK                (NIL NIL REPEAT (EVAL)))
(define-walker-template CATCH                (NIL EVAL REPEAT (EVAL)))
(define-walker-template COMPILER-LET         walk-compiler-let)
(define-walker-template DECLARE              walk-unexpected-declare)
(define-walker-template EVAL-WHEN            (NIL QUOTE REPEAT (EVAL)))
(define-walker-template FLET                 walk-flet/labels)
(define-walker-template FUNCTION             (NIL CALL))
(define-walker-template GO                   (NIL QUOTE))
(define-walker-template IF                   (NIL TEST RETURN RETURN))
(define-walker-template LABELS               walk-flet/labels)
(define-walker-template LAMBDA               walk-lambda)
(define-walker-template LET                  walk-let)
(define-walker-template LET*                 walk-let*)
(define-walker-template MACROLET             walk-macrolet)
(define-walker-template MULTIPLE-VALUE-CALL  (NIL EVAL REPEAT (EVAL)))
(define-walker-template MULTIPLE-VALUE-PROG1 (NIL RETURN REPEAT (EVAL)))
(define-walker-template MULTIPLE-VALUE-SETQ  (NIL (REPEAT (SET)) EVAL))
(define-walker-template PROGN                (NIL REPEAT (EVAL)))
(define-walker-template PROGV                (NIL EVAL EVAL REPEAT (EVAL)))
(define-walker-template QUOTE                (NIL QUOTE))
(define-walker-template RETURN-FROM          (NIL QUOTE REPEAT (RETURN)))
(define-walker-template SETQ                 (NIL REPEAT (SET EVAL)))
(define-walker-template TAGBODY              walk-tagbody)
(define-walker-template THE                  (NIL QUOTE EVAL))
(define-walker-template THROW                (NIL EVAL EVAL))
(define-walker-template UNWIND-PROTECT       (NIL RETURN REPEAT (EVAL)))

;;; The new special form.
;(define-walker-template pcl::LOAD-TIME-EVAL       (NIL EVAL))

;;;
;;; And the extra templates...
;;;
(define-walker-template DO      walk-do)
(define-walker-template DO*     walk-do*)
(define-walker-template PROG    walk-let)
(define-walker-template PROG*   walk-let*)
(define-walker-template COND    (NIL REPEAT ((TEST REPEAT (EVAL)))))


  ;;   
;;;;;; WALK-FORM
  ;;   
;;;
;;; The main entry-point is walk-form, calls back in should use walk-form-internal.
;;; 

(defun walk-form (form &key ((:declarations *declarations*) ())
			    ((:lexical-variables *lexical-variables*) ())
			    ((:environment *environment*) ())
			    ((:walk-function *walk-function*) #'(lambda (x y)
								  y x)))
  (walk-form-internal form 'eval))

;;;
;;; WALK-FORM-INTERNAL is the main driving function for the code walker. It
;;; takes a form and the current context and walks the form calling itself or
;;; the appropriate template recursively.
;;;
;;;   "It is recommended that a program-analyzing-program process a form
;;;    that is a list whose car is a symbol as follows:
;;;
;;;     1. If the program has particular knowledge about the symbol,
;;;        process the form using special-purpose code.  All of the
;;;        standard special forms should fall into this category.
;;;     2. Otherwise, if macro-function is true of the symbol apply
;;;        either macroexpand or macroexpand-1 and start over.
;;;     3. Otherwise, assume it is a function call. "
;;;     

(defun walk-form-internal (form context
			   &aux newform newnewform
				walk-no-more-p macrop
				fn template)
  ;; First apply the *walk-function* to perform whatever translation
  ;; the user wants to to this form.  If the second value returned
  ;; by *walk-function* is T then we don't recurse...
  (multiple-value-setq (newform walk-no-more-p)
    (funcall *walk-function* form context))
  (cond (walk-no-more-p newform)
	((not (eq form newform)) (walk-form-internal newform context))
	((not (consp newform)) newform)
	((setq template (get-walker-template (setq fn (car newform))))
         (if (symbolp template)
             (funcall template newform context)
             (walk-template newform template context)))
	((progn (multiple-value-setq (newnewform macrop)
		  (macroexpand-1 newform *environment*))
		macrop)
	 (walk-form-internal newnewform context))
	((and (symbolp fn)
	      (not (fboundp fn))
	      (special-operator-p fn))
	 (error
	   "~S is a special form, not defined in the CommonLisp manual.~%~
            This code walker doesn't know how to walk it.  Please define a~%~
            template for this special form and try again."
	   fn))
	(t
         ;; Otherwise, walk the form as if its just a standard function
         ;; call using a template for standard function call.
         (walk-template newform '(call repeat (eval)) context))))

(defun walk-template (form template context)
  (if (atom template)
      (ecase template
        ((QUOTE NIL) form)
        ((EVAL FUNCTION TEST EFFECT RETURN)
         (walk-form-internal form :EVAL))
        (SET
          (walk-form-internal form :SET))
        ((LAMBDA CALL)
	 (if (symbolp form)
	     form
	     (walk-lambda form context))))
      (case (car template)
        (IF
          (let ((*walk-form* form))
            (walk-template form
			   (if (if (listp (cadr template))
				   (eval (cadr template))
				   (funcall (cadr template) form))
			       (caddr template)
			       (cadddr template))
			   context)))
        (REPEAT
          (walk-template-handle-repeat form
                                       (cdr template)
				       ;; For the case where nothing happens
				       ;; after the repeat optimize out the
				       ;; call to length.
				       (if (null (cddr template))
					   ()
					   (nthcdr (- (length form)
						      (length
							(cddr template)))
						   form))
                                       context))
        (REMOTE
          (walk-template form (cadr template) context))
        (otherwise
          (cond ((atom form) form)
                (t (recons form
                           (walk-template
			     (car form) (car template) context)
                           (walk-template
			     (cdr form) (cdr template) context))))))))

(defun walk-template-handle-repeat (form template stop-form context)
  (if (eq form stop-form)
      (walk-template form (cdr template) context)
      (walk-template-handle-repeat-1 form
				     template
				     (car template)
				     stop-form
				     context)))

(defun walk-template-handle-repeat-1 (form template repeat-template
					   stop-form context)
  (cond ((null form) ())
        ((eq form stop-form)
         (if (null repeat-template)
             (walk-template stop-form (cdr template) context)       
             (error "While handling repeat:
                     ~%Ran into stop while still in repeat template.")))
        ((null repeat-template)
         (walk-template-handle-repeat-1
	   form template (car template) stop-form context))
        (t
         (recons form
                 (walk-template (car form) (car repeat-template) context)
                 (walk-template-handle-repeat-1 (cdr form)
						template
						(cdr repeat-template)
						stop-form
						context)))))

(defun recons (x car cdr)
  (if (or (not (eq (car x) car))
          (not (eq (cdr x) cdr)))
      (cons car cdr)
      x))

(defun relist* (x &rest args)
  (relist*-internal x args))

(defun relist*-internal (x args)
  (if (null (cdr args))
      (car args)
      (recons x (car args) (relist*-internal (cdr x) (cdr args)))))


  ;;   
;;;;;; Special walkers
  ;;

(defun walk-declarations (body fn
			       &optional doc-string-p declarations old-body
			       &aux (form (car body)))
  (cond ((and (stringp form)			;might be a doc string
              (cdr body)			;isn't the returned value
              (null doc-string-p)		;no doc string yet
              (null declarations))		;no declarations yet
         (recons body
                 form
                 (walk-declarations (cdr body) fn t)))
        ((and (listp form) (eq (car form) 'declare))
         ;; Got ourselves a real live declaration.  Record it, look for more.
         (dolist (declaration (cdr form))
           (note-declaration declaration)
           (push declaration declarations))
         (recons body
                 form
                 (walk-declarations
		   (cdr body) fn doc-string-p declarations)))
        ((and form
	      (listp form)
	      (null (get-walker-template (car form)))
	      (not (eq form (setq form (macroexpand-1 form *environment*)))))
         ;; When we macroexpanded this form we got something else back.
         ;; Maybe this is a macro which expanded into a declare?
	 ;; Recurse to find out.
         (walk-declarations
	   (cons form (cdr body)) fn doc-string-p declarations (or old-body
								   body)))
        (t
         ;; Now that we have walked and recorded the declarations, call the
	 ;; function our caller provided to expand the body.  We call that
	 ;; function rather than passing the real-body back, because we are
	 ;; RECONSING up the new body.
         (funcall fn (or old-body body)))))

(defun fix-lucid-1.2 (x) x)

(defun walk-unexpected-declare (form context)
  (declare (ignore context))
  (warn "Encountered declare ~S in a place where a declare was not expected."
	form)
  form)

(defun walk-arglist (arglist context &optional (destructuringp nil) &aux arg)
  (cond ((null arglist) ())
        ((symbolp (setq arg (car arglist)))
         (or (member arg lambda-list-keywords :test #'eq)
             (note-lexical-binding arg))
         (recons arglist
                 arg
                 (walk-arglist (cdr arglist)
                               context
                               (and destructuringp
				    (not (member arg lambda-list-keywords
						 :test #'eq))))))
        ((consp arg)
         (prog1 (if destructuringp
                    (walk-arglist arg context destructuringp)
                    (recons arglist
                            (relist* arg
                                     (car arg)
                                     (walk-form-internal (cadr arg) 'eval)
                                     (cddr arg))
                            (walk-arglist (cdr arglist) context nil)))
                (if (symbolp (car arg))
                    (note-lexical-binding (car arg))
                    (note-lexical-binding (cadar arg)))
                (or (null (cddr arg))
                    (not (symbolp (caddr arg)))
                    (note-lexical-binding arg))))
          (t
	   (error "Can't understand something in the arglist ~S" arglist))))

(defun walk-let (form context)
  (walk-let/let* form context nil))

(defun walk-let* (form context)
  (walk-let/let* form context t))

(defun walk-do (form context)
  (walk-do/do* form context nil))

(defun walk-do* (form context)
  (walk-do/do* form context t))

(defun walk-let/let* (form context sequentialp)
  (let ((old-declarations *declarations*)
	(old-lexical-variables *lexical-variables*))
    (with-new-contour
      (let* ((let/let* (car form))
             (bindings (cadr form))
             (body (cddr form))
             walked-bindings
             (walked-body
               (walk-declarations 
                 body
                 #'(lambda (real-body)
                     (setq walked-bindings
                           (walk-bindings-1 bindings
					    old-declarations
					    old-lexical-variables
					    context
					    sequentialp))
                     (walk-template real-body '(repeat (eval)) context)))))
        (relist*
	  form let/let* (fix-lucid-1.2 walked-bindings) walked-body)))))

(defun walk-do/do* (form context sequentialp)
  (let ((old-declarations *declarations*)
	(old-lexical-variables *lexical-variables*))
    (with-new-contour
      (let* ((do/do* (car form))
             (bindings (cadr form))
             (end-test (caddr form))
             (body (cdddr form))
             walked-bindings
             (walked-body
               (walk-declarations
                 body
                 #'(lambda (real-body)
                     (setq walked-bindings
                           (walk-bindings-1 bindings
					    old-declarations
					    old-lexical-variables
					    context
					    sequentialp))
                     (walk-template real-body '(repeat (eval)) context)))))
        (relist* form
                 do/do*
                 (walk-bindings-2 bindings walked-bindings context)
                 (walk-template end-test '(test repeat (eval)) context)
                 walked-body)))))
                            
(defun walk-bindings-1 (bindings old-declarations old-lexical-variables
				 context sequentialp)
  (and bindings
       (let ((binding (car bindings)))
         (recons bindings
                 (if (symbolp binding)
                     (prog1 binding
                            (note-lexical-binding binding))
                     (prog1 (let ((*declarations* old-declarations)
				  (*lexical-variables*
				    (if sequentialp
					*lexical-variables*
					old-lexical-variables)))
                              (relist* binding
                                       (car binding)
                                       (walk-form-internal (cadr binding)
							   context)
                                       (cddr binding)))	;save cddr for DO/DO*
						        ;it is the next value
						        ;form. Don't walk it
						        ;now though.
                            (note-lexical-binding (car binding))))
                 (walk-bindings-1 (cdr bindings)
				  old-declarations old-lexical-variables
				  context sequentialp)))))

(defun walk-bindings-2 (bindings walked-bindings context)
  (and bindings
       (let ((binding (car bindings))
             (walked-binding (car walked-bindings)))
         (recons bindings
		 (if (symbolp binding)
		     binding
		     (relist* binding
			      (car walked-binding)
			      (cadr walked-binding)
			      (walk-template (cddr binding) '(eval) context)))		 
                 (walk-bindings-2 (cdr bindings)
				  (cdr walked-bindings)
				  context)))))

(defun walk-lambda (form context)
  (with-new-contour    
    (let* ((arglist (cadr form))
           (body (cddr form))
           (walked-arglist nil)
           (walked-body
             (walk-declarations body
	       #'(lambda (real-body)
		   (setq walked-arglist (walk-arglist arglist context))
		   (walk-template real-body '(repeat (eval)) context)))))
      (relist* form
               (car form)
               (fix-lucid-1.2 walked-arglist)
               walked-body))))

(defun walk-tagbody (form context)
  (recons form (car form) (walk-tagbody-1 (cdr form) context)))

(defun walk-tagbody-1 (form context)
  (and form
       (recons form
               (walk-form-internal (car form)
				   (if (symbolp (car form)) 'quote context))
               (walk-tagbody-1 (cdr form) context))))

(defun walk-compiler-let (form context)
  (with-new-contour
    (let ((vars ())
	  (vals ()))
      (dolist (binding (cadr form))
	(cond ((symbolp binding) (push binding vars) (push nil vals))
	      (t
	       (push (car binding) vars)
	       (push (eval (cadr binding)) vals))))
      (relist* form
               (car form)
               (cadr form)
               (progv vars vals
                 (note-declaration (cons 'special vars))
                 (walk-template (cddr form) '(repeat (eval)) context))))))

(defun walk-macrolet (form context)
  (labels ((walk-definitions (definitions)
             (and (not (null definitions))
                  (let ((definition (car definitions)))
                    (recons definitions
                            (with-new-contour
                              (relist* definition
                                       (car definition)
                                       (walk-arglist (cadr definition)
						     context t)
                                       (walk-declarations (cddr definition)
					 #'(lambda (real-body)
					     (walk-template
					       real-body
					       '(repeat (eval))
					       context)))))
                            (walk-definitions (cdr definitions)))))))
    (with-new-contour
      (relist* form
               (car form)
               (walk-definitions (cadr form))
               (progn (setq *environment*
			    (make-lexical-environment form *environment*))
                      (walk-declarations (cddr form)
			#'(lambda (real-body)
			    (walk-template real-body
						    '(repeat (eval))
						    context))))))))

(defun walk-flet/labels (form context)
  (with-new-contour
    (labels ((walk-definitions (definitions)
               (if (null definitions)
                   ()
                   (recons definitions
                           (walk-lambda (car definitions) context)
                           (walk-definitions (cdr definitions)))))
             (update-environment ()
               (setq *environment*
		     (make-lexical-environment form *environment*))))
      (relist* form
               (car form)
               (ecase (car form)
                 (flet
                   (prog1 (walk-definitions (cadr form))
                          (update-environment)))
                 (labels
                   (update-environment)
                   (walk-definitions (cadr form))))
               (walk-declarations (cddr form)
		 #'(lambda (real-body)
		     (walk-template real-body '(repeat (eval)) context)))))))

;;; make-lexical-environemnt is kind of gross.  It would be less gross if
;;; EVAL took an environment argument.
;;;
;;; Common Lisp nit:
;;;    if Common Lisp should provide mechanisms for playing with
;;;    environments explicitly.  making them, finding out what
;;;    functions are bound in them etc.  Maybe compile should
;;;    take an environment argument too?
;;;    

(defun make-lexical-environment (macrolet/flet/labels-form environment)
  (evalhook (list (car macrolet/flet/labels-form)
                  (cadr macrolet/flet/labels-form)
                  (list 'make-lexical-environment-2))
            'make-lexical-environment-1
            ()
            environment))


(defun make-lexical-environment-1 (form env)
  (setq form (macroexpand form #-excl env
			       #+excl (cadr env)))
  (evalhook form  'make-lexical-environment-1 nil env))

(defmacro make-lexical-environment-2 (&environment env)
  (list 'quote (copy-tree env)))

;;; eof
