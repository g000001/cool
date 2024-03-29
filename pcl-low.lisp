;;;-*-Mode:LISP; Package:(PCL (LISP WALKER) 1000); Base:10; Syntax:Common-lisp -*-
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
;;; This file contains portable versions of low-level functions and macros
;;; which are ripe for implementation specific customization.  None of the
;;; code in this file *has* to be customized for a particular Common Lisp
;;; implementation. Moreover, in some implementations it may not make any
;;; sense to customize some of this code.
;;;
;;; But, experience suggests that MOST Common Lisp implementors will want
;;; to customize some of the code in this file to make PCL run better in
;;; their implementation.  The code in this file has been separated and
;;; heavily commented to make that easier.
;;;
;;; Implementation-specific version of this file already exist for:
;;; 
;;;    Symbolics 3600 family       3600-low.lisp
;;;    Lucid Lisp                  lucid-low.lisp
;;;    Xerox 1100 family           1100-low.lisp
;;;    Ti Explorer                 ti-low.lisp
;;;    Vaxlisp                     vaxl-low.lisp
;;;    Spice Lisp                  spice-low.lisp
;;;    Kyoto Common Lisp           kcl-low.lisp
;;;    ExCL (Franz)                excl-low.lisp
;;;    H.P. Common Lisp            hp-low.lisp
;;;    
;;;
;;; These implementation-specific files are loaded after this file.  Because
;;; none of the macros defined by this file are used in functions defined by
;;; this file the implementation-specific files can just contain the parts of
;;; this file they want to change.  They don't have to copy this whole file
;;; and then change the parts they want.
;;;
;;; If you make changes or improvements to these files, or if you need some
;;; low-level part of PCL re-modularized to make it more portable to your
;;; system please send mail to CommonLoops.pa@Xerox.com.
;;;
;;; Thanks.
;;; 

(in-package :cool.pcl)

  ;;   
;;;;;; without-interrupts
  ;;   
;;; OK, Common Lisp doesn't have this and for good reason.  But For all of
;;; the Common Lisp's that PCL runs on today, there is a meaningful way to
;;; implement this.  WHAT I MEAN IS:
;;;
;;; I want the body to be evaluated in such a way that no other code that is
;;; running PCL can be run during that evaluation.  I agree that the body
;;; won't take *long* to evaluate.  That is to say that I will only use
;;; without interrupts around small computations.
;;;
;;; OK?
;;;
(defmacro without-interrupts (&body body)
  `(progn ,.body))

  ;;   
;;;;;; Load Time Eval
  ;;
;;;
;;; #, is woefully inadequate.  You can't use it inside of a macro and have
;;; the expansion of part of the macro be evaluated at load-time.
;;;
;;; load-time-eval is used to provide an interface to implementation
;;; dependent implementation of load time evaluation.
;;;
;;; A compiled call to load-time-eval:
;;;   should evaluated the form at load time,
;;;   but if it is being compiled-to-core evaluate it at compile time
;;; Interpreted calls to load-time-eval:
;;;   should just evaluate form at run-time.
;;; 
;;; The portable implementation just evaluates it every time, and PCL knows
;;; this.  PCL is careful to only use load-time-eval in places where (except
;;; for performance penalty) it is OK to evaluate the form every time.
;;; 
(defmacro load-time-eval (form)
  #+lispworks `(load-time-value ,form)
  #-lispworks `(progn ,form)
  )

  ;;   
;;;;;; Memory Blocks (array-like blocks of memory)
  ;;
;;; The portable implementation of memory-blocks is as arrays.
;;;
;;; The area argument to make-memory-block is based on the area feature of
;;; LispM's.  As it is used in PCL that argument will always be an unquoted
;;; symbol.  So a call to make-memory-block will look like:
;;;     (make-memory-block 100 class-wrapper-area)
;;; This allows any particular implementation of make-memory-block to look at
;;; the symbol at compile time (macroexpand time) and know where the memory-
;;; block should be consed.  Currently the only values ever used as the area
;;; argument are:
;;; 
;;;    CLASS-WRAPPER-AREA        used when making a class-wrapper
;;;
;;; NOTE:
;;;     It is perfectly legitimate for an implementation of make-memory-block
;;;     to ignore the area argument.  It only exists to try to improve paging
;;;     performance in systems which do allow control over where memory is
;;;     allocated.
;;; 
(defmacro class-wrapper-class (class-wrapper)
  `(memory-block-ref ,class-wrapper 0))

(defmacro make-memory-block (size &optional area)
  (declare (ignore area))
  `(make-array ,size :initial-element nil))

(defmacro memory-block-size (block)
  `(array-dimension ,block 0))

(defmacro memory-block-ref (block offset)
  `(svref ,block ,offset))

(cltl1-eval-when (compile load eval)

(defun make-memory-block-mask (size &optional (words-per-entry 2))
  (logxor (1- (expt 2 (floor (log size 2))))
	  (1- (expt 2 (ceiling (log words-per-entry 2))))))

)

;;;
;;; clear-memory-block sets all the slots of a memory block to nil starting
;;; at start.  This really shouldn't be a macro, it should be a function.
;;; It has to be a macro because otherwise its call to memory-block-ref will
;;; get compiled before people get a chance to change memory-block-ref.
;;; This argues one of:
;;;  - this should be a function in another file.  No, it belongs here.
;;;  - Common Lisp should have defsubst.  Probably
;;;  - Implementors should take (proclaim '(inline xxx)) more seriously.
;;;  
(defmacro clear-memory-block (block start &optional times)
  (once-only (block)
    `(do ((end ,(if times `(+ ,start ,times) `(length ,block)))
	  (index ,start (+ index 1)))
	 ((= index end))
       (setf (memory-block-ref ,block index) nil))))

  ;;   
;;;;;; CLASS-OF
  ;;
;;;
;;; *class-of* is the lisp code for the definition of class-of.
;;;
;;; This version uses type-of to determine the class of an object.  Because
;;; of the underspecification of type-of, this does not always produce the
;;; "most specific class of which x is an instance".  But it is the best I
;;; can do portably.
;;;
;;; Specific ports of PCL should feel free to redefine *class-of* to provide
;;; a more accurate definition.  At some point in any definition of class-of
;;; there should be a test to determine if the argument is a %instance, and
;;; if so the %instance-class-of macro should be used to determine the class
;;; of the instance.
;;;
;;; Whenever a new meta-class is defined, the portable code will take care of
;;; modifying the definition of %instance-class-of and recompiling class-of.
;;;
(defvar *class-of*
	'(lambda (x) 
	   (or (and (%instancep x)
		    (%instance-class-of x))
	      ;(%funcallable-instance-p x)
	       (class-named (type-of x) t)
	       (error "Can't determine class of ~S" x))))

(defvar *meta-classes* ())


(defmacro %instance-class-of (arg)
  `(cond ,@(iterate ((mc in *meta-classes*))
	     (collect
	       `((eq (%instance-meta-class ,arg)
		     ;; %^&$%& KCL has to have this stupid call to
		     ;; load-time-eval here because their compiler
		     ;; always creates a file and compiles that file.
		     #-KCL',(class-named (car mc))
		     #+KCL (load-time-eval (class-named ',(car mc))))
		 #|(funcall (function ,(cdr mc)) ,arg)|#
                 (funcall ,(cdr mc) ,arg)
                 )))
	 (t
	  (error
           "~A: Internal error in %INSTANCE-CLASS-OF.  The argument to~%~
             %instance-class-of is a %instance, but its meta-class is~%~
             not one of the meta-classes defined with define-meta-class."
	    (%instance-meta-class ,arg)))))

(defmacro define-meta-class (name class-of-function &rest options)
  (declare (ignore options))
  (check-type name symbol "a symbol which is the name of a meta-class")
  ;; (check-type class-of-function function "a function")  
  `(load-define-meta-class ',name #',class-of-function))

(defun load-define-meta-class (name class-of-function)
  (or (eq name 'class)
      (class-named name t)
      (error "In define-meta-class, there is no class named ~S.~%~
              The class ~S must be defined before evaluating this~%~
              define-meta-class form."
             name name))
  (let ((existing (assq name *meta-classes*)))
    (if existing
	(setf (cdr existing) class-of-function)
	(setq *meta-classes* (nconc *meta-classes*
				    (list (cons name class-of-function)))))
    (recompile-class-of)))

(declaim (notinline class-of))

(defun recompile-class-of ()
    ;; Change the definition of class-of so that the next time it is
    ;; called it will recompile itself.
    ;; NOTE:  This does not have to be written this way.  If we impose
    ;;        the constraint that any define-meta-class must be loaded
    ;;        in the same environment as it was compiled then there is
    ;;        no need for a compiler at run or load time.
    ;;        By same environment I mean with the same define-meta-class
    ;;        forms already in force, and this certainly seems like a
    ;;        reasonable constraint to me.
    (setf (symbol-function 'class-of)
           #'(lambda (x)
               
               ;; Now recompile class-of so that the new definition
               ;; of %instance-class-of will take effect.
               (compile 'class-of *class-of*)
               (class-of x))))


  ;;
;;;;;; TYPEP and TYPE-OF support.
  ;;
;;; Portable CommonLoops makes no changes to typep or type-of.  In order for
;;; those functions to work with CommonLoops objects each implementation will
;;; have to fix its typep and type-of.  It shouldn't be hard though, and
;;; these macros should help.

(defmacro %instance-typep (x type)
  `(not (null (memq (class-named ,type ())
                    (class-class-precedence-list (class-of ,x))))))

(defmacro %instance-type-of (x)
  `(class-name (class-of ,x)))

  ;;   
;;;;;; The primitive instances.
  ;;
;;;
;;; Conceptually, a %instance is an array-like datatype whose first element
;;; points to the meta-class of the %instance and whose remaining elements
;;; are used by the meta-class for whatever purpose it wants.
;;;
;;; What would like to do is use defstruct to define a new type with a
;;; variable number of slots.  Unfortunately, Common Lisp itself does not
;;; let us do that.  So we have to define a new type %instance, and have
;;; it point to an array which is the extra slots.
;;;
;;; Most any port of PCL should re-implement this datatype.  Implementing it
;;; as a variable length type so that %instance are only one vector in memory
;;; (the "extra slots" are in-line with the meta-class) will have significant
;;; impact on the speed of many CommonLoops programs.  As an example of how
;;; to do this re-implementation of %instance, please see the file 3600-low.
;;; 

(defstruct (%instance (:print-function print-instance)
		      (:constructor %make-instance-1 (meta-class storage))
		      (:predicate %instancep))
  meta-class
  storage)

(defmacro %make-instance (meta-class size)
  `(%make-instance-1 ,meta-class (make-array ,size)))

(defmacro %instance-ref (instance index)
  `(aref (%instance-storage ,instance) ,index))

(defun print-instance (instance stream depth) ;This is a temporary definition
  (declare (ignore depth))                      ;used mostly for debugging the
  (printing-random-thing (instance stream)    ;bootstrapping code.
    (format stream "instance ??")))

  ;;
;;;;;;  Very Low-Level representation of instances with meta-class class.
  ;;
;;; As shown below, an instance with meta-class class (iwmc-class) is a three
;;; *slot* structure.
;;;   
;;; 
;;;                                             /------["Class"]
;;;                  /-------["Class Wrapper"  /  <slot-and-method-cache>]
;;;                 /
;;;  Instance--> [ / , \  ,  \ ]
;;;                     \     \
;;;                      \     \---[Instance Slot Storage Block]
;;;                       \
;;;                        \-------[Dynamic Slot plist]
;;;
;;; Instances with meta-class class point to their class indirectly through
;;; the class's class wrapper (each class has one class wrapper, not each
;;; instance).  This is done so that all the extant instances of a class can
;;; have the class they point to changed quickly.  See change-class.
;;;
;;; Static-slots are a 1-d-array-like structure.
;;; The default PCL implementation is as a memory block as described above.
;;; Particular ports are free to change this to a lower-level block of memory
;;; type structure. Once again, the accessor for static-slots storage doesn't
;;; need to do bounds checking, and static-slots structures don't need to be
;;; able to change size.  This is because new slots are added using the
;;; dynamic slot mechanism, and if the class changes or the class of the
;;; instance changes a new static-slot structure is allocated (if needed).
;;
;;; Dynamic-slots are a plist-like structure.
;;; The default PCL implementation is as a plist.
;;;
;;; *** Put a real discussion here of where things should be consed.
;;;  - if all the class wrappers in the world are on the same page that
;;;    would be good because during method lookup we only use the wrappers
;;;    not the classes and once a slot is cached, we only use the wrappers
;;;    too.  So a page of just wrappers would stay around all the time and
;;;    you would never have to page in the classes at least in "tight" loops.
;;;

(defmacro iwmc-class-p (x)
  `(and (%instancep ,x)
	(eq (%instance-meta-class ,x)
	    (load-time-eval (class-named 'class)))))

;(defmacro %allocate-iwmc-class ()
;  `(%make-instance (load-time-eval (class-named 'class)) 3))

(defmacro iwmc-class-class-wrapper (iwmc-class)
  `(%instance-ref ,iwmc-class 0))

(defmacro iwmc-class-static-slots (iwmc-class)
  `(%instance-ref ,iwmc-class 1))

(defmacro iwmc-class-dynamic-slots (iwmc-class)
  `(%instance-ref ,iwmc-class 2))


(defmacro %allocate-instance--class (no-of-slots &optional class-class)
  `(let ((iwmc-class
	   (%make-instance ,(or class-class
				'(load-time-eval (class-named 'class)))
			   3)))
     (%allocate-instance--class-1 ,no-of-slots iwmc-class)
     iwmc-class))

(defmacro %allocate-instance--class-1 (no-of-slots instance)
  (once-only (instance)
    `(progn 
       (setf (iwmc-class-static-slots ,instance)
	     (%allocate-static-slot-storage--class ,no-of-slots))
       (setf (iwmc-class-dynamic-slots ,instance)
	     (%allocate-dynamic-slot-storage--class)))))


(defmacro %allocate-class-class (no-of-slots)	;This is used to allocate the
  `(let ((i (%make-instance nil 3)))		;class class.  It bootstraps
     (setf (%instance-meta-class i) i)		;the call to class-named in
     (setf (class-named 'class) i)		;%allocate-instance--class.
     (%allocate-instance--class-1 ,no-of-slots i)
     i))

(defmacro %convert-slotd-position-to-slot-index (slotd-position)
  slotd-position)


(defmacro %allocate-static-slot-storage--class (no-of-slots)
  `(make-memory-block ,no-of-slots))

(defmacro %static-slot-storage-get-slot--class (static-slot-storage
						slot-index)
  `(memory-block-ref ,static-slot-storage ,slot-index))

(defmacro %allocate-dynamic-slot-storage--class ()
  ())

(defmacro %dynamic-slot-storage-get-slot--class (dynamic-slot-storage
						 name
						 default)
  `(getf ,dynamic-slot-storage ,name ,default))

(defmacro %dynamic-slot-storage-remove-slot--class (dynamic-slot-storage
						    name)
  `(remf ,dynamic-slot-storage ,name))



(defmacro class-of--class (iwmc-class)
  `(class-wrapper-class (iwmc-class-class-wrapper ,iwmc-class)))

(define-meta-class class (lambda (x) (class-of--class x)))


  ;;   
;;;;;; Class Wrappers  (the Watercourse Way algorithm)
  ;;
;;; Well, we had this really cool scheme for keeping multiple different
;;; caches tables in the same block of memory.  Unfortunately, we only
;;; cache one thing in class wrappers these days, and soon class wrappers
;;; will go away entirely so its kind of lost generality.  I am leaving
;;; the old comment here cause the hack is worth remembering.
;;;
;;; * Old Comment
;;; * The key point are:
;;; *
;;; *  - No value in the cache can be a key for anything else stored
;;; *    in the cache.
;;; *
;;; *  - When we invalidate a wrapper cache, we flush it so that when
;;; *    it is next touched it will get a miss.
;;; *
;;; * A class wrapper is a block of memory whose first two slots have a
;;; * deadicated (I just can't help myself) purpose and whose remaining
;;; * slots are the shared cache table.  A class wrapper looks like:
;;; *
;;; *  slot 0:   <pointer to class>
;;; *  slot 1:   T if wrapper is valid, NIL otherwise.
;;; *   .
;;; *   .          shared cache
;;; *   .
;;;

(cltl1-eval-when (compile load eval)

(defconstant class-wrapper-cache-size 32
  "The size of class-wrapper caches.")

(defconstant class-wrapper-leader 2
  "The number of slots at the beginning of a class wrapper which have a
   special purpose.  These are the slots that are not part of the cache.")

; due to a compiler bug, the extra "2" default argument has been added
; to the following function invocation, for HP Lisp. rds 3/6/87
(defconstant class-wrapper-cache-mask 
	     (make-memory-block-mask class-wrapper-cache-size 2))

)

(defmacro make-class-wrapper (class)
  `(let ((wrapper (make-memory-block ,(+ class-wrapper-cache-size
					 class-wrapper-leader)
				     class-wrapper-area)))
     (setf (class-wrapper-class wrapper) ,class)
     (setf (class-wrapper-valid-p wrapper) t)
     wrapper))

;;; class-wrapper-class here

(defmacro class-wrapper-valid-p (class-wrapper)
  `(memory-block-ref ,class-wrapper 1))

(defmacro class-wrapper-cached-key (class-wrapper offset)
  `(memory-block-ref ,class-wrapper ,offset))

(defmacro class-wrapper-cached-val (class-wrapper offset)
  `(memory-block-ref ,class-wrapper (+ ,offset 1)))

(defmacro class-wrapper-get-slot-offset (class-wrapper slot-name)
  (declare (ignore class-wrapper))
  `(+ class-wrapper-leader
      0
      (symbol-cache-no ,slot-name ,class-wrapper-cache-mask)))


(defmacro flush-class-wrapper-cache (class-wrapper)
  `(clear-memory-block ,class-wrapper
		       ,class-wrapper-leader
		       ,class-wrapper-cache-size))

(defmacro class-wrapper-cache-cache-entry (wrapper offset key val)
  (once-only (wrapper offset key val)
    `(without-interrupts
       (setf (class-wrapper-cached-key ,wrapper ,offset) ,key)	 ;store key
       (setf (class-wrapper-cached-val ,wrapper ,offset) ,val))));store value

(defmacro class-wrapper-cache-cached-entry (wrapper offset key)
  (once-only (wrapper offset)
    `(and (eq (class-wrapper-cached-key ,wrapper ,offset) ,key)
	  (class-wrapper-cached-val ,wrapper ,offset))))

(defmacro invalidate-class-wrapper (wrapper)
  (once-only (wrapper)
    `(progn (flush-class-wrapper-cache ,wrapper)
	    (setf (class-wrapper-valid-p ,wrapper) nil))))

(defmacro validate-class-wrapper (iwmc-class)	          ;HAS to be a macro!
  `(let ((wrapper (iwmc-class-class-wrapper ,iwmc-class)));So that xxx-low
     (if (class-wrapper-valid-p wrapper)	          ;can redefine the
	 wrapper				          ;macros we use.
	 (progn (setf (iwmc-class-class-wrapper ,iwmc-class)
		      (class-wrapper (class-wrapper-class wrapper)))
		(setf (class-wrapper-valid-p wrapper) t)))))

  ;;   
;;;;;; Generating CACHE numbers
  ;;
;;; These macros should produce a CACHE number for their first argument
;;; masked to fit in their second argument.  A useful cache number is just
;;; the symbol or object's memory address.  The memory address can either
;;; be masked to fit the mask or folded down with xor to fit in the mask.
;;; See some of the other low files for examples of how to implement these
;;; macros. Except for their illustrative value, the portable versions of
;;; these macros are nearly worthless.  Any port of CommonLoops really
;;; should redefine these to be faster and produce more useful numbers.

(defvar *warned-about-symbol-cache-no* nil)
(defvar *warned-about-object-cache-no* nil)

(defmacro symbol-cache-no (symbol mask)
  (unless *warned-about-symbol-cache-no*
    (setq *warned-about-symbol-cache-no* t)
    (warn
      "Compiling PCL without having defined an implementation-specific~%~
       version of SYMBOL-CACHE-NO.  This is likely to have a significant~%~
       effect on slot-access performance.~%~
       See the definition of symbol-cache-no in the file low to get an~%~
       idea of how to implement symbol-cache-no."))
  `(logand (sxhash ,symbol) ,mask))

(defmacro object-cache-no (object mask)
  (declare (ignore object))
  (unless *warned-about-object-cache-no*
    (setq *warned-about-object-cache-no* t)
    (warn
      "Compiling PCL without having defined an implementation-specific~%~
       version of OBJECT-CACHE-NO.  This effectively disables method.~%~
       lookup caching.  See the definition of object-cache-no in the file~%~
       low to get an idea of how to implement object-cache-no."))
  `(logand 0 ,mask))


  ;;   
;;;;;; FUNCTION-ARGLIST
  ;;
;;; Given something which is functionp, function-arglist should return the
;;; argument list for it.  PCL does not count on having this available, but
;;; MAKE-SPECIALIZABLE works much better if it is available.  Versions of
;;; function-arglist for each specific port of pcl should be put in the
;;; appropriate xxx-low file. This is what it should look like:
;(defun function-arglist (function)
;  (<system-dependent-arglist-function> function))

(defun function-pretty-arglist (function)
  (declare (ignore function))
  ())

(defsetf function-pretty-arglist set-function-pretty-arglist)

(defun set-function-pretty-arglist (function new-value)
  (declare (ignore function))
  new-value)



  ;;   
;;;;;; Templated functions
  ;;   
;;; In CommonLoops there are many program-generated functions which
;;; differ from other, similar program-generated functions only in the
;;; values of certain in-line constants.
;;;
;;; A prototypical example is the family of discriminating functions used by
;;; classical discriminators.  For all classical discriminators which have
;;; the same number of required arguments and no &rest argument, the
;;; discriminating function is the same, except for the value of the
;;; "in-line" constants (the cache and discriminator).
;;;
;;; Naively, whenever we want one of these functions we have to produce and
;;; compile separate lambda. But this is very expensive, instead what we
;;; would like to do is copy the existing compiled code and replace the
;;; values of the inline constants with the right new values.
;;;
;;; Templated functions provide a nice interface to this abstraction of
;;; copying an existing compiled function and replacing certain constants
;;; with others.  Templated functions are based on the assumption that for
;;; any given CommonLisp one of the following is true:
;;;   Either:
;;;     Funcalling a lexical closure is fast, and lexical variable access
;;;     is as fast (or about as fast) in-line constant access.  In this
;;;     case we implement templated functions as lexical closures closed
;;;     over the constants we want to change from one instance of the
;;;     templated function to another.
;;;   Or:
;;;     Code can be written to take a compiled code object, copy it and
;;;     replace references to certain in-line constants with references
;;;     to other in-line constants.
;;;
;;; Actually, I believe that for most Lisp both of the above assumptions are
;;; true.  For certain lisps the explicit copy and replace scheme *may be*
;;; more efficient but the lexical closure scheme is completely portable and
;;; is likely to be more efficient since the lexical closure it returns are
;;; likely to share compiled code objects and only have separate lexical
;;; environments.
;;;
;;; Another thing to notice about templated functions is that they provide
;;; the modularity to support special objects which a particular
;;; implementation's low-level function-calling code might know about.   As
;;; an example, when a classical discriminating function is created, the
;;; code says "make a classical discriminating function with 1 required
;;; arguments". It then uses whatever comes back from the templated function
;;; code as the the discriminating function So, a particular port can easily
;;; make this return any sort of special data structure instead of one of
;;; the lexical closures the portable implementation returns.
;;;
(defvar *templated-function-types* ())
(defmacro define-function-template (name
				    template-parameters
				    instance-parameters
				    &body body)
  `(progn
     (pushnew ',name *templated-function-types*)
     ;; Get rid of all the cached constructors.
     (setf (get ',name 'templated-fn-constructors) ())
     ;; Now define the constructor constructor.
     (setf (get ',name 'templated-fn-params)
	   (list* ',template-parameters ',instance-parameters ',body))
     (setf (get ',name 'templated-fn-constructor-constructor)
	   ,(make-templated-function-constructor-constructor
	      template-parameters instance-parameters body))))

(defun reset-templated-function-types ()
  (dolist (type *templated-function-types*)
    (setf (get type 'templated-fn-constructors) ())))

(defun get-templated-function-constructor (name &rest template-parameters)
  (setq template-parameters (copy-list template-parameters)) ;Groan.
  (let ((existing (assoc template-parameters
			 (get name 'templated-fn-constructors)
			 :test #'equal)))
    (if existing
	(progn (setf (nth 3 existing) t)	;Mark this constructor as
						;having been used.
	       (cadr existing))			;And return the actual
						;constructor.
	(let ((new-constructor
		(apply (get name 'templated-fn-constructor-constructor)
		       template-parameters)))
	  (push (list template-parameters new-constructor 'made-on-the-fly t)
		(get name 'templated-fn-constructors))
	  new-constructor))))

(defmacro pre-make-templated-function-constructor (name
						   &rest template-parameters)
  (setq template-parameters (copy-list template-parameters))	;Groan.
  (let* ((params (get name 'templated-fn-params))
	 (template-params (car params))
	 (instance-params (cadr params))
	 (body (cddr params))
	 (dummy-fn-name (gensym)))   ;For the 3600, which doesn't bother to 
				     ;compile top-level forms, we do the
				     ;top-level form compilation by hand.
    (progv template-params
	   template-parameters
      `(progn
	 (defun ,dummy-fn-name ()
	   (let ((entry
		   (or (assoc ',template-parameters 
			      (get ',name 'templated-fn-constructors)
			      :test #'equal)
		       (let ((new-entry
			       (list ',template-parameters () () ())))
			 (push new-entry
			       (get ',name 'templated-fn-constructors))
			 new-entry))))
	     (setf (caddr entry) 'pre-made)
	     (setf (cadr entry)
		   (function (lambda ,(eval instance-params)
			       ,(eval (cons 'progn body)))))))
	 (,dummy-fn-name)))))

(defun make-templated-function-constructor-constructor (template-params
							instance-params
							body)
  `(function
     (lambda ,template-params
       (compile () (list 'lambda ,instance-params ,@body)))))

  ;;   
;;;;;; 
  ;;   

(defun record-definition (name type &rest args)
  (declare (ignore name type args))
  ())

(defun compile-time-define (&rest ignore)
  (declare (ignore ignore)))

