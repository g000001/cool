;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         co-dmeth.l
; RCS:          $Revision: 1.1 $
; SCCS:         %A% %G% %U%
; Description:  Defining CommonObjects methods
; Author:       James Kempf
; Created:      March 10, 1987
; Modified:     12-Mar-87 09:21:38 (James Kempf)
; Language:     Lisp
; Package:      COMMON-OBJECTS
; Status:       Distribution
;
; (c) Copyright 1987, HP Labs, all rights reserved.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 1987 Hewlett-Packard Corporation. All rights reserved.
;
; Use and copying of this software and preparation of derivative works based
; upon this software are permitted.  Any distribution of this software or
; derivative works must comply with all applicable United States export
; control laws.
; 
; This software is made available AS IS, and Hewlett-Packard Corporation makes
; no warranty about the software, its performance or its conformity to any
; specification.
;
; Suggestions, comments and requests for improvement may be mailed to
; aiws@hplabs.HP.COM

;;;-*-Mode:LISP; Package:(CO (PCL LISP)); Base:10; Syntax: Common-lisp-*-
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

(in-package :common-objects)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;  nued) Support for Using Keywords as Method Names
;
;  These macros and functions translate keyword method names into
;  names in a package. Some Common Lisps do allow keyword symbols
;  to have an associated function, others don't. Rather than
;  differentiating, a single package, KEYWORD-STANDIN, is used
;  for method symbols which are keywords.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun keyword-standin (keyword)

  ;;An example of a special method is :print which gets
  ;;  translated into the symbol pcl:print-instance

  (if (special-keyword-p keyword)
    (keyword-standin-special keyword)
    (intern (symbol-name keyword) *keyword-standin-package*)
  )

) ;end keyword-standin

;;unkeyword-standin-Return the keyword for a standin symbol

(defun unkeyword-standin (symbol)
  
  (if (special-method-p symbol)
    (unkeyword-standin-special symbol)
      (if (eq (symbol-package symbol) *keyword-standin-package*)
	(setf symbol (intern (symbol-name symbol) (find-package :keyword)))
	symbol

       ) ;if

  ) ;if

) ;end unkeyword-standin

;;Set up the universal method selector list, for fast messaging

(cltl1-eval-when (load eval)
  (dolist (l *universal-methods*)
    (push (keyword-standin l) *universal-method-selectors*)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;	Runtime Interface to the Slots
;
;  The extra slots are used for the pointer to self and for parents. Each 
;  ancestor is actually a fully fledged object of the ancestor type, except its 
;  pointer to self slot points back to the original object piece.
;  Slot indicies can be calculated directly at compile time, since they do
;  not change after the object is created.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;self-from-inner-self-Return the pointer to the original object

(defmacro self-from-inner-self ()
  `(%instance-ref .inner-self. ,$SELF-INDEX)

) ;end self-from-inner-self

;;parent-from-inner-self-Given the parent's name, return a pointer
;;  to the object piece in which the instance variables are stored.

(defmacro parent-from-inner-self (parent-class-name)
  `(get-slot .inner-self. ',(local-super-slot-name parent-class-name))

) ;end parent-from-inner-self

;;local-super-slot-name-Generate a slot name for the parent's instance
;;  variable

(defun local-super-slot-name (local-super-name)
  (intern (concatenate 'string
		"Slot For "
		    (symbol-name local-super-name)))

) ;end local-super-slot-name

;;calculate-slot-index-Return the index of the slot in the vector

(defun calculate-slot-index (slotname parents slots)

  (let
    (
      (parloc (position slotname parents))
      (sloc  (position slotname slots))
    )

    (if parloc
     (+ $START-OF-PARENTS parloc)
     (+ $START-OF-PARENTS (length parents) sloc)
    )

  )

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;	New Method Class For CommonObjects
;
;  CommonObjects methods need to keep track of their method symbol, so
;  that the symbol can be looked up and inserted into a CALL-METHOD
;  or APPLY-METHOD when a method including one of these forms is loaded.
;  The new method keeps track of a method symbol as an instance variable,
;  and maintains the symbol's function cell with an accurate pointer to
;  the current function implementing the method. The function is called
;  through this symbol during run-time processing of a CALL-METHOD.
;  Note that, since the method object gets created when the method
;  is loaded (or, alternatively, looked up, if a CALL-METHOD was
;  processed before the method was defined), the symbol will be GENSYM'ed
;  in the load time environment. Fully qualified symbols are needed for
;  the method names because they are not exported from the PCL package.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;common-objects-method-Add an additional slot for the function symbol name

(ndefstruct 
  (common-objects-method (:class class)
    (:include cool.pcl::method)
    (:conc-name method-)
  )
    (function-symbol NIL)	;;name of the method function
				;;  used for call-method

) ;end common-objects-method

;;method-function-Need this to have the SETF
;;  method work correctly

(defmeth method-function  ((method common-objects-method))

  ;;This was RUN-SUPER-INTERNAL, but now changed to accomodate
  ;;  new code.

  (call-next-method)


) ;end method-function

;;method-function-Even though we may not yet be able to
;;  determine what the function symbol is, the SETF method
;;  must reset the symbol's function, in the event the
;;  method object is recycled. 

(defmeth (method-function (:setf (nv))) ((method common-objects-method))


    ;;If the method function symbol for the CALL-METHOD optimization
    ;;  has not yet been set, do it.

    (when (method-function-symbol method)
      (setf (symbol-function (method-function-symbol method)) 
	    nv
      )

    )


    ;;This was RUN-SUPER-INTERNAL, but now changed to accomodate
    ;; new code.

    (call-next-method)

) ;end method-function :setf

;;method-discriminator-Need this to have the SETF
;;  method work correctly

(defmeth method-discriminator  ((method common-objects-method))

  ;;This was RUN-SUPER-INTERNAL, but now changed to accomodate
  ;;  new code.

  (call-next-method)


) ;end method-discriminator

;;method-discriminator-By the time the method's discriminator is
;;  set, the method has enough information to generate the
;;  symbol for CALL-METHOD optimization.

(defmeth (method-discriminator (:setf (nv))) ((method common-objects-method))


    ;;If the method function symbol for the CALL-METHOD optimization
    ;;  has not yet been set, do it.

    (when (not (method-function-symbol method))
      (setf (method-function-symbol method) 
            (generate-method-function-symbol
	      (class-name (car (method-type-specifiers method)))
	      (discriminator-name nv)
	    )
      )
      (setf (symbol-function (method-function-symbol method)) 
	    (method-function method)
      )

    )


    ;;This was RUN-SUPER-INTERNAL, but now changed to accomodate
    ;; new code.

    (call-next-method)

) ;end method-discriminator :setf

;;generate-method-function-symbol-Generate a method function
;;  symbol for the method. Used in the CALL-METHOD optimization.

(defun generate-method-function-symbol (class-name message)

  ;;Generate a symbol for the function to be called.
  ;;  This is in the same package as the method name
  ;;  symbol, and its name as the form:
  ;;  <class package name>;;<class name> <message package name>;;<message>
  ;;  Note that this will avoid collisions for two methods with
  ;;  the same name and different packages, because the symbol
  ;;  names (as well as the packages) are different.
  ;;  We hope that this should avoid collision.

  (intern
    (concatenate 'simple-string 
		 (package-name (symbol-package class-name))
		 ";;"
		 (symbol-name class-name)
		 " " 
		 (package-name     
		   (if (keywordp message)
                     (find-package 'keyword-standin)
                     (symbol-package message)
                   )
                 )
		 ";;"
		 (symbol-name message)
    )
    (if (keywordp message)
      (find-package 'keyword-standin)
      (symbol-package message)
    )
 ) 

) ;generate-method-function-symbol

;;expand-with-make-entries-Returns an alist of the form:
;; 
;;   (<prefix+slot-name> <instance-form> <class> <slotd> <use-slot-value-p>)
;;

(defmeth expand-with-make-entries ((method common-objects-method) first-arg)
         (declare (ignore first-arg))   ; rds 3/8
  (let* 
    (
      (entries ())
      (method-argument (first (method-arglist method)))
      (method-type-spec (first (method-type-specifiers method)))
    )          

    ;;CommonObjects methods only discriminate on the first 
    ;;  argument. Also, we always want to use the slot value,
    ;;  since there is no slotd-accessor.

    (dolist (slotd (class-slots method-type-spec))
      (push
        (list
          (slotd-name slotd)	;;the slot name
          method-argument	;;the instance arg name
          method-type-spec	;;the class
          slotd			;;the slot descriptor
          T              	;;use the slot value directly
        )
        entries
      )
    ) ;dolist

    entries

  ) ;let*

) ;expand-with-make-entries

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;              Messaging Macros and Functions
;
;   Message sending becomes funcalling the message.
;   We convert all message sends to a funcall of the message.  Because
;   CommonObjects encourages messages to be keywords and keywords are
;   not funcallable, we have to have a special package in which keywords
;   are interned before their use as messages.
;
;   As an example of all this, take the expansion of a sample =>:
;
;      (=> object :message arg-1 arg-2)  expands into:
;
;      (funcall 'keyword-standin::message object arg-1 arg-2)
;
;   This means that all CommonObjects discriminators will be classical.
;   That is they will discriminator only on the class of their first
;   argument.
; 
;   The first argument to any method will always be the inner self, that is
;   an instance of the same class as the method was defined on.  This is
;   bound to the symbol .INNER-SELF., special macros SELF-FROM-INNER-SELF
;   and PARENT-FROM-INNER-SELF are used to access outer-self and parent
;   instances.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;make-set-message-Construct a :SET-xxx message for SETF

(defmacro make-set-message (message)
  
  `(intern
    (concatenate 'simple-string 
                 "SET-" 
                 (symbol-name ,message)
    )
    (symbol-package ,message)

  )

) ;make-set-message

;;=>-Convert to PCL messaging. Note that no error or type checking occurs.

(defmacro => (object message &rest args)

  `(funcall
      ,(if (keywordp message)
	`',(keyword-standin message)
        message
      )
      ,object 
      ,@args
  )

) ;end =>

;;send?-Messaging macro which returns NIL if something is wrong.

(defmacro send? (object message &rest args)

  `(send?-internal 
    ,object 
    ,(if (keywordp message)
	`',(keyword-standin message)
	 message
    )  
    ,@args
  )

) ;end send?

;;Setf definitions for messaging macros.

(defsetf => (obj message) (new-value)

  `(progn
      (=> ,obj 
          ,(if (keywordp message)
            (make-set-message message)
            `(make-set-message ,message)
          )
	  ,new-value
      )
    )
) ;end defsetf for =>

(defsetf send? (obj message) (new-value)
  `(progn
      (send? ,obj 
             ,(if (keywordp message)
               (make-set-message message)
               `(make-set-message ,message)
              )
	    ,new-value
      )
    )
) ;end defsetf for send?

;;send?-internal-Process the message invocation into correct code for
;; SEND?

(defun send?-internal (object message &rest args)

  (if object
    (let*
       (
         (class (class-of object))
         (class-name (class-name class))
         (metaclass-name (class-name (class-of class)))

       )

      ;;Check if OBJECT is an instance and class is still defined
      ;;  and operation is supported.

      (if (and
           (eq metaclass-name 'common-objects-class)
           (not (eq class-name $UNDEFINED-TYPE-NAME))
           (fast-supports-operation-p class message)
          )

    	  (apply message  object args)

          NIL

      ) ;if

    ) ;let*

  ) ;if

) ;send?-internal

;;fast-supports-operation-p-Does no checking on CLASS

(defun fast-supports-operation-p (class message)

;;Check first if its a universal method

  (if (member (unkeyword-standin message) *universal-methods*)

    T

    ;;Otherwise, check in the class object if it's got them

    (dolist (methobj (cool.pcl::class-direct-methods class))

      (when (eq (method-name methobj) message)
        (return-from fast-supports-operation-p T)
      )

    ) ;dolist
  ) ;if

) ;fast-supports-operation-p

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Method Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;defcommon-objects-meth-Create method and discriminator objects and
;;  call EXPAND-DEFMETH-INTERNAL. The method object is of class
;;  common-objects-method. Note that this macro gets expanded at the
;;  time this file is compiled.

(defmacro defcommon-objects-meth (message arglist body &optional decls)
  `(let ((discriminator-class-object (class-named 'cool.pcl::discriminator t))
         (method-class-object (class-named 'common-objects-method t)))
     (cool.pcl::expand-defmeth-internal (class-prototype
                                         discriminator-class-object)
                                        (class-prototype method-class-object)
                                        (if (listp ,message)
                                            ,message
                                            (list ,message))
                                        ,arglist
                                        (append ,decls (list ,body)))))

;;define-method-Top level programmer interface to method
;;  definition

(defmacro define-method (spec arglist &body body)

  ;;Syntax check the call first

  (co-parse-method-macro-call spec arglist body)

  (let* 
    (
      (class-name (car spec))
      (message (if (keywordp (cadr spec))
		     (keyword-standin (cadr spec))
		     (cadr spec)))
    )


    ;;Check first to be sure that class is a CommonObjects class

    (if (not 
          (eq (class-name (class-of (class-named class-name T))) 'common-objects-class)
        )
      (error "DEFINE-METHOD: `~S' is not a CommonObjects type." class-name)
    )

    ;;The compiler-let of *CURRENT-METHOD-CLASS-NAME* is to support
    ;;  CALL-METHOD.
    ;;  Also, bind SELF around the body to outer self.
    ;;  Note that this allows someone to rebind SELF in the body, but
    ;;  that rebinding will not affect CALL-METHOD, APPLY-METHOD or IV
    ;;  access since they don't really use SELF.
    ;;  Also, use WITH to allow lexical access to the instance 
    ;;  variables.

    (setq body `(compiler-let 
                  (
                    (*current-method-class-name* ',class-name)
	          )

                  (let ((self (self-from-inner-self)))
		    (with* 
		      (
			(.inner-self. "" ,class-name)
		      )

                      self
		      (progn . ,body))
                 )

	      ) ;compiler-let
    )	  


    `(progn

        ,(defcommon-objects-meth message 
           `((.inner-self. ,class-name) ,@arglist) 

	   body

         )

         (list ',class-name ',(cadr spec))

       ) ;progn

   ) ;let*

) ;end define-method

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;	Call-Method and Optimizations
;
;  Because of pf the ambiguous nature of the definition of #, in CLtL,
;  the implementation of #, may not work correctly on a particular system
;  when used within the backquote macro in compiled code.
;  The kind of behavior which is needed is as follows (with reference
;  to 5.3.3, pg. 70)
;
;  1) If the situation is EVAL, then execute the function
;     LOAD-TIME-GET-CALL-METHOD-FUNCTION-SYMBOL and cache the 
;     method symbol in line when the code is macroexpanded.
;
;  2) If the situation is compile, then arrange for the function
;     LOAD-TIME-GET-CALL-METHOD-FUNCTION-SYMBOL to be executed
;     and the result cached only when the file gets loaded.
;
;  What I want to say is:
;
;   `(,caller
;         #,(load-time-get-call-method-function ',class-name ',method-name
;					       ',arglist
;          )
;          <rest of form>
;     )
;
;  and have it work correctly. Well, it doesn't always.
;
;  Alternatively, I would like to generate a closure at compile time
;  which will get fasled into the output file and will cache the
;  method symbol the first time it is called. But that doesn't
;  always work either.
;
;  So, instead, I tried using an elaborate scheme which creates vectors
;  at compile time and uses a top level (EVAL-WHEN (LOAD) ...) to 
;  depost the method symbol at load time. The special variable
;  *LIST-OF-CALL-METHOD-FIXUPS* gets bound to NIL before every
;  DEFINE-METHOD invocation. The CALL-METHOD macro creates
;  instances of the DEFSTRUCT CALL-METHOD-RECORD and pushes them
;  on *LIST-OF-CALL-METHOD-FIXUPS* recording CALL-METHODs and
;  vectors for caching the method symbol. The CALL-METHOD macro
;  can do this because the PCL method EXPAND-DEFMETH-INTERNAL
;  is replaced in the patches file. This new method walks
;  them method code body during the execution of EXPAND-DEFMETH-INTERNAL
;  rather than at the top level, as in the stock PCL system.
;  If this change is NOT made, then the method body must
;  be prewalked before code generation, because the code
;  walk (during which CALL-METHOD gets expanded) doesn't
;  occur until after DEFINE-METHOD returns to the top level.
;
;  As the last part of the DEFINE-METHOD code generation,
;  a top level (EVAL-WHEN (LOAD EVAL) ...) is generated to get
;  the method symbol at load time and deposit it in the
;  vector. The SVREF gets the symbol at the time the CALL-METHOD
;  is invoked. So, in effect, I'm generating my own
;  closure.
;
;  Well, that doesn't work either. Why? Because once the
;  vector is deposited into the code, there is no guarantee
;  that it will be EQ to the one in the list. And, in any
;  event, this scheme won't work in traditional interpreters
;  which expand macros as they are encountered, since the
;  top level (EVAL-WHEN (LOAD EVAL) ... ) gets done before
;  the CALL-METHOD macro is fully expanded.
;
;  Sigh. The only choice is to GENSYM a symbol at compile
;  time and pray that it doesn't trash something at load time.
;  But maybe that's OK.
;
;  Note that the general behavior which is desired here is loadtime 
;  execution within generated code, rather than at the top level.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;call-method-Top level macro for CALL-METHOD.

(defmacro call-method (spec &rest args)
  (call-method-internal 'call-method spec args) 

) ;end call-method

;;apply-method-Top level macro for APPLY-METHOD.

(defmacro apply-method (spec &rest args)
  (call-method-internal 'apply-method spec args)

) ;end apply-method

;;call-method-internal-Process a CALL-METHOD invocation.

(defun call-method-internal (for spec args)
  (declare (special *current-method-class-name*))
  (if (null (boundp '*current-method-class-name*))
      (error "Attempt to use ~S other than inside a method.~%" for)
      (let* ((caller (ecase for
		      (call-method 'funcall)
		      (apply-method 'apply)))
	    (class-name (if (listp spec)
			    (car spec)
			    *current-method-class-name*))
	    (message (if (listp spec) (cadr spec) spec))

	    (fsym (generate-method-function-symbol class-name message))

          )


         ;;Check the syntax

         (co-parse-call-to-method (list for spec args) 
                                  (symbol-name for)
                                  *current-method-class-name*
         )


         ;;Generate code. Note there is no need to check
         ;;  whether or not the method function symbol
         ;;  is bound or to do any fixing up at all.
         ;;  If it is not, then its an error, because
         ;;  the method hasn't yet been defined. The
         ;;  function cell will be bound when the 
         ;;  method gets defined.

	`(,caller (symbol-function ',fsym)

	  ,(if (listp spec)
	       `(parent-from-inner-self ,class-name)
	       '.inner-self.)
	  ,@args)
    ) ;let
  ) ;if

) ;end call-method-internal


;;legal-parent-p-Is parent-name a legal parent of class-name?

(defun legal-parent-p (class-name parent-name)

  (member parent-name 
          (class-local-super-names (class-named class-name T))
          :test #'eq

  )
) ;legal-parent-p


