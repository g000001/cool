
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         co-dtype.l
; RCS:          $Revision: 1.1 $
; SCCS:         %A% %G% %U%
; Description:  CommonObjects types.
; Author:       James Kempf
; Created:      March 10, 1987
; Modified:     12-Mar-87 09:58:43 (James Kempf)
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
;  Define-Type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;define-type-Define a CommonObjects type

(defmacro define-type (&rest body)

     (internal-define-type body)

) ;end define-type

;;internal-define-type-Parse a CommonObjects type definition and
;;  generate code for creating the type.

(defun internal-define-type (body)

   (let
     (
       (doc-string NIL) ;;documentation string, if any
       (name NIL)	;;type name
       (parents NIL)    ;;list of parents
       (slots   NIL)	;;list of instance variables
       (options NIL)    ;;options list
       (phonytiv NIL)	;;phony type info vector. Used to
                        ;;  hold type definition during
                        ;;  parsing.
       (assignments NIL);;variable initializations
       (settables NIL)	;;settable method names
       (gettables NIL)	;;gettable method names
       (inherited NIL)	;;inherited methods w. parents
       (keywords NIL)   ;;keywords for initialization
       (init-key-check  ;;T if a check should occur
        NIL
       )
       (dont-define NIL)  ;;methods to not define
     )

  
     ;;Get name and options

     (multiple-value-setq
      (name doc-string options)
        (co-parse-define-type-call (cons 'define-type body) 
				   name doc-string options
        )
     )

     ;;Make a phony type info for use with options parsing code

     (setf phonytiv (build-phony-type-info name))     

     ;;Get variable names, assignments, and other options

     (multiple-value-setq
      (slots assignments options)
      (co-process-var-options phonytiv options slots assignments)
     )

     ;;Fill in phony type info with option information

     (co-parse-options phonytiv slots options)

     (setf parents (svref phonytiv $PARENT-TYPES-SLOT))

     (setf gettables (svref phonytiv $GETTABLE-VARIABLES-SLOT))
     (setf settables (svref phonytiv $SETTABLE-VARIABLES-SLOT))
     (setf inherited (svref phonytiv $METHODS-TO-INHERIT-SLOT))
     (setf init-key-check 
	(not (svref phonytiv $NO-INIT-KEYWORD-CHECK-SLOT))
     )
     (setf dont-define 
       (svref phonytiv $METHODS-TO-NOT-DEFINE-SLOT)
     )

     ;;Make keywords out of initiable variables and merge with
     ;;  keywords

     (setf keywords 
           (append
	     (svref phonytiv  $INIT-KEYWORDS-SLOT)
             (mapcar 
               #'(lambda (x) 
                 (intern (symbol-name x) (find-package 'keyword))
               )
               (svref phonytiv $INITABLE-VARIABLES-SLOT)
             )
           )

    ) ;setf

    ;;All compile-time checking must be done BEFORE the compile-time
    ;;  class definition is done, so that errors don't leave
    ;;  around a bogus class.

    ;;Merge duplicate method names and check for inheritance
    ;;  funny business

    (merge-duplicates name gettables settables inherited dont-define)

     ;;Fully define the class at compile-time, so that 
     ;;  method definition works. Note that this means that
     ;;  any pre-existing definition will be clobbered.
     ;;  Compile time definition is needed for
     ;;  any other methods which are defined in the same
     ;;  file as a type definition. This is necessary because
     ;;  the metaobject protocol doesn't distinguish between
     ;;  a partially defined type and a fully defined one.
     ;;  Compile-time definition is no longer needed for
     ;;  definition of inherited, universal, and get/set
     ;;  methods, since the metaobject protocol is gone
     ;;  around for these, except for the :INITIALIZE-VARIABLES
     ;;  method, which is still generated in full.

     (fully-define-type name slots parents keywords init-key-check)

     ;;Generate code for the class definition. This code
     ;;  defines the class at load time and the universal
     ;;  methods.

    `(progn

       ;;This only needs to get done at load time, since
       ;;  class definition at compile time (to take
       ;;  care of :INITIALIZE-VARIABLES method generation
       ;;  and others in the file) is done during the macro
       ;;  expansion. Also, it need not get done if the
       ;;  definition is being evaluated, since the macro
       ;;  has already done in.

       (cltl1-eval-when (load)
	 (fully-define-type ',name 
		            ',slots 
			    ',parents
			    ',keywords
			    ',init-key-check
	 )
       )

        ;;Define the initialization, get/set, and inherited methods.

        ;;Variable initialization is handled by generating an
        ;;  initialization method. The :INITIALIZE-VARIABLES method 
        ;;  is the only universal one  generated on a type by type basis.
        ;;  Since the user can insert anything into the initialization
        ;;  forms, the code must go through the full processing
        ;;  for method definition, including code walking of
        ;;  WITH-SLOTS. This requires that the PCL class be
        ;;  defined at compile time.

        ,(if (not (member ':initialize-variables dont-define))
          (build-init-vars-method
            name
            (svref phonytiv $INITABLE-VARIABLES-SLOT)
	    assignments
          )
        )

	;;Universal methods are no longer defined on a per type
        ;;  basis, but rather default methods are defined
        ;;  for all CommonObjects types. The user can define
	;;  their own methods which override the default ones,
	;;  but the defaults can't be undefined or renamed.
	;;  Using defaults saves time during type definition.

        ;;Inherited methods  must be defined
        ;;  at compile time, otherwise the CLASS-DIRECT-METHODS
        ;;  call in METHOD-ALIST won't find the gettable and
        ;;  settable methods during compilation. This is
        ;;  also true for gettable and settable methods.
        ;;  Note, however, that other methods defined in
        ;;  the same file will NOT get inherited, because
        ;;  they are not fully defined at compile time.
        ;;  This means that users should avoid defining
        ;;  parent and child types in the same file.
        ;;  In particular, the ADD-METHOD call generated
        ;;  by the PCL method generation code only gets
        ;;  done at load time, and hence seperately defined
        ;;  methods are only returned by CLASS-DIRECT-METHODS
        ;;  after loading. The code below  will cause the 
        ;;  (CLTL1-EVAL-WHEN (LOAD) ...) top level forms returned 
        ;;  by the PCL method code generation to be overridden.


          ;;Inherited methods

          ,@(build-inherited-methods name inherited dont-define parents slots)

          ;;Gettables and settables

	  ,@(build-gs-methods name gettables settables dont-define parents slots)

          ',name

       ) ;progn


  ) ;end let
) ;end internal-define-type

;;fully-define-type-Fully define the CommonObjects type 

(defun fully-define-type (name slots parents keywords init-key-check)

  (let
    (
      (classprot (class-prototype (class-named 'common-objects-class)))
    )

    ;;Check for redefinition incompatibility, if any.

    (check-for-redefinition-incompatibility name parents slots)

    (add-named-class classprot
		     name
		     parents
		     slots
		     NIL
    )


    ;;Now set the slots for the initialization keywords and
    ;;  the check flag

    (setf classprot (class-named name))
    (setf (class-init-keywords classprot) keywords)      
    (setf (class-init-keywords-check classprot) init-key-check)

  ) ;let

) ;end fully-define-type

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Auxillary Type Definition Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;build-phony-type-info-Make a phony type info vector, to hold the
;;  information while the DEFINE-TYPE call is being parsed.

(defun build-phony-type-info (name)

;;Check if the name is OK first

  (unless (co-legal-type-or-method-name name)
    (co-deftype-error "legal type names must be symbols and NOT the symbol NIL."
      name
    )
  )

  ;;Set the name and origin slots and return

  (let
    (
      (phonytiv 
	(make-array 
	  $INFO-NUMBER-OF-SLOTS
	  :initial-element NIL
        )
      )
    )

    (setf (svref phonytiv $TYPE-NAME-SLOT) name)

    phonytiv

    ;;Note that we don't check for predefined type info's here
    ;;  because that should (eventually!) be handled by
    ;;  the CommonLoops kernel

  ) ;end let

) ;end build-phony-type-info

;;check-for-redefinition-incompatibility-Check to see if redefining
;;  will cause an incompatible change

(defun check-for-redefinition-incompatibility (name newparents newslots)

  (let*
    (
      (oldclass (class-named name T))
    )


    ;;If no class object, then this is new      

    (when oldclass

      ;;Check instance variable incompatibility

      (if (not (slots-compatible-p newslots (class-user-visible-slots oldclass)))
        (co-deftype-error
	    "please rename, since changing instance variables is incompatible.~%"
	    name
        )
      )

      ;;Check for parent incompatibility

      (if (not 
	    (slots-compatible-p 
	      newparents 
	      (class-local-super-names oldclass)
            )
          )
        (co-deftype-error
	    "please rename, since changing parents is incompatible.~%"
	    name
        )
      )

    ) ;when

  ) ;let

) ;end check-for-redefinition-incompatibility

;;slots-compatible-p-Check if the number and ordering
;;  of the slots in the old and new lists is the same

(defun slots-compatible-p (newslots oldslots)

  ;;Check that number of slots is the same

  (when (not (= (length oldslots) (length newslots)))
    (return-from slots-compatible-p NIL) 
  )

  ;;Check slot names
    
  (do
    (
      (ns newslots (cdr ns))
      (os oldslots (cdr os))
    )
    ( (or (null ns) (null os)) )

    (if (not (eq (car ns) (car os)))
      (return-from slots-compatible-p NIL)
    ) ;if
  ) ;do

  T
) ;end slots-compatible-p

;;merge-duplicates-Merge duplicates and check for conflicts
;;   in parents.

(defun merge-duplicates (name gettables settables parents dont-define)

  ;;Destructively modify gettables and settables
  ;;to get rid of duplicates

  (merge-methods gettables settables)

  ;;Check for funny business in inheritance

  (check-for-funny-inheritance name parents)

  ;;Check if any conflicts with parents and among parents

  (check-for-method-conflicts name gettables parents dont-define)

  NIL
) ;end merge-duplicates

;;merge-methods-Put settables on gettable list

(defun merge-methods (gettables settables)

  (dolist (meth settables)

    (when (not (member meth gettables :test #'equal))
      (setf (cdr (last gettables)) (list meth ) )
    )
  ) ;dolist

) ;end merge-methods

;;check-for-funny-inheritance-Check for attempts to inherit
;;  from yourself

(defun check-for-funny-inheritance (name parents)

  ;;Check me

  (dolist (p parents)

    ;; Check me

    (if (eq name (class-name (car p)))
      (co-deftype-error"this type has itself as an ancestor.~%" name)
    )

    ;;Check parent

    (check-for-funny-inheritance name (mapcar #'list (class-local-supers (car p))))
  )

) ;end check-for-funny-inheritance

;;check-for-method-conflicts-Merge gettable and parent lists and
;;  check for conflicts.

(defun check-for-method-conflicts (name gettables parents dont-define)

  (let
    (
      (kwp (find-package 'keyword))
      (meths NIL)
    )

    ;;Intern the gettable names in the keyword package

    (dolist (g gettables)
      (setf meths (cons (intern (symbol-name g) kwp) meths))
    ) ;dolist

    ;;Concatenate the parent methods onto the end

    (dolist (p parents)

      (setf meths 
	(concatenate 
	  'list 
	  meths 
	  (cdr p)
	)
      )

    ) ;dolist

    ;;Now check for duplicates

    (check-for-conflicts name meths dont-define)

  ) ;let

) ;end check-for-method-conflicts

;;check-for-conflicts-Check if any generated methods
;;  conflict

(defun check-for-conflicts (name list dont-define)

    (setf list (sort list #'(lambda (x y) (string-lessp (symbol-name x) (symbol-name y)))))

    (do*
      (
        (item (car list) (car clist))
        (clist (cdr list) (cdr clist))
      )
      ((eq clist NIL))

      ;;Check if a method already exists and isn't on the don't define
      ;;  list

      (if (and (equal item (car clist)) (not (member item dont-define)))
        (co-deftype-error
	  "two methods ~S exist during method generation.~%~
           Please undefine one or the other.~%"
	  name item
        )
      )
    ) ;do

) ;end check-for-conflicts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Top Level Method Building Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;build-inherited-methods-Build the list of inherited methods by using
;;  apply-method

(defun build-inherited-methods (name parents dont-define parent-names slots)

  (let
    (
      (methcode NIL)
    )

    ;;Do all the parents

    (dolist (p parents)

      ;;Do this parent's list

      (dolist (m (cdr p))

        ;;Check first to be sure it should be defined

        (if (not (member m dont-define))

	  (push
            (build-inherited-method 
              name 
              m 
              (class-name (car p)) 
              parent-names 
              slots
            )
            methcode
          )

        )

      ) ;dolist
    ) ;dolist

    methcode

  ) ;let

) ;build-inherited-methods

;;build-gs-methods-Build gettable and settable methods

(defun build-gs-methods (typename gettables settables dont-define parents slots)

  (let
    (
      (methcode NIL)
      (kwp (find-package 'keyword))
      (meth NIL)
    )

    ;;First do gettables

    (dolist (g gettables)

      (setf meth (intern (symbol-name g) kwp))

      ;;Check first to be sure it must be defined

      (if (not (member meth dont-define))

        (push  
          (build-get-method typename 
                            meth
                            g 
                            parents 
                            slots
           )
          methcode
        )
      )


    ) ;dolist

    ;;Now do settables

    (dolist (s settables)

      (setf meth 
            (intern (concatenate 'simple-string "SET-" (symbol-name s)) kwp)
      )

      ;;Check first to be sure it must be defined

      (if (not (member s dont-define))
        (push
	  (build-set-method 
	    typename 
            meth 
	    s
            parents
            slots
          )
	  methcode
        )
      )

    ) ;dolist

    methcode

  ) ;let
) ;end build-gs-methods  

;;build-init-vars-method-Return code for the :INITIALIZE-VARIABLES
;;  method. Note that this must be a fully-blown CommonObjects
;;  method, because the users can put anthing they want into
;;  the initialization code, including CALL-METHOD.

(defun build-init-vars-method (name initable-slots assignments)
  (let ((form NIL) 
        (kwpak (find-package 'keyword))
        (code NIL))
    ;;This code is stolen from DEFINE-METHOD and is
    ;;  inserted in line here so that, when it
    ;;  gets returned to the top level, COOL.PCL::EXPAND-DEFMETH-INTERNAL
    ;;  gets invoked while the DEFINE-TYPE macro is executing,
    ;;  rather than at the top level, when the macro has
    ;;  finished executing.
    (setf code
          `(compiler-let ((*current-method-class-name* ',name))   
             (let ((self (self-from-inner-self)))
               (declare (ignorable self))
               #|(declare (optimize (speed 3) (safety 0)))|#
               (with* ((.inner-self. "" ,name))
                 ,(if initable-slots
                      `(do* ((unprocessed-keys keylist (cddr unprocessed-keys))
                             (keyword (car unprocessed-keys)
                                      (car unprocessed-keys))
                             (value (cadr unprocessed-keys)
                                    (cadr unprocessed-keys)))
                            ((null unprocessed-keys))
                         (case keyword
                           ,@(dolist (var initable-slots form)
                               (push `((,(intern (symbol-name var) kwpak) ) 
                                       (setf ,var value))
                                     form)))))
                 ,@assignments))))
    ;;Now define as a full blown CommonObjects method, with code
    ;; walking and everything. Add in CALL-METHOD processing.
    `(progn
       ,(defcommon-objects-meth
            'keyword-standin::initialize-variables
            `((.inner-self. ,name) &rest keylist)
          code
          '((declare (ignorable keylist)))
          ))))

;;build-pcl-method-def-Build a PCL method definition without
;; all the overhead of code walking and method object creation
;; at compile time

(defun build-pcl-method-def (type method func-args code)
  (setf method
        (if (keywordp method)
            (keyword-standin method)
            method))
  (let* ((type-spec (list type))
         (method-function-name (cool.pcl::make-method-name method type-spec)))
    ;;The extra list is so the forms get inserted at the
    ;;  top level OK
   `((cltl1-eval-when (compile load eval)
       (cool.pcl::record-definition 
        ',method 'cool.pcl::method ',type-spec NIL)
       (defun ,method-function-name ,func-args
         #|(declare (optimize (speed 3) (safety 0)))|#
	,code))
     ;;Note that this must be done at compile time
     ;;  as well, since inherited methods must
     ;;  be there for other types in the file
     (cltl1-eval-when (compile load eval)
       (let ((method (cool.pcl::load-method-1
                      'cool.pcl::discriminator
                      'common-objects-method
                      ',method
                      ',type-spec
                      ',func-args
                      NIL))) 
         (setf (method-function method)
               (symbol-function ',method-function-name))
         (add-method (discriminator-named ',method) method NIL))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Get/Set and Inherited Method Building Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;build-get-method-Build a gettable method

(defun build-get-method (name methname var parents slots)

  `(progn
    ,@(build-pcl-method-def 
      name 
      methname 
      '(.inner-self.) 
      `(%instance-ref .inner-self. ,(calculate-slot-index var parents slots))
    )
  )

) ;end build-get-method

;;build-set-method-Build a settable method

(defun build-set-method (name methname var parents slots)

  `(progn
    ,@(build-pcl-method-def 
      name 
      methname
      '(.inner-self. .new-value.)
      `(setf 
        (%instance-ref .inner-self. ,(calculate-slot-index var parents slots))
        .new-value.
       )
    )
  )

) ;end build-set-method

;;build-inherited-method-Return code for an inherited method.

(defun build-inherited-method (name m p parents slots)

  ;;Now generate code

  `(progn
    ,@(build-pcl-method-def
        name
        m
        '(.inner-self. &rest .arg-list.)
        `(apply
	    (symbol-function 
              ',(generate-method-function-symbol
	           p m
                )
	    )
            (%instance-ref
	      .inner-self.
	      ,(calculate-slot-index 
	        p
	        parents
                slots
              )
           )
	   .arg-list.

         )
      )

  )

) ;end build-inherited-method

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Default Universal Methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;define-universal-method-Macro to define universal methods. Note that
;;  DEFCOMMON-OBJECTS-METH could probably be used directly, but this
;;  tells what we're doing. We need a CommonObjects method here because
;;  we may need a symbol for CALL-METHOD

(defmacro define-universal-method (name arglist &body body)

   ;;Check for undefined type in body

    (setf body 
      `(progn 
        (if (eq (class-name (class-of ,(first (first arglist)))) 
                $UNDEFINED-TYPE-NAME
            )
             (no-matching-method (discriminator-named ',name))
        )
        ,@body
      )
    )

  (defcommon-objects-meth name arglist body)

 ) ;define-universal-method

;;keyword-standin::init-Default :INIT method does nothing

(define-universal-method keyword-standin::init 
  ((self common-objects-class) &rest keylist))
 
;;keyword-standin::initialize-Default :INITIALIZE initializes
;;  parents, then variables

(define-universal-method keyword-standin::initialize 
  ((self common-objects-class) &rest keylist)

    (let
      (
        (class (class-of self))
      )

      (dolist (l (class-local-super-slot-names class))

        ;;GET-SLOT is inserted in-line here

        (apply 'keyword-standin::initialize 
          (%instance-ref self (slot-index class l))
          keylist
        )
      )

      ;;Now initialize variables

      (apply 'keyword-standin::initialize-variables self (car keylist))
      (apply 'keyword-standin::init self (car keylist))

  ) ;let

) ;keyword-standin::initialize

;;print-instance-Print the instance

(define-universal-method print-instance
  ((self common-objects-class) output-stream integer)

  (if (or (not integer) 
          (not *print-level*) 
          (< integer *print-level*)
      )

      (cool.pcl::printing-random-thing (self output-stream)
	(format output-stream  "~A" (class-name (class-of self)))
      )
              
  )

) ;print-instance

;;keyword-standin::describe-Default :DESCRIBE method

(define-universal-method keyword-standin::describe 
  ((self common-objects-class) &optional describe-inner-loop)

  (let
    (
      (class (class-of self))
    )

    (when (equal 
            (class-name (class-of class))
            'common-objects-class
           )

      ;;Give name of this guy

      (if (not describe-inner-loop)
        (format T 
	        "This object of type ~A has variables:~%" 
	        (class-name (class-of self))
        )
        (format T 
                "For parent ~A:~%"
	        (class-name (class-of self))
        )
      ) ;if

      ;;Now print instance variables

      (dolist (slot (class-user-visible-slots class))
        (format T "    ~A: ~S~%" slot (get-slot-using-class class self slot))
      )

      ;;Now print for parents

      (dolist (lss (class-local-super-slot-names class))
        (keyword-standin::describe (get-slot-using-class class self lss) T)
      )

    ) ;when

  ) ;let

) ;keyword-standin::describe

;;keyword-standin::eql-Default :EQL predicate method

(define-universal-method keyword-standin::eql 
  ((self common-objects-class) .any.)

      (eq self .any.)

) ;keyword-standin::eql

;;keyword-standin::equal-Default :EQUAL predicate method
                                             
(define-universal-method keyword-standin::equal 
  ((self common-objects-class) .any.)

   (keyword-standin::eql self .any.)

) ;keyword-standin::equal

;;keyword-standin::equalp-Default :EQUALP predicate method

(define-universal-method keyword-standin::equalp 
  ((self common-objects-class) .any.)

  (keyword-standin::equal self .any.)

) ;keyword-standin::equalp

;;keyword-standin::typep-Default :TYPEP predicate method

(define-universal-method keyword-standin::typep 
  ((self common-objects-class) .any.)

  (or (equal (class-name (class-of self)) .any.)
      (eq .any. 'instance)
      (eq .any. 't)
  )

) ;keyword-standin::typep

;;keyword-standin::copy-Default :COPY method 

(define-universal-method keyword-standin::copy 
  ((self common-objects-class))

      self

) ;keyword-standin::copy

;;keyword-standin::copy-instance-Default :COPY-INSTANCE method

(define-universal-method keyword-standin::copy-instance 
  ((self common-objects-class))

  (let
    (
      (class (class-of self))
      (inst NIL)
    )

    (when (equal 
            (class-name (class-of class))
            'common-objects-class
           )

      (setf inst (make-instance (class-name class)))

      ;Copy state from inner-self to instance

      (co::set-slot-values self inst class)

      inst
   ) ;when

  ) ;let

) ;keyword-standin::copy-instance

;;keyword-standin::copy-state-Default :COPY-STATE method

(define-universal-method keyword-standin::copy-state 
  ((self common-objects-class))

      self

) ;keyword-standin::copy-state

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Support Methods and Functions for Universal Methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;set-slot-values-Set the slot values in OBJECT to those in .INNER-SELF.

(defmeth set-slot-values (.inner-self. object class)

  ;;Set in this guy

  (dolist (slot (class-user-visible-slots class))
    (setf (get-slot object slot) (get-slot .inner-self. slot))
  )

  ;;Now set in parents

  (dolist (lss (class-local-super-slot-names class))
      (set-slot-values 
	(get-slot .inner-self. lss) 
	(get-slot object lss) 
	(class-of (get-slot .inner-self. lss))
      )
  )

) ;end set-slot-values

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Renaming and Undefining Types and Methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;rename-type-Rename type1 to type2

(defun rename-type (type1 type2)
  (declare (type symbol type1 type2))

  (let
    (
      (class (class-named type1 T))
      (newclass (class-named type2 T))
    )

    ;;Signal an error for special cases

    (when (or (null type2) (eq type2 't))
      (error "RENAME-TYPE: New name cannot be NIL or T.~%")
    )

    ;;Signal an error when arguments aren't symbols

    (when (or (not (symbolp type1)) (not (symbolp type2)))
      (error "RENAME-TYPE: Arguments must be symbols.~%")
    )

    ;;Signal error if TYPE2 already exists

    (when newclass
      (error "RENAME-TYPE: Type ~S already exists.~%" type2)
    )

    ;;Signal an error if class isn't CommonObjects class

    (when (not (eq (class-name (class-of class)) 'common-objects-class))
      (error "RENAME-TYPE: Can't rename a built-in type or nonCommonObjects class ~S.~%" type1)
    )

    ;;Signal an error if the class is not defined

    (if class
      (progn
	(rename-class class type2)
        type2

      ) ;progn
      (error "RENAME-TYPE: The type ~S is not defined.~%" type1)
    ) ;if

  ) ;let

) ;end rename-type

;;undefine-type-Undefine type typename

(defun undefine-type (typename)
  #|(declare (type symbol typename))|#

  ;;Check if typename is a symbol

  (when (not (symbolp typename))
    (error "UNDEFINE-TYPE: Argument must be a symbol.~%")
  )

  (let
    (
     (class (class-named typename T))
    )

    (if (and class (eq (class-name (class-of class)) 'common-objects-class))
     (progn

        ;;Undefine all the methods first

        (undefine-methods class)

        ;;Now set the class name

        (setf (class-name class) $UNDEFINED-TYPE-NAME)
	(setf (class-named typename) NIL)
        T
      ) ;progn

      NIL

    ) ;if

  ) ;let

) ;end undefine-type

;;undefine-methods-Undefine all the methods on class

(defun undefine-methods (class)

  (dolist (meth (class-direct-methods class))

    ;;Remove the method from the discriminator

    (remove-method (method-discriminator meth) meth)
 
    ;;Now unbind the symbol cell, so call-methods don't work

    (fmakunbound (method-function-symbol meth))
  )

) ;undefine-methods

;;undefine-method-Use PCL remove-method to get
;;  rid of method.

(defun undefine-method (typename operation)
  #|(declare (type symbol typename operation))|#

  ;;Check if the arguments are symbols
  (when (not (symbolp typename)) 
    (error "UNDEFINE-METHOD: Type name must be a symbol.~%"))

  ;;If the operation is not a symbol, just return.
  (when (not (symbolp operation))
    (return-from undefine-method NIL))

  (let* (;;The class object
         (class (class-named typename))
         ;;The operation
         (opname (if (keywordp operation)
                     (keyword-standin operation)
                     operation))
         ;;The discriminator (if any)
         (disc (discriminator-named opname))
         ;;The method (if any)
         (meth (if disc
                   (find-method disc (list typename) NIL T))))

    ;;Check if the class is a CommonObjects class
    (when (not (eq (class-name (class-of class)) 'common-objects-class))
      (error "UNDEFINE-TYPE: Tried to undefine ~S ~  
              which is not a CommonObjects class.~%"
              typename))

    ;;Check if the method is a universal method and there
    ;; is no type specific method. Warn the user.
    (when (and (null meth) 
               (member operation *universal-methods* :test #'eq))
      (warn "UNDEFINE-TYPod"))

    (let* (;;The class ob% which cannot be undefined.
           typename
           operation)
      ;; ********* lost?? *********
      ;; https://groups.google.com/forum/?fromgroups=#!searchin/comp.sources.unix/co-dtype/comp.sources.unix/T_FDe2f3nQI/tORs8-xlUKsJ
      ;; **************************
      (when (and (null meth)
                 (null disc))
        (return-from undefine-method NIL))
      
      ;;If a method was found, undefine it
      (if (and meth disc)
          (progn
            (remove-method disc meth)
            
            ;;Now unbind the symbol cell, so CALL-METHODs don't work
            (fmakunbound (method-function-symbol meth))
            
            ;;Remove the symbol from the package, so that future
            ;;  attempts to create CALL-METHODs can't find it.
            ;;  But hopefully, existing CALL-METHODs will still
            ;;  work.
            (unintern (method-function-symbol meth) 
                      (symbol-package (method-function-symbol meth)))
            T)
        NIL))))

;;assignedp-Indicate whether or not an instance variable is
;;  assigned

(defmacro assignedp (var)

  (declare (special co::*current-method-class-name*))

  ;;Check for attempt to access outside of a method

  (if (null (boundp 'co::*current-method-class-name*))
    (error "DEFINE-METHOD: Attempt to use assignedp outside of a method.~%")
  )

  ;;Check for attempt to use on something other than an instance variable

  (unless (has-slot-p (class-named *current-method-class-name*) var)
    (error "DEFINE-METHOD: Argument ~S to assignedp ~
           must be an instance variable name.~%" 
           var
     )
  )

  `(not (equal ,var ',$UNINITIALIZED-VARIABLE-FLAG))
    
) ;;end assignedp

;;instancep-Return T if this thing is an instance and has a CommonObjects
;;  class

(defun instancep (thing)

  ;;Check first if thing is NIL

  (if (not thing)
    NIL
    (eq (class-name (class-of (class-of thing))) 'common-objects-class)
  )


) ;end instancep

;;supports-operation-p-Return T if method operation METH is supported on type
;;  of OBJ

(defun supports-operation-p (obj meth)
  (declare (special *universal-methods*))

  (let
    (
      (class (if obj (class-of obj) obj))
    )

    ;;If not a CommonObjects class, then return NIL

    (when (or (not class) 
              (not (eq (class-name (class-of class)) 'common-objects-class))
          )
      (return-from supports-operation-p NIL)
    )

    ;;Check first if its a universal method

    (if (member meth *universal-methods*)

      T

      ;;Otherwise, check in the class object if it's got them

      (dolist (methobj (class-direct-methods class))

        (when (eq (unkeyword-standin (method-name methobj)) meth)
          (return-from supports-operation-p T)
        )

      ) ;dolist

    ) ;if

  ) ;let

) ;end supports-operation-p

;;Define the instance type

(deftype instance ()
  (list 'apply 'instancep)

) ;end deftype



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Make-Instance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;make-instance-Make an instance given the CommonObjects type name

(defmeth make-instance ((class-name symbol) &rest keylist)

    ;;Check if the key list and class are OK.

    (if (null (listp keylist))
      (error "Make-instance requires a list for the keyword list.~%")
    )

    (if (null (class-named class-name T))
      (error "~S is not a defined type.~%" class-name)
    )

   (make-instance (class-named class-name) keylist)

) ;end make-instance

;;make-instance-Make an instance given the CommonObjects class object

(defmeth make-instance ((class common-objects-class) &rest keylist)
  (declare (special *outer-self*))
  
  (let*
    (
      (instance NIL)
      (numslots (length (class-user-visible-slots class)))
      (start-slots 
	(+ $START-OF-PARENTS (length (class-local-supers class)))
      )
    )
      (let 
	(
          (*outer-self* (and (boundp '*outer-self*) *outer-self*))
        )
        (declare (special *outer-self*))

        (setf instance (%make-instance (class-of class)
				       (+ 2 (class-instance-size class))
                       )
        )
        (setf (%instance-ref instance $CLASS-OBJECT-INDEX) class
	      (%instance-ref instance $SELF-INDEX) (or *outer-self*
					                (setq *outer-self* instance)
						    )
        )

        ;;Initialize the slots with the uninitialized flag

        (dotimes (i numslots)
          (setf 
	    (%instance-ref instance (+ i start-slots))
            $UNINITIALIZED-VARIABLE-FLAG
          )
        )

        ;;Now go through and make parent objects

        (do 
          (
            (supers (class-local-supers class) (cdr supers))
	    (index $START-OF-PARENTS (1+ index))
          )
	  ((null supers))
	  (setf (%instance-ref instance index)
	        (make-instance (car supers) (car keylist))
          )
        ) ;do

    ) ;end let for dynamic binding

    ;;Check initialization keywords and initialize, but only if
    ;;  creating outer self object.

    (when (not (boundp '*outer-self*))

      ;;If keyword check needed, then check keyword list

      (if (class-init-keywords-check class)
        (check-init-keywords class keylist)
      )
      ;;Now initialize, if doing outer self.

      (keyword-standin::initialize instance (car keylist))

    ) ;when

    instance

  ) ;end let for lexical binding

) ;end make-instance

