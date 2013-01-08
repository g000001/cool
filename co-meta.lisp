;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         co-meta.l
; RCS:          $Revision: 1.1 $
; SCCS:         %A% %G% %U%
; Description:  Metaclass for CommonObjects
; Author:       James Kempf
; Created:      March 10, 1987
; Modified:     March 10, 1987  13:30:58 (Roy D'Souza)
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
;	CommonObjects Class Ndefstruct
;
;  Instances are represented as trees of their parent instances just like
;  in the original CommonObjects implementation except that we do not make
;  make the single inheritance optimization of in-lining the first parent.
;  The first slot of every instance is the class object.
;  The second slot of every instance is named .SELF. and is a pointer to
;  the acutal object. Then come slots for each of the parent class instances,
;  then the slots for this class.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ndefstruct (common-objects-class
	      (:class class)
	      (:include (essential-class))	
	      (:conc-name class-)
            )

  (instance-size 1)             ;The total number of slots every instance
				;of this class must have.  This includes
				;one slot for the pointer to outer self and
				;one slot for each of the parent instances.

  (local-super-slot-names ())   ;A list of the names of the slots used to
				;store the parent instances.  This list
				;exactly parallels the local-supers as
				;stored in class-local-supers.

  (slots ())			;The slots required by CommonLoops.

  (user-visible-slots ())	;Instance variable names.

  (children ())			;Children of this guy. Not currently used.

  (init-keywords                ;Initialization keywords
    () 
  )		
  (init-keywords-check T)       ;Whether to check the initialization 
				;keywords
) ;end ndefstruct for common-objects-class

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Establishment of the CommonObjects MetaClass
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cltl1-eval-when (load)
 (define-meta-class common-objects-class 
   (lambda (x) (%instance-ref x $CLASS-OBJECT-INDEX))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  CommonObjects MetaClass Protocol  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;add-class-Add a CommonObjects class. Part of the metaclass protocol.

(defmeth add-class ((class common-objects-class)
		    new-local-supers
		    new-local-slots
		    extra
                   )

  (let 
    ( 
      (local-super-slot-names
	  (mapcar #'(lambda (nls) (local-super-slot-name (class-name nls)))
		  new-local-supers
          )
       )
     )

    (setf (class-local-super-slot-names class) local-super-slot-names)

    (setf (class-user-visible-slots class) new-local-slots)

    (setq new-local-slots 
          (mapcar #'(lambda (x) (make-slotd class :name x))
					(append local-super-slot-names
						new-local-slots)
          )
    )

    (setf (class-instance-size class) (length new-local-slots))

    (run-super)

  ) ;let

) ;end add-class

;;class-slots-Return the slot names for the parents

(defmeth class-slots ((class common-objects-class))

  (class-local-slots class)

) ;end class-slots

;;has-slot-p-Return T if class has user visible slot symbol

(defmeth has-slot-p ((class common-objects-class) symbol)

  (let
    (
      (bool NIL)
    )

    (dolist (slotd (class-user-visible-slots class))
      (when  (equal symbol (slot-name-from-slotd slotd))
	(setf bool T)
        (return)
      )
    )
    bool

  ) ;end let

) ;end has-slot-p

;;init-keywords-Return the initialization keywords

(defmeth init-keywords ((class common-objects-class))

  (class-init-keywords class)

) ;init-keywords

;;class-local-super-names-Return the names of the local supers for
;;  this class.

(defmeth class-local-super-names ((class common-objects-class))

  (mapcar #'(lambda (x) (class-name x)) (class-local-supers class))

) ;end class-local-super-names

;;compute-class-precedence-list-Calculate class precedence.
;;  CommonObjects classes don't inherit in the CommonLoops sense.  
;;  Tell CommonLoops that they only inherit from themselves, 
;;  the class COMMON-OBJECTS-CLASS itself which they need for 
;;  GET-SLOT-USING-CLASS and PUT-SLOT-USING-CLASS and default printing
;;  to work right.

(defmeth compute-class-precedence-list ((class common-objects-class))

  (list class (class-named 'common-objects-class) (class-named 'object))

) ;end compute-class-precedence-list

;;method-alist-Return the a-list of names v.s. method objects. Only
;;  methods which are CommonObjects methods are returned. This
;;  is to accomodate system generated methods, like TYPE-OF, which
;;  should not be identified as methods on CommonObjects instances.
;;  This routine is primarily used in parsing.

(defmeth method-alist ((class common-objects-class))
  (declare (special *universal-methods*))

  (let
    (
      (alist NIL)
    )

    ;;First get the direct methods

    (dolist (methobj (class-direct-methods class))

        (if (eq (class-name (class-of methobj)) 'common-objects-method)

          (push 
	    (list (unkeyword-standin (method-name methobj)) methobj)
	    alist
	  )
        ) ;if
    )

    ;;Now check if any of the universal methods need to be added

    (dolist (univmeth *universal-methods*)

      (if (not (assoc univmeth alist))
        (push
          (list 
	    univmeth 
	    (find-method 
	      (discriminator-named (keyword-standin univmeth))
              '(common-objects-class)
	      NIL
	      T
            )
          )
          alist
        )

      ) ;if

    ) ;dolist            

    alist

  ) ;end let

) ;end method-alist

;;check-init-keywords-Check if the initialization keywords are
;;  correct

(defmeth check-init-keywords ((class common-objects-class) keylist)

  (let
    (
      (legalkeys (class-init-keywords class))
    )
    
    (do
      (
        (key (car keylist) (cddr key) )
      )
      ( (null key) )

      (if (not (and (keywordp (car key)) (>= (length key) 2)))
        (error "MAKE-INSTANCE: For type ~S, keylist must have alternating keys and values. List:~S~%"
		 (class-name class) (car keylist)
        )
      )

      (when (not (member (car key) legalkeys))
        (error "MAKE-INSTANCE: For type ~S, ~S is not a legal initialization keyword.~%"
		 (class-name class) (car key)
        )
      )
    ) ;dolist

  ) ;let

) ;end check-init-keywords

;;optimize-get-slot-Optimize a get slot by returning
;;  the right code. CommonObjects instances are statically
;;  allocated, so "hard" indicies can be used for them.
;;  Stolen from the protocol for BASIC-CLASS.

;(defmeth optimize-get-slot ((method common-objects-method)
;			         (class common-objects-class)
;			         form)
;  (declare (ignore method)) ; rds 3/9
(defmeth optimize-get-slot ((class common-objects-class) form)
    `(%instance-ref ,(second form) ,(slot-index class (second (third form))))



) ;end optimize-get-slot

;;pcl::optimize-setf-of-get-slot-Optimize a setf of a slot
;;  by returning the right code. Again, "hard" indicies
;;  can be used since in-line allocation is the rule.
;;  Stolen from the protocol for BASIC-CLASS.

;(defmeth pcl::optimize-setf-of-get-slot ((method common-objects-method)
;				         (class common-objects-class)
;				         form)
;  (declare (ignore method))
(defmeth cool.pcl::optimize-setf-of-get-slot ((class common-objects-class)
                                         form)
    `(setf 
      (%instance-ref , (nth 1 form) ,(slot-index class (second (nth 2 form))))
           ,(nth 3 form)
     )

) ;end optimize-setf-of-get-slot

;;slot-index-Calculate the slot index for the indicated slot

(defmeth slot-index ((class common-objects-class) slotname)

  ;;Treat .SELF. as a special case

  (if (eq slotname '.self.)
    $SELF-INDEX

    (calculate-slot-index 
      slotname
      (class-local-super-slot-names class) 
      (class-user-visible-slots class)
    )

  ) ;if

) ;end slot-index

;;get-slot-using-class-Generic version for all CommonObjects classes.
;;  Normally, this will be optimized out by the optimization method
;;  but just in case.

(defmeth get-slot-using-class ((class common-objects-class) object slot-name)

  (%instance-ref object (slot-index class slot-name))

) ;get-slot-using-class 

;;put-slot-using-class-Generic version for all CommonObjects classes.
;;  A bug in the default code-walker makes this necessary, although
;;  ultimately a custom walking function for CommonObjects methods
;;  might make the optimization work. Note that the code walker
;;  bug is fixed in the specialized walker method WALK-METHOD-BODY-INTERNAL
;;  for CommonObjects methods.

(defmeth cool.pcl::put-slot-using-class  
  ((class common-objects-class) object slot-name new-value)

  (setf 
    (%instance-ref object (slot-index class slot-name) )
    new-value
  )
  
) ;put-slot-using-class


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  CommonObjects MetaClass Utility Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;defined-classes-List the defined CommonObjects classes

(defun defined-classes ()

  (let 
    (
      (defined-types NIL)
      (class (class-named 'common-objects-class))
    )

    (maphash 
	#'(lambda (key val) 
	    (when (and val (eq (class-of val) class))
	      (setf defined-types (cons key defined-types))
            )
	  )
          cool.pcl::*class-name-hash-table*
    )
    defined-types
  )
) ;end defined-classes

;;slot-name-from-slotd-Return the name of the slot, given the SLOTD.

(defun slot-name-from-slotd (slotd)
  slotd

) ;slot-name-from-slotd

;;method-name-Return the method name, given the method object

(defun method-name (methobj)

  (discriminator-name (method-discriminator methobj))
)

