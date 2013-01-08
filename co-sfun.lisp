
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         co-sfun.l
; RCS:          $Revision: 1.1 $
; SCCS:         %A% %G% %U%
; Description:  Override System Functions
; Author:       James Kempf
; Created:      March 10, 1987
; Modified:     March 10, 1987  13:31:39 (Roy D'Souza)
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
;	Overridden System Functions
;
;  The semantics of CommonObjects requires that the Lisp functions EQL, EQUAL,
;  EQUALP, and TYPEP go through the corresponding universial methods rather
;  than having their default behavior, and that TYPE-OF return the CommonObjects
;  type. To avoid circularity problems, these functions are defined as
;  special, non-interned symbols, and are SHADOWING-IMPORTED into the
;  package by the user if this behavior is desired. Note, however,
;  that the default Lisp symbols can't be specialized because otherwise
;  circularity problems in PCL functions like CLASS-OF may occur. An application
;  wanting to use them must call the function IMPORT-SPECIALIZED-FUNCTIONS
;  (below) to get access.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cltl1-eval-when (load eval)

  (progn

   ;;For TYPE-OF

    (setf 
      (symbol-function 
        (cdr (assoc ':type-of *special-functions-list* :test #'eq))
      )
      (function (lambda (object) (class-name (class-of object))))

    ) ;setf

   ;;For TYPEP

    (setf 
     (symbol-function 
        (cdr (assoc ':typep *special-functions-list* :test #'eq))
      )
      (function
        (lambda (object type) 
          (cond

            ;;Object is a CommonObjects instance

            ( 
              (instancep object)
	      (keyword-standin::typep object type)
            )

            ;;Type is a CommonObjects type

            (
              (member type (defined-classes))
              NIL
            )

            ;;Default

            (
              T        
              (ignore-errors (cl::typep object type))
            )

          ) ;cond 
        )
      )
    ) ;setf

   ;;For EQL

    (setf 
      (symbol-function 
        (cdr (assoc ':eql *special-functions-list* :test #'eq))
      )
      (function
        (lambda (object1 object2) 
          (if (instancep object1)
	    (keyword-standin::eql object1 object2)
            (cl:eql object1 object2)
          )
        )
      )
    ) ;setf

   ;;For EQUAL

    (setf 
      (symbol-function 
        (cdr (assoc ':equal *special-functions-list* :test #'eq))
      )
      (function
        (lambda (object1 object2) 
          (if (instancep object1)
	    (keyword-standin::equal object1 object2)
            (cl::equal object1 object2)
          )
        )
      )
    ) ;setf

   ;;For EQUALP

    (setf 
      (symbol-function 
        (cdr (assoc ':equalp *special-functions-list* :test #'eq))
      )
      (function
        (lambda (object1 object2) 
          (if (instancep object1)
	    (keyword-standin::equalp object1 object2)
            (cl::equalp object1 object2)
          )
        )
      )
    ) ;setf

  ) ;progn

) ;cltl1-eval-when

;;import-specialized-functions-Import the specialized functions into
;;  the current package. This will override the Lisp package 
;;  symbols.

(defmacro import-specialized-functions ()

  (let
    ( (import-list NIL) )

    `(shadowing-import
      ',(dolist (p *special-functions-list* import-list)
         (push (cdr p) import-list)
       )

      )
    )

) ;end import-specialized-functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



