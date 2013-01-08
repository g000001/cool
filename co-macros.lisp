;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         co-macros.l
; RCS:          $Revision: 1.1 $
; SCCS:         %A% %G% %U%
; Description:  Macros used by Interface For CommonObjects
;               with co parser in CL.
; Author:       James Kempf, HP/DCC
; Created:      31-Jul-86
; Modified:     11-Mar-87 22:22:44 (James Kempf)
; Language:     Lisp
; Package:      COMMON-OBJECTS
; Status:       Distribution
;
; (c) Copyright 1987, HP Labs, all rights reserved.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Preliminaries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;The CommonObjects interface is in the COMMON-OBJECTS package. We need
;;;  both PCL and the CommonObjects parser, which is in the 
;;   COMMON-OBJECTS-PARSER package. Note that PCL is assumed to be
;;   loaded.

#|(provide "co-macros")|#

(in-package :common-objects)

;
;Export these symbols. They are the only ones which clients should see.
;;Need PCL and patches

;;(require "pcl")
;; (require "pcl-patches")

;;Need the parser

;; (require "co-parse")

;;Use the parser's package

;; (use-package 'co-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Constant Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Need this flag to indicate that an instance variable is uninitialized.

(defconstant* $UNINITIALIZED-VARIABLE-FLAG '::*UNDEFINED*)

;;Offsets of important things in instances.
;;Location of class object.

(defconstant* $CLASS-OBJECT-INDEX 0)

;;Location of pointer to self.

(defconstant* $SELF-INDEX 1)

;;Starting index of parents.

(defconstant* $START-OF-PARENTS 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Special Variable Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;*special-functions-list*-Holds a list of uninterned symbols for TYPE-OF,
;;  TYPEP, EQL, EQUAL, and EQUALP. These symbols have their function cells
;;  bound to special functions which use CommonObjects messaging if the
;;  argument is a CommonObjects object.

(defvar *special-functions-list*
  (list
    (cons ':type-of (make-symbol "TYPE-OF"))
    (cons ':typep (make-symbol "TYPEP"))
    (cons ':eql (make-symbol "EQL"))
    (cons ':equal (make-symbol "EQUAL"))
    (cons ':equalp (make-symbol "EQUALP"))
  )
)

;;*universal-methods*-List of universal methods

(defvar *universal-methods*
  '(
    :init
    :initialize
    :print
    :describe
    :eql
    :equal
    :equalp
    :typep
    :copy
    :copy-instance
    :copy-state 
  )
)

;;*universal-method-selectors*-List of selectors for universal
;;  methods

(defvar *universal-method-selectors* NIL)

;;*keyword-standin-package*-Package for interning methods as functions.
;;  CommonObjects "encourages" the use of keywords as method names,
;;  but not all CL's allow keyword symbol function cells to be
;;  occupied.

(cltl1-eval-when (compile load eval)
  (defvar *keyword-standin-package* 
    (or (find-package 'keyword-standin) (make-package 'keyword-standin))
  )
)

;;;Unuse the lisp package in the keyword-standin package, to
;;;  avoid conflicts with named functions.

;; (unuse-package 'lisp *keyword-standin-package*)

;;*special-method-symbols*-List of special method symbols which 
;;  shouldn't go into the keyword-standin package, paired with
;;  their method names.

(defvar *special-method-symbols* 
  (list
      (cons ':print 'print-instance)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;	Support for Using Keywords as Method Names
;
;  These macros and functions translate keyword method names into
;  names in a package. Some Common Lisps do allow keyword symbols
;  to have an associated function, others don't. Rather than
;  differentiating, a single package, KEYWORD-STANDIN, is used
;  for method symbols which are keywords.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;special-keyword-p-Return T if the keyword is a special method
;;  symbol.

(defmacro special-keyword-p (keyword)
  `(assoc ,keyword *special-method-symbols* :test #'eq)

) ;end special-keyword-p

;;keyword-standin-special-Return the special symbol for this
;;  keyword.

(defmacro keyword-standin-special (keyword)
  `(cdr (assoc ,keyword *special-method-symbols* :test #'eq))

) ;end keyword-standin-special

;;special-method-p-Return T if the symbol is a special method
;;  symbol.

(defmacro special-method-p (symbol)
  `(rassoc ,symbol *special-method-symbols* :test #'eq)

) ;end special-method-p

;;unkeyword-standin-special-Return the keyword for this
;;  special method

(defmacro unkeyword-standin-special (symbol)
  `(car (rassoc ,symbol *special-method-symbols* :test #'eq))

) ;end unkeyword-standin-special

;;keyword-standin-Get a standin symbol for a keyword

;;; end of co-macros.l ;;;;;

