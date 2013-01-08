
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         co-test.l
; RCS:          $Revision: 1.1 $
; SCCS:         %A% %G% %U%
; Description:  Portable Test Macro for Testing COOL
; Author:       James Kempf, HP/DCC
; Created:      24-Feb-87
; Modified:     25-Feb-87 08:45:43 (James Kempf)
; Language:     Lisp
; Package:      PCL
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

;;;-*- Mode:LISP; Package: PCL; Base:10; Syntax:Common-lisp -*-
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
;;; Testing code. Note: This file is derived from the PCL file test.l and
;;; removes some of the PCL specific stuff from the test macro.

(in-package :cool.pcl)
;; (use-package 'lisp)

#|(require "pcl")|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;		Catching Errors
;
; Since CLtL defines no portable way of catching errors, most system
; implementors have done their own. Certainly it would be possible
; to code a portable error catcher, but the complexity involved
; (catching errors at macroexpand time as well, etc.) is considerable.
; As a stopgap, the *WITHOUT-ERRORS* special is provided for people
; bringing up COOL on a new system to add their system's special error
; catching code. It is taken from the original PCL test file.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Other info needed for exception handling

#+HP (require "exception")

(defvar *without-errors*
	(or #+Symbolics #'(lambda (form)
			    `(multiple-value-bind (.values. .errorp.)
				 (si::errset ,form nil)
			       (declare (ignore .values.))
			       .errorp.))
	    #+Xerox     #'(lambda (form)
			    `(xcl:condition-case (progn ,form nil)
			       (error () t)))
	    
            #+HP	#'(lambda (form)
			    `(extn:when-error 
			       (progn ,form NIL)
			       T
			    )
			)
	    nil
        )

) ;defvar

;;without-errors-This macro generates code for error testing

(defmacro without-errors (&body body)

    (if *without-errors*
      (funcall *without-errors* `(progn ,@body))
      (error "Calling WITHOUT-ERRORS when *without-errors* is nil.")
    )


) ;without-errors

;;with-return-value-Set up each form in BODY to match return value

(defmacro with-return-value (form return-value)

  ;;Note the use of full qualification on EQUALP
  ;;  to avoid problems with redefinition from CO

  `(cl::equalp ',return-value ,form)

) ;with-return-value

;;do-test-Execute BODY according to the options list

(defmacro do-test (name&options &body body)
  (let 
    (
      (name (if (listp name&options) (car name&options) name&options))
      (options (if (listp name&options) (cdr name&options) ()))
      (code NIL)
    )

    ;;Bind the options from keywords
  
    (keyword-bind 
      (
        (should-error nil)
        (return-value nil)
      )

      options
    
      ;;Check if errors should be caught and can be

      (cond 

        ;;Errors can't be caught in this Lisp, so don't do it

        (
          (and should-error (null *without-errors*))
	  `(format t
	    "~&Skipping testing ~A,~%~
	     because can't ignore errors in this Common Lisp."
	     ',name
          )
        )

        ;;Generate code for test. If the return value was supplied
        ;;  as an option, check if the return values are the same.
        ;;  Note the use of LISP::EQUALP. This is because CommonObjects
        ;;  redefines EQUALP.

        (t
	  `(progn
	    (format t "~&Testing ")
	    (format t ,name)
	    (format t "... ")
            ,@(dolist (form  body (reverse code))
                (push
                  `(if
		     ,(cond
			(
			  should-error
			  `(without-errors ,form)
                        )
                        (
                          return-value
                          `(with-return-value ,@form)
                        )
                        (
                         T
                         `(progn ,form)
                        )
                     )
                     (format T "~&OK: ~S~%" ',form)
		     (format T "~&FAILED: ~S~%" ',form)
                  )
                  code

               ) ;push
            ) ;dolist

          ) ;progn
        )
      ) ;cond

    ) ;keyword-bind

  ) ;let

) ;do-test

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide "co-test")

