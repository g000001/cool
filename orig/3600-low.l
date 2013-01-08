;;; -*- Mode:LISP; Package:(PCL Lisp 1000); Base:10.; Syntax:Common-lisp; Patch-File: Yes -*-
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
;;; This is the 3600 version of the file portable-low.
;;;

(in-package 'pcl)

(defmacro without-interrupts (&body body)
  `(zl:without-interrupts ,.body))

  ;;   
;;;;;; Load Time Constants
  ;;
;;;
;;; This implementation of load-time-eval exploits the perhaps questionable
;;; feature that it is possible to define optimizers on macros.
;;; 
;;;   WHEN                       EXPANDS-TO
;;;   compile to a file          (#:EVAL-AT-LOAD-TIME-MARKER . <form>)
;;;   compile to core            '<result of evaluating form>
;;;   not in compiler at all     (progn <form>)
;;;
(defmacro load-time-eval (form)
  ;; The interpreted definition of load-time-eval.  This definition
  ;; never gets compiled.
  (let ((value (gensym)))
    `(multiple-value-bind (,value)
	 (progn ,form)
       ,value)))

(compiler:deftransformer (load-time-eval compile-load-time-eval)
			 (form &optional interpreted-form)
  (ignore interpreted-form)
  ;; When compiling a call to load-time-eval the compiler will call
  ;; this optimizer before the macro expansion.
  (if zl:compiler:(and (boundp '*compile-function*) ;Probably don't need
						    ;this boundp check
						    ;but it can't hurt.
		       (funcall *compile-function* :to-core-p))
      ;; Compiling to core.
      ;; Evaluate the form now, and expand into a constant
      ;; (the result of evaluating the form).
      `',(eval (cadr form))
      ;; Compiling to a file.
      ;; Generate the magic which causes the dumper compiler and loader
      ;; to do magic and evaluate the form at load time.
      `',(cons compiler:eval-at-load-time-marker (cadr form))))

  ;;   
;;;;;; Memory Block primitives.
  ;;   


(defmacro make-memory-block (size &optional area)
  `(make-array ,size :area ,area))

(defmacro memory-block-ref (block offset)	;Don't want to go faster yet.
  `(aref ,block ,offset))

(defvar class-wrapper-area)
(eval-when (load eval)
  (si:make-area :name 'class-wrapper-area
		:room t
		:gc :static))


;;;
;;; Reimplementation OF %INSTANCE
;;;
;;; We take advantage of the fact that Symbolics defstruct doesn't depend on
;;; the fact that Common Lisp defstructs are fixed length.  This allows us to
;;; use defstruct to define a new type, but use internal structure allocation
;;; code to make structure of that type of any length we like.
;;;
;;; In Symbolics Common Lisp, structures are really just arrays with a magic
;;; bit set.  The first element of the array points to the symbol which is
;;; the name of this structure.  The remaining elements are used for the
;;; slots of the structure.
;;;
;;; In our %instance datatype, the array look like
;;;
;;;  element 0:  The symbol %INSTANCE, this tells the system what kind of
;;;              structure this is.
;;;  element 1:  The meta-class of this %INSTANCE
;;;  element 2:  This is used to store the value of %instance-ref slot 0.
;;;  element 3:  This is used to store the value of %instance-ref slot 1.
;;;     .                                .
;;;     .                                .
;;;
(defstruct (%instance (:print-function print-instance)
		      (:constructor nil)
		      (:predicate %instancep))
  meta-class)

(zl:defselect ((:property %instance zl:named-structure-invoke))
  (:print-self (iwmc-class stream print-depth *print-escape*)
	       (print-instance iwmc-class stream print-depth))
  (:describe   (iwmc-class &optional no-complaints)
	       (ignore no-complaints)
	       (describe-instance iwmc-class)))

(defmacro %make-instance (meta-class size)
  (let ((instance-var (gensym)))
    `(let ((,instance-var (make-array (+ 2 ,size))))
       (setf (SI:ARRAY-NAMED-STRUCTURE-BIT ,instance-var) 1
	     (aref ,instance-var 0) '%instance
	     (aref ,instance-var 1) ,meta-class)
       ,instance-var)))

(defmacro %instance-ref (instance index)
  `(aref ,instance (+ ,index 2)))

  ;;   
;;;;;; Cache No's
  ;;  

(zl:defsubst symbol-cache-no (symbol mask)
  (logand (si:%pointer symbol) mask))		    

(compiler:defoptimizer (symbol-cache-no fold-symbol-cache-no) (form)
  (if (and (constantp (cadr form))		                    
	   (constantp (caddr form)))
      `(load-time-eval (logand (si:%pointer ,(cadr form)) ,(caddr form)))
      form))

(defmacro object-cache-no (object mask)
  `(logand (si:%pointer ,object) ,mask))

  ;;   
;;;;;; printing-random-thing-internal
  ;;
(defun printing-random-thing-internal (thing stream)
  (format stream "~O" (si:%pointer thing)))

  ;;   
;;;;;; function-arglist
  ;;
;;;
;;; This is hard, I am sweating.
;;; 
(defun function-arglist (function) (zl:arglist function t))

(defun function-pretty-arglist (function) (zl:arglist function))

;; Unfortunately, this doesn't really work...
(defun set-function-pretty-arglist (function new-value)
  (ignore function new-value))

;; But this does...
(zl:advise zl:arglist
	   :after
	   pcl-patch-to-arglist
	   ()
  (let ((function (car zl:arglist))
	(discriminator nil))
      (when (and (symbolp function)
		 (setq discriminator (discriminator-named function)))
	(setq values (list (discriminator-pretty-arglist discriminator))))))


  ;;   
;;;;;; 
  ;;   

(defun record-definition (name type &rest args)
  (case type
    (method (si:record-source-file-name name 'zl:defun t))
    (class ())))

(defun compile-time-define (type name &rest ignore)
  (case type
    (defun (compiler:file-declare name 'zl:def 'zl:ignore))))

  ;;   
;;;;;; Environment support and Bug-Fixes
  ;;
;;; Some VERY basic environment support for the 3600, and some bug fixes and
;;; improvements to 3600 system utilities.  These may need some work before
;;; they will work in release 7.
;;; 
(eval-when (load eval)
  (setf
    (get 'defmeth 'zwei:definition-function-spec-type) 'defun
   ;(get 'defmeth 'zwei:definition-function-spec-finder-template) '(0 1)
    (get 'ndefstruct 'zwei:definition-type-name) "Class"
    (get 'ndefstruct 'zwei:definition-function-spec-finder-template) '(0 1))
  )

;;; These changes let me dump instances of PCL metaclasses in files, and also arrange
;;; for the #S syntax to work for PCL instances.
;;; si:dump-object and si:get-defstruct-constructor-macro-name get "advised".
;;; Actually the advice is done by hand since it doesn't get compiled otherwise.

(defvar *old-dump-object*)
(defun patched-dump-object (object stream)
  (if (or (si:send si:*bin-dump-table* :get-hash object)
	  (not (and (%instancep object)
		    (class-standard-constructor (class-of object)))))
      (funcall *old-dump-object* object stream)
      ;; Code pratically copied from dump-instance.
      (let ((index (si:enter-table object stream t t)))
	(si:dump-form-to-eval
	  (cons (class-standard-constructor (class-of object))
		(iterate
		  ((slot in (all-slots object) by cddr)
		   (val in (cdr (all-slots object)) by cddr))
		  (collect (make-keyword slot))
		  (collect `',val)))
	  stream)
	(si:finish-enter-table object index))))

(unless (boundp '*old-dump-object*)
  (setf *old-dump-object* (symbol-function 'si:dump-object)
	(symbol-function 'si:dump-object) 'patched-dump-object))

(defvar *old-get-defstruct-constructor-macro-name*)
(defun patched-get-defstruct-constructor-macro-name (structure)
  (let ((class (class-named structure t)))
    (if class
	(class-standard-constructor class)
	(funcall *old-get-defstruct-constructor-macro-name* structure))))


(unless (boundp '*old-get-defstruct-constructor-macro-name*)
  (setf *old-get-defstruct-constructor-macro-name*
	   (symbol-function 'si:get-defstruct-constructor-macro-name)
	(symbol-function 'si:get-defstruct-constructor-macro-name)
	   'patched-get-defstruct-constructor-macro-name))

