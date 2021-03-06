;;; -*- Mode:LISP; Package:(PCL LISP 1000); Base:10; Syntax:Common-lisp -*-
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
;;; This is the EXCL (Franz) lisp version of the file portable-low.
;;; 
;;; This is for version 1.1.2.  Many of the special symbols now in the lisp
;;; package (e.g. lisp::pointer-to-fixnum) will be in some other package in
;;; a later release so this will need to be changed.
;;; 

(in-package 'pcl)

(eval-when (load)
  (setq *class-of*
	'(lambda (x) 
	   (or (and (%instancep x)
		    (%instance-class-of x))	       
	      ;(%funcallable-instance-p x)
	       (and (stringp x) (class-named 'string))
	       (class-named (type-of x) t))))
  )

(defmacro load-time-eval (form)
  (cond ((and sys:*macroexpand-for-compiler* sys:*compile-to-core*)
	 `',(eval form))
	((and sys:*macroexpand-for-compiler* sys:*compile-to-file*)
	;(cerror "go ahead" "called load-time-eval in compile-to-file")
	 `'(,compiler::*eval-when-load-marker* . ,form))
	(t
	 `(progn ,form))))

(eval-when (compile load eval)
  (unless (fboundp 'excl::sy_hash)
    (setf (symbol-function 'excl::sy_hash)
	  (symbol-function 'excl::_sy_hash-value))))

(defmacro symbol-cache-no (symbol mask)
  (if (and (constantp symbol)
	   (constantp mask))
      `(load-time-eval (logand (ash (excl::sy_hash ',symbol) -1) ,mask))
      `(logand (ash (the fixnum (excl::pointer-to-fixnum ,symbol)) -1)
	       (the fixnum ,mask))))

(defmacro object-cache-no (object mask)
  `(logand (the fixnum (excl::pointer-to-fixnum ,object))
	   (the fixnum ,mask)))

(defun printing-random-thing-internal (thing stream)
  (format stream "~O" (excl::pointer-to-fixnum thing)))


(defun function-arglist (f)
  (excl::arglist f))


(defun symbol-append (sym1 sym2 &optional (package *package*))
   ;; This is a version of symbol-append from macros.cl
   ;; It insures that all created symbols are of one case and that
   ;; case is the current prefered case.
   ;; This special version of symbol-append is not necessary if all you
   ;; want to do is compile and run pcl in a case-insensitive-upper 
   ;; version of cl.  
   ;;
   (let ((string (string-append sym1 sym2)))
      (case excl::*current-case-mode*
	 ((:case-insensitive-lower :case-sensitive-lower)
	  (setq string (string-downcase string)))
	 ((:case-insensitive-upper :case-sensitive-upper)
	  (setq string (string-upcase string))))
      (intern string package)))

;(eval-when (compile load eval)
;  (let ((consts 
;	  (sys:memref (symbol-function 'compiler::pa-macrolet)
;		      (compiler::mdparam 'compiler::md-function-constant-adj)
;		      0
;		      :lisp)))
;    (dotimes (i (length consts))
;      (cond ((eq 'compiler::macro (svref consts i))
;	     (setf (svref consts i) 'excl::macro)
;	     (format t "fixed in slot ~s~%" i))
;	    ((eq 'excl::macro (svref consts i))
;	     (format t "already fixed in slot ~s~%" i))))))

