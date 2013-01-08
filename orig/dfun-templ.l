;;;-*-Mode:LISP; Package: PCL; Base:10; Syntax:Common-lisp -*-
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

(in-package 'pcl)


;;; 
;;; A caching discriminating function looks like:
;;;   (lambda (arg-1 arg-2 arg-3 &rest rest-args)
;;;     (prog* ((class-1 (class-of arg-1))
;;;             (class-2 (class-of arg-2))
;;;             method-function)
;;;        (and (cached-method method-function CACHE MASK class-1 class-2)
;;;             (go hit))
;;;      miss
;;;        (setq method-function
;;;              (cache-method DISCRIMINATOR
;;;                            (lookup-method-function DISCRIMINATOR
;;;                                                    class-1
;;;                                                    class-2)))
;;;      hit
;;;        (if method-function
;;;            (return (apply method-function arg-1 arg-2 arg-3 rest-args))
;;;            (return (no-matching-method DISCRIMINATOR)))))
;;;
;;; The upper-cased variables are the ones which are lexically bound.

;;; There is a great deal of room to play here.  This open codes the
;;; test to see if the instance is iwmc-class-p.  Only if it isn't is
;;; there a function call to class-of.  This is done because we only have
;;; a default implementation of make-discriminating-function, we don't
;;; have one which is specific to discriminator-class DISCRIMINATOR and
;;; meta-class CLASS.
;;;
;;; Of course a real implementation of CommonLoops wouldn't even do a
;;; real function call to get to the discriminating function.

(eval-when (compile load eval)

(defun default-make-class-of-form-fn (arg)
  `(if (iwmc-class-p ,arg)
       (class-of--class ,arg)
       (class-of ,arg)))

(defvar *make-class-of-form-fn* #'default-make-class-of-form-fn)

(define-function-template caching-discriminating-function
                          (required restp
				    specialized-positions
				    lookup-function)
                          '(.DISCRIMINATOR. .CACHE. .MASK.)
  (let* ((args (iterate ((i from 0 below required))
                 (collect (make-symbol (format nil "Disc-Fn-Arg ~D" i)))))
         (class-bindings
           (iterate ((i from 0 below required)
                     (ignore in specialized-positions))
             (if (member i specialized-positions)
                 (collect
		   (list (make-symbol (format nil "Class of ARG ~D" i))
			 (funcall *make-class-of-form-fn* (nth i args))))
                 (collect nil))))
         (classes (remove nil (mapcar #'car class-bindings)))
         (method-function-var (make-symbol "Method Function"))
         (rest-arg-var (and restp (make-symbol "Disc-Fn-&Rest-Arg"))))
    `(function
       (lambda (,@args ,@(and rest-arg-var (list '&rest rest-arg-var)))
         (prog (,@(remove nil class-bindings) ,method-function-var)
	   (and (cached-method ,method-function-var .CACHE. .MASK. ,@classes)
		(go hit))
	  ;miss
	   (setq ,method-function-var
		 (cache-method .CACHE.
			       .MASK.
			       (,lookup-function .DISCRIMINATOR.
						 ,@(mapcar #'car
							   class-bindings))
			       ,@classes))
	   hit
	   (if ,method-function-var
	       (return ,(if restp
			    `(apply ,method-function-var
				    ,@args
				    ,rest-arg-var)
			    `(funcall ,method-function-var ,@args)))
	       (no-matching-method .DISCRIMINATOR.)))))))
)

(eval-when (compile)
(defmacro pre-make-caching-discriminating-functions (specs)
  `(progn . ,(iterate ((spec in specs))
	       (collect `(pre-make-templated-function-constructor
			   caching-discriminating-function
			   ,@spec))))))

(eval-when (load)
  (pre-make-caching-discriminating-functions
    ((2 NIL (0 1) LOOKUP-MULTI-METHOD)
     (4 NIL (0) LOOKUP-CLASSICAL-METHOD)
     (5 NIL (0) LOOKUP-CLASSICAL-METHOD)
     (1 T (0) LOOKUP-CLASSICAL-METHOD)
     (3 NIL (0 1) LOOKUP-MULTI-METHOD)
     (4 T (0) LOOKUP-CLASSICAL-METHOD)
     (3 T (0) LOOKUP-CLASSICAL-METHOD)
     (3 NIL (0) LOOKUP-CLASSICAL-METHOD)
     (1 NIL (0) LOOKUP-CLASSICAL-METHOD)
     (2 NIL (0) LOOKUP-CLASSICAL-METHOD))))

  ;;   
;;;;;; 
  ;;

(eval-when (compile load eval)

(define-function-template checking-discriminating-function
	(required restp defaultp checks)
	`(discriminator method-function default-function
			,@(make-checking-discriminating-function-1 checks))
  (let* ((arglist (make-discriminating-function-arglist required restp)))
    `(function
       (lambda ,arglist
	 (declare (optimize (speed 3) (safety 0)))
	 discriminator default-function ;ignorable
         (if (and ,@(iterate ((check in
				     (make-checking-discriminating-function-1
				       checks))
                              (arg in arglist))
                      (when (neq check 'ignore)
			(collect
			  `(memq ,check
				 (let ((.class. (class-of ,arg)))
				   (get-slot--class .class.
						    'class-precedence-list)))))))
             ,(if restp
                  `(apply method-function ,@(remove '&rest arglist))
                  `(funcall method-function ,@arglist))
             ,(if defaultp
                  (if restp
                      `(apply default-function ,@(remove '&rest arglist))
                      `(funcall default-function ,@arglist))
                  `(no-matching-method discriminator)))))))

(defun make-checking-discriminating-function-1 (check-positions)
  (iterate ((pos in check-positions))
    (collect (if (null pos) 'ignore (intern (format nil "Check ~D" pos))))))

)

(eval-when (compile)
(defmacro pre-make-checking-discriminating-functions (specs)
  `(progn . ,(iterate ((spec in specs))
	       (collect `(pre-make-templated-function-constructor
			   checking-discriminating-function
			   ,@spec))))))

(eval-when (load)
  (pre-make-checking-discriminating-functions ((3 NIL NIL (0 1))
					       (7 NIL NIL (0 1))
					       (5 NIL NIL (0 1))
					       (3 NIL NIL (0 NIL 2))
					       (6 NIL NIL (0))
					       (5 NIL NIL (0))
					       (4 T NIL (0))
					       (3 T NIL (0))
					       (1 T NIL (0))
					       (4 NIL NIL (0))
					       (3 NIL NIL (0))
					       (3 NIL T (0 1))
					       (2 NIL T (0))
					       (5 NIL T (0 1))
					       (1 T T (0))
					       (1 NIL T (0))
					       (2 NIL T (0 1))
					       (3 NIL T (0))
					       (2 T T (0))
					       (6 NIL T (0 1))
					       (3 NIL T (0 NIL 2))
					       (4 NIL T (0 1))
					       (4 NIL T (0))
					       (5 NIL T (0))
					       (1 NIL NIL (0))
					       (2 NIL NIL (0)))))

