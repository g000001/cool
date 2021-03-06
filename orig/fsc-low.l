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

#|  To do:

figure out bootstrapping issues

fix problems caused by make-iwmc-class-accessor

polish up the low levels of iwmc-class, 

fix use of get-slot-using-class--class-internal

|#
  ;;   
;;;;;; FUNCALLABLE INSTANCES
  ;;

#|

In CommonLoops, generic functions are instances whose meta class is
funcallable-standard-class.  Instances with this meta class behave
something like lexical closures in that they have slots, just like
instances with meta class standard-class, and are also funcallable.
When an instance with meta class funcallable-standard-class is
funcalled, the value of its function slot is called.

It is possible to implement funcallable instances in pure Common Lisp.
A simple implementation which uses lexical closures as the instances and
a hash table to record that the lexical closures are funcallable
instances is easy to write.  Unfortunately, this implementation adds
such significant overhead:

   to generic-function-invocation (1 function call)
   to slot-access (1 function call)
   to class-of a generic-function (1 hash-table lookup)

In other words, it is too slow to be practical.

Instead, PCL uses a specially tailored implementation for each common
Lisp and makes no attempt to provide a purely portable implementation.
The specially tailored implementations are based on each the lexical
closure's provided by that implementation and tend to be fairly easy to
write.

|#

(in-package 'pcl)

;;;
;;; The first part of the file contains the implementation dependent code
;;; to implement the low-level funcallable instances.  Each implementation
;;; must provide the following functions and macros:
;;; 
;;;    MAKE-FUNCALLABLE-INSTANCE-1 ()
;;;       should create and return a new funcallable instance
;;;
;;;    FUNCALLABLE-INSTANCE-P (x)
;;;       the obvious predicate
;;;
;;;    SET-FUNCALLABLE-INSTANCE-FUNCTION-1 (fin new-value)
;;;       change the fin so that when it is funcalled, the new-value
;;;       function is called.  Note that it is legal for new-value
;;;       to be copied before it is installed in the fin (the Lucid
;;;       implementation in particular does this).
;;;
;;;    FUNCALLABLE-INSTANCE-DATA-1 (fin data-name)
;;;       should return the value of the data named data-name in the fin
;;;       data-name is one of the symbols in the list which is the value
;;;       of funcallable-instance-data.  Since data-name is almost always
;;;       a quoted symbol and funcallable-instance-data is a constant, it
;;;       is possible (and worthwhile) to optimize the computation of
;;;       data-name's offset in the data part of the fin.
;;;       

(defconstant funcallable-instance-data
	     '(class wrapper static-slots dynamic-slots)
  "These are the 'data-slots' which funcallable instances have so that
   the meta-class funcallable-standard-class can store class, and static
   and dynamic slots in them.")

#+Lucid
(progn
  
(defconstant funcallable-instance-procedure-size 50)
(defconstant funcallable-instance-flag-bit #B1000000000000000)
(defvar *funcallable-instance-trampolines* ()
  "This is a list of all the procedure sizes which were too big to be stored
   directly in a funcallable instance.  For each of these procedures, a
   trampoline procedure had to be used.  This is for metering information
   only.")

(defun make-funcallable-instance-1 ()
  (let ((new-fin (lucid::new-procedure funcallable-instance-procedure-size)))
    ;; Have to set the procedure function to something for two reasons.
    ;;   1. someone might try to funcall it.
    ;;   2. the flag bit that says the procedure is a funcallable
    ;;      instance is set by set-funcallable-instance-function.
    (set-funcallable-instance-function
      new-fin
      #'(lambda (&rest ignore)
	  (declare (ignore ignore))
	  (error "Attempt to funcall a funcallable-instance without first~%~
                  setting its funcallable-instance-function.")))
    new-fin))

(defmacro funcallable-instance-p (x)
  (once-only (x)
    `(and (lucid::procedurep ,x)
	  (logand (lucid::procedure-ref ,x lucid::procedure-flags)
		  funcallable-instance-flag-bit))))

(defun set-funcallable-instance-function-1 (fin new-value)
  (unless (funcallable-instance-p fin)
    (error "~S is not a funcallable-instance"))
  (cond ((not (functionp new-value))
	 (error "~S is not a function."))
	((not (lucid::procedurep new-value))
	 ;; new-value is an interpreted function.  Install a
	 ;; trampoline to call the interpreted function.
	 (set-funcallable-instance-function fin
					    (make-trampoline new-value)))
	(t
	 (let ((new-procedure-size (lucid::procedure-length new-value))
	       (max-procedure-size (- funcallable-instance-procedure-size
				      (length funcallable-instance-data))))
	   (if (< new-procedure-size max-procedure-size)
	       ;; The new procedure fits in the funcallable-instance.
	       ;; Just copy the new procedure into the fin procedure,
	       ;; also be sure to update the procedure-flags of the
	       ;; fin to keep it a fin.
	       (progn 
		 (dotimes (i max-procedure-size)
		   (setf (lucid::procedure-ref fin i)
			 (lucid::procedure-ref new-value i)))
		 (setf (lucid::procedure-ref fin lucid::procedure-flags)
		       (logand funcallable-instance-flag-bit
			       (lucid::procedure-ref
				 fin lucid::procedure-flags)))
		 new-value)
	       ;; The new procedure doesn't fit in the funcallable instance
	       ;; Instead, install a trampoline procedure which will call
	       ;; the new procecdure.  First make note of the fact that we
	       ;; had to trampoline so that we can see if its worth upping
	       ;; the value of funcallable-instance-procedure-size.
	       (progn
		 (push new-procedure-size *funcallable-instance-trampolines*)
		 (set-funcallable-instance-function
		   fin
		   (make-trampoline new-value))))))))


(defmacro funcallable-instance-data-1 (instance data)
  `(lucid::procedure-ref ,instance
			 (- funcallable-instance-procedure-size
			    (position ,data funcallable-instance-data))))
  
);dicuL+#

;;;
;;; All of these Lisps (Xerox Symbolics ExCL KCL and VAXLisp) have the
;;; following in Common:
;;; 
;;;    - they represent their compiled closures as a pair of
;;;      environment and compiled function
;;;    - they represent the environment using a list or a vector
;;;    - I don't (YET) know how to add a bit to the damn things to
;;;      say that they are funcallable-instances and so I have to
;;;      use the last entry in the closure environment to do that.
;;;      This is a lose because that is much slower, I have to CDR
;;;      down to the last element of the environment.
;;;      
#+(OR Xerox Symbolics ExCL KCL (and DEC VAX))
(progn

(defvar *funcallable-instance-marker* (list "Funcallable Instance Marker"))

(defconstant funcallable-instance-closure-size 15)

(defmacro lexical-closure-p (lc)
  #+Xerox         `(typep ,lc 'il:compiled-closure)
  #+Symbolics     `(si:lexical-closure-p ,lc)
  #+ExCL          `()
  #+KCL           `()
  #+(and DEC VAX) (once-only (lc)
		    `(and (listp ,lc)
			  (eq (car ,lc) 'system::%compiled-closure%))))

(defmacro lexical-closure-env (lc)
  #+Xerox         `()
  #+Symbolics     `(si:lexical-closure-environment ,lc)
  #+ExCL          `()
  #+KCL           `()
  #+(and DEC VAX) `(caadr ,lc))

(defmacro lexical-closure-env-size (env)
  #+Xerox         `()
  #+Symbolics     `(length ,env)
  #+ExCL          `()
  #+KCL           `()
  #+(and DEC VAX) `(array-dimension ,env 0))  

(defmacro lexical-closure-env-ref (env index check) check
  #+Xerox         `()
  #+Symbolics     `(let ((env ,env))
		     (dotimes (i ,index)
		       (setq env (cdr env)))
		     (car env))
  #+ExCL          `()
  #+KCL           `()
  #+(and DEC VAX) (once-only (env)
		    `(and ,(or checkp
			       `(= (array-dimension ,env 0)
				   funcallable-instance-closure-size))
			  (svref ,env 0))))

(defmacro lexical-closure-env-set (env index new checkp) checkp
  #+Xerox         `()
  #+Symbolics     `(let ((env ,env))
		     (dotimes (i ,index)
		       (setq env (cdr env)))
		     (setf (car env) ,new))
  #+ExCL          `()
  #+KCL           `()
  #+(and DEC VAX) (once-only (env)
		    `(and ,(or checkp
			       `(= (array-dimension ,env 0)
				   funcallable-instance-closure-size))
			  (setf (svref ,env ,index) ,new))))

(defmacro lexical-closure-code (lc)
  #+Xerox         `()
  #+Symbolics     `(si:lexical-closure-function ,lc)
  #+ExCL          `()
  #+KCL           `()
  #+(and DEC VAX) `(caddr ,lc))

(defmacro compiled-function-code (cf)  
  #+Xerox         `()
  #+Symbolics     cf
  #+ExCL          `()
  #+KCL           `()
  #+(and DEC VAX) `())

(eval-when (load eval)
  (let ((dummies ()))
    (dotimes (i funcallable-instance-closure-size)
      (push (gentemp "Dummy Closure Variable ") dummies))
    (compile 'make-funcallable-instance-1	;For the time being, this use
	     `(lambda ()			;of compile at load time is
		(let (new-fin ,@dummies)	;simpler than using #.
		  (setq new-fin #'(lambda ()
				    ,@(mapcar #'(lambda (d)
						  `(setq ,d (dummy-fn ,d)))
					      dummies)))
		  (lexical-closure-env-set
		    (lexical-closure-env new-fin)
		    (1- funcallable-instance-closure-size)
		    *funcallable-instance-marker*
		    t)
		  new-fin)))))

(defmacro funcallable-instance-p (x)
  (once-only (x)
    `(and (lexical-closure-p ,x)
	  (let ((env (lexical-closure-env ,x)))
	    (and (eq (lexical-closure-env-ref
		       env (1- funcallable-instance-closure-size) t)
		     *funcallable-instance-marker*))))))

(defun set-funcallable-instance-function-1 (fin new-value)
  (cond ((lexical-closure-p new-value)
	 (let* ((fin-env (lexical-closure-env fin))
		(new-env (lexical-closure-env new-value))
		(new-env-size (lexical-closure-env-size new-env))
		(fin-env-size (- funcallable-instance-closure-size
				 (length funcallable-instance-data))))
	   (cond ((<= new-env-size fin-env-size)
		  (dotimes (i new-env-size)
		    (lexical-closure-env-set
		      fin-env i (lexical-closure-env-ref new-env i nil) nil))
		  (setf (lexical-closure-code fin)
			(lexical-closure-code new-value)))
		 (t		    
		  (set-funcallable-instance-function-1
		    fin (make-trampoline new-value))))))
	(t
	 #+Symbolics
	 (set-funcallable-instance-function-1 fin
					      (make-trampoline new-value))
	 #-Symbolics
	 (setf (lexical-closure-code fin)
	       (compiled-function-code new-value)))))
	
(defmacro funcallable-instance-data-1 (fin data)
  `(lexical-closure-env-ref
     (lexical-closure-env ,fin)
     (- funcallable-instance-closure-size
	(position ,data funcallable-instance-data)
	2)
     nil))

(defsetf funcallable-instance-data-1 (fin data) (new-value)
  `(lexical-closure-env-set
     (lexical-closure-env ,fin)
     (- funcallable-instance-closure-size
	(position ,data funcallable-instance-data)
	2)
     ,new-value
     nil))

);


(defun make-trampoline (function)
  #'(lambda (&rest args)
      (apply function args)))

(defun set-funcallable-instance-function (fin new-value)
  (cond ((not (funcallable-instance-p fin))
	 (error "~S is not a funcallable-instance"))
	((not (functionp new-value))
	 (error "~S is not a function."))
	((compiled-function-p new-value)
	 (set-funcallable-instance-function-1 fin new-value))
	(t
	 (set-funcallable-instance-function-1 fin
					      (make-trampoline new-value)))))


(defmacro funcallable-instance-class (fin)
  `(funcallable-instance-data-1 ,fin 'class))

(defmacro funcallable-instance-wrapper (fin)
  `(funcallable-instance-data-1 ,fin 'wrapper))

(defmacro funcallable-instance-static-slots (fin)
  `(funcallable-instance-data-1 ,fin 'static-slots))

(defmacro funcallable-instance-dynamic-slots (fin)
  `(funcallable-instance-data-1 ,fin 'dynamic-slots))

(defun make-funcallable-instance (class wrapper number-of-static-slots)
  (let ((fin (make-funcallable-instance-1))
	(static-slots (make-memory-block number-of-static-slots))
	(dynamic-slots ()))
    (setf (funcallable-instance-class fin) class
	  (funcallable-instance-wrapper fin) wrapper
	  (funcallable-instance-static-slots fin) static-slots
	  (funcallable-instance-dynamic-slots fin) dynamic-slots)
    fin))

