;;;-*-Mode:LISP; Package: PCL; Base:10; Syntax:Common-lisp; Patch-File: Yes -*-
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

(eval-when (compile load eval)
  (setq *real-methods-exist-p* nil)
  (setf (symbol-function 'expand-defmeth)
	(symbol-function 'real-expand-defmeth)))

(eval-when (load)
  (clrhash *discriminator-name-hash-table*)
  (fix-early-defmeths)
 ;; This now happens at the end of loading HIGH to make it
 ;; possible to compile and load pcl in the same environment.
 ;(setq *error-when-defining-method-on-existing-function* t)
  )

(eval-when (compile load eval)
  (setq *real-methods-exist-p* t))

  ;;   
;;;;;; Pending defmeths which I couldn't do before.
  ;;


(eval-when (load eval)
  (setf (discriminator-named 'print-instance) ())
  (make-specializable 'print-instance :arglist '(instance stream depth)))

(defmeth print-instance ((instance object) stream depth)
  (let ((length (if (numberp *print-length*) (* *print-length* 2) nil)))
    (format stream "#S(~S" (class-name (class-of instance)))
    (iterate ((slot-or-value in (all-slots instance))
	      (slotp = t (not slotp)))
      (when (numberp length)
	(cond ((<= length 0) (format stream " ...") (return ()))
	      (t (decf length))))
      (princ " " stream)
      (let ((*print-level* (cond ((null *print-level*) ())
				 (slotp 1)
				 (t (- *print-level* depth)))))
	(if (and *print-level* (<= *print-level* 0))
	    (princ "#" stream)
	    (prin1 slot-or-value stream))))
    (princ ")" stream)))

(defmeth print-instance ((class essential-class) stream depth)
  (named-object-print-function class stream depth))


(defmethod print-instance ((method essential-method) stream depth)
  (ignore depth)
  (printing-random-thing (method stream)
    (let ((discriminator (method-discriminator method))
	  (class-name (capitalize-words (class-name (class-of method)))))
      (format stream "~A ~S ~:S"
	      class-name
	      (and discriminator (discriminator-name discriminator))
	      (method-type-specifiers method)))))

(defmethod print-instance ((method basic-method) stream depth)
  (ignore depth)
  (printing-random-thing (method stream)
    (let ((discriminator (method-discriminator method))
	  (class-name (capitalize-words (class-name (class-of method)))))
      (format stream "~A ~S ~:S"
	      class-name
	      (and discriminator (discriminator-name discriminator))
	      (unparse-type-specifiers method)))))

(defmethod print-instance ((discriminator essential-discriminator) stream depth)
  (named-object-print-function discriminator stream depth))

(defmethod print-instance ((discriminator basic-discriminator) stream depth)
  (named-object-print-function
    discriminator stream depth (list (method-combination-type discriminator))))

(eval-when (load)

(define-meta-class essential-class (lambda (x) (%instance-ref x 0)))

(defmeth class-slots ((class essential-class))
  (ignore class)
  ())

(defmeth make-instance ((class essential-class))
  (let ((primitive-instance
	  (%make-instance (class-named 'essential-class)
			  (1+ (length (class-slots class))))))
    (setf (%instance-ref primitive-instance 0) class)
    primitive-instance))

(defmeth get-slot-using-class ((class essential-class) object slot-name)
  (let ((pos (position slot-name (class-slots class) :key #'slotd-name)))
    (if pos
	(%instance-ref object (1+ pos))
	(slot-missing ;class
	  object slot-name))))

(defmeth put-slot-using-class ((class essential-class)
			       object
			       slot-name
			       new-value)
  (let ((pos (position slot-name (class-slots class) :key #'slotd-name)))
    (if pos
	(setf (%instance-ref object (1+ pos)) new-value)
	(slot-missing ;class
		      object slot-name))))

(defmeth optimize-get-slot (class form)
  (declare (ignore class))
  form)

(defmeth optimize-setf-of-get-slot (class form)
  (declare (ignore class))
  form)

(defmeth make-slotd ((class essential-class) &rest keywords-and-options)
  (ignore class)
  (apply #'make-slotd--essential-class keywords-and-options))

(defmeth add-named-class ((proto-class essential-class) name
			  local-supers
			  local-slot-slotds
			  extra)
  ;; First find out if there is already a class with this name.
  ;; If there is, call class-for-redefinition to get the class
  ;; object to use for the new definition.  If there is no exisiting
  ;; class we just make a new instance.
  (let* ((existing (class-named name t))
	 (class (if existing
		    (class-for-redefinition existing proto-class name 
					    local-supers local-slot-slotds
					    extra)
		    (make (class-of proto-class)))))

    (setq local-supers
	  (mapcar
	    #'(lambda (ls)
		(or (class-named ls t)
		    (error "~S was specified as the name of a local-super~%~
                            for the class named ~S.  But there is no class~%~
                            class named ~S." ls name ls)))
	    local-supers))
    
    (setf (class-name class) name)
;   (setf (class-ds-options class) extra)	;This is NOT part of the
;						;standard protocol.
   
    (add-class class local-supers local-slot-slotds extra)
    
    (setf (class-named name) class)
    name))

(defmeth supers-changed ((class essential-class)
			 old-local-supers
			 old-local-slots
			 extra
			 top-p)
  (ignore old-local-supers old-local-slots top-p)
  (let ((cpl (compute-class-precedence-list class)))
    (setf (class-class-precedence-list class) cpl)
;   (update-slots--class class cpl)		         ;This is NOT part of
;						         ;the essential-class
;						         ;protocol.
    (dolist (sub-class (class-direct-subclasses class))
      (supers-changed sub-class
		      (class-local-supers sub-class)
		      (class-local-slots sub-class)
		      extra
		      nil))
;   (when top-p                                          ;This is NOT part of
;     (update-method-inheritance class old-local-supers));the essential-class
; 					                 ;protocol.
    ))

(defmeth slots-changed ((class essential-class)
			old-local-slots
			extra
			top-p)
  (ignore top-p old-local-slots)
  ;; When this is called, class should have its local-supers and
  ;; local-slots slots filled in properly.
; (update-slots--class class (class-class-precedence-list class))
  (dolist (sub-class (class-direct-subclasses class))
    (slots-changed sub-class (class-local-slots sub-class) extra nil)))

(defmeth method-equal (method argument-specifiers options)
  (ignore options)
  (equal argument-specifiers (method-type-specifiers method)))

(defmeth methods-combine-p ((d essential-discriminator))
  (ignore d)
  nil)

)

  ;;   
;;;;;; 
  ;;

(define-method-body-macro call-next-method ()
  :global :error
  :method (expand-call-next-method
	    (macroexpand-time-method macroexpand-time-environment)
	    nil
	    macroexpand-time-environment))

(defmethod expand-call-next-method ((mex-method method) args mti)
  (ignore args)
  (let* ((arglist (and mex-method (method-arglist mex-method)))
	 (uid (macroexpand-time-method-uid mti))
	 (load-method-1-args (macroexpand-time-load-method-1-args mti))
	 (load-time-eval-form `(load-time-eval
				 (if (boundp ',uid)
				     ,uid
				     (setq ,uid
					   (apply #'load-method-1
						  ',load-method-1-args)))))
	 (applyp nil))
    (multiple-value-setq (arglist applyp) (make-call-arguments arglist))
    (cond ((null (method-type-specifiers mex-method))
	   (warn "Using call-next-method in a default method.~%~
                  At run time this will generate an error.")
	   '(error "Using call-next-method in a default method."))
	  (applyp
	   `(apply
	      #'call-next-method-internal ,load-time-eval-form . ,arglist))
	  (t
	   `(call-next-method-internal ,load-time-eval-form . ,arglist)))))

(defun call-next-method-internal (current-method &rest args)
  (let* ((discriminator (method-discriminator current-method))
	 (type-specifiers (method-type-specifiers current-method))
	 (most-specific nil)
	 (most-specific-type-specifiers ())
	 (dispatch-order (get-slot--class discriminator 'dispatch-order)))
    (iterate ((method in (discriminator-methods discriminator)))
      (let ((method-type-specifiers (method-type-specifiers method))
            (temp ()))
        (and (every #'(lambda (arg type-spec)
			(or (eq type-spec 't)
			    (memq type-spec
				  (get-slot--class
				    (class-of arg) 'class-precedence-list))))
                    args method-type-specifiers)
             (eql 1 (setq temp (compare-type-specifier-lists
				 type-specifiers
				 method-type-specifiers
				 ()
				 args
				 ()
				 dispatch-order)))
             (or (null most-specific)
                 (eql 1 (setq temp (compare-type-specifier-lists
                                     method-type-specifiers
                                     most-specific-type-specifiers
                                     ()
                                     args
                                     ()
				     dispatch-order))))
             (setq most-specific method
                   most-specific-type-specifiers method-type-specifiers))))
    (if (or most-specific
            (setq most-specific (discriminator-default-method
				  discriminator)))
        (apply (method-function most-specific) args)
        (error "no super method found"))))

;;;
;;; This is kind of bozoid because it always copies the lambda-list even
;;; when it doesn't need to.  It also doesn't remember things it could
;;; remember, causing it to call memq more than it should.  Fix this one
;;; day when there is nothing else to do.
;;; 
(defun make-call-arguments (lambda-list &aux applyp)
  (setq lambda-list (reverse lambda-list))
  (when (memq '&aux lambda-list)
    (setq lambda-list (cdr (memq '&aux lambda-list))))
  (setq lambda-list (nreverse lambda-list))
  (let ((optional (memq '&optional lambda-list)))
    (when optional
      ;; The &optional keyword appears in the lambda list.
      ;; Get rid of it, by moving the rest of the lambda list
      ;; up, then go through the optional arguments, replacing
      ;; them with the real symbol.
      (setf (car optional) (cadr optional)
	    (cdr optional) (cddr optional))
      (iterate ((loc on optional))
	(when (memq (car loc) lambda-list-keywords)
	  (unless (memq (car loc) '(&rest &key &allow-other-keys))
	    (error
	      "The non-standard lambda list keyword ~S appeared in the~%~
               lambda list of a method in which CALL-NEXT-METHOD is used.~%~
               PCL can only deal with standard lambda list keywords."))
	  (when (listp (car loc)) (setf (car loc) (caar loc)))))))
  (let ((rest (memq '&rest lambda-list)))
    (cond ((not (null rest))
	   ;; &rest appears in the lambda list. This means we
	   ;; have to do an apply. We ignore the rest of the
	   ;; lambda list, just grab the &rest var and set applyp.
	   (setf (car rest) (if (listp (cadr rest))
				(caadr rest)
				(cadr rest))
		 (cdr rest) ())
	   (setq applyp t))
	  (t
	   (let ((key (memq '&key lambda-list)))
	     (when key
	       ;; &key appears in the lambda list.  Remove &key from the
	       ;; lambda list then replace all the keywords with pairs of
	       ;; the actual keyword followed by the value variable.
	       ;; Have to parse the hairy triple case of &key.
	       (let ((key-args
		       (iterate ((arg in (cdr key)))
			 (until (eq arg '&allow-other-keys))
			 (cond ((symbolp arg)
				(collect (make-keyword arg))
				(collect arg))
			       ((cddr arg)
				(collect (caddr arg))
				(collect (car arg)))
			       (t
				(collect (make-keyword (car arg)))
				(collect (car arg)))))))
		 (if key-args
		     (setf (car key) (car key-args)
			   (cdr key) (cdr key-args))
		     (setf (cdr key) nil
			   lambda-list (remove '&key lambda-list)))))))))
  (values lambda-list applyp))
