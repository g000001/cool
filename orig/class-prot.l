;;;-*-Mode:LISP; Package:(PCL Lisp 1000); Base:10; Syntax:Common-lisp -*-
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
;;; ADD-NAMED-CLASS  proto-class name local-supers local-slot-slotds extra
;;; protocol: class-definition
;;;
;;; Creates or updates the definition of a class with a named class.  If
;;; there is already a class named name, calls class-for-redefinition to
;;; find out which class to use for the redefinition.  Once it has a class
;;; object to use it stores the relevant information from the ds-options in
;;; the class and calls add-class to add the class to the class
;;; lattice.
;;; 
(defmeth add-named-class ((proto-class basic-class) name
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
    (setf (class-ds-options class) extra)	;This is NOT part of the
						;standard protocol.
   
    (add-class class local-supers local-slot-slotds extra)
    
    (setf (class-named name) class)
    name))

(defmeth add-class
	 ((class essential-class) new-local-supers new-local-slots extra)
  (ignore extra)
  (let ((old-local-supers (class-local-supers class))
	(old-local-slots (class-local-slots class)))
    
    (setf (class-local-supers class) new-local-supers)
    (setf (class-local-slots class) new-local-slots)

    (if (and old-local-supers			;*** YUCH!! There is a bug
	     new-local-supers			;*** when old and new are ()
	     (equal old-local-supers new-local-supers))
	(if (and old-local-slots
		 new-local-slots
		 (equal old-local-slots new-local-slots))
	    ;; If the supers haven't changed, and the slots haven't changed
	    ;; then not much has changed and we don't have to do anything.
	    ()
	    ;; If only the slots have changed call slots-changed.
	    (slots-changed class old-local-slots extra t))
	;; If the supers have changed, first update local-supers and
	;; direct-subclasses of all the people involved.  Then call
	;; supers-changed.
	(progn
	  (dolist (nls new-local-supers)
	    (unless (memq nls old-local-supers)
	      (check-super-metaclass-compatibility class nls)
	      (push class (class-direct-subclasses nls))))
	  (dolist (ols old-local-supers)
	    (unless (memq ols new-local-supers)
	      (setf (class-direct-subclasses ols)
		    (delq class (class-direct-subclasses ols)))))
	  (supers-changed class old-local-supers old-local-slots extra t)))))


(defmeth supers-changed ((class basic-class)
			 old-local-supers
			 old-local-slots
			 extra
			 top-p)
  (ignore old-local-slots)
  (let ((cpl (compute-class-precedence-list class)))
    (setf (class-class-precedence-list class) cpl)
    (update-slots--class class cpl)		         ;This is NOT part of
						         ;the essential-class
						         ;protocol.
    (dolist (sub-class (class-direct-subclasses class))
      (supers-changed sub-class
		      (class-local-supers sub-class)
		      (class-local-slots sub-class)
		      extra
		      nil))
    (when top-p                                          ;This is NOT part of
      (update-method-inheritance class old-local-supers));the essential-class
						         ;protocol.
    ))

(defmeth slots-changed ((class basic-class)
			old-local-slots
			extra
			top-p)
  (ignore top-p old-local-slots)
  ;; When this is called, class should have its local-supers and
  ;; local-slots slots filled in properly.
  (update-slots--class class (class-class-precedence-list class))
  (dolist (sub-class (class-direct-subclasses class))
    (slots-changed sub-class (class-local-slots sub-class) extra nil)))

(defun update-slots--class (class cpl)
  (let ((obsolete-class nil))
    (multiple-value-bind (instance-slots non-instance-slots)
	(collect-slotds class (class-local-slots class) cpl)
      ;; If there is a change in the shape of the instances then the
      ;; old class is now obsolete.  Make a copy of it, then fill
      ;; ourselves in properly and obsolete it.
      (when (and (class-has-instances-p class)
		 (not (same-shape-slots-p (class-instance-slots class)
					  instance-slots)))
	(setq obsolete-class (copy-class class)))
      (setf (class-no-of-instance-slots class) (length instance-slots))
      (setf (class-instance-slots class) instance-slots)
      (setf (class-non-instance-slots class) non-instance-slots)
      (when obsolete-class
	(flush-class-caches class)
	(make-class-obsolete class (copy-class class))))))

;;;
;;; CLASS-FOR-REDEFINITION old-class proto-class name ds-options slotds
;;; protocol: class definition
;;; 
;;; When a class is being defined, and a class with that name already exists
;;; a decision must be made as to what to use for the new class object, and
;;; whether to update the old class object.  For this, class-for-redefinition
;;; is called with the old class object, the prototype of the new class, and
;;; the name ds-options and slotds corresponding to the new definition.
;;; It should return the class object to use as the new definition.  It is
;;; OK for this to be old-class if that is appropriate.
;;; 
(defmeth class-for-redefinition ((old-class essential-class)
				 proto-class
				 name
				 local-supers
				 local-slot-slotds
				 extra)
  (ignore local-supers local-slot-slotds extra)
  (cond ((not (compatible-meta-class-change-p old-class proto-class))
	 (error "The class ~A already exists; its class is ~A.~%~
		 The :class argument in the defstruct is ~A.
		 This is an incompatible meta-class change.~%"
		name
		(class-name (class-of old-class))
		(class-name (class-of proto-class))))
	(t (values old-class (copy-class old-class)))))

(defmeth update-method-inheritance ((class basic-class) old-local-supers)
  ;; In the absence of method combination, we have to flush all the
  ;; discriminators which we used to inherit and all the discriminators
  ;; which we now inherit.
  (let ((old-mil
	  (compute-method-inheritance-list class old-local-supers))
	(new-mil
	  (compute-method-inheritance-list class
					   (class-local-supers class)))
	(discriminators ())
	(combined-discriminators ()))
    (dolist (old-donor old-mil)
      (when (setq discriminators (class-direct-discriminators old-donor))
	(dolist (old-discriminator discriminators)	  
	  (flush-discriminator-caches old-discriminator)
	  (when (methods-combine-p old-discriminator)
	    (pushnew old-discriminator combined-discriminators)))))
    (dolist (new-donor new-mil)
      (when (setq discriminators (class-direct-discriminators new-donor))
	(unless (memq new-donor old-mil)
	  (dolist (new-discriminator discriminators)
	    (when (methods-combine-p new-discriminator)
	      (pushnew new-discriminator combined-discriminators))
	    (flush-discriminator-caches new-discriminator)))))
    (when (fboundp 'combine-methods)		         ;***
      (COMBINE-METHODS CLASS COMBINED-DISCRIMINATORS)))) ;***


(defmeth discriminator-changed ((discriminator essential-discriminator)
				method
				added-p)
  (ignore method added-p)
  (make-discriminating-function discriminator)
  (flush-discriminator-caches discriminator))


(defun make-class-obsolete (class obsolete-class)
  (setf (class-wrapper-class (class-wrapper obsolete-class)) obsolete-class)
  (setf (class-wrapper class) nil)
  (setf (class-local-supers obsolete-class) (list class))
  (setf (class-class-precedence-list obsolete-class)
        (cons obsolete-class (class-class-precedence-list class)))
  (setf (class-name obsolete-class)
	(symbol-append "obsolete-" (class-name class)))
  (setf (iwmc-class-class-wrapper obsolete-class)
        (wrapper-of (class-named 'obsolete-class)))
  obsolete-class)

(defun copy-class (class) 
  (let* ((no-of-instance-slots (class-no-of-instance-slots (class-of class)))
         (new-class (%allocate-instance--class no-of-instance-slots)))
    (setf (iwmc-class-class-wrapper new-class)
	  (iwmc-class-class-wrapper class))
    (iterate ((i from 0 below no-of-instance-slots))
      (let ((index (%convert-slotd-position-to-slot-index i)))
	(setf (get-static-slot--class new-class index)			
	      (get-static-slot--class class index))))
    (setf (iwmc-class-dynamic-slots new-class)
          (copy-list (iwmc-class-dynamic-slots class)))
    new-class))

(defun wrapper-of (class)
  (or (class-wrapper class)
      (setf (class-wrapper class) (make-class-wrapper class))))

(defmeth collect-slotds ((class basic-class) local-slots cpl)
  (let ((slots ()))
    (flet ((add-slotd (slotd)
	     (let ((entry
		     (or (assq (slotd-name slotd) slots)
			 (progn (push (list (slotd-name slotd)) slots)
				(car slots)))))
	       (push slotd (cdr entry)))))
      (dolist (super (reverse (cdr cpl)))	;fix this consing later
	(dolist (super-slotd (class-local-slots super))
	  (add-slotd super-slotd)))

      (dolist (local-slotd local-slots)
	(add-slotd local-slotd)))
      
    ;; Now use compute-effective-slotd to condense all the
    ;; inherited slotds into the one effective slotd.
    (dolist (slot slots)
      (setf (car slot)
	    (compute-effective-slotd class (cdr slot))))
    ;; Now we need to separate it back out into instance and non-instance
    ;; slots.
    (let ((instance ())
	  (non-instance ()))
      (dolist (slot slots)
	(setq slot (car slot))
	(if (eq (slotd-allocation slot) ':instance)
	    (push slot instance)
	    (push slot non-instance)))
      (values instance non-instance slots))))

(defmethod compute-effective-slotd ((class class) slotds)
  (ignore class)
  (let ((slotd  (if (null (cdr slotds))
		    (car slotds)
		    (copy-slotd (car slotds)))))
    (flet ((merge-values (default type read-only accessor allocation)
	     (macrolet ((merge-value (name value)
			  `(when (eq (,name slotd) *slotd-unsupplied*)
			     (setf (,name slotd) ,value))))
	       (merge-value slotd-default default)
	       (merge-value slotd-type type)
	       (merge-value slotd-read-only read-only)
	       (merge-value slotd-accessor accessor)
	       (merge-value slotd-allocation allocation))))
      (dolist (s (cdr slotds))
	(merge-values (slotd-default s)
		      (slotd-type s)
		      (slotd-read-only s)
		      (slotd-accessor s)
		      (slotd-allocation s)))
      (merge-values 'nil      ;default value -- for now
		    't        ;type
		    'nil      ;read-only
		    'nil      ;accessor
		  :instance)) ;allocation
	slotd))

(defmethod compute-class-precedence-list ((root class))
  #+Lucid (declare (optimize (speed 0) (safety 3)))
  (let ((*cpl* ())
	(*root* root)
	(*must-precede-alist* ()))
    (declare (special *cpl* *root* *must-precede-alist*))
    ;; We start by computing two values.
    ;;   CPL
    ;;     The depth-first left-to-right up to joins walk of the supers tree.
    ;;     This is equivalent to breadth-first left-to-right walk of the
    ;;     tree with all but the last occurence of a class removed from
    ;;     the resulting list.  This is in fact how the walk is implemented.
    ;;
    ;;   MUST-PRECEDE-ALIST
    ;;     An alist of the must-precede relations. The car of each element
    ;;     of the must-precede-alist is a class, the cdr is all the classes
    ;;     which either:
    ;;       have this class as a local super
    ;;      or
    ;;       appear before this class in some other class's local-supers.
    ;;
    ;;     Thus, the must-precede-alist reflects the two constraints that:
    ;;       1. A class must appear in the CPL before its local supers.
    ;;       2. Order of local supers is preserved in the CPL.
    ;;
    (labels
   ;(flet
       (
;	(walk-supers (class &optional precedence)
;	  (let ((elem (assq class must-precede-alist)))
;	    (if elem
;		(setf (cdr elem) (union (cdr elem) precedence))
;		(push (cons class precedence) must-precede-alist)))
;	  (let ((rsupers (reverse (cons class (class-local-supers class)))))
;	    (iterate ((sup in rsupers)
;		      (pre on (cdr rsupers))
;		      (temp = nil))
;	      ;; Make sure this element of supers is OK.
;	      ;;  Actually, there is an important design decision hidden in
;	      ;;  here.  Namely, at what time should symbols in a class's
;	      ;;  local-supers be changed to the class objects they are
;	      ;;  forward referencing.
;	      ;;   1. At first make-instance (compute-class-precedence-list)?
;	      ;;   2. When the forward referenced class is first defined?
;	      ;;  This code does #1.
;	      (cond ((classp sup))
;		    ((and (symbolp sup)
;			  (setq temp (class-named sup t)))
;		     ;; This is a forward reference to a class which is
;		     ;; now defined.  Replace the symbol in the local
;		     ;; supers with the actual class object, and set sup.
;		     (nsubst temp sup (class-local-supers class))
;		     (setq sup temp))
;		    ((symbolp sup)
;		     (error "While computing the class-precedence-list for ~
;                             the class ~S.~%~
;                             The class ~S (from the local supers of ~S) ~
;                             is undefined."
;			    (class-name root) sup (class-name class)))
;		    (t
;		     (error "INTERNAL ERROR --~%~
;                             While computing the class-precedence-list for ~
;                             the class ~S,~%~
;                             ~S appeared in the local supers of ~S."
;			    root sup class)))
;	      (walk-supers sup pre))
;	    (unless (memq class cpl) (push class cpl))))
	(must-move-p (element list &aux move)
	  (dolist (must-precede (cdr (assq element *must-precede-alist*)))
	    (when (setq move (memq must-precede (cdr list)))
	      (return move))))
	(find-farthest-move (element move)
	  (let ((closure (compute-must-precedes-closure element)))
	    (dolist (must-precede closure)
	      (setq move (or (memq must-precede move) move)))
	    move))
	(compute-must-precedes-closure (class)
	  (let ((closure ()))
	    (labels ((walk (element path)
		       (when (memq element path)
			 (class-ordering-error
			   *root* element path *must-precede-alist*))
		       (dolist (precede
				 (cdr (assq element
					    *must-precede-alist*)))
			 (unless (memq precede closure)
			   (pushnew precede closure)
			   (walk precede (cons element path))))))
	      (walk class nil)
	      closure))))
      
      (walk-supers *root*)			;Do the walk
      ;; For each class in the cpl, make sure that there are no classes after
      ;; it which should be before it.  We do this by cdring down the list,
      ;; making sure that for each element of the list, none of its
      ;; must-precedes come after it in the list. If we find one, we use the
      ;; transitive closure of the must-precedes (call find-farthest-move) to
      ;; see where the class must really be moved. We use a hand-coded loop
      ;; so that we can splice things in and out of the CPL as we go.
      (let ((tail *cpl*)
	    (element nil)
	    (move nil))
	(loop (when (null tail) (return))
	      (setq element (car tail)
		    move (must-move-p element tail))
	      (cond (move
		     (setq move (find-farthest-move element move))
		     (setf (cdr move) (cons element (cdr move)))
		     (setf (car tail) (cadr tail)) ;Interlisp delete is OK
		     (setf (cdr tail) (cddr tail)) ;since it will never be
						   ;last element of list.
		     )
		    (t
		     (setq tail (cdr tail)))))
	(copy-list *cpl*)))))

(defun walk-supers (class &optional precedence)
  (declare (special *cpl* *root* *must-precede-alist*))
  (let ((elem (assq class *must-precede-alist*)))
    (if elem
	(setf (cdr elem) (union (cdr elem) precedence))
	(push (cons class precedence) *must-precede-alist*)))
  (let ((rsupers (reverse (cons class (class-local-supers class)))))
    (iterate ((sup in rsupers)
	      (pre on (cdr rsupers))
	      (temp = nil))
      ;; Make sure this element of supers is OK.
      ;;  Actually, there is an important design decision hidden in
      ;;  here.  Namely, at what time should symbols in a class's
      ;;  local-supers be changed to the class objects they are
      ;;  forward referencing.
      ;;   1. At first make-instance (compute-class-precedence-list)?
      ;;   2. When the forward referenced class is first defined?
      ;;  This code does #1.
      (cond ((classp sup))
	    ((and (symbolp sup)
		  (setq temp (class-named sup t)))
	     ;; This is a forward reference to a class which is
	     ;; now defined.  Replace the symbol in the local
	     ;; supers with the actual class object, and set sup.
	     (nsubst temp sup (class-local-supers class))
	     (setq sup temp))
	    ((symbolp sup)
	     (error "While computing the class-precedence-list for ~
                             the class ~S.~%~
                             The class ~S (from the local supers of ~S) ~
                             is undefined."
		    (class-name *root*) sup (class-name class)))
	    (t
	     (error "INTERNAL ERROR --~%~
                             While computing the class-precedence-list for ~
                             the class ~S,~%~
                             ~S appeared in the local supers of ~S."
		    *root* sup class)))
      (walk-supers sup pre))
    (unless (memq class *cpl*) (push class *cpl*))))

(defun class-ordering-error (root element path must-precede-alist)
  (ignore root)
  (setq path (cons element (reverse (memq element (reverse path)))))
  (flet ((pretty (class) (or (class-name class) class)))
    (let ((explanations ()))
      (do ((tail path (cdr tail)))
	  ((null (cdr tail)))
	(let ((after (cadr tail))
	      (before (car tail)))
	  (if (memq after (class-local-supers before))
	      (push (format nil
			    "~% ~A must precede ~A -- ~
                              ~A is in the local supers of ~A."
			    (pretty before) (pretty after)
			    (pretty after) (pretty before))
		    explanations)
	      (dolist (common-precede
			(intersection
			  (cdr (assq after must-precede-alist))
			  (cdr (assq before must-precede-alist))))
		(when (memq after (memq before
					(class-local-supers common-precede)))
		  (push (format nil
				"~% ~A must precede ~A -- ~
                                  ~A has local supers ~S."
				(pretty before) (pretty after)
				(pretty common-precede)
				(mapcar #'pretty
					(class-local-supers common-precede)))
			explanations))))))
      (error "While computing the class-precedence-list for the class ~A:~%~
              There is a circular constraint through the classes:~{ ~A~}.~%~
              This arises because:~{~A~}"
	     (pretty root)
	     (mapcar #'pretty path)
	     (reverse explanations)))))

(defmeth compute-method-inheritance-list ((class essential-class)
					  local-supers)
  (compute-class-precedence-list class))

(defmeth compatible-meta-class-change-p (class proto-new-class)
  (eq (class-of class) (class-of proto-new-class)))

(defmeth check-super-metaclass-compatibility (class new-super)
  (unless (eq (class-of class) (class-of new-super))
    (error "The class ~S was specified as a~%super-class of the class ~S;~%~
            but the meta-classes ~S and~%~S are incompatible."
	   new-super class (class-of new-super) (class-of class))))

(defun classp (x)
  (and (iwmc-class-p x) (typep--class x 'essential-class)))



(defmeth class-standard-constructor ((class basic-class))
  (dolist (constructor (ds-options-constructors (class-ds-options class)))
    (when (null (cdr constructor)) (return (car constructor)))))


(defmeth flush-class-caches ((class basic-class))
  (let ((wrapper (class-wrapper class)))
    (and wrapper (flush-class-wrapper-cache wrapper))
    (iterate ((subclass in (class-direct-subclasses class)))
      (flush-class-caches subclass))))


  ;;   
;;;;;; CHANGE-CLASS
  ;;   

(defun change-class (object new-class)
  (or (classp new-class)
      (setq new-class (class-named new-class)))
  (let ((new-object (make new-class)))
    ;; Call change-class-internal so that a user-defined method
    ;; (or the default method) can copy the information from the
    ;; old instance to the dummy instance of the new class.
    (change-class-internal object new-object)
    ;; Now that the dummy new-object has the right information,
    ;; move all that stuff into the old-instance.
    (setf (iwmc-class-class-wrapper object)
	  (wrapper-of new-class))
    (setf (iwmc-class-static-slots object)
	  (iwmc-class-static-slots new-object))
    (setf (iwmc-class-dynamic-slots object)
	  (iwmc-class-dynamic-slots new-object))
    object))

(defmeth change-class-internal ((old object) (new object))
  (let ((all-slots (all-slots old)))
    (iterate ((name in all-slots by cddr)
              (value in (cdr all-slots) by cddr))
      (put-slot-always new name value))))

  ;;   
;;;;;; WITH-SLOTS
  ;;

(define-method-body-macro with-slots (instance-forms-and-options
				       &body body
				       &environment env)
  :global (expand-with-slots nil nil instance-forms-and-options env body)
  :method (expand-with-slots (macroexpand-time-generic-function
			       macroexpand-time-environment)
			     (macroexpand-time-method
			       macroexpand-time-environment)
			     instance-forms-and-options
			     env
			     body))

(defun expand-with-slots (proto-discriminator proto-method first-arg env body)
  (ignore proto-discriminator)
  (setq first-arg (iterate ((arg in first-arg))
		    (collect (if (listp arg) arg (list arg)))))
  (let ((entries (expand-with-make-entries proto-method first-arg))
	(gensyms ()))
    (dolist (arg first-arg)
      (push (list (if (listp arg) (car arg) arg)
		  (gensym))
	    gensyms))
    `(let ,(mapcar #'reverse gensyms)
       ,(walk-form (cons 'progn body)
	  :environment env
	  :walk-function
	  #'(lambda (form context &aux temp)
	      (cond ((and (symbolp form)
			  (eq context ':eval)
			  (null (variable-lexical-p form))
			  (null (variable-special-p form))
			  (setq temp (assq form entries)))
		     (if (car (cddddr temp))	;use slot-value?
			 (let ((get-slot 
				 `(get-slot ,(cadr (assq (cadr temp) gensyms))
					    ',(slotd-name (cadddr temp)))))
			   (optimize-get-slot (caddr temp)
					      get-slot))
			 `(,(slotd-accessor (cadddr temp))
			   ,(cadr (assq (cadr temp) gensyms)))))
		    ((and (listp form)
			  (or (eq (car form) 'setq)
			      (eq (car form) 'setf)))
		     (cond ((cdddr form)
			    (cons 'progn
				  (iterate ((pair on (cdr form) by cddr))
				    (collect (list (car form)
						   (car pair)
						   (cadr pair))))))
			   ((setq temp (assq (cadr form) entries))
			    (if (car (cddddr temp))
				(let ((get-slot 
					`(setf-of-get-slot
					   ,(cadr (assq (cadr temp) gensyms))
					   ',(slotd-name (cadddr temp))
					   ,(caddr form))))
				  (optimize-setf-of-get-slot (caddr temp)
							     get-slot))
				`(setf (,(slotd-accessor (cadddr temp))
					,(cadr (assq (cadr temp) gensyms)))
				       ,(caddr form))))
			   (t form)))
		    (t form)))))))

;;; Returns an alist of the form:
;;; 
;;;   (<prefix+slot-name> <instance-form> <class> <slotd> <use-slot-value-p>)
;;;
(defmeth expand-with-make-entries (method first-arg)
  (let* ((entries ())
         (method-arguments
	   (when (method-p method)
	     (iterate ((arg in (method-arglist method))
		       (spec in (method-type-specifiers method)))
	       (when (classp spec) (collect (cons arg spec)))))))
    (iterate ((instance-and-keys in first-arg))
      (keyword-bind ((use-slot-value nil)
		     (class nil class-specified-p)
		     (prefix nil prefix-specified-p))
		    (cdr instance-and-keys)
	(let ((instance (car instance-and-keys)))
	  (setq class
		(or (and class-specified-p
			 (or (class-named class t)
			     (error "In WITH-SLOTS the class specified for ~
                                     ~S, ~S ~%~
                                     is not the name of a defined class."
				    instance class)))
		    (cdr (assq instance method-arguments))
		    (error "The class of (the value of) ~S was not given in ~
                           in the call to with-slots and could not be ~
                           inferred automatically."
			  instance)))
	  (iterate ((slotd in (class-slots class)))
	    (push (list (if (null prefix-specified-p)
			    (slotd-name slotd)
			    (intern (string-append prefix
						   (slotd-name slotd))
				    (symbol-package
				      (if (symbolp prefix)
					  prefix
					  (slotd-name slotd)))))
			instance
			class
			slotd
			use-slot-value)
		  entries)))))
    entries))


(defun named-object-print-function (instance stream depth
					     &optional (extra nil extra-p))
  (ignore depth)
  (printing-random-thing (instance stream)
    ;; I know I don't have to do this this way.  I know I
    ;; could use ~[~;~], but how many Common Lisps do you
    ;; think have that completely debugged?
    (if extra-p					
	(format stream "~A ~S ~:S"
		(capitalize-words (class-name (class-of instance)))
		(get-slot instance 'name)
		extra)
	(format stream "~A ~S"
		(capitalize-words (class-name (class-of instance)))
		(get-slot instance 'name)))))

