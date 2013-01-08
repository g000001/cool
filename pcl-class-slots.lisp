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

(in-package :cool.pcl)

  ;;   
;;;;;; Slot access for the class class.
  ;;   get-slot-using-class and friends
;;; At last the meta-braid is up.  The method class-instance-slots exists and there
;;; is peace in the land.  Now we can finish get-slot, put-slot and friends.

(defmacro get-slot-using-class--class (class object slot-name
                                       dont-call-slot-missing-p default)
  (once-only (slot-name)
    `(let* ((.wrapper.
	      (iwmc-class-class-wrapper ,object))
            (.get-slot-offset.
	      (class-wrapper-get-slot-offset .wrapper. ,slot-name)))
       (if (eq (class-wrapper-cached-key .wrapper. .get-slot-offset.)
	       ,slot-name)
           (get-static-slot--class
             ,object (class-wrapper-cached-val .wrapper. .get-slot-offset.))
           (get-slot-using-class--class-internal
             ,class ,object ,slot-name ,dont-call-slot-missing-p ,default)))))


(defmacro put-slot-using-class--class (class object slot-name new-value
                                       dont-call-slot-missing-p)
  (once-only (slot-name)
    `(let* ((.wrapper. (iwmc-class-class-wrapper ,object))
            (.get-slot-offset. (class-wrapper-get-slot-offset .wrapper. ,slot-name)))
       (if (eq (class-wrapper-cached-key .wrapper. .get-slot-offset.) ,slot-name)
           (setf (get-static-slot--class
                   ,object (class-wrapper-cached-val .wrapper. .get-slot-offset.))
                 ,new-value)
            (put-slot-using-class--class-internal
              ,class ,object ,slot-name ,new-value ,dont-call-slot-missing-p)))))

(defmacro get-slot--class (object slot-name)
  (once-only (object)
    `(get-slot-using-class--class
       (class-of--class ,object) ,object ,slot-name () ())))

(defmacro put-slot--class (object slot-name new-value)
  (once-only (object)
    `(put-slot-using-class--class
       (class-of--class ,object) ,object ,slot-name ,new-value ())))

(defmeth get-slot-using-class ((class basic-class) object slot-name
			       &optional dont-call-slot-missing-p default)
  (get-slot-using-class--class
    class object slot-name dont-call-slot-missing-p default))

(defmeth put-slot-using-class ((class basic-class) object slot-name new-value
			       &optional dont-call-slot-missing-p)
  (put-slot-using-class--class
    class object slot-name new-value dont-call-slot-missing-p))

#|(defmeth remove-dynamic-slot-using-class ((class basic-class)
					  object slot-name)
  (declare (ignore class))
  (remove-dynamic-slot--class object slot-name))|#

;;;
;;; with-slot-internal--class is macro which makes code which accesses the
;;; slots of instances with meta-class class more readable.  The macro itself
;;; is kind of dense though.  In the following call:
;;;   (WITH-SLOT-INTERNAL--CLASS (CLASS OBJECT SLOT-NAME T)
;;;     (:INSTANCE (INDEX) . instance-case-code)
;;;     (:DYNAMIC (LOC NEWP) . dynamic-case-code)
;;;     (:CLASS (SLOTD) . class-case-code)
;;;     (NIL () . nil-case-code))
;;; If the slot is found and has allocation:
;;;   :instance   instance-case-code is evaluated with INDEX bound to the
;;;               index of the slot.
;;;   :dynamic    dynamic-case-code is evaluated with LOC bound to the cons
;;;               whose car holds the value of this dynamic slot, and NEWP
;;;               bound to t if the slot was just created and nil otherwise.
;;;   :class      class-case-code is evaluated with slotd bound to the slotd
;;;               of the slot.
;;; If the slot is not found.
;;;   If createp is t it is created and things proceed as in the allocation
;;;   :dynamic case.
;;; Otherwise, and if the allocation is nil the nil-case code is evaluated.
;;;               
(defmacro with-slot-internal--class ((class object slot-name createp)
				     &body cases)
  (let ((temp1 (gensym))
        (temp2 (gensym))
        (createp-var (gensym))
        (instance-case (cdr (assq :instance cases)))
        (dynamic-case (cdr (assq :dynamic cases)))
        (class-case (cdr (assq :class cases)))
        (nil-case (cdr (assq nil cases))))
    `(prog (,temp1                              ;The Horror! Its a PROG,
            ,temp2                              ;but its in a macro so..
            (,createp-var ,createp))
         (cond
           ((setq ,temp1 (slotd-position ,slot-name
					 (class-instance-slots ,class)))
            ;; We have the slots position in the instance slots.  Convert
	    ;; that to the slots index and then cache the index and return
	    ;; the result of evaluating the instance-case.
            (setq ,temp1 (%convert-slotd-position-to-slot-index ,temp1))
            (let ((wrapper (validate-class-wrapper ,object)))
              (class-wrapper-cache-cache-entry
                wrapper
                (class-wrapper-get-slot-offset wrapper ,slot-name)
                ,slot-name
                ,temp1))
            (return (let ,(and (car instance-case)
			       `((,(caar instance-case) ,temp1)))
                      . ,(cdr instance-case))))
           ((setq ,temp1 (slotd-assoc ,slot-name
				      (class-non-instance-slots ,class)))
            ;; We have a slotd -- this is some sort of declared slot.
            (ecase (slotd-allocation ,temp1)
              (:class      (return
                             (let ,(and (car class-case)
                                        `((,(caar class-case) ,temp1)))
                               . ,(cdr class-case))))
              ((:none nil) (go nil-case))
              (:dynamic    (setq ,createp-var :dynamic
                                 ,temp2       (slotd-default ,temp1))))))
         ;; When we get here, either:
         ;;  - we didn't find a slot-description for this slot, so try to
         ;;    find it in the dynamic slots creating it if createp-var is
         ;;    non-null.
         ;;  - we found a :dynamic slot-description, createp-var got set
         ;;    to :dynamic and we dropped through to here where we try
         ;;    to find the slot.  If we find it we return the loc.  If
         ;;    not we create it and initialize it to its default value.
         (multiple-value-setq (,temp1 ,createp-var)
           (dynamic-slot-loc--class ,object ,slot-name ,createp-var))
         (when ,temp1
           (when (and ,createp-var ,temp2)
             (setf (car ,temp1) (eval ,temp2)))
           (let
             (,@(and (caar dynamic-case) `((,(caar dynamic-case) ,temp1)))
              ,@(and (cadar dynamic-case) `((,(cadar dynamic-case)
					     ,createp-var))))
             (return . ,(cdr dynamic-case))))
      nil-case
         ;; This slot is either explicitly declared :allocation nil (we
         ;; jumped here by (GO NIL-CASE) or there is no declaration for
         ;; this slot and we didn't find it in the dynamic-slots, we fell
         ;; through from the dynamic lookup above.
         (let ,(and (car nil-case) `((,(caar nil-case) ,temp1)))
           . ,(cdr nil-case)))))

(defun dynamic-slot-loc--class (object slot-name createp)
  (let ((plist (iwmc-class-dynamic-slots object)))
    (or (iterate ((prop on plist by cddr))
          (when (eq (car prop) slot-name) (return (cdr prop))))
        (and createp
             (values (cdr (setf (iwmc-class-dynamic-slots object)
                                (list* slot-name () plist)))
                     createp)))))

(defun get-slot-using-class--class-internal (class object slot-name
                                                   dont-call-slot-missing-p
						   default)
  (with-slot-internal--class (class object slot-name nil)
    (:instance (index) (get-static-slot--class object index))
    (:dynamic (loc newp) (if (eq newp t) (setf (car loc) default) (car loc)))
    (:class (slotd) (slotd-default slotd))
    (nil () (unless dont-call-slot-missing-p
	      (slot-missing object slot-name)))))

(defun put-slot-using-class--class-internal (class object slot-name new-value
                                                   dont-call-slot-missing-p)
  (with-slot-internal--class
	  (class object slot-name dont-call-slot-missing-p)
    (:instance (index) (setf (get-static-slot--class object index)
			     new-value))
    (:dynamic (loc) (setf (car loc) new-value))
    (:class (slotd) (setf (slotd-default slotd) new-value))
    (nil () (unless dont-call-slot-missing-p
	      (slot-missing object slot-name)))))

(defun all-slots (object)
  (all-slots-using-class (class-of object) object))

(defmeth all-slots-using-class ((class basic-class) object)
  (append (iterate ((slotd in (class-instance-slots class)))
            (collect (slotd-name slotd))
            (collect (get-slot--class object (slotd-name slotd))))
          (iwmc-class-dynamic-slots object)))

;;; --- TODO: twicep?
(defmeth remove-dynamic-slot-using-class ((class basic-class) object
							      slot-name)
  (declare (ignore class))
  (remove-dynamic-slot--class object slot-name))

(defun slot-allocation (object slot-name)
  (slot-allocation-using-class (class-of object) object slot-name))

(defmeth slot-allocation-using-class ((class basic-class) object slot-name)
  (with-slot-internal--class (class object slot-name nil)
    (:instance () :instance)
    (:dynamic () :dynamic)
    (:class () :class)
    (nil    () nil)))

(defun slot-exists-p (object slot-name)
  (let* ((flag "")
         (val
	   (get-slot-using-class (class-of object) object slot-name t flag)))
    (neq val flag)))

(defmeth slot-missing (object slot-name)
  (error "The slot: ~S is missing from the object: ~S" slot-name object))

(defmacro typep--class (iwmc-class type)
  `(not (null (memq (class-named ,type ())
                    (class-class-precedence-list 
                      (class-wrapper-class
                        (iwmc-class-class-wrapper ,iwmc-class)))))))

(defmacro type-of--class (iwmc-class)
  `(class-name
     (class-wrapper-wrapped-class (iwmc-class-class-wrapper ,iwmc-class))))

(defun subclassp (class1 class2)
  (or (classp class1) (setq class1 (class-named class1)))
  (or (classp class2) (setq class2 (class-named class2)))
  (memq class2 (class-class-precedence-list class1)))

(defun sub-class-p (x class)
  (if (symbolp class) (setq class (class-named class)))
  (not (null (memq class (class-class-precedence-list (class-of x))))))


(defmeth class-has-instances-p ((class basic-class))
  (class-wrapper class))

(defmeth make-instance ((class basic-class))
  (let ((class-wrapper (class-wrapper class)))
    (if class-wrapper                           ;Are there any instances?
        ;; If there are instances, the class is OK, just go ahead and
        ;; make the instance.
        (let ((instance (%allocate-instance--class
                          (class-no-of-instance-slots class))))
          (setf (iwmc-class-class-wrapper instance) class-wrapper)
          instance)
        ;; Do first make-instance-time error-checking, build the class
        ;; wrapper and call ourselves again to really build the instance.
        (progn
          ;; no first time error checking yet.
          (setf (class-wrapper class) (make-class-wrapper class))
          (make-instance class)))))

(defun make (class &rest init-plist)
  (when (symbolp class) (setq class (class-named class)))
  (let ((object (make-instance class)))
    (initialize object init-plist)
    object))

(defmeth initialize ((object object) init-plist)
  (initialize-from-defaults object)
  (initialize-from-init-plist object init-plist))

(defmeth initialize-from-defaults ((self object))
  (iterate ((slotd in (class-instance-slots (class-of self))))
    (setf (get-slot self (slotd-name slotd)) (eval (slotd-default slotd)))))

(defmeth initialize-from-init-plist ((self object) init-plist)
  (when init-plist
    (let* ((class (class-of self))
	   (instance-slots (class-instance-slots class))
	   (non-instance-slots (class-non-instance-slots class)))
      (flet ((find-slotd (keyword)
	       (flet ((find-internal (slotds)
			(dolist (slotd slotds)
			  (when (eq (slotd-keyword slotd) keyword)
			    (return slotd)))))
		 (or (find-internal instance-slots)
		     (find-internal non-instance-slots)))))
	(do* ((keyword-loc init-plist (cdr value-loc))
	      (value-loc (cdr keyword-loc) (cdr keyword-loc))
	      (slotd () ())
	      (allow-other-keys-p () allow-other-keys-p))
	     (())
	  (flet ((allow-other-keywords-p ()
		   (when (null allow-other-keys-p)
		     (setq allow-other-keys-p
			   (do ((loc keyword-loc (cddr loc)))
			       ((null loc) 0)
			     (when (eq (car loc) ':allow-other-keys)
			       (if (cadr loc) 1 0)))))
		   (if (= allow-other-keys-p 1) t nil)))
	    (cond ((null keyword-loc) (return nil))
		  ((eq (car keyword-loc) :allow-other-keys)
		   (setq allow-other-keys-p
			 (if (cadr keyword-loc) 1 0)))
		  ((null value-loc)
		   (error "No value supplied for the init-keyword ~S."
			  (car keyword-loc)))
		  ((null (setq slotd (find-slotd (car keyword-loc))))
		   (unless (allow-other-keywords-p)
		     (error "~S is not a valid keyword in the init-plist."
			    (car keyword-loc))))
		  (t
		   (setf (get-slot self (slotd-name slotd))
			 (car value-loc))))))))))



(defmeth class-default-includes ((class basic-class))
  (declare (ignore class))
  (list 'object))

