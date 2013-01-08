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
;;; Testing code.
;;;

(in-package :cool.pcl)

;;; Because CommonLoops runs in itself so much, the notion of a test file for
;;; it is kind of weird.
;;;
;;; If all of PCL loads then many of the tests in this file (particularly
;;; those at the beginning) are sure to work.  Those tests exists primarily
;;; to help debug things when low-level changes are made to PCL, or when a
;;; particular port customizes low-level code.
;;;
;;; Some of the other tests are "real" in the sense that they test things
;;; that PCL itself does not use, so might be broken.
;;; 
;;; NOTE:
;;;   The tests in this file do not appear in random order!  They
;;;   depend on state  which has already been set up in order to run.
;;;
;;;   As a convention foo, bar and baz are used for classes and
;;;   discriminators which are just for the current test.  By
;;;   default, do-test resets those names before running the current
;;;   test.  Other names like x, y, z, method-1... are used to name
;;;   classes and discriminators which last the life of the file.
;;; 

(defvar *without-errors*
	(or #+Symbolics #'(lambda (form)
			    `(multiple-value-bind (.values. .errorp.)
				 (si::errset ,form nil)
			       (declare (ignore .values.))
			       .errorp.))
	    #+Xerox     #'(lambda (form)
			    `(xcl:condition-case (progn ,form nil)
			       (error () t)))
	    
	    nil))

(defmacro without-errors (&body body)
  (if *without-errors*
      (funcall *without-errors* `(progn ,@body))
      (error "Calling WITHOUT-ERRORS when *without-errors* is nil.")))

#-HP
(defmacro do-test (name&options &body body)
  (let ((name (if (listp name&options) (car name&options) name&options))
	(options (if (listp name&options) (cdr name&options) ())))
    (keyword-bind ((clear t)
		   (should-error nil))
		  options
      (cond ((and should-error (null *without-errors*))
	     `(format t
		"~&Skipping testing ~A,~%~
	         because can't ignore errors in this Common Lisp."
		',name))
	    (t
	     `(progn
		(format t "~&Testing ")
		(format t ,name)
		(format t "... ")
		,(when clear
		   '(progn (dolist (x '(foo bar baz))
			     (setf (discriminator-named x) nil)
			     (fmakunbound x)
			     (setf (class-named x) nil))))
		(if ,(if should-error
			 `(without-errors (progn ,@body))
			 `(progn ,@body))
		    (format t "OK")
		    (progn (format t "FAILED")
			   (error "Test Failed: ~A" ',name)))))))))

#+HP (defmacro do-test (name&options &body body)
  (let ((name (if (listp name&options) (car name&options) name&options))
	(options (if (listp name&options) (cdr name&options) ())))
    (keyword-bind ((clear t)
		   (should-error nil))
		  options
      (cond ((and should-error (null *without-errors*))
	     `(format t
		"~&Skipping testing ~A,~%~
	         because can't ignore errors in this Common Lisp."
		',name))
	    (t
	     `(progn
		(format t "~&Testing ~A..." ,name)
		,(when clear
		   '(progn (dolist (x '(foo bar baz))
			     (setf (discriminator-named x) nil)
			     (fmakunbound x)
			     (setf (class-named x) nil))))
		
		 ,@(butlast body)
		 (if ,(if should-error
			 `(without-errors (progn ,@body))
			 `(progn ,@(last body)))
		    (format t "OK")
		    (progn (format t "FAILED")
			   (error "Test Failed: ~A" ',name)))))))))

(defun permutations (elements length)
  (if (= length 1)
      (iterate ((x in elements)) (collect (list x)))
      (let ((sub-permutations (permutations elements (- length 1))))
        (iterate ((x in elements))
          (join (iterate ((y in sub-permutations))
                  (collect (cons x y))))))))

  ;;   
;;;;;; 
  ;;   


(cltl1-eval-when (load eval)
  (format t "~&~%~%Testing Extremely low-level stuff..."))

(do-test ("Memory Block Primitives" :clear nil)
  (let ((block (make-memory-block 10))
        (tests (iterate ((i from 0 below 10)) (collect (make-list 1)))))
    (and (numberp (memory-block-size block))
         (= (memory-block-size block) 10)
         (progn (iterate ((i from 0) (test in tests))
                  (setf (memory-block-ref block i) test))
                (iterate ((i from 0) (test in tests))
                  (unless (eq (memory-block-ref block i) test) (return nil))
                  (finally (return t)))))))

(do-test ("Class Wrapper Caching" :clear nil)
  (let* ((wrapper (make-class-wrapper 'test))
         (offset (class-wrapper-get-slot-offset wrapper 'foo))
         (value (list ())))
    
    (and (eq 'foo  (setf (class-wrapper-cached-key wrapper offset) 'foo))
         (eq value (setf (class-wrapper-cached-val wrapper offset) value))
         (eq 'foo  (class-wrapper-cached-key wrapper offset))
         (eq value (class-wrapper-cached-val wrapper offset)))))

(do-test ("Flushing Class-Wrapper caches" :clear nil)
  (let* ((wrapper (make-class-wrapper 'test))
         (offset (class-wrapper-get-slot-offset wrapper 'foo)))
    (setf (class-wrapper-cached-key wrapper offset) 'foo)
    (flush-class-wrapper-cache wrapper)
    (neq 'foo  (class-wrapper-cached-key wrapper offset))))

(do-test "Class Wrapper Caching"
  (let ((slots '(;; Some random important slots.
		 name class-wrapper class-precedence-list
		 direct-supers direct-subclasses direct-methods
		 no-of-instance-slots instance-slots
		 local-supers
		 non-instance-slots local-slots  prototype))
	(wrapper (make-class-wrapper 'test))
	(hits 0))
    (iterate ((slot in slots))
      (let ((offset (class-wrapper-get-slot-offset wrapper slot)))
	(setf (class-wrapper-cached-key wrapper offset) slot)))
    (iterate ((slot in slots))
      (let ((offset (class-wrapper-get-slot-offset wrapper slot)))
	(and (eq (class-wrapper-cached-key wrapper offset) slot)
	     (incf hits))))
    (format t
	    " (~D% hit) "
	    (* 100.0 (/ hits (float (length slots)))))
    t))

;(do-test "static slot-storage"
;  (let ((static-slots (%allocate-static-slot-storage--class 5)))
;    (iterate ((i from 0))
;      (when (= i 5) (return t))
;      (let ((cons (list ()))
;            (index (%convert-slotd-position-to-slot-index i)))
;        (setf (%static-slot-storage-get-slot--class static-slots index) cons)
;        (or (eq cons
;		(%static-slot-storage-get-slot--class static-slots index))
;            (return nil))))))


(cltl1-eval-when (load eval) (format t "~&~%~%Testing High-Level stuff..."))



(defvar *built-in-classes*
        '((T              T)
          (NUMBER         1)
          (RATIO       1/2                          1/2)
          (COMPLEX)
          (INTEGER        1)
          (RATIO)
          (FIXNUM         most-positive-fixnum         most-positive-fixnum)
          (BIGNUM         (+ most-positive-fixnum 1)   (+ most-positive-fixnum 1)) 
          SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT
          (FLOAT          1.1)
          (NULL           ()                           ())
          (STANDARD-CHAR  #\a)
          (STRING-CHAR    #\a)
          (CHARACTER      #\a                          #\a)
          BIT-VECTOR
          (STRING         (make-string 1)              (make-string 1))
          (ARRAY          (make-array 1))
          SIMPLE-ARRAY SIMPLE-VECTOR SIMPLE-STRING SIMPLE-BIT-VECTOR
          (VECTOR         (make-string 1))
          (VECTOR         (make-array 1))
          (LIST           '(1 2 3))
          (SEQUENCE       (make-string 1))
          (SEQUENCE       (make-array 1))
          (SEQUENCE       (make-list 1))                             
          (HASH-TABLE     (make-hash-table :size 1)    (make-hash-table :size 1))
          (READTABLE      *readtable*                  *readtable*)
          (PACKAGE        *package*                    *package*)
          (PATHNAME       (make-pathname :name "foo")  (make-pathname :name "foo"))
          (STREAM         *terminal-io*                *terminal-io*)
          (RANDOM-STATE   (make-random-state)          (make-random-state))
          (CONS           (cons 1 2)                   (cons 1 2))
          (SYMBOL         'foo                         'foo)
          COMMON))

(do-test "existence of built-in classes"
  (not (dolist (entry *built-in-classes*)
         (let ((type (if (listp entry) (car entry) entry)))
           (or (class-named type t)
               (progn (format t "Missing the built-in class named: ~S" type)
                      (return t)))))))  

;;; See how CLASS-OF works.
;(cltl1-eval-when (load eval)
;  (format t "~%Check to see how well portable CLASS-OF works... ")
;  (let ((lost ()))
;    (dolist (entry *built-in-classes*)
;      (or (not (listp entry))
;	  (null (cddr entry))
;	  (let* ((thing (eval (caddr entry)))
;		 (class (class-of thing)))
;	    (and class (eq (class-name class) (car entry))))
;	  (progn (setq lost t)
;		 (format t
;			 "~&WARNING: Can't define methods on: ~S."
;			 (car entry)))))
;    (when lost (terpri) (terpri))
;    (format t "OK")))

(do-test "existence of discriminators for accessors of early classes"
  ;; Because accessors are done with add-method, and this has to be done
  ;; specially for early classes it is worth testing to make sure that
  ;; the discriminators got created for the accessor of early classes.
  (not
    (dolist (class '(t object essential-class class discriminator method))
      (setq class (class-named class))
      (or (not (dolist (slotd (class-instance-slots class))
                 (and (slotd-accessor slotd)
                      (or (discriminator-named (slotd-accessor slotd))
                          (return nil)))))
          (not (dolist (slotd (class-non-instance-slots class))
                 (and (slotd-accessor slotd)
                      (or (discriminator-named (slotd-accessor slotd))
                          (return nil)))))))))

(do-test "a simple defstruct"
  (ndefstruct (x (:class class))
    (a 1)
    (b 2))

  (and (fboundp 'make-x)
       (fboundp 'x-p)
       (fboundp 'copy-x)
       (fboundp 'x-a)
       (fboundp 'x-b)
       (typep--class (make-x) 'x)
       (x-p (make-x))
       (equal (x-a (make-x)) 1)
       (equal (x-a (make-x :a 3)) 3)
       (x-p (copy-x (make-x)))
       ))

(do-test "obsolete-class stuff"
  (and (class-named 'obsolete-class)
       (let ((old-x-class (class-named 'x))
             (old-x-instance (make-x)))
         
         (ndefstruct (x (:class class))
                     (a 3))
         (and (neq (class-of old-x-instance) (class-named 'x))
              (= (x-a old-x-instance) 1)))))

(do-test "multiple constructors"
  (ndefstruct (x (:class class)
                 (:constructor make-x)
                 (:constructor make-x-1 (a b)))
    a
    b)
  (and (fboundp 'make-x)
       (fboundp 'make-x-1)
       (equal (get-slot (make-x :a 1 :b 2) 'a) 1)
       (equal (get-slot (make-x :a 1 :b 2) 'b) 2)
       (equal (get-slot (make-x-1 2 1) 'a) 2)
       (equal (get-slot (make-x-1 2 1) 'b) 1)))

(do-test "the :print-function defstruct-option"

  (ndefstruct (x (:class class)
                 (:print-function x-print-function))
    a
    b)

  (defun x-print-function (object stream level)
    (when (and (x-p object)
               (streamp stream)                 ;Don't be breaking my test file
               (numberp level))                 ;because of your problems.
      (throw 'x 'x)))

  (eq (catch 'x (prin1 (make 'x))) 'x))

;;; ** need more tests in here,
;;; test the basic iwmc-class structure
;;; test class-wrappers some more
;;; 

;;; OK, now we know that simple defstruct works and that obsolete classes work.
;;; Now we set up some real simple classes that we can use for the rest of the
;;; file.
;;;
(ndefstruct (i (:class class)))                     ;(i ..)
(ndefstruct (j (:class class)))                     ;(j ..)
(ndefstruct (k (:class class)))                     ;(k ..)

(ndefstruct (l (:class class) (:include (i))))      ;(l i ..)
(ndefstruct (m (:class class) (:include (i j))))    ;(m i j ..)
(ndefstruct (n (:class class) (:include (k))))      ;(n k ..)

(ndefstruct (q (:class class) (:include (i))))      ;(q i ..)
(ndefstruct (r (:class class) (:include (m))))      ;(r m i j ..)
(ndefstruct (s (:class class) (:include (n i k))))  ;(s n i k ..)

(do-test "classical methods"
  
  (defmeth foo ((x i)) x 'i)  
  (defmeth foo ((x n)) x 'n)
  (defmeth foo ((x s)) x 's)

  (and (eq (foo (make-i)) 'i)
       (eq (foo (make-n)) 'n)
       (eq (foo (make-s)) 's)))

(do-test "run-super"

  (defmeth foo (o) o ())
  
  (defmeth foo ((o i)) o (cons 'i (run-super)))
  (defmeth foo ((o m)) o (cons 'm (run-super)))
  (defmeth foo ((o n)) o (cons 'n (run-super)))
  (defmeth foo ((o q)) o (cons 'q (run-super)))
  (defmeth foo ((o r)) o (cons 'r (run-super)))
  (defmeth foo ((o s)) o (cons 's (run-super)))

  (let ((i (make-i)) (m (make-m)) (q (make-q)) (r (make-r)) (s (make-s)))
    (and (equal (foo i) '(i))
         (equal (foo m) '(m i))
         (equal (foo q) '(q i))
         (equal (foo r) '(r m i))
         (equal (foo s) '(s n i)))))

(do-test "multi-methods when first 3 args are discriminated on"
  (let ((permutations (permutations '(i n r) 3)))
    (mapcar #'(lambda (p)
                (EVAL `(defmeth foo ,(mapcar 'list '(x y z) p) x y z ',p)))
            permutations)
    (every #'(lambda (p)
               (equal (apply 'foo (mapcar 'make p)) p))
           permutations)))

(do-test "multi-methods when assorted args are discriminated on"
  (let ((permutations (permutations '(i n r nil) 3)))
    (mapc #'(lambda (p)
	      (EVAL `(defmeth foo
			      ,(mapcar #'(lambda (arg type-spec)
					   (if type-spec
					       (list arg type-spec) arg))
				       '(arg1 arg2 arg3)
				       p)
		       arg1 arg2 arg3 ',p)))
	  permutations)
    (every #'(lambda (p)
               (equal (apply 'foo
			     (mapcar #'(lambda (x) (and x (make x))) p)) p))
           permutations)))



;(do-test "anonymous discriminators"
;  
;  (let ((foo (make 'discriminator))
;        (proto-method (class-prototype (class-named 'method))))
;    (add-method-internal  foo proto-method '(thing) (list (class-named 'x)) '(lambda (thing) thing 'x))
;    (add-method foo '(thing) (list (class-named 'y)) '(lambda (thing) thing 'y))
;    (add-method foo '(thing) (list (class-named 'z)) '(lambda (thing) thing 'z))
;
;    (let ((function (discriminator-discriminating-function foo)))
;      (and (eq (funcall function (make 'x)) 'x)
;          (eq (funcall function (make 'y)) 'y)
;          (eq (funcall function (make 'z)) 'z)))))



(do-test "Simple with test -- does not really exercise the walker."
  
  (ndefstruct (foo (:class class))
    (x 0)
    (y 0))

  (defmeth foo ((obj foo))
    (with (obj)
      (list x y)))

  (defmeth bar ((obj foo))
    (with ((obj obj-))
      (setq obj-x 1
            obj-y 2)))

  (and (equal '(0 0) (foo (make-foo)))
       (equal '(1 2) (foo (make-foo :x 1 :y 2)))
       (let ((foo (make-foo)))
         (bar foo)
         (and (equal (get-slot foo 'x) 1)
              (equal (get-slot foo 'y) 2)))))

(do-test "Simple with* test -- does not really exercise the walker."
  
  (ndefstruct (foo (:class class))
    (x 0)
    (y 0))

  (defmeth foo ((obj foo))
    (with* (obj)
      (list x y)))

  (defmeth bar ((obj foo))
    (with* ((obj obj-))
      (setq obj-x 1
            obj-y 2)))

  (and (equal '(0 0) (foo (make-foo)))
       (equal '(1 2) (foo (make-foo :x 1 :y 2)))
       (let ((foo (make-foo)))
         (bar foo)
         (and (equal (get-slot foo 'x) 1)
              (equal (get-slot foo 'y) 2)))))

'(

;;; setup for :daemon combination test
;;;

(do-test "setting up for :daemon method combination test"
  
  (ndefstruct (foo (:class class)))
  (ndefstruct (bar (:class class) (:include (foo))))
  (ndefstruct (baz (:class class) (:include (bar)))))

(defvar *foo*)

(defmeth foo ((x foo)) (push 'foo *foo*) 'foo)
(defmeth (foo :before) ((x foo)) (push '(:before foo) *foo*))
(defmeth (foo :after)  ((x foo)) (push '(:after foo) *foo*))

(do-test (":before primary and :after all on same class." :clear nil)

  (let ((*foo* ()))
    (and (eq (foo (make 'foo)) 'foo)
	 (equal *foo* '((:after foo) foo (:before foo))))))

(defmeth foo ((x bar)) (push 'bar *foo*) 'bar)

(do-test (":before and :after inherited, primary from this class" :clear nil)

  (let ((*foo* ()))
    (and (eq (foo (make 'bar)) 'bar)
	 (equal *foo* '((:after foo) bar (:before foo))))))

(do-test ("make sure shadowing primary in sub-class has no effect here"
	  :clear nil)
  (let ((*foo* ()))
    (and (eq (foo (make 'foo)) 'foo)
	 (equal *foo* '((:after foo) foo (:before foo))))))

(defmeth (foo :before) ((x bar)) (push '(:before bar) *foo*))
(defmeth (foo :after) ((x bar))  (push '(:after bar) *foo*))

(do-test (":before both here and inherited~%~
           :after both here and inherited~%~
           primary from here"
	  :clear nil)
  (let ((*foo* ()))
    (and (eq (foo (make 'bar)) 'bar)
	 (equal (reverse *foo*)
		'((:before bar) (:before foo) bar (:after foo) (:after bar))))))

(defmeth foo ((x baz)) (push 'baz *foo*) 'baz)

(do-test ("2 :before and 2 :after inherited, primary from here" :clear nil)
  (let ((*foo* ()))
    (and (eq (foo (make 'baz)) 'baz)
	 (equal (reverse *foo*)
		'((:before bar) (:before foo) baz (:after foo) (:after bar))))))


(do-test "setting up for :list method combination test"
  (make-specializable 'foo :arglist '(x) :method-combination-type :list)
  
  (ndefstruct (foo (:class class)))
  (ndefstruct (bar (:class class) (:include (foo))))
  (ndefstruct (baz (:class class) (:include (bar)))))

(defmeth foo ((x foo)) 'foo)

(do-test ("single method, :list combined, from here" :clear nil)
  (equal (foo (make 'foo)) '(foo)))

(defmeth foo ((x bar)) 'bar)
(do-test ("method from here and one inherited, :list combined" :clear nil)
  (equal (foo (make 'bar)) '(foo bar)))

(defmeth foo ((x baz)) 'baz)

(do-test ("method from here, two inherited, :list combined" :clear nil)
  (equal (foo (make 'baz)) '(foo bar baz)))

(do-test ("make sure that more specific methods aren't in my combined method"
	  :clear nil)
  (and (equal (foo (make 'foo)) '(foo))
       (equal (foo (make 'bar)) '(foo bar))
       (equal (foo (make 'baz)) '(foo bar baz))))

)

  ;;   
;;;;;; things that bug fixes prompted.
  ;;   


(do-test "with inside of lexical closures"
  ;; 6/20/86
  ;; The walker was confused about what (FUNCTION (LAMBDA ..)) meant.  It
  ;; didn't walk inside there.  Its sort of surprising this didn't get
  ;; caught sooner.

  (ndefstruct (foo (:class class))
    (x 0)
    (y 0))

  (defun foo (fn foos)
    (and foos (cons (funcall fn (car foos)) (foo fn (cdr foos)))))

  (defun bar ()
    (let ((the-foo (make 'foo :x 0 :y 3)))
      (with ((the-foo () foo))
	(foo #'(lambda (foo) (incf x) (decf y))
	     (make-list 3)))))

  (equal (bar) '(2 1 0)))

(do-test "redefinition of default method has proper effect"
  ;; 5/26/86
  ;; This was caused because the hair for trying to avoid making a
  ;; new discriminating function didn't know that changing the default
  ;; method was a reason to make a new discriminating function.  Fixed
  ;; by always making a new discriminating function when a method is
  ;; added or removed.  The template stuff should keep this from being
  ;; expensive.

  (defmeth foo ((x class)) 'class)
  (defmeth foo (x) 'default)
  (defmeth foo (x) 'new-default)

  (eq (foo nil) 'new-default))


(do-test ("extra keywords in init-plist cause an error" :should-error t)
  ;; 5/26/86
  ;; Remember that Common-Lisp defstruct signals errors if there are
  ;; extra keywords in the &rest argument to make-foo.
  
  (ndefstruct (foo (:class class)) a b c)

  (make 'foo :d 3))

(do-test "run-super with T specifier for first arg"
  ;; 5/29/86
  ;; This was caused because run-super-internal didn't know about the
  ;; type-specifier T being special.  This is yet another reason to
  ;; flush that nonsense about keeping T special.

  (defmeth foo (x y) '((t t)))

  (defmeth foo (x (y k)) '((t k)))

  (defmeth foo (x (y n)) (cons '(t n) (run-super)))

  (defmeth foo ((x i) (y k)) '((i k)))

  (defmeth foo ((x l) (y n)) (cons '(l n) (run-super)))


  (and (equal (foo (make 'l) (make 'n)) '((l n) (i k)))
       (equal (foo (make 'i) (make 'k)) '((i k)))
       (equal (foo () (make 'k)) '((t k)))
       (equal (foo () (make 'n)) '((t n) (t k)))))

(do-test "with inside of with scopes correctly"
  ;; 7/07/86

  (ndefstruct (foo (:class class)
		   (:conc-name nil))
    (foo 1))

  (ndefstruct (bar (:class class)
		   (:conc-name nil))
    (foo 1))


  (defmeth foo ((bar bar)) bar ())

  (defun bar (x)
    (with* ((x "" foo))
      (list foo (with ((x "" bar)) foo))))

  (defun baz (x)
    (with ((x "" bar))
      (list foo (with* ((x "" foo)) foo))))

  (and (equal (bar (make 'bar)) '(1 nil))
       (equal (baz (make 'bar)) '(nil 1))

       (equal (bar (make 'foo)) '(1 1))
       (equal (baz (make 'foo)) '(1 1))))



