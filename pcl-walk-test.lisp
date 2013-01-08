(in-package :cool.pcl.walker)


  ;;   
;;;;;; Tests tests tests
  ;;


#+sbcl
(defmacro take-it-out-for-a-test-walk (form)
  `(progn 
     (terpri)
     (terpri)
     (let ((copy-of-form (copy-tree ',form))
           (result 
            (sb-walker:walk-form 
             ',form 
             nil
             (lambda (x y env)
               (format t "~&Form: ~S ~3T Context: ~A" x y)
               (when (symbolp x)
                 (multiple-value-bind (lexical special)
                                      (values (sb-walker:var-lexical-p x env)
                                              (sb-walker:var-special-p x env))
                   (when lexical
                     (format t ";~3T")
                     (format t "lexically bound"))
                   (when special
                     (format t ";~3T")
                     (format t "declared special"))
                   (when (boundp x)
                     (format t ";~3T")
                     (format t "bound: ~S " (eval x)))))
               x))))
       (cond ((not (equal result copy-of-form))
              (format t "~%Warning: Result not EQUAL to copy of start."))
             ((not (eq result ',form))
              (format t "~%Warning: Result not EQ to copy of start.")))
       (#+Symbolics zl:grind-top-level
                    #-Symbolics print
                    result)
       result)))


#-sbcl
(defmacro take-it-out-for-a-test-walk (form)
  `(progn 
     (terpri)
     (terpri)
     (let ((copy-of-form (copy-tree ',form))
           (result 
            (walk-form ',form :walk-function
                       (lambda (x y)
                         (format t "~&Form: ~S ~3T Context: ~A" x y)
                         (when (symbolp x)
                           (multiple-value-bind (lexical special)
                                                (variable-lexically-boundp x)
                             (when lexical
                               (format t ";~3T")
                               (format t "lexically bound"))
                             (when special
                               (format t ";~3T")
                               (format t "declared special"))
                             (when (boundp x)
                               (format t ";~3T")
                               (format t "bound: ~S " (eval x)))))
                         x))))
       (cond ((not (equal result copy-of-form))
              (format t "~%Warning: Result not EQUAL to copy of start."))
             ((not (eq result ',form))
              (format t "~%Warning: Result not EQ to copy of start.")))
       (#+Symbolics zl:grind-top-level
                    #-Symbolics print
                    result)
       result)))



(defun foo (&rest ignore) 
  (declare (ignore ignore))
  ())

(defmacro bar (x) `'(global-bar-expanded ,x))

(defun baz (&rest ignore) 
  (declare (ignore ignore))
  ())

(take-it-out-for-a-test-walk (foo arg1 arg2 arg3))
(take-it-out-for-a-test-walk (foo (baz 1 2) (baz 3 4 5)))

(take-it-out-for-a-test-walk (block block-name a b c))
(take-it-out-for-a-test-walk (block block-name (foo a) b c))

(take-it-out-for-a-test-walk (catch catch-tag (foo a) b c))
(take-it-out-for-a-test-walk (compiler-let ((a 1) (b 2)) (foo a) b))
(take-it-out-for-a-test-walk (prog () (declare (special a b))))
(take-it-out-for-a-test-walk (let (a b c)
                               (declare (special a b))
                               (foo a) b c))
(take-it-out-for-a-test-walk (let (a b c)
                               (declare (special a) (special b))
                               (foo a) b c))
(take-it-out-for-a-test-walk (let (a b c)
                               (declare (special a))
                               (declare (special b))
                               (foo a) b c))
(take-it-out-for-a-test-walk (let (a b c)
                               (declare (special a))
                               (declare (special b))
                               (let ((a 1))
                                 (foo a) b c)))
(take-it-out-for-a-test-walk (eval-when ()
                               a
                               (foo a)))
(take-it-out-for-a-test-walk (eval-when (eval when load)
                               a
                               (foo a)))
(take-it-out-for-a-test-walk (progn (function foo)))
(take-it-out-for-a-test-walk (progn a b (go a)))
(take-it-out-for-a-test-walk (if a b c))
(take-it-out-for-a-test-walk (if a b))
(take-it-out-for-a-test-walk ((lambda (a b) (list a b)) 1 2))
(take-it-out-for-a-test-walk ((lambda (a b) (declare (special a)) (list a b))
			      1 2))
(take-it-out-for-a-test-walk (let ((a a) (b a) (c b)) (list a b c)))
(take-it-out-for-a-test-walk (let* ((a a) (b a) (c b)) (list a b c)))
(take-it-out-for-a-test-walk (let ((a a) (b a) (c b))
                               (declare (special a b))
                               (list a b c)))
(take-it-out-for-a-test-walk (let* ((a a) (b a) (c b))
                               (declare (special a b))
                               (list a b c)))
(take-it-out-for-a-test-walk (let ((a 1) (b 2))
                               (foo bar)
                               (declare (special a))
                               (foo a b)))
(take-it-out-for-a-test-walk (multiple-value-call #'foo a b c))
(take-it-out-for-a-test-walk (multiple-value-prog1 a b c))
(take-it-out-for-a-test-walk (progn a b c))
(take-it-out-for-a-test-walk (progv vars vals a b c))
(take-it-out-for-a-test-walk (quote a))
(take-it-out-for-a-test-walk (return-from block-name a b c))
(take-it-out-for-a-test-walk (setq a 1))
(take-it-out-for-a-test-walk (setq a (foo 1) b (bar 2) c 3))
(take-it-out-for-a-test-walk (tagbody a b c (go a)))
(take-it-out-for-a-test-walk (the foo (foo-form a b c)))
(take-it-out-for-a-test-walk (throw tag-form a))
(take-it-out-for-a-test-walk (unwind-protect (foo a b) d e f))


(take-it-out-for-a-test-walk (flet ((flet-1 (a b) (list a b)))
                               (flet-1 1 2)
                               (foo 1 2)))
(take-it-out-for-a-test-walk (labels ((label-1 (a b) (list a b)))
                                (label-1 1 2)
                                (foo 1 2)))
(take-it-out-for-a-test-walk (macrolet ((macrolet-1 (a b) (list a b)))
                               (macrolet-1 a b)
                               (foo 1 2)))

(take-it-out-for-a-test-walk (macrolet ((foo (a) `(inner-foo-expanded ,a)))
                               (foo 1)))

(take-it-out-for-a-test-walk (progn (bar 1)
                                    (macrolet ((bar (a)
						 `(inner-bar-expanded ,a)))
                                      (bar 1))))

(take-it-out-for-a-test-walk (progn (bar 1)
                                    (macrolet ((bar (s)
						 (bar s)
						 `(inner-bar-expanded ,s)))
                                      (bar 2))))

(take-it-out-for-a-test-walk (cond (a b)
                                   ((foo bar) a (foo a))))

#-sbcl
(let ((the-lexical-variables ()))
  (walk-form '(let ((a 1) (b 2))
		#'(lambda (x) (list a b x y)))
	     :walk-function #'(lambda (form context)
				(when (and (symbolp form)
					   (variable-lexical-p form))
				  (push form the-lexical-variables))
				form))
  (or (and (= (length the-lexical-variables) 3)
	   (member 'a the-lexical-variables)
	   (member 'b the-lexical-variables)
	   (member 'x the-lexical-variables))
      (error "Walker didn't do lexical variables of a closure properly.")))

#+sbcl
(let ((the-lexical-variables ()))
  (sb-walker:walk-form '(let ((a 1) (b 2))
                         #'(lambda (x) (list a b x y)))
                       nil
                       (lambda (form context env)
                         (when (and (symbolp form)
                                    (sb-walker:var-lexical-p form env))
                           (push form the-lexical-variables))
                         form))
  (or (and (= (length the-lexical-variables) 3)
	   (member 'a the-lexical-variables)
	   (member 'b the-lexical-variables)
	   (member 'x the-lexical-variables))
      (error "Walker didn't do lexical variables of a closure properly.")))

|#

()

