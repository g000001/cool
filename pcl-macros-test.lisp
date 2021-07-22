(in-package :cool.pcl)

(iterate ((e in '(a b c d))) 
  (collect e))
;=>  (A B C D)

(iterate ((e on '(a b c d))) 
  (collect e)) 
;=>  ((A B C D) (B C D) (C D) (D))

(defun permutations (elements length)
  (if (= length 1)
      (iterate ((x in elements)) (collect (list x)))
      (let ((sub-permutations (permutations elements (- length 1))))
        (iterate ((x in elements))
          (join (iterate ((y in sub-permutations))
                  (collect (cons x y))))))))


(permutations '(1 2 3) 3)


(defmacro foo (&body body)
  `(progn ,@(iterate ((elt in body))
              (collect elt))))


(foo 1 2 3 4)

(defmacro simple-type-specs (arglist)
  `(let ((type-specs
          (iterate ((arg in (print ,arglist)))
            (until (memq arg '(&optional &rest &key &aux)))
            (collect (if (listp arg) (cadr arg) 't)))))
     (setq type-specs (nreverse type-specs))
     (iterate ((type-spec in type-specs))
       (until (neq type-spec 't))
       (pop type-specs))
     (nreverse type-specs)))


(defmacro simple-args (arglist)
  `(iterate ((arg in ,arglist))
	    (until (eq arg '&aux))
	    (unless (memq arg '(&optional &rest &key))
	      (collect (if (listp arg) (car arg) arg))))
  #|`(loop :for arg in ,arglist
         :until (eq arg '&aux)
         :unless (memq arg '(&optional &rest &key))
         :collect (if (listp arg) (car arg) arg))|#)

(defun foo (xs)
  (simple-args 'xs))

(foo '(1 2 3 4))

(walker:walk-form )

(walker::walk-form-internal harlequin-common-lisp:*traced-arglist* ((progn (until (eq arg (quote &aux))) (unless (memq arg (quote (&optional &rest &key))) (collect (if (listp arg) (car arg) arg)))) :eval #<Environment venv (#<Venv 275415883328  setf-args> #<Venv 275415883176  args> #<Venv 275415813016  method-arglist> #<Venv 275415786544  method-name> #<Venv 275415687704  discriminator-name> #<Venv 275415686976  name> #<Venv 275415686448  setfp> #<Venv 275415686296  setf> #<Venv 275415686080  g201736> #<Venv 275415683584  m-v-b-&rest201733> #<Venv 275415683432  body> #<Venv 275415683192  declares> #<Venv 275415682952  documentation> #<Venv 275415682400  body> #<Venv 275415682248  arglist> #<Venv 275415682096  name&options>)  fenv ((#:g306 compiler::macro #<Closure 2 subfunction of iterate 40500041D4> nil nil nil)) benv ((compiler::dummy-lambda . #<Block compiler::dummy-lambda>) (bootstrap-expand-defmeth . #<Block bootstrap-expand-defmeth>)) tenv nil>))
