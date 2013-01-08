(in-package :cool.pcl.walker)

;;; orig:
;;; (walk-form form :declarations *declarations*
;;;            :lexical-variables *lexical-variables*
;;;            :environment *environment*
;;;            :walk-function *walk-function*)
;;; 
;;; sbcl:
;;; (sb-walker:walk-form form [environment] [walk-function])

(defun walk-form (form &key
                       (walk-function
                        (lambda (subform context env)
                          (declare (ignore context env))
                          subform))
                       (environment nil))
  (sb-walker:walk-form form environment walk-function))


