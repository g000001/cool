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


(defun variable-lexical-p (var env)
  (sb-walker:var-lexical-p var env))


(defun variable-special-p (var env)
  (sb-walker:var-special-p var env))


(defun function-arglist (function-name)
  (sb-introspect:function-lambda-list function-name))


(defmacro defconstant* (sym value &optional doc)
   `(defconstant ,sym (if (boundp ',sym)
                          (symbol-value ',sym)
                          ,value)
      ,@(when doc (list doc))))


