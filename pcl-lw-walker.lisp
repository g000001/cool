(in-package :cool.pcl.walker)

;;; orig:
;;; (walk-form form :declarations *declarations*
;;;            :lexical-variables *lexical-variables*
;;;            :environment *environment*
;;;            :walk-function *walk-function*)
;;; 
;;; lispworks:
;;; (walker:walk-form form [environment] [walk-function])

(defun walk-form (form &key
                       (walk-function
                        (lambda (subform context env)
                          (declare (ignore context env))
                          subform))
                       (environment nil))
  environment
  (walker:walk-form form nil walk-function)) ;FIXME environment


(defun variable-lexical-p (var env)
  (walker:variable-lexical-p var env))


(defun variable-special-p (var env)
  (walker:variable-special-p var env))


(defun function-arglist (function-name)
  (lw:function-lambda-list function-name))


(defmacro defconstant* (sym value &optional doc)
   `(defconstant ,sym (if (boundp ',sym)
                          (symbol-value ',sym)
                          ,value)
      ,@(when doc (list doc))))


