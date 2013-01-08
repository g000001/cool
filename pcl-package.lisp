(cl:in-package :cl-user)


(defpackage :cool.pcl.walker
  (:use :cl)
  (:export 
   :define-walker-template
   :walk-form
   :variable-lexical-p
   :variable-special-p
   ))


(defpackage :cool.pcl
  (:use :cl :fiveam))
