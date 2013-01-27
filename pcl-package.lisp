(cl:in-package :cl-user)


(defpackage :cool.pcl.walker
  (:use :cl)
  (:export 
   :define-walker-template
   :walk-form
   :variable-lexical-p
   :variable-special-p
   :cltl1-eval-when
   :defun-compile-time
   :function-arglist
   :defconstant*
   ))

;; (delete-package :cool.pcl)
(defpackage :cool.pcl
  (:use :cl :fiveam :cool.pcl.walker)
  (:nicknames :cool.portable-commonloops)
  (:shadow 
   :add-method :class-name :find-method :defclass :class :defmethod
   :print-object
   :class-of :make-instance :slot-missing :remove-method
   ;; :destructuring-bind
   :change-class
   :slot-exists-p
   :with-slots
   :make-method
   :method
   :call-next-method
   :call-method)
  (:export :do-test)
  (:export 
   :print-instance :make-specializable :rename-class :call-next-method
   :expand-with-make-entries :method-type-specifiers :method-arglist)
  (:export 
   :defclass :defmethod :print-object :print-instance :ndefstruct
   :defmeth :run-super :make :initialize :get-slot :with :with* :class-of
   :class-named :discriminator-named :class-prototype :class :object
   :essential-class :class-name :class-precedence-list :class-local-supers
   :class-local-slots :class-direct-subclasses :class-direct-methods :class-slots
   :essential-discriminator :discriminator-name :discriminator-methods
   :discriminator-discriminating-function :essential-method :method-discriminator
   :method-arglist :method-argument-specifiers :method-function :method-equal
   :discriminator-methods :slotd-name :slot-missing :define-meta-class
   :%make-instance :%instance-ref :%instancep :%instance-meta-class
   :make-instance :get-slot :put-slot :get-slot-using-class :optimize-slot-access
   :define-class-of-clause :add-named-class :class-for-redefinition :add-class
   :supers-changed :slots-changed :check-super-meta-class-compatibility
   :check-meta-class-change-compatibility :make-slotd
   :compute-class-precedence-list :walk-method-body :walk-method-body-form
   :optimize-get-slot :optimize-set-of-get-slot :variable-lexical-p
   :add-named-method :add-method :remove-named-method :remove-method :find-method
   :find-method-internal :make-discriminating-function
   :install-discriminating-function :no-matching-method
   :class-class-precedence-list :class-local-supers :class-direct-subclasses
   :class-name)
  (:export :defclass
           :defmethod
           :print-object
           :print-instance
           :ndefstruct
           :defmeth
           :run-super
           :make
           :initialize
           :get-slot
           :with
           :with*
           :class-of
           :class-named
           :discriminator-named
           :class-prototype
           :class
           :object
           :essential-class
           :class-name
           :class-precedence-list
           :class-local-supers
           :class-local-slots
           :class-direct-subclasses
           :class-direct-methods
           :class-slots
           :essential-discriminator
           :discriminator-name
           :discriminator-methods
           :discriminator-discriminating-function

           :essential-method

           :method-discriminator
           :method-arglist
           :method-argument-specifiers
           :method-function

           :method-equal

           :discriminator-methods

           :slotd-name
           :slot-missing

           :define-meta-class
           :%make-instance
           :%instance-ref
           :%instancep
           :%instance-meta-class

           :make-instance
           :get-slot
           :put-slot
           :get-slot-using-class
           :optimize-slot-access
           :define-class-of-clause
           :add-named-class
           :class-for-redefinition
           :add-class
           :supers-changed
           :slots-changed
           :check-super-meta-class-compatibility
           :check-meta-class-change-compatibility
           :make-slotd
           :compute-class-precedence-list
           :walk-method-body
           :walk-method-body-form
           :optimize-get-slot
           :optimize-set-of-get-slot
           :add-named-method
           :add-method
           :remove-named-method
           :remove-method
           :find-method
           :find-method-internal
           :make-discriminating-function
           :install-discriminating-function
           :no-matching-method
           :class-class-precedence-list
           :class-local-supers
           :class-direct-subclasses
           :class-name
           )
  #|(:shadow :ignore)|#)

