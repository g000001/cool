;;;; package.lisp

(cl:in-package :cl-user)

;; (delete-package :co-parser)
(defpackage :common-objects-parser 
  (:use :cl :cool.pcl :cool.pcl.walker)
  (:nicknames :co-parser)
  (:export 
    :co-parse-define-type-call
    :co-parse-method-macro-call
    :co-parse-call-to-method
    :co-process-var-options
    :co-parse-options
    :co-deftype-error
    :co-legal-type-or-method-name
    :$UNDEFINED-TYPE-NAME
    :$TYPE-INFO-SLOT
    :$TYPE-NAME-SLOT
    :$VARIABLE-NAMES-SLOT
    :$INITABLE-VARIABLES-SLOT
    :$SETTABLE-VARIABLES-SLOT
    :$GETTABLE-VARIABLES-SLOT
    :$PARENT-TYPES-SLOT
    :$PARENTS-INFO-SLOT
    :$A-LIST-METHOD-TABLE-SLOT
    :$TREAT-AS-VARIABLES-SLOT
    :$INIT-KEYWORDS-SLOT
    :$NO-INIT-KEYWORD-CHECK-SLOT
    :$METHODS-TO-NOT-DEFINE-SLOT
    :$METHODS-TO-INHERIT-SLOT
    :$LET-PSEUDO-INFO-SLOT
    :$INFO-NUMBER-OF-SLOTS)
  (:shadowing-import-from :cool.pcl
   :add-method :class-name :find-method :defclass :class :defmethod
   :print-object
   :class-of :make-instance :slot-missing :remove-method
   :change-class
   :slot-exists-p
   :with-slots
   :make-method
   :method
   :call-next-method)
  (:import-from :cool.pcl.walker :defconstant*))

;; (delete-package :co)
(defpackage :common-objects
  (:use :cl :cool.pcl.walker :co-parser :cool.pcl)
  (:nicknames :cool :co)
  (:export
   :=>
   :self
   :make-instance
   :define-type
   :define-method
   :undefine-method
   :call-method
   :apply-method
   :assignedp
   :undefine-type
   :rename-type
   :undef ;; Artifical Intelligence Systems
;;;   2400 Hanovration-p
   :send?
   :instance
   :import-specialized-functions
   :instancep)
  (:export
   :supports-operation-p)
  (:intern :method-alist
           :init-keywords
           :legal-parent-p)
  #|(:shadow 
   COOL:IMPORT-SPECIALIZED-FUNCTIONS COOL:INSTANCE COOL:SEND? COOL:UNDEF
   COOL:RENAME-TYPE COOL:UNDEFINE-TYPE COOL:ASSIGNEDP COOL:APPLY-METHOD
   COOL:DEFINE-METHOD COOL:DEFINE-TYPE)|#
  (:shadowing-import-from :cool.pcl
   :add-method :class-name :find-method :defclass :class :defmethod
   :print-object
   :class-of :make-instance :slot-missing :remove-method
   :change-class
   :slot-exists-p
   :with-slots
   :make-method
   :method
   :call-method
   :call-next-method)
  (:import-from
   #+sbcl :sb-cltl2
   #+lispworks :lw
   :compiler-let))


(defpackage :cool.sfun
  (:use)
  (:export :type-of :typep :eql :equal :equalp))


#|(defpackage :cool.internal
  (:use :cool :cl :fiveam))|#


(defpackage :co-test
  (:use :co :cl :fiveam)
  (:import-from :cool.pcl :do-test)
  (:shadowing-import-from :co
   :add-method :class-name :find-method :defclass :class :defmethod
   :print-object
   :class-of :make-instance :slot-missing :remove-method
   :change-class
   :slot-exists-p
   :with-slots
   :make-method
   :method
   :call-next-method
   :call-method)
  (:shadowing-import-from :cool.sfun
                          :type-of :typep :eql :equal :equalp)
  #|(:import-from :co 
                ;; :=>
                :instancep
                :instance
                :make-instance
                :supports-operation-p)|#
  (:import-from :cl
                :nil
                :t
                :setq
                :>
                :null
                :member
                :symbolp
                :eq
                :eval))
