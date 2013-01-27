
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         regress.l
; RCS:          $Revision: 1.1 $
; SCCS:         %A% %G% %U%
; Description:  Regression Tests for COOL.
; Author:       James Kempf, HP/DCC
; Created:      24-Feb-87
; Modified:     25-Feb-87 08:45:24 (James Kempf)
; Language:     Lisp
; Package:      TEST
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 1987 Hewlett-Packard Corporation. All rights reserved.
;
; Use and copying of this software and preparation of derivative works based
; upon this software are permitted.  Any distribution of this software or
; derivative works must comply with all applicable United States export
; control laws.
; 
; This software is made available AS IS, and Hewlett-Packard Corporation makes
; no warranty about the software, its performance or its conformity to any
; specification.
;
; Suggestions, comments and requests for improvement may be mailed to
; aiws@hplabs.HP.COM


(in-package :co-test)

(def-suite co-test)
(in-suite co-test)


(defmacro == (x y)
  `(is (cl:equal ,x ,y)))


(defmacro >_< (x)
  `(signals (cl:error) ,x))


(defmacro do-test (name &body body)
  `(test ,(intern (concatenate 'string "co-test\\" name))
     ,@body))


(do-test "define-type"
  (== (define-type car 
        (:var name :gettable)
        (:var top-speed :settable)
        (:var turbo-p :initable)
        :all-initable)
      'car)
  (== (instancep 'car) NIL)
  (== (typep 'car 'instance) NIL))


(def-fixture car ()
  (let ((c (make-instance 'car)))
    (&body)))


(do-test "make-instance"
  (with-fixture car ()
    (is-true (instancep c))
    (is-true (=> c :typep 'car))))


(do-test "make-instance error cases"
  (>_< (make-instance NIL))
  (>_< (make-instance (gensym)))
  (>_< (make-instance 'not-a-type))
  (>_< (make-instance 'float))
  (>_< (make-instance 'car :not-initkw 314159)))


(do-test "make-instance syntax"
  (>_< (make-instance))
  (>_< (make-instance '(a b)))
  (>_< (make-instance 'car :boink))
  (>_< (make-instance 'car :name))
  (>_< (make-instance 'car 'truck 'van)))


(do-test "the right methods there?"
  (with-fixture car ()
    (== (supports-operation-p c :name)            T)
    (== (supports-operation-p c :set-name)        NIL)
    (== (supports-operation-p c :set-top-speed)   T)
    (== (supports-operation-p c :top-speed)       T)
    (== (supports-operation-p c :turbo-p)         NIL)
    (== (supports-operation-p c :set-turbo-p)     NIL)
    (== (supports-operation-p c :not-a-method)    NIL)
    (== (supports-operation-p c 'describe)        NIL)
    (== (supports-operation-p c 'init)            NIL)
    (== (supports-operation-p c 'channelprin)     NIL)
    (== (supports-operation-p c 'init)            NIL)
    (== (supports-operation-p c :describe)        T)
    (== (supports-operation-p c :print)           T)
    (== (supports-operation-p c :initialize)      T)
    (== (supports-operation-p c :initialize-variables)  T)
    (== (supports-operation-p c :init)            T)
    (== (supports-operation-p c :eql)             T)
    (== (supports-operation-p c :equal)           T)
    (== (supports-operation-p c :equalp)          T)
    (== (supports-operation-p c :typep)           T)
    (== (supports-operation-p c :copy)            T)
    (== (supports-operation-p c :copy-state)      T)
    (== (supports-operation-p c :copy-instance)   T)))


(do-test "typep"
  (with-fixture car ()
    (== (typep c 'car)                           T)
    (== (typep c 'instance)                      T)
    (== (typep c t)                              T)
    (== (typep c 'integer)                       NIL)
    (== (typep '(frog) 'car)                     NIL)
    (== (type-of c)                              'car)))


(def-fixture car-class ()
  (let ((car (gentemp "car-"))
        (auto (gentemp "auto-")))
    (eval `(define-type ,car 
             (:var name (:init 'porsche) :gettable)
             (:var top-speed :settable)
             (:var turbo-p :initable)
             :all-initable))
    (let ((c (make-instance car)))
      (declare (ignorable c))
      (&body))))


(do-test "rename-type"
  (with-fixture car-class ()
    (== (rename-type car auto)                 auto)
    (== (typep c car)                           NIL)
    (== (typep c auto)                          T)
    (== (type-of c)                              auto)
    (== (undefine-type car)                     NIL)
    (== (typep c auto)                          T)))


(def-fixture car->auto-class ()
  (let ((car (gentemp "car-"))
        (auto (gentemp "auto-")))
    (eval `(define-type ,car 
             (:var name (:init 'porsche) :gettable)
             (:var top-speed :settable)
             (:var turbo-p :initable)
             :all-initable))
    (let ((c (make-instance car)))
      (declare (ignorable c))
      (rename-type car auto)
      (&body))))


(do-test "rename-type error cases"
  (with-fixture car->auto-class ()
    (>_< (rename-type 'float 'pneuname))
    (>_< (rename-type auto auto))
    (>_< (rename-type car auto))))


(do-test "define-method error case"
  (with-fixture car->auto-class ()
    (>_< (eval `(define-method (,car :flat) ())))))


(do-test "now that type car is renamed"
  (with-fixture car->auto-class ()
    (== (=> c :name)           'porsche)
    (== (=> c :set-top-speed 157)           157)
    (== (=> c :top-speed)                   157)
    (== (eval `(define-method (,auto :sportscar-p) ()
                 (> top-speed 130)))
        `(,auto :sportscar-p))
    (== (=> c :sportscar-p)                 T)))


(do-test "define a new type car"
  (with-fixture car->auto-class ()
    (== (eval `(define-type ,car (:var railroad) (:var type) :all-settable))
        car)))


(def-fixture car->auto->car-class ()
  (let ((car (gentemp "car-"))
        (auto (gentemp "auto-")))
    (eval `(define-type ,car 
             (:var name (:init 'porsche) :gettable)
             (:var top-speed :settable)
             (:var turbo-p :initable)
             :all-initable))
    (let ((c (make-instance car)))
      (declare (ignorable c))
      (rename-type car auto)
      (eval `(define-type ,car (:var railroad) (:var type) :all-settable))
      (&body))))


(do-test "now that we have a new type car"
  (with-fixture car->auto->car-class ()
    (== (=> c :name)  'porsche) 
    (== (=> c :set-top-speed 157)  157) 
    (== (=> c :top-speed)  157)
    (== (eval `(define-method (,auto :sportscar-p) () (> top-speed 130)))
        `(,auto :sportscar-p))
    (== (=> c :sportscar-p)                 T)
    (== (undefine-type car)                T)))


(do-test "type for rename-type and undefine-type"
  (== (define-type other)  'other))


(do-test "rename-type syntax"
  (with-fixture car->auto->car-class ()
    (>_< (rename-type auto NIL))
    (>_< (rename-type 'other auto))
    (>_< (rename-type NIL auto))
    (>_< (rename-type '(a) 'other))
    (>_< (rename-type 'other '(a b)))
    (>_< (rename-type))
    (>_< (rename-type auto))))
	   

(def-fixture car->auto->car-other-class ()
  (let ((car (gentemp "car-"))
        (auto (gentemp "auto-"))
        (other (gentemp "other-")))
    (eval `(define-type ,car 
             (:var name (:init 'porsche) :gettable)
             (:var top-speed :settable)
             (:var turbo-p :initable)
             :all-initable))
    (let ((c (make-instance car)))
      (declare (ignorable c))
      (rename-type car auto)
      (eval `(define-type ,car (:var railroad) (:var type) :all-settable))
      (eval `(define-type ,other))
      (&body))))


(do-test "undefine-type"
  (with-fixture car->auto->car-other-class ()
    (== (undefine-type auto)                    T)
    (== (null (type-of c))                        NIL)
    (== (eq (type-of c) T)                        NIL)
    (== (member (type-of c) `(,auto ,car))          NIL)
    (== (symbolp (type-of c))                     T)
    (== (undefine-type auto)                     NIL)
    (== (undefine-type other)                    T)
    (== (undefine-type 'float)                    NIL)))


(do-test "let's use those undefined types"
  (with-fixture car->auto->car-other-class ()
    (undefine-type auto)
    (undefine-type other)
    ;; 
    (>_< (make-instance auto))
    (>_< (cl:eval `(define-method (,auto :burp) () T)))
    (>_< (=> c :name))))


(do-test "send? to object with undefined type"
  (with-fixture car->auto->car-other-class ()
    (undefine-type auto)
    (undefine-type other)
    (== (send? c :name)  NIL)))


(do-test "undefine-type syntax"
  (>_< (undefine-type '(a big dog))))


(do-test "define-type syntax"
  (>_< (eval '(define-type))) 
  (>_< (eval '(define-type (a list))))
  (>_< (eval '(define-type actress ann-margret)))
  (>_< (eval '(define-type actress (ann-margret))))
  (>_< (eval '(define-type actress (:var)))) 
  (>_< (eval '(define-type actress (:var :var)))) 
  (>_< (eval '(define-type actress (:var :a-keyword)))) 
  (>_< (eval '(define-type actress (:var twin) (:var not-twin) (:var twin)))) 
  (>_< (eval '(define-type actress (:var ann-margret ())))) 
  (>_< (eval '(define-type actress (:var ann-margret dyan-cannon)))) 
  (>_< (eval '(define-type actress (:var ann-margret (:not-option lips)))))
  (>_< (eval '(define-type actress (:var ann-margret (:init)))))
  (>_< (eval '(define-type actress (:var ann-margret (:init 'one 'two)))))
  (>_< (eval '(define-type actress (:var ann-margret :not-an-option))))
  (>_< (eval '(define-type actress (:var ann-margret (:gettable))))))


(do-test "various define-types that should work"
  (== (undefine-type 'actress) NIL)
  (== (undefine-type 'self) NIL))


(do-test "define an actress"
  (== (define-type actress (:var actress)) 'actress)
  (undefine-type 'actress))


(do-test "check self"
  (== (eval '(define-type self (:var me :settable (:init 'hit)))) 'self)
  (== (let ((self (make-instance 'self))) (=> self :me)) 'hit)
  ;; "get rid of self"
  (== (undefine-type 'self) T))


(do-test "initial funny business setup"
  (== (define-type oedipus-rex) 'oedipus-rex)
  (== (define-type laius (:inherit-from oedipus-rex)) 'laius)
  (== (define-type jocasta (:inherit-from laius)) 'jocasta)
  ;; "check for inheritence funny business"
  (>_< (eval '(define-type oedipus-rex (:inherit-from oedipus-rex))))
  (>_< (eval '(define-type oedipus-rex (:inherit-from laius))))
  (>_< (eval '(define-type oedipus-rex (:inherit-from jocasta))))
  ;; "clean up after funny business check"
  (== (undefine-type 'jocasta) T)
  (== (undefine-type 'laius) T)
  (== (undefine-type 'oedipus-rex) T))


(do-test "get rid of it"
  (== (undefine-type 'animal) NIL))


(do-test "general animal test"
  (let (an-animal)
    (== (list (makunbound 'name)
              (makunbound 'num-legs)
              (makunbound 'color)
              (makunbound 'lives-where))
        '(name num-legs color lives-where))
    (== (define-type animal 
          (:var name :gettable)
          (:var num-legs :gettable)
          (:var color (:init 'brown))
          (:var lives-where (:init 'on-ground) :settable)
          :all-initable)
        'animal)
    (== (instancep 
         (setq an-animal (make-instance 'animal :name 'horse :num-legs 4)))
        T)
    (== (type-of an-animal) 'animal)
    (== (typep an-animal 'animal) T)
    (== (supports-operation-p an-animal :name) T)
    (== (supports-operation-p an-animal :set-name) NIL)
    (== (supports-operation-p an-animal :num-legs) T)
    (== (supports-operation-p an-animal :set-num-legs) NIL)
    (== (supports-operation-p an-animal :color) NIL)
    (== (supports-operation-p an-animal :set-color) NIL)
    (== (supports-operation-p an-animal :lives-where) T)
    (== (supports-operation-p an-animal :set-lives-where) T)
    (== (=> an-animal :num-legs) 4)
    (== (=> an-animal :name) 'horse)
    (== (=> an-animal :lives-where) 'on-ground)
    (== (=> an-animal :set-lives-where 'ocean) 'ocean)
    (== (=> an-animal :lives-where) 'ocean)))


(def-fixture animal ()
  (unwind-protect (progn
                    (progn (makunbound 'name)
                           (makunbound 'num-legs)
                           (makunbound 'color)
                           (makunbound 'lives-where))
                    (define-type animal 
                      (:var name :gettable)
                      (:var num-legs :gettable)
                      (:var color (:init 'brown))
                      (:var lives-where (:init 'on-ground) :settable)
                      :all-initable)
                    (let ((an-animal (make-instance 'animal 
                                                    :name 'horse
                                                    :num-legs 4))
                          no-animal)
                      (declare (ignorable an-animal no-animal))
                      (&body)))
    (undefine-type 'animal)))


(do-test "=> error case to animal"
  (with-fixture animal ()
    (>_< (setq no-animal (make-instance 'animal :rocky 'bullwinkle)))
    (>_< name)
    (>_< (=> an-animal :set-name 'new-name))
    (>_< name)                              
    (>_< num-legs)                          
    (>_< (=> an-animal :set-num-legs))      
    (>_< (=> an-animal :set-num-legs 8))    
    (>_< (=> an-animal :color))             
    (>_< color)                             
    (>_< (=> an-animal :set-color 'red))    
    (>_< lives-where)                       
    (>_< (=> an-animal :not-a-method))      
    (>_< (=> an-animal :set-lives-where))))


(do-test "=> syntax error check"
  (with-fixture animal ()
    (>_< (eval '(=>)))           
    (>_< (eval '(=> an-animal))) 
    (>_< (=> animal :lives-where))
    (>_< (=> an-animal NIL))      
    (>_< (=> NIL :lives-where))   
    (>_< (=> an-animal :lives-where 'extra-parm))))


(do-test "supports-operation-p syntax"
  (with-fixture animal ()
    (supports-operation-p animal :lives-where)))


(do-test "supports-operation-p syntax"
  (with-fixture animal ()
    (== (supports-operation-p an-animal NIL) NIL)
    (== (supports-operation-p NIL :lives-where) NIL)))


(do-test "instancep syntax"
  (with-fixture animal ()
    (== (instancep 'float) NIL)
    (== (instancep an-animal) T)))


(do-test "send? to animal"
  (with-fixture animal ()
    (=> an-animal :set-lives-where 'ocean)
    (== (send? an-animal :name)                'horse)
    (== (send? an-animal :set-name 'new-name)  NIL)
    (== (send? an-animal :num-legs)            4)
    (== (send? an-animal :set-num-legs)        NIL)
    (== (send? an-animal :set-num-legs 8)      NIL)
    (== (send? an-animal :color)               NIL)
    (== (send? an-animal :set-color 'red)      NIL)
    (== (send? an-animal :lives-where)         'ocean)
    (== (send? an-animal :not-a-method)        NIL)
    ;; (== (send? an-animal :set-lives-where 'mars)  'mars)
    ;; (== (send? an-animal :lives-where)         'mars)
    (== (send? an-animal NIL)            NIL)
    (== (send? NIL :lives-where)         NIL)))


(do-test "send? syntax and error case"
  (with-fixture animal ()
    (>_< (send? an-animal :set-lives-where))
    (>_< (eval '(send?))) 
    (>_< (eval '(send? an-animal)))
    (>_< (send? animal :lives-where)) 
    (>_< (send? an-animal :lives-where 'extra-parm))))


(do-test "define-method in general"
  (with-fixture animal ()
    (== (define-method (animal :num-legs) ()
          num-legs)
        '(animal :num-legs))
    (== (define-method (animal :num-legs) ()
          num-legs)
        '(animal :num-legs))
    (== (define-method (animal :set-num-legs) (new-num-legs)
          (setq num-legs new-num-legs))
        '(animal :set-num-legs))
    (== (=> an-animal :num-legs)  4)
    (== (=> an-animal :num-legs)  4)
    (== (=> an-animal :set-num-legs 2)  2)
    (== (=> an-animal :num-legs)  2)
    (== (define-method (animal :doc) ()
          "doctari" "veterinarian")
        '(animal :doc))
    (== (define-method (animal :quote-two) 'train (list quote train))
        '(animal :quote-two))))


(do-test "define-method syntax"
  (with-fixture animal ()
    (>_< (eval '(define-method (float :nines) () )))
    (>_< (=> an-animal :set-num-legs))
    (>_< (=> an-animal :set-num-legs 1 'and 'a 2))
    (>_< (eval '(define-method)))
    (>_< (eval '(define-method 'frog)))
    (>_< (eval '(define-method (corn mash))))
    (>_< (eval '(define-method (animal mash) bleach)))))


#|(with-fixture animal ()
  (define-method (animal :doc) () "doctari" "veterinarian")
  (=> (make-instance 'animal) :doc))|#

(do-test "undefine-method"
  (with-fixture animal ()
    (define-method (animal :doc) () "doctari" "veterinarian")
    (define-method (animal :quote-two) 'train (list quote train))
    ;; 
    (== (=> (make-instance 'animal) :doc)  "veterinarian")
    (== (undefine-method 'animal 'not-a-method)  NIL)
    ;; (== (undefine-method 'animal '(a))  NIL) ;???
    (== (undefine-method 'animal :quote-two)  T)
    (== (co::undefine-method 'animal :quote-two)  NIL)
    (== (=> an-animal :doc)  "veterinarian")
    (== (co::undefine-method 'animal :doc)  T)
    ))


(undefine-type 'animal)


(do-test "undefine-method error cases"
  (>_< (=> an-animal :doc))
  (>_< (undefine-method '(a) :quote-two))
  (>_< (eval '(undefine-method)))
  (>_< (undefine-method 'not-a-type :quote-two))
  (>_< (undefine-method 'integer :quote-two)))


(do-test "undefine bird"
  (undefine-type 'bird)
  (== (undefine-type 'bird) NIL))


(do-test "define bird type"
  (with-fixture animal ()
    (== (define-type bird 
          (:inherit-from animal 
                         :init-keywords 
                         (:methods :name :num-legs :set-num-legs 
                                   :lives-where :set-lives-where))
          (:var aquatic-p (:init NIL)) 
          :all-initable
          :all-settable)
        'bird)))


(def-fixture bird ()
  (let ((animal (gentemp "animal-"))
        (bird (gentemp "bird-")))
    (unwind-protect (let (ibis)
                      (eval
                       `(define-type ,animal 
                          (:var name :gettable)
                          (:var num-legs :settable)
                          (:var color (:init 'brown))
                          (:var lives-where (:init 'on-ground) :settable)
                          :all-initable))
                      (eval
                       `(define-type ,bird 
                          (:inherit-from ,animal 
                                         :init-keywords 
                                         (:methods :name :num-legs :set-num-legs 
                                                   :lives-where :set-lives-where))
                          (:var aquatic-p (:init NIL)) 
                          :all-initable
                          :all-settable))
                      (&body))
      (eval `(undefine-type ',animal))
      (eval `(undefine-type ',bird)))))


(do-test "make bird instances"
  (with-fixture bird ()
    (== (instancep (setf ibis
                         (make-instance bird
                                        :name 'ibis
                                        :num-legs 2
                                        :aquatic-p T)))   
        T)
    (== (=> ibis :name)                      'ibis)
    (== (=> ibis :num-legs)                  2)
    (== (=> ibis :aquatic-p)                 T)
    (== (=> ibis :lives-where)               'on-ground)))


(do-test "make-instance error cases"
  (with-fixture bird ()
    (>_< (make-instance bird :num-legs))
    (>_< (make-instance bird :not-init-keyword 89)) 
    (>_< (=> ibis :color))))


(do-test "undefine horse"
  (== (undefine-type 'horse) NIL))


(def-fixture horse ()
  (let ((animal (gentemp "animal-"))
        (horse (gentemp "horse-")))
    (unwind-protect (let (wildfire)
                      (declare (ignorable wildfire))
                      (eval
                       `(define-type ,animal 
                          (:var name :gettable)
                          (:var num-legs :settable)
                          (:var color (:init 'brown))
                          (:var lives-where (:init 'on-ground) :settable)
                          :all-initable))
                      (eval
                       `(define-type ,horse
                          (:inherit-from ,animal 
                                         :init-keywords 
                                         (:methods 
                                          :except :num-legs :set-num-legs))
                          (:var races-won (:init NIL) :settable)))
                      (&body))
      (eval `(undefine-type ',animal))
      (eval `(undefine-type ',horse)))))


(do-test "define horse type"
  (with-fixture horse ()
    t))


(do-test "make horse instances"
  (with-fixture horse ()
    (== (instancep (setf wildfire
                         (make-instance horse :name 'wildfire)))   T)
    (== (=> wildfire :name) 'wildfire)
    (== (=> wildfire :lives-where) 'on-ground)))


(do-test "make horse instance error cases"
  (with-fixture horse ()
    (>_< (=> wildfire :num-legs)) 
    (>_< (=> wildfire :color))    
    (>_< (=> wildfire :aquatic-p))
    (>_< (make-instance horse :not-init-keyword 89)) 
    (>_< (make-instance horse :name))))


(do-test "call method on horse"
  (with-fixture horse ()
    (== (eval `(define-method (,horse horses-name) ()
                 (call-method (,animal :name)))) 
        `(,horse horses-name))
    (let ((wildfire (make-instance horse :name 'wildfire)))
      (== (=> wildfire 'horses-name) 'wildfire))
    (== (eval `(define-method (,horse :num-legs) ()
                 (call-method (,animal :num-legs)))) 
        `(,horse :num-legs))
    (== (eval `(define-method (,horse :set-num-legs) (new-num-legs)
                 (call-method (,animal :set-num-legs) new-num-legs)))
        `(,horse :set-num-legs))
    (let ((wildfire (make-instance horse :name 'wildfire)))
      (== (=> wildfire :set-num-legs 6) 6)
      (== (=> wildfire :num-legs) 6))))


(do-test "apply method on horse"
  (with-fixture horse ()
    (== (eval `(define-method (,horse horses-name) ()
                 (apply-method (,animal :name) ())))
        `(,horse horses-name))
    (let ((wildfire (make-instance horse :name 'wildfire)))
      (== (=> wildfire 'horses-name) 'wildfire)
      (== (eval `(define-method (,horse :num-legs) ()
                   (apply-method (,animal :num-legs) ()))) 
          `(,horse :num-legs))
      
      (== (eval `(define-method (,horse :set-num-legs) (new-num-legs)
                   (apply-method (,animal :set-num-legs) (list new-num-legs))))
          `(,horse :set-num-legs))
      (== (=> wildfire :set-num-legs 6) 6)
      (== (=> wildfire :num-legs) 6))))


(do-test "call-method syntax error cases"
  (>_< (eval '(call-method (wildfire :name)))) 
  (>_< (eval '(apply-method (horse :name))))   
  (>_< (eval '(apply-method (horse :name) 'not-a-list))) 
  (>_< (eval '(define-method (horse horses-name) () (apply-method (horse)) ))) 
  (>_< (eval '(define-method (horse horses-name) ()
               (apply-method (horse :name)) ))) 
  (>_< (eval '(define-method (horse horses-name) ()
               (apply-method (horse :name) 'not-a-list) )))  
  (>_< (eval '(define-method (horse horses-name) ()
               (apply-method (horse :name 'should-not-be-here))))))


(do-test "undefine-method part II"
  (with-fixture horse ()
    (eval `(define-method (,horse horses-name) ()
             (apply-method (,animal :name) ())))
    (== (undefine-method horse 'unknown-method)  NIL)
    (== (undefine-method horse 'horses-name)  T)
    (== (undefine-method horse 'horses-name)  NIL)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



