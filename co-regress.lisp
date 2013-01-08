
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


#|(provide "co-regress")|#

(in-package :co-test)


;;This is needed to be sure the Lisp functions are
;;  correctly redefined

(import-specialized-functions)

(do-test ("define-type" :return-value T)
     (
      (co:define-type car 
         (:var name :gettable)
         (:var top-speed :settable)
         (:var turbo-p :initable)
         :all-initable
       )
       car
     )
     ( (instancep 'car) NIL)
     ( (typep 'car 'instance) NIL)
)


(do-test "make-instance"
      (instancep (cl:setq c (make-instance 'car :name 'porsche)))
      (=> c :typep 'car)
)

(do-test ("make-instance error cases" :should-error T)
      (make-instance NIL)
      (make-instance (gensym))
      (make-instance 'not-a-type)
      (make-instance 'float)
      (make-instance 'car :not-initkw 314159)
)

(do-test ("make-instance syntax" :should-error T)
      (make-instance)
      (make-instance '(a b))
      (make-instance 'car :boink)
      (make-instance 'car :name)
      (make-instance 'car 'truck 'van)
)



(do-test ("the right methods there?" :return-value T)
    ((supports-operation-p c :name)            T)
    ((supports-operation-p c :set-name)        NIL)
    ((supports-operation-p c :set-top-speed)   T)
    ((supports-operation-p c :top-speed)       T)
    ((supports-operation-p c :turbo-p)         NIL)
    ((supports-operation-p c :set-turbo-p)     NIL)
    ((supports-operation-p c :not-a-method)    NIL)
    ((supports-operation-p c 'describe)        NIL)
    ((supports-operation-p c 'init)            NIL)
    ((supports-operation-p c 'channelprin)     NIL)
    ((supports-operation-p c 'init)            NIL)
    ((supports-operation-p c :describe)        T)
    ((supports-operation-p c :print)           T)
    ((supports-operation-p c :initialize)      T)
    ((supports-operation-p c :initialize-variables)  T)
    ((supports-operation-p c :init)            T)
    ((supports-operation-p c :eql)             T)
    ((supports-operation-p c :equal)           T)
    ((supports-operation-p c :equalp)          T)
    ((supports-operation-p c :typep)           T)
    ((supports-operation-p c :copy)            T)
    ((supports-operation-p c :copy-state)      T)
    ((supports-operation-p c :copy-instance)   T)
)


(do-test ("typep" :return-value T)
    ((typep c 'car)                           T)
    ((typep c 'instance)                      T)
    ((typep c t)                              T)
    ((typep c 'integer)                       NIL)
    ((typep '(frog) 'car)                     NIL)
    ((type-of c)                              car)
)

(do-test ("rename-type" :return-value T)
    ((rename-type 'car 'auto)                 auto)
    ((typep c 'car)                           NIL)
    ((typep c 'auto)                          T)
    ((type-of c)                              auto)
    ((undefine-type 'car)                     NIL)
    ((typep c 'auto)                          T)
    ((typep c 'auto)                          T)
)

(do-test ("rename-type error cases" :should-error T)
    (rename-type 'float 'pneuname)
    (rename-type 'auto 'auto)
    (rename-type 'car 'auto)
)

(do-test ("define-method error case" :should-error T)
    (eval '(define-method (car :flat) ()))
)

(do-test ("now that type car is renamed" :return-value T)
    ((=> c :name)                        porsche)
    ((=> c :set-top-speed 157)           157)
    ((=> c :top-speed)                   157)
    ((define-method (auto :sportscar-p) () (> top-speed 130))    (auto :sportscar-p))
    ((=> c :sportscar-p)                 T)
)


(do-test ("define a new type car" :return-value T)
    ((define-type car (:var railroad) (:var type) :all-settable)  car)
)

(do-test ("now that we have a new type car" :return-value T)
    ((=> c :name)  porsche) 
    ((=> c :set-top-speed 157)  157) 
    ((=> c :top-speed)  157)
    ((define-method (auto :sportscar-p) () (> top-speed 130))    (auto :sportscar-p))
    ((=> c :sportscar-p)                 T)
    ((undefine-type 'car)                T)
)


(do-test ("type for rename-type and undefine-type" :return-value T)
    ((define-type other)  other)
)

(do-test ("rename-type syntax" :should-error T)
    (rename-type 'auto NIL)
    (rename-type 'other 'auto)
    (rename-type NIL 'auto)
    (rename-type '(a) 'other)
    (rename-type 'other '(a b))
    (rename-type)
    (rename-type 'auto)
)
	   

(do-test ("undefine-type" :return-value T)
   ((undefine-type 'auto)                    T)
   ((null (type-of c))                        NIL)
   ((eq (type-of c) T)                        NIL)
   ((member (type-of c) '(auto car))          NIL)
   ((symbolp (type-of c))                     T)
   ((undefine-type 'auto)                     NIL)
   ((undefine-type 'other)                    T)
   ((undefine-type 'float)                    NIL)
)


(do-test ("let's use those undefined types" :should-error T)
   (make-instance 'auto)
   (cl:eval '(co:define-method (auto :burp) () T))
   (=> c :name)
)

(do-test ("send? to object with undefined type" :return-value T)

   ((send? c :name)  NIL)

)


(do-test ("undefine-type syntax" :should-error T)
   (undefine-type '(a big dog))
)

(do-test ("define-type syntax" :should-error T)
    (cl:eval '(define-type)) 
    (cl:eval '(define-type (a list)))
    (cl:eval '(define-type actress ann-margret))
    (cl:eval '(define-type actress (ann-margret)))
    (cl:eval '(define-type actress (:var))) 
    (cl:eval '(define-type actress (:var :var))) 
    (cl:eval '(define-type actress (:var :a-keyword))) 
    (cl:eval '(define-type actress (:var twin) (:var not-twin) (:var twin))) 
    (cl:eval '(define-type actress (:var ann-margret ()))) 
    (cl:eval '(define-type actress (:var ann-margret dyan-cannon))) 
    (cl:eval '(define-type actress (:var ann-margret (:not-option lips))))
    (cl:eval '(define-type actress (:var ann-margret (:init))))
    (cl:eval '(define-type actress (:var ann-margret (:init 'one 'two))))
    (cl:eval '(define-type actress (:var ann-margret :not-an-option)))
    (cl:eval '(define-type actress (:var ann-margret (:gettable))))
)
    
(do-test ("various define-types that should work" :return-value T)
    ((undefine-type 'actress) NIL)
    ((undefine-type 'self) NIL)
)

(do-test ("define an actress" :return-value T)
    ((define-type actress (:var actress))  actress)
)
    
(do-test ("check self" :return-value T)
    ((eval '(define-type self (:var me :settable (:init 'hit))))  self)
    ((cl:let ((self (make-instance 'self))) (=> self :me))  hit)

)

(do-test "get rid of self"
    (undefine-type 'self)
)

(do-test ("initial funny business setup" :return-value T)
    ((define-type oedipus-rex)    oedipus-rex)
    ((define-type laius (:inherit-from oedipus-rex))  laius)
    ((define-type jocasta (:inherit-from laius))  jocasta)
)

(do-test ("check for inheritence funny business" :should-error T)
    (eval '(define-type oedipus-rex (:inherit-from oedipus-rex)))
    (eval '(define-type oedipus-rex (:inherit-from laius)))
    (eval '(define-type oedipus-rex (:inherit-from jocasta)))
)

(do-test ("clean up after funny business check" :return-value T)    
    ((undefine-type 'jocasta) T)
    ((undefine-type 'laius) T)
    ((undefine-type 'oedipus-rex) T)
)
     
(do-test ("get rid of it" :return-value T)
      ((undefine-type 'animal) NIL)
)

(do-test ("general animal test" :return-value T)
    ((cl:list (cl:makunbound 'name)
	   (cl:makunbound 'num-legs)
	   (cl:makunbound 'color)
	   (cl:makunbound 'lives-where))  (name num-legs color lives-where))
    ((define-type animal 
	     (:var name :gettable)
             (:var num-legs :gettable)
	     (:var color (:init 'brown))
	     (:var lives-where (:init 'on-ground) :settable)
	     :all-initable
	     )  animal)
    ((instancep (setq an-animal (make-instance 'animal :name 'horse :num-legs 4)))   T)
    ((type-of an-animal)                 animal)
    ((typep an-animal 'animal)           T)
    ((supports-operation-p an-animal :name)               T)
    ((supports-operation-p an-animal :set-name)           NIL)
    ((supports-operation-p an-animal :num-legs)           T)
    ((supports-operation-p an-animal :set-num-legs)       NIL)
    ((supports-operation-p an-animal :color)              NIL)
    ((supports-operation-p an-animal :set-color)          NIL)
    ((supports-operation-p an-animal :lives-where)        T)
    ((supports-operation-p an-animal :set-lives-where)    T)
    ((=> an-animal :num-legs)            4)
    ((=> an-animal :name)                horse)
    ((=> an-animal :lives-where)         on-ground)
    ((=> an-animal :set-lives-where 'ocean)  ocean)
    ((=> an-animal :lives-where)         ocean)
)

(do-test ("=> error case to animal" :should-error T)
    (setq no-animal (make-instance 'animal :rocky 'bullwinkle))
    name
    (=> an-animal :set-name 'new-name)
    name                              
    num-legs                          
    (=> an-animal :set-num-legs)      
    (=> an-animal :set-num-legs 8)    
    (=> an-animal :color)             
    color                             
    (=> an-animal :set-color 'red)    
    lives-where                       
    (=> an-animal :not-a-method)      
    (=> an-animal :set-lives-where)   
)


(do-test ("=> syntax error check" :should-error T)
    (eval '(=>))           
    (eval '(=> an-animal)) 
    (=> animal :lives-where)
    (=> an-animal NIL)      
    (=> NIL :lives-where)   
    (=> an-animal :lives-where 'extra-parm)
)



(do-test ("supports-operation-p syntax" :should-error T)
    (supports-operation-p animal :lives-where) 
)

(do-test ("supports-operation-p syntax" :return-value T)
    ((supports-operation-p an-animal NIL)            NIL)
    ((supports-operation-p NIL :lives-where)         NIL)
)	    


(do-test ("instancep syntax" :return-value T)
    ((instancep 'float)                     NIL)
    ((instancep an-animal)                  T)
)



(do-test ("send? to animal"  :return-value T)
    ((send? an-animal :name)                horse)
    ((send? an-animal :set-name 'new-name)  NIL)
    ((send? an-animal :num-legs)            4)
    ((send? an-animal :set-num-legs)        NIL)
    ((send? an-animal :set-num-legs 8)      NIL)
    ((send? an-animal :color)               NIL)
    ((send? an-animal :set-color 'red)      NIL)
    ((send? an-animal :lives-where)         ocean)
    ((send? an-animal :not-a-method)        NIL)
    ((send? an-animal :set-lives-where 'mars)  mars)
    ((send? an-animal :lives-where)         mars)
    ((send? an-animal NIL)            NIL)
    ((send? NIL :lives-where)         NIL)
)


(do-test ("send? syntax and error case" :should-error T)
    (send? an-animal :set-lives-where)
    (eval '(send?)) 
    (eval '(send? an-animal))
    (send? animal :lives-where) 
    (send? an-animal :lives-where 'extra-parm) 
)



(do-test ("define-method in general" :return-value T)
    ((define-method (animal :num-legs) ()
		num-legs)            (animal :num-legs))
    ((define-method (animal :num-legs) ()
		num-legs)            (animal :num-legs))
    ((define-method (animal :set-num-legs) (new-num-legs)
		(setq num-legs new-num-legs))
                                     (animal :set-num-legs))
    ((=> an-animal :num-legs)  4)
    ((=> an-animal :num-legs)  4)
    ((=> an-animal :set-num-legs 2)  2)
    ((=> an-animal :num-legs)  2)
    ((define-method (animal :doc) () "doctari" "veterinarian")  (animal :doc))
    ((define-method (animal :quote-two) 'train (cl:list quote train))  (animal :quote-two))
)


(do-test ("define-method syntax" :should-error T)
    (eval '(define-method (float :nines) () ))
    (=> an-animal :set-num-legs)
    (=> an-animal :set-num-legs 1 'and 'a 2)
    (eval '(define-method))
    (eval '(define-method 'frog))
    (eval '(define-method (corn mash)))
    (eval '(define-method (animal mash) bleach))
)


(do-test ("undefine-method" :return-value T)
    ((=> (make-instance 'animal) :doc)  "veterinarian")
    ((co::undefine-method 'animal 'not-a-method)  NIL)
    ;; ((co::undefine-method 'animal '(a))  NIL) ;???
    ((co::undefine-method 'animal :quote-two)  T)
    ((co::undefine-method 'animal :quote-two)  NIL)
    ((=> an-animal :doc)  "veterinarian")
    ((co::undefine-method 'animal :doc)  T)
)

(do-test ("undefine-method error cases" :should-error T)
    (=> an-animal :doc)
    (undefine-method '(a) :quote-two)
    (eval '(undefine-method))
    (undefine-method 'not-a-type :quote-two)
    (undefine-method 'integer :quote-two)
)

	   
(do-test ("undefine bird" :return-value T)
      ((undefine-type 'bird)                   NIL)
)

(do-test ("define bird type" :return-value T)
    ((define-type bird 
	     (:inherit-from animal 
			    :init-keywords 
			    (:methods :name :num-legs :set-num-legs 
				      :lives-where :set-lives-where
				      )
			    )
	     (:var aquatic-p (:init NIL))
	     :all-initable
	     :all-settable
	     )                           bird)
)


(do-test ("make bird instances" :return-value T)
    ((instancep (cl:setf ibis
	(make-instance 'bird :name 'ibis :num-legs 2 :aquatic-p T)))   T)
    ((=> ibis :name)                      ibis)
    ((=> ibis :num-legs)                  2)
    ((=> ibis :aquatic-p)                 T)
    ((=> ibis :lives-where)               on-ground)
)


(do-test ("make-instance error cases" :should-error T)
    (make-instance 'bird :num-legs)
    (make-instance 'bird :not-init-keyword 89) 
    (=> ibis :color)               
)


(do-test ("undefine horse" :return-value T)
    ((undefine-type 'horse)                   NIL)
)

(do-test ("define horse type" :return-value T)

    ((define-type horse
	     (:inherit-from animal 
			    :init-keywords 
			    (:methods :except :num-legs :set-num-legs
				      )
			    )
	     (:var races-won (:init NIL) :settable)
	     )                           horse)
)


(do-test ("make horse instances" :return-value T)
    ((instancep (cl:setf wildfire
	(make-instance 'horse :name 'wildfire)))   T)
    ((=> wildfire :name)                      wildfire)
    ((=> wildfire :lives-where)               on-ground)
)

(do-test ("make horse instance error cases" :should-error T)
    (=> wildfire :num-legs) 
    (=> wildfire :color)    
    (=> wildfire :aquatic-p)
    (make-instance 'horse :not-init-keyword 89) 
    (make-instance 'horse :name) 
)


(do-test ("call method on horse" :return-value T)
    ((define-method (horse horses-name) () (call-method (animal :name))) 
                                              (horse horses-name))
    ((=> wildfire 'horses-name)               wildfire)
    ((define-method (horse :num-legs) () (call-method (animal :num-legs))) 
                                              (horse :num-legs))
    ((define-method (horse :set-num-legs) (new-num-legs) (call-method (animal :set-num-legs) new-num-legs))
                                              (horse :set-num-legs))
    ((=> wildfire :set-num-legs 6)            6)
    ((=> wildfire :num-legs)                  6)
)


(do-test ("apply method on horse" :return-value T)
    ((define-method (horse horses-name) () (apply-method (animal :name) ())) 
                                          (horse horses-name))
    ((=> wildfire 'horses-name)                wildfire)
    ((define-method (horse :num-legs) () (apply-method (animal :num-legs) ())) 
                                          (horse :num-legs))

    ((define-method (horse :set-num-legs) (new-num-legs) (apply-method (animal :set-num-legs) (cl:list new-num-legs)))
                                          (horse :set-num-legs))
    ((=> wildfire :set-num-legs 6)          6)
    ((=> wildfire :num-legs)                     6)
)    	   

(do-test ("call-method syntax error cases" :should-error T)
    (eval '(call-method (wildfire :name))) 
    (eval '(apply-method (horse :name)))   
    (eval '(apply-method (horse :name) 'not-a-list)) 
    (eval '(define-method (horse horses-name) () (apply-method (horse)) )) 
    (eval '(define-method (horse horses-name) () (apply-method (horse :name)) )) 
    (eval '(define-method (horse horses-name) () (apply-method (horse :name) 'not-a-list) ))  
    (eval '(define-method (horse horses-name) () (apply-method (horse :name 'should-not-be-here)) )) 
)

(do-test ("undefine-method part II" :return-value T)
    ((co::undefine-method 'horse 'unknown-method)  NIL)
    ((co::undefine-method 'horse 'horses-name)  T)
    ((co::undefine-method 'horse 'horses-name)  NIL)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


