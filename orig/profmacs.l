
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         profmacs.l
; SCCS:         %A% %G% %U%
; Description:  Macros For Profiling
; Author:       James Kempf, HP/DCC
; Created:      7-Feb-87
; Modified:     25-Feb-87 09:06:08 (James Kempf)
; Language:     Lisp
; Package:      TEST
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'test)
(use-package 'lisp)

;;Need COOL

(require "co")
(use-package 'co)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 	System Dependent Customizations
;
;  Some systems will have special, hardware or software dependent profiling
;  packages. If your system has one, put it in here. Otherwise, the default
;  timing functions from CLtL will be used. In addition, the system dependent
;  function for garbage collection should be inserted, if your system 
;  requires garbage collection. Otherwise, no garbage collection will be done.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;warn-garbage-collect-Warn that no garbage collection function is in use.

(defun warn-garbage-collect ()
  (warn 
    "~&********  Profiling Measurments Could Be Interrupted By Garbage Collection *******"
  )
) ;warn-garbage-collect

;;*clock-increment-in-milliseconds*-Increment of the clock

(defvar *clock-increment-in-milliseconds* 0)

;;Use the 10 microsecond clock

#+HP 
(eval-when (load eval)
  (require "measure")
  (setf (symbol-function 'get-time) (symbol-function measure:time10usec))
  (setf *clock-increment-in-milliseconds* 0.01)
  (setf (symbol-function 'do-garbage-collect) (symbol-function 'system:gc))

)

;;Default is to just use the functions from Steele

#-HP
(eval-when (load eval)
  (setf (symbol-function 'get-time) (symbol-function 'get-internal-real-time))
  (setf *clock-increment-in-milliseconds* 
    (* (float (/ 1.0 internal-time-units-per-second)) 1000.0)
  )
  (setf (symbol-function 'do-garbage-collect) (symbol-function 'warn-garbage-collect))

)

;;Switch for Class Definition Syntax

(defvar *define-type-switch* T)

;;Vector containing names of types with zero, one, two, and
;;  three instance variables.

(defvar *iv-defined-types* (make-array '(4 4) :initial-element NIL))

;;Lists of results

;;For type definition (iterations ivs parents time)

(defvar *define-type-results* NIL)

;;For instance creation (interations ivs parents time)

(defvar *creation-results* NIL)

;;For method definition (iterations preexisting time)

(defvar *define-method-results* NIL)

;;For messaging (iterations functions time)

(defvar *messaging-results* NIL)

;;For inherited messaging (iterations parents time)

(defvar *inherited-messaging-results* NIL)

;;These variables and macros are used for inserting the result of
;;  macroexpantion times into the calculations

(defvar *macro-start-clock* 0)
(defvar *macro-end-clock* 0)
(defvar *macro-total-time* 0)

(defmacro macro-start-clock ()

  (setf *macro-start-clock* (get-time))
  NIL
)

(defmacro macro-end-clock ()

  (setf *macro-end-clock* (get-time))
  (setf *macro-total-time* (- *macro-end-clock* *macro-start-clock*))
  (setf *macro-end-clock* 0)
  (setf *macro-start-clock* 0)

  NIL
)

(defmacro macro-insert-sum ()

  (let
    (
     (returned-sum *macro-total-time*)
    )

    (setf *macro-total-time* 0)
    returned-sum
 )

)

;;do-type-definition-Profile Type or Class Definition

(defmacro do-type-definition (record variables parents)

  (let
    (
      (iv-names NIL)
      (code NIL)
      (tname NIL)
      (pnames NIL)
    )

    ;;Construct a new function symbol for this test

    (push (gensym) *function-symbols*)

    ;;Generate a list of instance variable names

    (dotimes (i variables )
      (setf iv-names
        (list*
          (if *define-type-switch*
            `(:var ,(gentemp)) 
            (gentemp)
          )
          iv-names
        ) 
      )
    )

    ;;Generate list of parent names

    (dotimes (i parents)
      (setf pnames
        (list* 
          (if *define-type-switch*
            `(:inherit-from ,(nth i (aref *iv-defined-types* 0 0))) 
             (nth i (aref *iv-defined-types* 0 0))
          )
          pnames
        )
      )
    )

    ;;Generate code for type definition    

    (dotimes (i 20)
    
      ;;Generate the name for this type and
      ;;  push onto the appropriate list

      (setf tname (gentemp))

      (setf (aref *iv-defined-types* parents variables)
            (push tname (aref *iv-defined-types* parents variables))
      )

      ;;Generate the type code

      (push
        (if *define-type-switch*
          `(define-type ,tname
             ,@iv-names
             ,@pnames
           )
           `(ndefstruct 
             (,tname
               (:class class)
	       ,pnames
             )
             ,@iv-names
           )
        ) ;if

        code

      ) ;push

    )

    ;;Return code, inserting prolog and cache heating

  `(defun ,(first *function-symbols*) ()
    (let
       (
         (after 0)
         (before 0)
         (sum 0)
       )

       (tagbody 
       again

         (do-garbage-collect)

         ,(if *define-type-switch*
           `(define-type ,(gentemp)
             ,@iv-names
             ,@pnames
            )
            `(ndefstruct 
              (,(gentemp)
               (:class class)
	       ,pnames
             )
             ,@iv-names
           )
         ) ;if

         (setf before (get-time))
         (macro-start-clock)
         ,@code
         (macro-end-clock)
         (setf after (get-time))

         (setf sum (macro-insert-sum))


         (if (< (the integer after) (the integer before))
           (go again)
         )
       )

       (if ,record
         (push (list 20 ,variables ,parents (- after before) sum) *define-type-results*)
       )

      )

    )

   ) ;let

) ;do-type-definition

(setf (symbol-function 'do-type-definition-macro) (macro-function 'do-type-definition))
(compile 'do-type-definition-macro)
(setf (macro-function 'do-type-definition) (symbol-function 'do-type-definition-macro))

;;do-instance-creation-Create instances of types as above

(defmacro do-instance-creation (record ivs parents)

  (let
    (
      (code NIL)
    )

    ;;Generate a new function symbol

    (push (gensym) *function-symbols*)

    ;;Generate code to create

    (dotimes (i 20)

      (push
        `(make-instance ',(nth i (aref *iv-defined-types* parents ivs)))
        code
      )

    ) ;dotimes

    ;;Return code, inserting prolog and cache heating

  `(defun ,(first *function-symbols*) ()
    (let
       (
         (after 0)
         (before 0)
       )

       (tagbody
       again

         (do-garbage-collect)

         (make-instance ',(nth 1 (aref *iv-defined-types* parents ivs)))

          (setf before (get-time))
          ,@code
          (setf after (get-time))

	  (if (< (the integer after) (the integer before))
            (go again)
          )
        )
      
        (if ,record
          (push (list 20 ,ivs ,parents (- after before)) *creation-results*)
        )

      )
    )

  ) ;let

) ;do-instance-creation

(setf (symbol-function 'do-instance-creation-macro) (macro-function 'do-instance-creation))
(compile 'do-instance-creation-macro)
(setf (macro-function 'do-instance-creation) (symbol-function 'do-instance-creation-macro))

;;switch-define-types-Define types depending on switch

(defmacro switch-define-types ( parent &rest t-list)

  (let
    (
      (code NIL)
    )

    (dolist (ty t-list)
      (push
        (if *define-type-switch*
          `(define-type ,ty ,@(if parent `((:inherit-from ,parent)) NIL))
          `(ndefstruct (,ty (:class class) ,@(if parent `((:include (,parent))) `() ) )  )
        )
        code
      )
    )

    `(progn
       ,@code
    )

  )
) ;switch-define-types

;;switch-define-method-Define method depending on switch

(defmacro switch-define-method (name)

  (if *define-type-switch*
    `(define-method (,name ,(intern (symbol-name name) (find-package 'keyword)) ) () )
    `(defmeth ,(intern (symbol-name name) co::*keyword-standin-package*)
       ((.inner-self. ,name))
     )
  )

) ;switch-define-method

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
; Define Types For Method Definition Tests and Make Instances
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;For testing method definition and invocation with varying methods on
;;  discriminator

(funcall 
  (compile () 
    `(lambda () (switch-define-types NIL temp0 temp1 temp2 temp3 temp4))
  )
)

(setf temp0 (make-instance 'temp0))
(setf temp1 (make-instance 'temp1))
(setf temp2 (make-instance 'temp2))
(setf temp3 (make-instance 'temp3))
(setf temp4 (make-instance 'temp4))

;;For testing method invocation of inherited methods

(funcall 
  (compile () 
    `(lambda () (switch-define-types NIL g3f))
  )
)

(funcall 
  (compile () 
    `(lambda () (switch-define-method g3f))
  )
)

(funcall 
  (compile () 
    `(lambda () (switch-define-types g3f g2f))
  )
)

(funcall 
  (compile () 
    `(lambda () (switch-define-method g2f))
  )
)

(funcall 
  (compile () 
    `(lambda () (switch-define-types g2f g1f))
  )
)

(funcall 
  (compile () 
    `(lambda () (switch-define-method g1f))
  )
)

(funcall 
  (compile () 
    `(lambda () (switch-define-types g1f g0f))
  )
)

(funcall 
  (compile () 
    `(lambda () (switch-define-method g0f))
  )
)

;;Make an instance of g0f

(setf g0f (make-instance 'g0f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Method symbol List

(defvar *list-of-method-symbols* NIL)

;;do-method-definition-Do the method definition

(defmacro do-method-definition (record  predefined fortype)

  (let
    (
      (code NIL)
    )

    ;;Generate a new function symbol

    (push (gensym) *function-symbols*)

    ;;Generate method symbols if necessary

    (if (not *list-of-method-symbols*)

      (dotimes (i 20)
        (push (intern (format NIL "T~D" i) (find-package :keyword))
	      *list-of-method-symbols*
        )
      )
    ) ;if

    ;;Generate code for method definition
 
    (dolist (l *list-of-method-symbols*)

      (push 
        (if *define-type-switch*
          `(define-method (,fortype ,l) ()  ) 
          `(defmeth ,(intern (symbol-name l) co::*keyword-standin-package*) 
             ((.inner-self. ,fortype)) 
           )
         )
         code
      )

    )

    ;;Return code, inserting prolog and cache heating

  `(defun ,(first *function-symbols*) ()
    (let
       (
         (after 0)
         (before 0)
         (sum 0)
       )

       (tagbody
       again
         (do-garbage-collect)
 
         ,(if *define-type-switch*
           `(define-method (,fortype ,(gentemp)) ()  ) 
           `(defmeth ,(gentemp) ((.inner-self. ,fortype)) )
         )

         (setf before (get-time))
         (macro-start-clock)
         ,@code
	 (macro-end-clock)
         (setf after (get-time))

         (setf sum (macro-insert-sum))

         (if (< (the integer after) (the integer before))
           (go again)
         )
       )

       (if ,record
         (push (list 20 ,predefined (- after before) sum) *define-method-results*)
       )

      )

    )

  ) ;let

) ;do-method-definition

(setf (symbol-function 'do-method-definition-macro) (macro-function 'do-method-definition))
(compile 'do-method-definition-macro)
(setf (macro-function 'do-method-definition) (symbol-function 'do-method-definition-macro))

;;do-messaging-Messaging macro code construction

(defmacro do-messaging (record predefined &rest type-list)

  (let
    (
      (code NIL)
    )

    ;;Generate a new function symbol

    (push (gensym) *function-symbols*)

    ;;Push on 20 messagings

    (dotimes (i 20)

      ;;Message for each type      

      (dolist (ty type-list)

         (push 
           (if *define-type-switch*
             `(=> ,ty ,(first *list-of-method-symbols*))
             `(,(intern 
                (symbol-name (first *list-of-method-symbols*)) 
                co::*keyword-standin-package*
               )
               ,ty
              )
            )
            code

         ) ;push

      ) ;dolist

    ) ;dotimes

    ;;Return code, inserting prolog and hardware cache
    ;;  heating to another message.

  `(defun ,(first *function-symbols*) ()
    (let
       (
         (after 0)
         (before 0)
         (sum 0)
       )

       (tagbody
       again

         (do-garbage-collect)

         ,(if *define-type-switch*
           `(=> ,(first type-list) ,(second *list-of-method-symbols*))
           `(,(intern 
              (symbol-name (second *list-of-method-symbols*)) 
              co::*keyword-standin-package*
             )
             ,(first type-list)
            )
          )


         (setf before (get-time))
         (macro-start-clock)
         ,@code
         (macro-end-clock)
         (setf after (get-time))

         (setf sum (macro-insert-sum))

         (if (< (the integer after) (the integer before))
           (go again)
         )
       )

       (if ,record
          (push (list (* 20 ,(length type-list)) 
		      ,predefined
	              (- after before)
                      sum
                 )
                 *messaging-results*
          )
       )

      )

    )

  ) ;let

) ;do-messaging

(setf (symbol-function 'do-messaging-macro) (macro-function 'do-messaging))
(compile 'do-messaging-macro)
(setf (macro-function 'do-messaging) (symbol-function 'do-messaging-macro))

;;do-inherited-messaging-Generate code for profiling inherited messaging

(defmacro do-inherited-messaging (record level method)

  (let
    (
      (code NIL)
    )

    ;;Generate a new function symbol

    (push (gensym) *function-symbols*)

    ;;Push on 20 messagings

    (dotimes (i 20)

      (push 
        (if *define-type-switch*
          `(=> g0f ,(intern (symbol-name method) (find-package 'keyword)))
          `(,(intern 
               (symbol-name method) 
               co::*keyword-standin-package*
             )
             g0f
          )
        )
        code

      ) ;push

    ) ;dotimes

    ;;Return code, inserting prolog and hardware cache
    ;;  heating to another message.

  `(defun ,(first *function-symbols*) ()
    (let
       (
         (after 0)
         (before 0)
         (sum 0)
       )

       (tagbody
       again

         (do-garbage-collect)

         ,(if *define-type-switch*
           `(=> g0f ,(intern (symbol-name method) (find-package 'keyword)))
           `(,(intern 
              (symbol-name method) 
              co::*keyword-standin-package*
             )
             g0f
            )
          )


         (setf before (get-time))
         (macro-start-clock)
          ,@code
         (macro-end-clock)
         (setf after (get-time))

         (setf sum (macro-insert-sum))

         (if (< (the integer after) (the integer before))
           (go again)
         )
       )

       (if ,record
         (push (list 20 ,level (- after before) sum) *inherited-messaging-results*)
       )

      )

    )

  ) ;let

) ;do-inherited-messaging

(setf (symbol-function 'do-inherited-messaging-macro) (macro-function 'do-inherited-messaging))
(compile 'do-inherited-messaging-macro)
(setf (macro-function 'do-inherited-messaging) (symbol-function 'do-inherited-messaging-macro))

;;print-results-Print the results to the file

(defun print-results (filename fromwho)

  (with-open-file
    (istream filename :direction :output 
		      :if-exists :append 
		      :if-does-not-exist :create
    )
    
    (format istream "~%~%~A~%~%" fromwho)
    (format istream "~%~%Times are in msec. Clock increment:~F~%~%" *clock-increment-in-milliseconds*)



    (format istream "~1,8@T~1,8@T~1,8@TMacroexpand Times~%~%")
    (format istream 
            "Operation~1,8@TSlots~1,8@TParents~1,8@TIterations~1,8@TTotal Time~1,8@TTime per Call~%~%"
    )
    (dolist (l (reverse *define-type-results*))
      (format istream 
              "Define Type~1,8@T~D~1,8@T~D~1,8@T~D~1,8@T~1,8@T~8,2F~1,8@T~8,2F~%" 
              (second l)
              (third l)
              (first l)
              (* (fifth l) *clock-increment-in-milliseconds*)
              (* (float (/ (fifth l) (first l))) *clock-increment-in-milliseconds*)
       )
    )
    (format istream 
            "~%~%Operation~1,8@TIterations~1,8@TFunctions~1,8@TTotal Time~1,8@TTime per Call~%~%"
    )
    (dolist (l (reverse *define-method-results*))
      (format istream 
              "Define Operation~1,8@T~D~1,8@T~D~1,8@T~1,8@T~8,2F~1,8@T~8,2F~%" 
              (first l)
              (second l)
              (* (fourth l) *clock-increment-in-milliseconds*)
              (* (float (/ (fourth l) (first l))) *clock-increment-in-milliseconds*)
       )
    )
    (dolist (l (reverse *messaging-results*))
      (format istream 
              "Operation Invocation~1,8@T~D~1,8@T~D~1,8@T~1,8@T~8,2F~1,8@T~8,2F~%" 
              (first l)
              (second l)
              (* (fourth l) *clock-increment-in-milliseconds*)
              (* (float (/ (fourth l) (first l))) *clock-increment-in-milliseconds*)
       )
    )
    (format istream "~|")


    (format istream "~%~%~A~%~%" fromwho)
    (format istream "~%~%All Times in msec~%~%")

    (format istream "~1,8@T~1,8@T~1,8@TType Definition and Instance Creation~%~%")
    (format istream 
            "Operation~1,8@TSlots~1,8@TParents~1,8@TIterations~1,8@TTotal Time~1,8@TTime per Call~%~%"
    )
    (dolist (l (reverse *define-type-results*))
      (format istream 
              "Define Type~1,8@T~D~1,8@T~D~1,8@T~D~1,8@T~1,8@T~8,2F~1,8@T~8,2F~%" 
              (second l)
              (third l)
              (first l)
              (* (fourth l) *clock-increment-in-milliseconds*)
              (* (float (/ (fourth l) (first l))) *clock-increment-in-milliseconds*)
       )
    )

    (dolist (l (reverse *creation-results*))
      (format istream 
              "Create Instance~1,8@T~D~1,8@T~D~1,8@T~D~1,8@T~1,8@T~8,2F~1,8@T~8,2F~%" 
              (second l)
              (third l)
              (first l)
              (* (fourth l) *clock-increment-in-milliseconds*)
              (* (float (/ (fourth l) (first l))) *clock-increment-in-milliseconds*)
       )
    )

    (format istream "~%~%~1,8@T~1,8@TOperation Creation and Invocation~%~%")
    (format istream 
            "Operation~1,8@TIterations~1,8@TFunctions~1,8@TTotal Time~1,8@TTime per Call~%~%"
    )
    (dolist (l (reverse *define-method-results*))
      (format istream 
              "Define Operation~1,8@T~D~1,8@T~D~1,8@T~1,8@T~8,2F~1,8@T~8,2F~%" 
              (first l)
              (second l)
              (* (third l) *clock-increment-in-milliseconds*)
              (* (float (/ (third l) (first l))) *clock-increment-in-milliseconds*)
       )
    )

    (dolist (l (reverse *messaging-results*))
      (format istream 
              "Operation Invocation~1,8@T~D~1,8@T~D~1,8@T~1,8@T~8,2F~1,8@T~8,2F~%" 
              (first l)
              (second l)
              (* (third l) *clock-increment-in-milliseconds*)
              (* (float (/ (third l) (first l))) *clock-increment-in-milliseconds*)
       )
    )

    (format istream "~%~%~1,8@T~1,8@TInherited Operation Invocation~%~%")
    (format istream 
            "Operation~1,8@TIterations~1,8@TParents~1,8@TTotal Time~1,8@TTime per Call~%~%"
    )

    (dolist (l (reverse *inherited-messaging-results*))
      (format istream 
              "Operation Invocation~1,8@T~D~1,8@T~D~1,8@T~1,8@T~8,2F~1,8@T~8,2F~%" 
              (first l)
              (second l)
              (* (third l) *clock-increment-in-milliseconds*)
              (* (float (/ (third l) (first l))) *clock-increment-in-milliseconds*)
       )
    )
    (format istream "~|")
  ) ;with-open-file

  (setf *define-type-results* NIL)
  (setf *creation-results* NIL)
  (setf *define-method-results* NIL)
  (setf *messaging-results* NIL)
  (setf *inherited-messaging-results* NIL)

) ;print-results

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide "co-profmacs")

