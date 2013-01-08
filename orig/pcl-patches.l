
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         pcl-patches.l
; RCS:          $Revision: 1.1 $
; SCCS:         %A% %G% %U%
; Description:  Patches to Released PCL so CommonObjects works
; Author:       James Kempf, HP/DCC
; Created:      11-Nov-86
; Modified:     5-Mar-87 08:04:02 (James Kempf)
; Language:     Lisp
; Package:      PCL
; Status:       Distribution
;
; (c) Copyright 1987, HP Labs, all rights reserved.
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

;;Need the PCL module

(require "pcl")

(in-package 'pcl)
(use-package 'lisp)

;;These symbols are needed by CommonObjects

(export
  '(
    print-instance
    make-specializable
    rename-class
    call-next-method
    expand-with-make-entries
    method-type-specifiers
    method-arglist
  )
)

;;Note-Every implementation of CL will need to add the
;;  check for nonatomic type specifiers.

#+HP(setq *class-of*
	'(lambda (x) 
	   (cond ((%instancep x)
		  (%instance-class-of x))
		 ;; Ports of PCL should define the rest of class-of
		 ;; more meaningfully.  Because of the underspecification
                 ;; of type-of this is the best that I can do.
		 ((null x)
                  (class-named 'null))
                 ((stringp x)
                  (class-named 'string))
		 ((characterp x)
		  (class-named 'character))
		 (t
		  (or (class-named (atom-type-of (type-of x)) t)
		      (error "Can't determine class of ~S." x)
		  ))
            )
        )
)

#+ExCL(eval-when (load)
  (setq *class-of*
	'(lambda (x) 
	   (or (and (%instancep x)
		    (%instance-class-of x))	       
	      ;(%funcallable-instance-p x)
	       (and (stringp x) (class-named 'string))
	       (class-named (type-of x) t)
	       (error "Can't determine class of ~S." x)))
  )

)

;;Now arrange things so CLASS-OF gets recompiled when this file gets
;;  loaded

#-KCL(eval-when (load eval)

  (recompile-class-of)

)

;;atom-type-of-Return principle type. This is the first
;;  item on the type specifier list, or specifier itself,
;;  if the specifier is atomic.

(defun atom-type-of (x)

  (if (listp x)
    (car x)
    x
  )

) ;end atom-type-of

;;
;;
;;
;;
;; Default print-instance method
;;
;;
;;

(defmeth print-instance (instance stream depth) 
  (printing-random-thing (instance stream)    
    (format stream "instance ??")))

;;;New for CO


;;rename-class-Find the class object named old-name and rename to
;;  new-name

(defmeth rename-class ((old-name symbol) (new-name symbol))

  (rename-class (class-named old-name) new-name)

) ;end rename-class


;;rename-class-Change the name of the essential class's name to name

(defmeth rename-class ((class essential-class) (name symbol))

  (let
    (
      (old-name (class-name class))
    )


    (setf (class-name class) name)

    ;;Needed to be sure the naming hash table is OK

    (setf (class-named name) class)
    (setf (class-named old-name) NIL)
    name
  )

) ;end rename-class


;;
;;
;;
;; From class-prot.l
;;
;;
;;

;;JAK 2/15/86 Additional bug. OPTIMIZE-GET-SLOT and OPTIMIZE-SETF-OF
;;  GET-SLOT didn't seem to be getting called. This version calls
;;  them. NOTE-this has been added to CLASS-PROT.L so that the
;;  optimization functions get called in the kernel as well.

(defun expand-with-slots
       (proto-discriminator proto-method first-arg env body)
  (ignore proto-discriminator)
  (let ((entries (expand-with-make-entries proto-method first-arg))
	(gensyms ()))
    (dolist (arg first-arg)
      (push (list (if (listp arg) (car arg) arg)
		  (gensym))
	    gensyms))
    `(let ,(mapcar #'reverse gensyms)
       ,(walk-form (cons 'progn body)
	  :environment env
	  :walk-function
	  #'(lambda (form context &aux temp)
	      (cond ((and (symbolp form)
			  (eq context ':eval)
			  (null (variable-lexical-p form))
			  (null (variable-special-p form))
			  (setq temp (assq form entries)))
		     (if (car (cddddr temp))	;use slot-value?
                         (optimize-get-slot 
                          ;;;;  proto-method 	;;the method object ;rds 3/8 
                           (third temp)		;;the class object
			   `(get-slot ,(cadr (assq (cadr temp) gensyms))
				    ',(slotd-name (cadddr temp)))
                         )
			 `(,(slotd-accessor (cadddr temp))
			   ,(cadr (assq (cadr temp) gensyms)))))
		    ((and (listp form)
			  (or (eq (car form) 'setq)
			      (eq (car form) 'setf)))
		     (cond ((cdddr form)
			    (cons 'progn
				  (iterate ((pair on (cdr form) by cddr))
				    (collect (list (car form)
						   (car pair)
						   (cadr pair))))))
			   ((setq temp (assq (cadr form) entries))

;;JAK 2/14/87 Bug found. The following IF was not included, causing
;;  the second form to always be returned. This caused forms like
;;;  (SETF (NIL #:G1234) 5) to be generated, which aren't SETF expandable

			     (if (not (slotd-accessor (cadddr temp)))
			       (optimize-setf-of-get-slot
			        ;;; proto-method  ; rds 3/8
                                 (third temp)
			         `(setf-of-get-slot
			           ,(cadr (assq (cadr temp) gensyms))
			           ',(slotd-name (cadddr temp))
			           ,(caddr form))
				)

			       `(setf (,(slotd-accessor (cadddr temp))
				    ,(cadr (assq (cadr temp) gensyms)))
				   ,(caddr form))))
			   (t form)))
		    (t form)))))))

;;Default methods for optimize-get-slot and optimize-setf-of-get-slot

; rds 3/9 changed arglist to conform to new PCL 
; (defmeth optimize-get-slot (method class form)
;  form
;)
(defmeth optimize-get-slot (class form)
 form
 )

; rds 3/9 changed arglist to conform to new PCL
;(defmeth optimize-setf-of-get-slot (method class form)
;  form
;)
(defmeth optimize-setf-of-get-slot (class form)
 form
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide "pcl-patches")

