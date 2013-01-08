;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         co-parse.l
; RCS:          $Revision: 1.1 $
; SCCS:         %A% %G% %U%
; Description:  Commonobjects parser for the Commonobjects-Commonloops
;               interface.
; Author:       Roy D'Souza, HPL/DCC
; Created:      20-Nov-86
; Modified:     4-Mar-87 11:22:29 (James Kempf)
; Mode:         Lisp
; Package:      COMMON-OBJECTS-PARSER
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Preliminaries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (provide "co-parse")

;;;Package COMMON-OBJECTS-PARSER contains the parser. For ease of
;;;  typing, CO-PARSER can be used.

;;;These symbols from the COMMON-OBJECTS package are needed at compile
;;;  time. Create the package if not there. Note that I don't want
;;;  to export them, because a user of the COMMON-OBJECTS package
;;;  shouldn't know about them. I therefore use fully qualified
;;;  symbols in the code.


(in-package :common-objects-parser)

;;Export functions needed for parsing


;;Need the PCL and pcl-patches module
;;(require "pcl")
;;(require "pcl-patches")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Constant Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Type names are set to this when types are undefined.

(defconstant* $UNDEFINED-TYPE-NAME '*now-an-undefined-type*)

;;Offsets into the vector used to parse type definitions.

(defconstant* $TYPE-INFO-SLOT 0)

(defconstant* $TYPE-NAME-SLOT 1)

(defconstant* $VARIABLE-NAMES-SLOT 2)

(defconstant* $INITABLE-VARIABLES-SLOT 3)

(defconstant* $SETTABLE-VARIABLES-SLOT 4)

(defconstant* $GETTABLE-VARIABLES-SLOT 5)

(defconstant* $PARENT-TYPES-SLOT 6)

(defconstant* $PARENTS-INFO-SLOT 7)
	  
(defconstant* $A-LIST-METHOD-TABLE-SLOT 8)

(defconstant* $TREAT-AS-VARIABLES-SLOT 9)

(defconstant* $INIT-KEYWORDS-SLOT 10)

(defconstant* $NO-INIT-KEYWORD-CHECK-SLOT 11)

(defconstant* $METHODS-TO-NOT-DEFINE-SLOT 12)

(defconstant* $METHODS-TO-INHERIT-SLOT 13)

(defconstant* $LET-PSEUDO-INFO-SLOT 14)
	  
(defconstant* $EXPLICITLY-LISTED-METHODS-SLOT 15)

;;List of all universal method names

(defconstant*
 $DEFINE-TYPE-UNIVERSAL-METHODS
 '(:describe
   :print
   :initialize
   :initialize-variables
   :init
   :eql
   :equal
   :equalp
   :typep
   :copy
   :copy-state
   :copy-instance)
)

;;Size of the vector used in type definition parsing.

(defconstant* $INFO-NUMBER-OF-SLOTS 16)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General Macro Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro get-parents-info (type-info)

; Allow for more convenient access of parent information.

 `(aref ,type-info $parents-info-slot))

(defmacro set-parents-info (type-info new-value)
 `(setf (aref ,type-info $parents-info-slot) ,new-value))

(defmacro co-deftype-error (format &rest arguments)

  `(error (concatenate 'simple-string
		       "DEFINE-TYPE: In type '~s', "
		       ,format)
	    ,@arguments))


(defmacro define-method-error (format &rest arguments)

 `(error
    (format nil
            (concatenate 'simple-string "DEFINE-METHOD: " ,format)
            ,@arguments)))

(defmacro return-keyword-from-variable (var)
    `(intern ,var (find-package "KEYWORD"))
)

;;type-partially-defined?-Find out if a CommonLoops class is
;;  defined and return the class object if so. 

(defmacro type-partially-defined? (name)

 `(class-named ,name T))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General Function and Method Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;type-name-Return the name of the type

(defun type-name (tinfo) 

   (if (%instancep tinfo)
     (class-name tinfo)
     (svref tinfo $TYPE-NAME-SLOT)
   )

) ;type-name

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Top Level Type Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun co-parse-define-type-call
 (define-type-call type-name doc-string options-list)

; Parse the various pieces of the call to DEFINE-TYPE.  Return multiple values
; of the form: (TYPE-NAME DOC-STRING OPTIONS-LIST) OPTIONS-LIST doesn't have to
; exist.  If it doesn't, NIL is returned for it value (assuming NIL is
; given as their initial value passed into the routine).  In either
; case it is disreguarded.  Example call, DEFINE-TYPE-CALL =
; (DEFINE-TYPE NOSE (:INHERIT-FROM PARENT)).

 (setf define-type-call (cdr define-type-call))

; This should now be the list of arguments to the DEFINE-TYPE.
; Example define-type-call = (NOSE (:INHERIT-FROM PARENT)).

 (unless (proper-list define-type-call)

 ; THEN The call to DEFINE-TYPE is not a proper list.

   (error
    (format nil
            "DEFINE-TYPE: The call,~% (DEFINE-TYPE '~S'),~% is missing arguments or is not a proper list."
            define-type-call)))

; Get the name of the type

 (setf type-name (first define-type-call))
 (setf define-type-call (cdr define-type-call))

; see if there is a documentation string

 (when
   (setq doc-string
          (and (consp define-type-call)
               (stringp (car (the cons define-type-call)))
               (list (car (the cons define-type-call)))
          ; list form for ,@ substitution
          ))
   (setf define-type-call (cdr define-type-call)))

; Example, define-type-call = ((:INHERIT-FROM PARENT)).
; Now look for options.

 (when (consp define-type-call)

 ; THEN We have options.

   (setf options-list define-type-call))

; Return the parsed fields as a list for MULTIPLE-VALUE-SETQ

 (values type-name doc-string options-list)

) ;co-parse-define-type-call

;;proper-list-Return T if X is a proper list, i.e., no dotted tail

(defun proper-list (x)

; Return T on if x is a proper list (i.e., not (a b c . d)).  NIL is
; not considered a proper list.

 (and (consp x) (not (cdr (last x)))))

(defun co-process-var-options
 (type-info options-list var-names var-assignments)

; Returns multiple values.  These values are:
;    (VAR-NAMES VAR-ASSIGNMENTS OPTIONS-LIST)
; Go through OPTIONS-LIST and find all the :VAR options.  Take
; these and process them producing the list of variable names, the
; variable assignment code and the list of options without the :VAR
; options.

  (let
   (
     (variable nil)
     (var-assignment nil)
     (new-options-list nil)
     (option-name nil)
     (option-info nil)
   )
;;;;    (Declare (ignore option-name))

   (dolist (option options-list 
            (values var-names var-assignments new-options-list)
           )

   (multiple-value-setq (option-name  option-info)
			(option-ok? option type-info 'regular-option)
   )

   ; Will only return to here if we didn't get an error.

   ; Check if spec is an instance variable spec

   (if (not (member 'variable-option (cdr option-info) :test #'eq))

    ;;THEN Add this non-:VAR option to the options list

    (setf new-options-list (nconc new-options-list (list option)))



   ; ELSE We have a instance variable specification.
   ;      Now return the name of the variable and initialization
   ;      code.

   
    (progn
      (multiple-value-setq (variable var-assignment)
		           (parse-option
			     type-info
			     var-names
			     option
			     option-info
                           )
      )
 

      (setf var-names (nconc var-names (list variable)))

      (when var-assignment

        ; THEN Add the assignment to the list of assignments.

        (setf var-assignments
	       (nconc var-assignments (list var-assignment))
        )
      ) ;when

    ) ;progn

  ) ;if
 ); dolist

 ) ;let

); end co-process-var-options

(defun co-parse-options (type-info var-names options)

; It is legal for OPTIONS to be NIL.
; Example: OPTIONS = ((:REDEFINED-METHODS m1 m2 m3)
;                     :ALL-INITABLE)

 (let ((options-so-far nil)
       (option-name nil)
       (option-info nil))

   (dolist (option options)

   ; OPTION-INFO will be NIL if OPTION-NAME is not a legal
   ; option, or a list of information that tells what
   ; characteristics this option has.  Note that currently, if an
   ; error occurs in OPTION-OK? we will NOT return to this
   ; function.  The check for '(WHEN OPTION-INFO...' is for future
   ; continuable errors. If 'ONCE' is on this list, it means the
   ; option can only occur once.

           (multiple-value-setq (option-name  option-info)
                          (option-ok? option type-info 'regular-option))
           (when option-info

           ; THEN The OPTION is a real one.
           ;      Now make sure it doesn't occur more then once.

             (if
               (and (member option-name options-so-far :test #'eq)

                    (member 'once (cdr option-info) :test #'eq))

             ; THEN We have duplicate options.  Give an error.

               (co-deftype-error
                "duplicate option,~% '~s',~% specified."
                (type-name type-info)
                option)

             ; ELSE Everything is ok.

               (progn
                 (setf options-so-far (cons option-name options-so-far))
                 (parse-option type-info var-names option option-info)))))
 ))

(defun parse-option (type-info var-names option option-info)

; This routine calls the right function to parse OPTION.  This
; function is the first element of OPTION-INFO.  Example: OPTION =
; (:REDEFINED-METHODS M1 M2 M3) The option given is either a symbol
; or a list.  When a list, the rest of the arguments will be passed to
; the function (may be NIL).  If a symbol, NIL is passed as arguments.
; NOTE: Should make sure that the value returned by the option is
;       the value of this routine, since some code may want to use
;       the value returned (like the caller of the :VAR option).

 (apply (car option-info)
        (list var-names (if (consp option) (cdr option) nil) type-info)))

(defun option-ok? (option type-info type-of-option)

; Return the information about this option or NIL.  Return the name of
; the option followed by the information for the option as a pair.  If
; the option is not of the correct form give an error message.  Check
; to make sure the option exists.  Also check that the form of option
; is legal according to the information returned.  This includes
; whether the option is allowed as a symbol or in list form.  And
; whether it is allowed to not have any arguments when in the list
; form.  Also if a list, check if each element is a symbol, and not NIL.
; This is done if CHECK-ARGUMENTS was included in the option
; information.  If the KEYWORDS option is also included with
; CHECK-ARGUMENTS, each of the symbols given must also be in the
; keyword package.  If VARIABLES is included in the option information,
; SELF is also checked for each option element.  The
; option CAN-HAVE-LIST-ELEMENTS causes list element arguments to be
; ignored. If this option is not there and a list element is
; found, an error message is issued.  Type-info is used strictly for
; error messages.  Will return NIL for the error conditions.  Sample,
;
; OPTION = '(:REDEFINED-METHODS A B C)' or ':ALL-SETTABLE'
;
; TYPE-OF-OPTION is used to decide wheter we are dealing with an
; option or a suboption of :INHERIT-FROM.  NOTE: Currently, this
; function will never return if an error occurs but we prepare for
; future continuable errors.  

 (let*
   ((option-info
     (if (consp option)

     ; THEN Use the first element of the option as the option name.

       (return-option-info (car option) type-of-option)

     ; ELSE Use the option itself as the option name.

       (return-option-info option type-of-option)))

    (type-name (type-name type-info))
    (check-as-variables (member 'variable option-info :test #'eq))
    (can-have-list-elements
     (member 'can-have-list-elements option-info :test #'eq))
    (keyword-arguments (member 'keywords option-info :test #'eq)))


   (unless option-info

   ; THEN We have an illegal option.

     (co-deftype-error
      "no such option (or suboption) as:~% '~s'."
      type-name
      option))

 ; We have a real option.  Make sure it is of the right form.

   (if (consp option)

   ; THEN Check to make sure it can be a pair.

     (if
       (not (member 'list (cdr option-info) :test #'eq))

     ; THEN Wrong form for option.

       (co-deftype-error
        "option,~% '~S',~% must occur as a symbol."
        type-name
        option)

     ; ELSE Ok so far.  Make sure the list form is a proper list.
     ; Now check if the option has no arguments and if
     ; if does make sure it can.

       (progn
         (unless (proper-list option)
         ; THEN Not a proper list.
           (co-deftype-error
            "the option,~% '~S',~% must be a proper list."
            type-name
            option))
         (if
           (and (not (cdr option))
                (not (member 'no-arguments (cdr option-info) :test #'eq)))
         
         ; THEN Arguments must be specified to option.
         
           (co-deftype-error
            "option,~% '~S',~% requires arguments."
            type-name
            option)

         ; ELSE Check each element of the list, if necessary, to
         ; make sure it is a symbol, not NIL. Also check for 
         ; SELF if VARIABES is in the
         ; option info.
         ; Return the information.

           (progn
             (when (member 'check-arguments (cdr option-info) :test #'eq)

             ; THEN Check the arguments.

               (dolist (option-arg (cdr option))
                       (if (consp option-arg)
                         (unless can-have-list-elements

                         ; THEN List arguments are not allowed.

                           (co-deftype-error
                            "illegal argument '~S' found in option,~% '~S'."
                            type-name
                            option-arg
                            option))

                       ; ELSE Check if a correct symbol.

                         (if
                           (or
                             (not (co-legal-type-or-method-name option-arg))
                             (and check-as-variables
                                  (not
                                   ;; TODO: fixed?
                                    (legal-instance-variable check-as-variables))))

                         ; THEN Illegal argument in option.

                           (co-deftype-error
                            "illegal argument '~S' found in option,~% '~S'."
                            type-name
                            option-arg
                            option)

                         ; ELSE Check if the option-arg must be a keyword.

                           (when
                             (and keyword-arguments
                                  (not (keywordp option-arg)))

                           ; THEN We have a DEFINE-TYPE in which the
                           ;      arguments must all be symbols in the
                           ;      keyword package.
                             (co-deftype-error
                              "'~S' of the option,~%'~S'~%is illegal.  Must be a symbol from the keyword package."
                              type-name
                              option-arg
                              option))))))
             (values (car option) option-info)))))

   ; ELSE We have the symbol form of the option.

     (if (member 'symbol (cdr option-info) :test #'eq)

     ; THEN Return the information.

       (values option option-info)

     ; ELSE Wrong form for option.

       (co-deftype-error
        "option,~% '~S',~% must occur in list form."
        type-name
        option)))))


(defun co-legal-type-or-method-name (type-or-method-name)

; Return T only if the name given is a non-nil symbol.

 (and (symbolp type-or-method-name) type-or-method-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Detailed Option Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun return-option-info (option-name option-type)

; Whenever a new option is added, this function must be updated.
; Should return NIL, if garbage option-names are given.  The option
; information returned has the form:
;     (FUNCTION-NAME . INFORMATION).
; FUNCTION-NAME is the name of the function to call that parses the
; given option.  INFORMATION is a list of information to use in
; syntaxing the option.  This list includes:
;    SYMBOL          - option can occur in symbol form.
;    LIST            - option can occur in list form.
;    CHECK-ARGUMENTS - When an option is in list form, this specifies that
;                      each element of the option list is to be checked to 
;                      be a symbol which is not NIL.
;    KEYWORDS        - An addition to the CHECK-ARGUMENTS option, this says
;                      that each element must be a symbol from the keyword 
;                      package.  Must occur with the CHECK-ARGUMENTS option.
;    NO-ARGUMENTS    - This specifies that a list form of the option can 
;                      occur without any arguments (i.e.,
;                      (:METHODS).
;    VARIABLE        - The items in this options list are instance variables.
;                      Check that they are not SELF or MYSELF.Make sure 
;		       they are not symbols from the
;                      keyword package.
;    CAN-HAVE-LIST-ELEMENTS - This option says that having list elements is 
;                             legal. These elements are simply ignored.
;    ONCE            - This option can only occur once.
;    VARIABLE-OPTION - Currently used in the :VAR option.  Tells whether
;                      an option is a variable option (:VAR) without
;                      using the name of the option.  This allows easy
;                      renaming of the :VAR option.
;    VALUE-RETURNED-SUBOPTION - States that this suboption returns a
;                               value that is needed.  A test is made
;                               to save the return value when a suboption
;                               has this characteristic.
;    
; Note that this list is used for parsing suboptions as well as
; options.  The handling of suboptions and options in the same way is
; done for flexibility and understandability even though some of the
; options may not currently apply to both options and suboptions.
;

 (case option-type

   (var-suboption (return-var-suboption-info option-name))

   (inherit-from-suboption
    (return-inherit-from-suboption-info option-name))

   (regular-option (return-regular-option-info option-name))))

(defun return-var-suboption-info (option-name)

; Return information as stated in comments of RETURN-OPTION-INFO
; about the suboptions of the :VAR option.

 (case option-name

   (:init '(parse-var-init-suboption list once value-returned-suboption))

   (:type '(parse-var-type-suboption list once))

   (:initable '(parse-var-initable-suboption symbol once))

   (:settable '(parse-var-settable-suboption symbol once))

   (:gettable '(parse-var-gettable-suboption symbol once))

   (otherwise nil)
 ))

(defun parse-var-initable-suboption (args initable-variable type-info)
  (declare (ignore args))

; ARGS will always be NIL.

 (setf (svref type-info $initable-variables-slot)
        (add-to-set
         (svref type-info $initable-variables-slot)
         initable-variable)))

(defun parse-var-gettable-suboption (args gettable-variable type-info)
  (declare (ignore args))

; ARGS will always be NIL.

 (setf (svref type-info $gettable-variables-slot)
        (add-to-set
         (svref type-info $gettable-variables-slot)
         gettable-variable)))

(defun parse-var-settable-suboption (args settable-variable type-info)
  (declare (ignore args))

; ARGS will always be NIL.

 (setf (svref type-info $initable-variables-slot)
        (add-to-set
         (svref type-info $initable-variables-slot)
         settable-variable))

 (setf (svref type-info $gettable-variables-slot)
        (add-to-set
         (svref type-info $gettable-variables-slot)
         settable-variable))

 (setf (svref type-info $settable-variables-slot)
        (add-to-set
         (svref type-info $settable-variables-slot)
         settable-variable)))

(defun add-to-set (set new-elements)

; Add the elements in NEW-ELEMENTS to SET if they are not already
; there.  NEW-ELEMENTS can be a list of id's or an id.  It is assumed
; that the order of the elements within the set is NOT important.  If
; NEW-ELEMENTS is NIL, simply return set.

 (cond ((null new-elements) set)
       ((symbolp new-elements)

       ; THEN Add the element to the set, if necessary.

        (adjoin new-elements set :test #'eq))

       (t 

       ; ELSE Add each element of the list of elements.

          (let ((new-set set))
            (dolist (element new-elements)
                    (setf new-set (adjoin element new-set :test #'eq)))
            new-set))))


(defun parse-var-type-suboption (args variable type-info)

; Example, ARGS = (FIXNUM).  A declaration like (:TYPE FIXNUM) =>
; (DECLARE (TYPE FIXNUM A)).

 (unless (and (consp args) (= (length args) 1))  ;rds 3/8 eq->=

 ; THEN We have something like (:TYPE . 2).

   (co-deftype-error
    "'~S'~% is an illegal form of :TYPE suboption."
    (type-name type-info)
    (cons :type args)))

; Add this declaration to the list of declarations.
; Note that more will be added to this slot when :VARIABLES suboptions are
; parsed, and at the end parsing the type. :VARIABLES is, however,
; currently unsupported.

 (setf (svref type-info $let-pseudo-info-slot)
        (nconc (svref type-info $let-pseudo-info-slot)
               (list `(declare (type ,(car args) ,variable))))))


(defun parse-var-init-suboption (args variable type-info)

; Return the variable initialization form. For example, if VARIABLE = 
; REAL-PART and ARGS = (0.0), would return:
;      (unless
;             (assignedp real-part)
;             (setf real-part 0.0))	     

 (unless (and (consp args) (= (length args) 1)) ;rds 3/8 eq->= 

 ; THEN We have something like (:INIT 1 2).

   (co-deftype-error
    "illegal initialization form,~%'~S',~%given for instance variable '~S'."
    (type-name type-info)
    (cons :init args)
    variable))

 (let ((default-value (first args)))
   `(unless 
       (co::assignedp ,variable)

    ; THEN

      (setf ,variable ,default-value))))

(defun return-inherit-from-suboption-info (option-name)

; Return information as stated in comments of RETURN-OPTION-INFO
; about the suboptions of the :INHERIT-FROM option.

 (case option-name

   (:init-keywords
    '(parse-init-keywords-suboption
      symbol
      list
      once
      check-arguments
      keywords))

   ;;:VARIABLES suboption not allowed in COOL. This is due to
   ;;  lack of code walker hooks.

#|
   (:variables
    '(parse-variables-suboption
      list
      once
      no-arguments
      check-arguments
      variable
      can-have-list-elements))
|#

   (:methods
    '(parse-methods-suboption list once check-arguments no-arguments))

   (otherwise nil)))

(defun return-regular-option-info (option-name)

; Return information as stated in comments of RETURN-OPTION-INFO
; about the options of DEFINE-TYPE.

 (case option-name


   ;;:FAST-METHODS not supported in COOL. Implementation dependent.

#|
   (:fast-methods
    '(parse-fast-methods-option list once check-arguments no-arguments))
|#

   ;;In line methods are not supported in COOL. Implementation dependent.

#|
   (:inline-methods
    '(parse-inline-methods-option list once check-arguments no-arguments))

   (:notinline-methods
    '(parse-notinline-methods-option
      list
      once
      check-arguments
      no-arguments))

|#

   (:init-keywords
    '(parse-init-keywords-option
      list
      once
      check-arguments
      no-arguments
      keywords))

   (:no-init-keyword-check
    '(parse-no-init-keyword-check-option symbol once))

   (:inherit-from '(parse-inherit-from-option list))

   (:var '(parse-var-option list variable-option))

   (:redefined-methods
    '(parse-redefined-methods-option
      list
      once
      check-arguments
      no-arguments))

   (:all-settable '(parse-all-settable-option symbol once))

   (:all-gettable '(parse-all-gettable-option symbol once))

   (:all-initable '(parse-all-initable-option symbol once))

   (otherwise nil)))

(defun parse-init-keywords-suboption (type-info parent-type-info args)

; If ARGS is NIL, we have the symbol form.  If ARGS is a list, we have
; the list form.  Examples: ARGS = NIL
;                           ARGS = (:EXCEPT j k l), (:EXCEPT)
; (:INIT-KEYWORDS :EXCEPT) is treated as all keywords.  If this
; function returns, then everything went ok as far as errors.  If ARGS
; is a list, we know it is proper, and each init keyword is a symbol and
; not NIL.  This function may change the $INIT-KEYWORDS-SLOT of
; type-info.

 (let*
   ((parent-init-keywords
     (co::init-keywords parent-type-info))
    (keywords-to-add parent-init-keywords))

   (when args

   ; THEN We have the except form.
   ;      Check and make sure the :EXCEPT is found.

     (if
       (not (eq (car args) ':except))

     ; THEN We have an error.

       (co-deftype-error
        "~%'~S'~% was found following the :INIT-KEYWORDS suboption, expected to see 'EXCEPT'."
        (type-name type-info)
        (car args))

     ; ELSE ok so far.

       (progn (setq args (cdr args))
              (when (consp args)

              ; THEN There is something following the :EXCEPT.

                (dolist (keyword args)

                ; See if the keyword is in the list of REAL 
                ; keywords for the parent.

                        (if
                          (not
                            (member keyword
                                    parent-init-keywords
                                    :test
                                    #'eq))

                        ; THEN Print a warning message is ignore.

                          (warn
                            (format
                              NIL
                              "DEFINE-TYPE: Init keyword, '~A', is not a keyword of '~A' in :INIT-KEYWORDS suboption."
                              keyword
                              (type-name parent-type-info)))

                        ; ELSE The keyword is legit.

                          (setf keywords-to-add
                                 (remove keyword
                                         keywords-to-add
                                         :test
                                         #'eq
                                         :count
                                         1))))))))

 ; keywords-to-add should be correctly setup now.
 ; Add the elements of this list that are not already there, to the
 ; existing list of keywords for this type.

   (setf (svref type-info $INIT-KEYWORDS-SLOT)
          (add-to-set
           (svref type-info $INIT-KEYWORDS-SLOT)
           keywords-to-add))))

(defun parse-methods-suboption (type-info parent-type-info args)

; At this point, we know that ARGS is a proper list where each element
; is a symbol that is not NIL.  Sample, args = (:EXCEPT M1 M2 M3),
; (:EXCEPT), ().  If method names are duplicated, the duplicates are
; ignored.  This function should change the $METHODS-TO-INHERIT-SLOT as
; in the following example:
;    PARENT-TYPE-INFO for PARENT2 and the total methods for PARENT2
;    are M1, M2,...,M6 and if ARGS = (:EXCEPT M1 M2 M3), and if
; $METHODS-TO-INHERIT-SLOT looked like:
;       ((<parent1 type info object> .(M1 M2 M3))), then
; $METHODS-TO-INHERIT-SLOT would look like:
;     ((<parent1 type info object> .  (M1 M2 M3))
;      (<parent2 type info object> . (M4 M5 M6)))
; after this routine completes.  When this routine finishes, we are
; guaranteed that each method added to the $METHODS-TO-INHERIT-SLOT is an
; existing methods of the parent.

 (let
   ((parent-methods
     (co::method-alist parent-type-info))
    (methods-to-inherit nil)
    (except-form?
     (when (and args (eq (car args) ':except))

     ; THEN Skip over the :EXCEPT argument.

       (setf args (cdr args))
       t)))

 ; ARGS will be NIL or a list at this point.  If NIL, we have (:METHODS)
 ; or (:METHODS :EXCEPT).

   (dolist (method args)
           (unless (assoc method parent-methods :test #'eq)

           ; THEN The method doesn't exits, give a warning.

             (warn
               (format nil
                       "DEFINE-TYPE: Method '~S' of the :METHODS suboption doesn't~% exist in parent '~S'."
                       method
                       (type-name parent-type-info)))))
 
   (if except-form?

   ; THEN We have the :EXCEPT form. List all methods that are not
   ;      specified and are not universal methods.  If 
   ;      (:METHODS :EXCEPT), all methods not universal methods are
   ;      added.

     (dolist (method-function-pair parent-methods)

     ; As long as the method is not an exception (:EXCEPT)
     ; and not a universal method of the parent, inherit it.

             (unless
               (or (member (car method-function-pair) args :test #'eq)
                   (member (car method-function-pair)
			   $DEFINE-TYPE-UNIVERSAL-METHODS
                           :test
                           #'eq))

             ; THEN The method we are looking at is desired 
             ;      for inheritance.

               (setf methods-to-inherit
                      (add-to-set
                       methods-to-inherit
                       (car method-function-pair)))))

   ; ELSE We have the normal form.  If some of the args were not real
   ;      methods. If (:METHODS), nothing is done.

     (dolist (method args)
             (when (assoc method parent-methods :test #'eq)

             ; THEN The method really exists.

               (setf methods-to-inherit
                      (add-to-set methods-to-inherit method))

             ; Add to the list of explicitly stated methods to inherit.
             ; This is used for error checking with methods to not
             ; redefine later.

               (setf
                 (svref type-info $EXPLICITLY-LISTED-METHODS-SLOT)
                  (add-to-set
                   (svref type-info
                          $EXPLICITLY-LISTED-METHODS-SLOT)
                   method)))))

 ; Now add this list of methods to the type-info vector.
 ; 'methods-to-inherit' may be NIL.

   (setf (svref type-info $METHODS-TO-INHERIT-SLOT)
          (append (svref type-info $METHODS-TO-INHERIT-SLOT)
                  (list (cons parent-type-info methods-to-inherit))))))


(defun parse-var-option (var-names args type-info)

; ARGS = (IV1 (:TYPE INTEGER) (:INIT 0.0) :SETTABLE) Return something
; of the form:
;    (VARIABLE-NAME . VAR-ASSIGNMENT)
; VARIABLE-NAME is the name of the instance variable.  VAR-ASSIGNMENT
; is the code needed to initialize this instance variable.

 (unless (and (consp args) (symbolp (car args)))

 ; THEN We have an error.

   (co-deftype-error
    "a symbol must follow a :VAR option."
    (type-name type-info)))

 (let ((variable (car args))
       (var-assignment nil))

 ; Make sure the instance variable name is legal.

   (instance-variable-ok? variable var-names (type-name type-info))

 ; Now parse all the suboptions of the :VAR option.
 ; VAR-ASSIGNMENT will be NIL if there is no :INIT suboption.

   (setf var-assignment
          (parse-var-suboptions type-info (cdr args) variable))
   (values variable var-assignment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Detailed :VAR Suboption Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-var-suboptions (type-info suboptions variable)

; This routine returns the code for initialization of the instance
; variable VARIABLE.  It is legal for suboptions to be NIL.  For
; understandibility, expandability, and consistancy the parsing of
; suboptions uses the same techniques with the same keywords that option
; option parsing does.  This is true even though some of the option
; information may not be shared between options and suboptions.  See
; CO-PARSE-OPTIONS and its constituent routines.
;
; Example: SUBOPTIONS = ((:INIT 0.0)
;                        (:TYPE INTEGER)
;                        :SETTABLE)

 (let ((suboptions-so-far nil)
       (suboption-name nil)
       (suboption-info nil)
       (init-info nil)
      )

   (dolist (suboption suboptions)

               ; SUBOPTION-INFO will be NIL if SUBOPTION-NAME is not a
               ; legal suboption, or a list of information that tells what
               ; characteristics this suboption has.  Note that currently,
               ; if an error occurs in SUBOPTION-OK? we will NOT return
               ; to this function. The check for '(WHEN SUBOPTION-INFO...)'
               ; is for future continuable errors. If 'ONCE' is on this
               ; list, it means the suboption can only occur once.

                (multiple-value-setq (suboption-name suboption-info)
                               (option-ok?
                                suboption
                                type-info
                                'var-suboption))
                (when suboption-info

                ; THEN The suboption is a real one.
                ;      Now make sure it doesn't occur more then once.

                  (if
                    (and
                      (member suboption-name suboptions-so-far :test #'eq)
                      (member 'once (cdr suboption-info) :test #'eq))

                  ; THEN We have duplicate suboptions.  Give an error.

                    (co-deftype-error
                     "duplicate suboption,~% '~S',~% specified to :VAR option."
                     (type-name type-info)
                     suboption)

                  ; ELSE Everything is ok.

                    (progn
                      (setf suboptions-so-far
                             (cons suboption-name suboptions-so-far))
                      (if
                        (member 'value-returned-suboption
                                (cdr suboption-info)
                                :test
                                #'eq)

                      ; THEN We must save the return value.

                        (setf init-info
                               (parse-var-suboption
                                type-info
                                variable
                                suboption
                                suboption-info))

                      ; ELSE We don't care about the return value.

                        (parse-var-suboption
                         type-info
                         variable
                         suboption
                         suboption-info)))))

      ) ;dolist

      ;;Return the init-info

      init-info

  ) ;let

) ;end parse-var-suboptions

(defun parse-var-suboption (type-info variable suboption suboption-info)

; This routine calls the right function to parse SUBOPTION.  This
; function is the first element of SUBOPTION-INFO.  Example:
; SUBOPTION = (:INIT 0.0) The SUBOPTION given is either a symbol or a
; list.  When a list, the rest of the arguments will be passed to the
; function (may be NIL).  If a symbol, NIL is passed as arguments.
; NOTE: Should make sure that the value returned by the suboption is
;       the value of this routine, since some code may want to use
;       the value returned (like the value of the :INIT suboption).

 (apply (car suboption-info)
        (list (if (consp suboption) (cdr suboption) nil)
              variable
              type-info)))


(defun instance-variable-ok? (variable list-of-variables type-name)

; Signal a standard error if the variable is SELF,
; one of the variables that are already in the list
; of variables, or a keyword.
; TYPE-NAME is used for error messages by CO-DEFTYPE-ERROR.

 (unless (legal-instance-variable variable)

 ; THEN error.

     (co-deftype-error
      "'SELF' NIL, or symbol from the keyword package~%was found as an instance variable."
      type-name))

 (when (member variable list-of-variables :test #'eq)

 ; THEN We have a duplicate variable.

   (co-deftype-error
    "instance variable '~S' occurs more~%than once."
    type-name
    variable)))

(defun legal-instance-variable (variable)

; Return T if VARIABLE satisfies restrictions on instance variables.
; Return NIL otherwise.  Currently, the variable must be a non-NIL symbol
; that is not SELF.
; Must also be a symbol that is NOT in the
; keyword package.

 (and (symbolp variable)
      variable
      (not (eq variable 'co::self))
      (not (keywordp variable))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Parsing of :ALL-xxx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-all-initable-option (var-names args type-info)

; Parses: :ALL-INITABLE.  ARGS will be NIL.

 (parse-initable-option var-names args type-info))

(defun parse-all-gettable-option (var-names args type-info)

; Parses: :ALL-GETTABLE.  ARGS will be NIL.

 (parse-gettable-option var-names args type-info))

(defun parse-all-settable-option (var-names args type-info)

; Parses: :ALL-SETTABLE.  ARGS will be NIL.

 (parse-settable-option var-names args type-info))

(defun parse-gettable-option (var-names args type-info)

; Example ARGS = (A B C D), NIL.
; Duplicate variables specified are ignored.

 (dolist (gettable-variable (or args var-names))

         (if (member gettable-variable var-names :test #'eq)

         ; THEN This variable is a real instance variable.

           (setf (svref type-info  $gettable-variables-slot)
                  (add-to-set
                   (svref type-info $GETTABLE-VARIABLES-SLOT)
                   gettable-variable))
         ; ELSE We have an illegal variable name.

           (co-deftype-error
            "variable '~S' in the settable~% options list is not an instance variable.~%"
            (type-name type-info)
            gettable-variable))))

(defun parse-settable-option (var-names args type-info)

; Example ARGS = (A B C D), NIL.  Duplicate variables specified are
; ignored. Each settable instance variable
; is added to the list of gettable and initable instance variables as
; well.

 (dolist (settable-variable (or args var-names))

         (if (not (member settable-variable var-names :test #'eq))

         ; THEN We have an illegal variable name.

           (co-deftype-error
            "variable '~S' in the settable~% options list is not an instance variable~%."
            (type-name type-info)
            settable-variable)

         ; ELSE This variable is a real instance variable.

           (progn
             (setf (svref type-info $initable-variables-slot)
                    (add-to-set
                     (svref type-info $initable-variables-slot)
                     settable-variable))
             (setf (svref type-info $gettable-variables-slot)
                    (add-to-set
                     (svref type-info $gettable-variables-slot)
                     settable-variable))
             (setf (svref type-info $settable-variables-slot)
                    (add-to-set
                     (svref type-info $settable-variables-slot)
                     settable-variable))))))

(defun parse-initable-option (var-names args type-info)

; Example ARGS = (A B C D), NIL. Duplicate
; variables specified are ignored.

 (dolist (initable-variable (or args var-names))

         (if (member initable-variable var-names :test #'eq)

         ; THEN This variable is a real instance variable.

           (setf (svref type-info  $initable-variables-slot)
                  (add-to-set
                   (svref type-info $initable-variables-slot)

                   initable-variable))
         ; ELSE We have an 
           (svref type-info $initable-variables-slot)
           ;;--- TODO: missing code?
           #|(error "variable '~S' in the initable~% options list is not an instance variable.~%"
           (type-name type-info)
           initable-variable)|#)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Parsing of :INIT-KEYWORDS Option and Suboption and :REDEFINED-METHODS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-init-keywords-option (var-names args type-info)
  (declare (ignore var-names))

; Parses: (:INIT-KEYWORDS <symbol>).  Doesn't use VAR-NAMES.  By the
; time this routine is called, each element of args has been checked to
; be a symbol not equal to NIL.  ARGS is also a proper list. For
; (:INIT-KEYWORDS), ARGS will be NIL.  We add the existing
; init-keywords because we may have hit :INIT-KEYWORDS suboptions from
; :INHERIT-FROM options.

 (setf (svref type-info $INIT-KEYWORDS-SLOT)
        (add-to-set (svref type-info $INIT-KEYWORDS-SLOT) args)))

(defun parse-no-init-keyword-check-option (var-names args type-info)
  (declare (ignore args var-names))

; Parses: :NO-INIT-KEYWORD-CHECK. VAR-NAMES is not used.

 (setf (svref type-info $NO-INIT-KEYWORD-CHECK-SLOT) t))

(defun parse-redefined-methods-option (var-names args type-info)

  (declare (ignore var-names))

; Parses: (:REDEFINED-METHODS M1 M2 M3), or (:REDEFINED-METHODS).  ARGS
; = (M1 M2 M3).  At this point, ARGS is guaranteed to be a proper list
; where each element is a symbol that is non-NIL.  For
; (:REDEFINED-METHODS), args is NIL.  NOTE: The order of arguments are
; stored away doesn't matter.

 (setf (svref type-info $methods-to-not-define-slot)
        (remove-duplicates args :test #'eq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Parsing of :INHERIT-FROM Option and Suboption
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-inherit-from-option (var-names args type-info)
  (declare (ignore var-names))

; ARGS is the list of remaining stuff inside the :INHERIT-FROM option.
; We know that ARGS is a proper list and that it has at least one element.
; Sample: ARGS = (PARENT1 (:METHODS M1 M2 M3)
;                         (:VARIABLES X Y Z)
;                         (:INIT-KEYWORDS :EXCEPT Q))
; VAR-NAMES is not used.  Note that for error handling to be changed to
; continuable errors, these options will have to be changed, since side
; effects to type info can occur before a syntax error occurs.  When
; finished, the $PARENT-TYPES-SLOT and the $PARENTS-INFO-SLOT may be
; changed.

 (if (and (consp args) (symbolp (car args))) 

 ; THEN The form of the parent is ok.
 ;      Now check if it is partially defined.

   (let ((parent-type-info (type-partially-defined? (car args)))
         (parents (svref type-info $PARENT-TYPES-SLOT))
         (new-parent (car args)))

     (if (not parent-type-info)

     ; THEN The parent isn't defined.  Give an error.

       (co-deftype-error
        "~%the parent '~s',~s of the :INHERIT-FROM option, is not defined."
        (type-name type-info)
        new-parent)
     
     ; ELSE The parent is partially defined.
     ;      First check that options specified are ok.
     ; Add the parent to the type-info slot.  We must append
     ; since the order is important -- the first :INHERIT-FROM option
     ; must be the first parent.
     ; Check that we don't have something like:
     ;      (INHERIT-FROM B...)
     ;      (INHERIT-FROM B...) within the type definition.
       (if
         (member new-parent parents :test #'eq)

       ; THEN Two or more parents that are the same parent.

         (co-deftype-error
          "~~Sarent '~s' of type '~s'~s can only be a parent once."
          (type-name type-info)
          new-parent
          (type-name type-info))

       ; ELSE Everything is ok.
       ; Add the parents type-info to be used later.
       ; This is stored in the same order as the parents in the
       ; $PARENT-TYPES-SLOT for consistency.
       
         (progn
           (set-parents-info
            type-info
            (append (get-parents-info type-info)
                    (list
                      (list new-parent parent-type-info '*place-holder*))))
           (setf (svref type-info $PARENT-TYPES-SLOT)
                  (append parents (list new-parent)))
           (parse-inherit-from-suboptions
            type-info
            parent-type-info
            (cdr args))))))

 ; ELSE The parent form is illegal.

   (co-deftype-error
    "~%a symbol must follow an :INHERIT-FROM~% option."
    (type-name type-info))))

(defun parse-inherit-from-suboptions
 (type-info parent-type-info suboptions)

; It is legal for SUBOPTIONS to be NIL.  For understandibility,
; expandability, and consistancy the parsing of subptions uses the same
; techniques with the same keywords for option information.  This is
; true even though some of the option information may not be shared
; between options and suboptions.  See CO-PARSE-OPTIONS and its
; constituent routines.  NOTE: If the name of :METHODS option is ever
; changed (in RETURN-OPTION-INFO) the references to :METHODS must be
; changed here as well.
;
; Example: SUBOPTIONS = ((:VARIABLES A B)
;                        (:METHODS C D)
;                        (:INIT-KEYWORDS EXCEPT J))

 (let ((suboptions-so-far nil)
       (suboption-name nil)
       (suboption-info nil))

   (dolist (suboption suboptions)

   ; SUBOPTION-INFO will be NIL if SUBOPTION-NAME is not a
   ; legal suboption, or a list of information that tells what
   ; characteristics this suboption has.  Note that currently,
   ; if an error occurs in SUBOPTON-OK? we will NOT return
   ; to this function. The check for (WHEN SUBOPTION-INFO...)
   ; is for future continuable errors. If 'ONCE' is on this
   ; list, it means the suboption can only occur once.

           (multiple-value-setq (suboption-name suboption-info)
				(option-ok?
				 suboption
				 type-info
				 'inherit-from-suboption))
           (when suboption-info

           ; THEN The suboption is a real one.
           ;      Now make sure it doesn't occur more then once.

             (if
               (and (member suboption-name suboptions-so-far :test #'eq)
                    (member 'once (cdr suboption-info) :test #'eq))

             ; THEN We have duplicate suboptions.  Give an error.

               (co-deftype-error
                "duplicate suboption,~s '~s',~s specified to :INHERIT-FROM option."
                (type-name type-info)
                suboption)

             ; ELSE Everything is ok.

               (progn
                 (setf suboptions-so-far
                        (cons suboption-name suboptions-so-far))
                 (parse-inherit-from-suboption
                  type-info
                  parent-type-info
                  suboption
                  suboption-info)))))

 ; Now check the one funny case: If the :METHODS option was NOT present.

   (unless (member ':methods suboptions-so-far :test #'eq)

   ; THEN We had no :METHODS suboption, so inherit all methods
   ;      (but not universal methods).  Do this by making
   ;      a suboption (:METHODS :EXCEPT), and having it parsed.

     (multiple-value-setq (suboption-name suboption-info)
                    (option-ok?
                     '(:methods :except)
                     type-info
                     'inherit-from-suboption))

     (parse-inherit-from-suboption
      type-info
      parent-type-info
      '(:methods :except)
      suboption-info))))

(defun parse-inherit-from-suboption
 (type-info parent-type-info suboption suboption-info)

; Example: SUBOPTION = (:INIT-KEYWORDS :EXCEPT J K L) The suboption
; given is either a symbol or a list.  When a list, the rest of the
; arguments will be passed to the function (may be NIL).  If a symbol,
; NIL is passed as arguments.

 (apply (car suboption-info)
        (list type-info
              parent-type-info
              (if (consp suboption) (cdr suboption) nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Method Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun co-parse-method-macro-call
 (spec argument-list body)

; Make sure that the type-name and method-name are ok.  Also, that the
; call is a proper list. 
; Note that use of instance variable names as formal
; parameter names to the method and use of SELF as a formal parameter
; name are not checked.

  (let
    (
      (type-name NIL)
      (method-name NIL)
    )

   ; Check to be sure the body is a proper list or NIL

   (unless (or (null body) (proper-list body))

   ; THEN the method definition is not a proper list

     (define-method-error
      "The call,~% '(DEFINE-METHOD ~S ~S ~S)',~% is missing arguments or is an improper list."
      spec argument-list body))

 ; Check the spec

   (unless (and (proper-list spec) (= (length spec) 2)) ;rds 3/8 eq->=

   ; THEN The form of the (type-name method-name) is incorrect.

     (define-method-error
      "The type-name and method-name in the call,~% '(DEFINE-METHOD ~S ~S ~S)',~% must be a two element proper list."
      spec argument-list body))

   (setf method-name (second spec))
   (setf type-name (first spec))
 
   (unless (co-legal-type-or-method-name type-name)

   ; THEN Invalid type.

     (define-method-error
      "Type name '~S' in the call,~% '(DEFINE-METHOD ~S ~S ~S)',~% must be a non-NIL symbol."
      type-name
      spec
      argument-list
      body))

   (unless (co-legal-type-or-method-name method-name)

   ; THEN Invalid method.

     (define-method-error
      "Method name '~S' in the call,~% '(DEFINE-METHOD ~S ~S ~S)',~% must be a non-NIL symbol."
      method-name
      spec
      argument-list
      body))

 ; Check that the argument-list is indeed a list.

   (unless (or (null argument-list) (proper-list argument-list))
     (define-method-error
      "The argument list in the call,~% '(DEFINE-METHOD ~S ~S ~S)',~%  is missing or must be a proper list."
      spec
      argument-list
      body))

  ) ;let
) ;co-parse-method-macro-call 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Call-Method Support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun co-parse-call-to-method (call-method-call which-func class-name)

; Parse a call to a CALL-METHOD or APPLY-METHOD. Signal any
; errors in syntax.
; 'which-func' is either "CALL-METHOD" or "APPLY-METHOD".

 (let ((method-name nil)
       (rest-of-call call-method-call))
 
   (setf rest-of-call (cdr rest-of-call))
 
 ; This should now be the list of arguments 

   (unless (proper-list rest-of-call)

   ; THEN The call to CALL-METHOD is not a proper list.

     (error
      (format nil
              "~A: The call,~% '~S',~% is missing arguments or is an improper list."
              which-func
              call-method-call)))

   ; If the form is APPLY-METHOD, check to be sure the argument list is
   ; not NIl

   (when (equalp which-func "APPLY-METHOD")
     (unless (cadr rest-of-call)
         
       (error
         (format nil
	         "APPLY-METHOD: The call,~% '~S',~% has no argument list."
                 call-method-call
         )
       )
     )
   )

   (setf method-name (first rest-of-call))
 
   (cond
     ((co-legal-type-or-method-name method-name)

     ; THEN We have the local form of call-method (i.e.,
     ;      (CALL-METHOD MOOSE 3) ) so just return.

      NIL
     )

     ; ELSE Check if a two element list, each element a symbol.

    ((consp method-name)
     (unless
       (and (= (length method-name) 2)
            (proper-list method-name)
            (co-legal-type-or-method-name (first method-name))
            (co-legal-type-or-method-name (second method-name))
            (co::legal-parent-p class-name (first method-name)))

     ; Incorrect parent form of call-method.

       (error
        (format nil
                "~A: Illegal parent reference '~S' in~% '~S'.~%  Must have the form: '(type-symbol operation-symbol)'."
                which-func
                method-name
                call-method-call)
      ))
    )       

    ; Anything else is an error.

    (t
      (error
       (format nil
               "~A: Incorrect form '~S' in~% '~S'.~%  Expecting non-NIL symbol or list or two non-NIL symbols."
               which-func
               method-name
               call-method-call))))

    ) ;let

) ;co-parse-call-to-method

(defun check-that-method-to-call-exists
 (possible-method-name child-name parent-name parent-methods)

; Return the name of the method we will be calling.
; The method name to use is determined as follows: First, always use the ':'
; version of the name.  If the method with this name is not defined,
; check if the name without the ':' is defined.  If it is, issue a
; warning message that we are calling this method.  If it isn't
; defined, issue a warning message that the method is not defined and
; that we will call the ':' version when it is defined.  For example,
; if we had the POSSIBLE-METHOD-NAME of A we would first check if a
; method named :A existed in the PARENT-METHODS.  If it does, we
; return :A.  If it doesn't, we see if a method with the name A
; exists.  If it does, we return this name and give a warning.  If it
; doesn't, we return :A and give a warning.

 (let*
   ((method-to-call
     (return-keyword-from-variable possible-method-name))
    (saved-method-to-call method-to-call))

   (unless (assoc method-to-call parent-methods :test #'eq)

   ; THEN The ':' version of the method doesn't exist.
   ;      Now check if the non-colon version exists.

     (setf method-to-call possible-method-name)

     (if
       (assoc method-to-call parent-methods :test #'eq)

     ; THEN We are calling the non-colon version of the method.
     ;      Give a warning message.

       (warn
         (format nil
                 "DEFINE-TYPE: In type, '~A', '~A' of :VARIABLES suboption, will reference the parent method '~A'."
                 child-name
                 possible-method-name
                 possible-method-name))

     ; ELSE Give a warning that we will assume calling the ':' version.

       (progn (setf method-to-call saved-method-to-call)
              (warn
                (format nil
                        "DEFINE-TYPE: In type, '~A', '~A' of :VARIABLES suboption, has no corresponding method defined in parent '~A'. Will assume you want to call method '~A'."
                        child-name
                        possible-method-name
                        parent-name
                        method-to-call)))))
   method-to-call))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

