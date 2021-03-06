(cl:in-package #:sicl-minimal-extrinsic-environment)

(defparameter *imported-functions*
  '(;; Functions on CONS cells.
    cons car cdr cadr cddr cdddr append list
    consp atom null listp not endp
    length rplaca rplacd first second third rest getf member
    ;; Functions for arithmetic
    + - * / floor ceiling round evenp oddp = /= < <= > >=
    ;; Various functions
    coerce eq values
    ;; Funcions related to symbols
    gensym symbolp
    ;; Functions for calling functions
    funcall apply
    sicl-genv:find-package
    sicl-genv:get-setf-expansion
    (setf sicl-genv:function-type)
    (setf sicl-genv:function-lambda-list)
    (setf sicl-genv:fdefinition)
    cleavir-environment:global-environment
    cleavir-environment:symbol-macro-expansion
    cleavir-environment:macro-function
    cleavir-code-utilities:parse-macro
    cleavir-code-utilities:parse-deftype
    cleavir-code-utilities:separate-ordinary-body
    cleavir-code-utilities:list-structure
    sicl-conditionals:or-expander
    sicl-conditionals:and-expander
    sicl-conditionals:cond-expander
    sicl-conditionals:case-expander
    sicl-conditionals:ecase-expander
    sicl-conditionals:ccase-expander
    sicl-conditionals:typecase-expander
    sicl-conditionals:etypecase-expander
    sicl-conditionals:ctypecase-expander
    sicl-standard-environment-macros:defconstant-expander
    sicl-standard-environment-macros:defvar-expander
    sicl-standard-environment-macros:defparameter-expander
    sicl-standard-environment-macros:deftype-expander
    sicl-standard-environment-macros:define-compiler-macro-expander
    sicl-data-and-control-flow:shiftf-expander
    sicl-data-and-control-flow:defun-expander
    sicl-data-and-control-flow:psetf-expander
    sicl-data-and-control-flow:rotatef-expander
    sicl-data-and-control-flow:destructuring-bind-expander
    sicl-evaluation-and-compilation:declaim-expander
    sicl-cons:pushnew-expander
    sicl-cons:push-expander
    sicl-cons:pop-expander
    sicl-cons:remf-expander
    sicl-iteration:dolist-expander
    sicl-iteration:dotimes-expander
    sicl-iteration:do-dostar-expander
    sicl-loop:expand-body))

(defparameter *imported-variables*
  '(*package* *macroexpand-hook*))

(defparameter *imported-packages*
  '(#:common-lisp
    #:sicl-evaluation-and-compilation
    #:sicl-global-environment
    #:sicl-data-and-control-flow
    #:sicl-conditionals
    #:sicl-arithmetic
    #:sicl-loop
    #:sicl-cons
    #:sicl-iteration
    #:sicl-standard-environment-macros
    #:sicl-standard-environment-functions))

(defun import-function-from-host (function-name to-environment)
  (setf (sicl-genv:fdefinition function-name to-environment)
        (fdefinition function-name)))

(defun import-package-from-host (package-name to-environment)
  (push (find-package package-name)
        (sicl-genv:packages to-environment)))

(defun import-from-host (environment)
  (host-load "Data-and-control-flow/defun-support.lisp")
  (host-load "Data-and-control-flow/shiftf-support.lisp")
  (host-load "Data-and-control-flow/psetf-support.lisp")
  (host-load "Data-and-control-flow/rotatef-support.lisp")
  (host-load "Data-and-control-flow/destructuring-bind-support.lisp")
  (host-load "Conditionals/support.lisp")
  (host-load "Environment/macro-support.lisp")
  (loop for name in *imported-functions*
        do (import-function-from-host name environment))
  ;; We can't import (SETF CAR) and (SETF CDR) etc directly, because
  ;; they might not be defined as functions.  They could be SETF
  ;; expanders.
  (setf (sicl-genv:fdefinition '(setf car) environment)
        (lambda (new-value cons)
          (setf (car cons) new-value)))
  (setf (sicl-genv:fdefinition '(setf cdr) environment)
        (lambda (new-value cons)
          (setf (cdr cons) new-value)))
  (setf (sicl-genv:fdefinition '(setf cadr) environment)
        (lambda (new-value cons)
          (setf (cadr cons) new-value)))
  (setf (sicl-genv:fdefinition '(setf cddr) environment)
        (lambda (new-value cons)
          (setf (cddr cons) new-value)))
  (loop for symbol in *imported-variables*
        for boundp = (boundp symbol)
        do (setf (sicl-genv:special-variable symbol environment boundp)
                 (if boundp (cl:symbol-value symbol) nil)))
  ;; Import all standard special operators
  (do-symbols (symbol (find-package '#:common-lisp) nil)
    (when (special-operator-p symbol)
      (setf (sicl-genv:special-operator symbol environment) t)))
  ;; Import some packages
  (loop for name in *imported-packages*
        do (import-package-from-host name environment)))
