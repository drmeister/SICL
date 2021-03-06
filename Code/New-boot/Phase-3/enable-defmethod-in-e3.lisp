(cl:in-package #:sicl-new-boot-phase-3)

;;; The specializers of the generic functions in E3 are the classes of
;;; the instances in E3, so they are the classes in E2.
(defun define-make-specializer (e2 e3)
  (setf (sicl-genv:fdefinition 'sicl-clos::make-specializer e3)
        (lambda (specializer)
          (cond ((symbolp specializer)
                 (sicl-genv:find-class specializer e2))
                (t
                 specializer)))))

;;; In order to make DEFMETHOD work in E3, we need to be able to add
;;; methods to a generic function.  We use the SICL-specific function
;;; ENSURE-METHOD for that, and ENSURE-METHOD calls the Common Lisp
;;; standard function ADD-METHOD.  But the full ADD-METHOD is a bit to
;;; complicated for what we need here.  For one thing, it checks for
;;; existing methods to remove first, and it does some error checking.
;;; We do not need that here, because we are never going to add a
;;; method that requires removing any existing method.  For that
;;; reason, we define a special version of it here.
;;;
;;; Now, we are dealing with generic function metaobjects in E3.  They
;;; are instances of classes in E1, so the accessor methods for those
;;; generic function metaobjects are to be found in E2.  Therefore,
;;; instead of just using PUSH, we have to find and call the
;;; slot-reader and the slot-writer explicitly which makes this code
;;; look a bit strange.
(defun define-add-method-in-e3 (boot)
  (with-accessors ((e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)) boot
    (let* ((name 'sicl-clos:generic-function-methods)
           (slot-reader (sicl-genv:fdefinition name e2))
           (slot-writer (sicl-genv:fdefinition `(setf ,name) e2)))
      (setf (sicl-genv:fdefinition 'add-method e3)
            (lambda (generic-function method)
              (funcall slot-writer
                       (cons method (funcall slot-reader generic-function))
                       generic-function))))))

(defun define-make-method-for-generic-function-in-e3 (boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e3 sicl-new-boot:e3)) boot
    (setf (sicl-genv:fdefinition 'sicl-clos::make-method-for-generic-function e3)
          (lambda (generic-function specializers keys)
            (declare (ignore generic-function))
            (apply #'make-instance
                   (sicl-genv:find-class 'standard-method e1)
                   :specializers specializers
                   keys)))))

(defun define-defmethod-expander (boot)
  (with-accessors ((e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)) boot
    (setf (sicl-genv:fdefinition 'sicl-clos:defmethod-expander e3)
          (lambda (ct-env function-name rest)
            (multiple-value-bind
                  (qualifiers required remaining
                   specializers declarations documentation forms)
                (sicl-clos::parse-defmethod rest)
              (let* ((lambda-list (append required remaining))
                     (generic-function-var (gensym)))
                `(let* ((,generic-function-var
                          (ensure-generic-function ',function-name :environment ,ct-env)))
                   (sicl-clos:ensure-method
                    ,generic-function-var
                    :lambda-list ',lambda-list
                    :qualifiers ',qualifiers
                    :specializers ,(sicl-clos::canonicalize-specializers specializers)
                    :documentation ,documentation
                    :function
                    ,(funcall
                      (sicl-genv:fdefinition 'sicl-clos:make-method-lambda e3)
                      nil
                      nil
                      `(lambda ,lambda-list
                         ,@declarations
                         ,@forms)
                      nil)))))))))

(defun enable-defmethod-in-e3 (boot)
  (with-accessors ((e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)) boot
    (let ((method-function (sicl-genv:fdefinition 'sicl-clos:method-function e2)))
      (setf (sicl-genv:fdefinition 'sicl-clos::make-method-lambda-default e3)
            (lambda (generic-function method lambda-expression environment)
              (declare (ignore generic-function method environment))
              (let ((args (gensym))
                    (next-methods (gensym)))
                (values
                 `(lambda (,args ,next-methods)
                    (flet ((next-method-p ()
                             (not (null ,next-methods)))
                           (call-next-method (&rest args)
                             (when (null ,next-methods)
                               (error "no next method"))
                             (funcall (funcall ,method-function (car ,next-methods))
                                      (or args ,args)
                                      (cdr ,next-methods))))
                      (declare (ignorable #'next-method-p #'call-next-method))
                      (apply ,lambda-expression
                             ,args)))
                 '())))))
    (load-file "CLOS/make-method-lambda-defuns.lisp" e3)
    (define-make-specializer e2 e3)
    (define-add-method-in-e3 boot)
    (define-make-method-for-generic-function-in-e3 boot)
    (import-functions-from-host '(copy-list) e3)
    (load-file "CLOS/ensure-method.lisp" e3)
    (define-defmethod-expander boot)
    (load-file "CLOS/defmethod-defmacro.lisp" e3)))
