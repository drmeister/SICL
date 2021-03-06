(cl:in-package #:sicl-new-boot)

(defclass environment (sicl-minimal-extrinsic-environment:environment)
  ())

(defmethod initialize-instance :after ((object environment) &key)
  (import-functions-from-host
   '(format
     cleavir-code-utilities:proper-list-p)
   object)
  (setf (sicl-genv:special-variable '*trace-output* object t) *trace-output*)
  (import-package-from-host 'sicl-clos object))

(defclass boot ()
  ((%e0 :initarg :e0 :accessor e0)
   (%e1 :initarg :e1 :accessor e1)
   (%e2 :initarg :e2 :accessor e2)
   (%e3 :initarg :e3 :accessor e3)
   (%e4 :initarg :e4 :accessor e4)))

(defun boot ()
  (let ((boot
          (let ((sicl-minimal-extrinsic-environment::*cache-p* t))
            (make-instance 'boot
              :e0 (make-instance 'environment)
              :e1 (make-instance 'environment)
              :e2 (make-instance 'environment)
              :e3 (make-instance 'environment)
              :e4 (make-instance 'environment)))))
    (sicl-new-boot-phase-0:boot-phase-0 boot)
    (sicl-new-boot-phase-1:boot-phase-1 boot)
    (sicl-new-boot-phase-2:boot-phase-2 boot)
    (sicl-new-boot-phase-3:boot-phase-3 boot)
    boot))
