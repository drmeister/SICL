(cl:in-package #:sicl-new-boot-phase-0)

;;; Environment E0.
;;;
;;; There are no SICL generic functions in this environment.
;;;
;;; The class T in this environment is the same as the host class T.
;;;
;;; The SICL classes STANDARD-CLASS, FUNCALLABLE-STANDARD-CLASS, and
;;; BUILT-IN-CLASS are identical.  They are subclasses of the host
;;; class FUNCALLABLE-STANDARD-CLASS.
;;;
;;; The SICL classes STANDARD-GENERIC-FUNCTION, STANDARD-METHOD, and
;;; STANDARD-DIRECT-SLOT-DEFINITION are the same as the their
;;; respective host classes.
;;;
;;; There are no other classes in this environment.

(defclass environment (sicl-new-boot:environment)
  ())
