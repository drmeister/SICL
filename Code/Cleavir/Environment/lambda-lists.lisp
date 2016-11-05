(cl:in-package #:cleavir-environment)

;;; Returns a list of lambda list keywords that are the same as
;;; &rest as far as Cleavir is concerned.
;;; They can be included and excluded from lambda-list types with
;;; ALLOWED-LAMBDA-LIST-KEYWORDS.
(defgeneric &rest-equivalents (environment)
  (:method-combination append))

(defmethod &rest-equivalents (environment)
  '(&rest &body))

;;; Returns a list of allowed lambda-list keywords for the given
;;; lambda list type. Currently, adding anything that is not also
;;; defined with &rest-equivalents has undefined consequences.
(defgeneric allowed-lambda-list-keywords
    (environment lambda-list-type)
  (:method-combination append))

(defmethod allowed-lambda-list-keywords append
    (environment (type (eql 'ordinary)))
  '(&optional &rest &key &allow-other-keys &aux))

(defmethod allowed-lambda-list-keywords append
    (environment (type (eql 'generic-function)))
  '(&optional &rest &key &allow-other-keys))

(defmethod allowed-lambda-list-keywords append
    (environment (type (eql 'specialized)))
  '(&optional &rest &key &allow-other-keys &aux))

(defmethod allowed-lambda-list-keywords append
    (environment (type (eql 'macro-dotted)))
  '(&whole &environment &optional))

(defmethod allowed-lambda-list-keywords append
    (environment (type (eql 'macro-normal)))
  '(&whole &environment &optional &rest &body
    &key &allow-other-keys &aux))

(defmethod allowed-lambda-list-keywords append
    (environment (type (eql 'destructuring-dotted)))
  '(&whole &optional))

(defmethod allowed-lambda-list-keywords append
    (environment (type (eql 'destructuring-normal)))
  '(&whole &optional &rest &body
    &key &allow-other-keys &aux))

(defmethod allowed-lambda-list-keywords append
    (environment (type (eql 'deftype-dotted)))
  '(&whole &environment &optional))

(defmethod allowed-lambda-list-keywords append
    (environment (type (eql 'deftype-normal)))
  '(&whole &environment &optional &rest &body
    &key &allow-other-keys &aux))

(defmethod allowed-lambda-list-keywords append
    (environment (type (eql 'defsetf)))
  '(&optional &rest &key &allow-other-keys &environment))

(defmethod allowed-lambda-list-keywords append
    (environment (type (eql 'define-modify-macro)))
  '(&optional &rest))

(defmethod allowed-lambda-list-keywords append
    (environment (type (eql 'define-method-combination-arguments)))
  '(&whole &optional &rest &key &allow-other-keys &aux))
