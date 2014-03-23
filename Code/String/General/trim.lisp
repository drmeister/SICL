(cl:in-package #:sicl-string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities.

(defun character-in-list-bag-p (character bag)
  (loop for c in bag
	when (eql character c)
	  return t))

(declaim (inline character-in-list-bag-p))

(defun character-in-simple-string-bag-p (character bag)
  (loop for i from 0 below (length bag)
	when (eql character (schar bag i))
	  return t))

(declaim (inline character-in-simple-string-bag-p))

(defun character-in-general-string-bag-p (character bag)
  (loop for i from 0 below (length bag)
	when (eql character (char bag i))
	  return t))

(declaim (inline character-in-general-string-bag-p))


(defun string-left-trim-list-simple-string
    (character-bag string-designator)
  

(defun string-trim (character-bag string-designator)
  (flet ((in-bag-p (char) (find char character-bag)))
    (let* ((string (string string-designator))
	   (first (position-if-not #'in-bag-p string)))
      (if (null first)
	  string
	  (let ((last (position-if-not #'in-bag-p string :from-end t)))
	    (subseq string first (1+ last)))))))

(defun string-left-trim (character-bag string-designator)
  (flet ((in-bag-p (char) (find char character-bag)))
    (let* ((string (string string-designator))
	   (first (position-if-not #'in-bag-p string)))
      (if (null first)
	  string
	  (subseq string first)))))

(defun string-right-trim (character-bag string-designator)
  (flet ((in-bag-p (char) (find char character-bag)))
    (let* ((string (string string-designator))
	   (last (position-if-not #'in-bag-p string :from-end t)))
      (if (null last)
	  string
	  (subseq string 0 (1+ last))))))
