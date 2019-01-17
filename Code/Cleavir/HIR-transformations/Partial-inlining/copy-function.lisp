(cl:in-package #:cleavir-partial-inlining)

(defvar *new-enter*)

(defun ensure-copy (mapping datum)
  (or (find-in-mapping mapping datum)
      (let ((new (typecase datum
                   (cleavir-ir:values-location
                    (cleavir-ir:make-values-location))
                   (cleavir-ir:lexical-location
                    (cleavir-ir:make-lexical-location
                     (cleavir-ir:name datum)))
                   (t datum))))
        (add-to-mapping mapping datum new)
        (setf (gethash new *location-ownerships*) *new-enter*)
        new)))

(defun translate-input-for-copy (input external-map internal-map stack)
  (let ((owner (gethash input *location-ownerships*)))
    (cond ((member owner stack)
           ;; Originally I expected that the input would always be in the map,
           ;; but this does not seem to be the case, for reasons I'm not sure of.
           ;; I think the ownership test should make it be correct anyway though.
           ;; FIXME: Maybe remove input-output distinction here then.
           (ensure-copy internal-map input))
          ((eq owner *original-enter-instruction*)
           (ensure-copy external-map input))
          (t input))))
(defun translate-inputs-for-copy (inputs external-map internal-map stack)
  (loop for input in inputs
        collect (translate-input-for-copy input external-map internal-map stack)))

(defun translate-output-for-copy (output external-map internal-map stack)
  (let ((owner (gethash output *location-ownerships*)))
    (cond ((member owner stack)
           (ensure-copy internal-map output))
          ((eq owner *original-enter-instruction*)
           (ensure-copy external-map output))
          (t output))))
(defun translate-outputs-for-copy (outputs external-map internal-map stack)
  (loop for output in outputs
        collect (translate-output-for-copy output external-map internal-map stack)))

(defgeneric patch-instruction (instruction copier)
  (:method (instruction copier)
    (declare (ignore instruction copier))))

(defmethod patch-instruction ((instruction cleavir-ir:enclose-instruction) copier)
  (setf (cleavir-ir:code instruction)
        (funcall copier (cleavir-ir:code instruction))))

(defmethod patch-instruction ((instruction cleavir-ir:unwind-instruction) copier)
  (setf (cleavir-ir:destination instruction)
        (funcall copier (cleavir-ir:destination instruction))))

;;; EXTERNAL-MAP is the mapping from inline.
;;; INTERNAL-MAP is a fresh mapping which is used only for internal locations.
;;; STACK is a list of ENTER-INSTRUCTIONS that we're in the middle of copying.
;;; If an output is owned by something in the stack, it ought to be copied,
;;; but if not, it's something closed over and must not be copied, unless it's
;;; owned by the function being inlined.
(defun copy-function-recur (enter external-map internal-map stack)
  (let ((copies nil)
        (stack (cons enter stack))
        (*new-enter* (cleavir-ir:clone-instruction enter)))
    (push *new-enter* copies)
    ;; We set the outputs after building like this so that (a) they have their ownership correct,
    ;; and (b) (setf cleavir-ir:outputs) synchronizes the lambda list properly.
    ;; We still copy the outputs in clone-instruction above so that the (setf outputs) method
    ;; knows how to do the substitution.
    (setf (cleavir-ir:outputs *new-enter*)
          (translate-outputs-for-copy (cleavir-ir:outputs enter)
                                      external-map internal-map stack))
    ;; First loop: Copy all instructions in the function, but leave
    ;; predecessors and successors disconnected.
    (cleavir-ir:map-local-instructions
     (lambda (instruction)
       (unless (typep instruction 'cleavir-ir:enter-instruction) ; FIXME: Ugly.
         (let* ((inputs (cleavir-ir:inputs instruction))
                (outputs (cleavir-ir:outputs instruction))
                (new-inputs (translate-inputs-for-copy inputs external-map internal-map stack))
                (new-outputs (translate-outputs-for-copy outputs external-map internal-map stack))
                (copy (cleavir-ir:clone-instruction instruction
                  :inputs new-inputs :outputs new-outputs)))
           (push copy copies)
           (add-to-mapping *instruction-mapping* instruction copy))))
     enter)
    ;; Second loop: Loop over the copies doing hookups.
    (labels ((maybe-replace (instruction)
               (let ((copy (find-in-mapping *instruction-mapping* instruction)))
                 (or copy instruction)))
             (copier (instruction)
               (if (typep instruction 'cleavir-ir:enter-instruction)
                   (copy-function-recur instruction external-map internal-map stack)
                   (maybe-replace instruction))))
      (loop for copy in copies
            do (setf (cleavir-ir:predecessors copy)
                     (mapcar #'maybe-replace (cleavir-ir:predecessors copy))
                     (cleavir-ir:successors copy)
                     (mapcar #'maybe-replace (cleavir-ir:successors copy)))
               (setf (gethash copy *instruction-ownerships*) *new-enter*)
            do (patch-instruction copy #'copier)))
    *new-enter*))

;;; Returns a copy of a HIR function.
;;; This is required because an internal function could have things it's mapped
;;; over refer to inputs that were copied by inlining.
;;; ENTER is an enter instruction representing the function being copied.
;;; MAPPING is the lexical variable mapping used during this inline.
;;; The copy of ENTER is returned.
(defun copy-function (enter mapping)
  (copy-function-recur enter mapping (make-hash-table :test #'eq) nil))
