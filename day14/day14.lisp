(ql:quickload :cl-ppcre)

(defparameter *input-file* "input")

(defstruct input string rules)

(defun read-input ()
  (with-open-file (stream *input-file*)
    (make-input
     :string (read-line stream)
     :rules (progn
              (read-line stream)
              (loop
                with rules = (make-hash-table :test #'equal)
                for line = (read-line stream nil) while line
                do (let* ((parts (cl-ppcre:split " -> " line))
                          (pair (first parts))
                          (middle (second parts))
                          )
                     (setf (gethash pair rules)
                           (cons (format nil "~A~A"  (char pair 0) middle)
                                 (format nil "~A~A" middle (char pair 1)))))
                finally (return rules))))))

(defun template-to-pairs-count (template rules)
  (let ((pc (make-hash-table :test #'equal)))
    ;; init to 0 all counts
    (loop for rule being the hash-keys in rules
          do (setf (gethash rule pc) 0))

    ;; count in template
    (loop
      for i from 0 below (1- (length template))
      for current-pair = (format nil "~A~A" (aref template i) (aref template (1+ i)))
      do (incf (gethash current-pair pc))
      finally (return pc))))

(defun next-pairs-count (pc rules)
  (let ((npc (make-hash-table :test #'equal)))
    (loop for rule being the hash-keys in rules
          do (setf (gethash rule npc) 0))
    (loop
      for k being the hash-keys in pc using (hash-value v)
      do (progn
           (incf (gethash (car (gethash k rules)) npc) (gethash k pc))
           (incf (gethash (cdr (gethash k rules)) npc) (gethash k pc))))
    npc))

(defun score (pc template)
  (loop with counts = (make-hash-table :test #'equal)
        for pair being the hash-keys in pc using (hash-value v)
        do (if (gethash (char pair 0) counts)
               (incf (gethash (char pair 0) counts)  v)
               (setf  (gethash (char pair 0) counts) v))
        finally (progn
                  (incf (gethash (aref template (1- (length template))) counts))
                  (return counts))))


(defun solve (count)
  (let* ((input (read-input))
         (rules (input-rules input))
         (template (input-string input))
         (pc (template-to-pairs-count template rules)))
    (dotimes (i count) (setf pc (next-pairs-count pc rules)))
    (loop for entry being the hash-keys in (score pc template) using (hash-value v)
          maximizing v into max
          minimizing v into min
          finally (return (- max min)))))

;; part1
(solve 10)

;; part2
(solve 40)
