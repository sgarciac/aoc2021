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
                           (format nil "~A~A"  (char pair 0) middle)))
                finally (return rules))))))

(defun apply-rules (rs s)
  (with-output-to-string (stream)
    (loop
      for i from 0 below (1- (length s))
      for rulesub = (gethash (subseq s i (+ 2 i)) rs)
      do (princ (or rulesub (char s i)) stream)
      finally (princ (char s (1- (length s))) stream))))

(defun apply-rules-n (rs s n)
  (loop for i from 1 to n
        for result = (apply-rules rs s) then (apply-rules rs result)
        do (print result)
        finally (return result)))

(defun score (s)
  (loop
    with counts = (make-hash-table)
    for c across s
    do (if (gethash c counts)
           (incf (gethash c counts))
           (setf (gethash c counts) 1))
    finally (return (loop for k being the hash-keys in counts using (hash-value v)
                          maximizing v into max
                          minimizing v into min
                          finally (return (- max min))))))

;;part 1
(score  (let ((input (read-input)))
          (apply-rules-n (input-rules input) (input-string input) 6)))

;;part2
(score  (let ((input (read-input)))
          (apply-rules-n (input-rules input) (input-string input) 40)))
