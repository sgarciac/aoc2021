(defparameter *input-file* "example")

(defun split (str) (let ((pos (position #\- str))) (cons (subseq str 0 pos) (subseq str (1+ pos)))))

(defun read-input ()
  (with-) (loop with graph = (make-hash-table)

                ))
