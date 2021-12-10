(defparameter *input-file* "input")

(defun open-char-p (char) (find char '(#\( #\{ #\[ #\<)))

(defun close-for (char) (case char (#\( #\)) (#\{ #\}) (#\[ #\]) (#\< #\>)))

(defun valid-p (chars stack)
  "returns one of: :OK, :INCOMPLETE and the stack, :CORRUPT and the first corrupt char"
  (cond
    ((and (not chars) (not stack)) :OK)
    ((and stack (not chars)) (values :INCOMPLETE stack))
    ((open-char-p (car chars))
     (valid-p (cdr chars) (cons (car chars) stack)))
    (t
     (if (char= (car chars) (close-for (car stack)))
         (valid-p (cdr chars) (cdr stack))
         (values :CORRUPT (car chars))))))

;; part 1
(defun corrupt-value (char) (case char  (#\) 3) (#\} 1197) (#\] 57) (#\> 25137)))

(with-open-file (stream *input-file*)
  (loop for line = (read-line stream nil) while line
        for chars = (loop for char across line collect char)
        summing (multiple-value-bind (result aux)
                    (valid-p chars '())
                  (if (eq result :CORRUPT)
                      (corrupt-value aux)
                      0))))

;; part 2
(defun missing-char-value (char) (case char  (#\( 1) (#\{ 3) (#\[ 2) (#\< 4)))

(defun stack-value (stack)
  (loop
    for char in stack
    for total = (missing-char-value char) then (+ (* 5 total) (missing-char-value char))
    finally (return total)))

(with-open-file (stream *input-file*)
  (loop for line = (read-line stream nil) while line
        for chars = (loop for char across line collect char)
        when (multiple-value-bind (result aux)
                 (valid-p chars '())
               (if (eq result :INCOMPLETE)
                   (stack-value aux)
                   nil)) collect it into scores
        finally (return (let ((sorted (sort scores #'>)))
                          (nth (floor (/ (length sorted) 2)) sorted)))))
