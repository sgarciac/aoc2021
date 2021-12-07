(ql:quickload :cl-ppcre)

(defparameter *input-file* "./input")

(defun read-crabs (file-name)
  "Read the input as a sorted vector"
  (with-open-file (stream file-name)
    (apply #'vector (sort (mapcar #'parse-integer (cl-ppcre:split "," (read-line stream))) #'<))))

;; Part 1 (calculate all in single pass, by cumulation)
(loop
  with crabs = (read-crabs *input-file*)
  with left = (make-array (length crabs) :initial-element 0)
  with right = (make-array (length crabs) :initial-element 0)
  for i from 1 below (length crabs)
  for j from (- (length crabs) 2) downto 0
  do
     (setf (aref left i)
           (+
            (aref left (1- i))
            (* i (- (aref crabs i) (aref crabs (1- i))))))

     (setf (aref right j)
           (+
            (aref right (1+ j))
            (* i (- (aref crabs (1+ j)) (aref crabs j)))))

  finally (return
            (loop
              with results = (make-array (length crabs))
              for i from 0 below (length crabs)
              do (setf (aref results i) (+ (aref left i) (aref right i)))
              finally (return (aref (sort results #'<) 0)))))


;; Part 2 (brute force)
(loop
  with crabs = (read-crabs *input-file*)
  with last = (aref crabs (1- (length crabs)))
  for i from 0 upto last
  minimizing
  (loop for j from 0 below (length crabs)
        summing (let ((diff (abs (- i (aref crabs j)))))
                  (/ (* diff (1+ diff)) 2))))
