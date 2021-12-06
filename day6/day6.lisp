(ql:quickload :cl-ppcre)

(defparameter *input-file* "./input")


(defun make-simulation ()
  (make-array 9 :initial-element 0))

(defun advance (simulation)
  "Creates the next state of the simulation and return it."
  (loop
    with next-simulation = (make-simulation)
    for i from 8 downto 0
    do (cond ((zerop i)
              (setf (aref next-simulation 8) (aref simulation 0))
              (setf (aref next-simulation 6) (+ (aref simulation 0)
                                                (aref simulation 7))))
             (t (setf (aref next-simulation (1- i))
                      (aref simulation i))))
    finally (return next-simulation)))

(defun read-simulation (file-name)
  (loop
    with simulation = (make-simulation)
    with numbers = (mapcar #'parse-integer
                           (with-open-file (stream file-name)
                             (cl-ppcre:split "," (read-line stream))))
    for number in numbers do (incf (aref simulation number))
    finally (return simulation)))

;; part 1
(let ((simulation (read-simulation *input-file*)))
  (dotimes (i 80)
    (setf simulation (advance simulation)))
  (loop for val across simulation summing val))

;;
(let ((simulation (read-simulation *input-file*)))
  (dotimes (i 256)
    (setf simulation (advance simulation)))
  (loop for val across simulation summing val))
