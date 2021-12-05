;;;; day5.lisp

(in-package #:day5)

(defparameter *input-file* "input")

(defstruct path p1 p2)

(defun make-point (x y) (cons x y))
(defun x (point) (car point))
(defun y (point) (cdr point))

(defun parse-path (line)
  (let ((numbers (cl-ppcre:split "( -> )|," line)))
    (make-path :p1 (make-point (parse-integer (first numbers)) (parse-integer (second numbers)))
               :p2 (make-point (parse-integer (third numbers)) (parse-integer (fourth numbers))))))

(defun horizontal-p (path)
  (= (x (path-p1 path))
     (x (path-p2 path))))

(defun vertical-p (path)
  (= (y (path-p1 path))
     (y (path-p2 path))))

(defun diagonal-p (path)
  (not (or (horizontal-p path)
           (vertical-p path))))

(defun each-point (path predicate)
  "call predicate on all points of path."
  (labels ((path-step (from to) (cond ((= from to) 0) ((< from to) 1) (t -1))))
    (let ((step-x (path-step (x (path-p1 path)) (x (path-p2 path))))
          (step-y (path-step (y (path-p1 path)) (y (path-p2 path)))))
      (loop
        for x = (x (path-p1 path)) then (+ x step-x)
        and y = (y (path-p1 path)) then (+ y step-y)

        until (and (= y (y (path-p2 path)))
                   (= x (x (path-p2 path))))

        do (funcall predicate (make-point x y))

        finally (funcall predicate (make-point x y)) ;; one last call.
        ))))

(defun read-input ()
  (with-open-file (stream *input-file*)
    (loop for line = (read-line stream nil)
          while line collect (parse-path line))))

;; sparse 2-d matrix
(defun make-coords ()
  (make-hash-table :test 'equal))

(defun get-point-count (coords point)
  (gethash point coords))

(defun incf-point-count (coords point)
  "Increases a position value by one."
  (if (gethash point coords)
      (incf (gethash point coords))
      (setf (gethash point coords) 1)))

;; misc
(defun remove-diagonals (paths)
  (remove-if #'diagonal-p paths))

(defun count-points-with-intersections (paths)
  (loop
    with coords = (make-coords)
    for path in paths
    do (each-point path (lambda (point) (incf-point-count coords point)))
    finally (return
              (loop for k being the hash-keys in coords using (hash-value v) counting (> v 1)))))

;; Part 1
(count-points-with-intersections (remove-diagonals (read-input)))

;; Part 2
(count-points-with-intersections (read-input))
