;;;; day3.lisp

(in-package #:day3)

(defparameter *input-file* "input")
(defparameter *line-size* 12)

;;(defparameter *input-file* "example")
;;(defparameter *line-size* 5)

(defun read-input (file-name)
  (with-open-file (stream *input-file*)
    (loop for line = (read-line stream nil)
          while line collect line)))

(defun gamma (lines)
  (let* ((one-counts (make-array *line-size* :initial-element 0))
         (input lines)
         (half-lines-count (/ (length input) 2)))
    (loop for line in input
          do (loop for i from 0 upto (1- *line-size*)
                   do (when (char= #\1 (aref line i))
                        (incf (aref one-counts i)))))
    (map 'string (lambda (one-count) (if (>= one-count half-lines-count) #\1 #\0)) one-counts)))

;; first part
(let ((lines (read-input *input-file*)))
  (let* ((gamma (parse-integer (gamma lines)  :radix 2) )
         (epsilon (1- (- (expt 2 *line-size*) gamma))))
    (* gamma epsilon)))

;; second part
(defun filter-round (lines position &optional (keep-most-common t))
  (let ((gamma (gamma lines)))
    (funcall (if keep-most-common #'remove-if-not #'remove-if)
             (lambda (line) (char= (aref line position) (aref gamma position)))
             lines)))

(defun part-2 (initial-lines oxygen)
  (loop
    with keep-most-common = oxygen
    for position from 0 upto (1- *line-size*)
    for lines = (filter-round initial-lines position keep-most-common) then (filter-round lines position keep-most-common)
    while (> (length lines) 1)
    finally (return (parse-integer (first lines) :radix 2) )))

(let ((lines (read-input *input-file*)))
  (* (part-2 lines t) (part-2 lines nil)))
