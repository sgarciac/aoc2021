;;;; day4.lisp

(in-package #:day4)

(defconstant +board-width+ 5)
(defconstant +board-height+ 5)
(defconstant +max-number+ 100)

(defparameter *input-file* "input")

;; the list of boards
(defvar *boards* '())

;; the full list of numbers to be drawn
(defvar *input-numbers* '())

;; The current drawn numbers. An array of booleans where *drawn-numbers*[number]
;; is true if the number has been drawn.
(defvar *drawn-numbers* nil)

(defun make-board ()
  (make-array `(,+board-width+ ,+board-height+)))

(defun board-score (board)
  "sum of all non drawn numbers of a board."
  (loop for col from 0 below +board-width+
        summing (loop for row from 0 below +board-height+
                      summing (if (drawn-number-p (board-val board col row))
                                  0
                                  (board-val board col row)))))

(defun board-val (board col row)
  (aref board col row))

(defun set-board-val (board col row val)
  (setf (aref board col row) val))

(defun drawn-number-p (number)
  "whether or not a number has been drawn"
  (aref *drawn-numbers* number))

(defun draw-number (number)
  (setf (aref *drawn-numbers* number) t))

(defun winner-p (board)
  "Return whether a board has a full row or column."
  (or
   ;; check columns
   (loop
     for col from 0 below +board-width+
       thereis (loop for row from 0 below +board-height+
                     always (drawn-number-p (board-val board col row))))
   ;; and rows
   (loop
     for row from 0 below +board-height+
       thereis (loop for col from 0 below +board-width+
                     always (drawn-number-p (board-val board col row))))))

(defun read-board (stream)
  "Read a board and return, or return nil if there are no more boards."
  (loop
    with board = (make-board)
    for col from 0 below +board-width+
    do (loop
         for row from 0 below +board-height+
         do (let ((next (read stream nil)))
              (if next
                  (set-board-val board col row next)
                  (return-from read-board nil))))
    finally (return board)))

(defun init-game ()
  (setf *boards* '()
        *input-numbers* '()
        *drawn-numbers* (make-array +max-number+ :initial-element nil))
  (with-open-file (stream *input-file*)
    (setf *input-numbers* (mapcar #'parse-integer (cl-ppcre:split "," (read-line stream))))
    (loop
      for board = (read-board stream) then (read-board stream)
      while board
      do (progn
           (push board *boards*)))))

;; Part 1
(init-game)

(loop
  for board = (find-if #'winner-p *boards*) until board
  for number in *input-numbers* do (draw-number number)
  finally (return (* number (board-score board))))


;; Part 2
(init-game)

(defun find-all-if (pred sequ &rest keyword-args &key &allow-other-keys)
  (apply #'remove-if (complement pred) sequ keyword-args))

(loop
  with boards = *boards*
  with last-found = nil
  with last-number = nil
  for number in *input-numbers* do
    (progn
      (draw-number number)
      (let ((winner-boards (find-all-if  #'winner-p boards)))
        (when winner-boards
          (setf last-found winner-boards)
          (setf last-number number)
          (setf boards (mapcan (lambda (item) (if (find item winner-boards) '() (list item))) boards))
          )))
  while boards
  finally  (return (* last-number (board-score (first last-found)))))
