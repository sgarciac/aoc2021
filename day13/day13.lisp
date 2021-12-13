(defun parse-coord (str) (let ((pos (position #\, str)))
                           (cons
                            (parse-integer (subseq str (1+ pos)))
                            (parse-integer (subseq str 0 pos))
                            )))

(defun parse-folding (str)
  (intern (string-upcase (subseq str 11 12)) :KEYWORD))

(defparameter *input-file* "input")

(defvar *folding-list*)
(defvar *paper*)
(defvar *rows*)
(defvar *cols*)

(defun init ()
  (with-open-file (stream *input-file*)
    (let ((dots-list (loop
                       for line = (read-line stream nil) while (not (zerop (length line)))
                       collecting (parse-coord line))))
      (setf *folding-list* (loop for line = (read-line stream nil) while line
                                 collecting (parse-folding line)))
      (let ((dimensions (loop for dot in dots-list
                              maximizing (car dot) into max-row
                              maximizing (cdr dot) into max-col
                              finally (return (cons (1+ max-row) (1+ max-col))))))
        (setf *rows* (car dimensions))
        (setf *cols* (cdr dimensions))
        (setf *paper* (make-array (list *rows* *cols*) :initial-element :O))
        (loop for dot in dots-list
              do (setf (aref *paper* (car dot) (cdr dot)) :X))
        ))))

(defun fold-vertical ()
  (let* ((new-rows (/ (1- *rows*) 2))
         (new-paper (make-array (list new-rows *cols*))))
    (loop for row from 0 below new-rows
          do (loop for col from 0 below *cols*
                   do (setf (aref new-paper row col)
                            (if (or (eq (aref *paper* row col) :X)
                                    (eq (aref *paper* (1- (- *rows* row))  col) :X))
                                :X
                                :O
                                ))))
    (setf *rows* new-rows)
    (setf *paper* new-paper)))

(defun fold-horizontal ()
  (let* ((new-cols (/ (1- *cols*) 2))
         (new-paper (make-array (list *rows* new-cols))))
    (loop for row from 0 below *rows*
          do (loop for col from 0 below new-cols
                   do (setf (aref new-paper row col)
                            (if (or (eq (aref *paper* row col) :X)
                                    (eq (aref *paper* row  (1- (- *cols* col))) :X))
                                :X
                                :O
                                ))))
    (setf *cols* new-cols)
    (setf *paper* new-paper)))

;; part 1
(init)
(fold-horizontal)

(loop for row from 0 below *rows*
      summing (loop for col from 0 below *cols*
                    counting (eq (aref *paper* row col) :X)))

;; part 2
(init)

(loop for folding in *folding-list*
      do (if (eq folding :X)
             (fold-horizontal)
             (fold-vertical)))

(loop for row from 0 below *rows*
      do (progn
           (print "")
           (loop for col from 0 below *cols*
                 do (if (eq (aref *paper* row col) :X)
                        (prin1 "H")
                        (prin1 " ")
                        ))))
