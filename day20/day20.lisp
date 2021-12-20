(defparameter *input-file* "input")

(defvar *algo*)
(defvar *image*)
(defvar *rows*)
(defvar *cols*)

(defun init ()
  (with-open-file (s *input-file*)
    (setf *algo* (read-line s))
    (read-line s)
    (let* ((lines (loop for line = (read-line s nil) while line collect line)))
      (setf *rows* (length lines))
      (setf *cols* (length (first lines)))
      (setf *image* (loop
                      with image = (make-array (list *rows* *cols*))
                      for line in lines
                      for row from 0 below *rows*
                      do (loop for char across line
                               for col from 0 below *cols*
                               do (setf (aref image row col) char))
                      finally (return image))))))

(defun char-to-bit (row col)
  (if (char= (aref *image* row col) #\.) 0 1))

(defun output (row col default)
  (aref *algo*
        (read-from-string
         (format nil "#b~A~A~A~A~A~A~A~A~A"
                 (if (and (> row 0) (> col 0))
                     (char-to-bit (1- row) (1- col))
                     default)
                 (if (and (> row 0) (>= col 0) (< col *cols*))
                     (char-to-bit  (1- row) col)
                     default)
                 (if (and (> row 0) (< col (1- *cols*)))
                     (char-to-bit (1- row) (1+ col))
                     default)
                 (if (and (> col 0) (>= row 0) (< row *rows*))
                     (char-to-bit row (1- col))
                     default)
                 (if (and (>= row 0) (>= col 0) (< col *cols*) (< row *rows*))
                     (char-to-bit row col)
                     default)
                 (if (and (>= row 0) (< row *rows*) (< col (1- *cols*)))
                     (char-to-bit row (1+ col))
                     default)
                 (if (and (< row (1- *rows*)) (> col 0))
                     (char-to-bit  (1+ row) (1- col))
                     default)
                 (if (and (< row (1- *rows*)) (>= col 0) (< col *cols*))
                     (char-to-bit (1+ row) col)
                     default)
                 (if (and (< row (1- *rows*)) (< col (1- *cols*)))
                     (char-to-bit  (1+ row) (1+ col))
                     default)))))

(defun step-image (default)
  (let* ((rows (+ *rows* 2))
         (cols (+ *cols* 2))
         (new-image (make-array (list rows cols))))
    (loop for row from 0 below rows
          do (loop  for col from 0 below cols
                    do (setf (aref new-image row col) (output (1- row) (1- col) default))))
    (setf *image* new-image)
    (setf *rows* rows)
    (setf *cols* cols)))

(defun print-image ()
  (terpri)
  (loop for row from 0 below *rows*
        do (progn  (loop for col from 0 below *cols*
                         do (princ (aref *image* row col)))
                   (terpri)
                   )))

(defun count-lights ()
  (loop for row from 0 below *rows*
        summing (loop for col from 0 below *cols* counting
                                                  (char=
                                                   (aref *image* row col)
                                                   #\#
                                                   ))))

(defun step-n (n)
  (loop for i from 0 below n
        do (step-image (mod i 2))))

;; part 1
(init)
(step-n 2)
(count-lights)

;; part 2
(init)
(step-n 50)
(count-lights)
