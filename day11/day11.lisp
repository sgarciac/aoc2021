(defparameter *input-file* "input")

(defparameter *size* 10)

(defvar *matrix* nil)

(defun get-up (r c) (when (> r 0) (aref *matrix* (1- r) c)))
(defun get-down (r c) (when (< r (1- *size*)) (aref *matrix* (1+ r) c)))
(defun get-left (r c) (when (> c 0) (aref *matrix* r (1- c))))
(defun get-right (r c) (when (< c (1- *size*)) (aref *matrix* r (1+ c))))

(defun get-up-left (r c) (when (and (> r 0) (> c 0)) (aref *matrix* (1- r) (1- c))))
(defun get-down-left (r c) (when (and (< r (1- *size*)) (> c 0)) (aref *matrix* (1+ r) (1- c))))
(defun get-up-right (r c) (when (and (> r 0) (< c (1- *size*))) (aref *matrix* (1- r) (1+ c))))
(defun get-down-right (r c) (when (and (< r (1- *size*)) (< c (1- *size*)))  (aref *matrix* (1+ r) (1+ c))))

(defun flashing-neighboors (r c)
  (loop for f in (list #'get-up #'get-down #'get-left #'get-right #'get-up-left #'get-up-right #'get-down-left #'get-down-right)
        counting (and (funcall f r c) (= 10 (funcall f r c)))))


(defun read-input ()
  (with-open-file (stream *input-file*)
    (loop with input = (make-array `(,*size* ,*size*))
          for line = (read-line stream nil) while line
          for row from 0 below *size*
          do (loop for char across line
                   for col from 0  below *size*
                   do (setf (aref input row col)
                            (- (char-code char)
                               48)))
          finally (return input))))

(defun init () (setf *matrix* (read-input)))

(defun inc-1 ()
  (loop for row from 0 below *size*
        do (loop for col from 0 below *size*
                 do (incf (aref *matrix* row col)))
        finally (return *matrix*)))

(defun matrix= (m1 m2)
  (loop for row from 0 below *size*
        always (
                loop for col from 0 below *size*
                always (= (aref m1 row col) (aref m2 row col)))))

(defun reset-flashed ()
  (loop for row from 0 below *size*
        do (loop for col from 0 below *size*
                 do (when (= (aref *matrix* row col) 11)
                      (setf (aref *matrix* row col) 0)))))

(defun count-flashes ()
  (loop for row from 0 below *size*
        summing (
                 loop for col from 0 below *size*
                 counting (= 11 (aref *matrix* row col)))))

(defun flash ()
  (loop
    with new-matrix = (make-array `(,*size* ,*size*))
    for row from 0 below *size*
    do (loop for col from 0 below *size*
             do (let* ((old-val (aref *matrix* row col))
                       (new-val (+ (aref *matrix* row col) (flashing-neighboors  row col))))
                  (setf (aref new-matrix row col)
                        (cond
                          ((and (< old-val 10) (>= new-val 10)) 10)
                          ((>= old-val 10) 11)
                          (t  new-val)))))
    finally (progn
              (let ((same (matrix= *matrix* new-matrix)))
                (setf *matrix* new-matrix)
                (return same)))))

(defun dostep ()
  (inc-1)
  (loop until (flash))
  (let ((count (count-flashes)))
    (reset-flashed)
    count
    ))


(defun dosteps (n)
  (loop for i from 1 to n
        summing (dostep)))

;; part 1
(init)
(dosteps 100)

;; part 2
(init)
(loop
  for i = 1 then (1+ i)
  until (= (dostep) 100)
  finally (return i))
