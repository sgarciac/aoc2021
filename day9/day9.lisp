(defparameter *input-file* "example")

(defparameter *rows* 5)
(defparameter *cols* 10)

;; variables

;; a matrix containing the heights
(defvar *heights* nil)

;; points are cons (row . col)
(defun get-height (p)
  (and p (aref *heights* (car p) (cdr p))))

(defun set-height (p h)
  (setf (aref *heights* (car p) (cdr p)) h))

(defun get-up (p) (when (> (car p) 0) (cons (1- (car p)) (cdr p))))
(defun get-down (p) (when (< (car p) (- *rows* 1)) (cons (1+ (car p)) (cdr p))))
(defun get-left (p) (when (> (cdr p) 0) (cons (car p) (1- (cdr p)) )))
(defun get-right (p) (when (< (cdr p) (- *cols* 1)) (cons (car p) (1+ (cdr p)))))

(defun get-height-up (p) (get-height (get-up p)))
(defun get-height-down (p) (get-height (get-down p)))
(defun get-height-left (p) (get-height (get-left p)))
(defun get-height-right (p) (get-height (get-right p)))

(defun init ()
  (setf *heights* (make-array `(,*rows* *cols*)))
  (with-open-file (stream *input-file*)
    (loop for line = (read-line stream nil) while line
          for row from 0 below *rows*
          do (loop for col from 0 below *cols*
                   do
                      (set-height (cons row col)
                                  (- (char-code (aref line col))
                                     48))))))

;; part 1
(defun low-point (p)
  (let ((h (get-height p))
        (u (get-height-up p))
        (d (get-height-down p))
        (l (get-height-left p))
        (r (get-height-right p)))
    (and
     (or (not u) (< h u))
     (or (not d) (< h d))
     (or (not l) (< h l))
     (or (not r) (< h r)))))

(init)

(loop for row from 0 below *rows*
      summing (loop for col from 0 below *cols* when (low-point (cons row col)) summing (1+ (get-height (cons row col))) ))

;; part 2

;; *basins* is a table mapping basins (a symbol) to its list of locations
(defvar *basins* nil)

;; *locations-basins* is a table mapping locations to its basin
(defvar *locations-basins* nil)

(defun init-basin (p)
  "init a basin containing a single location"
  (let ((b (gensym)))
    (setf (gethash b *basins*) (list p))
    (setf (gethash p *locations-basins*) b)))

(defun push-to-basin (b p)
  (push p (gethash b *basins*))
  (setf (gethash p *locations-basins*) b))

(defun init2 ()
  (setf *basins* (make-hash-table))
  (setf *locations-basins* (make-hash-table :test #'equal))
  (init))

(loop for row from 0 below *rows*
      do (loop for col from 0 below *cols*
               do (let* ((p (cons row col))
                         (h (get-height p))
                         (u (get-height-up p))
                         (l (get-height-left p)))
                    (when (not (= h 9))
                      (cond
                        ;; init a new basin
                        ((and
                          (or (= u 9) (not u))
                          (or (= l 9) (not l)))
                         (setf (gethash (gensym) *basins*) (list p))
                         (setf)
                         )

                        ))

                    )
               )
      )
