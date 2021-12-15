(defparameter *input-file* "input")

(defparameter *size* 100)


(defvar *risk*)
(defvar *path-risk*) ; cell value nil if not determined
(defvar *path*) ; cell value nil if not determined, 0 if origin
(defvar *frontier*)

(defun read-input ()
  (with-open-file (stream *input-file*)
    (loop
      with matrix = (make-array (list *size* *size*))
      for line = (read-line stream nil)
      for row from 0 below *size*
      do (loop for col from 0 below *size* do (setf (aref matrix row col) (- (char-code (char line col)) 48)))
      finally (return matrix))))

;; points are cons (row . col)
(defun pref (array p) (aref array (car p) (cdr p)))

(defun set-pref (array p v) (setf (aref array (car p) (cdr p)) v))

(defun neighboors (p)
  (remove nil
          (list
           (when (> (car p) 0) (cons (1- (car p)) (cdr p)))
           (when (< (car p) (1- *size*)) (cons (1+ (car p)) (cdr p)))
           (when (> (cdr p) 0) (cons (car p) (1- (cdr p))))
           (when (< (cdr p) (1- *size*)) (cons (car p) (1+ (cdr p)))))))


(defun init ()
  (setf *risk* (read-input))
  (setf *frontier* '((0 . 0)))
  (setf *path-risk* (make-array (list *size* *size*) :initial-element nil))
  (setf *path* (make-array (list *size* *size*) :initial-element nil))
  (set-pref *path-risk* (cons 0 0) 0))

(defun explored-p (p)
  (pref *path-risk* p))

(defun minimum-in-frontier ()
  (loop
    for p in *frontier*
    for min-point = p then (if (< (pref *path-risk* p) (pref *path-risk* min-point)) p min-point)
    finally (return min-point)))

(defun unexplored-neighboors (p)
  (mapcan (lambda (n) (when (not (explored-p n)) (list n))) (neighboors p)))

(defun recalculate-frontier ()
  (loop for p in *frontier* when (zerop (length (unexplored-neighboors p))) collect p into toremove
        finally (setf *frontier* (nset-difference *frontier* toremove :test #'equal))))

(defun expand-frontier-at (p)
  (loop for n in (unexplored-neighboors p)
        do (progn
             (set-pref *path-risk* n (+ (pref *path-risk* p) (pref *risk* n)))
             (set-pref *path* n p)
             (push n *frontier*))
        finally (recalculate-frontier)))

(defun expand-frontier ()
  (expand-frontier-at (minimum-in-frontier))
  *frontier*
  )

;; part 1
(init)
(time (loop while (expand-frontier)))
*path-risk*

;; part 2
