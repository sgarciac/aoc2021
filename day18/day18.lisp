(ql:quickload "cl-ppcre")

(defparameter *input-file* "input")

(defstruct bintree
  value
  left
  right
  parent)

(defun parsed-to-tree (p parent)
  (let ((new-tree (make-bintree
                   :parent parent
                   :value (and (numberp p) p)
                   )))
    (setf (bintree-left new-tree) (and (consp p) (parsed-to-tree (car p) new-tree)))
    (setf (bintree-right new-tree) (and (consp p) (parsed-to-tree (cdr p) new-tree)))
    new-tree))

(defun leafp (bt)
  (numberp (bintree-value bt)))

(defun rootp (bt)
  (null (bintree-parent bt)))

(defun leftp (bt)
  (and (bintree-parent bt) (eq (bintree-left (bintree-parent bt)) bt)))

(defun rightp (bt)
  (and (bintree-parent bt) (eq (bintree-right (bintree-parent bt)) bt)))

(defun parse-line (line)
  (parsed-to-tree (read-from-string (cl-ppcre:regex-replace-all
                                     "\\]"
                                     (cl-ppcre:regex-replace-all
                                      "\\["
                                      (cl-ppcre:regex-replace-all "," line " . ") "(")
                                     ")")) nil))

(defun bintree-to-string (bt)
  (if (leafp bt)
      (format nil "~A" (bintree-value bt))
      (format nil "[~A,~A]" (bintree-to-string (bintree-left bt))(bintree-to-string (bintree-right bt)) )))

(defun rightmost-leaf (bt)
  (if (leafp bt)
      bt
      (rightmost-leaf (bintree-right bt))))

(defun leftmost-leaf (bt)
  (if (leafp bt)
      bt
      (leftmost-leaf (bintree-left bt))))


(defun rightmost-leaf-to-the-left-of (bt)
  (cond ((rootp bt) nil)
        ((rightp bt) (rightmost-leaf (bintree-left (bintree-parent bt))))
        (t (rightmost-leaf-to-the-left-of (bintree-parent bt)))))

(defun leftmost-leaf-to-the-right-of (bt)
  (cond ((rootp bt) nil)
        ((leftp bt) (leftmost-leaf (bintree-right (bintree-parent bt))))
        (t (leftmost-leaf-to-the-right-of (bintree-parent bt)))))

(defun leftmost-tree-at (bt l)
  (cond
    ((leafp bt) nil)
    ((zerop l) bt)
    (t
     (or (leftmost-tree-at (bintree-left bt) (1- l))
         (leftmost-tree-at (bintree-right bt) (1- l))))))

(defun leaf-to-split (bt)
  (cond ((and (leafp bt) (>= (bintree-value bt) 10))
         bt
         )
        ((and (leafp bt) (< (bintree-value bt) 10))
         nil
         )
        (t (or (leftmost-leaf-to-split (bintree-left bt))
               (leftmost-leaf-to-split (bintree-right bt))))))

(defun split (bt)
  (setf (bintree-left bt) (make-bintree :value (floor (/ (bintree-value bt) 2))
                                        :parent bt))
  (setf (bintree-right bt) (make-bintree :value (ceiling (/ (bintree-value bt) 2))
                                         :parent bt))
  (setf (bintree-value bt) nil)
  t)

(defun split-one (bt)
  (let ((bt-to-split (leaf-to-split bt)))
    (and bt-to-split (split bt-to-split))))

(defun explode (bt)
  (when (or (not (leafp (bintree-left bt)))
            (not (leafp (bintree-right bt))))
    (error "can't explode a non two-numbers tree"))
  (let ((rightmost-to-left (rightmost-leaf-to-the-left-of bt))
        (leftmost-to-right (leftmost-leaf-to-the-right-of bt)))
    (when rightmost-to-left (incf (bintree-value rightmost-to-left)
                                  (bintree-value (bintree-left bt))))
    (when leftmost-to-right (incf (bintree-value leftmost-to-right)
                                  (bintree-value (bintree-right bt))))
    (setf (bintree-value bt) 0)
    (setf (bintree-left bt) nil)
    (setf (bintree-right bt) nil)
    t
    ))

(defun explode-one (bt)
  (let ((bt-to-explode (tree-to-explode bt)))
    (and bt-to-explode (explode bt-to-explode))))

(defun tree-to-explode (e)
  (leftmost-tree-at e 4))

(defun add-trees (bt1 bt2)
  (let ((new-root (make-bintree
                   :value nil
                   :left bt1
                   :right bt2
                   :parent nil
                   )))
    (setf (bintree-parent bt1) new-root)
    (setf (bintree-parent bt2) new-root)
    new-root))

(defun add-trees-and-reduce (bt1 bt2)
  (loop
    with bt3 = (add-trees bt1 bt2)
    while (or (explode-one bt3) (split-one bt3))
    finally (return bt3)))


(defun tree-score (bt)
  (if (leafp bt) (bintree-value bt)
      (+ (* 3 (tree-score (bintree-left bt)))
         (* 2 (tree-score (bintree-right bt))))))


;; part 1
(tree-score (with-open-file (stream *input-file*)
              (loop for line = (read-line stream nil) while line
                    for tree = (parse-line line) then (add-trees-and-reduce tree (parse-line line))
                    finally (return tree))))

;; part 2
(defun clone-tree (bt)
  (if (leafp bt)
      (make-tree :)
      )
  )

(with-open-file (stream *input-file*)
  (let ((lines (loop for line = (read-line stream nil) while line collect line)))
    (loop for line1 in lines
          for i = 0 then (1+ i)
          maximizing (loop for line2 in lines
                           for j = 0 then (1+ j)
                           when (not (= i j))
                             maximizing
                             (max (tree-score (add-trees-and-reduce (parse-line line1) (parse-line line2)))
                                  (tree-score (add-trees-and-reduce (parse-line line2) (parse-line line1))))))))
