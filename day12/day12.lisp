(defparameter *input-file* "input")

(defun split (str) (let ((pos (position #\- str)))
                     (cons (intern (subseq str 0 pos)) (intern (subseq str (1+ pos))))))

(defun bigp (node) (upper-case-p (aref (string node) 0)))

(defun read-input ()
  (with-open-file (stream *input-file*)
    (loop with graph = (make-hash-table)
          for line = (read-line stream nil) while line
          for line-parts = (split line)
          do (let ((p1 (car line-parts))
                   (p2 (cdr line-parts)))
               (push p2 (gethash p1 graph))
               (push p1 (gethash p2 graph))
               )
          finally (return graph))))

(defun valid-node-p (node visited max-dup)
  (or
   (not (find node visited))
   (and
    (not (eq node '|start|))
    (< (- (length visited) (length (remove-duplicates visited))) max-dup)
    )))

(defun paths (graph visited start end max-dup)
  (if (eq start end)
      '(nil)
      (loop
        for node in (gethash start graph)
        for updated-visited = (if (bigp start)
                                  visited
                                  (cons start visited))
        when (valid-node-p node updated-visited max-dup)
          append (mapcar (lambda (path) (cons node path))
                         (paths graph updated-visited node end max-dup)))))

;; part 1
(length (let ((graph (read-input)))
          (paths graph '() '|start| '|end| 0)))

;; part 2
(length (let ((graph (read-input)))
          (paths graph '() '|start| '|end| 1)))
