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
               (push p1 (gethash p2 graph)))
          finally (return graph))))

(defun has-duplicated (nodes)
  (> (- (length nodes) (length (remove-duplicates nodes))) 0))

(defun valid-step-p (node visited has-dups allow-dups)
  (and (not (eq node '|start|))
       (or
        (not (find node visited))
        (and allow-dups (not has-dups)))))

(defun paths (graph visited has-dups start end allows-dups)
  (if (eq start end)
      '(nil)
      (loop
        with updated-visited = (if (bigp start) visited (cons start visited))
        with updated-has-dups = (or has-dups (has-duplicated updated-visited))
        for node in (gethash start graph)
        when (valid-step-p node updated-visited updated-has-dups allows-dups)
          append (mapcar (lambda (path) (cons node path))
                         (paths graph updated-visited updated-has-dups node end allows-dups)))))

;; part 1
(length (paths (read-input) '() nil '|start| '|end| nil))

;; part 2
(time (length (paths (read-input) '() nil '|start| '|end| t)))
