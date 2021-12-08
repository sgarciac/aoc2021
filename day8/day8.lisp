(ql:quickload :cl-ppcre)
(defparameter *input-file* "input")

;; part 1
(with-open-file (stream *input-file*)
  (loop
    for line = (read-line stream nil nil) while line
    for parts = (cl-ppcre:split "\\|" line)
    for output = (cl-ppcre:split "\\s" (string-trim " " (second parts))  :limit 0)
    summing (count-if (lambda (entry) (find (length entry) '(2 4 3 7))) output)))

;; part 2
;;
;;  a
;; b c
;;  d
;; e f
;;  g
;;
;; apperances
;;
;; a: 8
;; b: 6 * (0 4 5 6 8 9)
;; c: 8
;; d: 7
;; e: 4 * (0 2 6 8
;; f: 9 * (0 1 3 4 5 6 7 8 9)
;; g: 7

;; we can identify b,e,f counting apperances
;; By having number 1 and f, we can identify c (the other one)
;; by having c, we can identify a (only other one with 8 apperances)
;; we have a,b,c,e,f
;; By having b,e,f,c and number 8, we can identify number 0 (only other one that have them all)
;; by having 0 and a,b,c,e,f, we can identify g (only letter missing)
;; by having g, we have d (other one with 7 appearances)
;; we havee a,b,c,d,e,f,g
