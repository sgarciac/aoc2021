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
;; appearances
;;
;; a: 8
;; b: 6 * (appears in: 0 4 5 6 8 9)
;; c: 8
;; d: 7
;; e: 4 * (appears in: 0 2 6 8)
;; f: 9 * (appears in: 0 1 3 4 5 6 7 8 9)
;; g: 7
;;
;; * = unique number of appearances

;; we can identify b,e,f counting apperances
;; By having number 1 and f, we can identify c (the other one)
;; by having c, we can identify a (only other one with 8 apperances)
;; we have a,b,c,e,f
;; By having b,e,f,c and number 8, we can identify number 0 (only other one that have them all)
;; by having 0 and a,b,c,e,f, we can identify g (only letter missing)
;; by having g, we have d (other one with 7 appearances)
;; we havee a,b,c,d,e,f,g, the rest follows...

;; part 2

;; a sample (smp) is a list of signal sets (ss).
;; a signal is represented by a char from a to g
;; a signal set is represented by the sorted string containing signals (chars)
;; the output is a list of 4 signal sets.

(defun extract-from-line (line part)
  (mapcar (lambda (ss) (sort ss #'char-lessp))
          (cl-ppcre:split "\\s"
                          (string-trim
                           " "
                           (funcall part (cl-ppcre:split "\\|" line))
                           ))))

(defun get-sample-from-line (line)
  "Extract the sample from an input line."
  (extract-from-line line #'first))

(defun get-output-from-line (line)
  "Extract the output from an input line."
  (extract-from-line line #'second))

(defun count-signal (s smp)
  (count-if (lambda (ss) (find s ss)) smp))

(defun count-appearances (smp)
  "returns an assoc list (s . <number of appearences in smp>)"
  (loop for s across "abcdefg"
        collecting `(,s . ,(count-signal s smp))))

(defun find-by-appearances (appearances number)
  (loop for (s . n) in appearances when (= number n) collect s ))

(defun signal-sample-contains-all (ss &rest signals)
  (loop for s in signals always (find s ss)))

(defun find-mapping (smp)
  "Given a sample, return an assoc list (ss . <number as a char>)"
  (let* ((appearances (count-appearances smp))
         (ss-for-1 (find-if (lambda (ss) (= 2 (length ss))) smp))
         (ss-for-8 (find-if (lambda (ss) (= 7 (length ss))) smp))
         (ss-for-4 (find-if (lambda (ss) (= 4 (length ss))) smp))
         (ss-for-7 (find-if (lambda (ss) (= 3 (length ss))) smp))
         (s-for-b (first (find-by-appearances appearances 6)))
         (s-for-e (first (find-by-appearances appearances 4)))
         (s-for-f (first (find-by-appearances appearances 9)))
         (s-for-c (find-if (lambda (ch) (char/= s-for-f ch)) ss-for-1))
         (s-for-a (find-if (lambda (ch) (char/= s-for-c ch)) (find-by-appearances appearances 8)))
         (ss-for-0 (find-if (lambda (ss)
                              (and
                               (string/= ss ss-for-8)
                               (signal-sample-contains-all
                                ss s-for-b s-for-e s-for-f s-for-c))) smp))
         (s-for-g (find-if (lambda (s) (not
                                        (find s (list s-for-a s-for-b s-for-c s-for-e s-for-f)))) ss-for-0))
         (s-for-d (find-if (lambda (ch) (char/= s-for-g ch)) (find-by-appearances appearances 7)))
         (ss-for-2 (sort (format nil "~A~A~A~A~A" s-for-a s-for-c s-for-d s-for-e s-for-g) #'char-lessp))
         (ss-for-3 (sort (format nil "~A~A~A~A~A" s-for-a s-for-c s-for-d s-for-f s-for-g) #'char-lessp))
         (ss-for-5 (sort (format nil "~A~A~A~A~A" s-for-a s-for-b s-for-d s-for-f s-for-g) #'char-lessp))
         (ss-for-6 (sort (format nil "~A~A~A~A~A~A" s-for-a s-for-b s-for-d s-for-e s-for-f s-for-g) #'char-lessp))
         (ss-for-9 (sort (format nil "~A~A~A~A~A~A" s-for-a s-for-b s-for-c s-for-d s-for-f s-for-g) #'char-lessp))
         )
    `((,ss-for-0 . #\0) (,ss-for-1 . #\1) (,ss-for-2 . #\2) (,ss-for-3 . #\3) (,ss-for-4 . #\4) (,ss-for-5 . #\5)
      (,ss-for-6 . #\6) (,ss-for-7 . #\7) (,ss-for-8 . #\8) (,ss-for-9 . #\9))
    ))

(with-open-file (stream *input-file*)
  (loop
    for line = (read-line stream nil nil) while line
    for smp = (get-sample-from-line line)
    for output = (get-output-from-line line)
    for mapping = (find-mapping smp)
    summing
    (parse-integer
     (format nil "~A~A~A~A"
             (cdr (assoc (first output) mapping :test #'string=))
             (cdr (assoc (second output) mapping :test #'string=))
             (cdr (assoc (third output) mapping :test #'string=))
             (cdr (assoc (fourth output) mapping :test #'string=))))))
