(defparameter *input-file* "input")

(defun char-to-bits (c)
  "hex char to a 4 characters string representing a 4-bit binary number using 0s and 1s"
  (let ((n (read-from-string (format nil "#x~A" c))))
    (format nil "~A~A~A~A" (ldb (byte 1 3) n) (ldb (byte 1 2) n) (ldb (byte 1 1) n) (ldb (byte 1 0) n))))

(defun hex-to-bits (s)
  "hex string to a string representing an array of 4-bit binary number using 0s and 1s"
  (with-output-to-string (stream)
    (loop for char across s
          do (princ (char-to-bits char) stream))))

(defun read-input ()
  (with-open-file (stream *input-file*)
    (hex-to-bits (read-line stream))))

(defstruct packet-header version id)

(defstruct packet header content)

;; the return type of a reading function.
;; it contains the read value, the size in bits of the consumed string
;; and the type of the value read.
(defstruct reading value size type)

(defun ph-number-p (header)
  (= (packet-header-id header) 4))

(defun packet-number-p (packet)
  (ph-number-p (packet-header packet)))

(defun read-bin-int (string start length)
  (read-from-string (format nil "#b~A" (subseq string start (+ start length)))))

(defun read-packet-header (string start)
  (make-packet-header
   :version (read-bin-int string start 3)
   :id (read-bin-int string (+ start 3) 3)))

(defun read-number (string start)
  (let* ((parts (loop for i = start then (+ 5 i)
                      for part = (subseq string i (+ i 5)) until (char= (aref part 0) #\0)
                      collecting part into parts
                      finally (return
                                (mapcar
                                 (lambda (p) (subseq p 1))
                                 (append  parts (list part))))))
         (read-size (* 5 (length parts)))
         (number (read-from-string (format nil "#b~A" (apply #'concatenate 'string parts))) ))
    (make-reading :value number :size read-size :type :number)))

(defun read-operation-size (string start)
  (if (char= (aref string start) #\0)
      (make-reading
       :size 16
       :value (read-bin-int string (1+ start) 15)
       :type :size-in-bits
       )
      (make-reading
       :size 12
       :value (read-bin-int string (1+ start) 11)
       :type :size-in-packets)))

(defun read-packet (string start)
  "returns (packet . read-size)"
  (let ((marker start)
        (header (read-packet-header string start)))
    (incf marker 6)                     ; move after header
    (cond (
           ;; parse a number
           (ph-number-p header)
           (let ((reading (read-number string marker)))
             (incf marker (reading-size reading)) ; move after number
             (make-reading
              :value (make-packet :header header :content (reading-value reading))
              :size (- marker start)
              :type :number-packet
              )))
          ;; parse an operation
          (t
           (let ((reading (read-operation-size string marker)))
             (incf marker (reading-size reading)) ; move after operation size reading
             (let ((packets-reading (if (eq (reading-type reading) :size-in-bits)
                                        (read-packets string marker (reading-value reading))
                                        (read-n-packets string marker (reading-value reading)))))
               (incf marker (reading-size packets-reading)) ; move after number
               (make-reading :value (make-packet :header header :content (reading-value packets-reading))
                             :size (- marker start)
                             :type :operation-packet)))))))

(defun read-n-packets (string start n)
  "reads the next N packets"
  (loop
    for i from 1 to n
    for marker = start then (+ marker (reading-size last-reading))
    for last-reading = (read-packet string marker)
    collecting last-reading into readings
    finally (return
              (make-reading
               :size (loop for reading in readings summing (reading-size reading))
               :type :packets
               :value (mapcar #'reading-value readings)))))

(defun read-packets (string start n)
  "reads the next set of packets up to N bits"
  (loop for marker = start then (+ marker (reading-size last-reading)) until (= (- marker start) n)
        for last-reading = (read-packet string marker)
        collecting last-reading into readings
        finally (return
                  (make-reading
                   :size n
                   :type :packets
                   :value (mapcar #'reading-value readings)))))
;; part 1
(defun sum-versions (packet)
  (if (packet-number-p packet)
      (packet-header-version (packet-header packet))
      (+
       (packet-header-version (packet-header packet))
       (loop
         for subpacket in (packet-content packet) summing (sum-versions subpacket)))))

;; part 1
(sum-versions (reading-value (read-packet (read-input) 0)))

;; part 2
(defun evaluate-packet (packet)
  (if (packet-number-p packet)
      (packet-content packet)
      (case (packet-header-id (packet-header packet))
        (0
         (apply #'+ (mapcar #'evaluate-packet (packet-content packet))))
        (1
         (apply #'* (mapcar #'evaluate-packet (packet-content packet))))
        (2
         (apply #'min (mapcar #'evaluate-packet (packet-content packet))))
        (3
         (apply #'max (mapcar #'evaluate-packet (packet-content packet))))
        (5
         (if (> (evaluate-packet (first (packet-content packet)))
                (evaluate-packet (second (packet-content packet))))
             1
             0))
        (6
         (if (< (evaluate-packet (first (packet-content packet)))
                (evaluate-packet (second (packet-content packet))))
             1
             0))
        (7
         (if (= (evaluate-packet (first (packet-content packet)))
                (evaluate-packet (second (packet-content packet))))
             1
             0)))))

(evaluate-packet (reading-value (read-packet (read-input) 0)))
