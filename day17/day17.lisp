;; Happy hacking, sergio - Emacs ♥ you!
(defparameter *minx* 48)
(defparameter *maxx* 70)
(defparameter *miny* -189)
(defparameter *maxy* -148)

(defun natural-p (n)
  (zerop (mod n 1)))

(defun xpos (vx s)
  (if (<= s vx)
      (- (* vx s) (/ (* (1- s) s) 2))
      (- (* vx vx) (/ (* (1- vx) vx) 2))))

(defun ypos (vy s)
  (- (* vy s) (/ (* (1- s) s) 2)))

(defun v-to-stop-at (d)
  (/ (1- (sqrt (1+ (* 8 d)))) 2))

(defun hits-y (vy s-min)
  (loop for s = s-min then (1+ s)
        for y = (ypos vy s) until (< y *miny*)
        do (when
               (and
                (>= y *miny*)
                (<= y *maxy*)
                )
             (return-from hits-y s))
        finally (return nil)))

;; part 1
(loop for x from *minx* upto *maxx*
      when (natural-p (v-to-stop-at x))
        maximizing (loop
                     for vy from 500 downto 1
                     when (hits-y vy (v-to-stop-at x)) maximizing vy) into maxvy
                       finally (return (ypos maxvy maxvy)))

;; part 2
(defun hits-xy-p (vx vy)
  (loop for s = 0 then (1+ s) until (or (< (ypos vy s) *miny*) (> (xpos vx s) *maxx*))
        do
           (progn
             (when (and
                    (>= (ypos vy s) *miny*)
                    (<= (ypos vy s) *maxy*)
                    (<= (xpos vx s) *maxx*)
                    (>= (xpos vx s) *minx*))
               (return-from hits-xy-p t)))
        finally (return nil)))

(loop for vx from 1 to *maxx*
      summing
      (loop for vy from 1000 downto *miny* counting (hits-xy vx vy)))
