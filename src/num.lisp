;;;; common lisp system library

;;; numerical functions

(defun zerop (x)
  "returns true if X is zero" 
  (= x 0))

(defun evenp (x) 
  "returns true if X is even"
  (zerop (mod x 2)))

(defun oddp (x) 
  "returns true if X is odd"
  (not (evenp x)))

(defun plusp (x) 
  "returns true if X is positive"
  (> x 0))

(defun minusp (x) 
  "returns true if X is negative"
  (< x 0))

(defun abs (n) 
  "returns the absolute value of N"
  (if (< n 0) (- n) n))

(defun signum (n) 
  "returns the sign of N"
  (if (zerop n) 0 (/ n (abs n))))

(defun ffloor (n)
  "returns the floor of n as a float" 
  (float (floor n)))

(defun fceiling (n) 
  "returns the ceiling of n as a float"
  (float (ceiling n)))

(defun ftruncate (n)
  "returns the truncation of n as a float" 
  (float (truncate n)))

(defun fround (n) 
  "returns the rounded value of n as a float"
  (float (round n)))

(defun max (n &rest args)
  "returns the maximum of all N and ARGS"
  (do ((m n (if (< m (car a)) (car a) m)) 
       (a args (cdr a)))
      ((null a) m)))

(defun min (n &rest args)
  "returns the minimum of all N and ARGS"
  (do ((m n (if (> m (car a)) (car a) m))
       (a args (cdr a)))
      ((null a) m)))

(defun fact (n)
  "returns the factorial for positive integral N"
  (assert (and (integerp n) (plusp n)) "arg ~A must be a positive integer" n)
  (do ((i 1 (1+ i)) 
       (f 1 (* f i)))
      ((> i n) f))) 

(defun recfact (n)
  (if (= n 0)
      1
      (* n (recfact (1- n)))))

(defun make-random-state (&optional state)
  (cond
   ((null state) *random-state*)
   ((eq state t) (list 'random-state (get-internal-real-time)))
   (t state)))

(defvar *random-state* (make-random-state t))

(defun nextrnd (random-state)
  (mod (+ (* (cadr random-state) 1664525) 1013904223) 4294967296))

(defun random (limit &key (random-state *random-state*))
  (setf (cadr random-state) (nextrnd random-state))
  (mod (truncate (cadr random-state) 4) limit))

(defun gcd2 (a b)
  (if (zerop b)
      a
      (gcd2 b (mod a b))))

(defun gcd (&rest numbers)
  "returns the greatest common divisor of the NUMBERS"
  (cond
   ((null numbers) 0)
   ((null (cdr numbers)) (abs (car numbers)))
   (t (let ((g (gcd2 (first numbers) (second numbers))))
        (do ((n (cddr numbers) (cdr n))
             (g g (gcd2 g (car n))))
            ((null n) g))))))

(defun lcm2 (a b)
  (/ (* a b) (gcd2 a b)))

(defun lcm (&rest numbers)
  "returns the least common multiple of the NUMBERS"
  (cond
   ((null numbers) 1)
   ((null (cdr numbers)) (abs (car numbers)))
   (t (let ((r (lcm2 (first numbers) (second numbers))))
        (do ((n (cddr numbers) (cdr n))
             (r r (lcm2 r (car n))))
            ((null n) r))))))
