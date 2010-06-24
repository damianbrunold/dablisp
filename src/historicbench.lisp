;;;; historical benchmarks ex. Gabriel Performance and Evaluation of Lisp Implementations

(format t "~%tak")

(defun tak (x y z)
  (if (not (< y x))
      z
      (tak (tak (1- x) y z)
           (tak (1- y) z x)
           (tak (1- z) x y))))

(time (print (tak 18 12 6)))

(format t "~%stak")

(defvar x nil)
(defvar y nil)
(defvar z nil)

(defun stak (x y z)
  (stak-aux))

(defun stak-aux ()
  (if (not (< y x))
      z
      (let ((x (let ((x (1- x))
                     (y y)
                     (z z))
                 (stak-aux)))
            (y (let ((x (1- y))
                     (y z)
                     (z x))
                 (stak-aux)))
            (z (let ((x (1- z))
                     (y x)
                     (z y))
                 (stak-aux))))
        (stak-aux))))

(time (print (stak 18 12 6)))

(format t "~%ctak")

(defun ctak (x y z)
  (catch 'ctak (ctak-aux x y z)))

(defun ctak-aux (x y z)
  (cond ((not (< y x))
         (throw 'ctak z))
        (t (ctak-aux
            (catch 'ctak
                   (ctak-aux (1- x) y z))
            (catch 'ctak
                   (ctak-aux (1- y) z x))
            (catch 'ctak
                   (ctak-aux (1- z) x y))))))

(time (print (ctak 18 12 6)))

(format t "~%takl")

(defun listn (n)
  (if (not (= 0 n))
      (cons n (listn (1- n)))))

(defvar 18l (listn 18))
(defvar 12l (listn 12))
(defvar 6l (listn 6))

(defun mas (xx yy zz)
  (if (not (shorterp yy xx))
      zz
      (mas (mas (cdr xx) yy zz)
           (mas (cdr yy) zz xx)
           (mas (cdr zz) xx yy))))

(defun shorterp (xx yy)
  (and yy (or (null xx)
             (shorterp (cdr xx)
                       (cdr yy)))))

(time (print (mas 18l 12l 6l)))

(values)
