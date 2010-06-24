;;;; code samples from ansi common lisp / Paul Graham

(defun test (test result &optional (msg "failure"))
  (let ((r (eval test)))
    (unless (equal r result)
      (format t "~%~A: ~A = ~A expected ~A" msg test r result))))

;----

(defun sum (n)
  (let ((s 0))
    (dotimes (i n s)
      (incf s i))))

(test '(sum 101) 5050)

(defun addn (n)
  #'(lambda (x) (+ x n)))

(test '(funcall (addn 5) 1) 6)

(test '(+ 2 3) 5)
(test '(+) 0)
(test '(+ 2) 2)
(test '(+ 2 3) 5)
(test '(+ 2 3 4) 9)
(test '(+ 2 3 4 5) 14)

(test '(/ (- 7 1) (- 4 2)) 3)

(test '(list '(+ 2 1) (+ 2 1)) '((+ 2 1) 3))

(test '(cons 'a '(b c d)) '(a b c d))

(test '(cons 'a (cons 'b nil)) '(a b))
(test '(list 'a 'b) '(a b))

(test '(car '(a b c)) 'a)
(test '(cdr '(a b c)) '(b c))
(test '(car (cdr (cdr '(a b c d)))) 'c)
(test '(third '(a b c d)) 'c)

(test '(listp '(a b c)) t)
(test '(listp 27) nil)
(test '(null nil) t)
(test '(not nil) t)
(test '(if (listp '(a b c))
           (+ 1 2)
           (+ 5 6))
      3)
(test '(if (listp 27)
           (+ 1 2)
           (+ 5 6))
      11)
(test '(if (listp 27)
           (+ 2 3))
      nil)
(test '(if 27 1 2) 1)
(test '(and t (+ 1 2)) 3)

(defun our-third (x)
  (car (cdr (cdr x))))

(test '(our-third '(a b c d)) 'c)

(test '(> (+ 1 4) 3) t)

(defun sum-greater (x y z)
  (> (+ x y) z))

(test '(sum-greater 1 4 3) t)

(defun our-member (obj lst)
  (if (null lst)
      nil
      (if (eql (car lst) obj)
          lst
          (our-member obj (cdr lst)))))

(test '(our-member 'b '(a b c)) '(b c))
(test '(our-member 'z '(a b c)) nil)

(test '(format nil "~A plus ~A equals ~A." 2 3 (+ 2 3)) "2 plus 3 equals 5.")

(defun askem (string)
  (format t "~A" string)
  (read))

(test '(with-input-from-string (*standard-input* "29") 
                               (askem "How old are you?")) 
      29)

(test '(let ((x 1) (y 2))
         (+ x y))
      3)

(defun ask-number ()
  (format t "Please enter a number. ")
  (let ((val (read)))
    (if (numberp val)
        val
        (ask-number))))

(test '(with-input-from-string (*standard-input* "a b 52") 
                               (ask-number)) 
      52)

(defparameter *glob* 99)
(test '(boundp '*glob*) t)
(test '(setf *glob* 98) 98)
(test '(let ((n 10))
         (setf n 2)
         n)
      2)
(setf x (list 'a 'b 'c))
(test 'x '(a b c))
(setf (car x) 'n)
(test 'x '(n b c))

(setf lst '(c a r a t))
(test 'lst '(c a r a t))
(test '(remove 'a lst) '(c r t))
(test 'lst '(c a r a t))
(setf lst (remove 'a lst))
(test 'lst '(c r t))

(defun show-squares (start end)
  (do ((i start (1+ i)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(terpri)
(show-squares 2 5)

(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (1+ len)))
    len))

(test '(our-length '(a b c)) 3)

(test '(apply #'+ '(1 2 3)) 6)
(test '(apply #'+ 1 2 '(3 4 5)) 15)
(test '(funcall #'+ 1 2 3) 6)

(test '((lambda (x) (+ x 100)) 1) 101)

(test '(funcall #'(lambda (x) (+ x 100)) 1) 101)

(test '(typep 27 'integer) t)

(setf x (cons 'a nil))
(test '(car x) 'a)
(test '(cdr x) nil)
 