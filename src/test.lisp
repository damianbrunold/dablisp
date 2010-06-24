;;;; unit tests

(format t "~%running tests")

(defmacro test (test result &optional (msg "failure"))
  (let ((et (eval test)) (er (eval result)))
    (unless (equalp et er)
      (format t "~%~A: ~A = ~A expected ~A" msg test et er))))

(require 'testprimitive)
(require 'testbase)
(require 'testnum)
(require 'teststring)
(require 'testlist)
(require 'testseq)
(require 'testcoerce)
(require 'testmacros)

(format t "~%test...")

(test (let ((f (complement #'evenp))) (list (funcall f 1) (funcall f 2) (funcall f 3) (funcall f 4))) '(t nil t nil))

(test (equal 1 1) t)
(test (equal 1.0 1.0) t)
(test (equal #\a #\a) t)
(test (equal "a" "a") t)
(test (equal '(1 . 2) '(1 . 2)) t)
(test (equal '(1 2 3) '(1 2 3)) t)
(test (equal '(1 (1 2)) '(1 (1 2))) t)
(test (equal 1 2) nil)
(test (equal 1.0 2.0) nil)
(test (equal #\a #\b) nil)
(test (equal "a" "b") nil)
(test (equal '(1 . 2) '(1 . 3)) nil)
(test (equal '(1 2 3) '(1 2 4)) nil)
(test (equal '(1 (1 2)) '(1 (1 3))) nil)
(test (equal 1 1.0) nil)
(test (equal '(1 2 3) (list 1 2 3)) t)
(test (equal '(1 2 nil) (list 1 2 nil)) t)
(test (equal #(1 2) #(1 2)) nil)

(test (equalp #(1 2) #(1 2)) t)
(test (equalp #(1 (2 3)) #(1 (2 3))) t)

;---

(test (format nil "~A" '(a b 'c 'd `(,e ,f ,@g))) "(a b 'c 'd `(,e ,f ,@g))")

(defparameter %%ht (make-hash-table))
(test (hash-table-p %%ht) t)
(test (gethash 'a %%ht) nil)
(test (gethash 'a %%ht 'x) 'x)
(test (setf (gethash 'a %%ht) 12) 12)
(test (gethash 'a %%ht) 12)
(test (gethash 'a %%ht 'x) 12)
(test (setf (gethash 'a %%ht) 13) 13)
(test (gethash 'a %%ht) 13)
(test (remhash 'a %%ht) 13)
(test (gethash 'a %%ht) nil)

(test (symbol-name 'abc) "abc")
(test (symbol-name :abc) "abc")
(test (symbol-name '|a b c|) "a b c")
(test (symbol-name '| |) " ")
(test (symbol-name 'a\bc) "abc")
(test (symbol-name 'a\\b) "a\\b")
(test (symbol-name '|a \b c|) "a b c")

(test (multiple-value-call #'list 1 '/ (values 2 3) '/ (values) '/ (floor 2.5)) '(1 / 2 3 / / 2 0.5))

(test (multiple-value-bind (a b c) (values 1 2 3) (list a b c)) '(1 2 3))

(test (multiple-value-list (floor -3 4)) '(-1 1))

;TODO(test (destructuring-bind (a (b c) d) '(1 (2 (3 4)) 5) (list a b c d)) '(1 2 (3 4) 5))
;TODO(test (destructuring-bind (a (b &key c d) &key (e 9) f) '(1 (2 :d 3 :c 4) :f 5) (list a b c d e f)) '(1 2 4 3 9 5))
;TODO(test (destructuring-bind ((a &rest x) &rest b &key c d) '((1 2 3 4) :c 5 :d 6) (list a x b c d)) '(1 (2 3 4) (:c 5 :d 6) 5 6))

(test (lognot 0) -1)
(test (lognot 7) -8)
(test (logior 1 2 4) 7)
(test (logand 5 6) 4)
(test (logxor 5 6) 3)

(test (write-to-string #\a) "#\\a")
(test (princ-to-string #\a) "a")
(test (prin1-to-string #\a) "#\\a")

(defparameter %%a (make-array 8 :fill-pointer 4))
(test %%a #(nil nil nil nil))
(test (fill-pointer %%a) 4)
(test (dotimes (i (length %%a)) (setf (aref %%a i) (* i i))) nil)
(test %%a #(0 1 4 9))
(test (setf (fill-pointer %%a) 3) 3)
(test %%a #(0 1 4))
(test (setf (fill-pointer %%a) 8) 8)
(test %%a #(0 1 4 9 nil nil nil nil))

;;; test evaluation of initforms of optional parameters
(test (funcall #'(lambda (&optional (x 1) (y x)) (list x y))) '(1 1))
(test (let ((x 1)) (labels ((f (&optional (y x)) y)) (setq x 2) (f))) 2)

(test (let* ((s (make-string-output-stream)) (*error-output* s))
        (warn "test ~s" 1) (get-output-stream-string s)) "test 1")

(test (with-output-to-string (*standard-output*) (write "abc")) "\"abc\"")
(test (with-output-to-string (s) (write "abc" :stream s :escape nil)) "abc")

(test (let ((s (make-string-input-stream "a b c")))
         (let* ((a (read s)) (b (read s)) (c (read s)))
           (list a b c))) '(a b c))

(test (let ((s (make-string-input-stream "a"))) (read-char s)) #\a)

(format t "~%done")
