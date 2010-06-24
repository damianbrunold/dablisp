;;;; macros unit tests

(format t "~%testmacros...")

(test (loop (return 12)) 12)

(test (catch 'error (assert nil "test ~a" 1)) "test 1")

(test (cond) nil)
(test (cond (t 'a 'b)) 'b)
(test (cond ((= 1 2) 'a) ((= 1 1) 'b)) 'b)
(test (cond (nil 'a)) nil) 

(test (let ((x 2)) (setf x 3) x) 3)
(test (setf (char "123" 0) #\a) "a23")
(test (setf (char "123" 1) #\a) "1a3")
(test (let ((x (make-array 3 :initial-element 1))) (setf (svref x 1) 2) x) #(1 2 1))
(test (setf (aref "123" 1) #\a) "1a3")
; setf-gethash is tested elsewhere
(test (let ((a (list 1 2 3))) (setf (car a) 4) a) '(4 2 3))
(test (let ((a (list 1 2 3))) (setf (cadr a) 4) a) '(1 4 3))
(test (let ((a (list 1 2 3))) (setf (cdr a) '(4 5)) a) '(1 4 5))
(test (let ((a (make-array 3 :fill-pointer 0 :initial-element 1))) (setf (fill-pointer a) 2) a) #(1 1))
(test (let ((a (list 1 2 3))) (setf (nth 0 a) 4) a) '(4 2 3))
(test (let ((a (list 1 2 3))) (setf (nth 1 a) 4) a) '(1 4 3))
(test (let ((a (list 1 2 3))) (setf (elt a 0) 4) a) '(4 2 3))
(test (let ((a (list 1 2 3))) (setf (elt a 1) 4) a) '(1 4 3))
(test (setf (elt "123" 0) #\a) "a23")
(test (setf (elt "123" 1) #\a) "1a3")
(test (setf (elt #(1 2 3) 1) 4) #(1 4 3))
(test (let ((x "abcde")) (list (setf (subseq x 1 3) "xy") x)) '("xy" "axyde")) 
(test (let ((str "012345")) (setf (subseq str 4) "abc") str) "0123ab")

(test (and) t)
(test (and 1) 1)
(test (and 2 1) 1)
(test (and 3 2 1) 1)
(test (and nil) nil)
(test (and 1 nil) nil)
(test (and 1 1 nil) nil)
(test (and (and 2 2) (and 1 1)) 1)
(test (and (and 1 1) (and 1 nil)) nil)

(test (or) nil)
(test (or nil) nil)
(test (or nil nil) nil)
(test (or nil nil nil) nil)
(test (or 1) 1)
(test (or nil 1) 1)
(test (or nil nil 1) 1)
(test (or (or nil nil) (or nil 1)) 1)
(test (or (or nil nil) (or nil nil)) nil)

(test (let ((x 1)) (incf x) x) 2)
(test (let ((x #(1 2 3))) (incf (aref x 0)) x) #(2 2 3))

(test (let ((x 1)) (decf x) x) 0)
(test (let ((x #(1 2 3))) (decf (aref x 0)) x) #(0 2 3))

(test (let ((x 1)) (case x ((1 2 3) 'a) ((4 5 6) 'b) (t 'c))) 'a)
(test (let ((x 3)) (case x ((1 2 3) 'a) ((4 5 6) 'b) (t 'c))) 'a)
(test (let ((x 4)) (case x ((1 2 3) 'a) ((4 5 6) 'b) (t 'c))) 'b)
(test (let ((x 7)) (case x ((1 2 3) 'a) ((4 5 6) 'b) (t 'c))) 'c)

(test (let ((x 1)) (ecase x ((1 2 3) 'a) ((4 5 6) 'b))) 'a)
(test (let ((x 3)) (ecase x ((1 2 3) 'a) ((4 5 6) 'b))) 'a)
(test (let ((x 4)) (ecase x ((1 2 3) 'a) ((4 5 6) 'b))) 'b)
(test (let ((x 7)) (catch 'type-condition (ecase x ((1 2 3) 'a) ((4 5 6) 'b)))) "no matching clause in ecase")

(test (prog1 1 2 3) 1)

(test (prog2 1 2 3) 2)

(test (progn 1 2 3) 3) ; actually a special form

(test (let* ((a 1) (b a) (c b)) c) 1)

(test (let ((x nil)) (pushnew 1 x) (pushnew 1 x) (pushnew 2 x) x) '(2 1))

(test (let ((x nil)) (push 1 x) (push 1 x) (push 2 x) x) '(2 1 1))

(test (let ((x '(1 2))) (pop x) x) '(2))

(test (let ((x nil)) (dotimes (i 10) (push i x)) x) '(9 8 7 6 5 4 3 2 1 0))

(test (let ((x nil)) (dolist (i '(1 2 3)) (push i x)) x) '(3 2 1))

(test (loop (return 2)) 2)

(test (when (= 1 1) 'a) 'a)
(test (when (= 1 2) 'a) nil)

(test (unless (= 1 2) 'a) 'a)
(test (unless (= 1 1) 'a) nil)

; time

; trace

; untrace

(test (with-output-to-string (s) (write 12 :stream s)) "12")
(test (with-output-to-string (s) (write #\a :stream s :escape t)) "#\\a")
(test (with-output-to-string (s) (write #\a :stream s :escape nil)) "a")
(test (with-output-to-string (s) (print #\a s)) (format nil "~%#\\a"))
(test (with-output-to-string (s) (princ #\a s)) "a")
(test (with-output-to-string (s) (prin1 #\a s)) "#\\a")
(test (with-output-to-string (s) (write-char #\a s)) "a")
(test (with-output-to-string (*standard-output*) (write-char #\a)) "a")

(test (with-input-from-string (s "hallo welt") (let* ((a (read s)) (b (read s))) (list a b))) '(hallo welt))

