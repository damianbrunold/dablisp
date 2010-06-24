;;;; seq unit tests

(format t "~%testseq...")

(test (find 1 '((1 . a) (2 . b) (3 . c)) :key #'car) '(1 . a))
(test (find 2 '((1 . a) (2 . b) (3 . c)) :key #'car) '(2 . b))
(test (find 3 '((1 . a) (2 . b) (3 . c)) :key #'car) '(3 . c))
(test (find 4 '((1 . a) (2 . b) (3 . c)) :key #'car) nil)
(test (find 2 '((1 . a) (2 . b) (3 . c) (2 . c)) :key #'car :from-end t) '(2 . c))
(test (find #\c "abcd") #\c)
(test (find #\e "abcd") nil)
(test (find 3 #(1 2 3 4)) 3)
(test (find 5 #(1 2 3 4)) nil)

(test (find-if #'oddp '((2 . a) (3 . b)) :key #'car) '(3 . b))
(test (find-if #'oddp #(2 4 3 5)) 3)
(test (find-if #'oddp #(2 4 3 5) :from-end t) 5)

(test (position 1 '(1 2 3)) 0)
(test (position 2 '(1 2 3)) 1)
(test (position 3 '(1 2 3)) 2)
(test (position 4 '(1 2 3)) nil)
(test (position 2 '(1 2 3 2 4) :from-end t) 3)
(test (position 1 #(1 2 3)) 0)
(test (position 2 #(1 2 3)) 1)
(test (position 3 #(1 2 3)) 2)
(test (position 4 #(1 2 3)) nil)
(test (position 2 #(1 2 3 2 4) :from-end t) 3)

(test (position-if #'oddp '(2 3 4)) 1)
(test (position-if #'oddp #(2 3 4)) 1)

(test (svref #(1 2 3) 0) 1)
(test (svref #(1 2 3) 1) 2)
(test (svref #(1 2 3) 2) 3)

(test (vector-list #(1 2 3)) '(1 2 3))

(test (list-vector '(1 2 3)) #(1 2 3))

(test (string-list "abc") '(#\a #\b #\c))
(test (string-list "") nil)

(test (list-string '(#\a #\b #\c)) "abc")
(test (list-string nil) "") 

(test (elt "123" 1) #\2)
(test (elt '(1 2 3) 1) 2)
(test (elt '(1 2 3) 0) 1)
(test (elt '(1 2 3) 1) 2)
(test (elt '(1 2 3) 2) 3)
(test (elt "abc" 1) #\b)

(test (vector-length #()) 0)
(test (vector-length #(1 2 3)) 3)

(test (length '()) 0)
(test (length '(1)) 1)
(test (length '(1 2)) 2)
(test (length '(1 2 3 4 5 6 7 8 9 0)) 10)
(test (length "abc") 3)

(test (equal "abc" (copy-vector "abc")) t)
(test (let* ((a "abc") (b (copy-vector a))) (setf (char b 0) #\z) (list a b)) '("abc" "zbc"))

(test (equal '(1 2 3) (copy-seq '(1 2 3))) t)
(test (equal "abc" (copy-seq "abc")) t)
(test (let* ((a "abc") (b (copy-seq a))) (setf (char b 0) #\z) (list a b)) '("abc" "zbc"))

(test (reverse '()) '())
(test (reverse '(1)) '(1))
(test (reverse '(1 2)) '(2 1))
(test (reverse '(1 2 3)) '(3 2 1))
(test (reverse "abc") "cba")
(test (reverse #(1 2 3)) #(3 2 1))

(test (nreverse '(1 2 3)) '(3 2 1))
(test (nreverse "a") "a")
(test (nreverse "ab") "ba")
(test (nreverse "abc") "cba")
(test (nreverse "abcd") "dcba")
(test (nreverse "abcde") "edcba")
(test (nreverse #(1 2 3)) #(3 2 1))
(test (let ((a "abc")) (nreverse a) a) "cba")

(test (vector-concatenate 'char "abc" "def") "abcdef")
(test (vector-concatenate 't "abc" "def") #(#\a #\b #\c #\d #\e #\f))

(test (concatenate 'list '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(test (concatenate 'string "abc" "def") "abcdef")
(test (concatenate 'string "a" "b" "c") "abc")

(test (elts 0 #(1 2) #(3 4 5)) '(1 3))
(test (elts 1 #(1 2) #(3 4 5)) '(2 4))
(test (elts 2 #(1 2) #(3 4 5)) nil)

(test (every #'oddp '(1 3 5 7)) t)
(test (every #'oddp '(1 3 4 7)) nil)
(test (every #'digit-char-p "1234") t)
(test (every #'digit-char-p "xyz") nil)
(test (every #'oddp #(1 3 5 7)) t)
(test (every #'oddp #(1 3 4 7)) nil)
(test (every #'= #(1 2 3) #(1 2 3) #(1 2 3)) t)
(test (every #'= #(1 2 3) #(1 3 3) #(1 2 3)) nil)

(test (some #'oddp '(2 4 5 6 8)) t)
(test (some #'oddp '(2 4 6 8)) nil)
(test (some #'digit-char-p "12z4") t)
(test (some #'digit-char-p "xyz") nil)
(test (some #'= #(3 2 1) #(1 2 3) #(1 2 1)) t)
(test (some #'= #(3 2 1) #(1 2 3) #(2 3 1)) nil)

(test (remove-duplicates '(1 2 3 4 2 1)) '(3 4 2 1))
(test (remove-duplicates '(1 2 3 4) :test #'(lambda (x y) (and (oddp x) (oddp y)))) '(2 3 4))
(test (remove-duplicates '(1 2 3 4 2 1) :from-end t) '(1 2 3 4))
(test (remove-duplicates '(1 2 3 4) :test #'(lambda (x y) (and (oddp x) (oddp y))) :from-end t) '(1 2 4))
(test (remove-duplicates #(1 2 3 4 2 1)) #(3 4 2 1))
(test (remove-duplicates #(1 2 3 4 2 1) :from-end t) #(1 2 3 4))
(test (remove-duplicates #(1 2 3 4) :test #'(lambda (x y) (and (oddp x) (oddp y)))) #(2 3 4))
(test (remove-duplicates "abbccdd") "abcd")

(test (delete-duplicates "abbccdd") "abcd")

(test (remove 1 '(1 2 3 1 2 3 1 2 3)) '(2 3 2 3 2 3))
(test (remove 1 '(1 2 3 1 2 3 1 2 3) :count 1) '(2 3 1 2 3 1 2 3))
(test (remove 1 '(1 2 3 1 2 3 1 2 3) :count 2) '(2 3 2 3 1 2 3))
(test (remove 1 '(1 2 3 1 2 3 1 2 3) :count 3) '(2 3 2 3 2 3))
(test (remove 1 '(1 2 3 1 2 3 1 2 3) :start 3 :end 6) '(1 2 3 2 3 1 2 3))
(test (remove 1 '(1 2 3 1 2 3 1 2 3) :count 2 :from-end t) '(1 2 3 2 3 2 3))
(test (remove #\1 "123123123") "232323")
(test (remove #\1 "123123123" :count 2) "2323123")
(test (remove #\1 "123123123" :count 2 :from-end t) "1232323")

(test (remove-if #'oddp '(1 2 3 1 2 3 1 2 3)) '(2 2 2))
(test (remove-if #'oddp '(1 2 3 1 2 3 1 2 3) :count 2 :from-end t) '(1 2 3 1 2 3 2))
(test (remove-if #'oddp #(1 2 3 1 2 3 1 2 3) :count 2 ) #(2 1 2 3 1 2 3))
(test (remove-if #'oddp #(1 2 3 1 2 3 1 2 3) :from-end t :count 2) #(1 2 3 1 2 3 2))

(test (delete #\1 "123123123" :count 2) "2323123")

(test (delete-if #'oddp '(1 2 3 1 2 3 1 2 3)) '(2 2 2))

(test (substitute 2 1 '(1 2 3 1) :count 1) '(2 2 3 1))
(test (substitute #\z #\a "abracadabra") "zbrzczdzbrz")
(test (substitute #\z #\a "abracadabra" :start 1 :end 4) "abrzcadabra")
(test (let ((x '(1 2 3))) (substitute 4 2 x) x) '(1 2 3))

(test (substitute-if #\a #'oddp '(1 2 3 4)) '(#\a 2 #\a 4))
(test (substitute-if #\a #'oddp #(1 2 3 4)) #(#\a 2 #\a 4))

(test (nsubstitute 2 1 '(1 2 3 1) :count 1) '(2 2 3 1))
(test (let ((x #(1 2 3))) (nsubstitute 4 2 x) x) #(1 4 3))

(test (nsubstitute-if #\a #'oddp '(1 2 3 4)) '(#\a 2 #\a 4))
(test (nsubstitute-if #\a #'oddp #(1 2 3 4)) #(#\a 2 #\a 4))

(test (subseq '(1 2 3 4 5) 0) '(1 2 3 4 5))
(test (subseq '(1 2 3 4 5) 1) '(2 3 4 5))
(test (subseq '(1 2 3 4 5) 2) '(3 4 5))
(test (subseq '(1 2 3 4 5) 5) nil)
(test (subseq '(1 2 3 4 5) 2 4) '(3 4))
(test (subseq "abcd" 1 3) "bc")
(test (subseq "abcd" 1) "bcd")

(test (sort '(3 1 4 2 1 5) #'<) '(1 1 2 3 4 5))
(test (sort "fesaza" #'char<) "aaefsz")

(test (stable-sort '(3 1 4 2 1 5) #'<) '(1 1 2 3 4 5))
(test (stable-sort "fesaza" #'char<) "aaefsz")
(test (stable-sort #((1 a) (4 d) (3 c) (2 b)) #'< :key #'car) #((1 a) (2 b) (3 c) (4 d)))
(test (stable-sort #((1 z) (2 d) (1 a) (2 b) (1 b)) #'< :key #'car) #((1 z) (1 a) (1 b) (2 d) (2 b)))

(defparameter %%v (make-array 8 :fill-pointer 0))
(test (vector-push 1 %%v) 0)
(test (vector-push 2 %%v) 1)
(test (vector-push 3 %%v) 2)
(test %%v #(1 2 3))
(test (vector-pop %%v) 3)
(test (vector-pop %%v) 2)
(test (vector-pop %%v) 1)
(test %%v #())

(test (replace "abcdefghij" "0123456789" :start1 4 :end1 7 :start2 4) "abcd456hij")
(test (replace "abcdefghij" '(#\0 #\1 #\1 #\3 #\4 #\5 #\6 #\7 #\8 #\9) :start1 4 :end1 7 :start2 4) "abcd456hij")
(test (replace #(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j) "0123456789" :start1 4 :end1 7 :start2 4) #(#\a #\b #\c #\d #\4 #\5 #\6 #\h #\i #\j))
(test (replace #(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j) '(#\0 #\1 #\1 #\3 #\4 #\5 #\6 #\7 #\8 #\9) :start1 4 :end1 7 :start2 4) #(#\a #\b #\c #\d #\4 #\5 #\6 #\h #\i #\j))
(test (replace '(1 2 3) '(4 5 6) :start1 0 :start2 0) '(4 5 6))
(test (replace '(1 2 3) '(4 5 6) :start1 0 :end1 1 :start2 0) '(4 2 3))
(test (replace '(1 2 3) '(4 5 6) :start1 0 :end1 1 :start2 1) '(5 2 3))
(test (replace '(1 2 3) '(4 5 6) :start1 1 :end1 3 :start2 0) '(1 4 5))
(test (let ((x '(1 2 3))) (replace x x :start1 0 :end1 2 :start2 1)) '(2 3 3))
(test (let ((x '(1 2 3))) (replace x x :start1 1 :end1 3 :start2 0)) '(1 1 2))

(test (make-sequence 'list 3 :initial-element 'a) '(a a a))
(test (make-sequence 'string 3 :initial-element #\a) "aaa")
(test (make-sequence 'string 3) "   ")
(test (make-sequence 'vector 3) #(nil nil nil))

(test (merge 'list '(1 3 5) '(2 4 6) #'<) '(1 2 3 4 5 6))
(test (merge 'list '((1 . a) (1 . b)) '((1 . c) (1 . d)) #'< :key #'car) '((1 . a) (1 . b) (1 . c) (1 . d)))

(test (count #\a "how many A's are there in here?") 2)
(test (count-if-not #'oddp '((1) (2) (3) (4)) :key #'car) 2)
(test (count-if #'upper-case-p "The Crying of Lot 49" :start 4) 2)

(test (count #\a "how many A's are there in here?" :from-end t) 2)
(test (count-if-not #'oddp '((1) (2) (3) (4)) :key #'car :from-end t) 2)
(test (count-if #'upper-case-p "The Crying of Lot 49" :start 4 :from-end t) 2)

(defparameter %%a (list 1 2 3 4))
(defparameter %%b (list 10 10 10 10))
(test (map-into %%a #'+ %%a %%b) '(11 12 13 14))
(test %%a '(11 12 13 14))
(defparameter %%c '(one two three))
(test (map-into %%a #'cons %%c %%a) '((one . 11) (two . 12) (three . 13) 14))
(test %%a '((one . 11) (two . 12) (three . 13) 14))

(test (map 'string #'(lambda (x y)
                       (char "01234567890ABCDEF" (mod (+ x y) 16)))
           '(1 2 3 4)
           '(10 9 8 7)) 
      "AAAA")
(test (map 'list #'- '(1 2 3 4)) '(-1 -2 -3 -4))
(test (map 'string
           #'(lambda (x) (if (oddp x) #\1 #\0))
           '(1 2 3 4))
      "1010")

(test (reduce #'* '(1 2 3 4 5)) 120)
(test (reduce #'append '((1) (2)) :initial-value '(i n i t)) '(i n i t 1 2))
(test (reduce #'append '((1) (2)) :from-end t :initial-value '(i n i t)) '(1 2 i n i t))
(test (reduce #'- '(1 2 3 4)) -8)
(test (reduce #'- '(1 2 3 4) :from-end t) -2)
(test (reduce #'+ '()) 0)
(test (reduce #'+ '(3)) 3)
(test (reduce #'+ '(foo)) 'foo)
(test (reduce #'* '()) 1)
(test (reduce #'list '(1 2 3 4)) '(((1 2) 3) 4))
(test (reduce #'list '(1 2 3 4) :from-end t) '(1 (2 (3 4))))
(test (reduce #'list '(1 2 3 4) :initial-value 'foo) '((((foo 1) 2) 3) 4))
(test (reduce #'list '(1 2 3 4) :from-end t :initial-value 'foo) '(1 (2 (3 (4 foo)))))

(test (mismatch "abcd" "ABCDE" :test #'char-equal) 4)
(test (mismatch '(3 2 1 1 2 3) '(1 2 3) :from-end t) 3)
(test (mismatch '(1 2 3) '(2 3 4) :test-not #'eq :key #'oddp) nil)
(test (mismatch '(1 2 3 4 5 6) '(3 4 5 6 7) :start1 2 :end2 4) nil)


