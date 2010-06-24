;;;; list unit tests

(format t "~%testlist...")

(test (listp '(1)) t)
(test (listp nil) t)
(test (listp 'a) nil)
(test (listp #(1 2 3)) nil)
(test (listp (cons 1 2)) t)

(test (assoc 1 '((1 . a) (2 . b))) '(1 . a))
(test (assoc 2 '((1 . a) (2 . b)) ) '(2 . b))
(test (assoc "b" '((("a" 1) . a) (("b" 2) . b)) :key #'car :test #'equal) '(("b" 2) . b))
(test (assoc 2 '((("a" 1) . a) (("b" 2) . b)) :key #'second) '(("b" 2) . b))
(test (assoc 3 '((1 . a) (2 . b))) nil)

(test (assoc-if #'oddp '((2 . a) (3 . b) (4 . c))) '(3 . b))

(test (assoc-if-not #'evenp '((2 . a) (3 . b) (4 . c))) '(3 . b))

(test (rassoc 'a '((1 . a) (2 . b))) '(1 . a))
(test (rassoc 'b '((1 . a) (2 . b))) '(2 . b))
(test (rassoc 'c '((1 . a) (2 . b))) nil)

(test (rassoc-if #'(lambda (x) (eq x 'b)) '((2 . a) (3 . b) (4 . c))) '(3 . b))

(test (rassoc-if-not #'(lambda (x) (eq x 'b)) '((2 . a) (3 . b) (4 . c))) '(2 . a))

(test (member 2 '(1 2 3)) '(2 3))
(test (member 4 '(1 2 3)) nil)
(test (member 1 '((1 . a) (2 . b)) :key #'car) '((1 . a) (2 . b)))
(test (member 4 '((1 . a) (2 . b)) :key #'car) nil)

(test (member-if #'oddp '(2 3 4)) '(3 4))
(test (member-if #'oddp '(2 4)) nil)

(test (adjoin 1 nil) '(1))
(test (adjoin 1 '(1 2)) '(1 2))
(test (adjoin 1 '(2 3)) '(1 2 3))

(test (union '(1 2) '(2 3)) '(3 1 2))
(test (union '(1 2) '(2)) '(1 2))

(test (nunion '(1 2) '(2 3)) '(3 1 2))
(test (nunion '(1 2) '(2)) '(1 2))

(test (intersection '(1 2) '(2 3)) '(2))
(test (intersection '(1 2) '(1 2)) '(2 1))
(test (intersection '(1 2) '(3 4)) nil)

(test (nintersection '(1 2) '(2 3)) '(2))
(test (nintersection '(1 2) '(1 2)) '(2 1))
(test (nintersection '(1 2) '(3 4)) nil)

(test (set-difference '(1 2 3) '(2 3 4)) '(1))
(test (set-difference '(1 2 3) '(4 5 6)) '(3 2 1))
(test (set-difference '(1 2 3) '(1 2 3)) nil)

(test (nset-difference '(1 2 3) '(2 3 4)) '(1))
(test (nset-difference '(1 2 3) '(4 5 6)) '(3 2 1))
(test (nset-difference '(1 2 3) '(1 2 3)) nil)

(test (list-length '()) 0)
(test (list-length '(a)) 1)
(test (list-length '(a b)) 2)

(test (equal '(1 2 3) (copy-list '(1 2 3))) t)
(test (let* ((a '(1 2 3)) (b (copy-list a))) (rplaca b 9) (list a b)) '((1 2 3) (9 2 3)))

(test (last nil) nil)
(test (last '(1)) '(1))
(test (last '(1 2 3)) '(3))

(test (append '(1 2) '(3 4)) '(1 2 3 4))
(test (append '(1 2) '() '(3 4)) '(1 2 3 4))

(test (cars '((2 3 4) (2) (2 3))) '(2 2 2))

(test (cdrs '((2 3 4) (2 3) (2 3))) '((3 4) (3) (3)))
(test (cdrs '((2 3 4) (2) (2 3))) nil)

(test (mapcar #'abs '(1 -2 3 -4)) '(1 2 3 4))
(test (mapcar #'+ '(1 2 3) '(4 5 6)) '(5 7 9))
(test (mapcar #'+ '(1 2 3 4) '(4 5 6)) '(5 7 9))

(test (maplist #'(lambda (x) (cons 'foo x)) '(a b c d)) '((foo a b c d) (foo b c d) (foo c d) (foo d)))
;TODO(test (maplist #'append '(1 2 3 4) '(1 2) '(1 2 3)) '((1 2 3 4 1 2 1 2 3) (2 3 4 2 2 3)))

(test (nconc '(1 2) '(3 4) '(5) '(6 7 8)) '(1 2 3 4 5 6 7 8))
(test (nconc '(1 2) 3) '(1 2 . 3))
(test (let ((x '(1 2))) (nconc x '(3 4)) x) '(1 2 3 4))
(test (nconc) nil)
(test (nconc '(1 2)) '(1 2))
(test (nconc '(1 2) nil '(3 4)) '(1 2 3 4))
(test (nconc '(1 2) nil nil nil nil '(3 4) nil) '(1 2 3 4))
(test (nconc nil '(1 2) nil '(3 4)) '(1 2 3 4))

(test (mapcan #'list '(1 2 3)) '(1 2 3))
(test (mapcan #'(lambda (x) (and (numberp x) (list x))) '(a 1 b c 3 4 d 5)) '(1 3 4 5))

;TODO(test (mapcon #'append '(1 2 3 4) '(1 2) '(1 2 3)) '(1 2 3 4 1 2 1 2 3 2 3 4 2 2 3))

(test (mapc #'abs '(1 -2 3)) '(1 -2 3))

;TODO(test (mapl #'append '(1 2 3) '(4 5 6)) '(1 2 3))

(test (copy-alist '((1 . a) (2 . b))) '((1 . a) (2 . b)))

(test (copy-tree '((1 2) (3 4))) '((1 2) (3 4)))
(test (let* ((x '((1 2) (3 4))) (y (copy-tree x))) (setf (car (car x)) 5) (list x y)) '(((5 2) (3 4)) ((1 2) (3 4))))

(test (endp nil) t)
(test (endp '(1)) nil)

(test (acons 1 'a nil) '((1 . a)))
(test (acons 1 'a '((2 . b))) '((1 . a) (2 . b)))

(test (pairlis '(1 2) '(a b)) '((2 . b) (1 . a)))
(test (pairlis '(1 2) '(a b) '((3 . c))) '((2 . b) (1 . a) (3 . c)))

(test (sublis '((1 . a) (2 . b)) '((1 2 3) ((4 1 5) 6 2))) '((a b 3) ((4 a 5) 6 b)))
(test (sublis '((1 . a) (2 . b)) '((1 2 3) ((4 1 5) 6 2)) :key nil) '((a b 3) ((4 a 5) 6 b)))

(test (nsublis '((1 . a) (2 . b)) '((1 2 3) ((4 1 5) 6 2))) '((a b 3) ((4 a 5) 6 b)))

(test (tree-equal '(1 2 (3 4) 5) '(1 2 (3 4) 5)) t)
(test (tree-equal '(1 2 (3 4) 5) '(1 2 (3 x) 5)) nil)
(test (tree-equal '(1 2 (3 4) 5) '(1 2 5)) nil)

(test (list* 1 2 3) '(1 2 . 3))
(test (list* 1) 1)
(test (list* 1 nil 2) '(1 nil . 2))
(test (list* 1 2 nil) '(1 2 . nil))

(test (nreconc '(3 2 1) '(4 5)) '(1 2 3 4 5))

(test (let ((a '(1 2 3))) (tailp a a)) t)
(test (let ((a '(1 2 3))) (tailp (cdr a) a)) t)
(test (let ((a '(1 2 3))) (tailp (cddr a) a)) t)
(test (let ((a '(1 2 3))) (tailp '(x y) a)) nil)

(test (let ((a '(1 2 3))) (ldiff a (cdr a))) '(1))

