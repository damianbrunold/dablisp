;;;; base unit tests

(format t "~%testbase...")

(test (catch 'error (error "test ~s" 1)) "test 1")

(test (caar '((1 2) 3)) 1)
(test (cadr '(1 2 3)) 2)
(test (cdar '((1 2) 3)) '(2))
(test (cddr '(1 2 3)) '(3))
(test (caaar '(((1 2) 3) 4 5)) 1)
(test (caadr '(1 (2 3) 4)) 2)
(test (cadar '((1 2) 3)) 2)
(test (caddr '(1 2 3)) 3)
(test (cdaar '(((1 2) 3) 4)) '(2))
(test (cdadr '(1 (2 3))) '(3))
(test (cddar '((1 2 3) 4)) '(3))
(test (cdddr '(1 2 3 4)) '(4))
                        
(test (nth 0 '(1 2 3)) 1)
(test (nth 1 '(1 2 3)) 2)
(test (nth 2 '(1 2 3)) 3)

(test (nthcdr 0 '(1 2 3)) '(1 2 3))
(test (nthcdr 1 '(1 2 3)) '(2 3))
(test (nthcdr 2 '(1 2 3)) '(3))

(test (first '(1 2 3 4 5 6 7 8 9 0)) 1)
(test (second '(1 2 3 4 5 6 7 8 9 0)) 2)
(test (third '(1 2 3 4 5 6 7 8 9 0)) 3)
(test (fourth '(1 2 3 4 5 6 7 8 9 0)) 4)
(test (fifth '(1 2 3 4 5 6 7 8 9 0)) 5)
(test (sixth '(1 2 3 4 5 6 7 8 9 0)) 6)
(test (seventh '(1 2 3 4 5 6 7 8 9 0)) 7)
(test (eighth '(1 2 3 4 5 6 7 8 9 0)) 8)
(test (nineth '(1 2 3 4 5 6 7 8 9 0)) 9)
(test (tenth '(1 2 3 4 5 6 7 8 9 0)) 0)

(test (rest '(1 2 3)) '(2 3))

(test (not nil) t)
(test (not 1) nil)

(test (null nil) t)
(test (null 1) nil)

(test (atom 'a) t)
(test (atom 1) t)
(test (atom *standard-output*) t)
(test (atom "a") t)
(test (atom #\a) t)
(test (atom '(1)) nil)
(test (atom nil) t)

(test (consp '(1)) t)
(test (consp nil) nil)
(test (consp 'a) nil)

(test (symbolp 'a) t)
(test (symbolp ':a) t)
(test (symbolp 1) nil)

(test (keywordp 'a) nil)
(test (keywordp ':a) t)

(test (numberp 1) t)
(test (numberp 1.0) t)
(test (numberp 'a) nil)

(test (integerp 1) t)
(test (integerp 1.0) nil)
(test (integerp 'a) nil)

(test (floatp 1) nil)
(test (floatp 1.0) t)
(test (floatp 'a) nil)

(test (realp 1) t)
(test (realp 1.0) t)
(test (realp 'a) nil)

(test (characterp #\a) t)
(test (characterp 'a) nil)

(test (arrayp "a") t)
(test (arrayp #(1)) t)
(test (arrayp '(1)) nil)

(test (vectorp "a") t)
(test (vectorp #(1)) t)
(test (vectorp '(1)) nil)

(test (stringp "a") t)
(test (stringp #\a) nil)
(test (stringp 'a) nil)

(test (functionp #'+) t)
(test (functionp #'oddp) t)
(test (functionp #'(lambda (x) x)) t)

(test (streamp *standard-output*) t)
(test (streamp *error-output*) t)
(test (streamp *standard-input*) t)

(test (identity 5) 5)

(test (conjugate 5) 5)

(test (append '(a b c) '(d e f) '() '(g)) '(a b c d e f g))

(test (revappend '(3 2 1) '(4 5)) '(1 2 3 4 5))

