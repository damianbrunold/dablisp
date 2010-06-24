;;;; common lisp system library

;;; constants and informational functions

;(defconstant t 't)
;(defconstant nil 'nil)
(defconstant pi 3.141592653589793)

(defconstant most-positive-fixnum 9223372036854775807)
(defconstant most-negative-fixnum -9223372036854775808)

(defconstant array-rank-limit 128)
(defconstant array-dimension-limit 2147483647)
(defconstant array-total-size-limit 2147483647)

(defun lisp-implementation-type () "dablisp")

