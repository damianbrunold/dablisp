;;;; common lisp system library

;;; constants and informational functions

(defconstant t 't)
(defconstant nil 'nil)
(defconstant pi 3.141592653589793)

(defconstant most-positive-fixnum 9223372036854775807)
(defconstant most-negative-fixnum -9223372036854775808)

(defconstant array-rank-limit 128)
(defconstant array-dimension-limit 2147483647)
(defconstant array-total-size-limit 2147483647)

(defun lisp-implementation-type () "dablisp")

(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cdar (x) (cdr (car x)))
(defun cddr (x) (cdr (cdr x)))

(defun caaar (x) (car (car (car x))))
(defun caadr (x) (car (car (cdr x))))
(defun cadar (x) (car (cdr (car x))))
(defun caddr (x) (car (cdr (cdr x))))
(defun cdaar (x) (cdr (car (car x))))
(defun cdadr (x) (cdr (car (cdr x))))
(defun cddar (x) (cdr (cdr (car x))))
(defun cdddr (x) (cdr (cdr (cdr x))))

(defun caaaar (x) (car (car (car (car x)))))
(defun caaadr (x) (car (car (car (cdr x)))))
(defun caadar (x) (car (car (cdr (car x)))))
(defun caaddr (x) (car (car (cdr (cdr x)))))
(defun cadaar (x) (car (cdr (car (car x)))))
(defun cadadr (x) (car (cdr (car (cdr x)))))
(defun caddar (x) (car (cdr (cdr (car x)))))
(defun cadddr (x) (car (cdr (cdr (cdr x)))))
(defun cdaaar (x) (cdr (car (car (car x)))))
(defun cdaadr (x) (cdr (car (car (cdr x)))))
(defun cdadar (x) (cdr (car (cdr (car x)))))
(defun cdaddr (x) (cdr (car (cdr (cdr x)))))
(defun cddaar (x) (cdr (cdr (car (car x)))))
(defun cddadr (x) (cdr (cdr (car (cdr x)))))
(defun cdddar (x) (cdr (cdr (cdr (car x)))))
(defun cddddr (x) (cdr (cdr (cdr (cdr x)))))

(defun first (x) (car x))
(defun rest (x) (cdr x))

(defun not (x) (if x nil t))
(defun null (x) (if x nil t))

(defun 1+ (n)
  "returns the increment of n" 
  (+ n 1))

(defun 1- (n) 
  "returns the decrement of n"
  (- n 1))

(defun nth (n list)
  (let ((i 0))
    (tagbody
     loop
     (if (null list) (go end))
     (if (= i n) (go end))
     (setq i (1+ i))
     (setq list (cdr list))
     (go loop)
     end)
    (if list (car list) nil)))

(defun nthcdr (n list)
  (let ((i 0))
    (tagbody
     loop
     (if (null list) (go end))
     (if (= i n) (go end))
     (setq i (1+ i))
     (setq list (cdr list))
     (go loop)
     end)
    list))

(defun second (x) (nth 1 x))
(defun third (x) (nth 2 x))
(defun fourth (x) (nth 3 x))
(defun fifth (x) (nth 4 x))
(defun sixth (x) (nth 5 x))
(defun seventh (x) (nth 6 x))
(defun eighth (x) (nth 7 x))
(defun nineth (x) (nth 8 x))
(defun tenth (x)  (nth 9 x))

(defun atom (x) (typep x 'atom))
(defun consp (x) (typep x 'cons))
(defun symbolp (x) (typep x 'symbol))
(defun keywordp (x) (typep x 'keyword))
(defun numberp (x) (typep x 'number))
(defun integerp (x) (typep x 'integer))
(defun floatp (x) (typep x 'float))
(defun realp (x) (typep x 'real))
(defun characterp (x) (typep x 'character))
(defun arrayp (x) (typep x 'array))
(defun vectorp (x) (typep x 'vector))
(defun stringp (x) (typep x 'string))
(defun functionp (x) (typep x 'function))
(defun streamp (x) (typep x 'stream))
(defun hash-table-p (x) (typep x 'hashtable))

(defun identity (x) x)

;;; no complex numbers, thus conjugate is a no-op
(defun conjugate (x) x)

(defun copy-list (list)
  (if (null list)
      nil
      (let ((result (cons (car list) nil)) 
            (last nil))
        (tagbody
         (setq last result)
         (setq list (cdr list))
         loop
         (if (null list) (go end))
         (rplacd last (cons (car list) nil))
         (setq last (cdr last))
         (setq list (cdr list))
         (go loop)
         end)
        result)))

(defun last (list)
  (if (null list)
      nil
      (let ((list list))
        (tagbody
         loop
         (if (null (cdr list)) (go end))
         (setq list (cdr list))
         (go loop)
         end)
        list)))

(defun append (&rest lists)
  (if (null lists)
      nil
      (let ((lists lists))
        ;; skip leading empty lists
        (tagbody
         loop
         (if (null lists) (go end))
         (if (car lists) (go end))
         (setq lists (cdr lists))
         (go loop)
         end)
        ;; append the lists
        (if (null lists)
            nil
            (let ((a (cdr lists))
                  (result (car lists))
                  (last (last (car lists))))
              (tagbody
               loop
               (if (null a) (go end))
               (if (null (car a)) (go cont))
               (if (null (cdr a))
                   (rplacd last (car a))
                   (let* ((c (copy-list (car a))) (newlast (last c)))
                     (rplacd last c)
                     (setq last newlast)))
               cont
               (setq a (cdr a))
               (go loop)
               end)
              result)))))

(defun list-length (list)
  (let ((result 0) (s list))
    (tagbody
     loop
     (if (null s) (go end))
     (setq result (1+ result))
     (setq s (cdr s))
     (go loop)
     end)
    result))

(defun list-reverse (x)
  (let ((s x) (result nil))
    (tagbody
     loop
     (if (eq s nil) (go end))
     (setq result (cons (car s) result))
     (setq s (cdr s))
     (go loop)
     end)
    result))

(defun revappend (list &optional tail)
  (append (list-reverse list) tail))

(defmacro return (&optional val)
  (list 'return-from nil val))

(defun error (val &rest args)
  "signals an error" 
  (throw 'error (apply #'format nil val args)))

(defmacro assert (test msg &rest args)
  (list 'if (list 'not test) (append (list 'error msg) args)))

(defmacro and (&rest args)
  "returns the last arg if all ARGS are true"
  (if args
      (if (cdr args)
          (list 'if (car args) (append (list 'and) (cdr args)) 'nil)
          (car args))
      t))
		
(defmacro or (&rest args)
  "returns the first non-nil argument or nil if all ARGS are nil"
  (if args
      (let ((c (gensym)))
        (list 'let (list (list c (car args)))
              (list 'if c
                    c
                    (append (list 'or) (cdr args)))))
      nil))

(defmacro cond (&rest clauses)
  (if clauses
      (list 'if (caar clauses)
            (if (> (list-length (cdar clauses)) 1)
                (append (list 'progn) (cdar clauses))
                (cadar clauses))
            (append (list 'cond) (cdr clauses)))
      nil))

(defmacro when (cond &body body)
  "evaluates BODY if COND is true"
  (list 'if cond (append (list 'progn) body)))

(defmacro unless (cond &body body)
  "evaluates BODY if CONS is false"
  (list 'if cond 'nil (append (list 'progn) body)))

(load "!backquote.lisp")

;;; finally, we got backquote syntax, thus much better macro defining capabilities

(load "!macros.lisp")
(load "!num.lisp")
(load "!string.lisp")
(load "!list.lisp")
(load "!seq.lisp")
(load "!various.lisp")
(load "!coerce.lisp")
