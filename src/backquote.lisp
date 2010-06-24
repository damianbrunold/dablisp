;;;; common lisp system library

;; adapted from CLtL2

;; simple test: 
;; (backquote (a ,(+ 1 2) b ,@(list 1 2) c)) 
;; => (a 3 b 1 2 c)

(defmacro backquote (x)
  (bq-process x))

(defun bq-process (x)
  (cond
   ((null x) nil)
   ((atom x) (list 'quote x))
   ((eq (car x) 'backquote)
    (bq-process (bq-process (cadr x))))
   ((eq (car x) 'comma)
    (cadr x))
   ((eq (car x) 'comma-atsign)
    (error ",@~S after `" (cadr x)))
   ((eq (car x) 'comma-dot)
    (error ",.~S after `" (cadr x)))
   (t (bq-process2 x))))

(defun bq-process2 (x)
  (unless x nil)
  (let ((p x) (q nil))
    (tagbody
     loop
     (cond 
      ((null p) (return-from bq-process2 (cons 'append (revappend q))))
      ((atom p) (return-from bq-process2 (cons 'append (revappend q (list (list 'quote p))))))
      ((eq (car p) 'comma)
       (unless (null cddr p) (bq-error "Malformed ,~S" p))
       (return-from bq-process2 (cons 'append (revappend q (list (cadr p))))))
      ((eq (car p) 'comma-atsign)
       (bq-error "Dotted ,@~S" p))
      ((eq (car p) 'comma-dot)
       (bq-error "Dotted ,.~S" p)))
     (setq q (cons (bracket (car p)) q))
     (setq p (cdr p))
     (go loop))))

(defun bracket (x)
  (cond
   ((atom x) 
    (list 'list (bq-process x)))
   ((eq (car x) 'comma) 
    (list 'list (cadr x)))
   ((eq (car x) 'comma-atsign) 
    (cadr x))
   ((eq (car x) 'comma-dot)
    (list 'identity (cadr x)))
   (t (list 'list (bq-process x)))))
