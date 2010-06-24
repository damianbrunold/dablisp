;;;; benchmarks

(defparameter *bench* nil)

(defmacro defbench (description benchmark)
  `(let ((s (make-string-output-stream)))
     (let ((*trace-output* s))
       (time ,benchmark))
     (format t ".")
     (push (format nil "~A ~A" ,description (string-trim '(#\Space #\Return #\Newline) (get-output-stream-string s))) *bench*)
     (close s)))

(defun showbenchmarks ()
  (dolist (b (reverse *bench*))
    (format t "~%~a" b))
  (values))

(format t "~%preparing...")

(defparameter %%list nil)
(dotimes (i 100000) (push i %%list))
(setq %%list (nreverse %%list))

(defparameter %%vec (make-array 100000))
(dotimes (i 100000) (setf (aref %%vec i) i))

(format t "~%running benchmarks...")

(defbench "list create" (let ((r nil)) (dotimes (i 100000) (push i r))))
(defbench "list length" (length %%list))
(defbench "list find first" (find 0 %%list)) 
(defbench "list find last" (find 99999 %%list)) 
(defbench "list reverse" (reverse %%list))
(defbench "list nreverse" (nreverse %%list))

(defbench "vector create" (let ((v (make-array 100000))) (dotimes (i 100000) (setf (aref v i) i))))
(defbench "vector find first" (find 0 %%vec))
(defbench "vector find last" (find 99999 %%vec))
(defbench "vector reverse" (reverse %%vec))
(defbench "vector nreverse" (nreverse %%vec))

(defbench "vector svref" (let ((v (make-array 10))) (dotimes (i 10000) (dotimes (j 10) (svref v j))))) 
(defbench "vector aref" (let ((v (make-array 10))) (dotimes (i 10000) (dotimes (j 10) (aref v j))))) 
(defbench "vector elt" (let ((v (make-array 10))) (dotimes (i 10000) (dotimes (j 10) (elt v j))))) 

(defbench "factorial" (dotimes (i 10000) (fact 20)))

(defparameter %%rnd (make-array 1000))
(defbench "randomize" (dotimes (i 1000) (setf (aref %%rnd i) (random 100000))))
(defbench "shellsort (random)" (shellsort %%rnd #'<))
(defbench "shellsort (sorted)" (shellsort %%rnd #'<))
(dotimes (i 1000) (setf (aref %%rnd i) (random 100000)))
(defbench "insertionsort (random)" (insertionsort %%rnd #'<))
(defbench "insertionsort (sorted)" (insertionsort %%rnd #'<))

(format t "~%results:")

(showbenchmarks)
