;;;; common lisp system library

;;; various functions

(defun make-hash-table (&key (test #'eql))
  "creates a new hash table"
  (%make-hash-table test)) 

(defun warn (val &rest args)
  "writes the warning VAL to *ERROR-OUTPUT*" 
  (apply #'format *error-output* val args))

(defun complement (fn) 
  #'(lambda (&rest arguments) 
      (not (apply fn arguments))))

(defun equal (a b)
  "returns true if A and B are structurally equal"
  (cond
   ((eq a b) t)
   ((eql a b) t)
   ((and (consp a) (consp b)) (and (equal (car a) (car b)) (equal (cdr a) (cdr b))))
   (t nil)))

(defun equalp (a b)
  "returns true if A and B are structurally equal"
  (cond
   ((eq a b) t)
   ((eql a b) t)
   ((equal a b) t)
   ((and (vectorp a) (vectorp b) (= (length a) (length b)))
    (do ((i 0 (1+ i)))
        ((= i (length a)) t)
      (unless (equalp (aref a i) (aref b i)) (return nil))))
   ((and (consp a) (consp b)) (and (equalp (car a) (car b)) (equalp (cdr a) (cdr b))))
   (t nil)))

(defun write (object &key (stream *standard-output*) (escape t))
  "writes OBJECT to STREAM or *STANDARD-OUTPUT*"
  (if escape
      (prin1 object stream)
      (princ object stream)))
  
(defun write-char (ch &optional (stream *standard-output*))
  "writes the character CH to STREAM or *STANDARD-OUTPUT*"
  (if (not (characterp ch)) (error "argument ~A must be of type character" ch))
  (write ch :stream stream :escape nil))
  
(defun write-to-string (obj)
  "returns the result of calling WRITE with OBJ as a string"
  (with-output-to-string (s) (write obj :stream s)))
                         
(defun princ-to-string (obj)
  "returns the result of calling PRINC with OBJ as a string"
  (with-output-to-string (s) (princ obj s)))

(defun prin1-to-string (obj)
  "returns the result of calling PRIN1 with OBJ as a string"
  (with-output-to-string (s) (prin1 obj s)))

(defun values-list (list)
  "returns the elements of LIST as multiple values"
  (apply #'values list))

(defun make-array (dimensions &key (element-type t) initial-element fill-pointer)
  "creates an array"
  (%make-array dimensions element-type initial-element fill-pointer)) 

(defun array-dimensions (array)
  "returns a list containing the dimensions of ARRAY"
  (let ((r nil))
    (dotimes (i (array-rank array))
      (push (array-dimension array i) r))
    (nreverse r)))

(defun array-total-size (array)
  "returns the total size of the ARRAY"
  (apply #'* (array-dimensions x)))

(defmacro multiple-value-list (form)
  `(multiple-value-call #'list ,form))

(defmacro multiple-value-bind ((&rest vars) values-form &body body)
  `(multiple-value-call #'(lambda (&optional ,@vars)
                            ,@body)
                        ,values-form)) 

