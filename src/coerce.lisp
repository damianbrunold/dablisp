;;;; common lisp system library

;;; coerce functions

(defun coerce-to-list (obj)
  (cond
   ((typep obj 'list) obj)
   ((typep obj 'vector) (vector-list obj))
   (t (throw 'type-error "cannot coerce to list")))) 

(defun coerce-to-vector (obj)
  (cond 
   ((typep obj 'vector) obj)
   ((typep obj 'list) (list-vector obj))
   (t (throw 'type-error "cannot coerce to vector"))))

(defun coerce-to-string (obj)
  (cond
   ((typep obj 'string) obj)
   ((typep obj 'list) (list-string obj))
   ((typep obj 'character) (list-string (list obj)))
   ((typep obj 'symbol) (symbol-name obj))
   ((typep obj 'integer) (princ-to-string obj))
   ((typep obj 'float) (princ-to-string obj))
      (t (throw 'type-error "cannot coerce to string"))))

(defun coerce-to-character (obj)
  (cond
   ((typep obj 'character) obj)
   ((typep obj 'integer) (code-char obj))
   ((typep obj 'string) (char obj 0))
   (t (throw 'type-error "cannot coerce to character"))))

(defun coerce-to-integer (obj)
  (cond
   ((typep obj 'integer) obj)
   ((typep obj 'float) (truncate obj))
   ((typep obj 'character) (char-code obj))
   ((typep obj 'string)
    (let ((i (with-input-from-string (s obj) (read s))))
      (if (typep i 'real)
          (values (truncate i))
          (throw 'type-error "cannot coerce to integer"))))
   (t (throw 'type-error "cannot coerce to integer"))))

(defun coerce-to-float (obj)  
  (cond
   ((typep obj 'float) obj)
   ((typep obj 'integer) (float obj))
   ((typep obj 'string)
    (let ((f (with-input-from-string (s obj) (read s))))
      (if (typep f 'real)
          (float f)
          (throw 'type-error "cannot coerce to float"))))
   (t (throw 'type-error "cannot coerce to float")))) 

(defun coerce-to-symbol (obj)
  (cond
   ((typep obj 'symbol) obj)
   ((typep obj 'string) (intern obj))
   (t (throw 'type-error "cannot coerce to symbol"))))
          
(defun coerce (obj type)
  (if (typep obj 'native)
      (if (eq type 'native)
          obj
          (coerce-from-native obj))
      (ecase type
             ((t) obj)
             ((nil) (throw 'type-error "cannot coerce to nil"))
             ((native) (coerce-to-native obj))
             ((list) (coerce-to-list obj))
             ((vector) (coerce-to-vector obj))
             ((string) (coerce-to-string obj))
             ((character) (coerce-to-character obj))
             ((integer) (coerce-to-integer obj))
             ((float) (coerce-to-float obj))
             ((symbol) (coerce-to-symbol obj)))))
