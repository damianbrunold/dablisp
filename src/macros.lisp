;;;; common lisp system library

;;; macros

(defmacro psetq (&rest rest)
  (let ((s nil) (list rest))
    (tagbody
     begin
     (if (null list) (go end))
     (setq s (cons (list (car list) (gensym) (cadr list)) s))
     (setq list (cddr list))
     (go begin)
     end)
    (let ((inits nil) (sets nil))
      (tagbody
       begin
       (if (null s) (go end))
       (setq inits (cons (list (cadar s) (caddar s)) inits))
       (setq sets (cons (caar s) (cons (cadar s) sets)))
       (setq s (cdr s))
       (go begin)
       end)
      `(let ,inits (setq ,@sets)))))

(defmacro do (clauses test &body body)
  (let ((begin (gensym)) (end (gensym)) (cl (gensym)))
    (if (null clauses)
        `(block nil
           (tagbody
            ,begin
            ,(if test `(if ,(car test) (go ,end)))  
            ,@body
            (go ,begin)
            ,end)
           ,@(if test (cdr test)))
        `(block nil
           (let ,(do-init clauses)
             (tagbody
              ,begin
              ,(if test `(if ,(car test) (go ,end)))  
              ,@body
              (psetq ,@(do-update clauses))
              (go ,begin)
              ,end)
             ,@(if test (cdr test)))))))

(defun do-init (clauses)
  (if clauses
      (cons
       (cond
        ((atom (car clauses)) 
         `(,(car clauses) nil))
        ((= (list-length (car clauses)) 1)
         `(,(caar clauses) nil))
        (t
         `(,(caar clauses) ,(cadar clauses))))
       (do-init (cdr clauses)))
      nil))
  
(defun do-update (clauses)
  (if clauses
      (if (or (atom (car clauses)) (< (list-length (car clauses)) 3))
          (do-update (cdr clauses))
          (cons (caar clauses) (cons (caddar clauses) (do-update (cdr clauses)))))
      nil))

(defmacro do* (clauses test &body body)
  (let ((begin (gensym)) (end (gensym)) (cl (gensym)))
    (if (null clauses)
        `(block nil
           (tagbody
            ,begin
            ,(if test `(if ,(car test) (go ,end)))  
            ,@body
            (go ,begin)
            ,end)
           ,@(if test (cdr test)))
        `(block nil
           (let* ,(do-init clauses)
             (tagbody
              ,begin
              ,(if test `(if ,(car test) (go ,end)))  
              ,@body
              (setq ,@(do-update clauses))
              (go ,begin)
              ,end)
             ,@(if test (cdr test)))))))

(defmacro setf (place value)
  (cond
   ((not (consp place)) `(setq ,place ,value))
   ((eq (car place) 'char) `(set-char ,(second place) ,(third place) ,value))
   ((eq (car place) 'svref) `(svset ,(second place) ,value ,(third place)))
   ((eq (car place) 'aref) `(aset ,(second place) ,value ,@(nthcdr 2 place)))
   ((eq (car place) 'gethash) `(sethash ,(second place) ,value ,(third place)))
   ((eq (car place) 'car) `(rplaca ,(second place) ,value))
   ((eq (car place) 'cadr) `(rplaca (cdr ,(second place)) ,value))
   ((eq (car place) 'cdr) `(rplacd ,(second place) ,value))
   ((eq (car place) 'fill-pointer) `(set-fill-pointer ,(second place) ,value))
   ((eq (car place) 'nth)
    (let ((idx (second place)) (seq (third place)))
      `(rplaca (nthcdr ,idx ,seq) ,value)))
   ((eq (car place) 'subseq)
    (let ((seq1 (second place)) (start1 (third place)) (end1 (if (>= (length place) 4) (fourth place) nil)))  
      `(progn (replace ,seq1 ,value :start1 ,start1 :end1 ,end1 :start2 0) ,value)))
   ((eq (car place) 'elt)
    (let ((idx (third place)) (seq (second place)))
      (let ((s (gensym)) (i (gensym)) (v (gensym)))
        `(let ((,s ,seq) (,i ,idx) (,v ,value)) 
           (if (consp ,s)
             (rplaca (nthcdr ,i ,s) ,v)
             (aset ,s ,v ,i))))))
   (t (error "non-setfable place ~A" place))))

(defmacro incf (place &optional (delta 1))
  (let ((p (gensym)) (r (gensym)))
    `(let* ((,p ,place) (,r (+ ,p ,delta)))
       ,(list 'setf place r))))

(defmacro decf (place &optional (delta 1))
  (let ((p (gensym)) (r (gensym)))
    `(let* ((,p ,place) (,r (- ,p ,delta)))
       ,(list 'setf place r))))
  
(defmacro case (keyform &rest clauses)
  (let ((kf (gensym)))
    (cond
     ((null clauses) nil)
     ((consp (caar clauses))
      `(let ((,kf ,keyform))
         (if (member ,kf ',(caar clauses)) 
             (progn ,@(cdar clauses))
             (case ,kf ,@(cdr clauses)))))
     ((or (eq t (caar clauses)) (eq 'otherwise (caar clauses)))
      `(progn ,@(cdar clauses)))
     (t nil))))

(defmacro ecase (keyform &rest clauses)
  (let ((kf (gensym)))
    (cond
     ((null clauses) `(throw 'type-condition "no matching clause in ecase"))
     ((consp (caar clauses))
      `(let ((,kf ,keyform))
         (if (member ,kf ',(caar clauses)) 
             (progn ,@(cdar clauses))
             (ecase ,kf ,@(cdr clauses)))))
     (t (throw 'syntax-condition (caar clauses))))))

(defmacro prog1 (arg1 &rest args)
  "evaluates all forms and returns the value of the first one"
  (let ((r (gensym)))
    `(let ((,r ,arg1))
       ,@args
       ,r)))

(defmacro prog2 (arg1 arg2 &rest args)
  "evaluates all forms and returns the value of the second one"
    (let ((r (gensym)))
    `(let ((,r (progn ,arg1 ,arg2)))
       ,@args
       ,r)))

(defmacro pushnew (obj list)
  (let ((o (gensym)) (lst (gensym)))
    `(let* ((,o ,obj) (,lst (adjoin ,o ,list)))
       ,(list 'setf list lst))))

(defmacro push (obj list)
  (let ((o (gensym)) (lst (gensym)))
    `(let* ((,o ,obj) (,lst (cons ,o ,list)))
       ,(list 'setf list lst))))

(defmacro pop (list)
  (let ((lst (gensym)))
    `(let ((,lst ,list))
       (prog1 
        (car ,lst)
        ,(list 'setf list (list 'cdr lst))))))

(defmacro dotimes ((var val &optional res) &body body)
  (if (not (symbolp var)) (error "do var must be a symbol"))
  (let ((begin (gensym)) (end (gensym)) (v (gensym)))
    `(block nil
       (let ((,var 0) (,v ,val))
         (tagbody
          ,begin
          (if (= ,var ,v) (go ,end))
          ,@body
          (setq ,var (1+ ,var))
          (go ,begin)
          ,end)
         ,res))))

(defmacro dolist ((var val) &body body)
  (let ((ls (gensym)))
    `(do ((,ls ,val (cdr ,ls)))
        ((null ,ls) nil)
        (let ((,var (car ,ls)))
          ,@body))))

(defmacro loop (&body body)
  "evaluates BODY in an endless loop"
  `(do () () ,@body))

(defmacro time (&body body)
  "performs body and writes timing statistics to *TRACE-OUTPUT*"
  (let ((start (gensym)))
    `(let* ((,start (get-internal-real-time)) (result (progn ,@body)))
       (format *trace-output* "~%time: ~D ms" (- (get-internal-real-time) ,start))
       result)))

(defmacro trace (&rest fn)
  (if (null fn)
      `(traces 'show)
      `(traces 'add '(,@fn))))

(defmacro untrace (&rest fn)
  (if (null fn)
      `(traces 'removeall)
      `(traces 'remove '(,@fn))))

(defmacro with-output-to-string ((strm &optional str) &rest body)
  (if str
      `(let ((,strm (make-fill-pointer-output-stream ,str)))
         (unwind-protect
           (progn ,@body)
           (close ,strm)))
      `(let ((,strm (make-string-output-stream)))
         (unwind-protect
           (progn ,@body)
           (close ,strm))
         (get-output-stream-string ,strm))))

(defmacro with-input-from-string ((var string &key (start 0) end) &rest body)
  `(let ((,var (make-string-input-stream ,string)))
     (unwind-protect
       (progn ,@body)
       (close ,var))))

(defmacro require (module)
  "loads a system MODULE"
  `(load (concatenate 'string "!" (string ,module) ".lisp")))
