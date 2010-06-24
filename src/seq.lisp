;;;; common lisp system library

;;; sequence functions

(defun list-find (item list &key (from-end nil) (test #'eql) (start 0) (end nil) (key #'identity))
  (do ((pos 0 (1+ pos))
       (r nil)
       (a list (cdr a)))
      ((if end (= pos end) (null a)) r)
    (if (and (>= pos start) (funcall test item (funcall key (car a))))
        (if from-end
            (setq r (car a))
            (return (car a))))))

(defun vector-find (item vector &key (from-end nil) (test #'eql) (start 0) (end nil) (key #'identity))
  (do ((end (or end (length vector)))
       (pos start (1+ pos)) (r nil))
      ((= pos end) r)
    (if (funcall test item (funcall key (aref vector pos)))
        (if from-end
            (setq r (aref vector pos))
            (return (aref vector pos))))))
    
(defun find (item sequence &key (from-end nil) (test #'eql) (start 0) (end nil) (key #'identity))
  "finds the ITEM in the SEQUENCE"
  (if (listp sequence)
      (list-find item sequence :from-end from-end :test test :start start :end end :key key)
      (vector-find item sequence :from-end from-end :test test :start start :end end :key key)))

(defun list-find-if (predicate sequence &key (from-end nil) (start 0) (end nil) (key #'identity))
  (do ((pos 0 (1+ pos))
       (r nil)
       (a sequence (cdr a)))
      ((if end (= pos end) (null a)) r)
    (if (and (>= pos start) (funcall predicate (funcall key (car a))))
        (if from-end
            (setq r (car a))
            (return (car a))))))
  
(defun vector-find-if (predicate sequence &key (from-end nil) (start 0) (end nil) (key #'identity))
  (do ((end (or end (length sequence)))
       (pos start (1+ pos)) (r nil))
      ((= pos end) r)
    (let ((s (elt sequence pos)))
      (if (funcall predicate (funcall key s))
          (if from-end
              (setq r s)
              (return s))))))
  
(defun find-if (predicate sequence &key (from-end nil) (start 0) (end nil) (key #'identity))
  "finds an item in SEQUENCE matching the PREDICATE"
  (if (listp sequence)
      (list-find-if predicate sequence :from-end from-end :start start :end end :key key)
      (vector-find-if predicate sequence :from-end from-end :start start :end end :key key)))

(defun list-position (item sequence &key (from-end nil) (test #'eql) (start 0) (end nil) (key #'identity))
  (do ((pos 0 (1+ pos))
       (r nil)
       (a sequence (cdr a)))
      ((if end (= pos end) (null a)) r)
    (if (and (>= pos start) (funcall test item (funcall key (car a))))
        (if from-end
            (setq r pos)
            (return pos)))))
  
(defun vector-position (item sequence &key (from-end nil) (test #'eql) (start 0) (end nil) (key #'identity))
  (do ((end (or end (length sequence)))
       (pos start (1+ pos)) 
       (r nil))
      ((= pos end) r)
    (let ((s (elt sequence pos)))
      (if (funcall test item (funcall key s))
          (if from-end
              (setq r pos)
              (return pos))))))
      
(defun position (item sequence &key (from-end nil) (test #'eql) (start 0) (end nil) (key #'identity))
  "returns the position of ITEM in the SEQUENCE or nil"
  (if (listp sequence)
      (list-position item sequence :from-end from-end :test test :start start :end end :key key)
      (vector-position item sequence :from-end from-end :test test :start start :end end :key key)))

(defun list-position-if (predicate sequence &key (from-end nil) (start 0) (end nil) (key #'identity))
  (do ((pos 0 (1+ pos))
       (r nil)
       (a sequence (cdr a)))
      ((if end (= pos end) (null a)) r)
    (if (and (>= pos start) (funcall predicate (funcall key (car a))))
        (if from-end
            (setq r pos)
            (return pos)))))
  
(defun vector-position-if (predicate sequence &key (from-end nil) (start 0) (end nil) (key #'identity))
  (do ((end (or end (length sequence)))
       (pos start (1+ pos)) (r nil))
      ((= pos end) r)
    (let ((s (elt sequence pos)))
      (if (funcall predicate (funcall key s))
          (if from-end
              (setq r pos)
              (return pos))))))
  
(defun position-if (predicate sequence &key (from-end nil) (start 0) (end nil) (key #'identity))
  "returns the position of the first item of SEQUENCE for which PREDICATE is true"
  (if (listp sequence)
      (list-position-if predicate sequence :from-end from-end :start start :end end :key key)
      (vector-position-if predicate sequence :from-end from-end :start start :end end :key key)))

(defun svref (vector idx)
  "returns the element IDX of VECTOR"
  (unless (typep vector 'simple-vector) (throw 'type-condition "svref needs a simple vector"))
  (aref vector idx))

(defun svset (vector val idx)
  "sets the element IDX of VECTOR to VAL"
  (unless (typep vector 'simple-vector) (throw 'type-condition "svset needs a simple vector"))
  (aset vector val idx))

(defun vector-list (v)
  "converts the general vector to a list" 
  (do ((i 0 (1+ i))
       (len (length v))
       (r nil))
      ((= i len) (nreverse r))
    (push (aref v i) r)))

(defun list-vector (list &key (element-type t))
  "converts the list to a general vector"
  (let* ((len (length list)) (s (make-array len :element-type element-type)))
    (do ((i 0 (1+ i))
         (a list (cdr a)))
        ((= i len) s)
      (setf (aref s i) (car a)))))

(defun string-list (s)
  "converts the string to a list of characters"
  (vector-list s))

(defun list-string (list)
  "converts the list of characters to a string"
  (list-vector list :element-type 'char))

(defun elt (seq n)
  "returns the nth element of SEQ" 
  (if (listp seq)
      (nth n seq)
      (if (array-has-fill-pointer-p seq)
          (if (< n (fill-pointer seq))
              (aref seq n)
              (throw 'index-too-large nil))
          (aref seq n))))

(defun vector-length (vector)
  "returns the length of the VECTOR"
  (if (array-has-fill-pointer-p vector)
      (fill-pointer vector)
      (array-dimension vector 0)))   
   	
(defun length (seq)
  "returns the length of SEQ"
  (if (listp seq)
      (list-length seq)
      (vector-length seq)))
	
(defun copy-vector (v)
  "makes a shallow copy of the vector V"
  (let ((w (make-array (vector-length v) :element-type (array-element-type v))))
    (do ((i 0 (1+ i)) (len (length v)))
        ((= i len) w)
      (setf (aref w i) (aref v i)))))

(defun copy-seq (seq)
  "makes a shallow copy of SEQ"
  (if (listp seq) 
      (copy-list seq)
      (copy-vector seq)))

(defun reverse (seq)
  "reverses SEQ"
  (labels ((listrev (list)
             (do ((a list (cdr a))
                  (b nil (cons (car a) b)))
                 ((null a) b)))
           (vecrev (v)
             (do ((len (length v))
                  (w (make-array (length v) :element-type (array-element-type v)))
                  (i 0 (1+ i))
                  (j (1- (length v)) (1- j)))
                 ((= i len) w)
               (setf (aref w i) (aref v j)))))
    (if (listp seq)
      (listrev seq)
      (vecrev seq))))

(defun nreverse (seq)
  "reverses SEQ destructively"
  (labels ((listnrev (list)
             (do ((curr list next)
                  (last nil)
                  (next nil))
                 ((null curr) last)
               (setq next (cdr curr))
               (rplacd curr last)
               (setq last curr)))
           (vecnrev (v)
             (let* ((len (length v)) (cutoff (floor (/ len 2))))
               (do ((i 0 (1+ i))
                    (j (1- len) (1- j)))
                   ((>= i cutoff) v)
                 (let ((tmp (aref v i)))
                   (setf (aref v i) (aref v j))
                   (setf (aref v j) tmp))))))
    (if (listp seq)
        (listnrev seq)
        (vecnrev seq))))

(defun vector-concatenate (element-type &rest vectors)
  "concatenates all VECTORS and returns a new vector consisting of ELEMENT-TYPE"
  (labels ((vlen (vectors)
             (if (null vectors)
                 0
                 (+ (length (car vectors)) (vlen (cdr vectors)))))
           (conc (v w idx)
             (do ((i idx (1+ i)) (len (length w)))
                 ((= (- i idx) len) v)
               (setf (aref v i) (aref w (- i idx))))))
    (let ((r (make-array (vlen vectors) :element-type element-type)))
      (do ((a vectors (cdr a))
           (i 0 (+ i (length (car a)))))
          ((null a) r)
        (conc r (car a) i))))) 

(defun concatenate (type &rest seqs)
  "concatenates all SEQS and returns a sequence of type TYPE"
  (cond
   ((eq type 'string) (apply #'vector-concatenate 'char seqs))
   ((eq type 'vector) (apply #'vector-concatenate 't seqs))
   ((eq type 'list) (apply #'append seqs))
   (t (error "cannot concatenate objs of type ~A" type))))

(defun elts (index &rest seqs)
  "returns a list containing all INDEX-th elements of SEQS"
  (let ((len (length seqs)))
    (do ((s seqs (cdr s))
         (r nil))
        ((null s) (nreverse r))
      (when (>= index (length (car s))) (return nil))
      (push (elt (car s) index) r))))

(defun every (predicate &rest seqs)
  "returns true if PREDICATE is true for all successive elements of SEQS"
  (labels ((everyvec ()
             (do* ((i 0 (1+ i))
                   (s (apply #'elts i seqs) (apply #'elts i seqs)))
                  ((null s) t)
                  (if (not (apply predicate s)) (return nil))))
           (everylist ()
             (do ((a seqs (cdrs a)))
                 ((null a) t)
               (let ((c (cars a)))
                 (unless c (return t))
                 (if (not (apply predicate (cars a))) (return nil))))))
  (if (and seqs (vectorp (car seqs)))
      (everyvec)
      (everylist))))

(defun some (predicate &rest seqs)
  "returns true if PREDICATE is true for some successive elements of SEQS"
  (labels ((somevec ()
             (do* ((i 0 (1+ i))
                   (s (apply #'elts i seqs) (apply #'elts i seqs)))
                  ((null s) nil)
                  (let ((r (apply predicate s)))
                    (if r (return r)))))
           (somelist ()
             (do ((a seqs (cdrs a)))
                 ((null a) nil)
               (let ((c (cars a)))
                 (unless c (return nil))
                 (let ((r (apply predicate (cars a))))
                   (if r (return r)))))))
    (if (and seqs (vectorp (car seqs)))
        (somevec)
        (somelist))))

(defun list-remove-duplicates (list &key from-end (test #'eql) (start 0) end (key #'identity))
  (do ((s list (cdr s))
       (i 0 (1+ i))
       (tmp nil)
       (r nil))
      ((null s) (nreverse r))
    (if (or (and start (< i start)) (and end (>= i end)))
        (push (car s) r)
        (if from-end
            (unless (member (funcall key (car s)) tmp :key key :test test)
              (push (car s) r)
              (push (car s) tmp)) 
            (if (null (cdr s))
                (push (car s) r)
                (unless (member (funcall key (car s)) (cdr s) :key key :test test)
                  (push (car s) r)))))))
    
(defun vector-remove-duplicates (sequence &key from-end (test #'eql) (start 0) end (key #'identity))
  (let ((r (make-array (length sequence) :element-type (array-element-type sequence))) (i 0) (j 0) (len (length sequence)))
    (do ()
        ((= i start))
      (setf (aref r j) (aref sequence i))
      (incf i) (incf j))
    (do ()
        ((= i end))
      (if from-end
          (unless (position (funcall key (aref sequence i)) r :start start :end j :test test :key key)
            (setf (aref r j) (aref sequence i))
            (incf j))
          (unless (position (funcall key (aref sequence i)) sequence :start (1+ i) :end end :test test :key key)
            (setf (aref r j) (aref sequence i))
            (incf j)))
      (incf i))
    (do ()
        ((= i len))
      (setf (aref r j) (aref sequence i))
      (incf i) (incf j))
    (subseq r 0 j))) 
  
(defun remove-duplicates (sequence &key from-end (test #'eql) (start 0) end (key #'identity))
  "returns a copy of SEQUENCE with duplicated elements removed"
  (if (listp sequence)
      (list-remove-duplicates sequence :from-end from-end :test test :start start :end end :key key)
      (vector-remove-duplicates sequence :from-end from-end :test test :start start :end (or end (length sequence)) :key key)))

(defun delete-duplicates (sequence &key from-end (test #'eql) (start 0) end (key #'identity))
  "removes duplicated elements destructively from SEQUENCE"
  (remove-duplicates sequence :from-end from-end :test test :start start :end end :key key))

(defun list-remove (item sequence &key from-end (test #'eql) (start 0) end count (key #'identity))
  (do ((s sequence (cdr s))
       (c 0)
       (i 0 (1+ i))
       (r nil))
      ((null s) (nreverse r))
    (if (or (and count (>= c count)) (and start (< i start)) (and end (>= i end)))
        (push (car s) r)
        (if (funcall test item (funcall key (car s)))
            (incf c)
            (push (car s) r)))))
  
(defun vector-remove (item sequence &key from-end (test #'eql) (start 0) end count (key #'identity))
  (let ((end (or end (length sequence))) (len (length sequence)))
    (if from-end
        (do ((r (make-array len :element-type (array-element-type sequence)))
             (i (1- len) (1- i))
             (j (1- len))
             (c 0))
            ((< i 0) (subseq r (1+ j) len))
          (when (>= i end)
            (setf (aref r j) (aref sequence i)) (decf j))
          (when (< i start)
            (setf (aref r j) (aref sequence i)) (decf j))
          (if (or (not count) (< c count))
              (if (funcall test item (funcall key (aref sequence i)))
                  (incf c)
                  (progn (setf (aref r j) (aref sequence i)) (decf j)))
              (progn (setf (aref r j) (aref sequence i)) (decf j))))
        (do ((r (make-array len :element-type (array-element-type sequence)))
             (i 0 (1+ i))
             (j 0)
             (c 0))
            ((= i len) (subseq r 0 j))
          (when (< i start)
            (setf (aref r j) (aref sequence i)) (incf j))
          (when (>= i end)
            (setf (aref r j) (aref sequence i)) (incf j))
          (if (or (null count) (< c count))
              (if (funcall test item (funcall key (aref sequence i)))
                  (incf c)
                  (progn (setf (aref r j) (aref sequence i)) (incf j)))
              (progn (setf (aref r j) (aref sequence i)) (incf j)))))))
      
(defun remove (item sequence &key from-end (test #'eql) (start 0) end count (key #'identity))
  "returns a copy of SEQUENCE with ITEM removed"
    (if (listp sequence)
        (if from-end
            (vector-list (vector-remove item (list-vector sequence) :from-end t :test test :start start :end end :count count :key key))
            (list-remove item sequence :from-end nil :test test :start start :end end :count count :key key))
        (vector-remove item sequence :from-end from-end :test test :start start :end end :count count :key key)))

(defun list-remove-if (predicate sequence &key from-end (start 0) end count (key #'identity))
  (do ((s sequence (cdr s))
       (c 0)
       (i 0 (1+ i))
       (r nil))
      ((null s) (nreverse r))
    (if (or (and count (>= c count)) (and start (< i start)) (and end (>= i end)))
        (push (car s) r)
        (if (funcall predicate (funcall key (car s)))
            (incf c)
            (push (car s) r)))))
  
(defun vector-remove-if (predicate sequence &key from-end (start 0) end count (key #'identity))
  (let ((end (or end (length sequence))) (len (length sequence)))
    (if from-end
        (do ((r (make-array len :element-type (array-element-type sequence)))
             (i (1- len) (1- i))
             (j (1- len))
             (c 0))
            ((< i 0) (subseq r (1+ j) len))
          (when (>= i end)
            (setf (aref r j) (aref sequence i)) (decf j))
          (when (< i start)
            (setf (aref r j) (aref sequence i)) (decf j))
          (if (or (not count) (< c count))
              (if (funcall predicate (funcall key (aref sequence i)))
                  (incf c)
                  (progn (setf (aref r j) (aref sequence i)) (decf j)))
              (progn (setf (aref r j) (aref sequence i)) (decf j))))
        (do ((r (make-array len :element-type (array-element-type sequence)))
             (i 0 (1+ i))
             (j 0)
             (c 0))
            ((= i len) (subseq r 0 j))
          (when (< i start)
            (setf (aref r j) (aref sequence i)) (incf j))
          (when (>= i end)
            (setf (aref r j) (aref sequence i)) (incf j))
          (if (or (null count) (< c count))
              (if (funcall predicate (funcall key (aref sequence i)))
                  (incf c)
                  (progn (setf (aref r j) (aref sequence i)) (incf j)))
              (progn (setf (aref r j) (aref sequence i)) (incf j)))))))
    
  
(defun remove-if (predicate sequence &key from-end (start 0) end count (key #'identity))
  "returns a copy of SEQUENCE with all items removed for which PREDICATE is true"
  (if (listp sequence)
      (if from-end
          (vector-list (vector-remove-if predicate (list-vector sequence) :from-end t :start start :end end :count count :key key))
          (list-remove-if predicate sequence :from-end nil :start start :end end :count count :key key))
      (vector-remove-if predicate sequence :from-end from-end :start start :end end :count count :key key)))

(defun delete (item sequence &key from-end (test #'eql) (start 0) end count (key #'identity))
  "removes ITEM from SEQUENCE destructively"
  (remove item sequence :from-end from-end :test test :start start :end end :count count :key key))

(defun delete-if (predicate sequence &key from-end (start 0) end count (key #'identity))
  "removes all items for which PREDICATE is true from SEQUENCE destructively"
  (remove-if predicate sequence :from-end from-end :start start :end end :count count :key key))
               
(defun substitute (newitem olditem sequence &key from-end (test #'eql) (start 0) end count (key #'identity))
  "returns a copy of SEQUENCE with OLDITEM replaces with NEWITEM"
  (nsubstitute newitem olditem (copy-seq sequence) :from-end from-end :test test :start start :end end :count count :key key))

(defun substitute-if (newitem predicate sequence &key from-end (start 0) end count (key #'identity))
  "returns a copy of SEQUENCE with all items satisfying PREDICATE replaced with NEWITEM"
  (nsubstitute-if newitem predicate (copy-seq sequence) :from-end from-end :start start :end end :count count :key key))

(defun list-nsubstitute (newitem olditem sequence &key from-end (test #'eql) (start 0) end count (key #'identity))
  (do ((s sequence (cdr s))
       (c 0)
       (i 0 (1+ i)))
      ((null s) sequence)
    (unless (or (and count (>= c count)) (and start (< i start)) (and end (>= i end)))
      (if (funcall test olditem (funcall key (car s)))
          (progn
            (setf (car s) newitem)
            (incf c))))))

(defun vector-nsubstitute (newitem olditem sequence &key from-end (test #'eql) (start 0) end count (key #'identity))
  (do ((end (or end (length sequence)))
       (i start (1+ i))
       (c 0))
      ((or (= i end) (and count (>= c count))) sequence)
    (when (funcall test olditem (funcall key (aref sequence i)))
      (setf (aref sequence i) newitem)
      (incf c))))
            
(defun nsubstitute (newitem olditem sequence &key from-end (test #'eql) (start 0) end count (key #'identity))
  "substitutes OLDITEM with NEWITEM in SEQUENCE destructively"
  (if (listp sequence)
      (list-nsubstitute newitem olditem sequence :from-end from-end :test test :start start :end end :count count :key key)
      (vector-nsubstitute newitem olditem sequence :from-end from-end :test test :start start :end end :count count :key key)))

(defun list-nsubstitute-if (newitem predicate sequence &key from-end (start 0) end count (key #'identity))
  (do ((s sequence (cdr s))
       (c 0)
       (i 0 (1+ i)))
      ((null s) sequence)
    (unless (or (and count (>= c count)) (and start (< i start)) (and end (>= i end)))
      (if (funcall predicate (funcall key (car s)))
          (progn
            (setf (car s) newitem)
            (incf c))))))
  
(defun vector-nsubstitute-if (newitem predicate sequence &key from-end (start 0) end count (key #'identity))
  (do ((end (or end (length sequence)))
       (i start (1+ i))
       (c 0))
      ((or (= i end) (and count (>= c count))) sequence)
    (when (funcall predicate (funcall key (aref sequence i)))
      (setf (aref sequence i) newitem)
      (incf c))))
      
(defun nsubstitute-if (newitem predicate sequence &key from-end (start 0) end count (key #'identity))
  "substitutes all items satisfying PREDICATE with NEWITEM in SEQUENCE destructively"
  (if (listp sequence)
      (list-nsubstitute-if newitem predicate sequence :from-end from-end :start start :end end :count count :key key)
      (vector-nsubstitute-if newitem predicate sequence :from-end from-end :start start :end end :count count :key key)))

(defun list-subseq (sequence start &optional end)
  (do ((s sequence (cdr s))
       (i 0 (1+ i))
       (r nil))
      ((null s) (nreverse r))
    (unless (or (< i start) (and end (>= i end)))
      (push (car s) r))))
  
(defun vector-subseq (sequence start &optional end)
  (do ((i start (1+ i))
       (end (or end (length sequence)))
       (r (make-array (- (or end (length sequence)) start) :element-type (array-element-type sequence))))
      ((>= i end) r)
    (setf (aref r (- i start)) (aref sequence i))))
                     
(defun subseq (sequence start &optional end)
  "returns the subsequence of SEQUENCE starting from START and ending at END"
  (if (listp sequence)
      (list-subseq sequence start end)
      (vector-subseq sequence start end)))

(defun shellsort (seq pred &key key)
  (let ((key (or key #'identity)))
    (do ((increment (truncate (/ (length seq) 2)) (if (= increment 2) 1 (truncate (/ increment 2.2)))))
        ((<= increment 0) seq)
      (do ((i increment (1+ i)))
          ((>= i (length seq)))
        (let ((j i) (temp (aref seq i)))
          (do ()
              ((or (< j increment) (funcall pred (funcall key (aref seq (- j increment))) (funcall key temp))))
            (setf (aref seq j) (aref seq (- j increment)))
            (setq j (- j increment)))
          (setf (aref seq j) temp))))))

(defun insertionsort (seq pred &key key)
  (let ((key (or key #'identity)) (len (length seq)))
    (do ((i 1 (1+ i)))
        ((= i len) seq)
      (do ((v (aref seq i))
           (k (funcall key (aref seq i)))
           (j (1- i) (1- j)))
          ((or (< j 0) (not (funcall pred k (funcall key (aref seq j))))) (setf (aref seq (1+ j)) v))
        (setf (aref seq (1+ j)) (aref seq j))))))
  
(defun stable-sort (seq pred &key (key #'identity))
  "stable sorts destructively SEQ according to PRED"
  (if (listp seq)
      (vector-list (insertionsort (list-vector seq) pred :key key))
      (insertionsort seq pred :key key)))

(defun sort (seq pred &key (key #'identity))
  "sorts destructively SEQ according to PRED"
  (if (listp seq)
      (vector-list (shellsort (list-vector seq) pred :key key))
      (shellsort seq pred :key key)))
  
(defun vector-push (new-element vector)
  "store NEW-ELEMENT in VECTOR"
  (let ((fp (fill-pointer vector)))
    (setf (aref vector fp) new-element)
    (incf (fill-pointer vector))
    fp))

(defun vector-pop (vector)
  "decreases the fill-pointer of VECTOR and returns the element"
  (decf (fill-pointer vector))
  (aref vector (fill-pointer vector)))

(defun list-replace (seq1 seq2 &key (start1 0) end1 (start2 0) end2)
  (do ((end2 (or end2 (length seq2)))
       (i start1 (1+ i))
       (j start2 (1+ j))
       (s (nthcdr start1 seq1) (cdr s)))
      ((or (null s) (and end1 (= i end1)) (= j end2)) seq1)
    (setf (car s) (elt seq2 j))))
  
(defun vector-replace (seq1 seq2 &key (start1 0) end1 (start2 0) end2)
  (do ((end1 (or end1 (length seq1)))
       (end2 (or end2 (length seq2)))
       (i start1 (1+ i))
       (j start2 (1+ j)))
      ((or (= i end1) (= j end2)) seq1)
    (setf (aref seq1 i) (elt seq2 j))))

(defun replace (seq1 seq2 &key (start1 0) end1 (start2 0) end2)
  "destructively changes a part of SEQ1 to contain a part of SEQ2"
  (if (eq seq1 seq2) ;; todo check for overlapping, and only copy seq2 if needed
      (replace seq1 (copy-seq seq2) :start1 start1 :end1 end2 :start2 start2 :end2 end2)
      (if (listp seq1)
          (list-replace seq1 seq2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)
          (vector-replace seq1 seq2 :start1 start1 :end1 end1 :start2 start2 :end2 end2))))

(defun make-list (size :initial-element initial-element)
  (do ((i 0 (1+ i))
       (r nil))
      ((= i size) r)
    (push initial-element r)))

(defun make-sequence (result-type size &key initial-element)
  "creates a sequence of type RESULT-TYPE and size SIZE with contents INITIAL-ELEMENT"
  (case result-type
        ((list) (make-list size :initial-element initial-element))
        ((string) (make-array size :element-type 'char :initial-element (or initial-element #\Space)))
        (otherwise (make-array size :element-type t :initial-element initial-element))))

(defun merge (result-type seq1 seq2 predicate &key (key #'identity))
  (let* ((len1 (length seq1)) 
         (len2 (length seq2))
         (len (+ len1 len2)) 
         (r (make-sequence result-type len)))
    (do ((i 0)
         (j 0)
         (k 0 (1+ k)))
        ((= k len) r)
      (cond
       ((= i len1) (setf (elt r k) (elt seq2 j)) (incf j))
       ((= j len2) (setf (elt r k) (elt seq1 i)) (incf i))
       ((funcall predicate (funcall key (elt seq1 i)) (funcall key (elt seq2 j)))
        (setf (elt r k) (elt seq1 i))
        (incf i))
       ((funcall predicate (funcall key (elt seq2 j)) (funcall key (elt seq1 i)))
        (setf (elt r k) (elt seq2 j))
        (incf j))
       (t 
        (setf (elt r k) (elt seq1 i)) 
        (incf i))))))

(defun count (item sequence &key from-end (start 0) (end nil) (key #'identity) (test #'eql))
  (let ((end (or end (length sequence))))
    (if from-end
        (do ((i (1- end) (1- i))
             (c 0))
            ((< i start) c)
          (when (funcall test item (funcall key (elt sequence i)))
            (incf c)))
        (do ((i start (1+ i))
             (c 0))
            ((= i end) c)
          (when (funcall test item (funcall key (elt sequence i)))
            (incf c))))))
    
(defun count-if (predicate sequence &key from-end (start 0) (end nil) (key #'identity))
  (let ((end (or end (length sequence))))
    (if from-end
        (do ((i (1- end) (1- i))
             (c 0))
            ((< i start) c)
          (when (funcall predicate (funcall key (elt sequence i)))
            (incf c)))
        (do ((i start (1+ i))
             (c 0))
            ((= i end) c)
          (when (funcall predicate (funcall key (elt sequence i)))
            (incf c))))))
  
(defun count-if-not (predicate sequence &key from-end (start 0) (end nil) (key #'identity))
  (let ((end (or end (length sequence))))
    (if from-end
        (do ((i (1- end) (1- i))
             (c 0))
            ((< i start) c)
          (unless (funcall predicate (funcall key (elt sequence i)))
            (incf c)))
        (do ((i start (1+ i))
             (c 0))
            ((= i end) c)
          (unless (funcall predicate (funcall key (elt sequence i)))
            (incf c))))))
  
(defun map-into (result-sequence fn &rest sequences)
  (let ((m (apply #'min (length result-sequence) (mapcar #'length sequences))))
    (do ((i 0 (1+ i)))
        ((= i m) result-sequence)
      (setf (elt result-sequence i)
            (apply fn
                   (mapcar #'(lambda (seq) (elt seq i))
                           sequences))))))

(defun map (result-type fn &rest sequences)
  (let* ((m (apply #'min (mapcar #'length sequences)))
         (r (make-sequence result-type m)))
    (do ((i 0 (1+ i)))
        ((= i m) r)
      (setf (elt r i) (apply fn (mapcar #'(lambda (seq) (elt seq i)) sequences))))))

(defun reduce (fn sequence &key key from-end (start 0) end initial-value)
  (let* ((end (or end (length sequence))) (len (- end start)) (key (or key #'identity)))
    (when (= len 0)
        (if initial-value
            (return-from reduce initial-value)
            (return-from reduce (funcall fn))))
    (when (and (= len 1) (null initial-value))
      (return-from reduce (elt sequence 0)))
    (if from-end
        (progn
          (unless initial-value
            (setf initial-value (funcall key (elt sequence (1- end))))
            (decf end))
          (do ((i (1- end) (1- i))
               (r initial-value (funcall fn (funcall key (elt sequence i)) r)))
              ((< i start) r)))
        (progn
          (unless initial-value
            (setf initial-value (funcall key (elt sequence start)))
            (incf start))
          (do ((i start (1+ i))
               (r initial-value (funcall fn r (funcall key (elt sequence i)))))
              ((= i end) r))))))         

(defun mismatch (seq1 seq2 &key from-end (test #'eql) test-not key (start1 0) (start2 0) end1 end2)
  (let ((key (or key #'identity)) (end1 (or end1 (length seq1))) (end2 (or end2 (length seq2))))
    (if from-end
        (do ((i (1- end1) (1- i))
             (j (1- end2) (1- j)))
            ((and (< i start1) (< j start2)) nil)
          (when (< i start1) (return (1+ i)))
          (when (< j start2) (return (1+ i)))
          (if test-not
              (when (funcall test-not 
                               (funcall key (elt seq1 i)) 
                               (funcall key (elt seq2 j)))
                (return i))
              (unless (funcall test 
                               (funcall key (elt seq1 i)) 
                               (funcall key (elt seq2 j)))
                (return i))))
        (do ((i start1 (1+ i))
             (j start2 (1+ j)))
            ((and (= i end1) (= j end2)) nil)
          (when (= i end1) (return i))
          (when (= j end2) (return i))
          (if test-not
              (when (funcall test-not 
                             (funcall key (elt seq1 i)) 
                             (funcall key (elt seq2 j)))
                (return i))
              (unless (funcall test 
                               (funcall key (elt seq1 i)) 
                               (funcall key (elt seq2 j)))
                (return i)))))))
  
  