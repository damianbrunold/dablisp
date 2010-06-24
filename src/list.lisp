;;;; common lisp system library

;;; list functions

(defun listp (x) (or (typep x 'cons) (typep x 'null)))

(defun assoc (item alist &key (key #'identity) (test #'eql))
  "returns the first pair of ALIST whose car is eql to ITEM or nil if none such exists"
  (do ((a alist (cdr a)))
      ((null a) nil)
    (if (funcall test (funcall key (caar a)) item) (return (car a)))))

(defun assoc-if (predicate alist &key (key #'identity))
  "returns the first pair of ALIST for whose car the PREDICATE is true or nil if none such exists"
  (do ((a alist (cdr a)))
      ((null a) nil)
    (if (funcall predicate (funcall key (caar a))) (return (car a)))))

(defun assoc-if-not (predicate alist &key (key #'identity))
  "returns the first pair of ALIST for whose car the PREDICATE is false or nil if none such exists"
  (assoc-if (complement predicate) alist :key key))

(defun rassoc (item alist &key (key #'identity) (test #'eql))
  "returns the first pair of ALIST whose cdr is eql to ITEM or nil if none such exists"
  (do ((a alist (cdr a)))
      ((null a) nil)
    (if (funcall test (funcall key (cdar a)) item) (return (car a)))))

(defun rassoc-if (predicate alist &key (key #'identity))
  "returns the first pair of ALIST for whose cdr the PREDICATE is true or nil if none such exists"
  (do ((a alist (cdr a)))
      ((null a) nil)
    (if (funcall predicate (funcall key (cdar a))) (return (car a)))))
  
(defun rassoc-if-not (predicate alist &key (key #'identity))
  "returns the first pair of ALIST for whose cdr the PREDICATE is false or nil if none such exists"
  (rassoc-if (complement predicate) alist :key key))

(defun member (item list &key (key #'identity) (test #'eql))
  (do ((a list (cdr a)))
      ((null a) nil)
    (if (funcall test (funcall key (car a)) item) (return a))))

(defun member-if (predicate list &key (key #'identity))
  (do ((a list (cdr a)))
      ((null a) nil)
    (if (funcall predicate (funcall key (car a))) (return a))))

(defun adjoin (item list &key (key #'identity) (test #'eql))
  (if (member item list :key key :test test)
      list
      (cons item list)))

(defun union (list1 list2 &key (key #'identity) (test #'eql))
  (do ((a list2 (cdr a)) (b list1 (adjoin (car a) b :key key :test test)))
      ((null a) b)))

(defun nunion (list1 list2 &key (key #'identity) (test #'eql))
  (union list1 list2 :key key :test test))

(defun intersection (list1 list2 &key (key #'identity) (test #'eql))
  (do ((a list1 (cdr a)) 
       (b nil (if (member (car a) list2 :key key :test test) (cons (car a) b) b)))
      ((null a) b)))

(defun nintersection (list1 list2 &key (key #'identity) (test #'eql))
  (intersection list1 list2 :key key :test test))

(defun set-difference (list1 list2 &key (key #'identity) (test #'eql))
  (do ((a list1 (cdr a))
       (b nil (if (member (car a) list2 :key key :test test) b (cons (car a) b))))
      ((null a) b)))

(defun nset-difference (list1 list2 &key (key #'identity) (test #'eql))
  (set-difference list1 list2 :key key :test test))

(defun cars (lists)
  (do ((a lists (cdr a))
       (result nil))
      ((null a) (nreverse result))
    (if (null (car a)) (return nil))
    (push (caar a) result)))

(defun cdrs (lists)   
  (do ((a lists (cdr a))
       (result nil))
      ((null a) (nreverse result))
    (if (null (cdar a)) (return nil))
    (push (cdar a) result)))
    
(defun mapcar (f &rest lists)
  (if (= (length lists) 1)
      (do ((a (car lists) (cdr a))
           (r nil))
          ((null a) (nreverse r))
        (push (funcall f (car a)) r))
      (do ((a lists (cdrs a))
           (r nil))
          ((null a) (nreverse r))
        (let ((c (cars a)))
          (unless c (return (nreverse r)))
          (push (apply f c) r)))))

(defun maplist (f &rest lists)
  (if (= (length lists) 1)
      (do ((a (car lists) (cdr a))
           (r nil))
          ((null a) (nreverse r))
        (push (funcall f a) r))
      (do ((a lists (cdrs a))
           (r nil))
          ((null a) (nreverse r))
        (print a)
        (if a
            (push (apply f a) r)
            (return (nreverse r))))))

(defun nconc (&rest lists)
  "returns a list that is the destructive concatenation of LISTS"
  (if (null lists)
      nil
      (do ((a lists (cdr a))
           (r nil)
           (p nil))
          ((null a) r)
        (when (car a)
          (if p
              (setf (cdr (last p)) (car a))
              (setq r (car a)))
          (setq p (car a))))))
  
(defun mapcan (f &rest lists)
  (apply #'nconc (apply #'mapcar f lists)))

(defun mapcon (f &rest lists)
  (apply #'nconc (apply #'maplist f lists)))

(defun mapc (f &rest lists)
  (apply #'mapcar f lists)
  (car lists))

(defun mapl (f &rest lists)
  (apply #'maplist f lists)
  (car lists))

(defun copy-alist (alist)
  "makes a shallow copy of ALIST"
  (if (null alist)
      nil
      (cons (cons (caar alist) (cdar alist)) (copy-alist (cdr alist)))))

(defun copy-tree (tree)
  "makes a copy of TREE"
  (if (null tree)
      nil
      (if (atom tree)
          tree
          (cons (copy-tree (car tree)) (copy-tree (cdr tree))))))

(defun endp (obj)
  (null obj))

(defun acons (key data alist)
  "adds KEY and DATA to the ALIST"
  (cons (cons key data) alist))

(defun pairlis (keys data &optional alist)
  "constructs an alist out of KEYS and DATA."
  (if (null keys)
      alist
      (pairlis (cdr keys) (cdr data) (acons (car keys) (car data) alist))))

(defun sublis (alist tree &key (key #'identity) (test #'eql))
  (let ((key (or key #'identity)))
    (if (null tree)
        nil
        (if (atom tree)
            (let ((a (assoc tree alist :key key :test test)))
              (if a
                  (cdr a)
                  tree))
            (cons (sublis alist (car tree) :key key :test test) 
                  (sublis alist (cdr tree) :key key :test test))))))

(defun nsublis (alist tree &key (key #'identity) (test #'eql))
  (sublis alist tree :key key :test test))

(defun tree-equal (a b &key (test #'eql))
  (if (null a)
      (null b)
      (if (atom a)
          (if (atom b)
              (funcall test a b)
              nil)
          (if (atom b)
              nil
              (and (tree-equal (car a) (car b) :test test)
                   (tree-equal (cdr a) (cdr b) :test test))))))

(defun list* (&rest objects)
  (when (null objects) (return-from list* nil))
  (when (null (cdr objects)) (return-from list* (car objects)))
  (do ((o objects (cdr o))
       (r nil))
      ((null (cdr o)) 
       (setf r (nreverse r))
       (setf (cdr (last r)) (car o))
       r)
    (push (car o) r)))

(defun nreconc (list tail)
  (nconc (nreverse list) tail))

(defun tailp (object list)
  (do ((list list (cdr list)))
      ((atom list) (eql list object))
    (if (eql object list)
        (return t))))
  
(defun ldiff (list object)
  (do ((list list (cdr list))
       (r '() (cons (car list) r)))
      ((atom list)
       (if (eql list object) (nreverse r) (nreconc r list)))
    (when (eql object list)
      (return (nreverse r)))))

  