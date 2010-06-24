;;;; common lisp system library

;;; string functions

(defun char (str idx)
  "returns the character at index IDX of string STR"
  (aref str idx))

(defun schar (str idx)
  "returns the character at index IDX of string STR"
  (aref str idx))

(defun whitespace-char-p (x)
  "true if x is a whitespace, non-printing character, false otherwise"
  (or (char= x #\Newline)
      (char= x #\Return)
      (char= x #\Tab)
      (char= x #\Space)))

(defun graphic-char-p (x)
  "true if x is a printing character, false otherwise"
  (not (whitespace-char-p x)))

(defun string-char-p (x) 
  "true if x can be stored in a string, false otherwise"
  t)

(defun alpha-char-p (x)
  "true if x is an alphabetic character, false otherwise"
  (or (char<= #\a x #\z)
      (char<= #\A x #\Z)))

(defun upper-case-p (x)
  "true if x is an upper case character, false otherwise"
  (char<= #\A x #\Z))

(defun lower-case-p (x)
  "true if x is a lower case character, false otherwise"
  (char<= #\a x #\z))

(defun both-case-p (x)
  "true if x is upper-case and a lower-case version exists or vice versa, false otherwise"
  (alpha-char-p x))

(defun char-upcase (x)
  "returns the upper case character corresponding to x"
  (if (lower-case-p x)
      (code-char (+ (char-code #\A) (- (char-code x) (char-code #\a))))
      x))

(defun char-downcase (x)
  "returns the lower case character corresponding to x"
  (if (upper-case-p x)
      (code-char (+ (char-code #\a) (- (char-code x) (char-code #\A))))
      x))

(defun last-digit-char (radix)
  "returns the character representing the highest value digit in RADIX representation"
  (if (<= radix 10)
      (code-char (+ (char-code #\0) (1- radix)))
      (code-char (+ (char-code #\a) (- (1- radix) 10)))))

(defun digit-char-p (x &optional (radix 10))
  "true if x is a digit char in the chose radix (default is 10)"
  (if (<= radix 10)
      (char<= #\0 (char-downcase x) (last-digit-char radix))
      (or
       (char<= #\0 (char-downcase x) #\9)
       (char<= #\a (char-downcase x) (last-digit-char radix)))))

(defun alphanumericp (x)
  "true if x is an alphabetic or numeric char, false otherwise"
  (or (alpha-char-p x) (digit-char-p x)))

(defun string (string)
  "if STRING is a symbol, returns its symbolname, otherwise return STRING"
  (if (symbolp string)
      (symbol-name string)
      string))

(defun stringcmp (string1 string2 &key (test #'char=) (start1 0) (end1 nil) (start2 0) (end2 nil))
  (let ((end1 (or end1 (length string1))) (end2 (or end2 (length string2))))
    (when (not (= (- end1 start1) (- end2 start2))) (return-from stringcmp nil))
    (do ((i1 start1 (1+ i1))
         (i2 start2 (1+ i2)))
        ((= i1 end1) t)
      (unless (funcall test (char string1 i1) (char string2 i2)) (return nil)))))

(defun string= (string1 string2 &key (start1 0) (end1 nil) (start2 0) (end2 nil))
  (stringcmp (string string1) (string string2) :test #'char= :start1 start1 :end1 end1 :start2 start2 :end2 end2))

(defun string/= (string1 string2 &key (start1 0) (end1 nil) (start2 0) (end2 nil))
  (not (stringcmp (string string1) (string string2) :test #'char= :start1 start1 :end1 end1 :start2 start2 :end2 end2)))

(defun string-equal (string1 string2 &key (start1 0) (end1 nil) (start2 0) (end2 nil))
  (stringcmp (string string1) (string string2) :test #'char-equal :start1 start1 :end1 end1 :start2 start2 :end2 end2))

(defun string-not-equal (string1 string2 &key (start1 0) (end1 nil) (start2 0) (end2 nil))
  (not (stringcmp (string string1) (string string2) :test #'char-equal :start1 start1 :end1 end1 :start2 start2 :end2 end2)))

(defun string< (string1 string2 &key (start1 0) (end1 nil) (start2 0) (end2 nil))
  (let ((str1 (string string1)) (str2 (string string2)))
    (let ((end1 (or end1 (length str1))) (end2 (or end2 (length str2))))
      (do ((i1 start1 (1+ i1))
           (i2 start2 (1+ i2)))
          ()
        (if (and (= i1 end1) (= i2 end2)) (return nil))
        (if (= i1 end1) (return t))
        (if (= i2 end2) (return nil))
        (if (char< (char str1 i1) (char str2 i2))
            (return t)
            (if (not (char= (char str1 i1) (char str2 i2)))
                (return nil)))))))

(defun string<= (string1 string2 &key (start1 0) (end1 nil) (start2 0) (end2 nil))
  (or (string= string1 string2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)
      (string< string1 string2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))

(defun string> (string1 string2 &key (start1 0) (end1 nil) (start2 0) (end2 nil))
  (not (string<= string1 string2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))

(defun string>= (string1 string2 &key (start1 0) (end1 nil) (start2 0) (end2 nil))
  (not (string< string1 string2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))

(defun string-lessp (string1 string2 &key (start1 0) (end1 nil) (start2 0) (end2 nil))
  (let ((str1 (string string1)) (str2 (string string2)))
    (let ((end1 (or end1 (length str1))) (end2 (or end2 (length str2))))
      (do ((i1 start1 (1+ i1))
           (i2 start2 (1+ i2)))
          ()
        (if (and (= i1 end1) (= i2 end2)) (return nil))
        (if (= i1 end1) (return t))
        (if (= i2 end2) (return nil))
        (if (char-lessp (char str1 i1) (char str2 i2))
            (return t)
            (if (not (char-equal (char str1 i1) (char str2 i2)))
                (return nil)))))))

(defun string-not-greaterp (string1 string2 &key (start1 0) (end1 nil) (start2 0) (end2 nil))
  (or (string-equal string1 string2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)
      (string-lessp string1 string2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))

(defun string-greaterp (string1 string2 &key (start1 0) (end1 nil) (start2 0) (end2 nil))
  (not (string-not-greaterp string1 string2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))

(defun string-not-lessp (string1 string2 &key (start1 0) (end1 nil) (start2 0) (end2 nil))
  (not (string-lessp string1 string2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))

(defun string-left-trim (char-bag string)
  "trims all characters in CHAR-BAG from STRING from left"
  (let ((str (string string)))
    (do ((i 0 (1+ i))
         (len (length str)))
        ((= i len) "")
      (if (not (find (char str i) char-bag)) (return (subseq str i))))))

(defun string-right-trim (char-bag string)
  "trims all characters in CHAR-BAG from STRING from right"
  (let ((str (string string)))
    (do ((i (1- (length str)) (1- i)))
        ((zerop i) "")
      (if (not (find (char str i) char-bag)) (return (subseq str 0 (1+ i)))))))
    
(defun string-trim (char-bag string)
  "trims all characters in CHAR-BAG from STRING from left and from right"
  (string-right-trim char-bag (string-left-trim char-bag string)))

(defun string-upcase (string &key (start 0) end)
  "returns a copy of STRING with all uppercase letters"
  (let* ((str (string string)) (len (length str)))
    (do ((end (or end len))
         (res (copy-seq str))
         (i 0 (1+ i)))
        ((= i len) res)
      (when (and (<= start i) (< i end))
          (setf (char res i) (char-upcase (char res i)))))))

(defun string-downcase (string &key (start 0) end)
  "returns a copy of STRING with all lowercase letters"
  (let* ((str (string string)) (len (length str)))
    (do ((end (or end len))
         (res (copy-seq str))
         (i 0 (1+ i)))
        ((= i len) res)
      (when (and (<= start i) (< i end))
          (setf (char res i) (char-downcase (char res i)))))))

(defun string-capitalize (string &key (start 0) end)
  "returns a copy of STRING with each word capitalized"
  (let* ((str (string string)) (len (length str)))
    (do ((end (or end len))
         (res (copy-seq str))
         (i 0 (1+ i))
         (cap t))
        ((= i len) res)
      (when (and (<= start i) (< i end))
        (if (whitespace-char-p (char res i))
            (setq cap t)
            (if cap
                (progn (setf (char res i) (char-upcase (char res i))) (setq cap nil))
                (setf (char res i) (char-downcase (char res i)))))))))

(defun nstring-upcase (string &key (start 0) end)
  "changes STRING to all uppercase letters"
  (do ((end (or end (length string)))
       (i 0 (1+ i)))
      ((= i (length string)) string)
    (when (and (<= start i) (< i end))
      (setf (char string i) (char-upcase (char string i))))))

(defun nstring-downcase (string &key (start 0) end)
  "changes STRING to all lowercase letters"
  (do ((end (or end (length string)))
       (i 0 (1+ i)))
      ((= i (length string)) string)
    (when (and (<= start i) (< i end))
      (setf (char string i) (char-downcase (char string i))))))

(defun nstring-capitalize (string &key (start 0) end)
  "changes STRING so that each word is capitalized"
  (do ((end (or end (length string)))
       (i 0 (1+ i))
       (cap t))
      ((= i (length string)) string)
    (when (and (<= start i) (< i end))
      (if (whitespace-char-p (char string i))
          (setq cap t)
          (if cap
              (progn (setf (char string i) (char-upcase (char string i))) (setq cap nil))
              (setf (char string i) (char-downcase (char string i))))))))

(defun make-string (size &key (initial-element #\Space))
  "creates a string of length SIZE containing INITIAL-ELEMENT"
  (make-sequence 'string size :initial-element initial-element))

(defun string-length (str)
  "returns the length of string STR"
  (vector-length str))

