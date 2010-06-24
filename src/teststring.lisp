;;;; string unit tests

(format t "~%teststring...")

(test (char "abc" 0) #\a)
(test (char "abc" 1) #\b)
(test (char "abc" 2) #\c)

(test (schar "abc" 0) #\a)
(test (schar "abc" 1) #\b)
(test (schar "abc" 2) #\c)

(test (whitespace-char-p #\Space) t)
(test (whitespace-char-p #\Newline) t)
(test (whitespace-char-p #\Tab) t)
(test (whitespace-char-p #\a) nil)

(test (graphic-char-p #\a) t)
(test (graphic-char-p #\A) t)
(test (graphic-char-p #\Space) nil)

(test (alpha-char-p #\a) t)
(test (alpha-char-p #\A) t)
(test (alpha-char-p #\1) nil)

(test (upper-case-p #\a) nil)
(test (upper-case-p #\A) t)
(test (upper-case-p #\1) nil)

(test (lower-case-p #\a) t)
(test (lower-case-p #\A) nil)
(test (lower-case-p #\1) nil)

(test (both-case-p #\a) t)
(test (both-case-p #\A) t)
(test (both-case-p #\Space) nil)

(test (char-upcase #\a) #\A)
(test (char-upcase #\A) #\A)
(test (char-upcase #\1) #\1)

(test (char-downcase #\a) #\a)
(test (char-downcase #\A) #\a)
(test (char-downcase #\1) #\1)

(test (last-digit-char 10) #\9)
(test (last-digit-char 8) #\7)
(test (last-digit-char 16) #\f)
(test (last-digit-char 36) #\z)

(test (digit-char-p #\0) t)
(test (digit-char-p #\9) t)
(test (digit-char-p #\a) nil)
(test (digit-char-p #\0 8) t)
(test (digit-char-p #\7 8) t)
(test (digit-char-p #\8 8) nil)
(test (digit-char-p #\0 16) t)
(test (digit-char-p #\f 16) t)
(test (digit-char-p #\g 16) nil)

(test (alphanumericp #\a) t)
(test (alphanumericp #\A) t)
(test (alphanumericp #\1) t)
(test (alphanumericp #\Space) nil)

(test (string "abc") "abc")
(test (string 'abc) "abc")
(test (string :abc) "abc")

(test (string= "abc" "abc") t)
(test (string= "abc" "abcd") nil)
(test (string= "abc" "abd") nil)
(test (string= "abc" "abC") nil)
(test (string= "abc" 'abc) t)

(test (string/= "abc" "abc") nil)
(test (string/= "abc" "abcd") t)
(test (string/= "abc" "abd") t)
(test (string/= "abc" "abC") t)

(test (string-equal "abc" "abc") t)
(test (string-equal "abc" "abcd") nil)
(test (string-equal "abc" "abd") nil)
(test (string-equal "abc" "abC") t)
(test (string-equal "abc" 'abc) t)

(test (string-not-equal "abc" "abc") nil)
(test (string-not-equal "abc" "abcd") t)
(test (string-not-equal "abc" "abd") t)
(test (string-not-equal "abc" "abC") nil)

(test (string< "abc" "abcd") t)
(test (string< "abc" "abc") nil)
(test (string< "abc" "bcd") t)
(test (string< "abc" "abd") t)
(test (string< "bcd" "abc") nil)

(test (string<= "abc" "abcd") t)
(test (string<= "abc" "abc") t)
(test (string<= "abc" "bcd") t)
(test (string<= "abc" "abd") t)
(test (string<= "bcd" "abc") nil)

(test (string> "abc" "abcd") nil)
(test (string> "abc" "abc") nil)
(test (string> "abc" "bcd") nil)
(test (string> "abc" "abd") nil)
(test (string> "bcd" "abc") t)

(test (string>= "abc" "abcd") nil)
(test (string>= "abc" "abc") t)
(test (string>= "abc" "bcd") nil)
(test (string>= "abc" "abd") nil)
(test (string>= "bcd" "abc") t)

(test (string-lessp "abc" "ABCD") t)
(test (string-not-greaterp "abc" "ABC") t)
(test (string-not-lessp "abc" "ABC") t)
(test (string-greaterp "ABCD" "abc") t)

(test (string-left-trim " " "  abc  ") "abc  ")
(test (string-left-trim '(#\Space) "  abc  ") "abc  ")
(test (string-left-trim " " '| abc |) "abc ")

(test (string-right-trim " " "  abc  ") "  abc")
(test (string-right-trim '(#\Space) "  abc  ") "  abc")

(test (string-trim " " "  abc  ") "abc")
(test (string-trim '(#\Space) "  abc  ") "abc")
(test (string-trim '(#\a #\b #\c) "abcdedcba") "ded")
(test (string-trim #(#\a #\b #\c) "abcdedcba") "ded")

(test (string-upcase "hallo welt") "HALLO WELT")
(test (string-upcase "abcdef" :start 2 :end 4) "abCDef")

(test (string-downcase "HALLO welt") "hallo welt")

(test (string-capitalize "hAlLo WeLt") "Hallo Welt")

(test (let ((s "hAlLo WeLt")) (nstring-upcase s) s) "HALLO WELT")

(test (let ((s "hAlLo WeLt")) (nstring-downcase s) s) "hallo welt")

(test (let ((s "hAlLo WeLt")) (nstring-capitalize s) s) "Hallo Welt")

(test (make-string 0) "")
(test (make-string 1) " ")
(test (make-string 5 :initial-element #\a) "aaaaa")

(test (string-length "") 0)
(test (string-length "a") 1)
(test (string-length "abc") 3)

