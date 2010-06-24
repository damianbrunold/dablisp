;;;; coerce unit tests

(format t "~%testcoerce...")

(test (coerce '(#\a #\b) 'string) "ab")
(test (coerce "ab" 'list) '(#\a #\b))
(test (coerce 1.5 'integer) 1)
(test (coerce 1 'float) 1.0)
(test (coerce "a" 'character) #\a)
(test (coerce #\a 'string) "a")
(test (coerce 'abc 'string) "abc")
(test (coerce "abc" 'symbol) 'abc)
(test (coerce #\a 'integer) 97)
(test (coerce 97 'character) #\a)
(test (coerce '(1 2 3) 'vector) #(1 2 3))
(test (coerce #(1 2 3) 'list) '(1 2 3))
(test (coerce 12 'string) "12")
(test (coerce "12" 'integer) 12)
(test (coerce 12.0 'string) "12.0")
(test (coerce "12.0" 'float) 12.0)

(test (coerce 'a 't) 'a)
(test (coerce 12 't) 12)
(test (coerce "12" 't) "12")
(test (coerce #(1 2 3) 't) #(1 2 3))
(test (coerce '(1 2 3) 't) '(1 2 3))

