;;;; num unit tests

(format t "~%testnum...")

(test (zerop 0) t)
(test (zerop 1) nil)
(test (zerop 0.0) t)

(test (evenp 2) t)
(test (evenp 3) nil)

(test (oddp 2) nil)
(test (oddp 3) t)

(test (plusp 1) t)
(test (plusp 0) nil)
(test (plusp -1) nil)

(test (minusp 1) nil)
(test (minusp 0) nil)
(test (minusp -1) t)

(test (1+ 7) 8)
(test (1+ 7.0) 8)

(test (1- 7) 6)
(test (1- 7.0) 6)

(test (abs 2) 2)
(test (abs 0) 0)
(test (abs -2) 2)

(test (signum 2) 1)
(test (signum 0) 0)
(test (signum -2) -1)
(test (signum 2.0) 1)
(test (signum 0.0) 0)
(test (signum -2.0) -1)

(test (ffloor 2.6) 2.0)

(test (fceiling 2.6) 3.0)

(test (ftruncate -2.6) -2.0)

(test (fround 2.6) 3.0)

(test (max 1 2 4 3) 4)

(test (min 4 2 1 3) 1)

(test (fact 1) 1)
(test (fact 2) 2)
(test (fact 3) 6)
(test (fact 4) 24)
(test (fact 20) 2432902008176640000)

(test (gcd2 1071 1029) 21)

(test (gcd) 0)
(test (gcd 5) 5)
(test (gcd -4) 4)
(test (gcd 1071 1029) 21)
(test (gcd 1071 1029 14) 7)

(test (lcm2 25 30) 150)

(test (lcm 10) 10)
(test (lcm 25 30) 150)
(test (lcm -24 18 10) 360)
(test (lcm 14 35) 70)
(test (lcm 0 5) 0)
(test (lcm 1 2 3 4 5 6) 60)
