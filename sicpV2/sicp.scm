;;Working through SICP
;;Jeff Gonis
;;;;;;;;;;;;;;;CHAPTER 1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Exercise 1.3
(import (rnrs))
(import (srfi :27 random-bits))
(import (larceny benchmarking))
(define (j-square x)
  (* x x))

(define (ex13 x y z)
  (cond ((< x y z) (+ (j-square y) (j-square z)))
        ((< y x z) (+ (j-square x) (j-square z)))
        ((< z x y) (+ (j-square x) (j-square y)))
        (else (+ (j-square x) (j-square y)))))

;;Exercise 14
;;An applicative-order interpreter will try to evaluate the argument
;;to test (p) which will result in an infinite loop, whereas an normal
;;order interpreter would substitute (p) for y, do the test, and
;;return 0.
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve-guess guess x) x)))

(define (improve-guess guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- x (j-square guess))) 0.001))

(define (j-sqrt x)
  (sqrt-iter 1.0 x))

;;Exercise 1.6
;;When Alyssa attemps to use the new if to calculate square roots she
;;enters into an infinite recursion, because the interpreter uses
;;applicative order evaluation and so it is always trying to evaluate
;;the else-clause which is a recursive call to sqrt-iter.  Thus the
;;code never enters the new-if procedure.

;;Exercise 1.7
;;Trying to test with a sentinel value means that we can never
;;correctly find the square root of numbers that are smaller than the
;;sentinel value itself.  Implementing good-enough? in terms of the
;;change in the guess with each iteration will also not be sufficient
;;for testing small values, as the change between two guesses could be
;;larger than the sentinel value, even though the number being guessed
;;themselves and very small and thus the guess is not
;;accurate. However if we define the good-enough? test as always being
;;in terms of a change that is a fixed fraction of the guess value (ie
;;1/1000th) then we could have better results than just the
;;comparison.
(define (good-enough-change? guess old-guess x)
  (< (abs (- guess old-guess)) (/ guess 100000)))

(define (sqrt-iter-change guess old-guess x)
  (if (good-enough-change? guess old-guess x)
      guess
      (sqrt-iter-change (improve-guess guess x) guess x)))

(define (j-sqrt-change x)
  (sqrt-iter-change 1.0 0.0 x))
;;Actually, it turns out that this produces far better answers for
;;small numbers, as it is in terms of the guesses change, which can be a small
;;number itself, rather than a predefined sentinel value.

;;Exercise 1.8
;;Use the good-enough? function based on the change between successive
;;guesses rather than the original one, because the original one is
;;defined in terms of squaring rather than cubing.
(define (j-cube-root-iter guess old-guess x)
  (if (good-enough-change? guess old-guess x)
      guess
      (j-cube-root-iter (improve-cube-guess guess x) guess x)))

(define (improve-cube-guess guess x)
  (/ (+ (/ x (j-square guess)) (* 2 guess)) 3))

(define (j-cube-root x)
  (j-cube-root-iter 1.0 0.0 x))

;;;;;;;;;;;;;;;;;;;; SECTION 1.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (j-factorial n)
  (if (or (= n 0) (= n 1))
      1
      (* n (j-factorial (- n 1)))))

(define (j-factorial-iterative n)
  (define (factorial-iterator product counter)
    (if (> counter n)
        product
        (factorial-iterator (* product counter)
                            (+ counter 1))))
  (factorial-iterator 1 1))

;;Exercise 1.9
;;The first procedure is defined recursively, because we keep passing
;;the result of a recursive call to the + procedure to the inc
;;procedure, meaning that we are "deferring" the inc operation, and
;;thus we will have to keep these records on the stack.

;;The second function is iterative, because the state of the procedure
;;is captured in the two parameters a and b, which is modified before
;;each recursive call.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Section 1.2.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (iterative-fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

;;;;;;;;;;;;;;;;;;;;;;;;;; Example ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Counting change
;;Number of ways to count change =
;;1) number of ways to count change using all but the first type of
;;coin.
;;2) number of ways to count a-d amount of change using all n coins,
;;where d is the denomination of the first kind of coin.

(define (count-change amount)
  (define (first-denomination n)
    (cond ((= n 1) 1)
          ((= n 2) 5)
          ((= n 3) 10)
          ((= n 4) 25)
          ((= n 5) 50)))
  (define (cc amount num-denominations)
    (cond ((= amount 0) 1)
          ((< amount 0) 0)
          ((= num-denominations 0) 0)
          (else (+ (cc amount (- num-denominations 1))
                   (cc (- amount (first-denomination num-denominations))
                       num-denominations)))))
  (cc amount 5))

;;Exercise 1.11
(define (ex111 n)
  (cond ((< n 3) n)
        (else (+ (ex111 (- n 1))
                 (* 2 (ex111 (- n 2)))
                 (* 3 (ex111 (- n 3)))))))

(define (ex111-iterative n)
  (define (ex111-iter a b c count)
    (cond ((= count 0) a)
          (else (ex111-iter b c (+ c (* 2 b) (* 3 a)) (- count 1)))))
  (ex111-iter 0 1 2 n))

;;Ex 1.12
(define (pascal-triangle row column)
  (cond ((and (= row 1) (= column 1)) 1)
        ((< column 1) 0)
        ((> column row) 0)
        (else (+ (pascal-triangle (- row 1) (- column 1))
                 (pascal-triangle (- row 1) column)))))

;;Section 1.2.4 Exponentiation
(define (j-expt b n)
  (if (= n 0)
      1
      (* b (j-expt b (- n 1)))))

(define (j-expt-iter b n)
  (define (j-expt-helper b counter total)
    (if (= counter 0)
        total
        (j-expt-helper b (- counter 1) (* total b))))
  (j-expt-helper b n 1))

(define (fast-expt b n)
  (define (j-square a)
    (* a a))
  (cond ((= n 0) 1)
        ((even? n) (j-square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;;Ex 1.16 Create an iterative exponentiation process that is also
;;fast, by using the successive squaring technique.
(define (fast-expt-iter b n)
  (define (fast-expt-helper b counter total)
    (cond ((= counter 0) total)
          ((odd? counter) (fast-expt-helper b (- counter 1) (* total b)))
          (else (fast-expt-helper (* b b) (/ counter 2) total))))
  (fast-expt-helper b n 1))

;;Ex 1.17
(define (fast-mult a b)
  (define (j-double a)
    (+ a a))
  (define (j-halve a)
    (/ a 2))
  (cond ((= b 0) 0)
        ((even? b) (j-double (fast-mult a (j-halve b))))
        (else (+ a (fast-mult a (- b 1))))))
;;Ex 1.18
;;Create an iterative version of fast-mult that uses successive
;;adding, doubling, and halving.
(define (fast-mult-iter a b)
  (define (j-double a)
    (+ a a))
  (define (j-halve a)
    (/ a 2))
  (define (fast-mult-helper a b total)
    (cond ((= b 0) total)
          ((odd? b) (fast-mult-helper a (- b 1) (+ total a)))
          (else (fast-mult-helper (j-double a) (j-halve b) total))))
  (fast-mult-helper a b 0))

;;Ex 1.19


;;;;;;;;;;;;;;;; Section 1.2.5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (j-gcd a b)
  (if (= b 0)
      a
      (j-gcd b (mod a b))))


;;;;;;;;;;;;;;;; Section 1.2.6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (smallest-divisor n)
  (define (square n)
    (* n n))
  (define (divides? a b)
    (= (mod b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (mod (j-square (expmod base (/ exp 2) m))
                 m))
        (else (mod (* base (expmod base (- exp 1) m))
                      m))))

(define (fermat-test n)
  (define  (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? n times)
  (cond ((= 0 times) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

;;Ex 1.21
;;Smallest divisor of 199 = 199
;;Smallest divisor of 1999 = 1999
;;Smallest divisor of 19999 = 7

;;Ex 1.22
(define (search-for-primes start-at)
  (define (search-for-primes-helper current found-primes)
    (cond ((> (length found-primes) 2) found-primes)
          (else (cond ((prime? current)
                       (search-for-primes-helper
                        (+ current 1)
                        (cons current found-primes)))
                       (else (search-for-primes-helper
                              (+ current 1) found-primes))))))
  (search-for-primes-helper start-at '()))

;;Time to test for primes running in larceny on a 2.9 Ghz i7
;;searching for primes greater than 1000000000: 161 ms
;;searching for primes greater than 10000000000: 2737 ms
;;searching for primes greater than 100000000000:  12273 ms
;;searching for primes greater than 1000000000000:  41040 ms
;;searching for primes greater than 10000000000000:  162743 ms
;;This does not exactly increase by sqrt(10) but at the higher numbers
;;it does, going up by roughly 3-4x with each magnitude of 10 added,
;;which makes sense, as sqrt(10) is ~3.16

;;Ex 1.23
(define (faster-smallest-divisor n)
  (define (square n)
    (* n n))
  (define (next n)
    (cond ((= n 2) 3)
          (else (+ n 2))))
  (define (divides? a b)
    (= (mod b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (find-divisor n 2))

(define (better-prime? n)
  (= (faster-smallest-divisor n) n))

(define (better-search-for-primes start-at)
  (define (search-for-primes-helper current found-primes)
    (cond ((> (length found-primes) 2) found-primes)
          (else (cond ((better-prime? current)
                       (search-for-primes-helper
                        (+ current 1)
                        (cons current found-primes)))
                       (else (search-for-primes-helper
                              (+ current 1) found-primes))))))
  (search-for-primes-helper start-at '()))

;;Time to test for primes running in larceny on a 2.9 Ghz i7
;;searching for primes greater than 1000000000: 80 ms
;;searching for primes greater than 10000000000: 1402 ms
;;searching for primes greater than 100000000000:  6453 ms
;;searching for primes greater than 1000000000000:  20623 ms
;;searching for primes greater than 10000000000000:  87359 ms
;;Using the modified smallest-divisor test we see times that are
;;almost exactly 1/2 of the previous times, which is what we expected
;;to see, as 1/2 as many divisions are being performed.

;;Exercise 1.24
(define (fermat-search-for-primes start-at)
  (define (search-for-primes-helper current found-primes)
    (cond ((> (length found-primes) 2) found-primes)
          (else (cond ((fast-prime? current 16)
                       (search-for-primes-helper
                        (+ current 1)
                        (cons current found-primes)))
                       (else (search-for-primes-helper
                              (+ current 1) found-primes))))))
  (search-for-primes-helper start-at '()))

;;Time to test for primes running in larceny on a 2.9 Ghz i7
;;searching for primes greater than 1000000000: 47 ms
;;searching for primes greater than 10000000000: 122 ms
;;searching for primes greater than 100000000000:  131 ms
;;searching for primes greater than 1000000000000:  138 ms
;;searching for primes greater than 10000000000000:  215 ms


;;;;;;;;;;;;;;;;;;;;;;; SECTION 1.3 ;;;;;;;;;;;;;;;;;;;;;;;
(define (sum-integers a b)
  (cond ((> a b) 0)
        (else (+ a (sum-integers (+ a 1) b)))))

(define (sum-cubes a b)
  (define (cube n)
    (* n n n))
  (cond ((> a b) 0)
        (else (+ (cube a) (sum-cubes (+ a 1) b)))))

;;;;;;;;;;;;;;;;;;;;;; CHAPTER 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (add-rat x y)
  (reduced-make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (reduced-make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (reduced-make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (reduced-make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  (cons n d))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define (reduced-make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

;;Ex 2.1
(define (improved-reduced-make-rat n d)
  (let ((g (gcd n d)))
    (cond ((< d 0) (cons (/ (* n -1) g)
                         (/ (* d -1) g)))
          (else (cons (/ n g)
                      (/ d g))))))
;;Ex 2.2
(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point point)
  (display "(")
  (display (x-point point))
  (display ", ")
  (display (y-point point))
  (display ")")
  (newline))

(define (midpoint-segment segment)
  (make-point (average (x-point (start-segment segment))
                       (x-point (end-segment segment)))
              (average (y-point (start-segment segment))
                       (y-point (end-segment segment)))))

;;Ex 2.3
(define (make-rect bottom-left top-right)
  (cons bottom-left top-right))

(define (bottom-left rect)
  (car rect))

(define (top-right rect)
  (cdr rect))

(define (rect-perimeter rect)
  (+ (* 2 (- (x-point (top-right rect)) (x-point (bottom-left rect))))
     (* 2 (- (y-point (top-right rect)) (y-point (bottom-left rect))))))

(define (rect-area rect)
  (* (- (y-point (top-right rect)) (y-point (bottom-left rect)))
     (- (x-point (top-right rect)) (x-point (bottom-left rect)))))

(define (alt-make-rect bottom-left width height)
  (cons bottom-left (cons width height)))

(define (alt-bottom-left rect)
  (car rect))

(define (rect-height rect)
   (cdr (cdr rect)))

(define (rect-width rect)
  (car (cdr rect)))

(define (alt-top-right rect)
  (make-point (+ (rect-width rect) (x-point (bottom-left rect)))
              (+ (rect-height rect) (y-point (bottom-left rect)))))



;;;;;;;;;;;;;;;;;; Section 2.1.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

