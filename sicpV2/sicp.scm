;;Working through SICP
;;Jeff Gonis
;;;;;;;;;;;;;;;CHAPTER 1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Exercise 1.3
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
        (factorial-iterator (* product counter) (+ counter 1))))
  (factorial-iterator 1 1))


