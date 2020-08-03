;;Adding a comment
(define jfib
  (lambda (x)
    (cond ((= x 0) 0)
	  ((= x 1) 1)
	  (else (+ (jfib (- x 1)) (jfib (- x 2)))))))

(assert (= (jfib 0) 0))
(assert (= (jfib 1) 1))
(assert (= (jfib 11) 89))

(define fast-fib
  (lambda (n)
    (letrec ((iter-fib (lambda (a b count)
			 (cond ((= count 0) b)
			       (else (iter-fib (+ a b) a 
					       (- count 1)))))))
      (iter-fib 1 0 n))))

(assert (= (fast-fib 0) 0))
(assert (= (fast-fib 1) 1))
(assert (= (fast-fib 11) 89))

;;Adding a naive power function
(define jpow
  (lambda (x y)
    (cond ((= y 0) 1)
	  (else (* x (jpow x (- y 1)))))))

(assert (= (jpow 2 0) 1))
(assert (= (jpow 2 17) 131072))

(define (recursiveFactorial n)
  (if (= n 1)
      1
      (* n (recursiveFactorial (- n 1)))))

(assert (= (recursiveFactorial 1) 1))
(assert (= (recursiveFactorial 7) 5040))

(define (iterativeFactorial n)
  (letrec ((factIter (lambda (n counter sum)
		       (if (= n counter)
			   (* sum counter)
			   (factIter n (+ counter 1) (* sum counter))))))
  (factIter n 1 1)))

(assert (= (iterativeFactorial 1) 1))
(assert (= (iterativeFactorial 7) 5040))

;;A test to compare new types of factorial functions against the known
;;good recursive version.  Test it for the factorial values 1 to 100
(define (factorialTest newDef)
  (letrec ((factorialTestIter (lambda (newDef start end)
				(if (= start end)
				    (factorialTestInst newDef start)
				    (and (factorialTestInst newDef start)
					 (factorialTestIter newDef
							    (+ start 1) end)))))
	   (factorialTestInst (lambda (newDef n)
				(= (newDef n) (recursiveFactorial n)))))
    (factorialTestIter newDef 1 100)))
;;Make sure that our iterative implementation of the factorial function
;;returns the same results as the recursive version.
(assert (factorialTest iterativeFactorial))

;;Exercise 117
(define (j* a b)
  (if ( = b 0)
      0
      (+ a (j* a (- b 1)))))

(assert (= (j* 5 10) (* 5 10)))

(define (double a)
  (* a 2))

(define (halve a)
  (/ a 2))

(define (fast-* a b)
  (cond ((= 0 b) 0)
        ((even? b) (double (fast-* a (halve b))))
        (else (+ a (fast-* a (- b 1))))))


;;Exercise 118
(define (fast-iter-* a b)
  (fast-iter-*-iter a b 0))

(define (fast-iter-*-iter a b sum)
  (cond ((= b 0) sum)
        ((even? b) (fast-iter-*-iter (double a) (halve b) sum))
        (else (fast-iter-*-iter a (- b 1) (+ sum a)))))

;;A test to compare new types of multiplication, multiplying
;;together the numbers from 1 to 500 and then comparing the results
;;against the built-in multiplication function.
(define (multTest newDef)
  (letrec ((multTestIter (lambda (newDef start end)
			     (if (= start end)
				 (multTestInst newDef start (- end start))
				 (and (multTestInst newDef start (- end start)) 
				      (multTestIter newDef (+ start 1) end)))))
	   (multTestInst (lambda (newDef a n)
			   (= (newDef a n) (* a n)))))
    (multTestIter newDef 1 500)))

(assert (multTest fast-iter-*))
(assert (multTest j*))
(assert (multTest fast-*))

;;Exercise 119
(define (recursiveExpt a n)
  (if (= n 0)
      1
      (* a (recursiveExpt a (- n 1)))))

(define (iterativeExpt a n)
  (expt-iter a n 1))

(define (expt-iter a n sum)
  (if (= n 0)
      sum
      (expt-iter a (- n 1) (* sum a))))

(define (fast-recurs-expt a n)
  (cond ((= n 0) 1)
        ((even? n) 
         (let ((temp (fast-recurs-expt a (/ n 2))))
	   (* temp temp)))
        (else (* a (fast-recurs-expt a (- n 1))))))

(define (fast-iter-expt a n)
  (fast-iter-expt-iter a n 1))

(define (fast-iter-expt-iter a n sum)
  (cond ((= n 0) sum)
        ((even? n) (fast-iter-expt-iter (* a a) (/ n 2) sum))
        (else (fast-iter-expt-iter a (- n 1) (* sum a)))))


;;A test to compare new types of exponentiation
(define (expTest newDef)
  (letrec ((expTestIter (lambda (newDef start end)
			   (if (= start end)
			       (expTestInst newDef start (- end start))
			       (and (expTestInst newDef start (- end start)) 
				    (expTestIter newDef (+ start 1) end)))))
	   (expTestInst (lambda (newDef a n)
			  (= (newDef a n) (recursiveExpt a n)))))
  (expTestIter newDef 1 500)))

(assert (expTest fast-iter-expt))
(assert (expTest fast-recurs-expt))
(assert (expTest iterativeExpt))

;;LCS exercise
(define lcs
  (lambda (xs ys)
    (lcs-format (lcs-revrs xs ys))))

(define lcs-format
  (lambda (list)
    (cond ((null? list) '())
          ((list? (car list)) (cons (lcs-format (car list))
				    (lcs-format (cdr list))))
          (else (car list)))))

(define lcs-revrs
  (lambda (xs ys)
    (reverse (lcs-recur xs ys)))) 

(define lcs-recur
  (lambda (xs ys)
    (cond ((and (not (null? xs)) (not (null? ys)))
           (cond ((eq? (last xs) (last ys))
		  (cons (cons (last xs) '()) (lcs-recur
					      (reverse 
					       (cdr (reverse xs)))
					      (reverse 
					       (cdr (reverse ys))))	))
                 ((>= (length (lcs-recur xs 
					 (reverse (cdr (reverse ys)))))
		      (length (lcs-recur 
			       (reverse (cdr (reverse xs))) ys)))
                  (lcs-recur xs (reverse (cdr (reverse ys))) ))
                 (else (lcs-recur (reverse (cdr (reverse xs))) ys))))
          (else '()))))

;;Exercise 125
;;Euclid's algorithm for determining the greatest common denominator of
;;two numbers.
(define jgcd
  (lambda (a b)
    (cond ((= b 0) a)
          (else (jgcd b (remainder a b))))))

;;Some tests to make sure that the above implementation is correct (relatively).
(assert (= (jgcd 54 24) 6))
(assert (= (jgcd 42 56) 14))
(assert (= (jgcd 18 84) 6))


;;Lame's Theorem If Euclid's algorithm requires k steps to compute the
;;GCD of some pair, then the smaller number of the pair must be greater
;;than or equal to the kth Fibonacci number.
(define gcd-steps
  (lambda (a b)
    (letrec ((gcd-count-steps (lambda (a b steps)
				(cond ((= b 0) steps)
				      (else (gcd-count-steps 
					     b
					     (remainder a b)
					     (+ steps 1)))))))
      (gcd-count-steps a b 0))))

(define test-lames-theorem
  (lambda (larger-number)
    (letrec ((test-theorem (lambda (a b)
			     (cond ((<= b a) 
				    (and (test-theorem a (+ b 1))
					 (>= b (fast-fib
						(gcd-steps a b)))))
				   (else #t)))))
      (cond ((> larger-number 0)
	     (and (test-theorem larger-number 1)
		  (test-lames-theorem (- larger-number 1))))
	    (else #t)))))
;;Execute the above function to (somewhat) test Lame's theorem
;;regarding the number of steps needed to calculate GCD.
(assert (test-lames-theorem 100))

;;1.2.6 Testing for primality
;;This is a function that takes an input number and tries to find its
;;smallest divisor, which may be the number itself, if it is prime.
(define smallest-divisor
  (lambda (n)
    (letrec ((find-divisor
	      (lambda (n test-divisor)
		(cond ((> (jsquare test-divisor) n) n)
		      ((divides? test-divisor n) test-divisor)
		      (else (find-divisor n (+ test-divisor 1))))))
	     (jsquare (lambda (n)
			(* n n)))
	     (divides? (lambda (a b)
			 (= (modulo b a) 0))))
      (find-divisor n 2))))

(assert (= (smallest-divisor 2) 2))
(assert (= (smallest-divisor 4) 2))
(assert (= (smallest-divisor 3) 1))
(assert (= (smallest-divisor 10) 2))
(assert (= (smallest-divisor 17) 17))
(assert (= (smallest-divisor 99) 3))
(assert (= (smallest-divisor 111) 3))

(define faster-smallest-divisor
  (lambda (n)
    (letrec ((find-divisor (lambda (n test-divisor)
			     (cond ((> (square test-divisor) n) n)
				   ((divides? test-divisor n) 
				    test-divisor)
				   (else
				    (find-divisor 
				     n
				     (+ test-divisor 2)))))))
      (cond ((> (square 2) n) n)
	    ((divides? 2 n) 2)
	    (else (find-divisor n 3))))))

;;Give an input number, this will attempt to find a number for which it 
;;is the smallest divisor.  In some cases, such as 9, this will never
;;be true as you can always divide the number by 3 if you can divide
;;it by 9. 
(define find-smallest-num-with-divisor
  (lambda (divisor)
    (letrec ((find-smallest-helper 
	      (lambda (current divisor)
		(cond ((and
			(= (smallest-divisor current)
			   divisor)
			(not (= current divisor)))
		       current)
		      ((> current 1000000) #f)
		      (else
		       (find-smallest-helper
			(+ current 1) divisor))))))
      (find-smallest-helper 1 divisor))))

;;This is a function I wrote to mess around with random number
;;generation and the modulo operation.  It just does a bunch of
;;iterations and then generates two random numbers and mods them.  This
;;will be usefull when I am testing my fermat prime number function.
(import (srfi srfi-27))
(define random-mod-test
  (lambda (upper-limit iterations)
    (letrec ((random-mod-test-iter 
	      (lambda (upper-limit iterations count)
		(cond ((< count iterations)
		       (begin
			 (let* ((a
				 (+ (random-integer
				     upper-limit) 1))
				(b
				 (+ (random-integer a)
				    1)))
			   (display "a: ")
			   (display a)
			   (display " b: ")
			   (display b)
			   (display " a mod b: ")
			   (display (modulo a b))
			   (newline))
			 (random-mod-test-iter
			  upper-limit
			  iterations
			  (+ count 1))))))))
      (random-mod-test-iter upper-limit iterations 0))))

(define divides?
  (lambda (a b)
    (= (modulo b a) 0)))

(define square
  (lambda (x)
    (* x x)))

(define slow-prime?
  (lambda (n)
    (= n (smallest-divisor n))))

(define naive-prime?
  (lambda (n)
    (cond ((= n 1) #f)
	  (else (= n (faster-smallest-divisor n))))))

;;Fermat Test for prime numbers
;;If n is a prime number and a is any positive integer less than
;;n, then a raised to the nth power is congruent to a modulo n.
;;So basically remainder of (a^n / n) = a

;;Therefore if a number fails this test, it is not prime.  If it
;;passes the test there is a chance it might be prime.  We can
;;show that if a number passes the fermat test for a given a, there
;;is a roughly 1/2 chance it is prime.  If it passes a second test,
;;there is a 3/4 chance it is prime.  By repeatedly applying the
;;test we can become as sure as we would like.

;;import the srfi responsible for providing random number
;;generation.  I am using Ypsilon scheme, so in this case it is
;;load with (import (srfi srfi-27))
(import (srfi srfi-27))
(define expmod
  (lambda (base exp m)
    (cond ((= exp 0) 1)
          (else (modulo (fast-iter-expt base exp) m)))))

(define alt-expmod
  (lambda (base exp m)
    (cond ((= exp 0) 1)
	  ((even? exp)
	   (modulo (square (alt-expmod base (/ exp 2) m)) m))
	  (else (modulo (* base (alt-expmod base (- exp 1) m)) m)))))

;;This function applies the actual fermat test to a number by applying
;;the expmod function and then determining if it is equal to the
;;original number.

(define generate-new-random
  (lambda (lower-limit upper-limit already-generated-randoms)
    (let ((new-rand (+ (random-integer upper-limit) lower-limit)) )
      (cond ((member new-rand already-generated-randoms)
	     (generate-new-random lower-limit
				  upper-limit
				  already-generated-randoms))
	    (else new-rand)))))

(define fermat-test
  (lambda (n)
    (letrec ((try-it (lambda (a)
		       (= (alt-expmod a n n) a))))
      (try-it (+ 1 (random-integer (- n 1)))))))

(define fast-prime?
  (lambda (n)
    (cond ((<= n 53) (naive-prime? n))
	  (else	(apply-test n 0 53 '())))))

(define apply-test
  (lambda (n times-applied times-to-apply tested-nums)
    (let ((rand (generate-new-random 1 n tested-nums)))
      (cond ((= times-applied times-to-apply) n)
	    ((fermat-test n)
	     (apply-test n
			 (+ times-applied 1)
			 times-to-apply
			 (append tested-nums (list rand))))
	    (else #f)))))

;;This is a quick function to determine for a give number of Fermat
;;tests, what a pessimistic certainty would be given the number of
;;tests we could perform.  Eg: 20 tests gives us a certainty of
;;99.99990463256836%
;;53 Tests gives a certainty of .9999999999999999
(define test-certainty
  (lambda (times-to-perform-test)
    (letrec ((perform-test (lambda (times-to-perform)
			     (cond ((< times-to-perform 1) 0)
				   ((= times-to-perform 1) 1/2)
				   (else
				    (* 1/2 
				       (perform-test
					(- times-to-perform 1))))))))
      (inexact (- 1 (perform-test times-to-perform-test))))))

;;This is a function that will test every integer up to a certain 
;;number to determine if it is prime.  If it is, then the program will 
;;display the integer and continue on.  You can pass in the function 
;;that you would like it to use when testing for primes to compare the 
;;speed of various methods for calculating these primes.
(define test-for-primes
  (lambda (test-func upper-limit)
    (letrec ((prime-tester
	      (lambda (current)
		(cond ((< current upper-limit)
		       (cond ((is-prime? current test-func)
			      (begin (display current)
				     (newline)
				     (prime-tester (+ current 1))))
			     (else (prime-tester (+ current 1)))))
		      (else (begin (display "Finished testing")
				   (newline))))))
	     (is-prime? (lambda (n test-func)
			  (test-func n))))
      (prime-tester 2))))

;;This function will return a count of the number of primes that it has
;;in the interval 2-upper-limit. It will also print off all of the 
;;numbers that it finds to be prime in this interval.  You can pass in 
;;the function that you would like it to use to test for primality to 
;;compare the speed of various methods.
(import (srfi srfi-27))
(define count-primes
  (lambda (test-func upper-limit output-func)
    (letrec ((prime-tester
	      (lambda (current prime-list)
		(cond ((< current upper-limit)
		       (cond ((is-prime? current test-func)
			      (prime-tester 
			       (+ current 1)
			       (append prime-list
				       (cons current '()))))
			     (else (prime-tester (+ current 1)
						 prime-list))))
		      (else (output-func prime-list)))))
	     (is-prime? (lambda (n test-func)
			  (test-func n))))
      (prime-tester 2 '()))))

;;If I pass in a list of primes I would like to print out each element
;;of the list on its own line and then state the total number of
;;elements in the list.
(define print-primes
  (lambda (prime-list)
    (let ((list-length (length prime-list)))
      (letrec ((print-list (lambda (primes)
			     (cond ((not (null? primes))
				    (begin (display (car primes))
					   (newline)
					   (print-list
					    (cdr primes))))
				   (else
				    (display "number of primes found: ")
				    (display list-length)
				    (newline))))))
	(print-list prime-list)))))

;;This function does not do any io, but merely returns the length of
;;list of prime elements.
(define return-number-of-primes
  (lambda (prime-list)
    (length prime-list)))

;;Exercise 1.21
;;Use the smallest-divisor procedure to find the smallest-divisor of 
;;each of the following numbers: 199, 1999, 19999
(smallest-divisor 199) ;;answer 199
(smallest-divisor 1999) ;;answer 1999
(smallest-divisor 19999) ;;answer 7

;;Exercise 1.22
;;Use the "time" procedure (notes this is the procedure ypsilon scheme
;;provides to determine runtime of a procedure.
;;Write a procedure called search-for-primes that checks the primality
;;of consecutive odd integers in a specified range.
;;The statement (import (ypsilon time)) is used to import ypsilon's
;;benchmarking library which will print out the amount of time used
;;by a function
(import (ypsilon time))

(define search-for-primes-old
  (lambda (lower-bound upper-bound test-func)
    (letrec ((prime-search
	      (lambda (current)
		(cond ((< current upper-bound)
		       (cond ((test-func current)
			      (begin (display "number: ")
				     (display current)
				     (display #\tab)
				     (time (test-func current))
				     (prime-search (+ current 2))))
			     (else (prime-search (+ current 2)))))))))
      (cond ((= lower-bound 2) (begin (display "number: ")
				      (display lower-bound)
				      (display #\tab)
				      (time (test-func lower-bound))
				      (prime-search (+ lower-bound 1))))
	    ((even? lower-bound) (prime-search (+ lower-bound 1)))
	    (else (prime-search lower-bound))))))

;;This is new code that I am writing to take care generating the prime
;;search with a new function to answer the subsequent questions of the
;;book
(define search-for-primes
  (lambda (num-of-results lower-bound test-func)
    (letrec ((prime-search
	      (lambda (current-num current-num-found)
		(cond ((< current-num-found num-of-results)
		       (cond ((test-func current-num)
			      (begin
				(display "prime found: ")
				(display current-num)
				(newline)
				(prime-search (+ current-num 1)
					      (+ current-num-found 1))))
			     (else (prime-search 
				    (+ current-num 1)
				    current-num-found))))))))
      (time (prime-search lower-bound 0)))))

;;Answer to Exercise 1.22
;;Using the slow naive prime search, which doesn't differentiate 
;;based on even numbers.
(define slow-search-for-primes
  (lambda (num-of-results lower-bound)
    (search-for-primes num-of-results lower-bound slow-prime?)))

;;I am increasing the numbers that we are asked to search for by a 
;;factor of 1 million, because my PC is roughly 1,000,000x faster 
;;than when the text was written and timing the procedure for 
;;the original values registers no time being spent on the 
;;calculations.
;;
;;I will call the function (slow-search-for-primes 3 N)
;;N = 1000000000    0.031250 s
;;N = 10000000000   0.296865 s - 10x slower than N = 1000000000
;;N = 100000000000  0.734352 s - 2.47x slower than N = 10000000000
;;N = 1000000000000 2.203054 s - 2.99x slower than N = 100000000000

;;The first difference is much greater than the sqrt(10) difference 
;;that we expect but this is probably because 10000000000 cannot fit 
;;into a 32-bit int and we had to switch over to bignum calculations 
;;which are much slower. The rest of the differences appear to be 
;;fairly close to this expected value.



;;Exercise 1.23
;;Now I will use the prime search function again, but this time I will
;;use the optimized naive search.  This discards with tests of even
;;numbers after testing division by 2.
(define optimized-search-for-primes
  (lambda (num-of-results lower-bound)
    (search-for-primes num-of-results lower-bound naive-prime?)))
;;I will call the function (slow-search-for-primes 3 N)
;;N = 1000000000    0.015625 s
;;N = 10000000000   0.140627 s - 9x slower than N   = 1000000000
;;N = 100000000000  0.453135 s - 3.22x slower than N = 10000000000
;;N = 1000000000000 1.218773 s - 2.69x slower than N = 100000000000

;;Compared to the un-optimized version of the naive prime test
;;N = 1000000000    0.015625 s - 2.00x faster than N = 1000000000
;;N = 10000000000   0.140627 s - 2.11x faster than N = 10000000000
;;N = 100000000000  0.453135 s - 1.62x faster than N = 100000000000
;;N = 1000000000000 1.218773 s - 1.81x faster than N = 1000000000000
;;This new form of the prime test did appear to be roughly 2x as fast
;;as the un-optimized version which fit well with our predictions as
;;it is performing roughly half as many operations when it tests.

;;Exercise 1.24
;;Now I will call the function fast-search-for-primes using the 
;;fermat test instead of the naive prime search test.
(define fast-search-for-primes
  (lambda (num-of-results lower-bound)
    (search-for-primes num-of-results lower-bound fast-prime?)))
;;I will call the function (fast-search-for-primes 3 N)
;;N = 1000000000     0.000001 s
;;N = 10000000000    0.015623 s - 15623x slower than N  = 1000000000
;;N = 100000000000   0.015625 s - 1.0001x slower than N = 10000000000
;;N = 1000000000000  0.015627 s - 1.0001x slower than N = 100000000000
;;The fermat test completes the testing much more quickly that the naive
;;tests and also barely grows in run-time as the integer being
;;evaluated grows 

;;Exercise 1.25
;;Alyssa P. Hacker's modification will actually make the test many times
;;slower because it will now involve doing the remainder operation on a
;;potentially enormous number, which will consume much more memory 
;;and time.

;;Exercise 1.26
;;Louis' code contains two calls to expmod in order to square the value
;;returned.  Because this is a recursive call, an enormouse amount of
;;computation is now duplicated to receive the same value.  In each call
;;we divide the value of exp by 2.  Thus 6 calls are made if exp is 16,
;;5 calls are made if exp is 8, 4 calls if exp is 4, etc.  32 will 
;;involve 7 calls.  This is a log(n) process.  If we call the log(n) 
;;process twice for each call we generate n recursive calls to the 
;;function.  Starting with exp = 32, this generates two calls with 
;;exp = 16.  Each call to exp = 16 generates two calls to exp = 8, 
;;giving 4 calls totals.  This translates to 8 calls with exp = 8, 
;;16 calls with exp = 4, 32 calls with exp = 2, and then each of those 
;;32 calls completes the calculation, calling exp = 1, and in turn 
;;exp = 0.  This means that n calls are made to expmod making the 
;;process O(n).

;;Exercise 1.27
;;Define a test for Carmichael numbers, then test it on the numbers
;;561, 1105, 1729, 2465, 2821, and 6601.  These are the smallest 6.
(define carmichael-test
  (lambda (n)
    (letrec ((do-test
	      (lambda (n current)
		(cond ((< current n)
		       (cond ((= (alt-expmod current n n) current)
			      (do-test n (+ current 1)))
			     (else #f)))
		      (else #t)))))
      (do-test n 1))))
;;Create a function to find carmichael numbers, with the number to 
;;find and the starting bound.
(define search-for-carmichael
  (lambda (num-of-results lower-bound)
    (letrec ((search-helper
	      (lambda (start)
		(cond ((> num-of-results 0)
		       (cond ((and (carmichael-test start)
				   (not (naive-prime? start)))
			      (begin
				(display "number found: ")
				(display start)
				(newline)
				(search-for-carmichael 
				 (- num-of-results 1)
				 (+ start 2))))
			     (else
			      (search-for-carmichael
			       num-of-results (+ start 2)))))))))
      (cond ((= (modulo lower-bound 2) 0)
	     (search-helper (+ lower-bound 1)))
	    ((< lower-bound 2) (search-helper 3))
	    (else (search-helper lower-bound))))))

(define naive-search-for-carmichael
  (lambda (num-of-results lower-bound)
    (cond ((> num-of-results 0)
	   (cond ((and (carmichael-test lower-bound)
		       (not (naive-prime? lower-bound)))
		  (begin
		    (display "number found: ")
		    (display lower-bound)
		    (newline)
		    (naive-search-for-carmichael (- num-of-results 1)
						 (+ lower-bound 1))))
		 (else
		  (naive-search-for-carmichael
		   num-of-results (+ lower-bound 1))))))))

;;Just a quick experiment to see what sort of values would be counted
;;as "true" by the cond special form.
(define conditional-test
  (lambda (n)
    (cond (n (begin (display "success")
		    (newline)))
	  (else (begin (display "failure")
		       (newline))))))

;;Exercise 1.28
;;Write a function that implements the miller-rabin test to determine
;;if a number is prime or not.
;;If a number n is prime and a is any positive integer less than n,
;;then a raised to the (n-1)st power is congruent to 1 modulo n.
;;Pick a random number a < n, raise a to (n-1)st power modulo n, using
;;expmod procedure.  Each time we perform the squaring procedure in
;;expmod, check to see if a nontrivial root of 1 modulo n has been
;;found.  Check for a number not equal to 1 or n-1, whose square is
;;equal 1 modulo n.  If such a nontrivial square root exists, then n
;;is not prime.

(define generate-random-list
  (lambda (list-size)
    (letrec ((gen-helper
	      (lambda (list-size new-list)
		(cond ((> list-size 0)
		       (gen-helper
			(- list-size 1)
			(append new-list (list 
					  (+ (random-integer 10) 1)))))
		      (else new-list)))))
      (gen-helper list-size '()))))

(define alt-generate-random-list
  (lambda (list-size)
    (letrec ((gen-helper
	      (lambda (list-size new-list)
		(cond ((> list-size 0)
		       (gen-helper (- list-size 1)
				   (cons
				    (+ (random-integer 10) 1)
				    new-list)))
		      (else new-list)))))
      (gen-helper list-size '()))))

(define generate-random-vector
  (lambda (vector-length)
    (letrec ((vec (make-vector vector-length))
	     (vec-initialize
	      (lambda (current-index)
		(cond ((> current-index 0)
		       (begin
			 (vector-set! vec (- current-index 1)
				      (+ (random-integer 10) 1))
			 (vec-initialize (- current-index 1))))
		      (else vec)))))
      (vec-initialize vector-length))))

;;generating a random list of 10,000 elements
;;  4.356469 real    3.168198 user    0.112007 sys
;;generating a random vector of 10,000 elements
;;  0.114209 real    0.076005 user    0.004 sys
;;generating a random vector of 10,000 elements and then casting it
;;to a list
;;  0.10639 real    0.064004 user    0.0 sys
;;generating a random list of 10,000 elements by cons'ing each
;;new element onto the front of the list
;;  0.122156 real    0.076005 user    0.0 sys
;;So it appears that append takes a large amount of time to add an
;;element to the back of the list.
;;Section 1.3 Formulating Abstractions with Higher-Order Predicates
;;Procedure that manipulate procedures are known as higher-order
;;procedures, and they can serve to express common patterns of
;;programming that can be specialized with other more specific
;;procedures.

;;Section 1.3.1 Procedures as Arguments
(define cube
  (lambda (a)
    (* a a a)))

(define sum-integers
  (lambda (start stop)
    (cond ((> start stop) 0)
	  (else (+ start (sum-integers (+ start 1) stop))))))

(define sum-cubes
  (lambda (start stop)
    (cond ((> start stop) 0)
	  (else (+ (cube start) (sum-cubes (+ start 1) stop))))))

(define pi-sum
  (lambda (start stop)
    (cond ((> start stop) 0)
	  (else (+ (/ 1.0 (* start (+ start 2)))
		   (pi-sum (+ start 4) stop))))))

(define iter-pi-sum
  (lambda (start stop)
    (letrec ( (iter-pi-sum-helper
	       (lambda (start stop total)
		 (cond ((> start stop) total)
		       (else (iter-pi-sum-helper
			      (+ start 4)
			      stop
			      (+ total 
				 (/ 1.0 (* start (+ start 2))))))))))
      (cond ((> start stop) 0)
	    (else (iter-pi-sum-helper start stop 0))))))

(define sum
  (lambda (term start next stop)
    (cond ((> start stop) 0)
	  (else
	   (+ (term start)
	      (sum term (next start) next stop))))))

(define integral
  (lambda (f a b dx)
    (let ((add-dx (lambda (x) (+ x dx))))
      (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))))

;;Exercise 1.29
;;Perform integration using Simpson's Rule for superior numerical
;;accuracy
(define better-integral
  (lambda (f a b n)
    (letrec ((h (/ (- b a) n)))
      (* (/ h 3)
	 (sum (lambda (input)
		(letrec ((result (f (+ a (* input h)))))
		  (cond ((= input 0) result)
			((= input n) result)
			((even? input) (* 2 result))
			(else (* 4 result)))))
	      0
	      (lambda (input) (+ input 1))
	      n)))))
;;Simpson's rule converges much more quickly that the previous integral
;;method and in the case of the cube method appears to provide an exact
;;answer.

;;Exercise 1.30
;;The current sum procedure is a linear recursive process.  Rewrite it
;;so that it can be performed iteratively.
(define iter-sum
  (lambda (term start next stop)
    (letrec ((iter-sum-helper
	      (lambda (term start next stop total)
		(cond ((> start stop) total)
		      (else (iter-sum-helper
			     term
			     (next start)
			     next
			     stop
			     (+ total (term start))))))))
      (cond ((> start stop) 0)
	    (else (iter-sum-helper term start next stop 0))))))

;;Exercise 1.31
;;Create an analogous procedure to sum, called product that returns
;;the product of the values of a function over a given range.
(define product
  (lambda (term start next stop)
    (cond ((> start stop) 1)
	  (else (* (term start) (product term (next start)
					 next stop))))))

(define iter-product
  (lambda (term start next stop)
    (letrec ((iter-product-helper
	      (lambda (term start next stop total)
		(cond ((> start stop) total)
		      (else (iter-product-helper
			     term
			     (next start)
			     next
			     stop
			     (* total (term start))))))))
      (cond ((> start stop) 0)
	    (else (iter-product-helper term start next stop 1))))))

;;Show how to define factorial in terms of product
(define higher-order-factorial
  (lambda (n)
    (product (lambda (n) n) 1 (lambda (n) (+ n 1)) n)))

(define iter-higher-order-factorial
  (lambda (n)
    (iter-product (lambda (n) n) 1 (lambda (n) (+ n 1)) n)))

;;Use product to compute approximations to pi using the formula:
;;Pi  2 * 4 * 4 * 6 * 6 * 8 ...
;;--  -------------------------
;;4   3 * 3 * 5 * 5 * 7 * 7 ...
(define higher-order-pi-approx
  (lambda (iterations)
    (/ (product (lambda (current)
		  (cond ((= current 1) 2)
			((even? current) (+ current 2))
			(else (+ current 1))))
		
		1
		(lambda (current) (+ current 1))
		iterations)
       (product (lambda (current)
		  (cond ((odd? current) (+ current 2))
			(else (+ current 1))))
		1
		(lambda (current) (+ current 1))
		iterations))))

(define iter-higher-order-pi-approx
  (lambda (iterations)
    (/ (iter-product (lambda (current)
		       (cond ((= current 1) 2)
			     ((even? current) (+ current 2))
			     (else (+ current 1))))
		     
		     1
		     (lambda (current) (+ current 1))
		     iterations)
       (iter-product (lambda (current)
		       (cond ((odd? current) (+ current 2))
			     (else (+ current 1))))
		     1
		     (lambda (current) (+ current 1))
		     iterations))))

(define iter-product-test
  (lambda (iterations)
    (iter-product (lambda (n)
		    (cond ((even? n) (/ 1 n))
			  (else n)))
		  1
		  (lambda (n) (+ n 1))
		  iterations)))

;;Exercise 1.32
;;Show that sum and product are both special cases of a general notion
;;called accumulate that combines a collection of terms, using a general
;;accumulation function
;;function signature (accumulate combiner null-value term a next b)
;;Accumulate takes as arguments the same term and range specifications
;;as sum and product together with a two-argument combiner function that
;;specifies how the current term is to be combined with the accumulation
;;of the preceeding terms, and a null-value that specifies what base 
;;value to use when the terms run out.
(define accumulate
  (lambda (combiner null-value term a next b)
    (cond ((> a b) null-value)
	  (else (combiner (term a)
			  (accumulate combiner
				      null-value
				      term
				      (next a)
				      next
				      b))))))

(define higher-order-sum
  (lambda (start stop)
    (accumulate + 0 (lambda (n) n) start (lambda (n) (+ n 1)) stop)))

(define higher-order-product
  (lambda (start stop)
    (accumulate * 1 (lambda (n) n) start (lambda (n) (+ n 1)) stop)))

;;Define an iterative version of accumulate
(define iter-accumulate
  (lambda (combiner null-value term a next b)
    (letrec ((iter-accumulate-helper
	      (lambda (combiner null-value term a next b total)
		(cond ((> a b) total)
		      (else (iter-accumulate-helper 
			     combiner
			     null-value
			     term
			     (next a)
			     next
			     b
			     (combiner (term a)
				       total)))))))
      (cond ((> a b) 0)
	    (else (iter-accumulate-helper combiner
					  null-value
					  term
					  a
					  next
					  b
					  null-value))))))

;;Define product and sum in terms of the iterative accumulate function
(define iter-higher-order-sum
  (lambda (start stop)
    (iter-accumulate + 0 
		     (lambda (n) n) 
		     start 
		     (lambda (n) (+ n 1)) stop)))

(define iter-higher-order-product
  (lambda (start stop)
    (iter-accumulate * 1 
		     (lambda (n) n) 
		     start 
		     (lambda (n) (+ n 1)) stop)))

;;Exercise 1.33
;;You can obtain an even more general version of accumulate by 
;;introducing the notion of a filter on the terms to be combined.  
;;A filter means to only combine those terms derived from values in 
;;the range that satisfy a specified condition.  Create a function 
;;called filtered-accumulate that takes an addition 1 argument 
;;predicate function that specifies the filter.

(define filtered-accumulate
  (lambda (filter combiner null-value term a next b)
    (cond ((> a b) null-value)
	  ((filter a) (combiner (term a)
				(filtered-accumulate filter
						     combiner
						     null-value
						     term
						     (next a)
						     next
						     b)))
	  (else (filtered-accumulate filter
				     combiner
				     null-value
				     term
				     (next a)
				     next
				     b)))))

(define filtered-sum
  (lambda (filter term start next stop)
    (cond ((> start stop) 0)
	  (else (filtered-accumulate 
		 filter + 0 term start next stop)))))

(define filtered-product
  (lambda (filter term start next stop)
    (cond ((> start stop) 0)
	  (else (filtered-accumulate 
		 filter * 1 term start next stop)))))

;;Section 1.3.2 Construction Procedures Using Lambda
;;Exercise 1.34
;;Define a procedure (define (f g) (g 2)).  What happens if we evaluate
;;(f f)
;;so evaluating this substitutes f for g in the body of function f.
;;We then need to evaluate (f 2).  This then causes us to substitute
;;2 for g in the body of f, giving us (2 2) and an error because 2
;;does not evaluate to a function.
(define f
  (lambda (g)
    (g 2)))

;;Section 1.3.3 Procedures as General Methods
(define close-enough?
  (lambda (x y)
    (< (abs (- x y)) 0.000000001)))

(define jsearch
  (lambda (func neg-point pos-point)
    (let ((midpoint (/ (+ neg-point pos-point) 2)))
      (cond ((close-enough? neg-point pos-point) midpoint)
	    (else
	     (let ((test-value (func midpoint)))
	       (cond ((positive? test-value)
		      (jsearch func neg-point midpoint))
		     ((negative? test-value)
		      (jsearch func midpoint pos-point))
		     (else midpoint))))))))

(define half-interval-method
  (lambda (func a b)
    (let ((a-value (func a))
	  (b-value (func b)))
      (cond ((and (negative? a-value) (positive? b-value))
	     (jsearch func a b))
	    ((and (negative? b-value) (positive? a-value))
	     (jsearch func b a))
	    (else (error "half-interval-method"
			 "Values are not of the opposite sign"))))))

;;Finding fixed points of functions.
;;A fixed point of a function is when f(x) = x.  We can try to find
;;this fixed point for some functions by repeatedly calling a function
;;using new argument values which we determine in basically a binary
;;search fashion
(define fixed-point
  (lambda (func first-guess)
    (letrec ((tolerance 0.00000001)
	     (close-enough? (lambda (num1 num2)
			      (< (abs (- num1 num2)) tolerance)))
	     (try (lambda (guess)
		    (let ((next (func guess)))
		      (cond ((close-enough? guess next) next)
			    (else (try next)))))))
      (try first-guess))))

(define javerage
  (lambda (x y)
    (/ (+ x y) 2)))

;;We need to average our current guess with the new guess that 
;;we will be generating so that we converge on a number.  When we 
;;average the equation we are doing a simple transformation of our 
;;original equation and so we still converge to the same answer.

;;The technique of average successive approximations is know as
;;average damping and is useful for making fixed-point searches
;;converge.
(define jsqrt
  (lambda (x)
    (fixed-point (lambda (y) (javerage y (/ x y))) 1.0)))

;;Exercise 1.35
;;Show that the golden ratio is a fixed point transformation of 
;;x = 1 + 1/x, and then computer this by means of the fixed-point
;;procedure.
(define jgolden-ratio
  (lambda ()
    (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)))

;;Exercise 1.36
;;Modify the fixed-point procedure so that it prints the sequence 
;;of approximations it generates.
(define verbose-fixed-point
  (lambda (func first-guess)
    (letrec ((tolerance 0.00000001)
	     (close-enough? (lambda (num1 num2)
			      (< (abs (- num1 num2)) tolerance)))
	     (try (lambda (guess)
		    (let ((next (func guess)))
		      (begin
			(display next)
			(newline)
			(cond ((close-enough? guess next) next)
			      (else (try next))))))))
      (try first-guess))))
;;Try to define the square root procedure in terms of the verbose
;;fixed point procedure to make sure that it is working
(define verbose-sqrt
  (lambda (x)
    (verbose-fixed-point (lambda (y) (javerage y (/ x y))) 1.0)))

(define ex136-no-average
  (lambda ()
    (verbose-fixed-point (lambda (x) (/ (log 1000) (log x))) 1.5)))

(define ex136-average
  (lambda ()
    (verbose-fixed-point (lambda (x) 
			   (javerage x (/ (log 1000) (log x)))) 
			 1.5)))
;;With average damping the convergence took roughly 1/4 the time 
;;that the non-damped version did.

;;Section 1.3.4 Procedures as Returned Values
(define average-damp
  (lambda (f)
    (lambda (x) (javerage x (f x)))))

;;Given that we have now looked at procedures that take and return
;;other procedures we can define a process for square and cube root
;;functions compactly.
(define compact-sqrt
  (lambda (x)
    (fixed-point (average-damp (lambda (y) (/ x y))) 1.0)))

(define compact-cbrt
  (lambda (x)
    (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0)))

;;We could infact use this equation to generate roots of any degree, 
;;although they may converge much more slowly, and we may need a
;;greater tolerance.
(define fixed-point-accuracy
  (lambda (tolerance)
    (lambda (func first-guess)
      (letrec ((close-enough? (lambda (num1 num2) 
				(< (abs (- num1 num2)) tolerance)))
	       (try (lambda (guess)
		      (let ((next (func guess)))
			(cond ((close-enough? guess next) next)
			      (else (try next)))))))
	(try first-guess)))))

;;for instance the 2nd (square) root of 4 would be
;;(arbitrary-root 4 2 0.00000001) with the amount of numerical
(define arbitrary-root
  (lambda (x root accuracy)
    ((fixed-point-accuracy accuracy)
     (average-damp (lambda (y) (/ x (fast-iter-expt y (- root 1)))))
     1.0)))

;;Exercise 1.41
;;Define a procedure double that takes a procedure of one argument
;;and returns a procedure that applies the original procedure twice.
(define jdouble
  (lambda (func)
    (lambda (x) (func (func x)))))

;;Exercise 1.42
;;Define a function compose that takes two fucntion arguments and
;;composes them so that x -> f(g(x))
(define jcompose
  (lambda (f g)
    (lambda (x) (f (g x)))))

(assert (= 49 ((jcompose square (lambda (x) (+ x 1))) 6)))

;;Exercise 1.43
;;If f is a numerical function and n is a positive integer, then
;;we can form the nth repeated application of f, which is defined to
;;be f(f(...(f(x))...)).  If f is x->x+1 then the nth repeated
;;application of the function is x+n.
(define repeated
  (lambda (f n)
    (cond ((= n 1) (lambda (x) (f x)))
	  (else (jcompose f (repeated f (- n 1)))))))

;;A basic test to make sure that the function is working
(assert (= 625 ((repeated square 2) 5)))

;;Exercise 1.44
;;The idea of a smoothing function is important in signal processing. If
;;f is a function and dx is some small number then the smoothed version 
;;of f is the function whose value at a point x is the average of 
;;f(x-dx), f(x) and f(x + dx).  Write a procedure smooth that takes as
;; input a procedure that computes f and returns the procedure that
;;computes the smoothed f.
(define smooth
  (lambda (func)
    (let ((dx 0.0000001))
      (lambda (x)
	(/ (+ (f (+ x dx)) (f (- x dx)) (f x)) 3)))))

;;Exercise 1.44 Part B
;;Create a function that will repeatedly smooth f, n times.  Use the
;;previous smooth function and then repeated function to define this.
(define n-fold-smooth
  (lambda (func n)
    (repeated (smooth func) n)))

;;Exercise 1.45
;;We saw in section 1.3.3 that attempting to compare square roots by
;;naively finding a fixed point of y -> x/y does not converge, and that
;;this can be fixed by average damping.  The same method works for
;;finding cube roots, but it does not work for 4th roots as a single
;;average damp does not cause the fixed-point function converge.
;;Figure out how many average damps are required to make other nth root
;;fixed-point searches converge and create a simple method for finding
;;nth roots.
(define arbitrary-root
  (lambda (x root accuracy)
    (letrec ((calculate-dampings
	      (lambda (start)
		(cond ((<= root 4) 2)
		      (else
		       (cond ((= (fast-iter-expt 2 start) root) start)
			     ((>= (fast-iter-expt 2 start) root) (- start 1))
			     (else (calculate-dampings (+ start 1)))))))))
      ((fixed-point-accuracy accuracy)
       ((repeated average-damp (calculate-dampings 2))
	(lambda (y) (/ x (fast-iter-expt y (- root 1)))))
       1.0))))

;;Create a generalized binary search function that we can use to figure out
;;what power of two to damp our arbitrary root finder by.
(define generate-binsearch
  (lambda (access-data compare-data )
    (lambda (data-vec low high value)
      (letrec ((binsearch
		(lambda (data-vec low high value)
		  (cond ((< high low) #f);;value not found
			(else
			 (let ((mid
				(exact (floor (+ low (/ (- high low) 2))))))
			   (cond ((= (compare-data
				      (access-data data-vec mid)
				      value) 1)
				  (binsearch data-vec low (- mid 1) value))
				 ((= (compare-data
				      (access-data data-vec mid)
				      value) -1)
				  (binsearch data-vec (+ mid 1) high value))
				 (else mid))))))));;value found
	(binsearch data-vec low high value)))))
