;;Work from Chapter 2 of the Structure and Interpretation of Computer Programs 
;;Building Abstractions with Data
;;The focus in chapter 1 was on building abstractions by combining procedures to
;;form "compound procedures".  The focus in chapter 2 is on building
;;abstractions by combining data objects to form "compound data".  

;;One of the important techniques for creating abstractions with compound data
;;is to isolate the parts of a program that deal with how data objects are
;;represented from the parts of the program that deal with how data objects are
;;used.

;;One of the key ideas in dealing with compound data is the notion of "closure"
;;-- that the glue used for combining data objects should work with both
;;primitive data objects and compound data objects as well.

;;A second key idea is that compound data objects can serve as conventional
;;interfaces for combining program modules in mix and match ways.  The power of
;;compound data objects can be enhanced by using "symbolic data" wherein data
;;can be composed of arbitrary symbols rather than just numbers.

;;Data abstraction is a methodology of separating how a compound data object is
;;used from the details of how it is constructed from more primitive data
;;objects.  The basic idea of data abstraction is to structure programs that use
;;compound data so that they operate on "abstract data".  Operations on this
;;data then defined in terms of this hypothetical data type.  Separately, a
;;"concrete" implementation of the data type is figured out and then
;;hypothetical operations are then implemented in terms of the this data type.
;;The interface between these two parts are called "selectors" and
;;"constructors".  These provide the interface in terms of the concrete
;;representation.

;;A Rational Number Arithmetic System
;;Our "constructor" is (make-rat <numerator> <denominator>)
;;Our "selectors" are (numer <rat-num>) and (denom <rat-num>)
(define add-rat
  (lambda (x y)
    (make-rat (+ (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y)))))

(define sub-rat
  (lambda (x y)
    (make-rat (- (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y)))))

(define mul-rat
  (lambda (x y)
    (make-rat (* (numer x) (numer y))
	      (* (denom x) (denom y)))))

(define div-rat
  (lambda (x y)
    (make-rat (* (numer x) (denom y))
	      (* (denom x) (numer y)))))

(define eq-rat?
  (lambda (x y)
    (= (* (numer x) (denom y))
       (* (denom x) (numer y)))))

;;Pairs
;;Scheme enables us to create a compound structure called a pair, which is
;;created with the primitive procedure "cons".  It takes 2 arguments and returns
;;a compound data object that contains the two arguments as parts.  Given a
;;pair, we can extract the parts using the primitive procedures "car" and "cdr".
;;Data objects constructed from pairs are called list-structured data and the
;;pair is useful in creating a wide variety of data structures.  Cons, car, and
;;cdr are the only "glue" that is needed.  We can now define make-rat, denom and
;;numer so that the functions defined above actually have a concrete basis.
(define older-make-rat
  (lambda (numerator denominator)
    (cons numerator denominator)))

(define numer
  (lambda (rat-num)
    (car rat-num)))

(define denom
  (lambda (rat-num)
    (cdr rat-num)))

(define print-rat
  (lambda (rat-num)
    (newline)
    (display (numer rat-num))
    (display "/")
    (display (denom rat-num))))

;;In order to maintain our rational number system in terms of the lowest common
;;denominators for the rational number we should use a gcd function when we are
;;constructing a new rational number.
(define jgcd
  (lambda (a b)
    (cond ((= b 0) a)
	  (else (jgcd b (remainder a b))))))

;;The new definitiona of make-rat incorporating gcd into the construction of the
;;rational number.
(define old-make-rat
  (lambda (numerator denominator)
    (let ((g (gcd numerator denominator)))
      (cons (/ numerator g) (/ denominator g)))))

;;Exercise 2.1
;;Define a better version of make-rat that handles both positive and negative
;;arguments.  Make
(define make-rat
  (lambda (numerator denominator)
    (let* ((numer (cond ((and (< numerator 0)
			      (< denominator 0))
			 (* numerator -1))
			((< denominator 0) (* numerator -1))
			(else numerator)))
	   (denom (cond ((and (< numerator 0)
			      (< denominator 0))
			 (* denominator -1))
			((< denominator 0) (* denominator -1))
			(else denominator)))
	   (g (jgcd numer denom)))
      (cons (/ numer g) (/ denom g)))))

;;Exercise 2.2 Consider the problem of representing line segments in a plane.
;;Each segment is represented as a pair of points: a starting point and an
;;ending point.  Define a constructor "make-segment" and selectors
;;"start-segment" and "end-segment" that define the representation of segments
;;in terms of points.  Each point can be represented as a pair of numbers, the x
;;and y coordinates.  Specify a constructor "make-point" and selectors "x-point"
;;and "y-point" to define the representation.  Finally, using the selectors and
;;constructors create a procedure midpoint-segment that takes a link segment as
;;an argument and returns its midpoint, which is the point whose coordinates are
;;the average of the coordinates of the endpoints.  Also define a procedure for
;;printing out points.
(define make-point
  (lambda (x y)
    (cons x y)))

(define x-point
  (lambda (point)
    (car point)))

(define y-point
  (lambda (point)
    (cdr point)))

(define println-point
  (lambda (point)
    (newline)
    (display "(")
    (display (x-point point))
    (display " , ")
    (display (y-point point))
    (display ")")))

(define print-point
  (lambda (point)
    (display "(")
    (display (x-point point))
    (display " , ")
    (display (y-point point))
    (display ")")))


;;Some tests of the point functions
(assert (= (x-point (make-point 0 5)) 0))
(assert (= (y-point (make-point -1 2)) 2))

(define make-segment
  (lambda (start-point end-point)
    (cons start-point end-point)))

(define start-segment
  (lambda (segment)
    (car segment)))

(define end-segment
  (lambda (segment)
    (cdr segment)))

(define midpoint-segment
  (lambda (segment)
    (make-point
     (/ (+ (x-point (start-segment segment))
	   (x-point (end-segment segment))) 2)
     (/ (+ (y-point (start-segment segment))
	   (y-point (end-segment segment))) 2))))

(define println-segment
  (lambda (segment)
    (newline)
    (print-point (start-segment segment))
    (display " -> ")
    (print-point (end-segment segment))))


(define print-segment
  (lambda (segment)
    (print-point (start-segment segment))
    (display " -> ")
    (print-point (end-segment segment))))


;;Some tests of the segment code
(assert (let ((segment (make-segment (make-point 0 0) (make-point 2 2))))
	  (and (= (x-point (midpoint-segment segment)) 1)
	       (= (y-point (midpoint-segment segment)) 1))))

;;Exercise 2.3
;;Implement a representation for rectangles in a plan.  In terms of the
;;constructors and selectors that you choose implement procedures that calculate
;;the perimeter and area of a given rectangle.  After this, implement a
;;different representation for rectangles.  Can the system work using either
;;representation.

;;These are helper functions that I wrote as part of chapter 1 that will assist
;;with this problem, specifically finding distance between points.
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

(define j-iterator
  (lambda (obj func times-to-apply base-case)
    (define j-iterator-helper
      (lambda (obj func times-to-apply total)
	(cond ((= times-to-apply 0) total)
	      (else (j-iterator-helper obj func (- times-to-apply 1)
				       (func obj total))))))
    (cond ((= times-to-apply 0) base-case)
	  (else (j-iterator-helper obj func (- times-to-apply 1)
				   (func obj base-case))))))

(define jsquare
  (lambda (x)
    (j-iterator x (lambda (x total) (* total x)) 2 1)))

(define jcube
  (lambda (x)
    (j-iterator x (lambda (x total) (* total x)) 3 1)))

;;Constructor procedure for rectangles
;;In this representation I just store store 2 segments of the rectangle.
;;Alternate representations could include storing 4 points for each of the
;;corners of the rectangle or storing two points for the top left and bottom
;;right points of the rectangle, as long as the rectangle is always assumed to
;;be axis-aligned.
(define make-rect
  (lambda (height-seg width-seg)
    (cons height-seg width-seg)))

;;Selector procedures for rectangles
(define rectangle-height
  (lambda (rectangle)
    (let* ((width-seg (car rectangle))
	   (start-point (start-segment width-seg))
	   (end-point (end-segment width-seg)))
      (jsqrt (+ (jsquare (- (x-point end-point) (x-point start-point)))
		(jsquare (- (y-point end-point) (y-point start-point))))))))

(define rectangle-width
  (lambda (rectangle)
    (let* ((height-seg (cdr rectangle))
	   (start-point (start-segment height-seg))
	   (end-point (end-segment height-seg)))
      (jsqrt (+ (jsquare (- (x-point end-point) (x-point start-point)))
		(jsquare (- (y-point end-point) (y-point start-point))))))))

					;Procedures for calculating the width and height of a rectangle
(define rectangle-area
  (lambda (rectangle)
    (* (rectangle-width rectangle)
       (rectangle-height rectangle))))

(define rectangle-perimeter
  (lambda (rectangle)
    (+ (* (rectangle-width rectangle) 2)
       (* (rectangle-height rectangle) 2))))

;;Some tests for rectangle code
(assert (= (rectangle-area
	    (make-rect (make-segment (make-point 0 0) (make-point 0 2))
		       (make-segment (make-point 0 0) (make-point 2 0))))
	   4))

;;Alternate, purely procedural representation of the cons, car, and cdr
;;procedures for creating pairs in data.
(define procedural-cons
  (lambda (x y)
    (lambda (z)
      (cond ((= z 0) x)
	    (else y)))))

(define procedural-car
  (lambda (pair)
    (pair 0)))

(define procedural-cdr
  (lambda (pair)
    (pair 1)))


;;Exercise 2.4 
;;This is an alternate procedural representation of pairs.  Verify that (car
;;(cons x y)) yields x for any objects x and y.
(define alt-procedural-cons
  (lambda (x y)
    (lambda (m)
      (m x y))))

(define alt-procedural-car
  (lambda (pair)
    (pair (lambda (p q) p))))

;;Now define the corresponding definition of cdr using the same method as
;;above. 
(define alt-procedural-cdr
  (lambda (pair)
    (pair (lambda (p q) q))))

;;Exercise 2.5
;;Show that we can represent pairs of non-negative integers using only numbers
;;and arithmetic operations if we represent the pair a and b as the integer that
;;is product (2^a)(3^b).  Give the corresponding definitions of cons, car, and
;;cdr.

;;Some helper procedures for calculating the numerical-cons, car, and cdr.
(define (fast-iter-expt a n)
  (fast-iter-expt-iter a n 1))

(define (fast-iter-expt-iter a n sum)
  (cond ((= n 0) sum)
        ((even? n) (fast-iter-expt-iter (* a a) (/ n 2) sum))
        (else (fast-iter-expt-iter a (- n 1) (* sum a)))))

;;The numerical cons definition, where we create a pair of x and y as the
;;product of (2^x)(3^y). 
(define numerical-cons
  (lambda (x y)
    (* (fast-iter-expt 2 x) (fast-iter-expt 3 y))))

;;Numerical-car given a pair, x and y, represented as (2^x)(3^y).
(define numerical-car
  (lambda (pair)
    (define numerical-car-helper
      (lambda (pair iterations)
	(cond ((not (= (mod pair 2) 0)) iterations)
	      (else (numerical-car-helper (/ pair 2) (+ iterations 1))))))
    (numerical-car-helper pair 0)))

;;Numerical-cdr given a pair, x and y, represented as (2^x)(3^y)
(define numerical-cdr
  (lambda (pair)
    (define numerical-cdr-helper
      (lambda (pair iterations)
	(cond ((not (= (mod pair 3) 0)) iterations)
	      (else (numerical-cdr-helper (/ pair 3) (+ iterations 1))))))
    (numerical-cdr-helper pair 0)))

;;Tests for the above car function
(assert (= (numerical-car (numerical-cons 0 0)) 0))
(assert (= (numerical-car (numerical-cons 1 0)) 1))
(assert (= (numerical-car (numerical-cons 1 1)) 1))
(assert (= (numerical-car (numerical-cons 2 1)) 2))
(assert (= (numerical-car (numerical-cons 3 2)) 3))
(assert (= (numerical-car (numerical-cons 4 3)) 4))
(assert (= (numerical-car (numerical-cons 4 5)) 4))
(assert (= (numerical-car (numerical-cons 4 6)) 4))
(assert (= (numerical-car (numerical-cons 6 2)) 6))

;;Tests for the above cdr function
(assert (= (numerical-cdr (numerical-cons 0 0)) 0))
(assert (= (numerical-cdr (numerical-cons 1 0)) 0))
(assert (= (numerical-cdr (numerical-cons 1 1)) 1))
(assert (= (numerical-cdr (numerical-cons 2 1)) 1))
(assert (= (numerical-cdr (numerical-cons 3 2)) 2))
(assert (= (numerical-cdr (numerical-cons 4 3)) 3))
(assert (= (numerical-cdr (numerical-cons 4 5)) 5))
(assert (= (numerical-cdr (numerical-cons 4 6)) 6))
(assert (= (numerical-cdr (numerical-cons 6 2)) 2))


;;Exercise 2.6
;;In a language that can manipulate procedures, you can get by without numbers
;;by implementing 0 and the operation of adding 1 as
(define zero
  (lambda (f)
    (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;;Defining one and two not in terms of add-1 or zero is as follows
(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define three
  (lambda (f) (lambda (x) (f (f (f x))))))

(assert (= 1 (((add-1 zero) (lambda (x) (+ x 1))) 0)))
(assert (= 2 (((add-1 (add-1 zero)) (lambda (x) (+ x 1))) 0)))
(assert (= 3 (((add-1 (add-1 (add-1 zero))) (lambda (x) (+ x 1))) 0)))

;;Section 2.1.4 Interval Arithmetic
;;A procedure for adding intervals giving a constructor, "make-interval" and two
;;selectors, "lower-bound" and "upper-bound".
(define add-interval
  (lambda (x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
		   (+ (upper-bound x) (lower-bound y)))))

(define mul-interval
  (lambda (x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
	  (p2 (* (lower-bound x) (upper-bound y)))
	  (p3 (* (upper-bound x) (lower-bound y)))
	  (p4 (* (upper-bound x) (upper-bound y))))
      (make-interval (min p1 p2 p3 p4)
		     (max p1 p2 p3 p4)))))

(define div-interval
  (lambda (x y)
    (mul-interval x
		  (make-interval (/ 1.0 (upper-bound y))
				 (/ 1.0 (lower-bound y))))))

;;Exercise 2.7
;;Given the definition of make-interval below, define upper-bound and
;;lower-bound
(define make-interval
  (lambda (lower upper)
    (cons lower upper)))

(define upper-bound
  (lambda (interval)
    (cdr interval)))

(define lower-bound
  (lambda (interval)
    (car interval)))

;;Exercise 2.8
;;Create a corresponding definition for sub-interval
(define subtract-interval
  (lambda (x y)
    (make-interval (- (lower-bound y) (upper-bound x))
		   (- (upper-bound y) (lower-bound x)))))

;;Exercise 2.9
;;The width of an interval is half of the difference between its upper and lower
;;bounds.  The width is a measure of the uncertainty of the number specified by
;;the interval.  For some arithmetic operations the width of the result of
;;combining two intervals is a function only of the widths of the argument
;;intervals, whereas for others the width of the combination is not a function
;;of the widths of the argument intervals.  Show that the width of the sum (or
;;difference) of two intervals is a function only of the widths of the intervals
;;being added (or subtracted), then give examples showing this is not true for
;;multiplication or division.
(define interval-width
  (lambda (interval)
    (/ (- (upper-bound interval) (lower-bound interval)) 2)))



;;Exercise 2.10
;;Modify interval divide to handle division by an interval of 0 span.
(define improved-div-interval
  (lambda (x y)
    (cond ((= (interval-width y) 0) (assert #f))
	  (else (div-interval x y)))))

;;Exercise 2.11
;;Test the signs of the endpoints of the intervals to break mul-interval into 9
;;cases, only one of which requires more than 2 multiplications
(define improved-mul-interval
  (lambda (x y)
    (+ 1 1)))

;;Tests for improved interval multiplication, one test for each of the 9 cases
;;Both intervals contains postive upper and lower bounds
(assert #f)
;;Interval 2 has a negative lower bound
(assert #f)
;;Interval 2 has negative upper and lower bounds
(assert #f)
;;Interval 1 has a negative lower bound
(assert #f)
;;Intervals 1 and 2 have negative lower bounds
(assert #f)
;;Interval 1 has a negative lower bound and Interval 2 has negative upper and
;;lower bounds.
(assert #f)
;;Interval 1 has negative upper and lower bounds
(assert #f)
;;Interval 1 has negative upper and lower bounds and Interval 2 has a negative
;;lower bound
(assert #f)
;;Intervals 1 and 2 have negative upper and lower bounds.
(assert #f)

;;What are cases for the signs of the intervals x and y
;;upper-x n | lower-x n | upper-y n | lower-y n | upper-r n | lower-r n
;;    0           0           0           0           0           0 
;;    0           0           0           1
;;    0           0           1           1
;;    0           1           0           0
;;    0           1           0           1
;;    0           1           1           1
;;    1           1           0           0
;;    1           1           0           1
;;    1           1           1           1           0           0



;;Section 2.2 Heirarchical Data nd the Closure Property
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define cc
  (lambda (amount coin-values)
    (cond ((= amount 0) 1)
	  ((or (< amount 0) (no-more? coin-values)) 0)
	  (else
	   (+ (cc amount (except-first-denomination coin-values))
	      (cc (- amount (first-denomination coin-values))
		  coin-values))))))

(define first-denomination
  (lambda (coin-values)
    (car coin-values)))

(define except-first-denomination
  (lambda (coin-values)
    (cdr coin-values)))

(define no-more?
  (lambda (coin-values)
    (null? coin-values)))

(define args-test
  (lambda y
    (display y)))

;;Exercise 2.20
(define same-parity
  (lambda (x . rest)
    (letrec ((helper (lambda (l pred)
		       (cond ((null? l) (quote ()))
			     ((pred (car l)) (cons (car l)
						   (helper
						    (cdr l)
						    pred)))
			     (else (helper (cdr l) pred))))))
      (cond ((even? x) (cons x (helper rest even?)))
	    (else (cons x(helper rest odd?)))))))

(define scale-list
  (lambda (items factor)
    (cond ((null? items) '())
	  (else (cons (* (car items) factor)
		      (scale-list (cdr items) factor))))))

(define alt-scale-list
  (lambda (items factor)
    (map (lambda (x) (* x factor)) items)))

;;Exercise 2.21
(define square-list
  (lambda (items)
    (map (lambda (x) (* x x)) items)))

;;exercise 2.23
(define for-each
  (lambda (proc items)
    (cond ((null? items) #t)
	  (else (let ((temp (proc (car items))))
		  (for-each proc (cdr items)))))))

(define count-leaves
  (lambda (tree)
    (cond ((null? tree) 0)
	  ((not (pair? tree)) 1)
	  (else (+ (count-leaves (car tree))
		   (count-leaves (cdr tree)))))))


;;Exercise 2.27
(define jappend
  (lambda (list1 list2)
    (cond ((null? list1) list2)
	  (else (cons (car list1) (jappend (cdr list1) list2))))))

(define jreverse
  (lambda (l)
    (cond ((null? (cdr l)) l)
		  (else (jappend (jreverse (cdr l)) (list (car l)))))))

(define iterative-reverse
  (lambda (l)
    (letrec ((helper (lambda (l result)
					   (cond ((null? l) result)
							 (else (helper (cdr l)
										   (cons (car l) result)))))))
      (helper l '()))))

(define deep-reverse
  (lambda (l)
    (letrec ((helper (lambda (l result)
					   (cond ((null? l) result)
							 ((pair? (car l))
							  (helper (cdr l) (cons (helper (car l) '())
													result)))
							 (else (helper (cdr l) (cons (car l) result)))))))
      (helper l '()))))

;;Exercise 2.28
(define fringe
  (lambda (tree)
    (cond ((null? tree) '())
		  ((not (pair? tree)) (list tree))
		  (else (jappend (fringe (car tree)) (fringe (cdr tree)))))))

;;Exercise 2.29
(define make-mobile
  (lambda (left-branch right-branch)
	(list left-branch right-branch)))

(define make-branch
  (lambda (length structure)
	(list length structure)))

(define alt-make-mobile
  (lambda (left right)
    (cons left right)))

(define alt-make-branch
  (lambda (length structure)
    (cons length structure)))

(define left-branch
  (lambda (mobile)
	(car mobile)))

(define right-branch
  (lambda (mobile)
	(car (cdr mobile))))

(define branch-length
  (lambda (branch)
	(car branch)))

(define branch-structure
  (lambda (branch)
    (car (cdr branch))))

(define total-weight
  (lambda (mobile)
    (letrec ((branch-weight (lambda (branch)
			      (let ((s (branch-structure branch)))
				(cond ((pair? s)
				       (+ (branch-weight (left-branch s))
					  (branch-weight (right-branch s))))
				      (else s))))))
      (cond ((not (pair? mobile)) mobile)
	    (else (+ (branch-weight (left-branch mobile))
		     (branch-weight (right-branch mobile))))))))

(define test-total-weight
  (lambda ()
    (let ((mobile1 (make-mobile (make-branch 0 1) (make-branch 0 2)))
	  (mobile2 (make-mobile (make-branch 0 1)
				(make-branch 0 (make-mobile
						(make-branch 0 2)
						(make-branch 0 3)))))
	  (mobile3 (make-mobile
		    (make-branch 0
				 (make-mobile
				  (make-branch 0 1)
				  (make-branch 0 (make-mobile
						  (make-branch 0 2)
						  (make-branch 0 3)))))
		    (make-branch 0 (make-mobile
				    (make-branch 0 4)
				    (make-branch 0
						 (make-mobile
						  (make-branch 0 5)
						  (make-branch 0 6))
						 ))))))
      (assert (= (total-weight (branch-structure (left-branch mobile1))) 1))
      (assert (= (total-weight mobile1) 3))
      (assert (= (total-weight (branch-structure (right-branch mobile1))) 2))
      (assert (= (total-weight (branch-structure (left-branch mobile2))) 1))
      (assert (= (total-weight (branch-structure (right-branch mobile2))) 5)))))

(define branch-torque
  (lambda (branch)
    (* (branch-length branch) (total-weight (branch-structure branch)))))

(define test-branch-torque
  (lambda ()
    (let ((mobile1 (make-mobile (make-branch 3 1) (make-branch 4 2))))
      (assert (= (branch-torque (left-branch mobile1)) 3))
      (assert (= (branch-torque (right-branch mobile1)) 8))
      )))

(define balanced-mobile?
  (lambda (mobile)
    (cond ((= (branch-torque (left-branch mobile))
	      (branch-torque (right-branch mobile)))
	   (and (balanced-branch? (left-branch mobile))
		(balanced-branch? (right-branch mobile))))
	  (else #f))))

(define balanced-branch?
  (lambda (branch)
    (cond ((not (pair? (branch-structure branch))) #t)
	  (else
	   (and (= (branch-torque (left-branch (branch-structure branch)))
		   (branch-torque (right-branch (branch-structure branch))))
		(balanced-branch? (left-branch (branch-structure branch)))
		(balanced-branch? (right-branch (branch-structure branch))))))))

(define test-balanced-mobile
  (lambda ()
    (let ((mobile1 (make-mobile (make-branch 4 2) (make-branch 2 4)))
	  (mobile2 (make-mobile (make-branch 6 2)
				(make-branch 2 (make-mobile
						(make-branch 2 4)
						(make-branch 4 2)))))
	  (mobile3 (make-mobile (make-branch 5 2)
				(make-branch 2 (make-mobile
						(make-branch 2 4)
						(make-branch 4 2))))))
      (assert (eq? (balanced-mobile? mobile1) #t))
      (assert (eq? (balanced-mobile? mobile2) #t))
      (assert (eq? (balanced-mobile? mobile3) #f)))))


;;Mapping over trees
;;Combining map with recursion is a powerful abstraction for dealing with
;;trees.

(define scale-tree
  (lambda (tree factor)
    (cond ((null? tree) '())
	  ((not (pair? tree)) (* tree factor))
	  (else (cons (scale-tree (car tree) factor)
		      (scale-tree (cdr tree) factor))))))

(define alt-scale-tree
  (lambda (tree factor)
    (map (lambda (sub-tree)
	   (cond ((pair? sub-tree)
		  (scale-tree sub-tree factor))
		 (else (* sub-tree factor)))) tree)))

;;Exercise 2.30
(define direct-square-tree
  (lambda (tree)
    (cond ((null? tree) '())
	  ((not (pair? tree)) (* tree tree))
	  (else (cons (direct-square-tree (car tree))
		      (direct-square-tree (cdr tree)))))))

(define higher-order-square-tree
  (lambda (tree)
    (map (lambda (sub-tree)
	   (cond ((pair? sub-tree)
		  (higher-order-square-tree sub-tree))
		 (else (* sub-tree sub-tree)))) tree)))

;;Exercise 2.31
(define tree-map
  (lambda (func tree)
	(map (lambda (sub-tree)
		   (cond ((pair? sub-tree)
				  (tree-map func sub-tree))
				 (else (func sub-tree)))) tree)))

;;Exercise 2.32 Create a function that creates a set of subsets of a set
(define subsets
  (lambda (set)
	(cond ((null? set) (list '()))
		  (else (let ((rest (subsets (cdr set))))
				  (append rest (map (lambda (x) (cons (car set) x)) rest)))))))

;;No accumulate function is standard so I will provide the one from the book
(define accumulate
  (lambda (op initial sequence)
	(cond ((null? sequence) initial)
		  (else (op (car sequence) 
					(accumulate op initial (cdr sequence)))))))

;;Also provide utility functions to generate intervals
(define enumerate-interval 
  (lambda (x y . op)
	(letrec ((enumerate-helper
			  (lambda (start finish comparison next)
				(cond ((comparison start finish) '())
					  (else (cons start (enumerate-helper (next start)
														  finish
														  comparison
														  next)))))))
	  (cond ((< x y) (cond ((null? op) 
							(enumerate-helper x y > (lambda (x) (+ x 1))))
						   (else (enumerate-helper x y > (car op)))))
			((> x y) (cond ((null? op)
							(enumerate-helper x y < (lambda (x) (- x 1))))
						   (else (enumerate-helper x y < (car op)))))
			((= x y) (list x))
			(else '())))))
	
(define opt-args-test
  (lambda (x . y)
	(cond ((null? y) (begin 
					   (display "y is null")
					   (newline)
					   (display y)
					   (newline)))
		  (else (begin
				  (display "y is not null")
				  (newline)
				  (display y)
				  (newline))))))

;;Exercise 2.33
(define accum-map 
  (lambda (procedure sequence)
	(accumulate (lambda (x y) (cons (procedure x) y)) '() sequence)))

(define accum-append
  (lambda (seq1 seq2)
	(accumulate cons seq2 seq1)))

(define accum-length
  (lambda (sequence)
	(accumulate (lambda (x y) (+ 1 y)) 0 sequence)))
;;Exercise 2.34 implement horner's rule for polynomial evaluation using 
;;accumulate
(define horner-eval
  (lambda (x coefficient-sequence)
	(accumulate (lambda (this-coeff higher-terms) 
				  (+ this-coeff (* x higher-terms)))
				0
				coefficient-sequence)))

;;Exercise 2.35 Redefine count-leaves method to use an accumulation
(define accum-count-leaves
  (lambda (tree)
	(accumulate x '() (map x tree))))

;;Exercise 2.36
(define accumulate-n
  (lambda (op init seqs)
	(cond ((null? (car seqs)) '())
		  (else (cons (accumulate op init
								  (accumulate (lambda (x y) (cons (car x) y))
											  '()
											  seqs))
					  (accumulate-n op init
									(accumulate (lambda (x y) (cons (cdr x) y))
												'()
												seqs)))))))

;;Exercise 2.37
(define dot-product
  (lambda (v w)
	(accumulate + 0 (map * v w))))

(define matrix-*-vector
  (lambda (matrix v)
	(map (lambda (x) (dot-product x v)) matrix)))

(define transpose
  (lambda (matrix)
	(accumulate-n cons '() matrix)))

(define matrix-*-matrix
  (lambda (matrix1 matrix2)
	(let ((cols (transpose matrix2)))
	  (map (lambda (x)
			 (map (lambda (y)
					(accumulate + 0 (map * x y)))
				  cols))
		   matrix1))))

;;Exercise 2.38
(define j-fold-left
  (lambda (op initial sequence)
	(letrec ((iter (lambda (result rest)
					 (cond ((null? rest) result)
						   (else (iter (op result (car rest))
									   (cdr rest)))))))
	  (iter initial sequence))))
;;To have fold-right and fold-left return the same values for any sequence,
;;(op x y) == (op y x)

;;Exercise 2.39 Define reverse in terms of fold-left and fold-right
(define fr-reverse
  (lambda (sequence)
	(fold-right (lambda (x y) (append y (list x))) '() sequence)))

(define fl-reverse
  (lambda (sequence)
	(fold-left (lambda (x y) (cons y x)) '() sequence)))

(define generate-pairs-up-to-n
  (lambda (n)
	(map (lambda (x) (map (lambda (y) (cons x y))
						  (enumerate-interval 1 x)))
		 (enumerate-interval 1 n))))

;;Nested Mappings
;;Here we are trying to create nested mappings of pair of numbers, and 
;;then we use append to flatten the nested list created by map
(define prime-sum-pairs) 

(filter (lambda (pair) (slow-prime? (+ (car pair) (car (cdr pair)))))
		(accumulate append '()
					(map (lambda (x)
						   (map (lambda (y)
								  (list x y))
								(enumerate-interval 1 (- x 1))))
						 (enumerate-interval 2 6))))
		
(define flatmap
  (lambda (proc seq)
	(accumulate append '() (map proc seq))))

(define permutations
  (lambda (set)
	(cond ((null? set) (list '()))
		  (else (flatmap (lambda (x) (map (lambda (p) (cons x p))
										  (permutations (remove x set))))
						 set)))))
												  
;;Exercise 2.40
(define unique-pairs
  (lambda (n)
	(flatmap (lambda (x)
			   (map (lambda (y) (list x y)) (enumerate-interval 1 (- x 1)))) (enumerate-interval 2 n))))

;;Exercise 2.41
(define ordered-triples
  (lambda (n s)
	(filter
	 (lambda (item)
	   (equal? s (+ (car item) (car (cdr item)) (car (cdr (cdr item))))))
	 (accumulate append '()
				 (flatmap (lambda (x)
							(map (lambda (y)
								   (map (lambda (z) (list x y z)) (enumerate-interval 1 (- y 1))))
								 (enumerate-interval 2 (- x 1))))
						  (enumerate-interval 3 n))))))
;;Exercise 2.42
  
