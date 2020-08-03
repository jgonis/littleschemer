;;The scheme programming language 4 Notes
#| Section 2.6 Top Level Definitions.
|#

(define flip
  (lambda ()
    (let ([count 0])
      (if (= 0 count)
	  (begin (set! count 1) count)
	  (begin (set! count 0) count)))))

(define doubler
  (lambda (f)
    (lambda (x) (f x x))))

(define double-any
  (lambda (f x)
    (f x x)))

;;Exercise 2.6.1
;;This function will go into an infinite recursion as it infinitely attemps to
;;apply double-any to the arguments double-any and double-any in the form
;;(double-any double-any double-any) equivalent to (f x x)

;;Exercise 2.6.2
(define compose
  (lambda (p1 p2)
    (lambda (x)
      (p1 (p2 x)))))

;;Exercise 2.6.3
;;caar
(define jcaar (compose car car))
;;cadr
(define jcadr (compose car cdr))
;;cdar
(define jcdar (compose cdr car))
;;cddr
(define jcddr (compose cdr cdr))
;;caaar
(define jcaaar (compose car (compose car car)))
;;caadr
(define jcaadr (compose car (compose car cdr)))
;;cadar
(define jcadar (compose car (compose cdr car)))
;;caddr
(define jcaddr (compose car (compose cdr cdr)))
;;cdaar
(define jcdaar (compose cdr (compose car car)))
;;cdadr
(define jcdadr (compose cdr (compose car cdr)))
;;cddar
(define jcddar (compose cdr (compose cdr car)))
;;cdddr
(define jcdddr (compose cdr (compose cdr cdr)))
;;caaaar
(define jcaaaar (compose (compose car car) (compose car car)))
;;caaadr
(define jcaaadr (compose (compose car car) (compose car cdr)))
;;caadar
(define jcaadar (compose (compose car car) (compose cdr car)))
;;caaddr
(define jcaaddr (compose (compose car car) (compose cdr cdr)))
;;cadaar
(define jcadaar (compose (compose car cdr) (compose car car)))
;;cadadr
(define jcadadr (compose (compose car cdr) (compose car cdr)))
;;caddar
(define jcaddar (compose (compose car cdr) (compose cdr car)))
;;cdaaar
(define jcdaaar (compose (compose cdr car) (compose car car)))
;;cdaadr
(define jcdaadr (compose (compose cdr car) (compose car cdr)))
;;cdadar
(define jcdadar (compose (compose cdr car) (compose cdr car)))
;;cdaddr
(define jcdaddr (compose (compose cdr car) (compose cdr cdr)))
;;cddaar
(define jcddaar (compose (compose cdr cdr) (compose car car)))
;;cddadr
(define jcddadr (compose (compose cdr cdr) (compose car cdr)))
;;cdddar
(define jcdddar (compose (compose cdr cdr) (compose cdr car)))
;;cddddr
(define jcddddr (compose (compose cdr cdr) (compose cdr cdr)))

;;Section 2.7
;;Exercise 2.7.1
;;Define a predicate atom? which returns true if its arguments is not a pair,
;;and false if it is.
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
;;Exercise 2.7.2
;;Define a procedure shorter which returns the shorter of two list arguments
;;or the first argument if they are of the same length
(define shorter
  (lambda (l1 l2)
    (cond ((not (pair? l1))
	   (assertion-violation 'shorter "improper argument" l1))
	  ((not (pair? l2))
	   (assertion-violation 'shorter "improper argument" l2))
	  (else
	   (cond ((< (length l2) (length l1)) l2)
		 (else l1))))))

;;Section 2.8 Simple Recursion
(define list-copy
  (lambda (l)
    (cond ((null? l) '())
	  (else (cons (car l) (list-copy (cdr l)))))))

(define dumb-list-copy
  (lambda (n)
    n))


(define jremv
  (lambda (l obj)
    (cond ((null? l) '())
	  ((eq? (car l) obj) (remv (cdr l) obj))
	  (else (cons (car l) (remv (cdr l) obj))))))

(define tree-copy
  (lambda (tree)
    (if (not (pair? tree))
	tree
	(cons (tree-copy (car tree)) (tree-copy (cdr tree))))))

(define tree-copy-reverse
  (lambda (tree)
    (if (not (pair? tree))
	tree
	(cons (tree-copy-reverse (cdr tree)) (tree-copy-reverse (car tree))))))



(define abs-all
  (lambda (ls)
    (cond ((null? ls) (quote ()))
	  (else (cons (abs (car ls)) (abs-all (cdr ls)))))))

(define map1
  (lambda (func ls)
    (cond ((null? ls) (quote ()))
	  (else (cons (func (car ls)) (map1 func (cdr ls)))))))

#| Exercise 2.8.1
Describe what would happen if you switched the order of the arguments to cons in
the definition of tree copy.
If you switched the statements (tree-copy (car ls)) with (tree-copy (cdr ls))
you would end up recursively swapping the branches of the tree that you passed
in, that branches that were on the "left" would be on the "right" and this would
be applied to all of their children as well.
|#

;;Exercise 2.8.2
;;Create a two argument version of append.
(define jappend
  (lambda (ls ls-to-add)
    (cond ((null? ls)
	   (cond ((pair? ls-to-add) ls-to-add) ;;Check to see if element being
		 (else (list ls-to-add))))     ;;appended is a list or not
	  (else (cons (car ls) (jappend (cdr ls) ls-to-add))))))

;;What would happend if you switched the arguments in the call to append within
;;your definition of append?
(define jappend-reverse
  (lambda (ls ls-to-add)
    (cond ((null? ls)
	   (cond ((pair? ls-to-add) ls-to-add)
		 (else (list ls-to-add))))
	  (else (cons (jappend-reverse (cdr ls) ls-to-add) (car ls))))))

;;Exercise 2.8.3
;;Define a procedure make-list which takes a non-negative integer and an object
;;and returns a new list, n long, each element of which is the object
;;ex: (make-list 7 '()) = (() () () () () () ())
(define make-list
  (lambda (length obj)
    (cond ((= length 0) (quote ()))
	  (else (cons obj (make-list (- length 1) obj))))))

;;Exercise 2.8.4
;;The procedures list-ref