;;;The simplest scheme expressions are constant data objects such as
;;;strings, numbers, symbols, and lists.

;;;Any procedure application in scheme, regardless of the number of
;;;arguments is written in prefix notation, ie (procedure arg ...)


;;;In scheme the basic aggregate data structure is the list, which is
;;;written as a sequence of objects surrounded by parentheses. Lists
;;;can contain heterogeneous objects, ie strings and numbers and
;;;symbols may all be stored within a single list. Lists can also
;;;store other lists.

;;;Generally scheme has several rules for determining if a list
;;;contains data, or if it is a procedure application, but we can
;;;explicitly tell the system to treat a list as data rather than as a
;;;procedure application via the use of the quote function. (+ 1 2) is
;;;a procedure application of the + function to 1 and 2, but (quote (+
;;;1 2)) will return the list (+ 1 2), ie (+ 1 2) is now seen as data
;;;and not as a procedure application. quote is one of the special
;;;forms of scheme, ie it is treated differently than straight
;;;procedure application.


;;;In a similar fashion symbols must be quoted to prevent scheme from
;;;trying to interpret them as variables and finding their binding.
;;;So (quote hello) returns the symbol hello, whereas plain hello will
;;;probably trigger an error as scheme attempts to determine the
;;;binding of the variable hello and is unable to.

;;;The cons procedure is used for building pairs, which are the
;;;building blocks of lists. A proper list is merely a set of pairs in
;;;which the cdr of the last pair is the empty list.  Otherwise the
;;;sequence is an improper list. An improper list is written out using
;;;"dot notation", ie (a . b) is an improper list, which in this case
;;;is a pair with a as the fist element and b as the last.

;;;Exercise 2.2.1 a
(+ (* 1.2 (- 2 1/3)) -8.7) 
;;;Exercise 2.2.1 b
(/ (+ 2/3 4/9) (- 5/11 4/3))
;;;Exercise 2.2.1 c
(+ 1 (/ 1 (+ 2 (/ 1 (+ 1 1/2)))))
;;;Exercise 2.2.1 d
(* 1(* -2 (* 3 (* -4 (* 5(* -6 7))))))

;;;Exercise 2.2.3 a Predicted answer (car . cdr)
(cons 'car 'cdr)
;;;Exercise 2.2.3 b Predicted answer (this is silly)
(cons 'this '(is silly))
;;;Exercise 2.2.3 c Predicted answer (is this silly?)
(cons 'is '(this silly?))
;;;Exercise 2.2.3 d Predicted answer (+ 2 3)
(quote (+ 2 3))
;;;Exercise 2.2.3 e Predicted answer (+ 2 3)
(cons '+ '(2 3))
;;;Exercise 2.2.3 f Precited answer +
(car '(+ 2 3))
;;;Exercise 2.2.3 g Predicted answer (2 3)
(cdr '(+ 2 3))
;;;Exercise 2.2.3 h Predicted answer: procedure named cons
cons
;;;Exercise 2.2.3 i Predicted answer: the symbol cons
(quote cons)
;;;Exercise 2.2.3 j Predicted answer: (quote cons)
(quote (quote cons))
;;;Exercise 2.2.3 k Predicted answer: symbol quote
(car (quote (quote cons)))
;;;Exercise 2.2.3 l Predicted answer: 5
(+ 2 3)
;;;Exercise 2.2.3 m Predicted answer: 5
(+ '2 '3)
;;;Exercise 2.2.3 n Predicted answer: 5
(+ (car '(2 3)) (car (cdr '(2 3))))
;;;Exercise 2.2.3 o Predicted answer: 5
((car (list + - * /)) 2 3)

;;;Exercise 2.2.4
(car (car '((a b) (c d)))) ;This yield a
;;;To yield b
(car (cdr (car '((a b) (c d)))))
;;;To yield c
(car (car (cdr '((a b) (c d)))))
;;;To yield d
(car (cdr (car (cdr '((a b) (c d))))))

;;;Exercise 2.2.5
(cons (cons 'a 'b) (cons (cons (cons 'c '()) (cons 'd '())) (cons '() '())))

;;;Exercise 2.4.1 a) (+ (- (* 3 a) b) (+ (* 3 a) b))
;;(let ((a (* 3 a)))
  ;;(+ (- a b) (+ a b)))

;;;Exercise 2.4.1 b) (cons (car (list a b c)) (cdr (list a b c)))
(let ((list (list 'a 'b 'c)))
  (cons (car list ) (cdr list)))

;;;Exercise 2.4.2 Predicted answer: 54
(let ((x 9))
  (* x (let ((x (/ x 3)))
         (+ x x))))

;;;Exercise 2.4.3 a) Predicted answer: ((c . b) (a . d))
(let ((x 'a) (y 'b))
  (list (let ((c 'c))
          (cons c y))
        (let ((d 'd))
          (cons x d))))

;;;Exercise 2.4.3 b) Predicted answer: (c b a b)
(let ((x '((a b) c)))
  (cons (let ((c (cdr x)))
          (car c))
        (let ((ab (car x)))
          (cons (let ((b (cdr ab)))
                  (car b))
                (cons (let ((a (car ab)))
                        a)
                      (cdr ab))))))

;;;Section 2.5 Lambda Expressions
;;;When creating a lambda expression we can have variables that are not
;;;explicitly bound in the expression.  These are called free
;;;variables. These free variables are bound to the environment
;;;defined lexically, ie in the program text.  If this environment is
;;;no-longer accessible when the procedure is executed, the bindings
;;;will still be in effect
;;;Example (let ((f (let ((x 'sam)) (lambda (y z) (list x y z))))) (f 'i
;;;'am))
;;;In this case the lambda is defined in an environment where x is bound to
;;;the symbol 'sam, but when f is executed that environment no longer
;;;exists and x has no binding. However f will still result in (sam i
;;;am) because f contains the environment it was defined in and so its
;;;x is still bound to 'sam when it is executed.  In fact even if x
;;;does exist when it is executed, and x has a different binding in
;;;the environment it is being executed in, it will still have the
;;;value that x was bound to when it was defined.

;;;Exercise 2.5.1 a) Predicted answer: a
(let ((f (lambda (x) x))) (f 'a))
;;;Exercise 2.5.1 b) Predicted answer: (a)
(let ((f (lambda x x))) (f 'a))
;;;Exercise 2.5.1 c) Predicted answer: a
(let ((f (lambda (x . y) x))) (f 'a))
;;;Exercise 2.5.1 d) Predicted answer: ()
(let ((f (lambda (x . y) y))) (f 'a))

;;;Exercise 2.5.2
((lambda x x) '1 '2 '3 '4) 

;;;Exercise 2.5.3 a) No free variables

;;;Exercise 2.5.3 b) +

;;;Exercise 2.5.3 c) f

;;;Exercise 2.5.3 d) cons f y

;;;Exercise 2.5.3 e) cons y

;;;Exercise 2.5.3 f) cons z y


;;;Top level definitions
;;;Bindings established with let or lambda expressions are not visible
;;;outside of the bodies of these expressions.  To create a binding
;;;that is visible anywhere you use "define" which creates a top-level
;;;definition.  These definitions are visible in every expression you
;;;enter, except when they are shadowed. Top-level definitions may be
;;;established for any object, not just procedures.

(define double-any (lambda (f x) (f x x)))
;;;Exercise 2.6.1 This will result in an infinite recursion, as it
;;;will try to apply double-any with two arguments, which result in
;;;calling the function double-any with those two arguments, and again
;;;recurse, over and over.

;;;Exercise 2.6.2
(define test-compose
  (lambda (f1 f2)
    (lambda (arg)
      (f1 (f2 arg)))))

(define composed-cadr
  (lambda (list-arg)
    ((compose car cdr) list-arg)))

(define composed-cddr
  (lambda (list-arg)
    ((compose cdr cdr) list-arg)))

;;;Exercise 2.6.3
;;;caar
(define composed-caar
  (lambda (list-arg)
    ((compose car car) list-arg)))
;;;cdar
(define composed-cdar
  (lambda (list-argn)
    ((compose cdr car) list-arg)))
;;;caaar
(define composed-caaar
  (lambda (list-arg)
    ((compose car composed-caar) list-arg)))
;;;caadr
(define composed-caadr 
  (lambda (list-arg)
    ((compose car composed-cadr) list-arg)))
;;;cadar
(define composed-cadar
  (lambda (list-arg)
     ((compose car composed-cdar) list-arg)))
;;;caddr
(define composed-caddr
  (lambda (list-arg)
    ((compose car composed-cddr) list-arg)))
;;;cdaar
(define composed-cdaar
  (lambda (list-arg)
    ((compose cdr composed-caar) list-arg)))
;;;cdadr
(define composed-cdadr
  (lambda (list-arg)
    ((compose cdr composed-cadr) list-arg)))
;;;cddar
(define composed-cddar
  (lambda (list-arg)
    ((compose cdr composed-cdar) list-arg)))
;;;cdddr
(define composed-cdddr
  (lambda (list-arg)
    ((compose cdr composed-cddr) list-arg)))
;;;caaaar
(define composed-caaaar
  (lambda (list-arg)
    ((compose car composed-caaar) list-arg)))
;;;caaadr
(define composed-caaadr
  (lambda (list-arg)
    ((compose car composed-caadr) list-arg)))
;;;caadar
(define composed-caadar
  (lambda (list-arg)
    ((compose car composed-cadar) list-arg)))
;;;caaddr
(define composed-caaddr
  (lambda (list-arg)
    ((compose car composed-caddr) list-arg)))
;;;cadaar
(define composed-cadaar
  (lambda (list-arg)
    ((compose car composed-cdaar) list-arg)))
;;;cadadr
(define composed-cadadr
  (lambda (list-arg)
    ((compose car composed-cdadr) list-arg)))
;;;caddar
(define composed-caddar
  (lambda (list-arg)
    ((compose car composed-cddar) list-arg)))
;;;cadddr
(define composed-cadddr
  (lambda (list-arg)
    ((compose car composed-cdddr) list-arg)))
;;;cdaaar
(define composed-cdaaar
  (lambda (list-arg)
    ((compose cdr composed-caaar) list-arg)))
;;;cdaadr
(define composed-cdaadr
  (lambda (list-arg)
    ((compose cdr composed-caadr) list-arg)))
;;;cdadar
(define composed-cdadar
  (lambda (list-arg)
    ((compose cdr composed-cadar) list-arg)))
;;;cdaddr
(define composed-cdaddr
  (lambda (list-arg)
    ((compose cdr composed-caddr) list-arg)))
;;;cddaar
(define composed-cddaar
  (lambda (list-arg)
    ((compose cdr composed-cdaar) list-arg)))
;;;cddadr
(define composed-cddadr
  (lambda (list-arg)
    ((compose cdr composed-cdadr) list-arg)))
;;;cdddar
(define composed-cdddar
  (lambda (list-arg)
    ((compose cdr composed-cddar) list-arg)))
;;;cddddr
(define composed-cddddr
  (lambda (list-arg)
    ((compose cdr composed-cdddr) list-arg)))

;;;Conditional Expressions

;;;Exercise 2.7.1
;;;Define the predicate atom?, which returns true if its argument is
;;;not a pair and false if it s.
(define atom?
  (lambda (arg)
    (if (pair? arg)
        #f
        #t)))

;;;Exercise 2.7.1
(define shorter
  (lambda (l1 l2)
    (let ((l1-length (length l1))
          (l2-length (length l2)))
      (cond ((< l1-length l2-length) l1)
            ((> l1-length l2-length) l2)
            (else l1)))))

;;;Section 2.8 Simple Recursion
;;;Generally recursive procedures require two basic elements, a base
;;;case, and a recursion step. With each recursion step the idea is to
;;;come closer to the base case so that eventually the recursion will
;;;terminate.
(define j-length
  (lambda (ls)
    (if (null? ls)
        0
        (+ 1 (j-length (cdr ls))))
    ))

(define j-list-copy
  (lambda (ls)
    (if (null? ls)
        '()
        (cons (car ls) (j-list-copy (cdr ls))))
    ))

(define tree-copy
  (lambda (tr)
    (if (not (pair? tr))
        tr
        (cons (tree-copy (car tr))
              (tree-copy (cdr tr))))))

;;;Exercise 2.8.1 What would happen if you reversed the order of the
;;;arguments to cons in tree-copy?
;;;You would end up reversing the tree, so that all right
;;;hand-branches are copies to the left side, and vice versa.
(define reverse-tree-copy
  (lambda (tr)
    (if (not (pair? tr))
        tr
        (cons (reverse-tree-copy (cdr tr))
              (reverse-tree-copy (car tr))))
    ))

;;;Exercise 2.8.2
(define my-append
  (lambda (list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (my-append (cdr list1) list2)))
    ))

;;;Exercise 2.8.3
(define make-list
  (lambda (length base-element)
    (if (<= length 0)
        '()
        (cons base-element (make-list (- length 1) base-element)))
    ))

;;;Exercise 2.8.4
(define my-list-tail
  (lambda (ls index)
    (if (= index 0)
        ls
        (my-list-tail (cdr ls) (- index 1)))
    ))

(define my-list-ref
  (lambda (ls index)
    (if (= index 0)
        (car ls)
        (my-list-ref (cdr ls) (- index 1)))
    ))

;;;Exercise 2.8.5
(define shorter?
  (lambda (l1 l2)
    (cond  ((and (null? l1) (not (null? l2))) #t)
           ((and (not (null? l1)) (not (null? l2)))
            (shorter? (cdr l1) (cdr l2)))
           (else #f))))

(define new-shorter
  (lambda (l1 l2)
    (cond ((shorter? l1 l2) l1)
          ((shorter? l2 l1) l2)
          (else l1))))

;;;Exercise 2.8.6
(define my-even?
  (lambda (num)
    (if (= num 0)
        #t
        (my-odd? (- num 1)))
    ))

(define my-odd?
  (lambda (num)
    (if (= num 0)
        #f
        (my-even? (- num 1)))
    ))

;;;Exercise 2.8.7
;;;Use map to define a procedure transpose that takes a list of pairs
;;;and returns a pair of lists
(define my-transpose
  (lambda (list-of-pairs)
    (cons (map car list-of-pairs)
          (map cdr list-of-pairs))))

;;;Section 2.9 Assignment
;;;Define a procedure for the quadratic formula that uses assignment
;;;to calculate and store its values
(define destructive-quadratic-formula
  (lambda (a b c)
    (let ((root1 0) (root2 0) (minusb 0) (radical 0) (divisor 0))
      (set! minusb (- 0 b))
      (set! radical (sqrt (- (* b b) (* 4 a c))))
      (set! divisor (* 2 a))
      (set! root1 (/ (+ minusb radical) divisor))
      (set! root2 (/ (- minusb radical) divisor))
      (cons root1 root2))
    ))

;;;;Define a procedure for the quadratic formula that does not use
;;;;explicit assignment, but instead bind values when the variable is
;;;;declared in the let expression.
(define quadratic-formula
  (lambda (a b c)
    (let ((minusb (- 0 b))
          (radical (sqrt (- (* b b) (* 4 a c))))
          (divisor (* 2 a)))
      (let ((root1 (/ (+ minusb radical) divisor))
            (root2 (/ (- minusb radical) divisor)))
        (cons root1 root2)))))

(define lazy
  (lambda (thunk)
    (let ((val #f)  (flag #f))
      (lambda ()
        (if (not flag)
            (begin (set! val (thunk))
                   (set! flag #t)))
        val))))

(define lazy-test
  (lazy (lambda ()
          (display "Ouch!")
          (newline)
          "got me")))

;;;An example showing assignment, along with a local let binding to
;;;create a stack data structure
(define make-stack
  (lambda ()
    (let ((ls '()))
      (lambda (msg . args)
        (cond ((eqv? msg 'empty?) (null? ls))
              ((eqv? msg 'push!) (set! ls (cons (car args) ls)))
              ((eqv? msg 'top) (car ls))
              ((eqv? msg 'pop!) (set! ls (cdr ls)))
              (else "oops"))))))

;;Import the assert module so that I can check assertions in my unit
;;tests
(use-modules (debugging assert))
;;;Create tests for the various stack functions that I will be
;;;creating, as a way of testing their accuracy
(define stack-test
  (lambda (stack-creation-procedure . args)
    (let ((stack '()))
      (if (null? args)
          (set! stack (stack-creation-procedure))
          (set! stack (stack-creation-procedure (car args))))
      (assert (stack 'empty?))
      (stack 'push! 1)
      (assert (not (stack 'empty?)))
      (assert (= (stack 'top) 1))
      (stack 'push! 2)
      (stack 'push! 3)
      (assert (= (stack 'top) 3))
      (stack 'pop!)
      (assert (= (stack 'top) 2)))))

(stack-test make-stack)

;;;Another example showing assignment to create a queue data structure.
(define make-queue
  (lambda ()
    (let ((end (cons 'ignored '())))
      (cons end end))))

(define putq!
  (lambda (q v)
    (let ((end (cons 'ignored '())))
      (set-car! (cdr q) v)
      (set-cdr! (cdr q) end)
      (set-cdr! q end))))

(define getq
  (lambda (q)
    (car (car q))))

(define delq!
  (lambda (q)
    (set-car! q (cdr (car q)))))

;;;Exercise 2.9.1
(define make-counter
  (lambda (initial increment)
    (lambda ()
      (let ((v initial))
        (set! initial (+ initial increment))
        v))))

;;;Exercise 2.9.2
(define case-make-stack
  (lambda ()
    (let ((ls '()))
      (lambda (msg . args)
        (case msg
          ((empty? mt?) (null? ls))
          ((push!) (set! ls (cons (car args) ls)))
          ((top) (car ls))
          ((pop!) (set! ls (cdr ls)))
          (else "oops"))))))

(define case-stack-test
  (lambda (stack-creation-procedure . args)
    (let ((stack '()))
      (if (null? args)
          (set! stack (stack-creation-procedure))
          (set! stack (stack-creation-procedure (car args))))
      (assert (stack 'empty?))
      (stack 'push! 1)
      (assert (not (stack 'empty?)))
      (assert (= (stack 'top) 1))
      (stack 'push! 2)
      (stack 'push! 3)
      (assert (= (stack 'top) 3))
      (stack 'pop!)
      (assert (= (stack 'top) 2)))))

(case-stack-test case-make-stack)

;;;Exercise 2.9.3
(define extended-make-stack
  (lambda ()
    (let ((ls '()))
      (lambda (msg . args)
        (case msg
          ((empty? mt?) (null? ls))
          ((push!) (set! ls (cons (car args) ls)))
          ((top) (car ls))
          ((pop!) (set! ls (cdr ls)))
          ((ref) (list-ref ls (car args)))
          ((set!) (set-car! (list-tail ls (car args)) (car (cdr args))))
          (else "oops"))))))

(define extended-stack-test
  (lambda (stack-creation-procedure . args)
    (let ((stack '()))
      (if (null? args)
          (set! stack (stack-creation-procedure))
          (set! stack (stack-creation-procedure (car args))))
      (assert (stack 'empty?))
      (stack 'push! 1)
      (assert (not (stack 'empty?)))
      (assert (= (stack 'top) 1))
      (stack 'push! 2)
      (stack 'push! 3)
      (assert (= (stack 'top) 3))
      (assert (= (stack 'ref 0) 3))
      (stack 'set! 0 4)
      (assert (= (stack 'top) 4))
      (assert (= (stack 'top) (stack 'ref 0)))
      (stack 'pop!)
      (assert (= (stack 'top) 2)))))

(extended-stack-test extended-make-stack)

;;;Exercise 2.9.4
(define make-vector-stack
  (lambda (stack-size)
    (let ((vec (make-vector stack-size))
          (current-index 0))
      (lambda (msg . args)
        (case msg
          ((empty? mt?) (= current-index 0)) 
          ((push!)
           (vector-set! vec current-index (car args))
           (set! current-index (+ current-index 1)))
          ((top)
           (vector-ref vec (- current-index 1)))
          ((pop!)
           (set! current-index (- current-index 1))
           (vector-ref vec current-index))
          ((ref) (vector-ref vec  (- current-index  (+ (car args) 1))))
          ((set!) (vector-set! vec
                               (- current-index (+ (car args) 1))
                               (car (cdr args))))
          (else 'oops!))))))

(extended-stack-test make-vector-stack 10)

;;;Exercise 2.9.5
(define emptyq?
  (lambda (q)
    (eqv? (car q) (cdr q))))

(define checked-getq
  (lambda (q)
    (if (emptyq? q)
        (error "The queue is empty and so there is nothing to get")
        (car (car q)))))

(define checked-delq!
  (lambda (q)
    (if (emptyq? q)
        (error "The queue is empty so there is nothing to delete")
        (set-car! q (cdr (car q))))))

;;;putq! getq delq! emptyq?
(define queue-test
  (lambda (queue-creation-function queue-put queue-get queue-delete queue-empty)
    (let ((q (queue-creation-function)))
      (assert (queue-empty q))
      (queue-put q 1)
      (assert (not (queue-empty q)))
      (queue-put q 2)
      (queue-put q 3)
      (assert (= (queue-get q) 1))
      (queue-delete q)
      (assert (= (queue-get q) 2))
      (queue-delete q)
      (assert (= (queue-get q) 3))
      (queue-delete q)
      (assert (queue-empty q))
      (queue-put q 'a)
      (queue-put q 'b)
      (assert (eq? (queue-get q) 'a))
      (queue-delete q)
      (assert (eq? (queue-get q) 'b))
      (queue-delete q)
      (queue-put q 'c)
      (queue-put q 'd)
      (assert (eq? (queue-get q) 'c))
      (queue-delete q)
      (assert (eq? (queue-get q) 'd)))))

(queue-test make-queue putq! getq delq! emptyq?)
(queue-test make-queue putq! checked-getq checked-delq! emptyq?)

;;;Exercise 2.9.6
(define new-make-queue
  (lambda ()
    (let ((element (cons '() '())))
      (cons element element))))

(define new-emptyq?
  (lambda (q)
    (and (null?  (car (car q))) (null? (car (cdr q))))))

(define new-putq!
  (lambda (q v)
    (if (new-emptyq? q)
        (set-car! (cdr q) v)
        (let ((element (cons v '())))
          (set-cdr! (cdr q) element)
          (set-cdr! q element)))))

(define new-getq
  (lambda (q)
    (car (car q))))

(define new-delq!
  (lambda (q)
    (if (null? (cdr (car q)))
        (let ((element (cons '() '())))
          (set-car! q element)
          (set-cdr! q element))
        (set-car! q (cdr (car q))))))

(queue-test new-make-queue new-putq! new-getq new-delq! new-emptyq?)

;;;Exercise 2.9.7
;;;In guile scheme the built in length function rejects the circular
;;;list. My implementation of length would give an infinite loop as it
;;;forever tries to find the empty list '() that would signal the end
;;;of the list.

;;;Exercise 2.9.8
;;;In guile scheme the built in list predicate returns #f when given a
;;;circular list.
(define my-list?
  (lambda (ls)
    (cond ((null? ls) #t)
          ((not (pair? ls)) #f)
          ((null? (cdr ls)) #t)
          ((not (pair? (cdr ls))) #f)
          (else (my-list? (cdr ls))))))

(define test-my-list?
  (lambda ()
    (assert (my-list? '()))
    (assert (not (my-list? '(a . b))))
    (assert (my-list? '(1 2 3)))
    (assert (not (my-list? '(a b . c))))
    (assert (not (my-list? 'a)))
    (assert (not (my-list? 1)))))

(test-my-list?)
