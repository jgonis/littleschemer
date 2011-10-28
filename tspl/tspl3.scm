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
(let ((a (* 3 a)))
  (+ (- a b) (+ a b)))

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

