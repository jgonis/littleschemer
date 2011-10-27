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

