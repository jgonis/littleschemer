;;Restarting The Little Schemer
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l)) (lat? (cdr l)))
          (else #f))))

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          ((eq? a (car lat)) #t)
          (else (member? a (cdr lat))))))

(define rember
  (lambda (a lat)
    (cond ((null? lat) (quote ()))
          ((eq? a (car lat)) (cdr lat))
          (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond ((null? l) (quote ()))
          (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? old (car lat)) (cons old (cons new (cdr lat))))
          (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? (car lat) old) (cons new (cons old (cdr lat))))
          (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? (car lat) old) (cons new (cdr lat)))
          (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond ((null? lat) (quote ()))
          ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
          (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond ((null? lat) (quote ()))
          ((eq? (car lat) a) (multirember a (cdr lat)))
          (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? (car lat) old) (cons old
                                     (cons new (multiinsertR new
                                                              old
                                                              (cdr lat)))))
          (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? (car lat) old) (cons new
                                     (cons old (multiinsertL new
                                                             old
                                                             (cdr lat)))))
          (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
          (else (cons (car lat) (multisubst new old (cdr lat)))))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define j-+
  (lambda (x y)
    (cond ((zero? y) x)
          (else (j-+ (add1 x) (sub1 y))))))

(define j--
  (lambda (x y)
    (cond ((zero? y) x)
          (else (j-- (sub1 x) (sub1 y))))))

(define addtup
  (lambda (tup)
    (cond ((null? tup) 0)
          (else (j-+ (car tup) (addtup (cdr tup)))))))

(define j-*
  (lambda (x y)
    (cond ((zero? y) 0)
          (else (j-+ x (j-* x (sub1 y)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond ((null? tup1) tup2)
          ((null? tup2) tup1)
          (else (cons (j-+ (car tup1) (car tup2))
                      (tup+ (cdr tup1) (cdr tup2)))))))

(define j->
  (lambda (x y)
    (cond ((zero? x) #f)
          ((zero? y) #t)
          (else (j-> (sub1 x) (sub1 y))))))

(define j-<
  (lambda (x y)
    (cond ((zero? y) #f)
          ((zero? x) #t)
          (else (j-< (sub1 x) (sub1 y))))))

(define j-=
  (lambda (x y)
    (cond ((zero? y) (zero? x))
          ((zero? x) #f)
          (else (j-= (sub1 x) (sub1 y))))))

(define alt-j-=
  (lambda (x y)
    (cond ((or (j-< x y) (j-> x y)) #f)
          (else #t))))

(define ^
  (lambda (x y)
    (cond ((zero? y) 1)
          ((zero? (sub1 y)) x)
          (else (j-* x (^ x (sub1 y)))))))

(define div
  (lambda (x y)
    (cond ((< x y) 0)
          (else (add1 (div (j-- x y) y))))))

(define j-length
  (lambda (lat)
    (cond ((null? lat) 0)
          (else (add1 (j-length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (car lat))
          (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (cdr lat))
          (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond ((null? lat) (quote ()))
          ((number? (car lat)) (no-nums (cdr lat)))
          (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond ((null? lat) (quote ()))
          ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
          (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (cond ((and (number? a1) (number? a2)) (j-= a1 a2))
          ((and (atom? a1) (atom? a2)) (eq? a1 a2))
          (else #f))))

(define occur
  (lambda (a lat)
    (cond ((null? lat) 0)
          ((eqan? (car lat) a) (add1 (occur a (cdr lat))))
          (else (occur a (cdr lat))))))

(define one?
  (lambda (n)
    (j-= n 1)))

(define new-rempick
  (lambda (n lat)
    (cond ((one? n) (cdr lat))
          (else (cons (car lat) (new-rempick (sub1 n) (cdr lat)))))))

;;Chapter 5
(define rember*
  (lambda (a l)
    (cond ((null? l) (quote ()))
          ((atom? (car l))
           (cond ((eq? a (car l)) (rember* a (cdr l)))
                 (else (cons (car l) (rember* a (cdr l))))))
          (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond ((null? l) (quote ()))
          ((atom? (car l))
           (cond ((eq? (car l) old) (cons old
                                          (cons new
                                                (insertR* new old (cdr l)))))
                 (else (cons (car l)
                             (insertR* new old (cdr l))))))
          (else (cons (insertR* new old (car l))
                      (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond ((null? l) 0)
          ((atom? (car l)) (cond ((eqan? a (car l))
                                  (j-+ 1 (occur* a (cdr l))))))
          (else (j-+ (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond ((null? l) (quote ()))
          ((atom? (car l))
           (cond ((eq? (car l) old)
                  (cons new (subst* new old (cdr l))))
                 (else (cons (car l) (subst* new old (cdr l))))))
          (else (cons (subst* new old (car l))
                      (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond ((null? l) (quote ()))
          ((atom? (car l))
           (cond ((eq? (car l) old)
                  (cons new (cons old (insertL* new old (cdr l)))))
                 (else (cons (car l) (insertL* new old (cdr l))))))
          (else (cons (insertL* new old (car l))
                      (insertL* new old (cdr l)))))))
(define member*
  (lambda (a l)
    (cond ((null? l) #f)
          ((atom? (car l))
           (cond ((eq? a (car l)) #t)
                 (else (member* a (cdr l)))))
          (else (or (member* a (car l)) (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond ((atom? (car l)) (car l))
          (else (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
          ((or (null? l1) (null? l2)) #f)
          (else (and (j-equal? (car l1) (car l2))
                     (eqlist? (cdr l1) (cdr l2)))))))

(define j-equal?
  (lambda (sexp1 sexp2)
  (cond ((and (atom? sexp1) (atom? sexp2)) (eqan? sexp1 sexp2))
        ((or (atom? sexp1) (atom? sexp2)) #f)
        (else (eqlist? sexp1 sexp2)))))

(define rember
  (lambda (s l)
    (cond ((null? l) (quote ()))
          ((j-equal? (car l) s) (cdr l))
          (else (cons (car l)
                      (rember s (cdr l)))))))


