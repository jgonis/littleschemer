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

