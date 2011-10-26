;;Test to learn working with git and git hub.
;;#lang scheme
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l)) (lat? (cdr l)))
          (else #f))))

(define rember 
  (lambda (a lat)
	(cond ((null? lat) (quote ()))
		  ((eq? (car lat) a) (cdr lat))
		  (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts
  (lambda (ll)
	(cond ((null? ll) (quote ()))
		  (else (cons (car (car ll)) (firsts (cdr ll)))))))

(define insertR
  (lambda (new old lat)
	(cond ((null? lat) (quote ()))
		  (else
		   (cond ((eq? (car lat) old) 
				  (cons (car lat) (cons new (cdr lat))))
				 (else (cons (car lat) 
							 (insertR new old (cdr lat)))))))))
(define insertL
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
		  (else
		   (cond ((eq? (car lat) old) (cons new lat))
				 (else (cons (car lat) (insertL new old (cdr lat)))))))))

(define subst
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
		  (else
		   (cond ((eq? (car lat) old) (cons new (cdr lat)))
				 (else (cons (car lat) (subst new old (cdr lat)))))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond ((null? lat) (quote ()))
		  (else
		   (cond ((or (eq? (car lat) o1) (eq? (car lat) o2)) 
				  (cons new (cdr lat)))
				 (else 
				  (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))

(define multirember
  (lambda (a lat)
    (cond ((null? lat) (quote ()))
		  (else
		   (cond ((eq? a (car lat)) (multirember a (cdr lat)))
				 (else (cons (car lat) (multirember a (cdr lat)))))))))

(define multiInsertR
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
		  ((eq? (car lat) old) (cons old 
									 (cons new 
										   (multiInsertR new old 
														 (cdr lat)))))
		  (else (cons (car lat) 
					  (multiInsertR new old 
									(cdr lat)))))))

(define multiInsertL
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
		  ((eq? (car lat) old) (cons new 
									 (cons old 
										   (multiInsertL new old 
														 (cdr lat)))))
		  (else (cons (car lat) 
					  (multiInsertL new old 
									(cdr lat)))))))

(define multisubst
  (lambda (new old lat)
	(cond ((null? lat) (quote ()))
		  (else
		   (cond ((eq? (car lat) old) 
				  (cons new (multisubst new old (cdr lat))))
				 (else (cons (car lat) 
							 (multisubst new old (cdr lat)))))))))


;;Chapter 4
(define add1
  (lambda (n)
	(+ n 1)))

(define sub1
  (lambda (n)
	(- n 1)))

(define zero?
  (lambda (n)
	(= n 0)))

(define o+
  (lambda (x y)
	(cond ((zero? y) x)
		  (else (o+ (add1 x) (sub1 y))))))

(define o-
  (lambda (x y)
	(cond ((zero? y) x)
		  (else (o- (sub1 x) (sub1 y))))))

(define addtup
  (lambda (tup)
	(cond ((null? tup) 0)
		  (else (o+ (car tup) (addtup (cdr tup)))))))

(define o*
  (lambda (x y)
	(cond ((zero? y) 0)
		  (else (o+ x (o* x (sub1 y)))))))

(define tup+
  (lambda (tup1 tup2)
	(cond ((null? tup1) tup2)
		  ((null? tup2) tup1)
		  (else (cons (o+ (car tup1) (car tup2)) 
					  (tup+ (cdr tup1) (cdr tup2)))))))

(define o>
  (lambda (x y)
	(cond ((zero? x) #f)
		  ((zero? y) #t)
		  (else (o> (sub1 x) (sub1 y))))))

(define o<
  (lambda (x y)
	(cond ((zero? y) #f)
		  ((zero? x) #t)
		  (else (o< (sub1 x) (sub1 y))))))

(define o=
  (lambda (x y)
	(cond ((zero? y) (zero? x))
		  ((zero? x) #f)
		  (else (= (sub1 x) (sub1 y))))))

(define alt-=
  (lambda (x y)
	(cond ((or (o< x y) (o> x y)) #f)
		  (else #t))))

(define o^
  (lambda (x y)
	(cond ((zero? y) 1)
		  (else (o* x (o^ x (sub1 y)))))))

(define o/
  (lambda (x y)
	(cond ((< x y) 0)
		  (else (add1 (o/ (o- x y) y))))))

(define length
  (lambda (lat)
	(cond ((null? lat) 0)
		  (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
	(cond ((one? n) (car lat))
		  (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
	(cond ((one? n) (cdr lat))
		  (else (cons (car lat) 
					  (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
	(cond ((null? lat) (quote ()))
		  ((number? (car lat)) (no-nums (cdr lat)))
		  (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
	(cond ((null? lat) (quote ()))
		  ((number? (car lat)) 
		   (cons (car lat) (all-nums (cdr lat))))
		  (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
	(cond ((and (number? a1) (number? a2)) (o= a1 a1))
		  ((and (atom? a1) (atom? a2)) (eq? a1 a2))
		  (else #f))))

(define occur
  (lambda (a lat)
	(cond ((null? lat) 0)
		  (else
		   (cond ((eqan? (car lat) a) (add1 (occur a (cdr lat))))
				 (else (occur a (cdr lat))))))))

(define one?
  (lambda (n)
	(o= n 1)))

;;Chapter 5
(define rember*
  (lambda (a list)
	(cond ((null? list) (quote ()))
		  ((atom? (car list))
		   (cond ((eq? (car list) a) (rember* a (cdr list)))
				 (else (cons (car list) (rember* a (cdr list))))))
		  (else (cons (rember* a (car list)) 
					  (rember* a (cdr list)))))))

(define insertR*
  (lambda (new old list)
	(cond ((null? list) (quote ()))
		  ((atom? (car list)) 
		   (cond ((eq? (car list) old) 
				  (cons old (cons new 
								  (insertR* new old (cdr list)))))
				 (else (cons (car list) 
							 (insertR* new old (cdr list))))))
		  (else (cons (insertR* new old (car list)) 
					  (insertR* new old (cdr list)))))))

(define occur*
  (lambda (a list)
	(cond ((null? list) 0)
		  ((atom? (car list))
		   (cond ((eq? a (car list)) 
				  (add1 (occur* a (cdr list))))
				 (else (occur* a (cdr list)))))
		  (else (o+ (occur* a (car list)) (occur* a (cdr list)))))))

(define subst*
  (lambda (new old list)
	(cond ((null? list) (quote ()))
		  ((atom? (car list)) 
		   (cond ((eq? old (car list)) 
				  (cons new (subst* new old (cdr list))))
				 (else (cons (car list) 
							 (subst* new old (cdr list))))))
		  (else (cons (subst* new old (car list))
					  (subst* new old (cdr list)))))))

(define insertL*
  (lambda (new old list)
	(cond ((null? list) (quote ()))
		  ((atom? (car list)) 
		   (cond ((eq? old (car list)) 
				  (cons new (cons old 
								  (insertL* new old (cdr list)))))
				 (else (cons (car list) 
							 (insertL* new old (cdr list))))))
		  (else (cons (insertL* new old (car list))
					  (insertL* new old (cdr list)))))))

(define member*
  (lambda (a list)
	(cond ((null? list) #f)
		  ((atom? (car list)) 
		   (cond ((eq? a (car list)) #t)
				 (else (member* a (cdr list)))))
		  (else (or (member* a (car list)) 
					(member* a (cdr list)))))))

(define leftmost
  (lambda (list)
	(cond ((atom? (car list)) (car list))
		  (else (leftmost (car list))))))

(define eqlist?
  (lambda (l1 l2)
	(cond ( (and (null? l1) (null? l2)) #t)
		  ( (or (null? l1) (null? l2)) #f)
		  ( (and (atom? (car l1)) (atom? (car l2))) 
			(and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
		  ( (or  (atom? (car l1)) (atom? (car l2))) #f)
		  (else (and (eqlist? (car l1) (car l2)) 
					 (eqlist? (cdr l1) (cdr l2)))))))

(define alteqlist?
  (lambda (l1 l2)
	(cond
	 ((and (null? l1) (null? l2)) #t)
	 ((and (null? l1) (atom? (car 12))) #f)
	 ((null? l1) #f)
	 ((and (atom? (car l1)) (null? l2)) #f)
	 ((and (atom? (car l1)) (atom? (car l2)))
	  (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
	 ((atom? (car l1)) #f)
	 ((null? l2) #f)
	 ((atom? (car l2)) #f)
	 (else (and (eqlist? (car l1) (car l2)) 
				(eqlist? (cdr l1) (cdr l2)))))))

(define jeff-equal?
  (lambda (s1 s2)
	(cond ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
		  ((or (atom? s1) (atom? s2)) #f)
		  (else (eqlist? s1 s2)))))

(define new-eqlist?
  (lambda (l1 l2)
	(cond ((and (null? l1) (null? l2)) #t)
		  ((or (null? l1) (null? l2)) #f)
		  (else 
		   (and (jeff-equal? (car l1) (car l2)) 
				(eqlist? (cdr l1) (cdr l2)))))))

(define jeff-rember
  (lambda (s l)
    (cond ((null? l) (quote ()))
          ((atom? (car l)) 
		   (cond ((equal? (car l) s) (cdr l))
				 (else (cons (car l) (rember s (cdr l))))))
          (else 
		   (cond ((equal? (car l) s) (cdr l))
				 (else (cons (car l) (rember s (cdr l)))))))))


(define numbered?
  (lambda (ae)
	(cond ((atom? ae) (number? ae))
		  ((eq? (car (cdr ae)) (quote +)) 
		   (and (numbered? (car ae)) 
				(numbered? (car (cdr (cdr ae))))))
		  ((eq? (car (cdr ae)) (quote *)) 
		   (and (numbered? (car ae)) 
				(numbered? (car (cdr (cdr ae))))))
		  ((eq? (car (cdr ae)) (quote ^)) 
		   (and (numbered? (car ae)) 
				(numbered? (car (cdr (cdr ae)))))))))

(define old-value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          ((null? nexp) 0)
          ((< (length nexp) 3) (old-value (car nexp)))
          (else (cond ((eq? (car (cdr nexp)) (quote +))
                       (+ (old-value (car nexp)) 
						  (old-value (cdr (cdr nexp)))))
                      ((eq? (car (cdr nexp)) (quote *))
                       (* (old-value (car nexp)) 
						  (old-value (cdr (cdr nexp)))))
                      ((eq? (car (cdr nexp)) (quote ^))
                       (o^ (old-value (car nexp)) 
						   (old-value (cdr (cdr nexp))))))))))

(define old-prefix-value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          ((eq? (prefix-operator nexp) (quote +))
           (+ (old-prefix-value (prefix-1st-sub-exp nexp))  
			  (old-prefix-value (prefix-2nd-sub-exp nexp))))
          ((eq? (prefix-operator nexp) (quote *))
           (* (old-prefix-value (prefix-1st-sub-exp nexp)) 
			  (old-prefix-value (prefix-2nd-sub-exp nexp))))
          (else (o^ (old-prefix-value (prefix-1st-sub-exp nexp)) 
					(old-prefix-value (prefix-2nd-sub-exp nexp)))))))

(define value
  (lambda (nexp first-sub-exp second-sub-exp operator)
    (cond ((atom? nexp) nexp)
          ((eq? (operator nexp) (quote +))
           (+ (value (first-sub-exp nexp) first-sub-exp 
                     second-sub-exp operator)  
              (value (second-sub-exp nexp) first-sub-exp 
                     second-sub-exp operator)))
          ((eq? (operator nexp) (quote *))
           (* (value (first-sub-exp nexp) first-sub-exp 
                     second-sub-exp operator) 
              (value (second-sub-exp nexp) first-sub-exp 
                     second-sub-exp operator)))
          (else (o^ (value (first-sub-exp nexp) first-sub-exp 
                           second-sub-exp operator) 
					(value (second-sub-exp nexp) first-sub-exp 
						   second-sub-exp operator))))))

(define prefix-value
  (lambda (nexp)
    (value nexp prefix-1st-sub-exp prefix-2nd-sub-exp prefix-operator)))

(define infix-value
  (lambda (nexp)
    (value nexp infix-1st-sub-exp infix-2nd-sub-exp infix-operator)))

(define infix-1st-sub-exp
  (lambda (nexp)
    (car nexp)))

(define infix-2nd-sub-exp
  (lambda (nexp)
    (car (cdr (cdr nexp)))))

(define infix-operator
  (lambda (nexp)
    (car (cdr nexp))))

(define prefix-1st-sub-exp
  (lambda (nexp)
    (car (cdr nexp))))

(define prefix-2nd-sub-exp
  (lambda (nexp)
    (car (cdr (cdr nexp)))))

(define prefix-operator
  (lambda (nexp)
    (car nexp)))
;;(prefix-value '(* (+ 1 3) (+ 1 2)))

;;Chapter 7 Defs
(define member?
  (lambda (atom lat)
    (cond ((null? lat) #f)
          ((eq? atom (car lat)) #t)
          (else (member? atom (cdr lat))))))

(define set?
  (lambda (lat)
    (cond ((null? lat) #t)
          ((member? (car lat) (cdr lat)) #f)
          (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond ((null? lat) (quote ()))
          ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
          (else (cons (car lat) (makeset (cdr lat)))))))

(define alt-makeset
  (lambda (lat)
    (cond ((null? lat) (quote ()))
          (else 
		   (cons (car lat) 
				 (alt-makeset (multirember (car lat) (cdr lat))))))))


(define subset?
  (lambda (set1 set2)
    (cond ((null? set1) #t)
          ((member? (car set1) set2) (subset? (cdr set1) set2))
          (else #f))))

(define alt-subset?
  (lambda (set1 set2)
    (cond ((null? set1) #t)
          (else (and (member? (car set1) set2) 
					 (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond ((null? set1) #f)
          (else (or (member? (car set1) set2) 
                    (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond ((null? set1) (quote ()))
          ((member? (car set1) set2) 
		   (cons (car set1) (intersect (cdr set1) set2)))
          (else (intersect (cdr set1) set2)))))

(define jeff-append
  (lambda (list1 list2)
    (cond ((null? list1) list2)
          (else (cons (car list1) 
                      (jeff-append (cdr list1) list2))))))

(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
          ((member? (car set1) set2) (union (cdr set1) set2))
          (else (cons (car set1) (union (cdr set1) set2))))))

(define alt-union
  (lambda (set1 set2)
    (makeset (jeff-append set1 set2))))

(define set-difference
  (lambda (set1 set2)
    (cond ((null? set1) (quote ()))
          ((member? (car set1) set2) (set-difference (cdr set1) set2))
          (else (cons (car set1) (set-difference (cdr set1) set2))))))

(define member-all?
  (lambda (atom l-set)
    (cond ((null? (cdr l-set)) (member? atom (car l-set)))
          (else (and (member? atom (car l-set)) 
					 (member-all? atom (cdr l-set)))))))

(define intersect-all
  (lambda (l-set)
	(cond ((null? (cdr l-set)) (car l-set))
		  (else (intersect (car l-set) (intersect-all (cdr l-set)))))))

(define pair?
  (lambda (var)
    (cond ((atom? var) #f)
          ((null? var) #f)
          ((null? (cdr var)) #f)
          ((null? (cdr (cdr var))) #t)
          (else #f))))

(define first
  (lambda (pair)
    (car pair)))

(define second
  (lambda (pair)
    (car (cdr pair))))

(define build
  (lambda (x y)
    (cons x (cons y (quote ())))))

(define third
  (lambda (list)
    (car (cdr (cdr list)))))

(define fun?
  (lambda (relation)
    (set? (firsts relation))))

(define jreverse
  (lambda (list)
    (cond ((null? list) (quote ()))
          (else (jeff-append (jreverse (cdr list)) 
							 (cons (car list) (quote ())))))))

(define revrel
  (lambda (relation)
    (cond ((null? relation) (quote ()))
		  (else (cons (revpair (car relation)) 
					  (revrel (cdr relation)))))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define fullfun?
  (lambda (fun)
    (fun? (revrel fun))))

(define seconds
  (lambda (ll)
    (cond ((null? ll) (quote ()))
		  (else (cons (car (cdr (car ll))) 
					  (seconds (cdr ll)))))))

(define alt-fullfun?
  (lambda (fun)
    (set? (seconds fun))))

;;Start of functions of chapter 8: Lambda the Ultimate

(define rember-f
  (lambda (test? atom list)
    (cond ((null? list) (quote ()))
		  ((test? atom (car list)) (cdr list))
		  (else (cons (car list) (rember-f test? atom (cdr list)))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define new-rember-f
  (lambda (test?)
    (lambda (atom list)
      (cond ((null? list) (quote ()))
			((test? atom (car list)) (cdr list))
			(else (cons (car list) 
						((new-rember-f test?) atom (cdr list))))))))

(define insertL-f
  (lambda (test?)
    (lambda (new old list)
      (cond ((null? list) (quote ()))
			((test? old (car list)) (cons new list))
			(else (cons (car list) 
						((insertL-f test?) new old (cdr list))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond ((null? lat) (quote ()))
			((test? old (car lat)) (cons (car lat) (cons new (cdr lat))))
			(else (cons (car lat) 
						((insertR-f test?) new old (cdr lat))))))))

(define insert-g
  (lambda (position)
    (lambda (test?)
      (lambda (new old lat)
		(cond ((null? lat) (quote ()))
			  ((test? old (car lat)) (position new old (cdr lat)))
			  (else 
			   (cons 
				(car lat) 
				(((insert-g position) test?) new old (cdr lat)))))))))
(define seqL
  (lambda (first second third)
    (cons first (cons second third))))

(define seqR
  (lambda (first second third)
    (cons second (cons first third))))

(define generic-insertL (insert-g seqL))

(define generic-eq?-insertL (generic-insertL eq?))

(define substFunc
  (lambda (new old lat)
    (cons new lat)))

(define generic-subst ((insert-g substFunc) eq?))

(define generic-rember ((insert-g (lambda (new old lat) lat)) eq?))

(define atom-to-function
  (lambda (x)
    (cond ((eq? x (quote +)) +)
		  ((eq? x (quote *)) *)
		  (else o^))))

(define generic-value
  (lambda (nexp first-sub-exp second-sub-exp operator)
    (cond ((atom? nexp) nexp)
          (else ((atom-to-function (operator nexp)) 
				 (value (first-sub-exp nexp) 
						first-sub-exp second-sub-exp operator)  
				 (value (second-sub-exp nexp) 
						first-sub-exp second-sub-exp operator))))))

(define generic-prefix-value
  (lambda (nexp)
    (value nexp prefix-1st-sub-exp prefix-2nd-sub-exp prefix-operator)))

(define generic-infix-value
  (lambda (nexp)
    (value nexp infix-1st-sub-exp infix-2nd-sub-exp infix-operator)))

(define multirember-f
  (lambda (test?)
    (lambda (lat)
      (cond ((null? lat) (quote ()))
			((test? (car lat)) ((multirember-f test?)  (cdr lat)))
			(else (cons (car lat) ((multirember-f test?) (cdr lat))))))))

(define multirember&co
  (lambda (a lat col)
    (cond ((null? lat) (col (quote ()) (quote ())))
		  ((eq? (car lat) a) 
		   (multirember&co a (cdr lat) 
						   (lambda (newlat seen)
							 (col newlat 
								  (cons (car lat) seen)))))
		  (else 
		   (multirember&co a (cdr lat) (lambda (newlat seen)
										 (col (cons (car lat) newlat)
											  seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat (cons (quote tuna) seen))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond ((not (eq? oldL oldR))
		   (cond ((null? lat) (quote ()))
				 ((eq? (car lat) oldR) 
				  (cons 
				   oldR (cons new (multiinsertLR new oldL oldR 
												 (cdr lat)))))
				 ((eq? 
				   (car lat) oldL) 
				  (cons 
				   new 
				   (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
				 (else 
				  (cons 
				   (car lat) 
				   (multiinsertLR new oldL oldR (cdr lat)))))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond ((null? lat) (col (quote ()) 0 0))
          ((eq? (car lat) oldL) 
		   (multiinsertLR&co new oldL oldR (cdr lat) 
							 (lambda (newlat li ri) 
							   (col (cons new (cons (car lat) newlat))
									(+ li 1) ri))))
          ((eq? (car lat) oldR) 
		   (multiinsertLR&co new oldL oldR (cdr lat)
							 (lambda (newlat li ri)
							   (col (cons (car lat) (cons new newlat))
									li (+ ri 1)))))
          (else 
		   (multiinsertLR&co new oldL oldR (cdr lat) 
							 (lambda (newlat li ri)
							   (col (cons (car lat) newlat) li ri)))))))

(define alt-multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond ((null? lat) (col (quote ()) (quote ())))
          ((eq? (car lat) oldL) 
		   (alt-multiinsertLR&co 
			new oldL oldR (cdr lat)
			(lambda (leftlat rightlat)
			  (col (cons new (cons (car lat) leftlat))
				   (cons (car lat) rightlat)))))
          ((eq? (car lat) oldR) 
		   (alt-multiinsertLR&co 
			new oldL oldR (cdr lat)
			(lambda (leftlat rightlat)
			  (col (cons (car lat) leftlat)
				   (cons (car lat) (cons new rightlat))))))
          (else (alt-multiinsertLR&co 
				 new oldL oldR (cdr lat)
				 (lambda (leftlat rightlat)
				   (col (cons (car lat) leftlat)
						(cons (car lat) rightlat))))))))

(define even?
  (lambda (n)
    (o= (o* (o/ n 2) 2) n)))

(define evens-only*
  (lambda (listOfNums)
    (cond ((null? listOfNums) (quote ()))
          ((atom? (car listOfNums)) 
		   (cond ((even? (car listOfNums)) 
				  (cons (car listOfNums) (evens-only* (cdr listOfNums))))
				 (else (evens-only* (cdr listOfNums)))))
          (else (cons (evens-only* (car listOfNums)) 
					  (evens-only* (cdr listOfNums)))))))

(define evens-only*&co
  (lambda (listOfNums col)
    (cond ((null? listOfNums) (col (quote ()) 1 0))
          ((atom? (car listOfNums)) 
           (cond ((even? (car listOfNums))
                  (cons (car listOfNums) 
                        (evens-only*&co (cdr listOfNums) 
										(lambda (list prod sum)
										  (col (cons (car listOfNums)
													 list)
											   (o* (car listOfNums) prod)
											   sum)))))
                 (else (evens-only*&co 
						(cdr listOfNums)
						(lambda (list prod sum)
						  (col list prod 
							   (o+ sum (car listOfNums))))))))
          (else (evens-only*&co 
				 (car listOfNums) 
				 (lambda (carList carProd carSum)
				   (evens-only*&co 
					(cdr listOfNums)
					(lambda (cdrList cdrProd cdrSum)
					  (col (cons carList cdrList)
						   (* carProd cdrProd)
						   (+ carSum cdrSum))))))))))

(define the-last-friend
  (lambda (newl prod sum)
    (cons sum (cons prod newl))))

;;The next functions are about building up an arithmetic system 
;;using null lists inside a list to represent numbers.  () is zero, 
;;(()) is one and (() ()) is two.
(define add
  (lambda (num1 num2 jzero? add1 sub1)
    (cond ((jzero? num2) num1)
          (else (add (add1 num1) (sub1 num2) jzero? add1 sub1))))) 

(define list-add1
  (lambda (num)
    (cons (quote ()) num)))

(define list-sub1
  (lambda (num)
    (cdr num)))

(define list-zero?
  (lambda (num)
    (null? num)))

(define list-add
  (lambda (num1 num2)
    (add num1 num2 list-zero? list-add1 list-sub1)))

;;Chapter 7 definitions - Friends and Relations

(lat? '(a b c))
(rember 'and '(bacon lettuce and tomato))
(rember 'sauce '(soy sauce and tomato sauce))
(firsts '((a b) (c d) (e f)))
(subst 'topping 'fudge '(ice cream with fudge for dessert))
(insertL 'jalapeno 'and '(tacos tamales and salsa))
(insertR 'e 'd '(a b c d f g d h))
(subst2 'vanilla 'chocolate 'banana 
		'(banana ice cream with chocolate topping))
(multirember 'cup '(coffee cup tea cup and hick cup))
(multiInsertL 'fried 'fish '(chips and fish or fish and fries))
(insertL* 'pecker 'chuck 
		  '((how much (wood)) could ((a (wood) chuck)) 
			(((chuck))) (if (a) ((wood chuck))) could chuck wood))
(subst* 'orange 'banana 
		'((banana) (split ((((banana ice))) (cream (banana)) sherbet)) 
		  (banana) (bread) (banana brandy)))
(occur* 'banana '((banana) 
				  (split ((((banana ice))) (cream (banana)) sherbet)) 
				  (banana) (bread) (banana brandy)))
(insertR* 'roast 'chuck '
		  ((how much (wood)) could ((a (wood) chuck)) 
		   (((chuck))) (if (a) ((wood chuck))) could chuck wood))
(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
(member* 'chips '((potato) (crisps ((with) fish) (crisps))))
(member* 'chips '((potato) (crisps ((with) fish) (crisps))))
(eqlist? '(strawberry ice cream) '(strawberry ice cream))
(eqlist? '(strawberry ice cream) '(strawberry cream ice))
(eqlist? '(beef ((sausage)) 
				(and (soda))) '(beef ((sausage)) (and (soda))))
(eqlist? '(beef ((sausage)) 
				(and (soda))) '(beef ((salami)) (and (soda))))

(infix-value 13)
(infix-value '(1 + 3))
(infix-value '(1 + (3 ^ 4)))
(prefix-value '(+ 3 4))
(prefix-value '(+ (* 3 6) (^ 8 2)))

;;The 3rd Commandment
;;  When describing a list, describe the first typical element
;;  and then cons it onto the natural recursion


;;Restarting work from Chapter 7 onward
(define set?
  (lambda (lat)
	(cond ((null? lat) #t)
		  ((member? (car lat) (cdr lat)) #f)
		  (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
	(cond ((null? lat) (quote ()))
		  ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
		  (else (cons (car lat) (makeset (cdr lat)))))))

(define multirember-makeset
  (lambda (lat)
	(cond ((null? lat) (quote ()))
		  (else (cons (car lat) 
					  (multirember-makeset (multirember (car lat) 
														lat)))))))

(define subset?
  (lambda (lat1 lat2)
	(cond ((null? lat1) #t)
		  ((member? (car lat1) lat2) (subset? (cdr lat1) lat2))
		  (else #f))))

(define subset-with-and?
  (lambda (lat1 lat2)
	(cond ((null? lat1) #t)
		  (else
		   (and (member? (car lat1) lat2)
				(subset? (cdr lat1) lat2))))))

(define eqset?
  (lambda (lat1 lat2)
	(cond ((and (subset? lat1 lat2)
				(subset? lat2 lat1)) #t)
		  (else #f))))

(define intersect?
  (lambda (lat1 lat2)
	(cond ((null? lat1) #t)
		  (else (or (member? (car lat1) lat2) 
					(intersect? (cdr lat1) lat2))))))

(define intersect
  (lambda (lat1 lat2)
	(cond ((or (null? lat1) (null? lat2)) (quote ()))
		  ((member? (car lat1) lat2) (cons (car lat1) 
										   (intersect (cdr lat1) lat2)))
		  (else (intersect (cdr lat1) lat2)))))

(define union
  (lambda (lat1 lat2)
	(cond ((null? lat1) lat2)
		  ((member? (car lat1) lat2)
		   (union (cdr lat1) lat2))
		  (else (cons (car lat1) (union (cdr lat1) lat2))))))

(define set-difference
  (lambda (lat1 lat2)
	(cond ((null? lat1) (quote ()))
		  ((member? (car lat1) lat2)
		   (set-difference (cdr lat1) lat2))
		  (else (cons (car lat1) (set-difference (cdr lat1) lat2))))))

(define a-pair?
  (lambda (l)
	(cond  ((atom? l) #f)
		   ((null? l) #f)
		   ((null? (cdr l)) #f)
		   ((null? (cdr (cdr l))) #t)
		   (else #f))))

;;fun? looks for a list of pairs, in which no pair has it's first
;;element repeated.		 
(define fun?
  (lambda (l)
	(set? (firsts l))))

(define revrel
  (lambda (l)
	(cond ((null? l) (quote ()))
		  (else (cons (revpair (car l))
					  (revrel (cdr l)))))))

(define revpair 
  (lambda (pair)
	(build (second pair) (first pair))))

(define rember-f
  (lambda (pred elem list)
	(cond ((null? list) (quote ()))
		  ((pred elem (car list)) (rember-f pred elem (cdr list)))
		  (else (cons (car list) (rember-f pred elem (cdr list)))))))

;;first test of rember-f
(equal? '(6 2 3) (rember-f = 5 '(6 2 5 3)))
;;second test of rember-f
(equal? '(beans are good) (rember-f eq? 'jelly '(jelly beans are good)))
;;third test of rember-f
(equal? '(lemonade and (cake)) (rember-f equal?
										 '(pop corn)
										 '(lemonade (pop corn) and (cake))))
(define rember-c
  (lambda (test?)
	(lambda (elem list)
	  (cond ((null? list) (quote ()))
			((test? elem (car list)) (cdr list))
			(else (cons (car list) ((rember-c test?) elem (cdr list))))))))

;;first test for rember-c
(equal? '(salad is good) ((rember-c eq?) 'tuna '(tuna salad is good)))
;;second test for rember-c
(equal? '(shrimp salad and salad) ((rember-c eq?)
								   'tuna
								   '(shrimp salad and tuna salad)))
;;third test for rember-c
(equal? '(equal? eqan? eqlist? eqpair?)
		((rember-c eq?)
		 'eq?
		 '(equal? eq? eqan? eqlist? eqpair?)))

(define insertL-f
  (lambda (test?)
	(lambda (new old lat)
	  (cond ((null? lat) (quote ()))
			((test? (car lat) old) (cons new lat))
			(else (cons (car lat)
						((insertL-f test?) new old (cdr lat))))))))

(define insertR-f
  (lambda (test?)
	(lambda (new old lat)
	  (cond ((null? lat) (quote ()))
			((test? (car lat) old) (cons old (cons new (cdr lat))))
			(else (cons (car lat)
						((insertL-f test?) new old (cdr lat))))))))

;;Chapter 9 ...and Again, and Again, and Again,...
(define looking
  (lambda (a lat)
	(keep-looking a (pick 1 lat) lat)))


(define keep-looking
  (lambda (a sorn lat)
	(cond ((number? sorn) (keep-looking a (pick sorn lat) lat))
		  (else (eq? sorn a)))))

(define shift
  (lambda (pair)
	(build (first (first pair))
		   (build (second (first pair)) (second pair)))))
;;first test of shift
(equal? '(a (b c)) (shift '((a b) c)))
;;second test of shift
(equal? '(a (b (c d))) (shift '((a b) (c d))))

(define align
  (lambda (pora)
	(cond ((atom? pora) pora)
		  ((a-pair? (first pora)) (align (shift pora)))
		  (else (build (first pora) (align (second pora)))))))

;;function length(0)
((lambda (length)
   (lambda (l)
	 (cond ((null? l) 0)
		   (else (add1 (length (cdr l))))))) eternity)

;;function length<=1
((lambda (length<=1)
   (lambda (l)
	 (cond ((null? l) 0)
		   (else (add1 (length (cdr l)))))))
 ((lambda (lenght0)
   (lambda (l)
	 (cond ((null? l) 0)
		   (else (add1 (lenght (cdr l))))))) eternity))
