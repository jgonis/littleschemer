(define expand-test
  (lambda (x)
	(= (factored-form x) (expanded-form x))))

(define expand-test-loop
  (lambda (loop-limit)
	(cond ((= loop-limit 0) #t)
		  (else (and (expand-test loop-limit)
					 (expand-test-loop (- loop-limit 1)))))))

(define factored-form
  (lambda (x)
	(* (+ (* x x) 1 (* -2 x))
	   (- x 3)
	   (- (* 4 (* x x x)) (* 3 x)))))

(define expanded-form
  (lambda (x)
	(+ (* 4 (* x x x x x x)) 
	   (* -20 (* x x x x x))
	   (* 25 (* x x x x))
	   (* 3 (* x x x))
	   (* -21 (* x x))
	   (* 9 x))))
