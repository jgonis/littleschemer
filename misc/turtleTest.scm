(import (turtle))

(define t (turtle-create-default '(a b c)))
(define margin .000000001)

(assert (< (- (turtle-get-position-x t) 0.0) margin))
(assert (< (- (turtle-get-position-y t) 0.0) margin))

(assert (< (abs
			(- (turtle-get-heading-x (turtle-left t 90)) -1.0))
		   margin))
(assert (<
		 (abs (turtle-get-heading-y (turtle-left t 90)))
		 margin))

(assert (<
		 (op)
		 margin))

(define turtle-repeat
  (lambda (turtle n)
	(cond ((> n 0) (turtle-repeat '() (- n 1)))
		  (else turtle))))