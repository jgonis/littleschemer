(library
 (turtle)
 (export turtle-create-default
	 turtle-create
	 ;;getters
	 turtle-get-position-x
	 turtle-get-position-y
	 turtle-get-heading-x
	 turtle-get-heading-y
	 turtle-get-penstate
	 turtle-get-context
	 turtle-get-position
	 turtle-get-heading
	 ;;setters
	 turtle-set-position-x
	 turtle-set-position-y
	 turtle-set-heading-x
	 turtle-set-heading-y
	 turtle-set-penstate
	 turtle-set-context
	 ;;mutators
	 turtle-forward
	 turtle-back
	 turtle-left
	 turtle-right)
 
 (import (rnrs)
	 (ypsilon cairo))
 (define turtle-pi 3.141592653589793)

 (define turtle-create-default
   (lambda (context)
     (list (list 0.0 0.0) (list 0.0 1.0) #f context)))

 (define turtle-create
   (lambda (position-x position-y heading-x heading-y pen-state context)
     (list (list (inexact position-x) (inexact position-y))
	   (list (inexact heading-x) (inexact heading-y))
	   pen-state
	   context)))

 ;;getter definitions
 (define turtle-get-position-x
   (lambda (turtle)
     (car (car turtle))))
 
 (define turtle-get-position-y
   (lambda (turtle)
     (car (cdr (car turtle)))))
 
 (define turtle-get-heading-x
   (lambda (turtle)
     (car (car (cdr turtle)))))
 
 (define turtle-get-heading-y
   (lambda (turtle)
     (car (cdr (car (cdr turtle))))))

 (define turtle-get-penstate
   (lambda (turtle)
     (car (cdr (cdr turtle)))))

 (define turtle-get-context
   (lambda (turtle)
     (car (cdr (cdr (cdr turtle))))))

(define turtle-get-position
  (lambda (turtle)
	(car turtle)))

(define turtle-get-heading
  (lambda (turtle)
	(car (cdr turtle))))

 ;;Setter definitions
(define turtle-set-position-x
   (lambda (turtle position-x)
     (turtle-create (inexact position-x)
		    (turtle-get-position-y turtle)
		    (turtle-get-heading-x turtle)
		    (turtle-get-heading-y turtle)
		    (turtle-get-penstate turtle)
		    (turtle-get-context turtle))))
		    
 
 (define turtle-set-position-y
   (lambda (turtle position-y)
     (turtle-create (turtle-get-position-x turtle)
		    (inexact position-y)
		    (turtle-get-heading-x turtle)
		    (turtle-get-heading-y turtle)
		    (turtle-get-penstate turtle)
		    (turtle-get-context turtle))))
 
 (define turtle-set-heading-x
   (lambda (turtle heading-x)
     (turtle-create (turtle-get-position-x turtle)
		    (turtle-get-position-y turtle)
		    (inexact heading-x)
		    (turtle-get-heading-y turtle)
		    (turtle-get-penstate turtle)
		    (turtle-get-context turtle))))
 
 (define turtle-set-heading-y
   (lambda (turtle heading-y)
     (turtle-create (turtle-get-position-x turtle)
		    (turtle-get-position-y turtle)
		    (turtle-get-heading-x turtle)
		    (inexact heading-y)
		    (turtle-get-penstate turtle)
		    (turtle-get-context turtle))))
 
 (define turtle-set-penstate
   (lambda (turtle penstate)
     (turtle-create (turtle-get-position-x turtle)
		    (turtle-get-position-y turtle)
		    (turtle-get-heading-x turtle)
		    (turtle-get-heading-y turtle)
		    penstate
		    (turtle-get-context turtle))))

(define turtle-set-context
  (lambda (turtle context)
    (turtle-create (turtle-get-position-x turtle)
		   (turtle-get-position-y turtle)
		   (turtle-get-heading-x turtle)
		   (turtle-get-heading-y turtle)
		   (turtle-get-penstate turtle)
		   context)))

;;Mutators
(define turtle-forward
  (lambda (turtle distance)
    (turtle-move turtle distance +)))
		
(define turtle-back
  (lambda (turtle distance)
    (turtle-move turtle distance -)))

(define turtle-move
  (lambda (turtle distance direction)
    (let ((new-position-x (direction
			   (turtle-get-position-x turtle)
			   (inexact (* (turtle-get-heading-x turtle)
					 distance))))
	  (new-position-y (direction
			   (turtle-get-position-y turtle)
			   (inexact (* (turtle-get-heading-y turtle)
				       distance)))))
      (cond ((turtle-get-penstate turtle)
	     (cairo_line_to (turtle-get-context turtle)
			    new-position-x
			    new-position-y))
	    (else
	     (cairo_move_to (turtle-get-context turtle)
			    new-position-x
			    new-position-y)))
      (turtle-set-position-x
       (turtle-set-position-y turtle new-position-y)
       new-position-x))))

(define turtle-left
  (lambda (turtle angle)
    (turtle-turn turtle angle -)))
    
(define turtle-right
  (lambda (turtle angle)
    (turtle-turn turtle angle +)))

(define turtle-turn
  (lambda (turtle angle direction)
    (let ((radian (* angle (/ turtle-pi 180))))
      (turtle-create (turtle-get-position-x turtle)
		     (turtle-get-position-y turtle)
		     (direction
		      (* (cos radian) (turtle-get-heading-x turtle))
		      (* (sin radian) (turtle-get-heading-y turtle)))
		     (direction
		      (* (- (sin radian)) (turtle-get-heading-x turtle))
		      (* (cos radian) (turtle-get-heading-y turtle)))
		     (turtle-get-penstate turtle)
		     (turtle-get-context turtle)))))

)