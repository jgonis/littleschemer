(load "ch7.lisp")
(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

(defun random-node ()
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eql a b)
	(list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num* collect 
					   (edge-pair (random-node) (random-node)))))

(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
				   (eql (car x) node))
				 edge-list))

(defun alt-make-edge-list (edge-list)
	(cond ((>= (length edge-list) 90) edge-list)
		  (t (alt-make-edge-list (append edge-list
										 (edge-pair (random-node) 
													(random-node)))))))
(defun compare-pair (pair1 pair2)
  (and (equal (car pair1) (car pair2)) 
	   (equal (cdr pair1) (cdr pair2))))

(defun contains-duplicate (pair list-of-pairs)
  (cond ((null list-of-pairs) nil)
		((compare-pair pair (car list-of-pairs)) (progn (princ pair) t))
		(t (contains-duplicate pair (cdr list-of-pairs)))))

(defun check-list-for-duplicates (list-of-pairs)
  (cond ((null list-of-pairs) nil)
		(t (or (contains-duplicate (car list-of-pairs) 
									(cdr list-of-pairs))
				(check-list-for-duplicates (cdr list-of-pairs))))))

(defun remove-duplicate-pairs (pair list-of-pairs)
  (cond ((null list-of-pairs) '())
		((equal pair (car list-of-pairs)) 
		 (remove-duplicate-pairs pair (cdr list-of-pairs)))
		(t (cons (car list-of-pairs) 
				 (remove-duplicate-pairs pair (cdr list-of-pairs))))))

(defun create-unique-list-of-pairs (list-of-pairs)
  (cond ((null list-of-pairs) '())
		(t (cons (car list-of-pairs)
				 (create-unique-list-of-pairs 
				  (cdr (remove-duplicate-pairs
						(car list-of-pairs)
						(cdr list-of-pairs))))))))


		