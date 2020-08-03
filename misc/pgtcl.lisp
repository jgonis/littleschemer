;;Ch 1
;;Filtering lists
(defun filter (list-of-numbers)
	(if (null list-of-numbers) nil
		(if (< (car list-of-numbers) 0)
			(filter (cdr list-of-numbers))
			(cons (car list-of-numbers)
				  (filter (cdr list-of-numbers))))))

;;Pumping bike pedals
(defun pump-pedals (bike)
  (set-x-position
   bike
   (+ (x-position bike)
	  (* (gear-ratio bike)
		 0.5 pi (wheel-size bike)))))

(defun make-bike (x-position gear-ratio wheel-size)
  (list x-position gear-ratio wheel-size))

(defun x-position (bike)
  (car bike))

(defun gear-ratio (bike)
  (cadr bike))

(defun wheel-size (bike)
  (caddr bike))

(defun set-x-position (bike new-value)
  (setf (car bike) new-value))

(defun set-gear-ratio (bike new-value)
  (setf (cadr bike) new-value))

(defun set-wheel-size (bike new-value)
  (setf (caddr bike) new-value))

;;Making Gumball Machines
(defvar *gum-colors* 
  '(brown blue red orange purple green yellow speckled))

(defun generate-gum-supply (size)
  (if (= 0 size) nil
      (cons (nth (random (length *gum-colors*)) *gum-colors*)
	    (generate-gum-supply (- size 1)))))

(defun gum-machine (supply-of-gum)
  (function 
   (lambda ()
    (prog1 (car supply-of-gum)
      (setq supply-of-gum (cdr supply-of-gum))))))

(defvar *barber-shop-machine*
  (gum-machine (generate-gum-supply 4)))

(defvar *grocery-store-machine
  (gum-machine (generate-gum-supply 6)))

(defun get-gum (machine)
  (funcall machine))

(defun nil-test (list)
  (cond ((null list) 'done)
	(t (prog1 (print (car list)) 
	     (nil-test (cdr list))))))

