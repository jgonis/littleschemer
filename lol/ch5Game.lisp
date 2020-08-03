(defparameter *nodes* '((living-room (You are in the living-room.  
									  A Wizard is snoring loudly on the 
									  couch.))
						(garden (You are in a beautiful garden.  
								 There is a well in front of you.))
						(attic (You are in the attic.  There is a giant 
								welding torch in the corner.))))

(defparameter *edges* '((living-room (garden west door)
						 (attic upstairs ladder))
						(garden (living-room east door))
						(attic (living-room downstairs ladder))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
								   (bucket living-room)
								   (chain garden)
								   (frog garden)))

(defparameter *location* 'living-room)
 
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
			 (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
			 `(You see a ,obj on the floor.)))
	(apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defun look ()
  (append (describe-location *location* *nodes*)
		  (describe-paths *location* *edges*)
		  (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction 
					(cdr (assoc *location* *edges*)) :key #'cadr)))
	(if next (progn (setf *location* (car next))
					(look))
		'(you cannot go that way.))))

(defun pickup (object)
  (cond ((member object
				 (objects-at *location* *objects* *object-locations*))
		 (push (list object 'body) *object-locations*)
		 `(You are now carrying the ,object))
		(t '(You cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun say-hello ()
  (princ "Please type your name: ")
  (let ((name (read-line)))
	(princ "Nice to meet you, ")
	(princ name)))

(defun game-repl ()
  (let ((cmd (game-read)))
	(unless (eq (car cmd) 'quit)
	  (game-print (game-eval cmd))
	  (game-repl))))

;;Read in commands from the user and then append "quotes" to the arguments
;;after the first element which is the command to execute.  In this way we
;;can take input like walk east and transform it to (walk 'east) so that
;;it can be properly interpreted by our functions.
(defun game-read ()
  (let ((cmd (read-from-string
			  (concatenate 'string "(" (read-line) ")"))))
	(flet ((quote-it (x)
			 (list 'quote x)))
	  (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

;;Check the commands that we have been passed by the player against the
;;list of allowed commands that we know are ok to execute.
(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
	  (eval sexp)
	  '(I do not know that command)))

;;Game print function
(defun tweak-text (list caps lit)
  (when list
	(let ((item (car list))
		  (rest (cdr list)))
	  (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
			((member item '(#\! #\? #\.)) (cons item (tweak-text rest
																 t
																 lit)))
			((eq item #\") (tweak-text rest caps (not lit)))
			(lit (cons item (tweak-text rest nil lit)))
			((or caps lit) (cons (char-upcase item) (tweak-text rest 
																nil
																lit)))
			(t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (list)
  (princ (coerce (tweak-text (coerce (string-trim "() "
												  (prin1-to-string list))
									 'list)
							 t
							 nil)
				 'string))
  (fresh-line))


;;Chapter 7 Commands
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))