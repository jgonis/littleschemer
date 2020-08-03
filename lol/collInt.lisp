(defparameter *critics* '(("Lisa Rose" . (("Lady in the Water" . 2.5) ("Snakes on a Plane" . 3.5) ("Just My Luck" . 3.0) ("Superman Returns" . 3.5) ("You, Me and Dupree" . 2.5) ("The Night Listener" . 3.0))) ("Gene Seymour" . (("Lady in the Water" . 3.0) ("Snakes on a Plane" . 3.5) ("Just My Luck" . 1.5) ("Superman Returns" . 5.0) ("The Night Listener" . 3.0) ("You, Me and Dupree" . 3.5))) ("Michael Phillips" . (("Lady in the Water" . 2.5) ("Snakes on a Plane" . 3.0)  ("Superman Returns" . 3.5) ("The Night Listener" . 4.0))) ("Claudia Puig" . (("Snakes on a Plane" . 3.5) ("Just My Luck" . 3.0) ("Superman Returns" . 4.0) ("You, Me and Dupree" . 2.5) ("The Night Listener" . 4.5))) ("Mick LaSalle" . (("Lady in the Water" . 3.0) ("Snakes on a Plane" . 4.0) ("Just My Luck" . 2.0) ("Superman Returns" . 3.0) ("You, Me and Dupree" . 2.0) ("The Night Listener" . 3.0))) ("Jack Matthews" . (("Lady in the Water" . 3.0) ("Snakes on a Plane" . 4.0) ("Superman Returns" . 5.0) ("You, Me and Dupree" . 3.5) ("The Night Listener" . 3.0))) ("Toby" . (("Snakes on a Plane" . 4.5) ("Superman Returns" . 4.0) ("You, Me and Dupree" . 1.0)))))

(defun get-movie-score (dbase critic-name movie-name)
  (let ((critics-reviews (cdr (assoc critic-name dbase :test #'equal))))
	(cdr (assoc movie-name critics-reviews :test #'equal))))

(defun set-critic-score (dbase critic-name movie-name movie-score)
  (let ((critics-reviews (cdr (assoc critic-name dbase :test #'equal))))
	
							 

(defun get-all-critics-scores (dbase critic-name)
  (cdr (assoc critic-name dbase :test #'equal)))

(defun get-all-movie-scores (dbase movie-name)
)