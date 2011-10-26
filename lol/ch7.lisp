;;creating graphviz graphs from edge/node descriptions
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

(defparameter *max-label-length* 30)


(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defun dot-label (exp)
  (if exp
	  (let ((s (write-to-string exp :pretty nil)))
		(if (> (length s) *max-label-length*)
			(concatenate 'string (subseq s 0 (- *max-label-length* 3))
						 "...")
			s)) 
	  ""))

(defun nodes->dot (nodes)
  (mapc (lambda (node)
		  (fresh-line)
		  (princ (dot-name (car node)))
		  (princ "[label=\"")
		  (princ (dot-label node))
		  (princ "\"];"))
		nodes))

(defun edges->dot (edges)
  (mapc (lambda (node)
		  (mapc (lambda (edge)
				  (fresh-line)
				  (princ (dot-name (car node)))
				  (princ "->")
				  (princ (dot-name (car edge)))
				  (princ "[label=\"")
				  (princ (dot-label (cdr edge)))
				  (princ "\"];"))
				(cdr node)))
		edges))

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
				   fname
				   :direction :output
				   :if-exists :supersede)
	(funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname)))

(defun graph->png (fname nodes edges)
  (dot->png fname
			(lambda ()
			  (graph->dot nodes edges))))

(defun uedges->dot (edges)
  (maplist (lambda (lst)
			 (mapc (lambda (edge)
					 (unless (assoc (car edge) (cdr lst))
					   (fresh-line)
					   (princ (dot-name (caar lst)))
					   (princ "--")
					   (princ (dot-name (car edge)))
					   (princ "[label=\"")
					   (princ (dot-label (cdr edge)))
					   (princ "\"];")))
				   (cdar lst)))
		   edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
			(lambda ()
			  (ugraph->dot nodes edges))))