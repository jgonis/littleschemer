;;This is some initial playing around with GLUT as I prepare to read
;;through the book "Turtle Geometry". I decided to use GLUT because
;;I wanted to be able to work in windows as well as Linux, and I didn't
;;want to compile the version of ypsilon from svn, which includes GTK+
;;bindings.
(import (ypsilon gl))
(import (ypsilon glut))
(import (ypsilon time))
(import (ypsilon glu))
(import (srfi srfi-27))
(import (xitomatl profiler srfi-time))

(define screen-width 900)
(define screen-height 900)
(define rect-height 5)
(define rect-width 5)
(define refresh-delay 16)
(define gameboard-columns (/ screen-width rect-height))
(define gameboard-rows (/ screen-height rect-width))

(define make-game-cell
  (lambda (state x y neighbor-count)
	(vector state x y neighbor-count)))

(define get-game-cell-state
  (lambda (game-cell)
	(vector-ref game-cell 0)))

(define get-game-cell-row
  (lambda (game-cell)
	(vector-ref game-cell 1)))

(define get-game-cell-column
  (lambda (game-cell)
	(vector-ref game-cell 2)))

(define get-game-cell-neighbor-count
  (lambda (game-cell)
	(vector-ref game-cell 3)))

(define set-game-cell-state
  (lambda (game-cell state)
	(vector-set! game-cell 0 state)))

(define set-game-cell-row
  (lambda (game-cell row)
	(vector-set! game-cell 1 row)))

(define set-game-cell-column
  (lambda (game-cell column)
	(vector-set! game-cell 2 column))) 

(define inc-game-cell-neighbor-count
  (lambda (game-cell)
	(vector-set! game-cell 3 
				 (+ (get-game-cell-neighbor-count game-cell) 1))))

(define dec-game-cell-neighbor-count
  (lambda (game-cell)
	(vector-set! game-cell 3 
				 (- (get-game-cell-neighbor-count game-cell) 1))))

(define make-gameboard
  (lambda (rows columns)
	(do ((vec (make-vector (* rows columns) 0))
		 (i 0 (+ i 1)))
		((= i (* rows columns)) vec)
	  (vector-set! vec i (make-game-cell 0 
										 (div i columns) 
										 (mod i columns)
										 0)))))

(define initialize-gameboard
  (lambda (gameboard)
    (set-gameboard-cell gameboard 30 30 1)
	(set-gameboard-cell gameboard 29 30 1)
	(set-gameboard-cell gameboard 29 29 1)
	(set-gameboard-cell gameboard 30 34 1)
	(set-gameboard-cell gameboard 30 35 1)
	(set-gameboard-cell gameboard 30 36 1)
	(set-gameboard-cell gameboard 28 35 1)))

(define update-gameboard
  (lambda (gameboard)
	(let* ((gb-rows (vector-length gameboard))
		   (gb-columns (vector-length (vector-ref gameboard 0)))
		   (new-gameboard (make-gameboard gb-rows gb-columns))
		   (row 0)
		   (column 0))
	  (letrec ((traverse-row
				(lambda (old-gameboard new-gameboard row column)
				  (cond ((= row gameboard-rows) new-gameboard)
						(else
						 (begin
						   (update-cell old-gameboard new-gameboard
										row column)
						   (cond ((= column (- gameboard-columns 1))
								  (traverse-row old-gameboard new-gameboard
												(+ row 1) 0))
								 (else (traverse-row old-gameboard
													 new-gameboard
													 row (+ column 1))))))))))
			  (begin
				(time (traverse-row gameboard new-gameboard row column))
				(traverse-row gameboard new-gameboard row column))))))

(define get-gameboard-cell
  (lambda (gameboard row column)
	(vector-ref (vector-ref gameboard row) column)))

(define set-gameboard-cell
  (lambda (gameboard row column value)
	(set-game-cell-state (vector-ref gameboard 
									 (+ (* row gameboard-columns) column))
						 value)
	(cond ((= value 0) ;;decrement neighboring cell's neighbor counts
		   )
		  ((= value 1) ;;increment neighboring cell's neighbor counts
		   ))))
									 
(define update-cell
  (lambda (old-gameboard new-gameboard row column)
	(let ((live-count (count-neighbors old-gameboard row column)))
	  (cond
	   ;;Rule: if the cell has fewer than 2 live neighbors it dies
	   ((< live-count 2)
		(set-gameboard-cell new-gameboard row column 0))
	   ;;Rule: if the cell has more than 3 live neighbors it dies
	   ((> live-count 3)
		(set-gameboard-cell new-gameboard row column 0))
	   ;;Rule: if the cell is dead and has 3 live neighbors it becomes alive
	   ((and (= live-count 3)
			 (= (get-gameboard-cell old-gameboard row column) 0))
		(set-gameboard-cell new-gameboard row column 1))
	   ;;The cell retains its original value otherwise
	   (else (set-gameboard-cell new-gameboard row column
								 (get-gameboard-cell old-gameboard
													 row
													 column)))))))
(define count-neighbors
  (lambda (old-gameboard row column)
	;;Check for three live neighbors and no more
	(+
	 ;;Cell diagonally up to the left of the current cell
	 (get-gameboard-cell old-gameboard
						 (modulo (- row 1) gameboard-rows)
						 (modulo (- column 1) gameboard-columns))
	 ;;Cell directly above the current cell
	 (get-gameboard-cell old-gameboard
						 (modulo (- row 1) gameboard-rows)
						 column)
	 ;;Cell diagonally up to the right of the current cell
	 (get-gameboard-cell old-gameboard
						 (modulo (- row 1) gameboard-rows)
						 (modulo (+ column 1) gameboard-columns))
	 ;;Cell to the left of the current cell
	 (get-gameboard-cell old-gameboard
						 row
						 (modulo (- column 1) gameboard-columns))
	 ;;Cell to the right of the current cell
	 (get-gameboard-cell old-gameboard
						 row
						 (modulo (+ column 1) gameboard-columns))
	 ;;Cell diagonally down to the left of the current cell
	 (get-gameboard-cell old-gameboard
						 (modulo (+ row 1) gameboard-rows)
						 (modulo (- column 1) gameboard-columns))
	 ;;Cell directly below the current cell
	 (get-gameboard-cell old-gameboard
						 (modulo (+ row 1) gameboard-rows)
						 column)
	 ;;Cell diagonally down to the right of the current cell
	 (get-gameboard-cell old-gameboard
						 (modulo (+ row 1) gameboard-rows)
						 (modulo (+ column 1) gameboard-columns)))))

(define display-func
  (lambda ()
	(glClear (+ GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
	(glColor3f 1.0 0.0 0.0)
	;;update gameboard
	(set! gameboard (update-gameboard gameboard))
	(glFlush)
	(glutSwapBuffers)))

(define timerFunc
  (lambda (value)
	(glutSwapBuffers)
	(glutPostRedisplay)
	(glutTimerFunc refresh-delay timerFunc 0)))


(define run-prog
  (lambda ()
	(glutInit (vector (length (command-line))) (apply vector (command-line)))
	(glutInitDisplayMode (+ GLUT_DEPTH GLUT_DOUBLE GLUT_RGBA))
	(glutInitWindowPosition 100 100)
	(glutInitWindowSize screen-width screen-height)
	(glutCreateWindow "Game of Life")
	(glClearColor 0.0 0.0 0.0 0.0)
	(glLineWidth 3.0)
	;;The following lines of code implement alpha blending and anti-aliasing
	;;of any lines that I draw.
	(glEnable GL_BLEND)
	(glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
	(glHint GL_LINE_SMOOTH_HINT GL_FASTEST)
	(glEnable GL_LINE_SMOOTH)
	(gluOrtho2D 0 screen-width screen-height 0)
	(glutTimerFunc refresh-delay timerFunc 0)
	(glutDisplayFunc display-func)
	(initialize-gameboard gameboard)
	(glutMainLoop)))

(define gameboard (make-gameboard gameboard-columns gameboard-rows))

(run-prog)
	
