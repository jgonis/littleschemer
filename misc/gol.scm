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

(define screen-width 900)
(define screen-height 900)
(define rect-height 5)
(define rect-width 5)
(define refresh-delay 16)
(define gameboard-columns (/ screen-width rect-height))
(define gameboard-rows (/ screen-height rect-width))

(define make-game-cell
  (lambda (state neighbor-count)
	(vector state neighbor-count)))

(define get-game-cell-state
  (lambda (game-cell)
	(vector-ref game-cell 0)))

(define set-game-cell-state
  (lambda (game-cell state)
	(vector-set! game-cell 0 state)))

(define get-game-cell-neighbor-count
  (lambda (game-cell)
	(vector-ref game-cell 1)))

;;Alter the game cells neighbor count by a certain amount, either up
;;or down from its previous value.
(define alter-game-cell-neighbor-count
  (lambda (game-cell operation amount)
	(vector-set! game-cell 1 (operation (get-game-cell-neighbor-count
										 game-cell) amount))))

(define inc-game-cell-neighbor-count
  (lambda (game-cell)
	(alter-game-cell-neighbor-count + 1)))

(define dec-game-cell-neighbor-count
  (lambda (game-cell)
	(alter-game-cell-neighbor-count - 1)))

;;The gameboard is stored as one long vector that will be indexed into
;;rows and columns using modulo division.
(define make-gameboard
  (lambda (rows columns)
	(do ((vec (make-vector (* rows columns) 0))
		 (i 0 (+ i 1)))
		((= i (* rows columns)) vec)
	  (vector-set! vec i (make-game-cell 0 0)))))

;;This initializes the live cells at the start of the game.
;;TODO make this read as an argument either from the command line
;;or from a text file.
(define initialize-gameboard
  (lambda (gameboard)
    (set-gameboard-cell gameboard (make-game-cell 1 0) 30 30)
	(set-gameboard-cell gameboard (make-game-cell 1 0) 29 30)
	(set-gameboard-cell gameboard (make-game-cell 1 0) 29 29)
	(set-gameboard-cell gameboard (make-game-cell 1 0) 30 34)
	(set-gameboard-cell gameboard (make-game-cell 1 0) 30 35)
	(set-gameboard-cell gameboard (make-game-cell 1 0) 30 36)
	(set-gameboard-cell gameboard (make-game-cell 1 0) 28 35)))

;;This is the most important function in the game, which is responsible
;;for updating the state from one turn to another.  It makes a new copy
;;of the gameboard to do so, to allow for the illusion that all the cells
;;update simultaneously.
(define update-gameboard
  (lambda (gameboard)
	(let* ((new-gameboard (make-gameboard gameboard-rows gameboard-columns))
		   (row 0)
		   (column 0))
	  gameboard)))

;;Get the value of the cell located at (row, column).  The vector
;;is just a linear array so we need to figure out the index with
;;some math
(define get-gameboard-cell
  (lambda (gameboard row column)
	(vector-ref gameboard (+ (* row gameboard-columns) column))))

;;Set the cell located at (row, column) to a particular value.
;;The vector is just a linear array so we need to figure out
;;the index with some math
(define set-gameboard-cell
  (lambda (gameboard game-cell row column)
	(vector-set! gameboard (+ (* row gameboard-columns) column) game-cell)))

(define update-cell
  (lambda (old-gameboard new-gameboard row column)
	(let ((live-count (count-neighbors old-gameboard row column)))
	  (cond
	   ;;Rule: if the cell has fewer than 2 live neighbors it dies
	   ((< live-count 2)
		(begin
		  (set-gameboard-cell new-gameboard row column 0)))
	   ;;Rule: if the cell has more than 3 live neighbors it dies
	   ((> live-count 3)
		(begin
		  (set-gameboard-cell new-gameboard row column 0)))
	   ;;Rule: if the cell is dead and has 3 live neighbors it becomes alive
	   ((and (= live-count 3)
			 (= (get-gameboard-cell old-gameboard row column) 0))
		(begin
		  (set-gameboard-cell new-gameboard row column 1)))
	   ;;The cell retains its original value otherwise
	   (else (begin
			   (set-gameboard-cell new-gameboard row column
								   (get-gameboard-cell old-gameboard
													   row
													   column))))))))

(define draw-gameboard
  (lambda (gameboard)
	(letrec ((traverse-board
			  (lambda (row column)
				;;if we are at the end of the row set column to 0 and row
				;;to +1
				(cond ((not (>= row gameboard-rows))
					   (cond ((>= (+ column 1) gameboard-columns)
							  (begin
								(draw-cell
								 (get-game-cell-state
								  (get-gameboard-cell gameboard row column))
								 row column)
								(traverse-board (+ row 1) 0)))
							 (else
							  (begin
								(draw-cell
								 (get-game-cell-state
								  (get-gameboard-cell gameboard row column))
								 row column)
								(traverse-board row (+ column 1))))))))))
	  (traverse-board 0 0))))

(define draw-cell
  (lambda (state row column)
	(cond ((= state 1)
		   (begin
			 (glBegin GL_QUADS)
			 (glVertex2f (* row rect-height)
						 (* column rect-width))
			 (glVertex2f (* row rect-height)
						 (+ (* column rect-width) rect-width))
			 (glVertex2f (+ (* row rect-height) rect-height)
						 (+ (* column rect-width) rect-width))
			 (glVertex2f (+ (* row rect-height) rect-height)
						 (* column rect-width))
			 (glEnd))))))

;;Right now this function doesn't draw anything as the game
;;is still in construction
(define display-func
  (lambda ()
	(glClear (+ GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
	(glColor3f 1.0 0.0 0.0)
	;;update gameboard
	;;(time (update-gameboard gameboard))
	(set! gameboard (update-gameboard gameboard))
	(draw-gameboard gameboard)
	(glFlush)
	(glutSwapBuffers)))

(define timerFunc
  (lambda (value)
	(glutSwapBuffers)
	(glutPostRedisplay)
	(glutTimerFunc refresh-delay timerFunc 0)))


(define run-prog
  (lambda ()
	(display (vector-length gameboard))
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
	;;This sets up the coordinate system for the screen
	(gluOrtho2D 0 screen-width screen-height 0)
	;;This sets the drawing function as well as the function that will
	;;refresh the screen according to the time period I have specified.
	;;My goal is between 30-60 refreshes per second as the board updates.
	(glutTimerFunc refresh-delay timerFunc 0)
	(glutDisplayFunc display-func)
	(initialize-gameboard gameboard)
	(glutMainLoop)))

(define gameboard (make-gameboard gameboard-columns gameboard-rows))

(run-prog)
	
