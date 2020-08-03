(define row-size 4)
(define column-size 4)
(define value-limit 4)

(define 4x4test '((0 0 0) (0 1 0) (0 2 1) (0 3 0) (1 0 0) (1 1 3) (1 2 0) (1 3 4) (2 0 3) (2 1 0) (2 2 4) (2 3 0) (3 0 0) (3 1 2) (3 2 0) (3 3 0)))

(define get-row
  (lambda (sudoku-cell)
	(car sudoku-cell)))

(define get-column
  (lambda (sudoku-cell)
	(car (cdr sudoku-cell))))

(define get-value
  (lambda (sudoku-cell)
	(car (cdr (cdr sudoku-cell)))))

(define make-sudoku-cell
  (lambda (row column value)
	(assert (and (> value 0) (< value value-limit)))
	(assert (and (>= row 0) (< row row-size)))
	(assert (and (>= column 0) (< column column-size)))
	(list row column value)))

(define get-sudoku-board-size
  (lambda (sudoku-board)
	(car sudoku-board)))

(define make-sudoku-board
  (lambda (board-size)

(define get-sudoku-cell
  (lambda (sudoku-board)
	

(define check-constraints
  