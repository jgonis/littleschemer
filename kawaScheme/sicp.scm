(define-library (jeffTest)
  (export testFunc)
  (import (scheme base))
  (begin 
    (define (testFunc n)
      (* n 2))))
