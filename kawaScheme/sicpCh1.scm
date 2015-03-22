(define-library (sicp timing)
  (export time)
  (import (scheme base))
  (begin 
    (define time
      (lambda (functionToExecute)
        (let ((startTime (java.lang.System:nanoTime)))
          (functionToExecute)
          (inexact (/ (- (java.lang.System:nanoTime) startTime)
                             1000000000)))))))  


(define-library (sicp ch1)
  (import (scheme base)
          (scheme file)
          (scheme inexact)
          (scheme write)
          (sicp timing))
  (export)
  (begin))

(import (sicp ch1)
        (sicp timing))
