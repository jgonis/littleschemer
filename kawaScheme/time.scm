(module-export time)

(define time
  (lambda (functionToExecute)
    (let ((startTime (java.lang.System:nanoTime)))
      (functionToExecute)
      (exact->inexact (/ (- (java.lang.System:nanoTime) startTime)
                         1000000000)))))
