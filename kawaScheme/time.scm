(module-export time)
;Getting kawa running on my emacs on windows, set this as the
;scheme program name: java -jar "d://PathTo//KawaJar//kawa-1.14.jar\" -s


(define time
  (lambda (functionToExecute)
    (let ((startTime (java.lang.System:nanoTime)))
      (functionToExecute)
      (exact->inexact (/ (- (java.lang.System:nanoTime) startTime)
                         1000000000)))))
