(include "timing.scm")
(define-library (sicp ch1)
  (export square)
  (import (except (scheme base) square)
          (scheme char)
          (scheme complex)
          (scheme inexact)
          (scheme file))
  (begin
    (define (square n)
      (* n n))))

(import (sicp ch1)
        (sicp timing))
