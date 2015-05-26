#lang eopl

;; in-S? : N -> Bool
;; usage: (in-S? n) = t if n is in S, #f otherwise

(define in-S?
  (lambda (n)
    (if (zero? n)
        #t
        (if (>= (- n 3) 0)
            (in-S? (- n 3))
            #f))))


