#lang eopl


(require test-engine/racket-tests)

;; ;; remove-before-first : Sym * Listof(Sym) -> Listof(Sym)
;; usage: (remove-before-first s los) return a list with the same elemnt
;;        arranged in the same order as los, except that the element before
;;        the first occurance of symbol s are all removed
(define remove-before-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (remove-before-first s (cdr los))))))



(check-expect (remove-before-first 'a '(b c d e)) '())
(check-expect (remove-before-first 'a '(b c a d e)) '(d e))
(check-expect (remove-before-first 'a '(b c d e a)) '())

(test)