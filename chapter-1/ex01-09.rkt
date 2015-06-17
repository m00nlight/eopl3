#lang eopl

(require test-engine/racket-tests)

;; remove : Sym * Listof(Sym) -> Listof(Sym)
;; usage: (remove s lst) = remove all element in list equal to s
(define remove
  (lambda (s lst)
    (if (null? lst)
        '()
        (if (eqv? s (car lst))
            (remove s (cdr lst))
            (cons (car lst) (remove s (cdr lst)))))))


(check-expect (remove 'a '(b c d)) '(b c d))
(check-expect (remove 'a '(b c a d e)) '(b c d e))
(check-expect (remove 'a '(a b c a b d)) '(b c b d))

(test)

