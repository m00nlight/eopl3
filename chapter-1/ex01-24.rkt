#lang eopl

(require test-engine/racket-tests)

(check-expect (every? number? '(a b c 3 e)) #f)
(check-expect (every? number? '(1 2 3 4 5)) #t)
(check-expect (every? number? '()) #t)

;; every? : (Symb -> Bool) * Listof(Symb) -> Bool
;; usage : (every? number? '(1 2 3 4 5)) = #t

(define every?
  (lambda (pred? lst)
    (cond
      [(null? lst) #t]
      [else (and (pred? (car lst))
                 (every? pred? (cdr lst)))])))

(test)