#lang eopl

(require test-engine/racket-tests)

(check-expect (exists? number? '(a b c 3 e)) #t)
(check-expect (exists? number? '(1 2 3 4 5)) #t)
(check-expect (exists? number? '()) #f)

;; exists? : (Symb -> Bool) * Listof(Symb) -> Bool
;; usage : (exists? number? '(1 2 3 4 5)) = #t

(define exists?
  (lambda (pred? lst)
    (cond
      [(null? lst) #f]
      [else (or (pred? (car lst))
                 (exists? pred? (cdr lst)))])))

(test)