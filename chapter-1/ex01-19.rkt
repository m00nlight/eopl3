#lang eopl

(require test-engine/racket-tests)

(check-expect (list-set '(a b c d) 2 '(1 2)) '(a b (1 2) d))
(check-expect (list-set '() 2 '(1 2)) '())
(check-expect (list-set '(1) 1 '(1 2)) '(1))

;; list-set : Listof(Symbol) * Int * Symbol -> Listof(Symbol)
(define list-set
  (lambda (lst n new)
    (cond
      [(null? lst) '()]
      [(= n 0) (cons new (cdr lst))]
      [else (cons (car lst) (list-set (cdr lst) (- n 1) new))])))


(test)