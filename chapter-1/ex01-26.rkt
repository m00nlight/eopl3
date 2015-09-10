#lang eopl

(require test-engine/racket-tests)

(check-expect (up '((1 2) (3 4))) '(1 2 3 4))
(check-expect (up '((x (y)) z)) '(x (y) z))

;; up : Listof(Symbol) -> Listof(Symbol)
;; usage : (up '((1 2) (3 4))) = '(1 2 3 4)

(define up
  (lambda (lst)
    (cond
      [(null? lst) '()]
      [(symbol? (car lst)) (cons (car lst) (up (cdr lst)))]
      [else (append (car lst) (up (cdr lst)))])))

(test)