#lang eopl

(require test-engine/racket-tests)

(check-expect (list-index number? '(a 2 (1 3) b 7)) 1)
(check-expect (list-index symbol? '(a (b c) 17 foo)) 0)
(check-expect (list-index symbol? '(1 2 (a b) 3)) #f)

;; list-index : (Symbol -> Bool) * Listof(Symbol) -> Int | #f
;; usage : (list-index number? '(a 2 (1 3) b 7)) = 1

(define list-index
  (lambda (pred? lst)
    (list-index-help pred? lst 0)))

(define list-index-help
  (lambda (pred? lst curr)
    (cond
      [(null? lst) #f]
      [(pred? (car lst)) curr]
      [else (list-index-help pred? (cdr lst) (+ curr 1))])))

(test)