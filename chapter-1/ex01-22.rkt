#lang eopl

(require test-engine/racket-tests)

(check-expect (filter-in number? '(a 2 (1 3) b 7)) '(2 7))
(check-expect (filter-in symbol? '(a 2 (1 3) b 7)) '(a b))
(check-expect (filter-in symbol? '(a (b c) 17 foo)) '(a foo))

;; filter-in : (Symb -> Bool) * Listof(Symb) -> Listof(Symb)
;; usage : (filter-in number? '(a 2 (1 3) b 7)) = '(2 7)
;;         (filter-in symbol? '(a 2 (1 3) b 7)) = '(a b)

(define filter-in
  (lambda (pred? lst)
    (cond
      [(null? lst) '()]
      [(pred? (car lst)) (cons (car lst) (filter-in pred? (cdr lst)))]
      [else (filter-in pred? (cdr lst))])))

(test)