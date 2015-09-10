#lang eopl

(require test-engine/racket-tests)

(check-expect (swapper 'a 'd '(a b c d)) '(d b c d))
(check-expect (swapper 'a 'd '()) '())
(check-expect (swapper 'a 'd '(a a a)) '(d d d))

;; swapper : Symb * Symb * Listof(Symb) -> Listof(Symb)
;; usage : (swapper 'a 'd '(a b c d)) = '(d b c d)
(define swapper
  (lambda (s1 s2 lst)
    (cond 
      [(null? lst) '()]
      [(equal? s1 (car lst)) (cons s2 (swapper s1 s2 (cdr lst)))]
      [else (cons (car lst) (swapper s1 s2 (cdr lst)))])))


(test)