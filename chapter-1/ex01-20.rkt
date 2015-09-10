#lang eopl

(require test-engine/racket-tests)

(check-expect (count-occurance 'x '((f x) y (((x z) x)))) 3)
(check-expect (count-occurance 'x '((f x) y (((x z) () x)))) 3)
(check-expect (count-occurance 'w '((f x) y (((x z) x)))) 0)

;; count-occurance : Symb * Listof(Symb) -> Int
;; usage : (count-occurane 'x '((f x) y (((x z) x)))) = 3
(define count-occurance
  (lambda (symb lst)
    (cond
      [(null? lst) 0]
      [(symbol? (car lst)) (+ (if (equal? (car lst) symb) 1 0)
                              (count-occurance symb (cdr lst)))]
      [else (+ (count-occurance symb (car lst))
               (count-occurance symb (cdr lst)))])))


(test)