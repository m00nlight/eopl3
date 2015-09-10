#lang eopl

(require test-engine/racket-tests)


(check-expect (invert '()) '())
(check-expect (invert '((a 1) (a 2) (1 b) (2 b)))
              '((1 a) (2 a) (b 1) (b 2)))

;; invert : Listof(Tuple2) -> Listof(Tuple2)
;; usage : (invert '((a 1) (b 2))) = '((1 a) (2 b))

(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list (cadar lst) (caar lst))
              (invert (cdr lst))))))


(test)