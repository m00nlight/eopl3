#lang eopl

(require test-engine/racket-tests)



(check-expect (duple 2 3) '(3 3))
(check-expect (duple 0 'a) '())
(check-expect (duple 1 'a) '(a))

;; duple : Int * Symb -> Listof(Symb)
;; usage : (duple 2 'a)
(define duple
  (lambda (n sym)
    (if (= n 0)
        '()
        (cons sym (duple (- n 1) sym)))))

(test)