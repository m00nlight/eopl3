#lang eopl

(require test-engine/racket-tests)

;; nth-element : List * Int -> SchemeVal
;; usage: (nth-element lst) = then n-th element of lst
(define nth-element
  (lambda (lst n)
    (letrec [(help (lambda (ls nn)
                     (if (null? ls)
                         (report-list-too-short lst n)
                         (if (zero? nn)
                             (car ls)
                             (help (cdr ls) (- nn 1))))))]
      (help lst n))))

(define report-list-too-short
  (lambda (lst n)
    (eopl:error 'nth-element
                "~s does not have ~s elements ~%" lst n)))


(check-expect (nth-element '(1 2 3) 1) '2)
(check-error (nth-element '(1 2 3) 3))

(test)