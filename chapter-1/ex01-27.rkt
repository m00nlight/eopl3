#lang eopl

(require test-engine/racket-tests)

(check-expect (flatten '(a b c)) '(a b c))
(check-expect (flatten '((a) () (b ()) () (c))) '(a b c))
(check-expect (flatten '((a b) c (((d)) e))) '(a b c d e))
(check-expect (flatten '(a b () (c))) '(a b c))

(define flatten
  (lambda (slist)
    (cond
      [(null? slist) '()]
      [(symbol? (car slist)) (cons (car slist) (flatten (cdr slist)))]
      [else (append (flatten (car slist)) (flatten (cdr slist)))])))

(test)