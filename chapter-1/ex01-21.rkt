#lang eopl

(require test-engine/racket-tests)

(check-expect (product '(a b c) '(x y)) '((a x) (a y) (b x) (b y) (c x) (c y)))

;; product : Listof(Symb) * Listof(Symb) -> Listof(Tuple(Symb, Symb))
;; usage : (product '(a b c) '(x y)) = '((a x) (a y) (b x) (b y) (c x) (c y))
(define product
  (lambda (lst1 lst2)
    (if (null? lst1)
        '()
        (append (map (lambda (x) (list (car lst1) x)) lst2)
                (product (cdr lst1) lst2)))))

(test)