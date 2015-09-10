#lang eopl

(require test-engine/racket-tests)

(check-expect (merge '(1 4) '(1 2 8)) '(1 1 2 4 8))
(check-expect (merge '(35 62 81 90 91) '(3 83 85 90)) 
              '(3 35 62 81 83 85 90 90 91))
(check-expect (merge '() '(1 2 3)) '(1 2 3))
(check-expect (merge '(1 2 3) '()) '(1 2 3))

;; merge : Listof(Int) * Listof(Int) -> Listof(Int)
;; usage : (merge '(1 4) '(2 3)) = '(1 2 3 4)
(define merge
  (lambda (lst1 lst2)
    (cond
      [(null? lst1) lst2]
      [(null? lst2) lst1]
      [(< (car lst1) (car lst2)) (cons (car lst1)
                                       (merge (cdr lst1) lst2))]
      [else (cons (car lst2) (merge lst1 (cdr lst2)))])))

(test)