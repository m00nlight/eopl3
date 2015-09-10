#lang eopl

(require test-engine/racket-tests)

(check-expect (take '(1 2 3) 2) '(1 2))
(check-expect (take '(1 2 3) 4) '(1 2 3))
(check-expect (drop '(1 2 3) 2) '(3))
(check-expect (drop '(1 2 3) 4) '())
(check-expect (sort/predicate < '(8 2 5 2 3)) '(2 2 3 5 8))
(check-expect (sort/predicate > '(8 2 5 2 3)) '(8 5 3 2 2))

;; merge : Listof(Int) * Listof(Int) -> Listof(Int)
;; usage : (merge '(1 4) '(2 3)) = '(1 2 3 4)
(define merge
  (lambda (pred? lst1 lst2)
    (cond
      [(null? lst1) lst2]
      [(null? lst2) lst1]
      [(pred? (car lst1) (car lst2)) 
       (cons (car lst1) (merge pred? (cdr lst1) lst2))]
      [else (cons (car lst2) (merge pred? lst1 (cdr lst2)))])))


;; sort : Listof(Int) -> Listof(Int)
;; usage : (sort '(8 2 5 2 3)) = '(2 2 3 5 8)
(define sort/predicate
  (lambda (pred? lst)
    (if (<= (length lst) 1)
        lst
        (let* [(n (length lst))
               (part1 (take lst (quotient n 2)))
               (part2 (drop lst (quotient n 2)))]
          (merge pred? (sort/predicate pred? part1) 
                 (sort/predicate pred? part2))))))


;; take : Listof(Int) * Int -> Listof(Int)
;; usage : (take '(1 2 3) 2) = '(1 2)
(define take
  (lambda (lst n)
    (cond
      [(null? lst) '()]
      [(= n 0) '()]
      [else (cons (car lst) (take (cdr lst) (- n 1)))])))

;; drop : Listof(Int) * Int -> Listof(Int)
(define drop
  (lambda (lst n)
    (cond
      [(null? lst) '()]
      [(= n 0) lst]
      [else (drop (cdr lst) (- n 1))])))

(test)