#lang eopl

(require test-engine/racket-tests)

(check-expect (path 17 '(14 (7 () (12 () ()))(26 (20 (17 () ())())(31 () ()))))
              '(right left left))

;; path : Int * BTree(Int) -> Listof(Symbol)
(define path
  (lambda (target bstree)
    (cond
      [(= target (car bstree)) '()]
      [(< target (car bstree)) (cons 'left (path target (cadr bstree)))]
      [else (cons 'right (path target (caddr bstree)))])))

(test)
    