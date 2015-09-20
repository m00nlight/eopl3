#lang eopl

(require test-engine/racket-tests)

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

;; bintree->list : bintree -> Listof(Any)
(define bintree->list
  (lambda (bt)
    (cases bintree bt
      [leaf-node (num) `(leaf-node ,num)]
      [interior-node (key left right)
                     `(interior-node
                       ,key
                       ,(bintree->list left)
                       ,(bintree->list right))])))

(check-expect (bintree->list (interior-node 'a (leaf-node 3) (leaf-node 4)))
              '(interior-node a (leaf-node 3) (leaf-node 4)))

(test)