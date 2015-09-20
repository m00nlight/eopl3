#lang eopl

(require test-engine/racket-tests)

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))


;; leaf-node? : bintree -> Bool
(define leaf-node?
  (lambda (bt)
    (cases bintree bt
      [leaf-node (num) #t]
      [else #f])))

;; interior-node? : bintree -> Bool
(define interior-node?
  (lambda (bt)
    (cases bintree bt
      [leaf-node (num) #f]
      [else #t])))


;; get-key : bintree -> Symbol
(define get-key
  (lambda (bt)
    (cases bintree bt
      [leaf-node (num) #f]
      [interior-node (key left right) key])))

;; max-interior : bintree -> Symbol
;; descp : Return the symbol associate with an interior node with a maximal
;;         leaf sum
(define max-interior
  (lambda (bt)
    (cases bintree bt
      [leaf-node (num) #f]
      [interior-node (key left right)
                     (let* [(la (tree-sum left))
                            (ra (tree-sum right))
                            (max-child (cond
                                         [(and (interior-node? left)
                                               (interior-node? right))
                                          (if (< la ra)
                                              right
                                              left)]
                                         [(interior-node? left) left]
                                         [(interior-node? right) right]
                                         [else #f]))]
                       (cond
                         [(not max-child) key]
                         [(equal? max-child left)
                          (if (>= ra 0) key (get-key left))]
                         [else (if (>= la 0) key (get-key right))]))])))


(define tree-1
  (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define tree-2
  (interior-node 'bar (leaf-node -1) tree-1))
(define tree-3
  (interior-node 'baz tree-2 (leaf-node 1)))

(check-expect (max-interior tree-2) 'foo)
(check-expect (max-interior tree-3) 'baz)
                     

;; tree-sum : bintree -> Integer
(define tree-sum
  (lambda (bt)
    (cases bintree bt
      [leaf-node (num) num]
      [interior-node (key left right)
                     (+ (tree-sum left)
                        (tree-sum right))])))

(test)