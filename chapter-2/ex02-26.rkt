#lang eopl

(require test-engine/racket-tests)


;; Red-blue-tree      ::= Red-blue-subtree
;; Red-blue-subtree   ::= (red-node Red-blue-subtree Red-blue-subtree)
;;                        (blue-node {Red-blue-subtree}*)
;;                        (leaf-node Int)

(define-datatype red-blue-tree red-blue-tree?
  (red-node
   (left red-blue-tree?)
   (right red-blue-tree?))
  (blue-node
   (trees (list-of red-blue-tree?)))
  (leaf-node
   (leaf integer?)))

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))

(check-expect (mark-leaves-with-red-depth
               (red-node (blue-node (list (leaf-node 26)
                                          (leaf-node 12)))
                         (red-node (leaf-node 2)
                                   (blue-node (list (leaf-node 2)
                                                    (leaf-node 2))))))
               (red-node (blue-node (list (leaf-node 1)
                                          (leaf-node 1)))
                         (red-node (leaf-node 2)
                                   (blue-node (list (leaf-node 2)
                                                    (leaf-node 2))))))
                    

;; leaf? : red-blue-tree -> Bool
(define leaf?
  (lambda (rbt)
    (cases red-blue-tree rbt
      [red-node (left right) #f]
      [blue-node (trees) #f]
      [leaf-node (leaf) #t])))


;; mark-leaves-with-red-depth : Bintree -> Bintree
(define mark-leaves-with-red-depth
  (lambda (bt)
    (helper bt 0)))

(define helper
  (lambda (bt cur)
    (cases red-blue-tree bt
      [red-node (left right)
                (red-node (helper left (+ cur 1))
                          (helper right (+ cur 1)))]
      [blue-node (trees)
                 (blue-node (map (lambda (r) (helper r cur)) trees))]
      [leaf-node (leaf) (leaf-node cur)])))


(test)
    