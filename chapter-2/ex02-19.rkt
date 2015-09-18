#lang eopl

(require test-engine/racket-tests)

;; Bintree ::= () | (Int Bintree Bintree)

;; at-leaf? : Bintree -> Bool
(define at-leaf?
  (lambda (bt)
    (null? bt)))

;; number->bintree : Int -> Bintree
(define number->bintree
  (lambda (elem)
    `(,elem () ())))

;; current-element : Bintree -> Int
(define current-element
  (lambda (bt)
    (if (at-leaf? bt)
        (eopl:error "Empty tree has no current element")
        (car bt))))


;; move-to-left-son : Bintree -> Bintree
(define move-to-left-son
  (lambda (bt)
    (if (at-leaf? bt)
        (eopl:error "Empty tree has no left subtree")
        (cadr bt))))

;; move-to-right-son : Bintree -> Bintree
(define move-to-right-son
  (lambda (bt)
    (if (at-leaf? bt)
        (eopl:error "Empty tree has no right subtree")
        (caddr bt))))

;; insert-to-left-son : Int * Bintree -> Bintree
(define insert-to-left-son
  (lambda (elem bt)
    (list (current-element bt)
          (list elem (move-to-left-son bt) '())
          (caddr bt))))

;; insert-to-right-son : Int * Bintree -> Bintree
(define insert-to-right-son
  (lambda (elem bt)
    (list (current-element bt)
          (cadr bt)
          (list elem (move-to-right-son bt) '()))))


(check-expect (number->bintree 13) '(13 () ()))
(define t1 (insert-to-right-son
            14
            (insert-to-left-son
             12
             (number->bintree 13))))

(check-expect t1 '(13 (12 () ()) (14 () ())))

(check-expect (current-element (move-to-left-son t1)) 12)

(check-expect (at-leaf? (move-to-right-son (move-to-left-son t1))) #t)

(check-expect (insert-to-left-son 15 t1)
              '(13 (15 (12 () ()) ()) (14 () ())))


(test)