#lang eopl

(require test-engine/racket-tests)

;; extend the Bintree structure to contains the parents
;; information to enable operation like move-up, at-root?
;; etc

;; Bintree ::= ()
;;           | (Int Listof(Bintree) Bintree Bintree)

;; at-leaf? : Bintree -> Bool
(define at-leaf?
  (lambda (bt)
    (null? bt)))

;; at-root? : Bintree -> Bool
(define at-root?
  (lambda (bt)
    (null? (cadr bt))))

;; get-parent : Bintree -> Listof(Bintree)
(define get-parent
  (lambda (bt)
    (cadr bt)))

;; number->bintree : Int -> Bintree
(define number->bintree
  (lambda (elem)
    `(,elem  () () ())))

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
        (caddr bt))))

;; move-to-right-son : Bintree -> Bintree
(define move-to-right-son
  (lambda (bt)
    (if (at-leaf? bt)
        (eopl:error "Empty tree has no right subtree")
        (car (cdddr bt)))))

;; insert-to-left-son : Int * Bintree -> Bintree
(define insert-to-left-son
  (lambda (elem bt)
    (list (current-element bt)
          (get-parent bt)
          (list elem (cons bt (get-parent bt))
                (move-to-left-son bt) '())
          (move-to-right-son bt))))

;; insert-to-right-son : Int * Bintree -> Bintree
(define insert-to-right-son
  (lambda (elem bt)
    (list (current-element bt)
          (get-parent bt)
          (move-to-left-son bt)
          (list elem (cons bt (get-parent bt))
                (move-to-right-son bt) '()))))


(check-expect (number->bintree 13) '(13 () () ()))
(define t1 (insert-to-right-son
            14
            (insert-to-left-son
             12
             (number->bintree 13))))

(check-expect t1 '(13 () (12 ((13 () () ())) () ())
                      (14 ((13 () (12 ((13 () () ())) () ()) ()))
                          () ())))

(check-expect (at-root? t1) #t)

(check-expect (current-element (move-to-left-son t1)) 12)

(check-expect (at-leaf? (move-to-right-son (move-to-left-son t1))) #t)


(test)