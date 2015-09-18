#lang eopl

(require test-engine/racket-tests)

;; NodeInSequence := (Int Listof(Int) Listof(Int))
;; The representation should be easy to go to left, right of the sequence.


(check-expect (number->sequence 7) '(7 () ()))

;; number->sequence : Int -> NodeInSequence
(define number->sequence
  (lambda (num)
    (list num '() '())))

(check-expect (current-element '(6 (5 4 3 2 1) (7 8 9))) 6)

;; current-element NodeInSequence -> Int
(define current-element
  (lambda (nis)
    (car nis)))

(check-expect (move-to-left '(6 (5 4 3 2 1) (7 8 9))) '(5 (4 3 2 1) (6 7 8 9)))
(check-error (move-to-left '(6 () (3 4 5))))

;; move-to-left : NodeInSequence -> NodeInSequence
(define move-to-left
  (lambda (nis)
    (if (at-left-end? nis)
        (eopl:error "Left empty, can not move to left")
        (list (car (cadr nis)) (cdr (cadr nis)) (cons (car nis) (caddr nis))))))


(check-expect (move-to-right '(6 (5 4 3 2 1) (7 8 9))) '(7 (6 5 4 3 2 1) (8 9)))

;; move-to-right : NodeInSequence -> NodeInSequence
(define move-to-right
  (lambda (nis)
    (if (at-right-end? nis)
        (eopl:error "Right empty, can not move to right")
        (list (car (caddr nis)) (cons (car nis) (cadr nis)) (cdr (caddr nis))))))


(check-expect (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9)))
              '(6 (13 5 4 3 2 1) (7 8 9)))

;; insert-to-left : Int * NodeInSequence -> NodeInSequence
(define insert-to-left
  (lambda (elem nis)
    (list (car nis)
          (cons elem (cadr nis))
          (caddr nis))))

(check-expect (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9)))
              '(6 (5 4 3 2 1) (13 7 8 9)))

;; insert-to-right : Int * NodeInSequence -> NodeInSequence
(define insert-to-right
  (lambda (elem nis)
    (list (car nis)
          (cadr nis)
          (cons elem (caddr nis)))))

;; at-left-end? : NodeInSequence -> Bool
(define at-left-end?
  (lambda (nis)
    (null? (cadr nis))))

;; at-right-end? : NodeInSequence -> Bool
(define at-right-end?
  (lambda (nis)
    (null? (caddr nis))))

(test)