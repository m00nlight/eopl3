#lang eopl

(require test-engine/racket-tests)

;; Diff-tree ::= (one) | (diff Diff-tree Diff-tree)
;; - (one) is represent 1
;; - t1 represent n1 and t2 represent n2, then (diff t1 t2) represent n1 - n2

;; Question 1
;; Since 0 can be represent by (diff num num), so 0 has infinitely many
;; since we can replace num with previously 0 result, and get an new
;; representation of 0. So every number can be expressed by (diff num (zero))
;; and since zero has infinitely many representations, so every number has
;; infinitely many representation.


;; Question 2

(define (one) '(one))
(define (diff left right) `(diff ,left ,right))

;; some help function
(define (one? diff-tree) (equal? (car diff-tree) 'one))
(define (diff? diff-tree) (equal? (cadr diff-tree) 'diff))
(define (diff-n1 diff-tree) (cadr diff-tree))
(define (diff-n2 diff-tree) (caddr diff-tree))

(define (n1 diff-tree)
  (if (one? diff-tree)
      (one)
      (diff-n1 diff-tree)))

(define (n2 diff-tree)
  (if (one? diff-tree)
      (zero)
      (diff-n2 diff-tree)))

(check-expect (zero) '(diff (one) (one)))

;; zero : Void -> Diff-tree
;; descp : Return an representation of zero in Diff-tree system
;; usage : (zero) = '(diff (one) (one))
(define zero
  (lambda ()
    (diff (one) (one))))

(check-expect (is-zero? (diff (one) (one))) #t)
(check-expect (is-zero? (diff (diff (one) (one))
                              (diff (diff (one) (one))
                                    (diff (one) (one))))) #t)

;; is-zero? : Diff-tree -> Bool
;; descp : Judge whether an diff-tree reprensentation is zero
;; usage : (is-zero '(diff (diff (one) (one)) (diff (one) (one))) = #t
(define (is-zero? dt)
  (define (to-int dt)
    (if (one? dt)
        1
        (- (to-int (diff-n1 dt))
           (to-int (diff-n2 dt)))))
  (zero? (to-int dt)))


(check-expect (successor (diff (one) (one)))
              (diff (one) (diff (one) (one))))

;; successor : Diff-tree -> Diff-tree
;; descp : Return the previous number in diff-tree representation
;; usage : (successor (diff (one) (one))
(define (successor dt)
  (diff (n1 dt)
        (diff (n2 dt) (one))))



(check-expect (predecessor (diff (one) (one)))
              (diff (diff (one) (one)) (one)))
;; predecessor : Diff-tree -> Diff-tree
;; descp : Return the predecessor of number in diff-tree representation
;; usage : (predecessor (diff (one) (one))
(define (predecessor dt)
  (diff dt (one)))


;; Question 3

;; since if dt1 is (diff n11 n12) and dt2 is (diff n21 n22), then
;; dt1 + dt2 = n11 + n21 - n12 - n22 = n11 - n12 - (n22 - n21)
;;                                   = (diff (diff n11 n12) (diff n22 n21))

(check-expect
 (diff-tree-plus (diff (one) (one)) (one))
 (diff (diff (one) (one)) (diff (diff (one) (one)) (one))))
 

;; diff-tree-plus : Diff-tree * Diff-tree -> Diff-tree
;; descp : Add to number in Diff-tree representation
;; usage : (diff-tree-plus (diff (one) (one)) (one))
(define (diff-tree-plus dt1 dt2)
  (diff dt1
        (diff (n2 dt2) (n1 dt2))))

(test)