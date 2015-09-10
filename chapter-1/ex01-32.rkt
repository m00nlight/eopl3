#lang eopl

(require test-engine/racket-tests)

(check-expect (leaf? 3) #t)
(check-expect (leaf? '(bar 1 (foo 1 2))) #f)
(check-expect (lson 3) 'nil)
(check-expect (lson '(bar 1 (foo 2 3))) 1)
(check-expect (rson 4) 'nil)
(check-expect (rson '(bar 1 (foo 2 3))) '(foo 2 3))
(check-expect (content-of 3) 3)
(check-expect (content-of '(bar 1 (foo 2 3))) 'bar)
(check-expect (double-tree 3) 6)
(check-expect (double-tree '(foo 1 (bar 2 3))) '(foo 2 (bar 4 6)))

;; Bintree ::= Int | (Symbol Bintree Bintree)
;; Example:
;;   - 1
;;   - 2
;;   - (bar 1 (foo 1 2))
;;   - (baz
;;       (bar 1 (foo 1 2))
;;       (biz 4 5))


;; leaf? : Bintree -> Bool
;; usage : (leaf? 3) = #t
;;         (leaf? (bar 1 (foo 1 2))) = #f
(define leaf?
  (lambda (bt)
    (number? bt)))

;; lson : Bintree -> Bintree
;; usge : (lson 3) = 'nil
;;        (lson (baz 1 (foo 1 2))) = 1
(define lson
  (lambda (bt)
    (if (number? bt)
        'nil
        (cadr bt))))

;; rson : Bintree -> Bintree
;; usge : (rson 3) = 'nil
;;        (rson (baz 1 (foo 1 2))) = '(foo 1 2)
(define rson
  (lambda (bt)
    (if (number? bt)
        'nil
        (caddr bt))))

;; content-of : Bintree -> Symbol | Int
;; usage : (content-of 3) = 3
;;         (content-of '(foo 1 (bar 2 3)) = 'foo
(define content-of
  (lambda (bt)
    (if (number? bt)
        bt
        (car bt))))


;; double-tree : Bintree -> Bintree
;; usage : (double-tree 3) = 6
;;         (double-tree '(foo 1 (bar 2 3))) = '(foo 2 (bar 4 6))
(define double-tree
  (lambda (bt)
    (if (leaf? bt)
        (* (content-of bt) 2)
        (list (car bt)
              (double-tree (lson bt))
              (double-tree (rson bt))))))

(test)