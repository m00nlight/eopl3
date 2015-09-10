#lang eopl

(require test-engine/racket-tests)

(check-expect (number-leaves '(foo (bar 26 12)
                                   (baz 2 (quux 3 4))))
              '(foo (bar 0 1)
                    (baz 2 (quux 3 4))))

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

;; mark-leaves-with-red-depth : Bintree -> Bintree
(define number-leaves
  (lambda (bt)
    (car (helper bt 0))))

(define helper
  (lambda (bt cur)
    (cond
      [(leaf? bt) (list cur (+ 1 cur))]
      [else (let* [(left (helper (lson bt) cur))
                   (right (helper (rson bt) (cadr left)))]
              (list
               (list (content-of bt)
                    (car left)
                    (car right))
               (cadr right)))])))

(test)
    