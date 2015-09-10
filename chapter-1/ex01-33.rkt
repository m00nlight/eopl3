#lang eopl

(require test-engine/racket-tests)

(check-expect (mark-leaves-with-red-depth
               '(red (bar 26 12)
                     (red 2 (quux 2 2))))
              '(red (bar 1 1)
                    (red 2 (quux 2 2))))

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
(define mark-leaves-with-red-depth
  (lambda (bt)
    (helper bt 0)))


(define helper
  (lambda (bt cur)
    (cond
      [(leaf? bt) cur]
      [(equal? (content-of bt) 'red)
       (list 'red (helper (lson bt) (+ cur 1))
             (helper (rson bt) (+ cur 1)))]
      [else (list (content-of bt) (helper (lson bt) cur)
                  (helper (rson bt) cur))])))


(test)
    