#lang eopl

(require test-engine/racket-tests)

(define-datatype stack stack?
  (empty-stack)
  (extend-stack
   (val (lambda (x) #t))
   (saved-stack stack?)))


(check-expect (push 3 (empty-stack)) (extend-stack 3 (empty-stack)))

;; push : SchemeVal * Stack -> Stack
(define push
  (lambda (elem st)
    (cases stack st
      [else (extend-stack elem st)])))

(check-error (pop (empty-stack)))
(check-expect (pop (push 3 (empty-stack))) (empty-stack))
(check-expect (pop (push 3 (push 4 (empty-stack))))
              (extend-stack 4 (empty-stack)))
;; pop : Stack -> Stack
(define pop
  (lambda (st)
    (cases stack st
      [empty-stack () (eopl:error "Empty stack can not pop")]
      [extend-stack (val saved-stack) saved-stack])))

(check-expect (top (push 3 (empty-stack))) 3)
(check-error (top (empty-stack)))
(check-expect (top (push 4 (push 3 (empty-stack)))) 4)

;; top : Stack -> SchemeVal
(define top
  (lambda (st)
    (cases stack st
      [empty-stack () (eopl:error "Empty stack can not get top element")]
      [extend-stack (val saved-stack) val])))

(check-expect (empty? (empty-stack)) #t)
(check-expect (empty? (push 4 (empty-stack))) #f)

;; empty : Stack -> Bool
(define empty?
  (lambda (st)
    (equal? st (empty-stack))))

(test)