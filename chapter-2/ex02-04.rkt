#lang eopl

(require test-engine/racket-tests)

;; (empty-stack)         = '()
;; (push u stack)        = (cons u stack)
;; (top stack)
;;   - (empty-stack? stack) = error
;;   - else = (car stack)
;; (pop stack)
;;   - (empty-stack? stack) = error
;;   - else = (cdr stack)
;; (empty? stack)
;;   - #t if stack is empty
;;   - #f otherwise


;; Operations of constructors are : empty-stack, push, pop
;; Operations of observers : empty-stack?, top