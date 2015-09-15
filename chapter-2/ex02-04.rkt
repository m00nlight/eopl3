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

;; Constructor : build elements of that data type
;; Observer : Extract information from the value of the data type

;; So based on the definition above, we can know that
;; Operations of constructors are : empty-stack, push
;; Operations of observers : empty-stack?, top, pop

;; Here pop is marked as an observer instead of a constructor since
;; it extract some information from the structure