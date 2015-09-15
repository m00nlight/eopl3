#lang eopl

(require test-engine/racket-tests)

;; Original stack specification

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

;; Procedure structure transform recipe

;; 1. Identify the lambda expressions in the client code whose evaluation yieldsvalues 
;; of the type. Create a constructor procedure for each such lambdaexpression. The 
;; parameters of the constructor procedure will be the freevariables of the lambda 
;; expression. Replace each such lambda expressionin the client code by an invocation 
;; of the corresponding constructor.


;; 2. Define an apply- procedure like apply-env above. Identify all theplaces in 
;; the client code, including the bodies of the constructor proce-dures, where a value 
;; of the type is applied. Replace each such applicationby an invocation of the 
;; apply-procedure.


;; The trick part of this problem is that there are two observers procedure, namely
;; top and pop. So we need to distinguish the two observers in constructors. 

;; At first, I also can not think out the solution, Here the solution was founded 
;; from the following url: https://groups.google.com/forum/#!topic/eopl3/Wf5nbWjhwKE


;; Every constructor use a procedure which take a bool value to distinguish the two 
;; observer

;; empty-stack : () -> Stack
(define empty-stack 
  (lambda () 
    (lambda (bool) 'nil))) 


;; push : SchemeVal * Stack -> Stack
(define push 
  (lambda (val s) 
    (lambda (bool) 
      (if bool s val)))) ; a way to return two things 

;; observer 1
;; pop : Stack -> Stack
(define pop 
  (lambda (s) 
    (s #t))) 

;; observer 2
;; top : Stack -> SchemeVal
(define top 
  (lambda (s) 
    (s #f))) 

(check-expect (top (empty-stack)) 'nil)
(check-expect (top (pop (push 3 (push 4 (empty-stack))))) 4)
(check-expect (top (pop (pop (push 3 (push 4 (push 5 (empty-stack))))))) 5)

(test)