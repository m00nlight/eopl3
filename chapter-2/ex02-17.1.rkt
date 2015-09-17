#lang eopl

(require test-engine/racket-tests)


;; LcExp ::= Identifier
;;       ::= (lambda (Identifier) LcExp)
;;       ::= (LcExp LcExp)

;; This exercise asked to invented two other representations of the lambda-calculus
;; expression.

;; 1 is to use the internal list to represent LcExp like in chapter one, only need 
;; to add observer and constructor proedure.

(define var-exp
  (lambda (var)
    var))

(define lambda-exp
  (lambda (var body)
    `(lambda ,var ,body)))

(define app-exp
  (lambda (lhs rhs)
    `(,lhs ,rhs)))

(define var-exp?
  (lambda (exp)
    (symbol? exp)))

(define lambda-exp?
  (lambda (exp)
    (eqv? (car exp) 'lambda)))

(define app-exp?
  (lambda (exp)
    (and (not (var-exp? exp))
         (not (lambda-exp? exp)))))


(define var-exp->var
  (lambda (exp)
    exp))

(define lambda-exp->bound-var
  (lambda (exp)
    (cadr exp)))

(define lambda-exp->body
  (lambda (exp)
    (caddr exp)))

(define app-exp->rator
  (lambda (exp)
    (car exp)))

(define app-exp->rand
  (lambda (exp)
    (cadr exp)))


;; the following are copy from exercise 02-16. Just to modify the representation
;; should not affect the test result of the following code.

(check-expect (occur-free? 'x (var-exp 'x)) #t)
(check-expect (occur-free? 'x (var-exp 'y)) #f)
(check-expect (occur-free? 'x (lambda-exp 'x (app-exp (var-exp 'x) (var-exp 'y)))) #f)
(check-expect (occur-free? 'x (lambda-exp 'y (app-exp (var-exp 'y) (var-exp 'x)))) #t)
(check-expect (occur-free? 'x (app-exp (lambda-exp 'x (var-exp 'x))
                                       (lambda-exp 'y (var-exp 'x)))) #t)

(check-expect (occur-free? 'x (lambda-exp 'x (var-exp 'x))) #f)

;; new definition
;; occur-free? : Sym * LcExp -> Bool
(define occur-free?
  (lambda (search-var exp)
    (cond
      [(var-exp? exp) (eqv? search-var (var-exp->var exp))]
      [(lambda-exp? exp)
       (and
        (not (eqv? search-var (lambda-exp->bound-var exp)))
        (occur-free? search-var (lambda-exp->body exp)))]
      [else
       (or
        (occur-free? search-var (app-exp->rator exp))
        (occur-free? search-var (app-exp->rand exp)))])))



(test)