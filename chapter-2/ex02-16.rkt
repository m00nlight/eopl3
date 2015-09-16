#lang eopl

(require test-engine/racket-tests)

;; LcExp ::= Identifier
;;       ::= (lambda (Identifier) LcExp)
;;       ::= (LcExp LcExp)

;; Modify the representation so that the bound variable in lambda expression
;; do not have parenthess. This can be done by just modify the lambda-exp, 
;; lambda-exp->bound-var procedure of the previous exercise.


;; var-exp : Var -> LcExp
(define var-exp
  (lambda (var)
    `(lc-exp var ,var)))

;; lambda-exp : Var * Lc-exp -> Lc-exp
(define lambda-exp
  (lambda (var body)
    `(lc-exp lambda ,var ,body)))

;; app-exp : Lc-exp * Lc-exp -> Lc-exp
(define app-exp
  (lambda (lhs rhs)
    `(lc-exp app ,lhs ,rhs)))

(check-expect (var-exp? (var-exp 'x)) #t)
(check-expect (var-exp? (lambda-exp 'x (var-exp 'y))) #f)

;; var-exp? : Lc-exp -> Bool
(define var-exp?
  (lambda (exp)
    (eqv? (cadr exp) 'var)))

;; lambda-exp? : Lc-exp -> Bool
(define lambda-exp?
  (lambda (exp)
    (eqv? (cadr exp) 'lambda)))

;; app-exp? LcExp -> Bool
(define app-exp?
  (lambda (exp)
    (eqv? (cadr exp) 'app)))

;; var-exp->var : LcExp -> Var
(define var-exp->var
  (lambda (exp)
    (caddr exp)))

;; lambda-exp->bound-var : LcExp -> Var
(define lambda-exp->bound-var
  (lambda (exp)
    (caddr exp)))

;; lambda-exp->body : LcExp -> LcExp
(define lambda-exp->body
  (lambda (exp)
    (cadddr exp)))

;; app-exp->rator : LcExp -> LcExp
(define app-exp->rator
  (lambda (exp)
    (caddr exp)))

;; app-exp->rand : LcExp -> LcExp
(define app-exp->rand
  (lambda (exp)
    (cadddr exp)))


(check-expect (occur-free? 'x (var-exp 'x)) #t)
(check-expect (occur-free? 'x (var-exp 'y)) #f)
(check-expect (occur-free? 'x (lambda-exp 'x (app-exp (var-exp 'x) (var-exp 'y)))) #f)
(check-expect (occur-free? 'x (lambda-exp 'y (app-exp (var-exp 'y) (var-exp 'x)))) #t)
(check-expect (occur-free? 'x (app-exp (lambda-exp 'x (var-exp 'x))
                                       (lambda-exp 'y (var-exp 'x)))) #t)

(check-expect (occur-free? 'x (lambda-exp 'x (var-exp 'x))) #f)

;; origin definition of occur-free?
#;
(define occur-free?
  (lambda (var exp)
    (cond
      ((symbol? exp) (eqv? var exp))
      ((eqv? (car exp) 'lambda)
       (and
        (not (eqv? var (car (cadr exp))))
        (occur-free? var (caddr exp))))
      (else
       (or
        (occur-free? var (car exp))
        (occur-free? var (cadr exp)))))))

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