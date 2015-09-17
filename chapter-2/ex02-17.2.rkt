#lang eopl

(require test-engine/racket-tests)


;; LcExp ::= Identifier
;;       ::= (lambda (Identifier) LcExp)
;;       ::= (LcExp LcExp)

;; This exercise asked to invented two other representations of the lambda-calculus
;; expression.

;; Another method is to represent the lambda calculus expression in procedure expression
;; each constructor is a list of procedure of every observers.

;; Procedure representation of lambda calculi expression

;; Constructor : var-exp, lambda-exp, app-exp
;; Observer :
;;   - 1. var-exp?
;;   - 2. lambda-exp?
;;   - 3. app-exp?
;;   - 4. var-exp->var 
;;   - 5. lambda-exp->bound-var
;;   - 6. lambda-exp->body
;;   - 7. app-exp->rator
;;   - 8. app-exp->rand
(define var-exp
  (lambda (var)
    (list (lambda () #t)
          (lambda () #f)
          (lambda () #f)
          ;; var-exp->var
          (lambda () var))))

(define lambda-exp
  (lambda (var body)
    (list (lambda () #f)
          (lambda () #t)
          (lambda () #f)
          ;; lambda-exp->bound-var
          (lambda () var)
          ;; lambda-exp->body
          (lambda () body))))

(define app-exp
  (lambda (lhs rhs)
    (list (lambda () #f)
          (lambda () #f)
          (lambda () #t)
          ;; app-exp->rator
          (lambda () lhs)
          ;; app-exp->rand
          (lambda () rhs))))
          
(define var-exp?
  (lambda (exp)
    ((car exp))))

(define lambda-exp?
  (lambda (exp)
    ((cadr exp))))

(define app-exp?
  (lambda (exp)
    ((caddr exp))))

(define var-exp->var
  (lambda (exp)
    ((cadddr exp))))

(define lambda-exp->bound-var
  (lambda (exp)
    ((cadddr exp))))

(define lambda-exp->body
  (lambda (exp)
    ((cadddr (cdr exp)))))
    
(define app-exp->rator
  (lambda (exp)
    ((cadddr exp))))

(define app-exp->rand
  (lambda (exp)
    ((cadddr (cdr exp)))))


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