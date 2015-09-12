#lang eopl

(require test-engine/racket-tests)

;; Env ::= '() | (cons (var . val) env)
;; Var ::= Sym


(check-expect (empty-env? '()) #t)
(check-expect (empty-env? '((a . b) (c . d))) #f)

;; empty-env? : Env -> Bool
(define empty-env?
  (lambda (env)
    (null? env)))

(check-expect (extend-env 'a 1 '()) '((a . 1)))
(check-expect (extend-env 'a 1 '((b . 2))) '((a . 1) (b . 2)))

;; extend-env : Var * SchemVal * Env -> Env
(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))


(check-expect (apply-env 'a '((b . 1) (a . 2))) 2)
(check-expect (apply-env 'b '((b . 1) (a . 2))) 1)
(check-error (apply-env 'c '((b . 1) (a . 2))))

;; apply-env : Var * Env -> SchemeVal
(define apply-env
  (lambda (var env)
    (cond
      [(empty-env? env) (report-no-binding-found var)]
      [(eqv? var (caar env)) (cdar env)]
      [else (apply-env var (cdr env))])))


(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))

(test)