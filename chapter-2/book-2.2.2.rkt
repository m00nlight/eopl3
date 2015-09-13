#lang eopl

(require test-engine/racket-tests)

;; The Interpreter Recipe
;; - Look at a piece of data
;; - Decide what kind of data it represent
;; - Extract the components of the datum and do the right thing with them


;; Env ::= (empty-env) | (extend-env Var SchemeVal Env)
;; Var ::= Sym

;; empty-env : Void -> Env
(define empty-env
  (lambda () (list 'empty-env)))


;; extend-env : Var * SchemeVal * Env -> Env
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))


;; apply-env : Env * Var -> SchemeVal
(define apply-env
  (lambda (env search-var)
    (cond
      [(eqv? (car env) 'empty-env) (report-no-binding-found search-var)]
      [(eqv? (car env) 'extend-env)
       (let [(saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env))]
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env saved-env search-var)))]
      [else
       (report-invalid-env env)])))


(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))
