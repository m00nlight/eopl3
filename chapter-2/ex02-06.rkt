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


;; apply-env-help : Env * Var * Env -> SchemeVal
;; descp : helper function to give more informative error message when there
;;         is no binding for a variable
(define apply-env-help
  (lambda (env search-var origin)
    (cond
      [(eqv? (car env) 'empty-env) (report-no-binding-found search-var origin)]
      [(eqv? (car env) 'extend-env)
       (let [(saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env))]
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env-help saved-env search-var origin)))]
      [else
       (report-invalid-env origin)])))

;; apply-env : Env * Var -> SchemeVal
(define apply-env
  (lambda (env search-var)
    (apply-env-help env search-var env)))


(define report-no-binding-found
  (lambda (search-var env)
    (eopl:error 'apply-env "No binding for ~s, Env is : ~s" search-var env)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))
