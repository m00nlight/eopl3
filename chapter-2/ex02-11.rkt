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

(check-expect (extend-env* '(a b) '(1 2) '()) '(((a b) . (1 2))))
(check-expect (extend-env* '(c d) '(3 4) '((a . 1) (b . 2))) 
              '(((c d) . (3 4)) (a . 1) (b . 2)))
;; extend-env* : Listof(Var) * Listof(SchemeVal) * Env -> Env
(define extend-env*
  (lambda (vars vals env)
    (cons (cons vars vals) env)))


;; ribs-find : Listof(Vars) * Listof(SchemeVals) * Var -> '() | SchemeVal
;; descp : Find whether an var is in a ribs, if it is not, return '(), otherwise
;;         return the value of the var in the ribs
(define ribs-find
  (lambda (vars vals search-var)
    (cond
      [(null? vars) '()]
      [(eqv? (car vars) search-var) (car vals)]
      [else (ribs-find (cdr vars) (cdr vals) search-var)])))


(check-expect (has-binding? 'c '((a . 1) (b . 2))) #f)
(check-expect (has-binding? 'b '((a . 1) (b . 2))) #t)
(check-expect (has-binding? 'd (extend-env* '(c d) '(3 4) '((a . 1) (b . 2)))) #t)
(check-expect (has-binding? 'e (extend-env* '(c d) '(3 4) '((a . 1) (b . 2)))) #f)



;; has-binding? Var * Env -> Bool
;; descp : Judge whehter an Var is in environment, and return #t if the 
;;         binding of the var is in the environment.

(define has-binding?
  (lambda (var env)
    (cond
      [(empty-env? env) #f]
      [(symbol? (caar env)) (if (eqv? (caar env) var)
                                #t
                                (has-binding? var (cdr env)))]
      [else (or (not (null? (ribs-find (caar env) (cdar env) var)))
                (has-binding? var (cdr env)))])))


(check-expect (apply-env 'a '((b . 1) (a . 2))) 2)
(check-expect (apply-env 'b '((b . 1) (a . 2))) 1)
(check-error (apply-env 'c '((b . 1) (a . 2))))
(check-expect (apply-env 'a (extend-env* '(a b) '(1 2) '())) 1)

;; apply-env : Var * Env -> SchemeVal
(define apply-env
  (lambda (var env)
    (apply-env-help var env env)))

;; apply-env-help : Var * Env * Env -> SchemeVal
(define apply-env-help
  (lambda (var env origin)
    (cond
      [(empty-env? env) (report-no-binding-found var origin)]
      [(symbol? (caar env)) (if (eqv? var (caar env))
                                (cdar env)
                                (apply-env-help var (cdr env) origin))]
      [else (let [(fd (ribs-find (caar env) (cdar env) var))]
              (if (null? fd)
                  (apply-env-help var (cdr env) origin)
                  fd))])))


(define report-no-binding-found
  (lambda (search-var env)
    (eopl:error 'apply-env "No binding for ~s, Env is : ~s" search-var env)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))

(test)