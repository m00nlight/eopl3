#lang eopl

(require test-engine/racket-tests)

(define identifier? symbol?)

;; Env ::= '() | (cons (var . val) env)
;; Var ::= Sym

;; rewrite the env program with define-datatype

(define-datatype env env?
  (empty-env)
  (extend-env
   (saved-var identifier?)
   (saved-val (lambda (x) #t))
   (saved-env env?)))

(define test-env
  (extend-env 'a 1 (extend-env 'b 2 (empty-env))))



(check-expect (apply-env 'a test-env) 1)
(check-expect (apply-env 'b test-env) 2)
(check-error (apply-env 'c test-env))

;; apply-env : Var * Env -> SchemeVal
(define apply-env
  (lambda (var e)
    (cases env e
      [empty-env () (report-no-binding-found)]
      [extend-env (saved-var saved-val saved-env)
                  (if (eqv? saved-var var)
                      saved-val
                      (apply-env var saved-env))])))

(check-expect (has-binding? 'a test-env) #t)
(check-expect (has-binding? 'b test-env) #t)
(check-expect (has-binding? 'c test-env) #f)

;; has-binding? : Var * Env -> SchemeVal
(define (has-binding? search-var search-env)
  (cases env search-env
    [empty-env () #f]
    [extend-env (saved-var saved-val saved-env)
                (or (eqv? saved-var search-var)
                    (has-binding? search-var saved-env))]))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))
(test)