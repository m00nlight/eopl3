#lang eopl

(require test-engine/racket-tests)
;;;; Procedure Representation

;;; Env = Var -> SchemeVal

;; empty-env : () -> Env
(define empty-env
  (lambda ()
    (lambda (search-var)
      (report-no-binding-found search-var))))


;; extend-env : Var * SchemeVal * Env -> Env
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (search-var)
      (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))))


(check-error (apply-env (extend-env 'a 1 (empty-env)) 'c))
(check-expect (apply-env
               (extend-env 'a 1 (extend-env 'b 2 (empty-env))) 'b) 2)

;; apply-env : Env * Var -> SchemeVal
(define apply-env
  (lambda (env search-var)
    (env search-var)))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(test)