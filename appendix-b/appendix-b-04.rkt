#lang eopl

(require test-engine/racket-tests)


;; ------------------- Problem Statement --------------------
;;
;; Add variable to the arithmetic language
;;
;;

;; -------------------Scanner Specification------------------
(define scanner-spec
  '((white-sp (whitespace) skip)
    (number (digit (arbno digit)) number)
    (identifier (letter (arbno (or letter digit))) symbol)
    (add-op ((or "+" "-")) symbol)
    (mul-op ((or "*" "/")) symbol)))

;; -------------------Grammer Specification------------------
(define grammar
  '((program (arith-expr) a-program)
    (arith-expr (arith-term (arbno add-op arith-term)) an-arith-expr)
    (arith-term (arith-factor (arbno mul-op arith-factor)) an-arith-term)
    (arith-factor (number) number-exp)
    (arith-factor (identifier) var-arith-factor)
    (arith-factor ("(" arith-expr ")") an-arith-factor)))

;; ------------------- Make Scanner and Parser --------------

(sllgen:make-define-datatypes scanner-spec grammar)

(define list-the-datatype
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec grammar)))

(define just-scan
  (sllgen:make-string-scanner scanner-spec grammar))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))


;; ------------------ Env related definition -----------------

(define identifier? symbol?)

(define-datatype env env?
  (empty-env)
  (extend-env
   (saved-var identifier?)
   (saved-val (lambda (x) #t))
   (saved-env env?)))

;; apply-env : Var * Env -> SchemeVal
(define apply-env
  (lambda (var e)
    (cases env e
      [empty-env () (report-no-binding-found)]
      [extend-env (saved-var saved-val saved-env)
                  (if (eqv? saved-var var)
                      saved-val
                      (apply-env var saved-env))])))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define init-env
  (lambda ()
    (extend-env
     'x 1 (extend-env 'y 2 (empty-env)))))


;; ------------------- Interpreter ---------------------------


;; value-of-arith-factor : ArithFactor * Env -> SchemeVal
(define value-of-arith-factor
  (lambda (expr env)
    (cases arith-factor expr
      [number-exp (num) num]
      [an-arith-factor (aexpr)
                       (value-of-arith-expr aexpr env)]
      [var-arith-factor (var)
                        (apply-env var env)])))


;; value-of-arith-term : ArithTerm * Env -> SchemeVal
(define value-of-arith-term
  (lambda (expr env)
    (cases arith-term expr
      [an-arith-term (factor ops factors)
                     (let [(factor-val (value-of-arith-factor factor env))
                           (factors-val (map
                                         (lambda (x) (value-of-arith-factor x env))
                                         factors))]
                       (if (null? ops)
                           factor-val
                           (evaluate-combine-exprs
                            factor-val
                            ops
                            factors-val)))])))


;; value-of-arith-expr : ArithExpr * Env -> SchemeVals
(define value-of-arith-expr
  (lambda (expr env)
    (cases arith-expr expr
      [an-arith-expr (term ops terms)
                     (let [(term-val (value-of-arith-term term env))
                           (terms-val (map
                                       (lambda (x) (value-of-arith-term x env))
                                       terms))]
                       (if (null? ops)
                           term-val
                           (evaluate-combine-exprs
                            term-val
                            ops
                            terms-val)))])))

(define eval-op
  (lambda (op val1 val2)
    (cond
      [(eqv? op '+) (+ val1 val2)]
      [(eqv? op '-) (- val1 val2)]
      [(eqv? op '*) (* val1 val2)]
      [else (/ val1 val2)])))

(define evaluate-combine-exprs
  (lambda (init-val ops vals)
    (if (null? ops)
        init-val
        (cond
          [(eqv? (car ops) '+) (evaluate-combine-exprs
                                (+ init-val (car vals))
                                (cdr ops)
                                (cdr vals))]
          [(eqv? (car ops) '-) (evaluate-combine-exprs
                                (- init-val (car vals))
                                (cdr ops)
                                (cdr vals))]
          [(eqv? (car ops) '*) (evaluate-combine-exprs
                                (* init-val (car vals))
                                (cdr ops)
                                (cdr vals))]
          [else (evaluate-combine-exprs
                 (/ init-val (car vals))
                 (cdr ops)
                 (cdr vals))]))))
                 
(define value-of--program
  (lambda (pgm)
    (cases program pgm
      [a-program (exp1)
                 (value-of-arith-expr exp1 (init-env))])))
                                        
(define read-eval-print
  (sllgen:make-rep-loop
   "--->" value-of--program
   (sllgen:make-stream-parser scanner-spec grammar)))

;; ------------------ Test ----------------------------------
(define program1 "3 + 2 * 66 - 5")

(define program2 "2 + 3")

(define program3 "3 + 1 / 2 / 3 - 5")

(define program4 "x * y")

(define program5 "2 * x + y * 3")


(check-expect (value-of--program (scan&parse program1)) 130)
(check-expect (value-of--program (scan&parse program2)) 5)
(check-expect (value-of--program (scan&parse program3)) (/ -11 6))
(check-expect (value-of--program (scan&parse program4)) 2)
(check-expect (value-of--program (scan&parse program5)) 8)

(test)