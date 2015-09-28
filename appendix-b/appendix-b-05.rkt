#lang eopl

(require test-engine/racket-tests)

;; Add unary operator minus to the language

;; -------------------Scanner Specification------------------
(define scanner-spec
  '((white-sp (whitespace) skip)
    (number (digit (arbno digit)) number)))

;; -------------------Grammer Specification------------------
(define grammar
  '((program (arith-expr) a-program)
    (arith-expr (arith-term (arbno add-op arith-term)) an-arith-expr)
    (arith-term (arith-factor (arbno mul-op arith-factor)) an-arith-term)
    (arith-factor (number) number-exp)
    (arith-factor ("(" arith-expr ")") an-arith-factor)
    (arith-factor ("-" arith-factor) neg-arith-factor)
    (add-op ("+") plus-add-op)
    (add-op ("-") minus-add-op)
    (mul-op ("*") mul-mul-op)
    (mul-op ("/") div-mul-op)))

;; ------------------- Make Scanner and Parser --------------

(sllgen:make-define-datatypes scanner-spec grammar)

(define list-the-datatype
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec grammar)))

(define just-scan
  (sllgen:make-string-scanner scanner-spec grammar))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))


;; ------------------- Interpreter ---------------------------


;; value-of-arith-factor : ArithFactor -> SchemeVal
(define value-of-arith-factor
  (lambda (expr)
    (cases arith-factor expr
      [number-exp (num) num]
      [an-arith-factor (aexpr)
                       (value-of-arith-expr aexpr)]
      [neg-arith-factor (factor) (- (value-of-arith-factor factor))])))


;; value-of-arith-term : ArithTerm -> SchemeVal
(define value-of-arith-term
  (lambda (expr)
    (cases arith-term expr
      [an-arith-term (factor ops factors)
                     (let [(factor-val (value-of-arith-factor factor))
                           (ops-val (map value-of-mul-op ops))
                           (factors-val (map value-of-arith-factor factors))]
                       (if (null? ops)
                           factor-val
                           (evaluate-combine-exprs
                            factor-val
                            ops-val
                            factors-val)))])))


;; value-of-arith-expr : ArithExpr -> SchemeVals
(define value-of-arith-expr
  (lambda (expr)
    (cases arith-expr expr
      [an-arith-expr (term ops terms)
                     (let [(term-val (value-of-arith-term term))
                           (ops-val (map value-of-add-op ops))
                           (terms-val (map value-of-arith-term terms))]
                       (if (null? ops)
                           term-val
                           (evaluate-combine-exprs
                            term-val
                            ops-val
                            terms-val)))])))

(define value-of-add-op
  (lambda (expr)
    (cases add-op expr
      [plus-add-op () '+]
      [minus-add-op () '-])))

(define value-of-mul-op
  (lambda (expr)
    (cases mul-op expr
      [mul-mul-op () '*]
      [div-mul-op () '/])))

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
                 (value-of-arith-expr exp1)])))
                                        
(define read-eval-print
  (sllgen:make-rep-loop
   "--->" value-of--program
   (sllgen:make-stream-parser scanner-spec grammar)))

;; ------------------ Test ----------------------------------
(define program1 "3 + 2 * 66 - 5")

(define program2 "2 + 3")

(define program3 "3 + 1 / 2 / 3 - 5")

(define program4 "3 * -2")

(define program5 "3 * -(2 + 3 * 2) - 5")


(check-expect (value-of--program (scan&parse program1)) 130)
(check-expect (value-of--program (scan&parse program2)) 5)
(check-expect (value-of--program (scan&parse program3)) (/ -11 6))
(check-expect (value-of--program (scan&parse program4)) -6)
(check-expect (value-of--program (scan&parse program5)) -29)



(test)