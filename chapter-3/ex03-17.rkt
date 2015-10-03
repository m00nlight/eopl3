#lang eopl

(require test-engine/racket-tests)

;; Extend the language with a let∗ expression that works like
;; Scheme’s let∗

;; Let language specification

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

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?)))

;; init-env : () -> Env
;; usage : (init-env) = [i = 1, v = 5, x = 10]
(define init-env
  (lambda ()
    (extend-env
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))


;; expval->num : ExpVal -> Int
(define expval->num
  (lambda (val)
    (cases expval val
      [num-val (num) num]
      [else (report-expval-extractor-error 'num val)])))

;; extend-env* : Listof(Symbol) * Listof(SchemVal) * Env -> Env
(define extend-env*
  (lambda (vars vals env)
    (if (null? vars)
        env
        (extend-env (car vars) (car vals)
                    (extend-env* (cdr vars) (cdr vals) env)))))

;; extend-env-star : Listof(Symbol) * Listof(Expression) * Env -> Env
(define extend-env-star
  (lambda (vars exps env)
    (if (null? vars)
        env
        (let [(val (value-of-expression (car exps) env))]
          (extend-env (car vars) val
                      (extend-env-star (cdr vars) (cdr exps)
                                       (extend-env (car vars) val env)))))))

;; expval-bool : ExpVal -> Bool
(define expval->bool
  (lambda (val)
    (cases expval val
      [bool-val (bool) bool]
      [else (report-expval-extractor-error 'bool val)])))


(define report-expval-extractor-error
  (lambda (proc val)
    (eopl:error "Extract in ~A, val is ~A" proc val)))


;; ---------------- The lexer specification -----------------
(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

;; ------------ The grammar specification ---------------------
(define the-grammar
  '(
    ;; specify whether it is an expression or an bool-exp
    (program (expression) a-program-exp)
    
    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)

    (expression
     ("minus" "(" expression ")")
     minus-exp)

    (expression
     ("add" "(" expression ","  expression ")")
     add-exp)

    (expression
     ("mul" "(" expression "," expression ")")
     mul-exp)

    (expression
     ("quot" "(" expression "," expression ")")
     quotient-exp)
    
    (expression (identifier) var-expression)
    
    (expression
     ("let" (arbno identifier "=" expression) "in" expression)
     let-expression)

    (expression
     ("let*" (arbno identifier "=" expression) "in" expression)
     let-star-expression)

    (expression
     ("if" expression "then" expression "else" expression)
     if-expression)

    (expression
     ("zero?" "(" expression ")")
     zero-exp?)

    (expression
     ("equal?" "(" expression "," expression ")")
     equal-exp?)

    (expression
     ("greater?" "(" expression "," expression ")")
     greater-exp?)

    (expression
     ("less?" "(" expression "," expression ")")
     less-exp?)

    (expression
     ("print" expression)
     print-exp)
    
    ))



;; ------------------- Make lexer and parser-------------------
(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define list-the-datatype
  (lambda ()
    (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))




;; run : String -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program-exp (exp1)
                     (value-of-expression exp1 (init-env))))))



;; value-of-expression : Expression * Env -> ExpVal
(define value-of-expression
  (lambda (exp env)
    (cases expression exp
      [var-expression (var) (apply-env var env)]
      [if-expression (exp1 exp2 exp3)
                     (let [(val1 (value-of-expression exp1 env))]
                       (if (expval->bool val1)
                           (value-of-expression exp2 env)
                           (value-of-expression exp3 env)))]
      [let-expression (vars exps body)
                      (let [(vals (map (lambda (x)
                                         (value-of-expression x env)) exps))]
                        (value-of-expression
                         body
                         (extend-env* vars vals env)))]
      [let-star-expression (vars exps body)
                           (let [(nenv (extend-env-star vars exps env))]
                             (value-of-expression body nenv))]
      [const-exp (num) (num-val num)]
      [diff-exp (exp1 exp2)
                (let [(val1 (value-of-expression exp1 env))
                      (val2 (value-of-expression exp2 env))]
                  (let [(num1 (expval->num val1))
                        (num2 (expval->num val2))]
                    (num-val
                     (- num1 num2))))]
      [minus-exp (exp)
                 (let [(val (value-of-expression exp env))]
                   (num-val (- (expval->num val))))]
      [add-exp (exp1 exp2)
               (let [(val1 (value-of-expression exp1 env))
                     (val2 (value-of-expression exp2 env))]
                 (num-val (+ (expval->num val1)
                             (expval->num val2))))]
      [mul-exp (exp1 exp2)
               (let [(val1 (value-of-expression exp1 env))
                     (val2 (value-of-expression exp2 env))]
                 (num-val (* (expval->num val1)
                             (expval->num val2))))]
      [quotient-exp (exp1 exp2)
                    (let [(val1 (value-of-expression exp1 env))
                          (val2 (value-of-expression exp2 env))]
                      (num-val (quotient (expval->num val1)
                                         (expval->num val2))))]
      [zero-exp? (exp1)
                 (let [(val1 (value-of-expression exp1 env))]
                   (if (zero? (expval->num val1))
                       (bool-val #t)
                       (bool-val #f)))]
      [equal-exp? (exp1 exp2)
                  (let [(val1 (value-of-expression exp1 env))
                        (val2 (value-of-expression exp2 env))]
                    (bool-val (eqv? (expval->num val1)
                                    (expval->num val2))))]
      [greater-exp? (exp1 exp2)
                    (let [(val1 (value-of-expression exp1 env))
                          (val2 (value-of-expression exp2 env))]
                      (bool-val (> (expval->num val1)
                                   (expval->num val2))))]
      [less-exp? (exp1 exp2)
                 (let [(val1 (value-of-expression exp1 env))
                       (val2 (value-of-expression exp2 env))]
                   (bool-val (< (expval->num val1)
                                (expval->num val2))))]
      [print-exp (exp)
                 (let [(val (value-of-expression exp env))]
                   (eopl:printf "~A\n" (expval->num val))
                   (num-val 1))]

      )))

;; lexer and parse test

(define program1 "print -(3, 4)")
(define program2 "let x = 3 in print -(x, add(2, 3))")
(define program3 "let x = add(2, 3) in print x")
(define program4
  "let x = 3
       y = 4
   in add(x, y)")

(define program5
  "let x = 30
   in let x = -(x, 1)
          y = -(x, 2)
      in -(x, y)")

(define program6
  "let x = 30
   in let* x = -(x, 1) y = -(x, 2)
      in -(x, y)")


(check-expect (run program1) (num-val 1))
(check-expect (run program2) (num-val 1))
(check-expect (run program3) (num-val 1))
(check-expect (run program4) (num-val 7))
(check-expect (run program5) (num-val 1))
(check-expect (run program6) (num-val 2))

(test)