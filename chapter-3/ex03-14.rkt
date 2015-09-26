#lang eopl

(require test-engine/racket-tests)

;; Extract the data type of bool-exp, and implement operation like
;; value-of-bool-exp

;; Let language specification

(define identifier? symbol?)

(define-datatype program program?
  (a-program
   (exp1 (lambda (x) (or (expression? x)
                         (bool-exp? x))))))


;; definition of the bool-exp
(define-datatype bool-exp bool-exp?
  (zero-exp?
   (exp1 expression?))
  (equal-exp?
   (exp1 expression?)
   (exp2 expression?))
  (greater-exp?
   (exp1 expression?)
   (exp2 expression?))
  (less-exp?
   (exp1 expression?)
   (exp2 expression?)))

(define-datatype expression expression?
  (var-expression
   (var identifier?))
  (if-expression
   (exp1 bool-exp?)
   (exp2 expression?)
   (exp3 expression?))
  (let-expression
   (var identifier?)
   (exp1 expression?)
   (body expression?))
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (minus-exp
   (exp expression?))
  (add-exp
   (exp1 expression?)
   (exp2 expression?))
  (mul-exp
   (exp1 expression?)
   (exp2 expression?))
  (quotient-exp
   (exp1 expression?)
   (exp2 expression?)))

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

;; expval-bool : ExpVal -> Bool
(define expval->bool
  (lambda (val)
    (cases expval val
      [bool-val (bool) bool]
      [else (report-expval-extractor-error 'bool val)])))


(define report-expval-extractor-error
  (lambda (proc val)
    (eopl:error "Extract in ~A, val is ~A" proc val)))


;;;;;;;;;;;;;;; grammatical specification;;;;;;;;;;;;;;;;;;;;
(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '(
    ;; specify whether it is an expression or an bool-exp
    (program (expression) a-program)
    (program (bool-exp) a-program)
    
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
     ("let" identifier "=" expression "in" expression)
     let-expression)

    (expression
     ("if" bool-exp "then" expression "else" expression)
     if-expression)

    (bool-exp
     ("zero?" "(" expression ")")
     zero-exp?)

    (bool-exp
     ("equal?" "(" expression "," expression ")")
     equal-exp?)

    (bool-exp
     ("greater?" "(" expression "," expression ")")
     greater-exp?)

    (bool-exp
     ("less?" "(" expression "," expression ")")
     less-exp?)
    ))
;;;;;;;;;;;;;;; end of grammatical specification;;;;;;;;;;;;

;;;;;;;;;;;;;;;;; lexer and parser ;;;;;;;;;;;;;;;;;;;;;;;;;
(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))
;;;;;;;;;;;;;;;;; end of lexer and parser ;;;;;;;;;;;;;;;;;;


;; run : String -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (cond
                   [(bool-exp? exp1) (value-of-bool-exp exp1 (init-env))]
                   [(expression? exp1) (value-of-expression exp1 (init-env))]
                   [else (eopl:error "unsupport datatype ~A" exp1)])))))



;; value-of-bool-exp : Bool-exp * Env -> ExpVal
(define value-of-bool-exp
  (lambda (exp env)
    (cases bool-exp exp
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
                                (expval->num val2))))])))

;; value-of-expression : Expression * Env -> ExpVal
(define value-of-expression
  (lambda (exp env)
    (cases expression exp
      [var-expression (var) (apply-env var env)]
      [if-expression (exp1 exp2 exp3)
                     (let [(val1 (value-of-bool-exp exp1 env))]
                       (if (expval->bool val1)
                           (value-of-expression exp2 env)
                           (value-of-expression exp3 env)))]
      [let-expression (var exp1 body)
                      (let [(val1 (value-of-expression exp1 env))]
                        (value-of-expression
                         body
                         (extend-env var val1 env)))]
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
                                         (expval->num val2))))])))


;; lexer and parse test

(define program1
  "let x = 5 in -(x, 3)")

(define program2
  "let x = 7 in let y = 2 in let y = let x = -(x, 1) in -(x, y)
in -(-(x, 8), y)")

(define program3
  "minus (-(minus(5), 9))")

(define program4
  "add(minus (- (minus(5), 9)), -(3,2))")

(define program5
  "mul(add(minus (- (minus(5), 9)), -(3,2)), add(4, -2))")

(define program6
  "quot(mul(add(minus (- (minus(5), 9)), -(3,2)), add(4, -2)),
add(4, 3))")

(define program7
  "equal?(add(3, 4), -(10, 3))")

(define program8
  "greater?(add(3, 4), -(9, 3))")

(define program9
  "less?(add(3, 4), -(11, 3))")


(check-expect (scan&parse "let x = 5 in -(x, 3)")
              (a-program (let-expression 'x (const-exp 5)
                                  (diff-exp (var-expression 'x)
                                            (const-exp 3)))))

(check-expect (run program1) (num-val 2))
(check-expect (run program2) (num-val -5))
(check-expect (run program3) (num-val 14))
(check-expect (run program4) (num-val 15))
(check-expect (run program5) (num-val 30))
(check-expect (run program6) (num-val 4))
(check-expect (run program7) (bool-val #t))
(check-expect (run program8) (bool-val #t))
(check-expect (run program9) (bool-val #t))

(test)