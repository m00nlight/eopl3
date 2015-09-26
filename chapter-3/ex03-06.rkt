#lang eopl

(require test-engine/racket-tests)

;; Extend the LET language by adding new operator `minus`

;; Let language specification

(define identifier? symbol?)

(define-datatype program program?
  (a-program
   (exp1 expression?)))

(define-datatype expression expression?
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero-exp?
   (exp1 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp
   (var identifier?))
  (minus-exp
   (exp expression?))
  (let-exp
   (var identifier?)
   (exp1 expression?)
   (body expression?)))

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
  '((program (expression) a-program)
    
    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    
    (expression
     ("zero?" "(" expression ")")
     zero-exp?)

    ;; add grammar for minus operation
    (expression
     ("minus" "(" expression ")")
     minus-exp)
    
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)
    
    (expression (identifier) var-exp)
    
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)   
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
                 (value-of exp1 (init-env))))))


;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
      [const-exp (num) (num-val num)]
      [var-exp (var) (apply-env var env)]
      [diff-exp (exp1 exp2)
                (let [(val1 (value-of exp1 env))
                      (val2 (value-of exp2 env))]
                  (let [(num1 (expval->num val1))
                        (num2 (expval->num val2))]
                    (num-val
                     (- num1 num2))))]
      [zero-exp? (exp1)
                 (let [(val1 (value-of exp1 env))]
                   (let [(num1 (expval->num val1))]
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f))))]
      [minus-exp (exp)
                 (let [(val (value-of exp env))]
                   (num-val (- (expval->num val))))]
      [if-exp (exp1 exp2 exp3)
              (let [(val1 (value-of exp1 env))]
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env)))]
      [let-exp (var exp1 body)
               (let [(val1 (value-of exp1 env))]
                 (value-of body
                           (extend-env var val1 env)))])))


;; lexer and parse test

(define program1
  "let x = 5 in -(x, 3)")

(define program2
  "let x = 7 in let y = 2 in let y = let x = -(x, 1) in -(x, y)
in -(-(x, 8), y)")

(define program3
  "minus (-(minus(5), 9))")

(check-expect (scan&parse "let x = 5 in -(x, 3)")
              (a-program (let-exp 'x (const-exp 5)
                                  (diff-exp (var-exp 'x)
                                            (const-exp 3)))))

(check-expect (run program1) (num-val 2))
(check-expect (run program2) (num-val -5))
(check-expect (run program3) (num-val 14))

(test)