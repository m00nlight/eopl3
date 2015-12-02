#lang eopl

(require test-engine/racket-tests)

;; The PROC language
;; Show that in dynamic binding, recursive procedure can be
;; used let to define, no special mechanism is needed.
;; See exercise 3.28 for reference.

;; -------------------- Lexer --------------------------------

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?" "+" "*")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

;; ------------------- Grammar -------------------------------
(define the-grammar
  '((program (expression) a-program)
    
    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)

    (expression
     ("+" "(" expression "," expression ")")
     add-exp)

    (expression
     ("*" "(" expression "," expression ")")
     mul-exp)

    (expression
     ("true")
     bool-lit-true-exp)

    (expression
     ("false")
     bool-lit-false-exp)
    
    (expression
     ("zero?" "(" expression ")")
     zero?-exp)
    
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)
    
    (expression (identifier) var-exp)
    
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)   
    
    (expression
     ("proc" "(" identifier ")" expression)
     proc-exp)
    
    (expression
     ("(" expression expression ")")
     call-exp)
    
    ))

;; ---------------- Make parser and lexer--------------------

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;; --------------- Env definition ----------------------------
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
      [empty-env () (report-no-binding-found var)]
      [extend-env (saved-var saved-val saved-env)
                  (if (eqv? saved-var var)
                      saved-val
                      (apply-env var saved-env))])))
                  

;; retain-free : Var * Env -> Env
;; descp :  Remain only the free variable for the procedure
(define retain-free
  (lambda (var e)
    (cases env e
      [empty-env () (empty-env)]
      [extend-env (saved-var saved-val saved-env)
                  (if (eqv? saved-var var)
                      (retain-free var saved-env)
                      (extend-env
                        saved-var
                        saved-val
                        (retain-free var saved-env)))])))


(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))


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

;; ----------------- Proc data definition ---------------------

(define-datatype proc proc?
  (procedure
   (var identifier?)
   (body expression?)
   (saved-env env?)))

;; apply-procedure : Proc * ExpVal * Env -> ExpVal
(define apply-procedure
  (lambda (proc1 val env)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of-expression body (extend-env var val env))))))




;; ----------------- ExpVal definition ------------------------

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (proc proc?)))

;; expval->num : ExpVal -> SchemeVal
(define expval->val
  (lambda (val)
    (cases expval val
      [num-val (num) num]
      [bool-val (bool) bool]
      [proc-val (proc) proc]
      [else (eopl:error "Wrong type of ~A\n" val)])))
  


;; ----------------- Interpreter ------------------------------

;; value-of-expression : Expression * Env -> ExpVal
(define value-of-expression
  (lambda (exp env)
    (cases expression exp
      [const-exp (num) (num-val num)]
      [diff-exp (exp1 exp2)
                (let [(val1 (value-of-expression exp1 env))
                      (val2 (value-of-expression exp2 env))]
                  (num-val (- (expval->val val1)
                              (expval->val val2))))]
      [add-exp (exp1 exp2)
               (let [(val1 (value-of-expression exp1 env))
                     (val2 (value-of-expression exp2 env))]
                 (num-val (+ (expval->val val1)
                             (expval->val val2))))]
      [mul-exp (exp1 exp2)
               (let [(val1 (value-of-expression exp1 env))
                     (val2 (value-of-expression exp2 env))]
                 (num-val (* (expval->val val1)
                             (expval->val val2))))]
      [bool-lit-true-exp () (bool-val #t)]
      [bool-lit-false-exp () (bool-val #f)]
      [zero?-exp (exp)
                 (let [(val (value-of-expression exp env))]
                   (bool-val (zero? (expval->val val))))]
      [if-exp (exp-cond exp-then exp-else)
              (let [(val-cond (value-of-expression exp-cond env))]
                (if (expval->val val-cond)
                    (value-of-expression exp-then env)
                    (value-of-expression exp-else env)))]
      [var-exp (var) (apply-env var env)]
      [let-exp (var exp body)
               (let [(val (value-of-expression exp env))]
                 (value-of-expression body (extend-env var val env)))]
      [proc-exp (var body)
                (proc-val (procedure var body (retain-free var env)))]
      [call-exp (proc exp)
                (let [(val (value-of-expression exp env))
                      (proc-val (expval->val (value-of-expression proc env)))]
                  (apply-procedure proc-val val env))])))

;; value-of-program : Program * Env -> ExpVal
(define value-of-program
  (lambda (pgm env)
    (cases program pgm
      [a-program (exp) (value-of-expression exp env)])))

;; value-of : String -> ExpVal
(define value-of
  (lambda (pgm)
    (value-of-program (scan&parse pgm) (init-env))))


;; ---------------------- Some test ---------------------------------------

(define program0
  "let a = 3
   in let p = proc (x) -(x, a)
      in let a = 5
        in -(a, (p 2))")


;; the use of dynamic binding make write recursive procedure
;; trival. Since the interpreter will evaluate every expression
;; at run time, so it is just to write the recursive procedure
;; in a natural style and we will get recursive automatically

(define program1
  "let fact = proc (n)
                 if zero?(n)
                 then 1
                 else *(n, (fact -(n, 1)))
   in (fact 5)")


(define program2
  "let even? = proc (n) if zero?(n) then 1 else (odd? -(n, 1))
   in let odd? = proc (n) if zero?(n) then 0 else (even? -(n, 1))
      in (odd? 13)")


(check-expect (value-of program0) (num-val 8))
(check-expect (value-of program1) (num-val 120))
(check-expect (value-of program2) (num-val 1))


(test)
