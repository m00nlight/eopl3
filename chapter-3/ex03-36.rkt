#lang eopl

(require test-engine/racket-tests)

;; The LETREC language : A Language with Recursive Procedure


;; -------------------- Lexer --------------------------------

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
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
     ("letrec" (arbno identifier "(" identifier ")" "=" expression)
               "in" expression)
     letrec-exp)
    
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

;; generate list of '(0 1 2 .. (n - 1))
(define range
  (lambda (n)
    (letrec ([helper (lambda (n acc)
                       (if (zero? n)
                           (cons 0 acc)
                           (helper (- n 1) (cons n acc))))])
      (helper (- n 1) '()))))

(define extend-env*
  (lambda (vars vals saved-env)
    (if (null? vars)
        saved-env
        (extend-env (car vars) (car vals)
                    (extend-env* (cdr vars) (cdr vals) saved-env)))))

(define extend-env-rec
  (lambda (p-names b-vars bodys saved-env)
    (let [(vecs (map (lambda (_) (make-vector 1)) p-names))]
      (let [(new-env (extend-env* p-names vecs saved-env))]
        (for-each (lambda (vec b-var body)
                    (vector-set! vec 0 (proc-val
                                        (procedure b-var body new-env))))
                  vecs b-vars bodys)
        new-env))))

;; apply-env : Var * Env -> ExpVal
(define apply-env
  (lambda (var e)
    (cases env e
      [empty-env () (report-no-binding-found var)]
      [extend-env (saved-var saved-val saved-env)
                  (if (eqv? var saved-var)
                      (if (not (vector? saved-val))
                          saved-val
                          (vector-ref saved-val 0))
                      (apply-env var saved-env))])))

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

;; apply-procedure : Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of-expression body (extend-env var val saved-env))))))




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
                (proc-val (procedure var body env))]
      [call-exp (proc exp)
                (let [(val (value-of-expression exp env))
                      (proc-val (expval->val (value-of-expression proc env)))]
                  (apply-procedure proc-val val))]
      [letrec-exp (proc-names bound-vars proc-bodys letrec-body)
                  (let [(nenv (extend-env-rec proc-names bound-vars 
                                              proc-bodys env))]
                    (value-of-expression letrec-body nenv))])))

;; value-of-program : Program * Env -> ExpVal
(define value-of-program
  (lambda (pgm env)
    (cases program pgm
      [a-program (exp) (value-of-expression exp env)])))

;; value-of : String -> ExpVal
(define value-of
  (lambda (pgm)
    (value-of-program (scan&parse pgm) (init-env))))
      


;; ---------------- Some test ---------------------------------

(define program1
  "letrec double (x) = if zero?(x) then 0 else -((double -(x, 1)), -2)
   in (double 6)")

(define program2
  "letrec sum (n) = if zero?(n) then 0 else -((sum -(n, 1)), -(0, n))
   in (sum 10)")

(define program3
  "letrec
     even(x) = if zero?(x) then 1 else (odd -(x, 1))
     odd(x)  = if zero?(x) then 0 else (even -(x, 1))
   in (odd 13)")

(define program4
  "letrec
     even(x) = if zero?(x) then 1 else (odd -(x, 1))
     odd(x)  = if zero?(x) then 0 else (even -(x, 1))
   in (odd 14)")


;; (eopl:printf "~A\n" (scan&parse program1))
(check-expect (value-of program1) (num-val 12))
(check-expect (value-of program2) (num-val 55))
(check-expect (value-of program3) (num-val 1))
(check-expect (value-of program4) (num-val 0))

(test)

