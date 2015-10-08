#lang eopl

(require test-engine/racket-tests)

;; The LETREC language : A Language with Recursive Procedure


;; -------------------- Lexer --------------------------------

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?" "+")))
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
     ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression) "in" expression)
     letrec-exp)
    
    (expression
     ("proc" "(" (arbno identifier) ")" expression)
     proc-exp)
    
    (expression
     ("(" expression (arbno expression) ")")
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

(define list-of
  (lambda (pred)
    (lambda (xs)
      (or (null? xs)
          (and (pair? xs)
               ((list-of pred) (cdr xs)))))))
            
            
;; Symbol * Listof(Symbol) -> Bool
(define in-list?
  (lambda (var vs)
    (if (null? vs)
        #f
        (or (eqv? var (car vs))
            (in-list? var (cdr vs))))))
  

(define-datatype env env?
  (empty-env)
  (extend-env
   (saved-var identifier?)
   (saved-val (lambda (x) #t))
   (saved-env env?))
  (extend-env-rec
   (proc-names (list-of identifier?))
   (bound-vars (list-of identifier?))
   (proc-bodys (list-of expression?))
   (saved-env env?)))


(define extend-env*
  (lambda (vars vals e)
    (if (null? vars)
        e
        (extend-env (car vars)
                    (car vals)
                    (extend-env* (cdr vars) (cdr vals) e)))))

;; apply-env : Var * Env -> ExpVal
(define apply-env
  (lambda (var e)
    (cases env e
      [empty-env () (report-no-binding-found var)]
      [extend-env (saved-var saved-val saved-env)
                  (if (eqv? saved-var var)
                      saved-val
                      (apply-env var saved-env))]
      [extend-env-rec (proc-names bound-vars proc-bodys saved-env)
                      (define find
                        (lambda (pnames bvars pbodys)
                          (cond [(null? pnames) (apply-env var saved-env)]
                                [(eqv? (car pnames) var) 
                                 (proc-val (procedure (car bvars) 
                                                      (car pbodys) e))]
                                [else (find (cdr pnames) (cdr bvars)
                                            (cdr pbodys))])))
                      (find proc-names bound-vars proc-bodys)])))
                                 

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
   (vars (list-of identifier?))
   (body expression?)
   (saved-env env?)))

;; apply-procedure : Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
      (procedure (vars body saved-env)
                 (if (not (eqv? (length vars)
                                (length vals)))
                     (eopl:error "Arith mismatch ~A\n" proc1)
                     (value-of-expression 
                        body (extend-env* vars vals saved-env)))))))




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
      [proc-exp (vars body)
                (proc-val (procedure vars body env))]
      [call-exp (proc exps)
                (let [(vals (map (lambda (x) (value-of-expression x env)) 
                                 exps))
                      (proc-val (expval->val (value-of-expression proc env)))]
                  (apply-procedure proc-val vals))]
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
  "letrec
     even? (x) = if zero?(x) then 1 else (odd? -(x, 1))
     odd? (x)  = if zero?(x) then 0 else (even? -(x, 1))
     sum (x acc) = if zero?(x) then acc else (sum -(x, 1) +(acc, x))
   in -((sum 10 0), (odd? 13))")
 


(check-expect (value-of program1) (num-val 54))


(test)
