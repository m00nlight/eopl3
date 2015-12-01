#lang eopl

(require test-engine/racket-tests)
(require racket/vector)


;; The EXPLICIT-REF language : A Language with explicit reference

;; Implement function call with multiple arguments

(define list-of
  (lambda (pred)
    (lambda (xs)
      (or (null? xs)
          (and (pred (car xs))
               ((list-of pred) (cdr xs)))))))


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
     ("letrec" identifier "(" identifier ")" "=" expression "in" expression)
     letrec-exp)
    
    (expression
     ("proc" "(" (arbno identifier) ")" expression)
     proc-exp)
    
    (expression
     ("(" expression (arbno expression) ")")
     call-exp)

    (expression
     ("newref" "(" expression ")")
     newref-exp)

    (expression
     ("deref" "(" expression ")")
     deref-exp)

    (expression
     ("setref!" "(" expression "," expression ")")
     setref-exp)

    (expression
     ("begin" (separated-list expression ";") "end")
     begin-end-exp)

    (expression
     ("list" "(" (separated-list expression ",") ")")
     list-exp)
    
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
   (saved-env env?))
  (extend-env-rec
   (proc-name identifier?)
   (bound-var identifier?)
   (proc-body expression?)
   (saved-env env?)))

(define extend-env*
  (lambda (vars vals saved-env)
    (if (null? vars)
        saved-env
        (extend-env (car vars) (car vals)
                    (extend-env* (cdr vars) (cdr vals) saved-env)))))

;; apply-env : Var * Env -> ExpVal
(define apply-env
  (lambda (var e)
    (cases env e
      [empty-env () (report-no-binding-found var)]
      [extend-env (saved-var saved-val saved-env)
                  (if (eqv? saved-var var)
                      saved-val
                      (apply-env var saved-env))]
      [extend-env-rec (proc-name bound-var proc-body saved-env)
                      (if (eqv? var proc-name)
                          (proc-val (procedure bound-var proc-body e))
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


;; ----------------- Store definition -------------------------

;; empty-store : () -> Store
(define empty-store
  (lambda () (vector)))

(define the-store 'unintialized)

;; get-store : () -> Store
(define get-store
  (lambda () the-store))

;; initialize-store! : () -> Unspecified
(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

;; newref : ExpVal -> Ref
(define newref
  (lambda (val)
    (let ([next-ref (vector-length the-store)])
      (set! the-store (vector-append the-store (vector val)))
      next-ref)))

;; deref : Ref -> ExpVal
(define deref
  (lambda (ref)
    (vector-ref the-store ref)))

;; setref! : Ref * ExpVal -> Unspecified
;; usage : sets the-store to a state like the original, but with the
;;         position ref containing val

(define setref!
  (lambda (ref val)
    (if (or (< ref 0) (>= ref (vector-length the-store)))
        (report-invalid-reference ref the-store)
        (vector-set! the-store ref val))))


(define report-invalid-reference
  (lambda (ref the-store)
    (eopl:error 'setref
                "illegal reference ~s in store ~s"
                ref the-store)))

;; ----------------- Proc data definition ---------------------

(define-datatype proc proc?
  (procedure
   (var (list-of identifier?))
   (body expression?)
   (saved-env env?)))

;; apply-procedure : Proc * Listof(ExpVal) -> ExpVal
(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
      (procedure (vars body saved-env)
                 (if (not (= (length vars) (length vals)))
                     (eopl:error "Argument length not equal to function ~s"
                                 proc1)
                     (value-of-expression
                      body (extend-env* vars vals saved-env)))))))




;; ----------------- ExpVal definition ------------------------

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (proc proc?))
  (ref-val
   (ref integer?))
  (list-val
   (ls list?)))

;; expval->num : ExpVal -> SchemeVal
(define expval->val
  (lambda (val)
    (cases expval val
      [num-val (num) num]
      [bool-val (bool) bool]
      [proc-val (proc) proc]
      [ref-val (ref) ref]
      [list-val (ls) ls]
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
      [proc-exp (vars body)
                (proc-val (procedure vars body env))]
      [call-exp (proc exps)
                (let [(vals (map (lambda (x)
                                   (value-of-expression x env)) exps))
                      (proc-val (expval->val (value-of-expression proc env)))]
                  (apply-procedure proc-val vals))]
      [letrec-exp (proc-name bound-var proc-body letrec-body)
                  (let [(nenv (extend-env-rec proc-name bound-var 
                                              proc-body env))]
                    (value-of-expression letrec-body nenv))]
      [newref-exp (exp1)
                  (let ([v1 (value-of-expression exp1 env)])
                    (ref-val (newref v1)))]
      [deref-exp (exp1)
                 (let ([v1 (value-of-expression exp1 env)])
                   (let ([ref1 (expval->val v1)])
                     (deref ref1)))]
      [setref-exp (exp1 exp2)
                  (let ([ref (expval->val (value-of-expression exp1 env))])
                    (let ([val2 (value-of-expression exp2 env)])
                      (begin
                        (setref! ref val2)
                        (num-val 23))))]
      [begin-end-exp (exps)
                     (letrec [(helper
                               (lambda (xs)
                                 (if (null? (cdr xs))
                                     (value-of-expression (car xs) env)
                                     (begin
                                       (value-of-expression (car xs) env)
                                       (helper (cdr xs))))))]
                       (helper exps))]
      [list-exp (exps)
                (let ([vs (map (lambda (x) (value-of-expression x env)) exps)])
                  (list-val vs))]
      
      )))

;; value-of-program : Program * Env -> ExpVal
(define value-of-program
  (lambda (pgm env)
    (initialize-store!)
    (cases program pgm
      [a-program (exp) (value-of-expression exp env)])))

;; value-of : String -> ExpVal
(define value-of
  (lambda (pgm)
    (value-of-program (scan&parse pgm) (init-env))))
      


;; ---------------- Some test ---------------------------------

(define program1
  "let g = let counter = newref(0)
           in proc (dummy)
                begin
                  setref!(counter, -(deref(counter), -1));
                  deref(counter)
                end
   in let a = (g 11)
      in let b = (g 11)
         in -(a, b)")

(define program2
  "let g = proc (dummy)
             let counter = newref(0)
             in begin
                  setref!(counter, -(deref(counter), -1));
                  deref(counter)
                end
   in let a = (g 11)
      in let b = (g 11)
         in -(a, b)")

(define program3
  "let x = newref(5)
   in begin
        setref!(x, -(deref(x), 1));
        deref(x)
      end")

(define program4
  "let x = newref(5)
   in list(deref(x), -(deref(x), 1), -(deref(x), 2))")

(define program5
  "let f = proc (a b)
            -(a, b)
   in let a = newref(3)
      in (f deref(a) 4)")
 


;; (eopl:printf "~A\n" (scan&parse program1))
(check-expect (value-of program1) (num-val -1))
(check-expect (value-of program2) (num-val 0))
(check-expect (value-of program3) (num-val 4))
(check-expect (value-of program4)
              (list-val (list (num-val 5)
                              (num-val 4)
                              (num-val 3))))

(check-expect (value-of program5) (num-val -1))

(test)

