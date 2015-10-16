#lang eopl

(require test-engine/racket-tests)

;; Nameless letaddr language 

(define list-of
  (lambda (pred?)
    (lambda (xs)
      (or (null? xs)
          (and (pair? xs) 
               (pred? (car xs))
               ((list-of pred?) (cdr xs)))))))


;; -------------------- Lexer --------------------------------

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?" "*")))
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
     ("*" "(" expression "," expression ")")
     mul-exp)
    
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
   (saved-env env?))
  (extend-env-rec
   (proc-name identifier?)
   (bound-var identifier?)
   (proc-body expression?)
   (saved-env env?)))


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

;; ----------------- Senv definition --------------------------

;; Senv : Listof(Sym)
;; Lexadd : Natural (0..)

;; empty-senv : () -> Senv
(define empty-senv
  (lambda ()
    '()))


;; extend-senv : Sym * Senv -> Senv
(define extend-senv
  (lambda (var senv)
    (cons var senv)))

;; extend-senv-var : Sym * Senv -> Senv
(define extend-senv-var
  (lambda (var senv)
    (cons (list 'var var) senv)))

;; extend-senv-letrec : Sym * Senv -> Senv
(define extend-senv-letrec
  (lambda (var senv)
    (cons (list 'letrec var) senv)))

;; apply-senv : Senv * Var -> (Int, SchemeVal) | Error
(define apply-senv
  (lambda (senv var)
    (cond
      [(null? senv) (report-no-binding-found var)]
      [(eqv? var (cadar senv)) (list 0 (caar senv))]
      [else
       (let [(res (apply-senv (cdr senv) var))]
         (list (+ 1 (car res)) (cadr res)))])))

(define init-senv
  (lambda ()
    (extend-senv-var
     'i (extend-senv-var
         'v (extend-senv-var
             'x (empty-senv))))))

;; ----------------- Proc data definition ---------------------

(define-datatype proc proc?
  (procedure
   (body nameless-exp?)
   (saved-nameless-env nameless-environment?)))

;; nameless-environment? : SchemeVal -> Bool
(define nameless-environment?
  (lambda (x)
    ((list-of expval?) x)))

;; empty-nameless-env : () -> Nameless-env
(define empty-nameless-env
  (lambda ()
    '()))

;; extend-nameless-env : ExpVal * Nameless-env -> Nameless-env
(define extend-nameless-env
  (lambda (val nameless-env)
    (cons val nameless-env)))

;; apply-nameless-env : Nameless-env * Lexaddr -> ExpVal
(define apply-nameless-env
  (lambda (nameless-env n)
    (list-ref nameless-env n)))


(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      [procedure (body saved-nameless-env)
                 (value-of body
                           (extend-nameless-env val saved-nameless-env))])))


(define init-nameless-env
  (lambda ()
    (extend-nameless-env 
     (num-val 1)			; was i
     (extend-nameless-env
      (num-val 5)			; was v
      (extend-nameless-env
       (num-val 10)			; was x
       (empty-nameless-env))))))



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


;; ----------------- nameless-exp definition ------------------

(define-datatype nameless-exp nameless-exp?
  (nameless-const-exp
   (num number?))
  (nameless-diff-exp
   (exp1 nameless-exp?)
   (exp2 nameless-exp?))
  (nameless-mul-exp
   (exp1 nameless-exp?)
   (exp2 nameless-exp?))
  (nameless-zero?-exp
   (exp nameless-exp?))
  (nameless-if-exp
   (exp-cond nameless-exp?)
   (exp-then nameless-exp?)
   (exp-else nameless-exp?))
  (nameless-var-exp
   (n number?))
  (nameless-letrec-var-exp
   (n number?))
  (nameless-let-exp
   (exp1 nameless-exp?)
   (body nameless-exp?))
  (nameless-proc-exp
   (body nameless-exp?))
  (nameless-letrec-exp
   (proc-body nameless-exp?)
   (body nameless-exp?))
  (nameless-call-exp
   (rator nameless-exp?)
   (rand nameless-exp?)))


;; ----------------- Interpreter ------------------------------

;; drop : Listof(Any) * Int -> Listof(Any)
(define drop
  (lambda (xs n)
    (cond
      [(null? xs) '()]
      [(= n 0) xs]
      [else (drop (cdr xs) (- n 1))])))

;; value-of : Nameless-exp * Nameless-env -> ExpVal
(define value-of
  (lambda (exp nameless-env)
    (cases nameless-exp exp
      [nameless-const-exp (num) (num-val num)]
      [nameless-diff-exp (exp1 exp2)
                         (let [(val1 (value-of exp1 nameless-env))
                               (val2 (value-of exp2 nameless-env))]
                           (num-val (- (expval->val val1)
                                       (expval->val val2))))]
      [nameless-mul-exp (exp1 exp2)
                        (let [(val1 (value-of exp1 nameless-env))
                              (val2 (value-of exp2 nameless-env))]
                          (num-val (* (expval->val val1)
                                      (expval->val val2))))]
      [nameless-zero?-exp (exp)
                          (let [(val (value-of exp nameless-env))]
                            (bool-val (zero? (expval->val val))))]
      [nameless-if-exp (exp-cond exp-then exp-else)
                       (let [(val-cond (value-of exp-cond nameless-env))]
                         (if (expval->val val-cond)
                             (value-of exp-then nameless-env)
                             (value-of exp-else nameless-env)))]
      [nameless-var-exp (n)
                        (apply-nameless-env nameless-env n)]
      [nameless-letrec-var-exp (n)
                               (let* [(new-nenv (drop nameless-env n))
                                      (proc-obj (expval->val (car new-nenv)))]
                                 (cases proc proc-obj
                                   (procedure (body saved-env)
                                              (proc-val (procedure body new-nenv)))
                                   (else (eopl:error 'value-of
                                                     "Expected a procedure"))))]
      [nameless-let-exp (exp1 body)
                        (let [(val (value-of exp1 nameless-env))]
                          (value-of body
                                    (extend-nameless-env val nameless-env)))]
      [nameless-proc-exp (body)
                         (proc-val
                          (procedure body nameless-env))]
      [nameless-letrec-exp (proc-body body)
                           (let [(the-proc (proc-val (procedure proc-body
                                                                nameless-env)))]
                             (value-of body
                                       (extend-nameless-env the-proc
                                                            nameless-env)))]
      [nameless-call-exp (rator rand)
                         (let [(val (value-of rand nameless-env))
                               (proc-val (expval->val (value-of rator nameless-env)))]
                           (apply-procedure proc-val val))]
      )))


;; value-of-program : Nameless-program -> ExpVa

(define value-of-program
  (lambda (pgm)
    (cases nameless-program pgm
      [a-nameless-program (exp1)
                          (value-of exp1 (init-nameless-env))])))


;; run : String -> ExpVal
(define run
  (lambda (string)
    (value-of-program
     (translation-of-program
      (scan&parse string)))))

;; ---------------- Translator to nameless definition ---------

(define-datatype nameless-program nameless-program?
  (a-nameless-program
   (exp1 nameless-exp?)))

;; translation-a-program : Program -> Nameless-program
(define translation-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (a-nameless-program
                  (translation-of exp1 (init-nameless-env)))))))


;; translation-of : Exp * Senv -> Nameless-exp
(define translation-of
  (lambda (exp senv)
    (cases expression exp
      [const-exp (num) (nameless-const-exp num)]
      [diff-exp (exp1 exp2)
                (nameless-diff-exp
                 (translation-of exp1 senv)
                 (translation-of exp2 senv))]
      [mul-exp (exp1 exp2)
               (nameless-mul-exp
                (translation-of exp1 senv)
                (translation-of exp2 senv))]
      [zero?-exp (exp1)
                 (nameless-zero?-exp
                  (translation-of exp1 senv))]
      [if-exp (exp1 exp2 exp3)
              (nameless-if-exp
               (translation-of exp1 senv)
               (translation-of exp2 senv)
               (translation-of exp3 senv))]
      [var-exp (var)
               (let* [(type-index (apply-senv senv var))
                      (type (cadr type-index))
                      (index (car type-index))]
                 (cond
                   [(eqv? type 'var) (nameless-var-exp index)]
                   [(eqv? type 'letrec) (nameless-letrec-var-exp index)]
                   [else (eopl:error 'translation-of "Unknow variable type: ~A"
                                     type)]))]
      [let-exp (var exp1 body)
               (nameless-let-exp
                (translation-of exp1 senv)
                (translation-of body
                                (extend-senv-var var senv)))]
      [proc-exp (var body)
                (nameless-proc-exp
                 (translation-of body
                                 (extend-senv-var var senv)))]
      [letrec-exp (pname var val body)
                  (nameless-letrec-exp
                   (translation-of val (extend-senv-var
                                        var
                                        (extend-senv-letrec pname senv)))
                   (translation-of body (extend-senv-letrec
                                         pname
                                         senv)))]
      [call-exp (proc exp)
                (nameless-call-exp
                 (translation-of proc senv)
                 (translation-of exp senv))]
      [else
       (report-invalid-source-expression exp)])))

(define report-invalid-source-expression
  (lambda (exp)
    (eopl:error "~A is invalid program" exp)))




;; ---------------- Some test ---------------------------------

(define program0
  "let x = 3
   in -(x, 2)")

(define program1
  "let x = 3
   in let y = 4
      in let z = 5
         in -(x, -(y, z))")

(define program2
  "let x = 3
   in let y = 4
      in let z = 1
         in zero?(-(x, -(y, z)))")

(define program3
  "let x = 3
   in let y = 4
      in let z = 2
         in zero?(-(x, -(y, z)))")

(define program4
  "letrec fact (n) = if zero?(n) then 1 else *(n, (fact -(n, 1)))
   in (fact 5)")


(eopl:printf "~A\n" (translation-of-program (scan&parse program0)))

(check-expect (run program0) (num-val 1))
(check-expect (run program1) (num-val 4))
(check-expect (run program2) (bool-val #t))

(check-expect (run program3) (bool-val #f))

(check-expect (run program4) (num-val 120))

(test)

