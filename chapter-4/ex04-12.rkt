#lang eopl

(require test-engine/racket-tests)

;; The EXPLICIT-REF language : A Language with explicit reference

;; 4.12 Store-passing interpreter


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
     ("proc" "(" identifier ")" expression)
     proc-exp)
    
    (expression
     ("(" expression expression ")")
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

;; Since we are using store passing style, every update to the
;; store return a new store. Here we use an association list to
;; represent the store sturcture.

;; update-assoc Assoc[(Ref . ExpVal)] * Ref * ExpVal -> Assoc[(Ref . ExpVal)]
(define update-assoc
  (lambda (lst ref val)
    (cond
      [(null? lst) (eopl:error "Element not in associative list")]
      [(equal? (caar lst) ref) (cons (cons ref val) lst)]
      [else (cons (car lst) (update-assoc (cdr lst)))])))

;; empty-store : () -> Store
(define empty-store
  (lambda () '()))


;; newref : ExpVal * Store -> (Ref . Store)
(define newref
  (lambda (val store)
    (let [(next-ref (length store))]
      (list next-ref (cons (cons next-ref val) store)))))


;; deref : Ref * Store -> ExpVal
(define deref
  (lambda (ref store)
    (cdr (assoc ref store))))


;; Ref * ExpVal * Store -> Store
(define setref
  (lambda (ref val store)
    (if (or (< ref 0) (>= ref (length store)))
        (report-invalid-reference ref store)
        (update-assoc store ref val))))


(define report-invalid-reference
  (lambda (ref the-store)
    (eopl:error 'setref
                "illegal reference ~s in store ~s"
                ref the-store)))

;; ----------------- Proc data definition ---------------------

(define-datatype proc proc?
  (procedure
   (var identifier?)
   (body expression?)
   (saved-env env?)))

;; apply-procedure : Proc * ExpVal * Store -> Answer
(define apply-procedure
  (lambda (proc1 val store)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of-exp body (extend-env var val saved-env)
                                                      store)))))
                                


(define (store? sto)
  (list? sto))


;; ----------------- Answer Definitino ------------------------
(define-datatype answer answer?
  (an-answer
   (val expval?)
   (store store?)))

(define answer->val
  (lambda (ans)
    (cases answer ans
      [an-answer (val store) val])))


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

;; value-of-expression : Expression * Env * Sto -> Answer
(define value-of-exp
  (lambda (exp env store)
    (cases expression exp
      [const-exp (num) (an-answer (num-val num) store)]
      [diff-exp (exp1 exp2)
                (let [(ans1 (value-of-exp exp1 env store))]
                  (cases answer ans1
                    (an-answer (val1 store1)
                               (let [(ans2 (value-of-exp exp2 env store1))]
                                 (cases answer ans2
                                   (an-answer (val2 store2)
                                              (an-answer (num-val
                                                          (-
                                                           (expval->val val1)
                                                           (expval->val val2)))
                                                         store2)))))))]
      [zero?-exp (exp)
                 (let [(ans (value-of-exp exp env store))]
                   (cases answer ans
                     (an-answer (val new-store)
                                (an-answer (bool-val (zero? (expval->val val)))
                                           new-store))))]
      [if-exp (exp-cond exp-then exp-else)
              (let [(ans-cond (value-of-exp exp-cond env store))]
                (cases answer ans-cond
                  (an-answer (val-cond new-store)
                             (if (expval->val val-cond)
                                 (value-of-exp exp-then env new-store)
                                 (value-of-exp exp-else env new-store)))))]
      [var-exp (var) (an-answer (apply-env var env) store)]
      [let-exp (var exp body)
               (let [(ans (value-of-exp exp env store))]
                 (cases answer ans
                   (an-answer (val new-store)
                              (value-of-exp body (extend-env var val env)
                                                   new-store))))]
      [proc-exp (var body)
                (an-answer (proc-val (procedure var body env)) store)]
      [call-exp (proc exp)
                (let [(val-ans (value-of-exp exp env store))]
                  (cases answer val-ans
                    (an-answer (val new-store)
                               (let [(proc-ans (value-of-exp proc env
                                                             new-store))]
                                 (cases answer proc-ans
                                   (an-answer (proc-val new-new-store)
                                              (apply-procedure
                                               (expval->val proc-val)
                                               val
                                               new-new-store)))))))]
      [letrec-exp (proc-name bound-var proc-body letrec-body)
                  (let [(nenv (extend-env-rec proc-name bound-var 
                                              proc-body env))]
                    (value-of-exp letrec-body nenv))]
      [newref-exp (exp1)
                  (let ([ans1 (value-of-exp exp1 env store)])
                    (cases answer ans1
                      (an-answer (v1 new-store)
                                 (let* [(tmp (newref v1 new-store))
                                        (ref (car tmp))
                                        (new-new-store (cadr tmp))]
                                   (an-answer (ref-val ref)
                                              new-new-store)))))]
      [deref-exp (exp1)
                 (let ([a1 (value-of-exp exp1 env store)])
                   (cases answer a1
                     (an-answer (ref new-store)
                                (an-answer (deref (expval->val ref) new-store)
                                           new-store))))]
      [setref-exp (exp1 exp2)
                  (let ([ans (value-of-exp exp1 env store)])
                    (cases answer ans
                      (an-answer (val1 new-store)
                                 (let* [(ref (expval->val val1))
                                        (ans2 (value-of-exp exp2 env
                                                            new-store))]
                                   (cases answer ans2
                                     (an-answer (val nn-store)
                                                (an-answer (num-val 23)
                                                           (setref
                                                            ref val
                                                            nn-store))))))))]
      [begin-end-exp (exps)
                     (letrec [(helper
                               (lambda (xs store)
                                 (if (null? (cdr xs))
                                     (value-of-exp (car xs) env store)
                                     (let [(tmp (value-of-exp (car xs) env
                                                              store))]
                                       (cases answer tmp
                                         [an-answer (val new-store)
                                                    (helper (cdr xs)
                                                            new-store)])))))]
                       (helper exps store))]
      [list-exp (exps)
                (let ([vs (map (lambda (x) (answer->val
                                            (value-of-exp x env store)))
                               exps)])
                  (an-answer (list-val vs) store))]
      
      )))

;; value-of-program : Program * Env -> ExpVal
(define value-of-program
  (lambda (pgm env)
    (cases program pgm
      [a-program (exp) (value-of-exp exp env (empty-store))])))

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
  "let counter = newref(3)
   in deref(counter)")

(define program6
  "let g = let counter = newref(0)
           in proc (dummy)
                begin
                  setref!(counter, -(deref(counter), -1));
                  deref(counter)
                end
   in let a = (g 11)
      in let b = (g 11)
         in list(a, b)")

 
;; (eopl:printf "~A\n" (scan&parse program1))
(check-expect (answer->val (value-of program1))
              (num-val -1))

(check-expect (answer->val (value-of program2))
              (num-val 0))
(check-expect (answer->val (value-of program3)) (num-val 4))
(check-expect (answer->val (value-of program4))
              (list-val (list (num-val 5)
                              (num-val 4)
                              (num-val 3))))

(check-expect (answer->val (value-of program5))
              (num-val 3))

(check-expect (answer->val (value-of program6))
              (list-val (list (num-val 1)
                              (num-val 2))))

(test)

