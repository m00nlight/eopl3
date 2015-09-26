#lang eopl

(require test-engine/racket-tests)

;; Extend the LET language with cond operator

;; Let language specification

(define identifier? symbol?)

(define-datatype program program?
  (a-program
   (exp1 expression?)))

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))

(define-datatype cond-atom cond-atom?
  (cond-atom-exp
   (cd expression?)
   (result expression?)))

(define-datatype expression expression?
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
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
   (exp2 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp
   (var identifier?))
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
   (exp2 expression?))
  (emptylist-exp)
  (cons-exp
   (head any?)
   (tail list-exp?))
  (list-exp
   (vals (list-of expression?)))
  (cond-exp
   (lhss (list-of expression?))
   (rhss (list-of expression?)))
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


(define any? expression?)

(define list-exp?
  (lambda (exp)
    (cases expression exp
      [emptylist-exp () #t]
      [cons-exp (head tail) #t]
      [else #f])))


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
   (bool boolean?))
  (list-val
   (ls list?)))

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

;; expval->bool : ExpVal -> Bool
(define expval->bool
  (lambda (val)
    (cases expval val
      [bool-val (bool) bool]
      [else (report-expval-extractor-error 'bool val)])))

;; expval->list ExpVal -> List
(define expval->list
  (lambda (val)
    (cases expval val
      [list-val (ls) ls]
      [else (report-expval-extractor-error 'list val)])))

;; expval->any : Expval -> Any
(define expval->any
  (lambda (val)
    (cases expval val
      [num-val (num) num]
      [bool-val (bool) bool]
      [list-val (ls) ls]
      [else (report-expval-extractor-error 'any val)])))


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

    (expression
     ("equal?" "(" expression "," expression ")")
     equal-exp?)

    (expression
     ("greater?" "(" expression "," expression ")")
     greater-exp?)

    (expression
     ("less?" "(" expression "," expression ")")
     less-exp?)

    ;; add grammar for minus operation
    (expression
     ("minus" "(" expression ")")
     minus-exp)

    (expression
     ("add" "(" expression "," expression ")")
     add-exp)

    (expression
     ("mul" "(" expression "," expression ")")
     mul-exp)

    (expression
     ("quot" "(" expression "," expression ")")
     quotient-exp)
    
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)
    
    (expression (identifier) var-exp)

    (expression
     ("emptylist")
     emptylist-exp)

    (expression
     ("cons" "(" expression "," expression ")")
     cons-exp)

    ;; following the answer here parsing the list expression
    ;; https://github.com/svenpanne/EOPL3/blob/master/chapter3/exercise-3-10.rkt
    (expression
     ("list" "(" (separated-list expression ",") ")")
     list-exp)
    
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

    (expression
     ("cond" (arbno expression "==>" expression) "end")
     cond-exp)
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
      [equal-exp? (exp1 exp2)
                  (let [(val1 (value-of exp1 env))
                        (val2 (value-of exp2 env))]
                    (bool-val (eqv? (expval->num val1)
                                    (expval->num val2))))]
      [greater-exp? (exp1 exp2)
                    (let [(val1 (value-of exp1 env))
                          (val2 (value-of exp2 env))]
                      (bool-val (> (expval->num val1)
                                   (expval->num val2))))]
      [less-exp? (exp1 exp2)
                 (let [(val1 (value-of exp1 env))
                       (val2 (value-of exp2 env))]
                   (bool-val (< (expval->num val1)
                                (expval->num val2))))]
      [minus-exp (exp)
                 (let [(val (value-of exp env))]
                   (num-val (- (expval->num val))))]
      [add-exp (exp1 exp2)
               (let [(val1 (value-of exp1 env))
                     (val2 (value-of exp2 env))]
                 (num-val (+ (expval->num val1)
                             (expval->num val2))))]
      [mul-exp (exp1 exp2)
               (let [(val1 (value-of exp1 env))
                     (val2 (value-of exp2 env))]
                 (num-val (* (expval->num val1)
                             (expval->num val2))))]
      [quotient-exp (exp1 exp2)
                    (let [(val1 (value-of exp1 env))
                          (val2 (value-of exp2 env))]
                      (num-val (quotient (expval->num val1)
                                         (expval->num val2))))]
      [if-exp (exp1 exp2 exp3)
              (let [(val1 (value-of exp1 env))]
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env)))]
      [emptylist-exp ()
                     (list-val '())]
      [cons-exp (head tail)
                (let [(val1 (value-of head env))
                      (val2 (value-of tail env))]
                  (list-val (cons (expval->any val1)
                                  (expval->list val2))))]
      [list-exp (vals)
                (let [(vvals (map (lambda (x) (value-of x env)) vals))]
                  (list-val (map (lambda (x) (expval->any x)) vvals)))]
      [cond-exp (lhss rhss)
                (value-of-cond lhss rhss env)]
      [let-exp (var exp1 body)
               (let [(val1 (value-of exp1 env))]
                 (value-of body
                           (extend-env var val1 env)))])))

;; value-of-cond : List-of(Expression) * List-of(Expression) -> Env -> Expval
(define value-of-cond
  (lambda (lhss rhss env)
    (cond
      [(null? lhss) (eopl:error "valud-of-cond, no matching found")]
      [(expval->bool (value-of (car lhss) env))
       (value-of (car rhss) env)]
      [else
       (value-of-cond (cdr lhss) (cdr rhss) env)])))


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

(define program10
  "let x = 4 in cons(x, cons(cons(-(x, 1), emptylist), emptylist))")

(define program11
  "let x = 4 in list(x, -(x, 1), -(x, 3))")

(define program12
  "let x = 4 in cond equal?(x, 3) ==> add(x, 1)
                     equal?(x, 4) ==> mul(x, 2)
                     equal?(x, 5) ==> mul(x, 3)
                end")

(check-expect (scan&parse "let x = 5 in -(x, 3)")
              (a-program (let-exp 'x (const-exp 5)
                                  (diff-exp (var-exp 'x)
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
(check-expect (run program10) (list-val (cons 4 (cons (cons 3 '()) '()))))
(check-expect (run program11) (list-val (list 4 3 1)))
(check-expect (run program12) (num-val 8))

(test)