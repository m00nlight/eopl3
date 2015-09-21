#lang eopl

(require test-engine/racket-tests)

;; Lc-exp :: = Identifier
;;             var-exp (var)
;;        :: = proc Identifier => Lc-exp
;;             (lambda-exp (bound-var body))
;;        :: = (Lc-exp Lc-exp)
;;             app-exp (rator rand)

(define-datatype lc-exp lc-exp?
  (var-exp
   (var symbol?))
  (lambda-exp
   (bound-var symbol?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))


(check-expect (parse-expression 'x) (var-exp 'x))
(check-expect (parse-expression '(lambda (x) x))
              (lambda-exp 'x (var-exp 'x)))
;; parse-expression : SchemeVal -> Lc-exp
(define parse-expression
  (lambda (datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      [(pair? datum)
       (if (eqv? (car datum) 'lambda)
           (lambda-exp
            (car (cadr datum))
            (parse-expression (caddr datum)))
           (app-exp
            (parse-expression (car datum))
            (parse-expression (cadr datum))))]
      [else
       (report-invalid-concrete-syntax datum)])))


(check-expect (unparse-lc-exp (var-exp 'x)) "x")
(check-expect (unparse-lc-exp (lambda-exp 'x (var-exp 'x)))
              "(lambda (x) x)")
(check-expect (unparse-lc-exp (app-exp (var-exp 'x) (var-exp 'y)))
              "(x y)")
;; unparse-lc-exp : Lc-exp -> String
(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var) (symbol->string var))
      (lambda-exp (bound-var body)
                  (string-append "(lambda (" (symbol->string bound-var) ") "
                                 (unparse-lc-exp body)
                                 ")"))
      (app-exp (rator rand)
               (string-append "(" (unparse-lc-exp rator)
                              " " (unparse-lc-exp rand)
                              ")")))))


(define report-invalid-concrete-syntax
  (lambda (datum)
    (eopl:error "Error syntax of ~A" datum)))

(test)