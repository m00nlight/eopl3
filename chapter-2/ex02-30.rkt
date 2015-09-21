#lang eopl

(require test-engine/racket-tests)

;; Lc-exp :: = Identifier
;;             var-exp (var)
;;        :: = proc Identifier => Lc-exp
;;             (lambda-exp (bound-vars body))
;;        :: = (Lc-exp Lc-exp)
;;             app-exp (rator rand)

(define identifier? symbol?)

(define-datatype lc-exp lc-exp?
  (var-exp
   (var symbol?))
  (lambda-exp
   (bound-vars (list-of identifier?))
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))

;; parse-expression : SchemeVal -> Lc-exp
(define parse-expression
  (lambda (datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      [(pair? datum)
       (cond
         [(and (= (length datum) 3) (eqv? (car datum) 'lambda))
          (lambda-exp
           (cadr datum)
           (parse-expression (caddr datum)))]
         [(and (= (length datum) 2))
          (app-exp
           (parse-expression (car datum))
           (parse-expression (cadr datum)))]
         [else (report-invalid-lc-expression datum)])]
      [else
       (report-invalid-concrete-syntax datum)])))


(check-expect (parse-expression 'x) (var-exp 'x))
(check-expect (parse-expression '(lambda (x) x))
              (lambda-exp '(x) (var-exp 'x)))
(check-expect (parse-expression '(lambda (x y) (x y)))
              (lambda-exp '(x y)
                          (app-exp (var-exp 'x)
                                   (var-exp 'y))))

(check-error (parse-expression '(lambda)))
(check-error (parse-expression '(a b c)))

(check-expect (unparse-lc-exp (var-exp 'x)) 'x)
(check-expect (unparse-lc-exp (lambda-exp '(x) (var-exp 'x)))
              '(lambda (x) x))

(check-expect (unparse-lc-exp (lambda-exp '(x y) (app-exp (var-exp 'x)
                                                          (var-exp 'y))))
              '(lambda (x y) (x y)))

;; unparse-lc-exp : Lc-exp -> SchemeVal
(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var) var)
      (lambda-exp (bound-vars body)
                  (list 'lambda bound-vars
                        (unparse-lc-exp body)))
      (app-exp (rator rand)
               `(,(unparse-lc-exp rator) ,(unparse-lc-exp rand))))))


(define report-invalid-concrete-syntax
  (lambda (datum)
    (eopl:error "Error syntax of ~A" datum)))

(define report-invalid-lc-expression
  (lambda (datum)
    (eopl:error "Error form for lc-exp ~A" datum)))

(test)