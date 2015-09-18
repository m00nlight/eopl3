#lang eopl

(require test-engine/racket-tests)

;; avoid identifier? is not defined problem
(define identifier? symbol?)

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(check-expect (occur-free? 'x (var-exp 'x)) #t)
(check-expect (occur-free? 'x (var-exp 'y)) #f)
(check-expect (occur-free?
               'x (lambda-exp 'x (app-exp (var-exp 'x) (var-exp 'y)))) #f)
(check-expect (occur-free?
               'x (lambda-exp 'y (app-exp (var-exp 'y) (var-exp 'x)))) #t)
(check-expect (occur-free? 'x (app-exp (lambda-exp 'x (var-exp 'x))
                                       (lambda-exp 'y (var-exp 'x)))) #t)

(check-expect (occur-free? 'x (lambda-exp 'x (var-exp 'x))) #f)
;; occur-free? : Sym * LcExp -> Bool
(define occur-free?
  (lambda (search-var exp)
    (cases lc-exp exp
      (var-exp (var) (eqv? var search-var))
      (lambda-exp (bound-var body)
                  (and (not (eqv? search-var bound-var))
                       (occur-free? search-var body)))
      (app-exp (rator rand)
               (or (occur-free? search-var rator)
                   (occur-free? search-var rand))))))


;; S-list ::= ({S-exp}*)
;; S-exp ::= Symbol | S-list

(define-datatype s-list s-list?
  (empty-s-list)
  (non-empty-s-list
   (first s-exp?)
   (rest s-list?)))

(define-datatype s-exp s-exp?
  (symbol-s-exp
   (sym symbol?))
  (s-list-s-exp
   (slst s-list?)))

(test)