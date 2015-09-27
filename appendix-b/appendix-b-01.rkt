#lang eopl

(require test-engine/racket-tests)


;; -------------------Scanner Specification------------------
(define scanner-spec
  '((white-sp (whitespace) skip)
    (number (digit (arbno digit)) number)
    (add-op ((or "+" "-")) symbol)
    (mul-op ((or "*" "/")) symbol)))

;; -------------------Grammer Specification------------------
(define grammar
  '((arith-expr (arith-term (arbno add-op arith-term)) an-arith-expr)
    (arith-term (arith-factor (arbno mul-op arith-factor)) an-arith-term)
    (arith-factor (number) number-exp)
    (arith-factor ("(" arith-expr ")") an-arith-factor)))

;; ------------------- Make Scanner and Parser --------------

(sllgen:make-define-datatypes scanner-spec grammar)

(define list-the-datatype
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec grammar)))

(define just-scan
  (sllgen:make-string-scanner scanner-spec grammar))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))

;; dummy interpreter
(define value-of--program #t)

(define read-eval-print
  (sllgen:make-rep-loop
   "--->" value-of--program
   (sllgen:make-stream-parser scanner-spec grammar)))

;; ------------------ Test ----------------------------------
(define program "3 + 2 * 66 - 5")

(check-expect (scan&parse program)
              (an-arith-expr
               (an-arith-term (number-exp 3) '() '())
               '(+ -)
               (list (an-arith-term
                      (number-exp 2)
                      '(*)
                      (list (number-exp 66)))
                     (an-arith-term (number-exp 5) '() '()))))

(test)