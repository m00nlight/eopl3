#lang eopl

(require test-engine/racket-tests)


;; Prefix-list ::= (Prefix-exp)
;; Prefix-exp  ::= Int
;;             ::= - Prefix-exp Prefix-exp

(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))


;; parse-list : Listof(Symbol) -> Prefix-List
(define parse-list
  (lambda (exp)
    (list->prefix-exp (reverse exp))))


(define example-exp
  (diff-exp
   (diff-exp
    (const-exp 3)
    (const-exp 2))
   (diff-exp
    (const-exp 4)
    (diff-exp
     (const-exp 12)
     (const-exp 7)))))

(check-expect (parse-list '(- - 3 2 - 4 - 12 7)) example-exp)

;; list->prefix-exp : Listof(Symbol) -> Prefix-List
(define (list->prefix-exp exp)
  (define (helper exp stack)
    (cond
      [(null? exp) (car stack)]
      [(number? (car exp)) (helper (cdr exp) (cons (const-exp (car exp)) stack))]
      [(eqv? (car exp) '-)
       (let [(operand1 (car stack))
             (operand2 (cadr stack))
             (nstack (cddr stack))]
         (helper (cdr exp) (cons (diff-exp operand1 operand2) nstack)))]
      [else (eopl:error "Invalid diff list expression")]))
  (helper exp '()))
    

(test)