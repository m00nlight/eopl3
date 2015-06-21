#lang eopl

(require test-engine/racket-tests)

;; S-list ::= ()
;;        ::= (S-exp . S-list)
;; S-exp  ::= Symbol | S-list

(check-expect (subst 'a 'b '((b c) (b () d))) '((a c) (a () d)))

;; subst : Sym * Sym * S-list -> S-list
(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons
         (if (symbol? (car slist))
             (if (eqv? (car slist) old) new (car slist))
             (subst new old (car slist)))
         (subst new old (cdr slist))))))

(test)