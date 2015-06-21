#lang eopl

(require test-engine/racket-tests)



(check-expect (subst 'a 'b '((b c) (b () d))) '((a c) (a () d)))

;; subst : Sym * Sym * S-list -> S-list
(define subst
  (lambda (new old slist)
    (map (lambda (sexp) (subst-in-s-exp new old sexp)) slist)))

;; subst-in-s-exp : Sym * Sym * S-exp -> S-exp
(define subst-in-s-exp
  (lambda (new old sexp)
    (if (symbol? sexp)
        (if (eqv? sexp old) new sexp)
        (subst new old sexp))))


(test)