#lang eopl

(require test-engine/racket-tests)

;; in-S? : N -> Bool
;; usage: (in-S? n) = t if n is in S, #f otherwise

(define in-S?
  (lambda (n)
    (if (zero? n)
        #t
        (if (>= (- n 3) 0)
            (in-S? (- n 3))
            #f))))


;; list-length :: List -> Int
;; usage: (list-length l) = the length of L
(define list-length
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (list-length (cdr lst))))))


;; nth-element : List * Int -> SchemeVal
;; usage: (nth-element lst) = then n-th element of lst
(define nth-element
  (lambda (lst n)
    (if (null? lst)
        (report-list-too-short n)
        (if (zero? n)
            (car lst)
            (nth-element (cdr lst) (- n 1))))))

(define report-list-too-short
  (lambda (n)
    (eopl:error 'nth-element
                "List too short by ~s element. ~%" (+ n 1))))


;; remove-first : Sym * Listof(Sym) -> Listof(Sym)
;; usage: (remove-first s los) return a list with the same elemnt
;;        arranged in the same order as los, except that the first
;;        occurance of symbol s is removed
(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (cons (car los) (remove-first (cdr los)))))))


;; LcExp ::= Identifier
;;       ::= (lambda (Identifier) LcExp)
;;       ::= (LcExp LcExp)

(check-expect (occur-free? 'x 'x) #t)
(check-expect (occur-free? 'x 'y) #f)
(check-expect (occur-free? 'x '(lambda (x) (x y))) #f)
(check-expect (occur-free? 'x '(lambda (y) (x y))) #t)
(check-expect (occur-free? 'x '((lambda (x) x) (x y))) #t)
(check-expect (occur-free? 'x '(lambda (y) (lambda (z) (x (y z))))) #t)

(define occur-free?
  (lambda (var exp)
    (cond
      ((symbol? exp) (eqv? var exp))
      ((eqv? (car exp) 'lambda)
       (and
        (not (eqv? var (car (cadr exp))))
        (occur-free? var (caddr exp))))
      (else
       (or
        (occur-free? var (car exp))
        (occur-free? var (cadr exp)))))))

(test)
