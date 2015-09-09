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
         (subst-in-s-exp new old (car slist))
         (subst new old (cdr slist))))))

;; subst-in-s-exp : Sym * Sym * S-exp -> S-exp
(define subst-in-s-exp
  (lambda (new old sexp)
    (if (symbol? sexp)
        (if (eqv? sexp old) new sexp)
        (subst new old sexp))))



(check-expect (number-elements-from '() 0) '())
(check-expect (number-elements-from '(a b c d) 3) '((3 a) (4 b) (5 c) (6 d)))

;; number-elements-from : Listof(SchemeVal) * Int -> Listof((Int, SchemeVal))
;; usage: (number-elements-from '(v0 v1 v2 ...) n) = ((n v0) (n + 1 v1) 
;;                                                    (n+2 v2) ...)
(define number-elements-from
  (lambda (lst n)
    (if (null? lst)
        '()
        (cons
         (list n (car lst))
         (number-elements-from (cdr lst) (+ n 1))))))


(check-expect (number-elements '(a b c d)) '((0 a) (1 b) (2 c) (3 d)))

;; number-elements : Listof(SchemeVal) -> Listof((Int, SchemeVal))
(define number-elements
  (lambda (lst)
    (number-elements-from lst 0)))


;; list-sum : Listof(Int) -> Int
(define list-sum
  (lambda (loi)
    (if (null? loi)
        0
        (+ (car loi)
           (list-sum (cdr loi))))))


(check-expect (list-sum '()) 0)
(check-expect (list-sum '(1 2 3)) 6)


;; partial-vector-sum : Vectorof(Int) * Int -> Int
;; usage : if 0 <= n < length(v), then
;;         (partial-vector-sum v n) = \sigma_{i = 0}^{i = n}v_i

(define partial-vector-sum
  (lambda (v n)
    (if (zero? n)
        (vector-ref v 0)
        (+ (vector-ref v n)
           (partial-vector-sum v (- n 1))))))


;; vector-sum : Vectorof(Int) -> Int
;; usage : (vector-sum v) = \sigma_{i = 0}^{i = length(v) - 1}v_i

(define vector-sum
  (lambda (v)
    (let ((n (vector-length v)))
      (if (zero? n)
          0
          (partial-vector-sum v (- n 1))))))


(check-expect (vector-sum (make-vector 5 1)) 5)
(check-expect (vector-sum (make-vector 5 2)) 10)
(check-expect (vector-sum (make-vector 0 '())) 0)

(test)
