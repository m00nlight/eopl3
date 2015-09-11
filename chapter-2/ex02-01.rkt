#lang eopl

(require test-engine/racket-tests)

;; bignum representation
;; [n] is one of
;;   - () n = 0
;;   - (r . [q]), n = q * N + r

;; N is the base number of the bignum system
(define N 16)


(check-expect (zero) '())

;; zero : Void -> Bignum
;; descp : return the zero value in Bignum
(define zero
  (lambda () '()))


(check-expect (is-zero? (zero)) #t)
(check-expect (is-zero? '(2 0 1)) #f)

;; is-zero? : Bignum -> Bool
;; descp : Judge whehter a bignum is zero
;; usage : (is-zero (zero)) = #t
;;         (is-zero '(2 0 1)) = #f
(define is-zero?
  (lambda (bignum)
    (null? bignum)))

(check-expect (successor '(2 0 1)) '(3 0 1))
(check-expect (successor '(15 0 1)) '(0 1 1))
(check-expect (successor '(15)) '(0 1))

;; successor : Bignum -> Bignum
;; descp : Return the next number in bignum representation
;; usage : (successor (zero)) = '(1)
(define successor
  (lambda (bignum)
    (cond
      [(is-zero? bignum) '(1)]
      [(equal? (car bignum) (- N 1)) (cons 0 (successor (cdr bignum)))]
      [else (cons (+ (car bignum) 1) (cdr bignum))])))


(check-expect (predecessor '(2 0 1)) '(1 0 1))
(check-expect (predecessor '(0 0 1)) '(15 15))
(check-expect (predecessor '(0 1)) '(15))

;; predecessor : Bignum -> Bignum
;; descp : Return the predecessor number in bignum representation
;; usage : (predecessor '(2 0 1)) = '(1 0 1)
;;         (predecessor '(0 0 1)) = '(15 15)
(define predecessor
  (lambda (bignum)
    (cond
      [(is-zero? bignum) (eopl:error "No predecessor of zero number")]
      [(equal? bignum '(1)) '()]
      [(zero? (car bignum)) (cons (- N 1) (predecessor (cdr bignum)))]
      [else (cons (- (car bignum) 1) (cdr bignum))])))


(check-expect (num->bigint 258) '(2 0 1))
(check-expect (num->bigint 16) '(0 1))

;; num->bigint : Int -> Bigint
;; descp : Normal number to bigint representation
;; usage : (num->bigint 258) = '(2 0 1)
(define num->bigint
  (lambda (num)
    (if (zero? num)
        (zero)
        (cons (modulo num N) (num->bigint (quotient num N))))))

(check-expect (bigint->num '(2 0 1)) 258)
(check-expect (bigint->num '(0 1)) 16)
(check-expect (bigint->num '()) 0)
;; bigint->num : Bigint -> Int
;; descp : Bigint representation to normal number in racket
;; usage : (bigint->num '(2 0 1)) = 258
;; usage : (bigint->num '(0 1)) = 16

(define bigint->num
  (lambda (bigint)
    (cond
      [(is-zero? bigint) 0]
      [else (+ (car bigint) (* N (bigint->num (cdr bigint))))])))


(check-expect (add '(0 1) '(0 1)) '(0 2))
(check-expect (add '(1) '(15)) '(0 1))
;; add : Bignum * Bignum -> Bignum
;; descp : Add two number in bignum represenation
(define add
  (lambda (num1 num2)
    (if (is-zero? num1)
        num2
        (successor (add (predecessor num1) num2)))))


(check-expect (multiply (num->bigint 16) (num->bigint 16))
              (num->bigint 256))
(check-expect (multiply (num->bigint 2) (num->bigint 64))
              (num->bigint 128))

;; multiply : Bigint * Bigint -> Bigint
;; descp : Multiply two bigint numbers
(define multiply
  (lambda (num1 num2)
    (if (is-zero? num1)
        (zero)
        (add num2 (multiply (predecessor num1) num2)))))


(check-expect (factorial (num->bigint 3))
              (num->bigint 6))
(check-expect (factorial (num->bigint 4))
              (num->bigint 24))
(check-expect (factorial (num->bigint 10))
              (num->bigint 3628800))

;; factorial : Bigint -> Bigint
;; descp : Calculate the bigint representation of n!.
(define factorial
  (lambda (num)
    (if (is-zero? num)
        (successor num)
        (multiply num (factorial (predecessor num))))))

(test)