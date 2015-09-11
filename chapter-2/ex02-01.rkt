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




(test)