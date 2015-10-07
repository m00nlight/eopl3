#lang eopl

;; Question : What is the purpose of the call to proc-val on the next-to-last
;; line of apply-env?

;; Answer: Because apply-env return ExpVal, and we need to change the proc
;; to a ExpVal type. Since it is a procedure, then we use proc-val to change
;; the procedure to a ExpVal.