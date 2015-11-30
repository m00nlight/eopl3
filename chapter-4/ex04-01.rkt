#lang eopl

;;let g = proc (dummy)
;;          let counter = newref(0)
;;          in begin
;;               setref(counter, -(deref(counter), -1));
;;               deref(counter)
;;             end
;;in let a = (g 11)
;;   in let b = (g 11)
;;      in -(a, b)

;; The above program will produce 0 instead of -1, since
;; now every call of g has its own private reference.
