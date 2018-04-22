#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Spring 2018
;;
;; Brandon Chavez
;; W012049191
;;
;; The purpose of this program is to get me a good grade.
;; Somehow, it accomplishes this by defining a procedure, "I", which is used to find
;; an approximation for the definite integral of a given function, 'f'.
;; Ironically, the function "I" itself cannot actually find the definite integral.
;; But it *will* return a function which can (and does) find the definite integral
;; provided an interval [a, b] on which the definite integral can then be defined.
;;
;; Please note that I did not devise the Integral procedure's companion procedure,
;; "D"! I merely copied it from the lab documentation provided to me by Professor Matthews
;; because "lab02-test.rkt" demanded that I provide such a definition upon running.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide I)
(provide D)

;; The procedure for calculating the integral of some function 'f'.
(define I
  (lambda (f)
    ; Returns a function that gathers more information (the interval) and then find the definite integral.
    (lambda (a b)
      (let* ((n 100000)
              (delta (/(- b a) n)))
          ; Compact definition and subsequent execution of the Riemann Summation formula.
          (let Idef-rec ((a a) (b b) (f f))
              (if (>= a b)
              0
              (+ (* (f a) delta) (Idef-rec (+ a delta) b f))))))))

;; A corresponding procedure for calculating the integral of some function 'f'.
;; Most definitely not originally written by me, Brandon.
(define D
  (lambda (f)
    (let* ((delta 0.00001)
           (two-delta (* 2 delta)))
      (lambda (x)
        (/(-(f(+ x delta))
            (f (- x delta)))
          two-delta)))))
            
  

;(define two-times-x
;  (lambda (x)
;    (* 2 x)))