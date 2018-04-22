#lang racket

;(define I
;  (lambda (f)
 ;   (define Idef
;      (lambda (a b)
;        (let* ((n 100000)
;              (delta (/(- b a) n)))
;          (if (>= a b)
;            0
;            (+ (* (f a) delta) (Idef (+ a delta) b))))))

(define Idef
      (lambda (a b f)
        (let* ((n 100000)
              (delta (/(- b a) n)))
          (Idef-rec a b f delta))))

          (define Idef-rec
            (lambda (a b f delta)
              (if (>= a b)
              0
              (+ (* (f a) delta) (Idef-rec (+ a delta) b f delta)))))
(define two-times-x
  (lambda (x)
    (* 2 x)))