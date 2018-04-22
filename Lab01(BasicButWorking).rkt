#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Spring 2018
;;
;; Lab #1
;;
;; Brandon Chavez
;; W01204919
;; 
;; The purpose of this programs is to thwart the dark and
;; mysterious forces presently beyond our control, which threaten
;; the wellbeing of our Papayas!
;;
;; It accomplishes this by calculating (and returning) Pi to a degree of tolerance
;; specified by the user using a simple summation formula.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide make-pi)

(define make-pi
  (lambda (tolerance)

    ; Either returns the sum, or invokes itself recursively using an improved sum.
    (define evaluateSum
      (lambda (sum numerator denominator)

    ; Define necessary helper procedures.

    ; Judges whether the guess is within the tolerance prescribed to make-pi.
    (define sufficiently-precise?
      (lambda (numerator denominator)
        (< (abs (/ numerator denominator)) tolerance)))

    ; Adds the current numerator and denominator to the sum, returning the result.
    (define improveSum
      (lambda (sum numerator denominator)
        (+ (/ numerator denominator) sum)))
        
    (if (sufficiently-precise? numerator denominator)
        sum
        ; Reevaluate after the sum has been improved, given an updated numerator and denominator.
        (evaluateSum (improveSum sum numerator denominator) (- numerator) (+ 2 denominator)))))
    
    ; Initializes necessary values that are not provided to make-pi.
    (let((sum 0.0)
        (numerator 4.0)
        (denominator 1.0))
      ; Passes control to a procedure that judges whether the sum is ready, or needs to be improved.
      (evaluateSum sum numerator denominator))))


         
  
               