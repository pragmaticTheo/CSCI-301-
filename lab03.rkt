#lang racket

(provide sublists)
(provide distribute)
(provide length-ordered?)
(provide element-ordered?)
(provide subsets)

(require racket/trace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Spring 2018
;;
;; A Series of List Processing Procedures by Brandon Chavez (W01204919)
;; Rated 'M' for 'My Goodness Gracious.'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generates an unordered power set of lists for the list provided to it.
(define sublists
  (lambda (ls)
    (if (null? ls)
        (list ls)
        (let ((A-not (sublists (cdr ls))))
          ;;Append A-1 to A-not to get the complete list of sublists (The power set).
          (append (distribute (car ls) A-not) A-not)))))

;; Handy helper procedure that adds a given element to every list in a given list (A-not).
(define distribute
  ;; Note that A-not is a list of lists. We must add element to every list.
  (lambda (element A-not)
    (if(null? (cdr A-not))
    (cons (cons element (car A-not)) '())
    (cons (cons element (car A-not)) (distribute element (cdr A-not))))))

;Simple procedure for determining whether two lists are sorted by length.
(define length-ordered?
  (lambda (ls1 ls2)
    (if (< (length ls1) (length ls2))
        #t
        (equal? ls1 ls2))))

;; A somewhat more complicated procedure that determines whether two lists are sort by element value.
(define element-ordered?
  (lambda (ls1 ls2)
    (equal? ls1 ls2)
    #t
    (let orderingLoop ((ls1 ls1) (ls2 ls2))
      ;If either of the lists is the empty set...
      (if (or (null? ls1) (null? ls2))
          (cond
            ; They are element-ordered if both sets are null. They are not if ls2 is null and ls1 is not, however.
            ((and (null? ls1) (null? ls2)) #t)
            ((null? ls2) #f))
          ;Otherwise just carry on with processing the list 'normally'.
          (cond
            ((> (car ls1) (car ls2)) #f)
            ((< (car ls1) (car ls2)) #t)
            ((or (null? (cdr ls1)) (null? (cdr ls2))) #t)
            (else (orderingLoop (cdr ls1) (cdr ls2))))))))

;; Combines the functionality of every procedure above to generate a neatly ordered power set for a given list.
(define subsets
  (lambda (ls)
    (sort (sort (sublists ls) element-ordered?) length-ordered?)))
     
        