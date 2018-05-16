#lang racket
(provide lookup)
(provide evaluate)
(provide special-form?)
(provide evaluate-special-form)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Spring 2018, Lab 6
;;
;; Brandon Chavez
;; W012049191
;;
;; Further improvement upon Lab5, now with added evaluation for the special forms "let!
;; Breakfast making functionality is unfortunately facing unexpected delays in development.
;;
;; Rated 'E' for "Even more capable."
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;The list which serves as the environment for our lookup function. Naturally, any program using the procedures
;provided by this file are free to use any environment they deem appropriate.
(define testEnvironment(list
                        (cons 'x 10)
                        (cons '+ +)
                        (cons '- -)
                        (cons '* *)
                        (cons 'y 20)
                        (cons 'cons cons)
                        (cons 'nil '())
                        (cons '= =)
                        (cons 'else #t)))
;Assumes that there is at least one element in the environment given for consideration. 
(define lookup
  (lambda (symbol environment)
    (if(symbol? symbol)
       (let recursiveLookup((symbol symbol) (environment environment))
         (if(equal? symbol (car (car environment)))
            (cdr (car environment))
            (if(null? (cdr environment))
               (error "This symbol does not exist in the provided environment!")
               (recursiveLookup symbol (cdr environment)))))                 
       (error "The provided argument is not a symbol."))))

(define evaluate
  (lambda (expression environment)
    (cond
      ;Return null if the expression itself is null.
      ((null? expression) null)
      ((not (pair? expression)) (if(number? expression)
                                   expression
                                   (if(null? (lookup expression environment))
                                      null
                                      (lookup expression environment))))
      ;Apply the first element (which must be a procedure) to the evaluated remainder of the list.
      ((special-form? expression) (evaluate-special-form expression environment))
      (else (let ((evaluatedExpression (map (lambda (listElement) (evaluate listElement environment)) expression)))
                      (apply (car evaluatedExpression) (cdr evaluatedExpression)))))))
; Determines if the argument provided is a list whose first element is "if" or "cond". Nothing more, nothing less (for now anyways).
(define special-form?
  (lambda (expression)
    (if (pair? expression)
        (if (or (equal? (car expression) 'if) (equal? (car expression) 'cond) (equal? (car expression) 'let))
            #t
            #f)
        #f)))
;Auxiliary function for evaluate which handles cases where the arugment passed in is an "if" or "cond" statement.
;Note that the two are intertwined, as this procedure will make calls to the "normal" evaluate function to evaluate
;statement whihc are *not* special forms.
(define evaluate-special-form
  (lambda (exprs environment)
    (if (equal? (car exprs) 'if)
        ;Only one element of the argument passed in is evaluated, based on the evaluation of the first argument after
        ;the "if" procedure itself (i.e., the condition).
        (if (evaluate (cadr exprs) environment)
            (evaluate (caddr exprs) environment)
            (evaluate (cadddr exprs) environment))
        (if (equal? (car exprs) 'cond)
            ;Recursive evaluation processes for "cond" statements.
            (let cond-processing ((exprs exprs))
              (if (evaluate (caadr exprs) environment)
                  (evaluate (cadadr exprs) environment)
                  (cond-processing (cdr exprs))))
            (if (equal? (car exprs) 'let)
                ;Add each of the symbols to our copy of the environment, preparing to evaluate the following expression in
                ;the context of the 'let' form. Then evaluate the actual expression outlined in the given 'let' form.
                (evaluate (caddr exprs) (append (map (lambda (symbolPair) (symvalToPair symbolPair environment)) (cadr exprs)) environment))
                (error "Symbol does not appear to be a valid special form."))))))
;Helper function built for evaluate-special-form which makes ordered pairs out of the local variables defined in a let statement.
(define symvalToPair
  (lambda (symbolPair environment)
    (cons (car symbolPair) (evaluate (cadr symbolPair) environment))))