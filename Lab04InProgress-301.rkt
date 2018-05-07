#lang racket
(provide lookup)
(provide evaluate)

(define-namespace-anchor mySpace)
(define ns (namespace-anchor->namespace mySpace))

;The list which serves as the environment for our lookup function.
(define testEnvironment(list
                        (cons 'x 10)
                        (cons '+ +)
                        (cons '- -)
                        (cons '* *)
                        (cons 'y 20)
                        (cons 'cons cons)
                        (cons 'nil '())))
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
    (if(not (pair? expression))
       (cond         
         ;If the expression is a number, then return that number.
         ((number? expression) expression)
         ;If the expression is null, or contains a symbol equivalent to null, return null.
         ((null? expression) null)
         ((equal? null (lookup expression environment)) 'null)
         ;Otherwise, the expression must be some kind of symbol, so look up the value of that symbol.
         (else (lookup expression environment)))
       ;If the expression is a list, then use map to evaluate the remaining elements in the expression. Then pair the first element (which is a procedure)
       ;to the resulting list. Finally, apply the procedure to the resulting function using evaluate to obtain the final value.
       ;(eval (cons (lookup (car expression) environment) (map (lambda (listElement) (evaluate listElement environment)) (cdr expression))) ns))))
       (if (equal? (cdr (map(lambda(listElement) (evaluate listElement environment)) (cdr expression))) '(null))
           (car (car (map(lambda(listElement) (evaluate listElement environment)) (cdr expression))))
           (eval (cons (lookup (car expression) environment) (map (lambda (listElement) (evaluate listElement environment)) (cdr expression))) ns)))))

;(evaluate '(+ 2 4 (- 3 3) 4) testEnvironment)
(evaluate '(cons 1 (cons 2 nil)) testEnvironment)

;(evaluate '(cons 1 nil) testEnvironment)

;((number? (symbol? expression)) (lookup expression environment))