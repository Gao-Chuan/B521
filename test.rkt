#lang racket

(let-values (
    [(x y) (values 1 2)]
    [(t u) (values 3 4)]
)   (+ x u)
)

(define valof
    (lambda (e env)
        (match e
            [`,y 
                #:when (symbol? y) 
                (env x)]
            [`(lambda (,x) ,body) 
                #:when (symbol? x) 
                (lambda (arg) (valof body (lambda y (if (eqv? y x) arg (env y)))))]
            [`(,rator ,rand)
                ((valof rand env) (valof rand env))])))