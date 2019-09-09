#lang racket

(define + (lambda (n m) (
    cond
    [(zero? m) n]
    [else (add 1 (+ n (sub1 m)))]
)))

(define *
    (lambda (n m)
        (cond
            [(zero? m) 0]
            [else (* n (sub1 m))]
        )    
    )
)