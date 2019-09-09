#lang racket
(define collatz
    (letrec
        ((odd-case
            (lambda (recur)
                (lambda (x)
                    (cond 
                        ((and (positive? x) (odd? x)) (collatz (add1 (* x 3)))) 
                        (else (recur x))))))
        (even-case
            (lambda (recur)
                (lambda (x)
                    (cond 
                        ((and (positive? x) (even? x)) (collatz (/ x 2))) 
                        (else (recur x))))))
        (one-case
            (lambda (recur)
                (lambda (x)
                    (cond
                        ((zero? (sub1 x)) 1)
                        (else (recur x))))))
        (base
            (lambda (x)
                (error 'error "Invalid value ~s~n" x))))
        (one-case (even-case (odd-case base)));; this should be a single line, without lambda
    ))
(collatz 3909089089)