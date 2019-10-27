#lang racket

(define fact/k
    (lambda (n k)
        (cond
            [(zero? n) (let/cc f (k f))]
            [else (* n (fact/k (sub1 n) k))]
        )
    )
)

(define five (let/cc k (fact/k 5 k)))

(five 1)

five