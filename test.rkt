#lang racket

(define (map-k f xs k)
    (if (empty? xs) 
        (k '())
        (f  (first xs) 
            (λ (v) 
                (map-k  f 
                        (rest xs) 
                        (λ (rest-v) (k (cons v rest-v))))))))
(map-k (λ (x k) (k (+ x 1))) '(1 2 3) (λ (x) x))
