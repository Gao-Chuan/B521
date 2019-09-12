#lang racket

(let-values (
    [(x y) (values 1 2)]
    [(t u) (values 3 4)]
)   (+ x u)
)