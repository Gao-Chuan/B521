#lang racket

(require racket/trace)

(define ≤ <=)
(define sub2
  (λ (n)
    (sub1 (sub1 n))))

(define fib
  (λ (n)
    (cond
      [(≤ n 1) 1]
      [else (+ (fib (sub1 n)) (fib (sub2 n)))])))

(define fib-cps
  (λ (n k)
      (cond
        [(≤ n 1) (k 1)]
        [else (fib-cps (sub1 n) 
                       (λ (fib-sub1)
                         (fib-cps (sub2 n) 
                                  (λ (fib-sub2)
                                    (k (+ fib-sub1 fib-sub2))))))])))

(trace fib-cps)

(fib-cps 10
         (λ (v) v))