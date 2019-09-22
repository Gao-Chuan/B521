#lang racket

(provide (all-defined-out))

;;; Part 1. Interpreters and Environments

(define value-of 
  (lambda (exp env)
    (match exp
      [`,n 
       #:when (number? n)
       n]
      [`,b
       #:when (boolean? b)
       b]
      [`(zero? ,x)
       (zero? (value-of x env))]
      [`(sub1 ,x)
       (sub1 (value-of x env))]
      [`(* ,a ,b)
       (* (value-of a env) (value-of b env))]
      [`(if ,c ,t, e)
       (if (value-of c env) (value-of t env) (value-of e env))]
      [`(let ([,x ,e]) ,body)
       (value-of body (lambda (y) (if (eqv? y x) (value-of e env) (env y))))]
      [`,x
       #:when (symbol? x)
       (env x)]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       (lambda (arg) (value-of body (lambda (y) (if (eqv? y x) arg (env y)))))]
      [`(,rator ,rand)
       ((value-of rator env) (value-of rand env))]
    )
  )
)


(define empty-env-fn
  (lambda (y) 
    (error 'value-of "unbound variable ~s" y)
  )
)
(define extend-env-fn
  (lambda (env x val)
    (lambda (y)
      (match y
        [x val]
        [_ (env y)]
      )
    )
  )
)
(define apply-env-fn
  (lambda (env y)
    (env y)
  )
)
(define value-of-fn
  (lambda (exp env)
    (apply-env-fn 
    (extend-env-fn )
    exp)
  )
)

 
; (define value-of-ds ...)
; (define empty-env-ds ...)
; (define extend-env-ds ...)
; (define apply-env-ds ...)