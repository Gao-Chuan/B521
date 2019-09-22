#lang racket

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

(define e
  (lambda (y) 
    (error 'value-of "unbound variable ~s" y)
  )
)

(extend-env-fn e `,n #:when (number? n) )