#lang racket

(require "parenthec.rkt")

(define-union expr
  (const cexp)
  (var n)
  (if test conseq alt)
  (mult nexp1 nexp2)
  (sub1 nexp)
  (zero nexp)
  (letcc body)
  (throw kexp vexp)
  (let exp body)
  (lambda body)
  (app rator rand))

(define value-of-cps
  (lambda (e env k)
    (union-case e expr
        [(const cexp)
            (let* ([v cexp])
            (apply-k k v))]
        [(mult nexp1 nexp2)
            (let* ([e nexp1]
                [k (kt_mult-outer-k nexp2 env k)])
            (value-of-cps e env k))]
        [(sub1 nexp)
            (let* ([e nexp]
                [k (kt_sub1-k k)])
            (value-of-cps e env k))]
        [(zero nexp)
            (let* ([e nexp]
                [k (kt_zero-k k)])
            (value-of-cps e env k))]
        [(if test conseq alt)
            (let* ([e test]
                [k (kt_if-k conseq alt env k)])
            (value-of-cps e env k))]
        [(letcc body)
            (let* ([e body]
                [env (envr_extend-env k env)])
            (value-of-cps e env k))]
        [(throw kexp vexp)
            (let* ([e kexp]
                [k (kt_throw-k vexp env)])
            (value-of-cps e env k))]
        [(let exp body)
            (let* ([e exp]
                [k (kt_let-k body env k)])
            (value-of-cps e env k))]
        [(var n)
            (let* ([y n])
            (apply-env env y k))]
        [(lambda body)
            (let* ([v (clos_closure body env)])
            (apply-k k v))]
        [(app rator rand)
            (let* ([e rator]
                [k (kt_app-outer-k rand env k)])
            (value-of-cps e env k))])))

(define-union envr
  (empty-env)
  (extend-env a^ env^))

(define empty-env
  (λ () (envr_empty-env)))

(define apply-env
  (λ (env y k)
    (union-case env envr
      [(empty-env) (error "unbound variable")]
      [(extend-env a^ env^)
       (if (zero? y)
           (let* ([v a^])
             (apply-k k v)) 
           (let* ([env env^]
                  [y (sub1 y)])
             (apply-env env y k)))])))

(define-union clos
  (closure b env))

(define apply-closure
  (λ (c a k)
    (union-case c clos
                [(closure b env)
                 (let* ([e b]
                        [env (envr_extend-env a env)])
                   (value-of-cps e env k))])))

(define-union kt
  (empty-k)
  (app-outer-k rand env k)
  (app-inner-k c k)
  (let-k b env k)
  (throw-k v-exp env)
  (if-k conseq alt env k)
  (zero-k k)
  (sub1-k k)
  (mult-outer-k x2 env k)
  (mult-inner-k x1 k))

(define empty-k
  (λ ()
    (kt_empty-k)))

(define apply-k
  (λ (k v)
    (union-case k kt
      [(empty-k) v]
      [(app-outer-k rand env k)
       (let* ([e rand]
            [k (kt_app-inner-k v k)])
         (value-of-cps e env k))]
      [(app-inner-k c k)
       (let* ([a v])
         (apply-closure c a k))]
      [(let-k b env k)
       (let* ([e b]
              [env (envr_extend-env v env)])
         (value-of-cps e env k))]
      [(throw-k v-exp env)
       (let* ([e v-exp]
              [k v])
         (value-of-cps e env k))]
      [(if-k conseq alt env k)
       (if v
           (let* ([e conseq])
             (value-of-cps e env k))
           (let* ([e alt])
             (value-of-cps e env k)))]
      [(zero-k k)
       (let* ([v (zero? v)])
         (apply-k k v))]
      [(sub1-k k)
       (let* ([v (sub1 v)])
         (apply-k k v))]
      [(mult-outer-k x2 env k)
       (let* ([e x2]
              [k (kt_mult-inner-k v k)])
         (value-of-cps e env k))]
      [(mult-inner-k x1 k)
       (let* ([v (* x1 v)])
         (apply-k k v))])))

(define main
  (lambda ()
    (value-of-cps
     (expr_let
      (expr_lambda
       (expr_lambda
        (expr_if
         (expr_zero (expr_var 0))
         (expr_const 1)
         (expr_mult (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_sub1 (expr_var 0)))))))
      (expr_mult
       (expr_letcc
        (expr_app
         (expr_app (expr_var 1) (expr_var 1))
         (expr_throw (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_const 4)))))
       (expr_const 5)))
     (empty-env)
     (empty-k))))

(main)
