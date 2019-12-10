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
      [(const cexp) (apply-k k cexp)]
      [(mult nexp1 nexp2) (value-of-cps nexp1 env (kt_mult-outer-k nexp2 env k))]
      [(sub1 nexp) (value-of-cps nexp env (kt_sub1-k k))]
      [(zero nexp) (value-of-cps nexp env (kt_zero-k k))]
      [(if test conseq alt) (value-of-cps test env (kt_if-k conseq alt env k))]
      [(letcc body) (value-of-cps body (envr_extend-env k env) k)]
      [(throw kexp vexp) (value-of-cps kexp env (kt_throw-k vexp env))]
      [(let exp body) (value-of-cps exp env (kt_let-k body env k))]
      [(var n) (apply-env env n k)]
      [(lambda body) (apply-k k (clos_closure body env))]
      [(app rator rand) (value-of-cps rator env (kt_app-outer-k rand env k))])))

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
           (apply-k k a^)
           (apply-env env^ (sub1 y) k))])))

(define-union clos
  (closure b env))

(define apply-closure
  (λ (c a k)
    (union-case c clos
        [(closure b env)
       (value-of-cps b (envr_extend-env a env) k)])))

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
      [(app-outer-k rand env k) (value-of-cps rand env (kt_app-inner-k v k))]
      [(app-inner-k c k) (apply-closure c v k)]
      [(let-k b env k) (value-of-cps b (envr_extend-env v env) k)]
      [(throw-k v-exp env) (value-of-cps v-exp env v)]
      [(if-k conseq alt env k)
       (if v (value-of-cps conseq env k) (value-of-cps alt env k))]
      [(zero-k k) (apply-k k (zero? v))]
      [(sub1-k k) (apply-k k (sub1 v))]
      [(mult-outer-k x2 env k)
       (value-of-cps x2 env (kt_mult-inner-k v k))]
      [(mult-inner-k x1 k)
       (apply-k k (* x1 v))])))

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
