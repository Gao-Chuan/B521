#lang racket

(require "parenthec.rkt")

;; ***** STEP 3 *****
;; union representation for env

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

;; the RI interpreter
(define value-of-cps
  (lambda (e env k)  ;; since env is a tagged list we remove the `-cps` suffix
    (union-case e expr
      [(const cexp) (apply-k k cexp)]
      [(mult nexp1 nexp2) (value-of-cps nexp1 env (make-mult-outer-k nexp2 env k))]
      [(sub1 nexp) (value-of-cps nexp env (make-sub1-k k))]
      [(zero nexp) (value-of-cps nexp env (make-zero-k k))]
      [(if test conseq alt) (value-of-cps test env (make-if-k conseq alt env k))]
      [(letcc body) (value-of-cps body (envr_extend-env k env) k)]
      [(throw kexp vexp) (value-of-cps kexp env (make-throw-k vexp env))]
      [(let exp body) (value-of-cps exp env (make-let-k body env k))]
      [(var n) (apply-env env n k)]
      [(lambda body) (apply-k k (clos_closure body env))]
      [(app rator rand) (value-of-cps rator env (make-app-outer-k rand env k))])))

;; union representation of env (RI)
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
           (apply-k k a^)    ;; pass to k
           (apply-env env^ (sub1 y) k))])))

;; union representation of closures (RI)
(define-union clos
  (closure b env))

(define apply-closure
  (λ (c a k)
    (union-case c clos
      [(closure b env)
       (value-of-cps b (envr_extend-env a env) k)])))

;; RI representation of continuations
;; empty-k
(define empty-k
  (λ ()
    '(empty-k)))

;; make-k
(define make-app-outer-k
  (λ (rand env k)
    `(app-outer-k ,rand ,env ,k)))
(define make-app-inner-k
  (λ (c k)
    `(app-inner-k ,c ,k)))
(define make-let-k
  (λ (b env k)
    `(let-k ,b ,env ,k)))
(define make-throw-k
  (λ (v-exp env)
    `(throw-k ,v-exp ,env)))
(define make-if-k
  (λ (conseq alt env k)
    `(if-k ,conseq ,alt ,env ,k)))
(define make-zero-k
  (λ (k) `(zero-k ,k)))
(define make-sub1-k
  (λ (k) `(sub1-k ,k)))
(define make-mult-outer-k
  (λ (x2 env k)
    `(mult-outer-k ,x2 ,env ,k)))
(define make-mult-inner-k
  (λ (x1 k)
    `(mult-inner-k ,x1 ,k)))

;; apply-k
(define apply-k
  (λ (k v)
    (match k
      [`(empty-k) v]
      [`(app-outer-k ,rand ,env ,k) (value-of-cps rand env (make-app-inner-k v k))]
      [`(app-inner-k ,c ,k) (apply-closure c v k)]
      [`(let-k ,b ,env ,k) (value-of-cps b (envr_extend-env v env) k)]
      [`(throw-k ,v-exp ,env) (value-of-cps v-exp env v)]
      [`(if-k ,conseq ,alt ,env ,k)
       (if v (value-of-cps conseq env k) (value-of-cps alt env k))]
      [`(zero-k ,k) (apply-k k (zero? v))]
      [`(sub1-k ,k) (apply-k k (sub1 v))]
      [`(mult-outer-k ,x ,env ,k)
       (value-of-cps x env (make-mult-inner-k v k))]
      [`(mult-inner-k ,x ,k)
       (apply-k k (* x v))])))

;; main
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

;; invoke of main
(main)
