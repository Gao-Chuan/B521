#lang racket

(require "parenthec.rkt")

(define-registers e* env* k* y* c* a* v*)

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
  (lambda () 
    (union-case e* expr
        [(const cexp)
            (begin
              (set! v* cexp)
              (apply-k))]
        [(mult nexp1 nexp2)
            (begin
              (set! e* nexp1)
              (set! k* (kt_mult-outer-k nexp2 env* k*))
              (value-of-cps))]
        [(sub1 nexp)
            (begin
              (set! e* nexp)
              (set! k* (kt_sub1-k k*))
              (value-of-cps))]
        [(zero nexp)
            (begin
              (set! e* nexp)
              (set! k* (kt_zero-k k*))
              (value-of-cps))]
        [(if test conseq alt)
            (begin
              (set! e* test)
              (set! k* (kt_if-k conseq alt env* k*))
              (value-of-cps))]
        [(letcc body)
            (begin
              (set! e* body)
              (set! env* (envr_extend-env k* env*))
              (value-of-cps))]
        [(throw kexp vexp)
            (begin
              (set! e* kexp)
              (set! k* (kt_throw-k vexp env*))
              (value-of-cps))]
        [(let exp body)
            (begin
              (set! e* exp)
              (set! k* (kt_let-k body env* k*))
              (value-of-cps))]
        [(var n)
            (begin
              (set! y* n)
              (apply-env))]
        [(lambda body)
            (begin
              (set! v* (clos_closure body env*))
              (apply-k))]
        [(app rator rand)
            (begin
              (set! e* rator)
              (set! k* (kt_app-outer-k rand env* k*))
              (value-of-cps))])))

(define-union envr
  (empty-env)
  (extend-env a^ env^))

(define empty-env
  (λ () (envr_empty-env)))

(define apply-env
  (λ ()
    (union-case env* envr
                [(empty-env)
                 (error "unbound variable")]
                [(extend-env a^ env^)
                 (if (zero? y*)
                     (begin
                       (set! v* a^)
                       (apply-k))
                     (begin
                       (set! env* env^)
                       (set! y* (sub1 y*))
                       (apply-env)))])))

(define-union clos
  (closure b env))

(define apply-closure
  (λ ()
    (union-case c* clos
                [(closure b env)
                 (begin
                   (set! e* b)
                   (set! env* (envr_extend-env a* env))
                   (value-of-cps))])))

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
  (λ ()
    (union-case k* kt
                [(empty-k) v*]
                [(app-outer-k rand env k)
                 (begin
                   (set! e* rand)
                   (set! env* env)
                   (set! k* (kt_app-inner-k v* k))
                   (value-of-cps))]
                [(app-inner-k c k)
                 (begin
                   (set! c* c)
                   (set! a* v*)
                   (set! k* k)
                   (apply-closure))]
                [(let-k b env k)
                 (begin
                   (set! e* b)
                   (set! env* (envr_extend-env v* env))
                   (set! k* k)
                   (value-of-cps))]
                [(throw-k v-exp env)
                 (begin
                   (set! e* v-exp)
                   (set! env* env)
                   (set! k* v*)
                   (value-of-cps))]
                [(if-k conseq alt env k)
                 (if v*
                     (begin
                       (set! e* conseq)
                       (set! env* env)
                       (set! k* k)
                       (value-of-cps))
                     (begin
                       (set! e* alt)
                       (set! env* env)
                       (set! k* k)
                       (value-of-cps)))]
                [(zero-k k)
                 (begin
                   (set! k* k)
                   (set! v* (zero? v*))
                   (apply-k))]
                [(sub1-k k)
                 (begin
                   (set! k* k)
                   (set! v* (sub1 v*))
                   (apply-k))]
                [(mult-outer-k x2 env k)
                 (begin
                   (set! e* x2)
                   (set! env* env)
                   (set! k* (kt_mult-inner-k v* k))
                   (value-of-cps))]
                [(mult-inner-k x1 k)
                 (begin
                   (set! k* k)
                   (set! v* (* x1 v*))
                   (apply-k))])))

(define main
  (λ ()
    (begin
      (set! k* (kt_empty-k))
      (set! env* (envr_empty-env))
      (set! e* (expr_let
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
                 (expr_const 5))))
      (value-of-cps))))

(main)
