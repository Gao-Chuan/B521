;;;#lang racket

;;(require "parenthec.rkt")

(define-registers e* env* k* y* c* a* v*)
(define-program-counter pc*)

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

(define-label value-of-cps
  (union-case e* expr
    [(const cexp)
      (begin
        (set! v* cexp)
        (set! pc* apply-k))]
    [(mult nexp1 nexp2)
      (begin
        (set! e* nexp1)
        (set! k* (kt_mult-outer-k nexp2 env* k*))
        (set! pc* value-of-cps))]
    [(sub1 nexp)
      (begin
        (set! e* nexp)
        (set! k* (kt_sub1-k k*))
        (set! pc* value-of-cps))]
    [(zero nexp)
      (begin
        (set! e* nexp)
        (set! k* (kt_zero-k k*))
        (set! pc* value-of-cps))]
    [(if test conseq alt)
      (begin
        (set! e* test)
        (set! k* (kt_if-k conseq alt env* k*))
        (set! pc* value-of-cps))]
    [(letcc body)
      (begin
        (set! e* body)
        (set! env* (envr_extend-env k* env*))
        (set! pc* value-of-cps))]
    [(throw kexp vexp)
      (begin
        (set! e* kexp)
        (set! k* (kt_throw-k vexp env*))
        (set! pc* value-of-cps))]
    [(let exp body)
      (begin
        (set! e* exp)
        (set! k* (kt_let-k body env* k*))
        (set! pc* value-of-cps))]
    [(var n)
      (begin
        (set! y* n)
        (set! pc* apply-env))]
    [(lambda body)
      (begin
        (set! v* (clos_closure body env*))
        (set! pc* apply-k))]
    [(app rator rand)
      (begin
        (set! e* rator)
        (set! k* (kt_app-outer-k rand env* k*))
        (set! pc* value-of-cps))]))

(define-union envr
  (empty-env)
  (extend-env a^ env^))

(define-label apply-env
  (union-case env* envr
        [(empty-env)
        (error 'empty-env "unbound variable")]
        [(extend-env a^ env^)
        (if (zero? y*)
          (begin
            (set! v* a^)
            (set! pc* apply-k))
          (begin
            (set! env* env^)
            (set! y* (sub1 y*))
            (set! pc* apply-env)))]))

(define-union clos
  (closure b env))

(define-label apply-closure
  (union-case c* clos
    [(closure b env)
      (begin
        (set! e* b)
        (set! env* (envr_extend-env a* env))
        (set! pc* value-of-cps))]))

(define-union kt
  (empty-k jumpout)
  (app-outer-k rand env k)
  (app-inner-k c k)
  (let-k b env k)
  (throw-k v-exp env)
  (if-k conseq alt env k)
  (zero-k k)
  (sub1-k k)
  (mult-outer-k x2 env k)
  (mult-inner-k x1 k))

(define-label apply-k
  (union-case k* kt
    [(empty-k jumpout) (dismount-trampoline jumpout)]
    [(app-outer-k rand env k)
      (begin
        (set! e* rand)
        (set! env* env)
        (set! k* (kt_app-inner-k v* k))
        (set! pc* value-of-cps))]
    [(app-inner-k c k)
      (begin
        (set! c* c)
        (set! a* v*)
        (set! k* k)
        (set! pc* apply-closure))]
    [(let-k b env k)
      (begin
        (set! e* b)
        (set! env* (envr_extend-env v* env))
        (set! k* k)
        (set! pc* value-of-cps))]
    [(throw-k v-exp env)
      (begin
        (set! e* v-exp)
        (set! env* env)
        (set! k* v*)
        (set! pc* value-of-cps))]
    [(if-k conseq alt env k)
      (if v*
          (begin
            (set! e* conseq)
            (set! env* env)
            (set! k* k)
            (set! pc* value-of-cps))
          (begin
            (set! e* alt)
            (set! env* env)
            (set! k* k)
            (set! pc* value-of-cps)))]
    [(zero-k k)
      (begin
        (set! k* k)
        (set! v* (zero? v*))
        (set! pc* apply-k))]
    [(sub1-k k)
      (begin
        (set! k* k)
        (set! v* (sub1 v*))
        (set! pc* apply-k))]
    [(mult-outer-k x2 env k)
      (begin
        (set! e* x2)
        (set! env* env)
        (set! k* (kt_mult-inner-k v* k))
        (set! pc* value-of-cps))]
    [(mult-inner-k x1 k)
      (begin
        (set! k* k)
        (set! v* (* x1 v*))
        (set! pc* apply-k))]))

(define-label main
  (begin
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
    (set! pc* value-of-cps)
    (mount-trampoline kt_empty-k k* pc*)
    (printf "Fact 5: ~s\n" v*)))

;;; I have talked with tianyu during A9.