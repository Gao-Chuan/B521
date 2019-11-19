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
      [(mult nexp1 nexp2) (value-of-cps nexp1 env (mult-construct nexp2 env k))]
      [(sub1 nexp) (value-of-cps nexp env (sub1-construct k))]
      [(zero nexp) (value-of-cps nexp env (zero-construct k))]
      [(if test conseq alt) (value-of-cps test env (if-construct conseq alt env k))]
      [(letcc body) (value-of-cps body (extend-env env k) k)]
      [(throw kexp vexp) (value-of-cps kexp env (throw-construct vexp env))]
      [(let exp body) (value-of-cps exp env (let-construct body env k))]
      [(var n) (apply-env env n k)]
      [(lambda body) (apply-k k (make-closure body env))]
      [(app rator rand) (value-of-cps rator env (app-construct rand env k))])))

(define empty-env
  (lambda ()
    '(unbound identifier)))
 
(define empty-k
  (lambda ()
    '(empty-k)))

(define apply-env
  (lambda (env y k^)
    (match env
      [`(extend ,v ,env^)
        (if (zero? y)
            (apply-k k^ v)
            (apply-env env^ (sub1 y) k^))]
      ['(unbound identifier) (error 'value-of "unbound identifier")])))

(define extend-env
  (lambda (env^ val^)
      (list 'extend val^ env^)))

(define make-closure
  (lambda (exp env)
    `(exe ,exp ,env)))
  
  (define apply-closure
    (lambda (f v k^)
      (match f
        [`(exe ,exp ,env)
          (value-of-cps exp (extend-env env v) k^)])))

(define apply-k
  (lambda (k v)
    (match k
      [`(mult-construct ,x2^ ,env^ ,k^) (value-of-cps x2^ env^ (mult-construct^ v k^))]
      [`(mult-construct^ ,v^ ,k^) (apply-k k^ (* v v^))]
      [`(sub1-construct ,k^) (apply-k k^ (sub1 v))]
      [`(zero-construct ,k^) (apply-k k^ (zero? v))]
      [`(if-construct ,conseq^ ,alt^ ,env^ ,k^) (if v
                                                    (value-of-cps conseq^ env^ k^)
                                                    (value-of-cps alt^ env^ k^))]
      [`(throw-construct ,v-exp^ ,env^) (value-of-cps v-exp^ env^ v)]
      [`(let-construct ,body^ ,env^ ,k^) (value-of-cps body^ (extend-env env^ v) k^)]
      [`(app-construct^ ,f^ ,k^) (apply-closure f^ v k^)]
      [`(app-construct ,rand^ ,env^ ,k^) (value-of-cps rand^ env^ (app-construct^ v k^))]
      ['(empty-k) v]
      )))

(define mult-construct^
  (lambda (v^ k^)
    `(mult-construct^ ,v^ ,k^)))

(define mult-construct
  (lambda (x2^ env^ k^)
    `(mult-construct ,x2^ ,env^ ,k^)))

(define sub1-construct
  (lambda (k^)
    `(sub1-construct ,k^)))

(define zero-construct
  (lambda (k^)
    `(zero-construct ,k^)))

(define if-construct
  (lambda (conseq^ alt^ env^ k^)
    `(if-construct ,conseq^ ,alt^ ,env^ ,k^)))

(define throw-construct
  (lambda (v-exp^ env^)
    `(throw-construct ,v-exp^ ,env^)))

(define let-construct
  (lambda (body^ env^ k^)
    `(let-construct ,body^ ,env^ ,k^)))

(define app-construct^
  (lambda (f^ k^)
    `(app-construct^ ,f^ ,k^)))

(define app-construct
  (lambda (rand^ env^ k^)
    `(app-construct ,rand^ ,env^ ,k^)))

;; (let ((f (lambda (f)
;;   	      (lambda (n)
;; 	        (if (zero? n) 
;; 		    1
;; 	            (* n ((f f) (sub1 n))))))))
;;   (* (letcc k ((f f) (throw k ((f f) 4)))) 5))
 
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