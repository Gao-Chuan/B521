#lang racket

;; Part I. let/cc
(define last-non-zero
  (lambda (ls)
    (let/cc k
      (letrec
	((last-non-zero
	   (lambda (ls)
	     (cond
	       ;; fill in lines here
           [(null? ls) '()]
           [(eqv? (car ls) '0) (k (last-non-zero (cdr ls)))]
           [else (cons (car ls) (last-non-zero (cdr ls)))]
  	       ))))
	(last-non-zero ls)))))

; (last-non-zero '(0))
; (last-non-zero '(1 2 3 0 4 5))
; (last-non-zero '(1 0 2 3 0 4 5))
; (last-non-zero '(1 2 3 4 5))

;; Part II. lex
(define lex
  (lambda (exp acc)
    (match exp
      [`,n
        #:when (number? n)
        `(const ,n)]
      [`(zero? ,v)
        `(zero? ,(lex v acc))]
      [`(* ,a ,b)
        `(mult ,(lex a acc) ,(lex b acc))]
      [`(sub1 ,n)
        `(sub1 ,(lex n acc))]
      [`(if ,c ,t ,e)
        `(if ,(lex c acc) ,(lex t acc) ,(lex e acc))]
      [`(let ((,arg ,v)) ,e)
        `(let ,(lex v acc) ,(lex e (cons `(,arg . 0) (map (lambda (p) `(,(car p) . ,(+ 1 (cdr p)))) acc))))]
      [`(let/cc ,k ,body)
        `(letcc ,(lex body (cons `(,k . 0) (map (lambda (p) `(,(car p) . ,(+ 1 (cdr p)))) acc))))]
      [`(throw ,k ,v)
        `(throw ,(lex k acc) ,(lex v acc))]
      [`,y
        #:when (symbol? y)
        `(var ,(cdr (assv y acc)))]
      [`(lambda (,x) ,body)
        #:when (symbol? x)
        `(lambda ,(lex body (cons `(,x . 0) (map (lambda (p) `(,(car p) . ,(+ 1 (cdr p)))) acc))))]
      [`(,rator ,rand)
        `(app ,(lex rator acc) ,(lex rand acc))])))

; (lex '((lambda (x) x) 5)  '())
; (lex '(lambda (!)
;   	  (lambda (n)
;   	    (if (zero? n) 1 (* n (! (sub1 n))))))
; 	'())

; (lex '(let ((! (lambda (!)
;   		   (lambda (n)
;   		     (if (zero? n) 1 (* n ((! !) (sub1 n))))))))
;           ((! !) 5))
;        '())

; (lex '(lambda (x) (let ((a 2)) (x a)))
;       '())

;; Part III. The interpreter 
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
          ((lambda (a) (value-of-cps exp (extend-env env a) k^)) v)])))

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

(define value-of-cps
  (lambda (expr env k)
    (match expr
      [`(const ,expr) (apply-k k expr)]
      [`(mult ,x1 ,x2) (value-of-cps x1 env (mult-construct x2 env k))]
      [`(sub1 ,x) (value-of-cps x env (sub1-construct k))]
      [`(zero ,x) (value-of-cps x env (zero-construct k))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env (if-construct conseq alt env k))]
      [`(letcc ,body) (value-of-cps body (extend-env env k) k)]
      [`(throw ,k-exp ,v-exp) (value-of-cps k-exp env (throw-construct v-exp env))]
      [`(let ,e ,body) (value-of-cps e env (let-construct body env k))]
      [`(var ,y) (apply-env env y k)]
      [`(lambda ,body) (apply-k k (make-closure body env))]
      [`(app ,rator ,rand) (value-of-cps rator env (app-construct rand env k))])))

;; Brainteaser
(define-syntax cons$
  (syntax-rules ()
    ((cons$ x y) (cons x (delay y)))))
 
(define car$ car)
 
(define cdr$
  (lambda ($) (force (cdr $))))

(define inf-1s (cons$ 1 inf-1s))

(define take$
  (lambda (n $)
    (cond
      ((zero? n) '())
      (else (cons (car$ $) (take$ (sub1 n) (cdr$ $)))))))

(define tri
  (lambda (trib$)
    (cons$ (+ (car$ trib$) (car$ (cdr$ trib$)) (car$ (cdr$ (cdr$ trib$)))) (tri (cdr$ trib$)))))

(define trib$
  (cons$ 0 (cons$ 1 (cons$ 1 (tri trib$)))))

(take$ 9 trib$)