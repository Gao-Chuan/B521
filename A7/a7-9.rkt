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
    (lambda (v)
      v)))

(define apply-env
  (lambda (env y k^)
    (match env
      [`(extend ,v ,env^)
        (if (zero? y)
            (apply-k k^ v)
            (apply-env env^ (sub1 y) k^))]
      ['(unbound identifier) (error 'value-of "unbound identifier")])))

(define apply-k
  (lambda (k v)
    (k v)))

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

(define mult-construct^
  (lambda (v^ k^)
    (lambda (v)
      (apply-k k^ (* v v^)))))

(define mult-construct
  (lambda (x2^ env^ k^)
    (lambda (v)
      (value-of-cps x2^ env^ (mult-construct^ v k^)))))

(define sub1-construct
  (lambda (k^)
    (lambda (v)
      (apply-k k^ (sub1 v)))))

(define zero-construct
  (lambda (k^)
    (lambda (v)
      (apply-k k^ (zero? v)))))

(define if-construct
  (lambda (conseq^ alt^ env^ k^)
    (lambda (v)
      (if v
          (value-of-cps conseq^ env^ k^)
          (value-of-cps alt^ env^ k^)))))

(define throw-construct
  (lambda (v-exp^ env^)
    (lambda (k)
      (value-of-cps v-exp^ env^ k))))

(define let-construct
  (lambda (body^ env^ k^)
    (lambda (e)
      (value-of-cps body^ (extend-env env^ e) k^))))

(define app-construct^
  (lambda (f^ k^)
    (lambda (v)
      (apply-closure f^ v k^))))

(define app-construct
  (lambda (rand^ env^ k^)
    (lambda (f)
      (value-of-cps rand^ env^ (app-construct^ f k^)))))

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
(define simple-test
  (λ(v1 v2)
    (if (eqv? v1 v2)
        (printf "success~n")
        (printf "fail~n"))))



(simple-test (value-of-cps '(const 5) (empty-env) (empty-k)) 5)
(simple-test (value-of-cps '(mult (const 5) (const 5)) (empty-env) (empty-k)) 25)
(simple-test (value-of-cps '(zero (const 5)) (empty-env) (empty-k)) #f)
(simple-test (value-of-cps '(sub1 (const 5)) (empty-env) (empty-k)) 4)
(simple-test (value-of-cps '(sub1 (sub1 (const 5))) (empty-env) (empty-k)) 3)
(simple-test (value-of-cps '(zero (sub1 (const 6))) (empty-env) (empty-k))  #f)
(simple-test (value-of-cps '(if (zero (const 5)) (const 3) (mult (const 2) (const 2))) (empty-env) (empty-k)) 4)
(simple-test (value-of-cps '(if (zero (const 0)) (mult (const 2) (const 2)) (const 3)) (empty-env) (empty-k)) 4)
(simple-test (value-of-cps '(app (lambda (const 5)) (const 6)) (empty-env) (empty-k))  5)
(simple-test (value-of-cps '(app (lambda (var 0)) (const 5)) (empty-env) (empty-k)) 5)
(simple-test (value-of-cps '(app (app (lambda (lambda (var 1))) (const 6)) (const 5)) (empty-env) (empty-k)) 6)
(simple-test (value-of-cps '(app (lambda (app (lambda (var 1)) (const 6))) (const 5)) (empty-env) (empty-k)) 5)
(simple-test (value-of-cps '(app (lambda (if (zero (var 0)) (const 4) (const 5))) (const 3)) (empty-env) (empty-k))5)
(simple-test (value-of-cps '(let (const 6) (const 4)) (empty-env) (empty-k))4)
(simple-test (value-of-cps '(let (const 5) (var 0)) (empty-env) (empty-k))5)
(simple-test (value-of-cps '(mult (const 5) (let (const 5) (var 0))) (empty-env) (empty-k)) 25)
(simple-test (value-of-cps '(app (if (zero (const 4)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 5)
(simple-test (value-of-cps '(app (if (zero (const 0)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 3)
(simple-test (value-of-cps '(letcc (const 5)) (empty-env) (empty-k)) 5)
(simple-test (value-of-cps '(letcc (throw (var 0) (const 5))) (empty-env) (empty-k)) 5)
(simple-test (value-of-cps '(letcc (throw (var 0) (mult (const 5) (const 5)))) (empty-env) (empty-k)) 25)
(simple-test (value-of-cps '(letcc (throw (app (lambda (var 0)) (var 0)) (mult (const 5) (const 5)))) (empty-env) (empty-k))  25)
(simple-test (value-of-cps '(letcc (sub1 (throw (var 0) (const 5)))) (empty-env) (empty-k)) 5)
(simple-test (value-of-cps '(letcc (throw (throw (var 0) (const 5)) (const 6))) (empty-env) (empty-k))  5)
(simple-test (value-of-cps '(letcc (throw (const 5) (throw (var 0) (const 5)))) (empty-env) (empty-k)) 5)
(simple-test (value-of-cps '(mult (const 3) (letcc (throw (const 5) (throw (var 0) (const 5))))) (empty-env) (empty-k))  15)
(simple-test (value-of-cps '(if (zero (const 5)) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))) (const 4))
              (empty-env)
              (empty-k)) 4)
         
(simple-test (value-of-cps '(if (zero (const 0)) (const 4) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))))
              (empty-env)
              (empty-k)) 4)
         
(simple-test (value-of-cps '(app (lambda (app (app (var 0) (var 0)) (const 2)))
                    (lambda
                        (lambda 
                            (if (zero (var 0))  
                                (const 1)
                                (app (app (var 1) (var 1)) (sub1 (var 0)))))))
              (empty-env)
              (empty-k)) 1)