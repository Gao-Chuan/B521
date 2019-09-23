#lang racket

(provide (all-defined-out))

;;; Part 1. Interpreters and Environments

(define value-of^
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
      [`(if ,c ,t ,e)
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
  (lambda ()
    (lambda (y) 
      (error 'value-of "unbound variable ~s" y)
    )
  )
)

(define apply-env-fn
  (lambda (env y)
    (env y)
  )
)

(define extend-env-fn
  (lambda (env x val)
    (lambda (y)
      (if (eqv? x y)
        val
        (apply-env-fn env y))
    )
  )
)

(define value-of-fn
  (lambda (exp env)
    (match exp
      [`,n 
       #:when (number? n)
       n]
      [`,b
       #:when (boolean? b)
       b]
      [`(zero? ,x)
       (zero? (value-of-fn x env))]
      [`(sub1 ,x)
       (sub1 (value-of-fn x env))]
      [`(* ,a ,b)
       (* (value-of-fn a env) (value-of-fn b env))]
      [`(if ,c ,t ,e)
       (if (value-of-fn c env) (value-of-fn t env) (value-of-fn e env))]
      [`(let ([,x ,e]) ,body)
       (value-of-fn body (extend-env-fn env x (value-of-fn e env)))]
      [`,x
       #:when (symbol? x)
       (apply-env-fn env x)]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       (lambda (arg) (value-of-fn body (extend-env-fn env x arg)))]
      [`(,rator ,rand)
       ((value-of-fn rator env) (value-of-fn rand env))]
    )
  )
)

(define empty-env-ds
  (lambda ()
    '()
  )
)

(define extend-env-ds
  (lambda (env x val)
    `((,x . ,val) . ,env)
  )
)

(define apply-env-ds
  (lambda (env y)
    (match env
      [`() (error "no val for" y)]
      [`((,x . ,val) . ,env^)
        (if (eqv? x y)
            val
            (apply-env-ds env^ y))]
      ['env (error (display y))]
    )
  )
)

(define value-of-ds
  (lambda (exp env)
    (match exp
      [`,n 
       #:when (number? n)
       n]
      [`,b
       #:when (boolean? b)
       b]
      [`(zero? ,x)
       (zero? (value-of-ds x env))]
      [`(sub1 ,x)
       (sub1 (value-of-ds x env))]
      [`(* ,a ,b)
       (* (value-of-ds a env) (value-of-ds b env))]
      [`(if ,c ,t ,e)
       (if (value-of-ds c env) (value-of-ds t env) (value-of-ds e env))]
      [`(let ([,x ,e]) ,body)
       (value-of-ds body (extend-env-ds env x (value-of-ds e env)))]
      [`,x
       #:when (symbol? x)
       (apply-env-ds env x)]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       (lambda (arg) (value-of-ds body (extend-env-ds env x arg)))]
      [`(,rator ,rand)
       ((value-of-ds rator env) (value-of-ds rand env))]
    )
  )
)

;;; Part 2 ''fo-eulav''
(define empty-env
  (lambda ()
    (lambda (y) 
      (error 'value-of "unbound variable ~s" y)
    )
  )
)

(define fo-eulav
  (lambda (exp env)
    (match exp
      [`,n 
       #:when (number? n)
       n]
      [`,b
       #:when (boolean? b)
       b]
      [`(,x ?orez)
       (zero? (fo-eulav x env))]
      [`(,x 1bus)
       (sub1 (fo-eulav x env))]
      [`(,a ,b *)
       (* (fo-eulav a env) (fo-eulav b env))]
      [`(,e ,t ,c fi)
       (if (fo-eulav c env) (fo-eulav t env) (fo-eulav e env))]
      [`(,body ([,e ,x]) tel)
       (fo-eulav body (lambda (y) (if (eqv? y x) (fo-eulav e env) (env y))))]
      [`,x
       #:when (symbol? x)
       (env x)]
      [`(,body (,x) adbmal)
       #:when (symbol? x)
       (lambda (arg) (fo-eulav body (lambda (y) (if (eqv? y x) arg (env y)))))]
      [`(,rator ,rand)
       ((fo-eulav rand env) (fo-eulav rator env))]
    )
  )
)

;;; Brainteasers value-of

;;; Problem 5. value-of
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
      [`(if ,c ,t ,e)
       (if (value-of c env) (value-of t env) (value-of e env))]
      [`(let ([,x ,e]) ,body)
          (let ([b (box (value-of e env))])
            (value-of body (lambda (y) (if (eqv? y x) b (env y)))))]
      ;  WHY THIS CAN NOT WORK????????
      ;  (value-of body (lambda (y) (if (eqv? y x) (box (value-of e env)) (env y))))]
      [`(begin2 ,s1 ,s2)
       (begin (value-of s1 env) (value-of s2 env))]
      [`(set! ,x ,v)
       (set-box! (env x) (value-of v env))]
      [`,x
       #:when (symbol? x)
       (unbox (env x))]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
      ;  WHY THIS CAN NOT WORK????????
      ;  (lambda (arg) (value-of body (lambda (y) (if (eqv? y x) (box arg) (env y)))))]
      (lambda (arg)
        (let ([t (box arg)])
          (value-of body (lambda (y)
            (if (eqv? y x)
                t
                (env y))
          ))
        )
      )]
      [`(,rator ,rand)
       ((value-of rator env) (value-of rand env))]
    )
  )
)

;;; Problem 6. value-of-lex
(define value-of-lex
  (lambda (exp env)
    (match exp
      [`(const ,expr) expr]
      [`(mult ,x1 ,x2) (* (value-of-lex x1 env) (value-of-lex x2 env))]
      [`(zero ,x) (zero? (value-of-lex x env))]
      (`(sub1 ,body) (sub1 (value-of-lex body env)))
      (`(if ,t ,c ,a) (if (value-of-lex t env) (value-of-lex c env) (value-of-lex a env)))
      (`(var ,num) (apply-env-lex env num))
      (`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env))))
      (`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))))))
 
(define empty-env-lex 
  (lambda () '()))

(define apply-env-lex
  list-ref
)

(define extend-env-lex
  cons
)

;;; Just Dessert. csub1 
; (define csub1
;   (lambda (c)
;     (lambda (f)
;       (lambda (x)
;         (
;           (
;               ;;; We have to make sure that in c,
;               ;;; (f x) return x, and (f (f x)) returns (f x).
;               ;;; So we can't directly pass f as the input of c.
;               ;;; Instead, we need this f^ returns x at the first time,
;               ;;; and returns (f result) after that.
;             (c
;               (lambda (f^)
;                 (if (eqv? (cdr (cdr f^)) '(#f))
;                     x
;                     (list (car f^) ((car f^) (car (cdr f^))) #t) 
;                 )
;               )
;               ; (lambda (g)
;               ;  (lambda (h)
;               ;    (h (g f))
;               ;   )
;               ; )
;             )
;             (lambda (u) x)
;           )
;           (lambda (u) u)
;         )
;       )
;     )
;   )
; )


(define csub1
  (lambda (c)
    (lambda (f)
      (lambda (x)
        (car (
          (c
            (lambda (f^)
              (if (eqv? (cdr f^) #f)
                  (list x #t)
                  (list (f (car f^)) #t) 
              )
            )
          )
          `(,x . #f)
        ))
      )
    )
  )
)

(define c0 (lambda (f) (lambda (x) x)))
(define c5 (lambda (f) (lambda (x) (f (f (f (f (f x))))))))
; (((csub1 c5) add1) 0)
; (((csub1 c0) add1) 0)