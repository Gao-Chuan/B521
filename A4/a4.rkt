#lang racket

(provide (all-defined-out))

;;; Part 1.
(define empty-env
  (lambda ()
    (lambda (y) 
      (error 'val-of-cb? "unbound variable ~s" y)
    )
  )
)

(define extend-env
  (lambda (env x val)
    (lambda (y)
      (if (eqv? x y)
        val
        (apply-env env y))
    )
  )
)

(define apply-env
  (lambda (env y)
    (env y)
  )
)

(define apply-closure
  (lambda (exp arg)
    (exp arg)
  )
)

(define make-closure-cbv^
  (lambda (x body env)
    (lambda (arg)
      (let ([t (box arg)])
        (val-of-cbv^ body (extend-env env x t))
      )
    )
  )
)

(define val-of-cbv^ 
  (lambda (exp env)
    (match exp
      [`,n 
       #:when (number? n)
       n]
      [`,b
       #:when (boolean? b)
       b]
      [`(zero? ,x)
       (zero? (val-of-cbv x env))]
      [`(sub1 ,x)
       (sub1 (val-of-cbv x env))]
      [`(* ,a ,b)
       (* (val-of-cbv a env) (val-of-cbv b env))]
      [`(if ,c ,t ,e)
       (if (val-of-cbv c env) (val-of-cbv t env) (val-of-cbv e env))]
      [`(let ([,x ,e]) ,body)
          (let ([b (box (val-of-cbv e env))])
            (val-of-cbv body (lambda (y) (if (eqv? y x) b (env y)))))]
      [`(begin2 ,s1 ,s2)
       (begin (val-of-cbv s1 env) (val-of-cbv s2 env))]
      [`(random ,n) 
       (random (val-of-cbv n env))]
      [`(set! ,x ,v)
       (set-box! (apply-env env x) (val-of-cbv v env))]
      [`,x
       #:when (symbol? x)
       (unbox (apply-env env x))]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       (make-closure-cbv x body env)
      ]
      [`(,rator ,rand)
       (apply-closure (val-of-cbv rator env) (val-of-cbv rand env))]
    )
  )
)

(define make-closure-cbr
  (lambda (x body env)
    (lambda (arg)
      (val-of-cbr body (extend-env env x arg))
    )
  )
)

(define val-of-cbr 
  (lambda (exp env)
    (match exp
      [`,n 
       #:when (number? n)
       n]
      [`,b
       #:when (boolean? b)
       b]
      [`(zero? ,x)
       (zero? (val-of-cbr x env))]
      [`(sub1 ,x)
       (sub1 (val-of-cbr x env))]
      [`(* ,a ,b)
       (* (val-of-cbr a env) (val-of-cbr b env))]
      [`(if ,c ,t ,e)
       (if (val-of-cbr c env) (val-of-cbr t env) (val-of-cbr e env))]
      [`(begin2 ,s1 ,s2)
       (begin (val-of-cbr s1 env) (val-of-cbr s2 env))]
       [`(random ,n) 
       (random (val-of-cbr n env))]
      [`(set! ,x ,v)
       (set-box! (apply-env env x) (val-of-cbr v env))]
      [`,x
       #:when (symbol? x)
       (unbox (apply-env env x))]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       (make-closure-cbr x body env)
      ]
      [`(,rator ,x)
       #:when (symbol? x)
       (apply-closure (val-of-cbr rator env) (apply-env env x))]
      [`(,rator ,rand)
       (apply-closure (val-of-cbr rator env) (box (val-of-cbr rand env)))]
    )
  )
)

(define make-closure-cbname
  (lambda (x body env)
    (lambda (arg)
      (val-of-cbname body (extend-env env x arg))
    )
  )
)

(define val-of-cbname 
  (lambda (exp env)
    (match exp
      [`,n 
       #:when (number? n)
       n]
      [`,b
       #:when (boolean? b)
       b]
      [`(zero? ,x)
       (zero? (val-of-cbname x env))]
      [`(sub1 ,x)
       (sub1 (val-of-cbname x env))]
      [`(* ,a ,b)
       (* (val-of-cbname a env) (val-of-cbname b env))]
      [`(if ,c ,t ,e)
       (if (val-of-cbname c env) (val-of-cbname t env) (val-of-cbname e env))]
      [`(begin2 ,s1 ,s2)
       (begin (val-of-cbname s1 env) (val-of-cbname s2 env))]
      [`(random ,n) 
       (random (val-of-cbname n env))]
      [`,x
       #:when (symbol? x)
       ((apply-env env x))]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       (make-closure-cbname x body env)
      ]
      [`(,rator ,rand)
       (apply-closure (val-of-cbname rator env) (lambda() (val-of-cbname rand env)))]
    )
  )
)

(define make-closure-cbneed
  (lambda (x body env)
    (lambda (arg)
      (val-of-cbneed body (extend-env env x arg))
    )
  )
)

(define val-of-cbneed 
  (lambda (exp env)
    (match exp
      [`,n 
       #:when (number? n)
       n]
      [`,b
       #:when (boolean? b)
       b]
      [`(zero? ,x)
       (zero? (val-of-cbneed x env))]
      [`(sub1 ,x)
       (sub1 (val-of-cbneed x env))]
      [`(* ,a ,b)
       (* (val-of-cbneed a env) (val-of-cbneed b env))]
      [`(if ,c ,t ,e)
       (if (val-of-cbneed c env) (val-of-cbneed t env) (val-of-cbneed e env))]
      [`(begin2 ,s1 ,s2)
       (begin (val-of-cbneed s1 env) (val-of-cbneed s2 env))]
      [`(random ,n) 
       (random (val-of-cbneed n env))]
      [`,x
       #:when (symbol? x)
       (let ([b (apply-env env x)])
        (let ([exe (unbox b)])
          (let ([v (exe)])
            (begin (set-box! b (lambda () v))
                    v)
          )
        )
       )]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       (make-closure-cbneed x body env)
      ]
      [`(,rator ,x)
       #:when (symbol? x)
       ((val-of-cbneed rator env) (apply-env env x))]
      [`(,rator ,rand)
       (apply-closure (val-of-cbneed rator env) (box (lambda() (val-of-cbneed rand env))))]
    )
  )
)

; (val-of-cbr
;    '((lambda (x) (begin2 (set! x #t)
;                          (if x 3 5))) #f)
;    (empty-env))
; ; ; 3
; (val-of-cbr
;    '((lambda (a)
;        ((lambda (p)
;           (begin2
;            (p a)
;            a)) (lambda (x) (set! x 4)))) 3)
;    (empty-env))
; (val-of-cbv
;    '((lambda (a)
;        ((lambda (p)
;           (begin2
;            (p a)
;            a)) (lambda (x) (set! x 4)))) 3)
;    (empty-env))

; ; ;; returns 44 under CBR...
; (val-of-cbr
;    '((lambda (f)
;        ((lambda (g)
;           ((lambda (z) (begin2
;                         (g z)
;                         z))
;            55))
;         (lambda (y) (f y)))) (lambda (x) (set! x 44)))
;    (empty-env))
; ; ;; ...but returns 55 under CBV!  You can change the "begin2" to
; ; ;; "begin" and evaluate this in the Racket REPL as evidence that
; ; ;; Racket uses CBV.
; (val-of-cbv
;    '((lambda (f)
;        ((lambda (g)
;           ((lambda (z) (begin2
;                         (g z)
;                         z))
;            55))
;         (lambda (y) (f y)))) (lambda (x) (set! x 44)))
;    (empty-env))
; ; ; 55
; ; ;; Returns 44 under CBR...
; (val-of-cbr
;    '((lambda (swap)
;        ((lambda (a)
;           ((lambda (b)
;              (begin2
;               ((swap a) b)
;               a)) 44)) 33))
;      (lambda (x)
;        (lambda (y)
;          ((lambda (temp)
;             (begin2
;              (set! x y)
;              (set! y temp))) x))))
;    (empty-env))
; ; ; 44
; ; ;; ...but returns 33 under CBV.
; (val-of-cbv
;    '((lambda (swap)
;        ((lambda (a)
;           ((lambda (b)
;              (begin2
;               ((swap a) b)
;               a)) 44)) 33))
;      (lambda (x)
;        (lambda (y)
;          ((lambda (temp)
;             (begin2
;              (set! x y)
;              (set! y temp))) x))))
;    (empty-env))

; (define random-sieve
;     '((lambda (n)
;         (if (zero? n)
;             (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) #t #f) #f) #f) #f) #f) #f)
;             (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f #t))))))))
;       (random 2)))
; (val-of-cbname random-sieve (empty-env))
; (val-of-cbneed random-sieve (empty-env))

; (val-of-cbname
;    '((lambda (z) 100)
;      ((lambda (x) (x x)) (lambda (x) (x x))))
;    (empty-env))

;;; Brainteaser
(define make-closure-cbv
  (lambda (x body env)
    (lambda (arg)
        (val-of-cbv body (extend-env env x arg))
    )
  )
)

(define val-of-cbv 
  (lambda (exp env)
    (match exp
      [`,n 
       #:when (number? n)
       n]
      [`,b
       #:when (boolean? b)
       b]
      [`(zero? ,x)
       (zero? (val-of-cbv x env))]
      [`(null? ,x)
       (null? (val-of-cbv x env))]
      [`(sub1 ,x)
       (sub1 (val-of-cbv x env))]
      [`(add1 ,x)
       (add1 (val-of-cbv x env))]
      [`(* ,a ,b)
       (* (val-of-cbv a env) (val-of-cbv b env))]
      [`(if ,c ,t ,e)
       (if (val-of-cbv c env) (val-of-cbv t env) (val-of-cbv e env))]
      [`(let ([,x ,e]) ,body)
          (let ([b (box (val-of-cbv e env))])
            (val-of-cbv body (lambda (y) (if (eqv? y x) b (env y)))))]
      [`(begin2 ,s1 ,s2)
       (begin (val-of-cbv s1 env) (val-of-cbv s2 env))]
      [`(random ,n) 
       (random (val-of-cbv n env))]
      [`(set! ,x ,v)
       (set-box! (apply-env env x) (val-of-cbv v env))]
      [`(cons ,head ,tail)
       (cons (val-of-cbv head env) (val-of-cbv tail env))]
      [`(cons^ ,head ,tail)
       (cons (box (lambda () (val-of-cbv head env))) (box (lambda () (val-of-cbv tail env))))]
      [`(car ,ele)
       (car ele)]
      [`(car^ ,ele)
       ((unbox (car (val-of-cbv ele env))))]
      [`(cdr ,ele)
       (cdr ele)]
      [`(cdr^ ,ele)
       ((unbox (cdr (val-of-cbv ele env))))]
      [`,x
       #:when (symbol? x)
       (unbox (apply-env env x))]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       (make-closure-cbv x body env)]
      [`(quote ,v) v]
      [`(,rator ,rand)
       (apply-closure (val-of-cbv rator env) (box (val-of-cbv rand env)))]
    )
  )
)

; (define cons-test
;     '(let ((fix (lambda (f)
;                  ((lambda (x) (f (lambda (v) ((x x) v))))
;                   (lambda (x) (f (lambda (v) ((x x) v))))))))
;         (let ((map (fix (lambda (map)
;                           (lambda (f)
;                             (lambda (l)
;                                (if (null? l)
;                                    '()
;                                    (cons^ (f (car^ l))
;                                           ((map f) (cdr^ l))))))))))
;           (let ((take (fix (lambda (take)
;                              (lambda (l)
;                                (lambda (n)
;                                  (if (zero? n)
;                                      '()
;                                       (cons (car^ l) 
;                                             ((take (cdr^ l)) (sub1 n))))))))))
;             ((take ((fix (lambda (m)
;                            (lambda (i)
;                              (cons^ 1 ((map (lambda (x) (add1 x))) (m i)))))) 0)) 5)))))
; (val-of-cbv cons-test (empty-env))

;;; Just Dessert
