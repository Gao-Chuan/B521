#lang racket

;;; Part I
(define lex
  (lambda (exp acc)
    (match exp
      [`,n
        #:when (number? n)
        `(const ,n)]
      [`(zero? ,v)
        (list 'zero? (lex v acc))]
      [`(* ,a ,b)
        (list '* (lex a acc) (lex b acc))]
      [`(sub1 ,n)
        (list 'sub1 (lex n acc))]
      [`(if ,c ,t ,e)
        (list 'if (lex c acc) (lex t acc) (lex e acc))]
      [`(let ((,arg ,v)) ,e)
        (if (assv arg acc)
        (list 'let (lex v acc) (lex e (cons `(,arg . 0) (remv (assv arg acc) acc))))
        (list 'let (lex v acc) (lex e (cons `(,arg . 0) acc))))]
      [`,y
        #:when (symbol? y)
        `(var ,(cdr (assv y acc)))]
      [`(lambda (,x) ,body)
        #:when (symbol? x)
         (if (assv x acc)
         (list 'lambda (lex body (cons `(,x . 0) (remv (assv x acc) (map (lambda (p) `(,(car p) . ,(+ 1 (cdr p)))) acc)))))
         (list 'lambda (lex body (cons `(,x . 0) (map (lambda (p) `(,(car p) . ,(+ 1 (cdr p)))) acc)))))]
      [`(,rator ,rand)
        (list (lex rator acc) (lex rand acc))])))
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

;;; Part II
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


(define empty-env
  (lambda ()
    (lambda (y) 
      (error 'value-of "unbound variable ~s" y)
    )
  )
)

(define apply-env
  (lambda (env y)
    (env y)
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

(define closure-fn
  (lambda (x body env)
    (lambda (arg)
      (value-of-fn body (extend-env env x arg))
    )
  )
)

(define apply-closure-fn
  (lambda (exp arg)
    (exp arg)
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
       (value-of-fn body (extend-env env x (value-of-fn e env)))]
      [`,x
       #:when (symbol? x)
       (apply-env env x)]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       (closure-fn x body env)]
      [`(,rator ,rand)
       (apply-closure-fn (value-of-fn rator env) (value-of-fn rand env))]
    )
  )
)

(define closure-ds
  (lambda (x body env)
    `(apply ,x ,body ,env)
  )
)

(define apply-closure-ds
  (lambda (exp arg)
    (match exp
      [`(apply ,x ,body ,env) (value-of-ds body (extend-env env x arg))]
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
       (value-of-ds body (extend-env env x (value-of-ds e env)))]
      [`,x
       #:when (symbol? x)
       (apply-env env x)]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       (closure-ds x body env)]
      [`(,rator ,rand)
       (apply-closure-ds (value-of-ds rator env) (value-of-ds rand env))]
    )
  )
)
; (value-of-fn 
;     '((lambda (x) (if (zero? x) 
;                       12 
;                       47)) 
;        0) 
;     (empty-env))
; (value-of-fn
;    '(let ([y (* 3 4)])
;       ((lambda (x) (* x y)) (sub1 6)))
;    (empty-env))
; (value-of-fn
;    '(let ([x (* 2 3)])
;       (let ([y (sub1 x)])
;         (* x y)))
;    (empty-env))
; (value-of-fn
;    '(let ([x (* 2 3)])
;       (let ([x (sub1 x)])
;         (* x x)))
;    (empty-env))
; (value-of-ds
;     '((lambda (x) (if (zero? x) 
;                       12 
;                       47)) 
;        0) 
;     (empty-env))
; (value-of-ds
;    '(let ([y (* 3 4)])
;       ((lambda (x) (* x y)) (sub1 6)))
;    (empty-env))
; (value-of-ds
;    '(let ([x (* 2 3)])
;       (let ([y (sub1 x)])
;         (* x y)))
;    (empty-env))
; (value-of-ds
;    '(let ([x (* 2 3)])
;       (let ([x (sub1 x)])
;         (* x x)))
;    (empty-env))

;;; Part III

(define closure-dynamic
  (lambda (x body)
    (lambda (arg env)
      (value-of-dynamic body (extend-env env x arg))
    )
  )
)

(define apply-closure-dynamic
  (lambda (exp arg env)
    (exp arg env)
  )
)

(define value-of-dynamic
  (lambda (exp env)
    (match exp
      [`,n 
       #:when (number? n)
       n]
      [`,b
       #:when (boolean? b)
       b]
      [`(quote ,v) v]
      [`(null? ,v)
        (null? (value-of-dynamic v env))]
      [`(zero? ,x)
       (zero? (value-of-dynamic x env))]
      [`(sub1 ,x)
       (sub1 (value-of-dynamic x env))]
      [`(* ,a ,b)
       (* (value-of-dynamic a env) (value-of-dynamic b env))]
      [`(if ,c ,t ,e)
       (if (value-of-dynamic c env) (value-of-dynamic t env) (value-of-dynamic e env))]
      [`(let ([,x ,e]) ,body)
       (value-of-dynamic body (extend-env env x (value-of-dynamic e env)))]
      [`(cons ,a ,b)
        (cons (value-of-dynamic a env) (value-of-dynamic b env))]
      [`(car ,l)
        (car (value-of-dynamic l env))]
      [`(cdr ,l)
        (cdr (value-of-dynamic l env))]
      [`,x
       #:when (symbol? x)
       (apply-env env x)]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       (closure-dynamic x body)]
      [`(,rator ,rand)
       (apply-closure-dynamic (value-of-dynamic rator env) (value-of-dynamic rand env) env)]
    )
  )
)
(value-of-dynamic '(let ([map (lambda (f)
                                (lambda (ls)
                                  (if (null? ls)
                                      '()
                                      (cons (f (car ls)) ((map f) (cdr ls))))))])
                        (let ([f (lambda (e) (cons e ls))])
                          ((map f) (cons 2 (cons 3 '())))))
                  (empty-env))
; (let ([map (lambda (f)
;                                 (lambda (ls)
;                                   (if (null? ls)
;                                       '()
;                                       (cons (f (car ls)) ((map f) (cdr ls))))))])
;                         (let ([f (lambda (e) (cons e ls))])
;                           ((map f) (cons 2 (cons 3 '())))))
; (value-of-dynamic '(let ([x 2])
;                        (let ([f (lambda (e) x)])
;                          (let ([x 5])
;                            (f 0))))
;                     (empty-env))
; (value-of-dynamic
;     '(let ([! (lambda (n)
;                 (if (zero? n) 
;                     1
;                     (* n (! (sub1 n)))))])
;        (! 5))
;     (empty-env))
; (value-of-dynamic
;     '((lambda (!) (! 5))
;         (lambda (n)
;           (if (zero? n) 
;               1
;               (* n (! (sub1 n))))))
;     (empty-env))
; (value-of-dynamic
;     '(let ([f (lambda (x) (cons x l))])
;        (let ([cmap 
; 	      (lambda (f)
; 		(lambda (l)               
; 		  (if (null? l) 
; 		      '()
; 		      (cons (f (car l)) ((cmap f) (cdr l))))))])
; 	 ((cmap f) (cons 1 (cons 2 (cons 3 '())))))) 
;     (empty-env))

;;; Brainteasers

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

(define closure-fn-ri
  (lambda (x body env)
    (lambda (arg)
      (value-of-fn body (extend-env env x arg))
    )
  )
)

(define apply-closure-fn-ri
  (lambda (exp arg)
    (exp arg)
  )
)

(define closure-ds-ri
  (lambda (x body env)
    `(apply ,x ,body ,env)
  )
)

(define apply-closure-ds-ri
  (lambda (exp arg)
    (match exp
      [`(apply ,x ,body ,env) (value-of-ds body (extend-env env x arg))]
    )
  )
)

(define value-of-ri
  (lambda (env ext-env app-env clo app-clo)
    (letrec ([value-of-ri
              (lambda (env)
                (lambda (exp)
                  (match exp
                    [`,n 
                    #:when (number? n)
                    n]
                    [`,b
                    #:when (boolean? b)
                    b]
                    [`(zero? ,x)
                    (zero? ((value-of-ri env) x))]
                    [`(sub1 ,x)
                    (sub1 ((value-of-ri env) x))]
                    [`(* ,a ,b)
                    (* ((value-of-ri env) a) ((value-of-ri env) b))]
                    [`(if ,c ,t ,e)
                    (if ((value-of-ri env) c) ((value-of-ri env) t) ((value-of-ri env) e))]
                    [`(let ([,x ,e]) ,body)
                    ((value-of-ri (ext-env env x ((value-of-ri env) e))) body)]
                    [`,x
                    #:when (symbol? x)
                    (app-env env x)]
                    [`(lambda (,x) ,body)
                    #:when (symbol? x)
                    (clo x body env)]
                    [`(,rator ,rand)
                    (app-clo ((value-of-ri env) rator) ((value-of-ri env) rand))]
                  )
                )
              )
            ])
      (value-of-ri env)
    )
  )
)

; ((value-of-ri empty-env-fn extend-env-fn apply-env-fn closure-fn-ri apply-closure-fn-ri) '((lambda (x) x) 5))
; ((value-of-ri empty-env-ds extend-env-ds apply-env-ds closure-ds-ri apply-closure-ds-ri) '((lambda (x) x) 5))
; ((value-of-ri empty-env-fn extend-env-fn apply-env-fn closure-ds-ri apply-closure-ds-ri) '((lambda (x) x) 5))
; ((value-of-ri empty-env-ds extend-env-ds apply-env-ds closure-fn-ri apply-closure-fn-ri) '(let ([x (* 2 3)])(let ([x (sub1 x)])(* x x))))

;;; Just Dessert
(define alpha-all
  (lambda (exp)
    (match exp
      [`,x #:when (symbol? x) x]
      [`(lambda (,x) ,body)
       (let ((g (gensym (symbol->string x))))
         `(lambda (,g) ,(subst g x (alpha-all body))))]
      [`(,rator ,rand)
       `(,(alpha-all rator) ,(alpha-all rand))])))

(define subst
  (lambda (g x body)
    (match body
      [`,y
        #:when (symbol? y)
        (if (eqv? y x)
            g
            y)]
      [`(lambda (,y) ,body)
        `(lambda (,(subst g x y)) ,(subst g x body))]
    )
  )
)

; (alpha-all '(lambda (x) (lambda (y) x)))