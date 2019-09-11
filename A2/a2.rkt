#lang racket

;; Part 1. Natural Recursion Refresher
;; Case 1. list-ref
(define list-ref
  (lambda (ls n)
    (letrec
      ((nth-cdr
         (lambda (n)
	          (cond 
              [(eqv? n 0) ls] 
              [else (cdr (nth-cdr (- n 1)))] );; complete the definition
           )))
      (car (nth-cdr n)))))
;;; (list-ref '(a b c) 2)
;;; (list-ref '(a b c) 0)

;; Case 2. union
(define union
  (lambda (l1 l2)
    (cond
      [(null? l2) l1]
      [(memv (car l2) l1) (union l1 (cdr l2))]
      [else (union (append l1 (list (car l2))) (cdr l2))])))
;;; (union '() '())
;;; (union '(x) '())
;;; (union '(x) '(x))
;;; (union '(x y) '(x z))

;; Case 3. extend
(define extend
  (lambda (x pred)
    (lambda (e)
      (cond
        [(eqv? e x) #t]
        [else (pred e)]))))
;;; ((extend 1 even?) 0)
;;; ((extend 1 even?) 1)
;;; ((extend 1 even?) 2)
;;; ((extend 1 even?) 3)
;;; (filter (extend 1 even?) '(0 1 2 3 4 5))
;;; (filter (extend 3 (extend 1 even?)) '(0 1 2 3 4 5))
;;; (filter (extend 7 (extend 3 (extend 1 even?))) '(0 1 2 3 4 5))

;; Case 4. walk-symbol
(define walk-symbol
  (lambda (x s)
    (cond
      [(not (assv x s)) x]
      [(assv (cdr (assv x s)) s) (walk-symbol (cdr (assv x s)) s)]
      [else (cdr (assv x s))])))
;;; (walk-symbol 'a '((a . 5)))
;;; (walk-symbol 'a '((b . c) (a . b)))
;;; (walk-symbol 'a '((a . 5) (b . 6) (c . a)))
;;; (walk-symbol 'c '((a . 5) (b . (a . c)) (c . a)))
;;; (walk-symbol 'b '((a . 5) (b . ((c . a))) (c . a)))
;;; (walk-symbol 'd '((a . 5) (b . (1 2)) (c . a) (e . c) (d . e)))
;;; (walk-symbol 'd '((a . 5) (b . 6) (c . f) (e . c) (d . e)))

;; Part 2. Free, Bound, Lexical Address
;; Case 5. lambda->lumbda
(define lambda->lumbda
  (lambda (exp)
    (match exp
      [`,y
        #:when (symbol? y) y]
      [`(lambda (,x) ,body)
        `(lumbda (,x) ,(lambda->lumbda body))]
      [`(,rator ,rand)
        (list (lambda->lumbda rator) (lambda->lumbda rand))])))
;;; (lambda->lumbda 'x)
;;; (lambda->lumbda '(lambda (x) x))
;;; (lambda->lumbda '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a))))))
;;; (lambda->lumbda '(lambda (lambda) lambda))
;;; (lambda->lumbda '((lambda (lambda) lambda) (lambda (y) y)))
;;; (lambda->lumbda '((lambda (x) x) (lambda (x) x)))

;;; Case 6. var-occurs?
(define var-occurs?
  (lambda (var exp)
    (match exp
      [`,y
        #:when (symbol? y)
        (eqv? y var)]
      [`(lambda (,x) ,body)
        (var-occurs? var body)]
      [`(,rator ,rand)
        (or (var-occurs? var rator) (var-occurs? var rand))])))
;;; (var-occurs? 'x 'x) 
;;; (var-occurs? 'x '(lambda (x) y))
;;; (var-occurs? 'x '(lambda (y) x))
;;; (var-occurs? 'x '((z y) x))

;;; Case 7. vars
(define vars
  (lambda (exp)
    (match exp
      [`,y
        #:when (symbol? y) `(,y)]
      [`(lambda (,x) ,body)
        (vars body)]
      [`(,rator ,rand)
        (append (vars rator) (vars rand))])))
;;; (vars 'x)
;;; (vars '(lambda (x) x))
;;; (vars '((lambda (y) (x x)) (x y)))
;;; (vars '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a))))))

;;; Case 8. unique-vars
(define unique-vars
  (lambda (exp)
    (match exp
      [`,y
        #:when (symbol? y) `(,y)]
      [`(lambda (,x) ,body)
        (unique-vars body)]
      [`(,rator ,rand)
        (union (unique-vars rator) (unique-vars rand))])))
;;; (unique-vars '((lambda (y) (x x)) (x y)))
;;; (unique-vars '((lambda (z) (lambda (y) (z y))) x))
;;; (unique-vars '((lambda (a) (a b)) ((lambda (c) (a c)) (b a))))

;;; Case 9. var-occurs-free?
(define var-occurs-free?
  (lambda (var exp)
    (match exp
      [`,y
        #:when (symbol? y)
        (eqv? y var)]
      [`(lambda (,x) ,body)
        (if (not (eqv? x var))
        (var-occurs-free? var body)
        #f)]
      [`(,rator ,rand)
        (or (var-occurs-free? var rator) (var-occurs-free? var rand))])))
;;; (var-occurs-free? 'z  '((lambda (z) (lambda (y) (z y))) x))
;;; (var-occurs-free? 'x 'x)
;;; (var-occurs-free? 'x '(lambda (y) y))
;;; (var-occurs-free? 'x '(lambda (x) (x y)))
;;; (var-occurs-free? 'x '(lambda (x) (lambda (x) x))) 
;;; (var-occurs-free? 'y '(lambda (x) (x y)))
;;; (var-occurs-free? 'y '((lambda (y) (x y)) (lambda (x) (x y))))
;;; (var-occurs-free? 'x '((lambda (x) (x x)) (x x)))
;;; (var-occurs-free? 'x '(lambda (x) (lambda (y) (x)))) ;;; not free?

;;; Case 10. var-occurs-bound?
(define var-occurs-bound?
  (lambda (var exp)
    (match exp
      [`,y
        #:when (symbol? y)
        #f]
      [`(lambda (,x) ,body)
        (if (not (eqv? x var))
        (var-occurs-bound? var body)
        (or (var-occurs-bound? var body) (var-occurs-free? var body)))]
      [`(,rator ,rand)
        (or (var-occurs-bound? var rator) (var-occurs-bound? var rand))])))
;;; (var-occurs-bound? 'x 'x)
;;; (var-occurs-bound? 'x '(lambda (x) x))
;;; (var-occurs-bound? 'y '(lambda (x) x))
;;; (var-occurs-bound? 'x '((lambda (x) (x x)) (x x)))
;;; (var-occurs-bound? 'z '(lambda (y) (lambda (x) (y z))))
;;; (var-occurs-bound? 'z '(lambda (y) (lambda (z) (y z))))
;;; (var-occurs-bound? 'x '(lambda (x) y))
;;; (var-occurs-bound? 'x '(lambda (x) (lambda (x) x)))

;;; Case 11. unique-free-vars
(define unique-free-vars
  (lambda (exp)
    (match exp
      [`,y
        #:when (symbol? y) `(,y)]
      [`(lambda (,x) ,body)
        (remv x (unique-free-vars body))]
      [`(,rator ,rand)
        (union (unique-free-vars rator) (unique-free-vars rand))])))
;;; (unique-free-vars 'x)
;;; (unique-free-vars '(lambda (x) (x y)))
;;; (unique-free-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c)))))))

;;; Case 12. unique-bound-vars
(define unique-bound-vars
  (lambda (exp)
    (match exp
      [`,y
        #:when (symbol? y)
        '()]
      [`(lambda (,x) ,body)
        (if (var-occurs-free? x body)
          (union `(,x) (unique-bound-vars body))
          (unique-bound-vars body))]
      [`(,rator ,rand)
        (union (unique-bound-vars rator) (unique-bound-vars rand))])))
;;; (unique-bound-vars 'x)
;;; (unique-bound-vars '(lambda (x) y))
;;; (unique-bound-vars '(lambda (x) (x y)))
;;; (unique-bound-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c)))))))
;;; (unique-bound-vars '(lambda (y) y))
;;; (unique-bound-vars '(lambda (x) (y z)))
;;; (unique-bound-vars '(lambda (x) (lambda (x) x)))

;;; Case 13. lex
(define lex
  (lambda (exp acc)
    (match exp
      [`,y
        #:when (symbol? y)
        `(var ,(cdr (assv y acc)))]
      [`(lambda (,x) ,body)
        #:when (and (symbol? x) (null? acc))
        (list 'lambda (lex body `((,x . 0))))]
      [`(lambda (,x) ,body)
        #:when (symbol? x)
         (if (assv x acc)
         (list 'lambda (lex body (cons `(,x . 0) (remv (assv x acc) (map (lambda (p) ((car p) . (+ 1 (cdr p)))) acc)))))
         (list 'lambda (lex body (cons `(,x . 0) (map (lambda (p) ((car p) . (+ 1 (cdr p)))) acc)))))]
      [`(,rator ,rand)
        (list (lex rator acc) (lex rand acc))])))
(lex '(lambda (x) x) '())
(lex '(lambda (y) (lambda (x) y)) '())