#lang racket

(provide (all-defined-out))

;;; case 1. countdown
(define countdown
    (lambda (n)
        (cond 
            [(zero? n) (cons 0 '())]
            [else      (cons n (countdown (- n 1)))])))
;; (countdown 5)

;;; case 2. insertR
(define insertR
    (lambda (m n l)
        (cond
            [(null? l) '()]
            [(eqv? (car l) m) (cons m (cons n (insertR m n (cdr l))))]
            [else (cons (car l) (insertR m n (cdr l)))])))
;;; (insertR 'x 'y '(x z z x y x))

;;; case 3. remv-1st
(define remv-1st
    (lambda (m l)
        (cond
            [(eqv? (car l) m) (cdr l)]
            [else (cons (car l) (remv-1st m (cdr l)))])))
;;; (remv-1st 'x '(x y z x))
;;; (remv-1st 'y '(x y z y x))

;;; case 4. list-index-ofv?
(define list-index-ofv?
    (lambda (m l)
        (cond
            [(eqv? (car l) m) 0]
            [else (add1 (list-index-ofv? m (cdr l)))])))
;;; (list-index-ofv? 'x '(x y z x x))
;;; (list-index-ofv? 'x '(y z x x))

;;; case 5. filter
(define filter
    (lambda (p l)
        (cond
            [(null? l) '()]
            [(p (car l)) (cons (car l) (filter p (cdr l)))]
            [else (filter p (cdr l))])))
;;; (filter even? '(1 2 3 4 5 6))

;;; case 6. zip
(define zip
    (lambda (l_m l_n)
        (cond
            [(or (null? (cdr l_m)) (null? (cdr l_n))) (cons (cons (car l_m) (car l_n)) '())]
            [else (cons (cons (car l_m) (car l_n)) (zip (cdr l_m) (cdr l_n)))])))
;;; (zip '(1 2 3) '(a b c))
;;; (zip '(1 2 3 4 5 6) '(a b c))
;;; (zip '(1 2 3) '(a b c d e f))

;;; case 7. map
(define map
    (lambda (p l)
        (cond
            [(null? l) '()]
            [else (cons (p (car l)) (map p (cdr l)))])))
;;; (map add1 '(1 2 3 4))

;;; case 8. append
(define append
    (lambda (l_m l_n)
        (cond
            [(null? l_m) l_n]
            [else (cons (car l_m) (append (cdr l_m) l_n))])))
;;; (append '(a b c) '(1 2 3))

;;; case 9. reverse
(define reverse
    (lambda (l)
        (cond
            [(null? l) '()]
            [else (append (reverse (cdr l)) (cons (car l) '()))])))
;;; (reverse '(a 3 x))

;;; case 10. fact
(define fact
    (lambda (m)
        (cond 
            [(eqv? m 0) 1]
            [else (* m (fact (sub1 m)))])))
;;; (fact 0)
;;; (fact 5)

;;; case 11. memv
(define memv
    (lambda (m l)
        (cond
            [(null? l) #f]
            [(eqv? (car l) m) l]
            [else (memv m (cdr l))])))
;;; (memv 'a '(a b c))
;;; (memv 'b '(a ? c))
;;; (memv 'b '(a b c b))

;;; case 12. fib
(define fib
    (lambda (m)
        (cond
            [(eqv? m 0) 0]
            [(eqv? m 1) 1]
            [else (+ (fib (- m 1)) (fib (- m 2)))])))
;;; (fib 0)
;;; (fib 1)
;;; (fib 7)

;;; case 13. as many dots as possible
;;; > '((w . (x . ())) . (y . ((z . ()) . ())))
;;; '((w x) y (z))
;;; > (equal? '((w . (x . ())) . (y . ((z . ()) . ()))) '((w x) y (z)))
;;; #t

;;; case 14. binary->natural
(define binary->natural
    (lambda (l)
        (cond
            [(null? l) 0]
            [else (+ (* 2 (binary->natural (cdr l))) (car l))])))
;;; (binary->natural '())
;;; (binary->natural '(0 0 1))
;;; (binary->natural '(0 0 1 1))
;;; (binary->natural '(1 1 1 1))
;;; (binary->natural '(1 0 1 0 1))
;;; (binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1))

;;; case 15. minus
(define minus
    (lambda (m n)
        (cond
            [(zero? n) m]
            [else (sub1 (minus m (sub1 n)))])))
;;; (minus 5 3)
;;; (minus 100 50)

;;; case 16. div
(define div
    (lambda (m n)
        (cond
            [(eqv? m n) 1]
            [else (add1 (div( - m n) n))])))
;;; (div 25 5)
;;; (div 36 6)
;;; (div 49 7)

;;; case 17. append-map
(define append-map
    (lambda (p l)
        (cond
            [(null? l) '()]
            [else (append (p (car l)) (append-map p (cdr l)))])))
;;; (append-map countdown (countdown 5))

;;; case 18. set-difference
(define set-difference
    (lambda (l_m l_n)
        (cond
            [(null? l_m) '()]
            [(member (car l_m) l_n) (set-difference (cdr l_m) l_n)]
            [else (cons (car l_m) (set-difference (cdr l_m) l_n))])))
;;; (set-difference '(1 2 3 4 5) '(2 4 6 8))

;;; case 19. powerset
(define powerset
    (lambda (l)
        (cond
            [(null? l) '(())]
            [else (append (map (lambda (le) (cons (car l) le)) (powerset (cdr l))) (powerset (cdr l)))])))
;;; (powerset '(3 2 1))

;;; case 20. cartesian-product
(define cartesian-product
    (lambda (l)
        (cond
            [(null? (car l)) `()]
            [else (append (map (lambda (le) (list (car (car l)) le)) (car (cdr l))) 
                          (cartesian-product (list (cdr (car l)) (car (cdr l))))) ])))
;;; (cartesian-product '((5 4) (3 2 1)))

;;; case 21. foldr
(define insertR-fr
    (lambda (m n l)
        (foldr (lambda (le partial) (cond
            [(eqv? le m) (cons m (cons n partial))]
            [else (cons le partial)])) '() l)))
;;; (insertR-fr 'x 'y '(x z z x y x))
(define filter-fr
    (lambda (p l)
        (foldr (lambda (le partial) (cond
            [(p le) (cons le partial)]
            [else partial])) '() l)))
;;; (filter-fr even? '(1 2 3 4 5 6))
(define map-fr
    (lambda (p l)
        (foldr (lambda (le partial) (cons (p le) partial)) '() l)))
;;; (map-fr add1 '(1 2 3 4))
(define append-fr
    (lambda (l_m l_n)
        (foldr (lambda (le partial) (cons le partial)) l_n l_m)))
;;; (append-fr '(a b c) '(1 2 3))
(define reverse-fr
    (lambda (l)
        (foldr (lambda (le partial) (append-fr partial (cons le '()))) '() l)))
;;; (reverse-fr '(a 3 x))
(define binary->natural-fr
    (lambda (l)
        (foldr (lambda (le partial) (+ (* partial 2) le)) 0 l)))
;;; (binary->natural '())
;;; (binary->natural '(0 0 1))
;;; (binary->natural '(0 0 1 1))
;;; (binary->natural '(1 1 1 1))
;;; (binary->natural '(1 0 1 0 1))
;;; (binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1))
(define append-map-fr
    (lambda (p l)
        (foldr (lambda (le partial) (append (p le) partial)) '() l)))
;;; (append-map countdown (countdown 5))

(define set-difference-fr
    (lambda (l_m l_n)
        (foldr (lambda (le partial) (cond
            [(member le l_n) partial]
            [else (cons le partial)])) '() l_m)))
;;; (set-difference-fr '(1 2 3 4 5) '(2 4 6 8))
(define powerset-fr
    (lambda (l)
        (foldr (lambda (le partial) (append (map (lambda (pe) (cons le pe)) partial) partial)) '(()) l)))
;;; (powerset '(3 2 1))
(define cartesian-product-fr
    (lambda (l)
        (foldr (lambda (le partial) (append (map (lambda (lde) (list le lde)) (car (cdr l))) partial)) '() (car l))))
;;; (cartesian-product '((5 4) (3 2 1)))
;;; (cartesian-product '((7 6 5) (3 2)))


;;; case 22. collatz
(define collatz
    (letrec
        ((odd-case
            (lambda (recur)
                (lambda (x)
                    (cond 
                        ((and (positive? x) (odd? x)) (collatz (add1 (* x 3)))) 
                        (else (recur x))))))
        (even-case
            (lambda (recur)
                (lambda (x)
                    (cond 
                        ((and (positive? x) (even? x)) (collatz (/ x 2))) 
                        (else (recur x))))))
        (one-case
            (lambda (recur)
                (lambda (x)
                    (cond
                        ((zero? (sub1 x)) 1)
                        (else (recur x))))))
        (base
            (lambda (x)
                (error 'error "Invalid value ~s~n" x))))
        (one-case (even-case (odd-case base)));; this should be a single line, without lambda
    ))
;; (collatz 12)
;; (collatz 120)
;; (collatz 9999)

;;; ;;; case Just Dessert. quine
;; (define quine ((lambda (x) (display `(,x ,x))) '((lambda (x) (display `(,x ,x))))))
;; (define quine ((lambda (x) `(,x ,x)) `(lambda (x) `(,x ,x))))

;; Discussed with Tianyu.
(define quine ((lambda (x y) (list x (list 'quote x) y))
               '(lambda (x y) (list x (list 'quote x) y))
               1))

;; (equal? quine (eval (eval quine)))
