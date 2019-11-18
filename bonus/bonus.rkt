#lang racket

;;; Part1. SPS
(define filter*
  (lambda (f ls)
    (cond
      [(null? ls) '()]
      [(pair? (car ls))
       (cons (filter* f (car ls)) (filter* f (cdr ls)))]
      [(null? (car ls)) '()]
      [(f (car ls)) (cons (car ls) (filter* f (cdr ls)))]
      [else (filter* f (cdr ls))])))

;; 1
(define filter*-sps
  (λ (f ls s)
    (cond
      [(null? ls) (values '() s)]
      [(pair? (car ls))
       (let-values ([(ret1 s1) (filter*-sps f (car ls) s)])
         (let-values ([(ret2 s2) (filter*-sps f (cdr ls) s)])
           (values (cons ret1 ret2) (cons s1 s2))))]
      [(null? (car ls)) (values '() s)]
      [(f (car ls))
       (let-values ([(ret s) (filter*-sps f (cdr ls) s)])
         (values (cons (car ls) ret) s))]
      [else
       (let-values ([(ret s) (filter*-sps f (cdr ls) s)])
         (values ret (cons (car ls) s)))])))
;; (filter*-sps (lambda (x) (or (even? x) (< 7 x))) '(1 (2 3 (4 5)) 6 7 ((8 9) 10)) '())

;; 2.
(define fib-sps
  (λ (n s)
    (cond
      [(assv n s) => (λ (pr) (values (cdr pr) s))]
      [(zero? n) (values 0 (cons `(,n . 0) s))]
      [(= 1 n) (values 1 (cons `(,n . 1) s))]
      [else (let-values ([(fib-sub1 s) (fib-sps (sub1 n) s)])
              (let-values ([(fib-sub2 s) (fib-sps (sub1 (sub1 n)) s)])
                (values (+ fib-sub1 fib-sub2)
                        (cons `(,n . ,(+ fib-sub1 fib-sub2)) s))))])))
;; (fib-sps 10 '())

;; 3.
(define-syntax (and* stx)
  (syntax-case stx ()
    [(and* x ...) #'(and x ...)]))
;; (and* #t #t #t #t #t #t #t #t #f)

;; 4.
(define-syntax (list* stx)
  (syntax-case stx ()
    [(_) (raise-syntax-error #f "Incorrect argument-count to list*")]
    [(_ x) #'x]
    [(_ x y ...) #'(cons x (list* y ...))]))
;; (list* 'a 'b 'c 'd)
;; (list*)

;; 5.
(define-syntax (macro-list stx)
  (syntax-case stx ()
    [(_) #''()]
    [(_ x y ...) #'(cons x (macro-list y ...))]))
;;(macro-list 1 'b 2 'd)
;;(macro-list)

;; 6.
(define-syntax (mcond stx)
  (syntax-case stx (else)
    [(_ (else clause)) #'clause]
    [(_ (test conseq) more ...) #'(if test conseq (mcond more ...))]
    [(_) #'(void)]))
;(mcond 
;    (else 'cat))
;(mcond
;    (#f #t)
;    (else 'dog))
;;(mcond 
;;    (#t #t) 
;;    (unbound variables))

;; 7.
(define-syntax (macro-map stx)
  (syntax-case stx ()
    [(_ m '()) #'null]
    [(_ m '(a more ...)) #'(cons (m a) (macro-map m '(more ...)))]))

;; 8. dessert
;; condre : else-let-cond