#lang racket

;; Part 1
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

;; Part 2.
