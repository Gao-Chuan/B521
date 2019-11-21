#lang racket
(require "mk.rkt")
(require "numbers.rkt")

;; Part I Write the answers to the following problems using your
;; knowledge of miniKanren.  For each problem, explain how miniKanren
;; arrived at the answer.  You will be graded on the quality of your
;; explanation; a full explanation will require several sentences.

;; 1 What is the value of 

(run 2 (q)
  (== 5 q)
  (conde
   [(conde 
     [(== 5 q)
      (== 6 q)])
    (== 5 q)]
   [(== q 5)]))
;;; The value of this is '((5)). 
;;; 1st line we associates q with the value 5.
;;; 2nd line we have a conde, but the first line will always return #f because q can't be two different number.
;;; In the 2st line in conde we associates q with the value 5 again.
;;; So we will have '((5)) in the end.

;; 2 What is the value of
(run 1 (q) 
  (fresh (a b) 
    (== `(,a ,b) q)
    (absento 'tag q)
    (symbolo a)))
;;; The value of this is '(((_0 _1)))
;;; 1st line we have 2 variable a and b
;;; 2nd line we associate q with `(,a ,b)
;;; 3rd line we hope that q does not contain 'tag
;;; 4th line we want a is a symbol
;;; So it will return a list with 2 lements and both of them are not 'tag. Besides, 1st element should be a symbol.

;; 3 What do the following miniKanren constraints mean?
;; a ==         equal
;; b =/=        not equal
;; c absento    2nd argument does not contain 1st argument
;; d numbero    The argument should be a number
;; e symbolo    The argument should be a symbol

;; Part II goes here.
;;; assoco
(define (assoco x ls ret)
    (fresh (a d aa da)
        (== `(,a . ,d) ls)
        (== `(,aa . ,da) a)
        (conde
            [(== aa x) (== a ret)]
            [(=/= aa x) (assoco x d ret)]
        )
    )
)
;;; (run* q (assoco 'x '((x . 6) (x . 5)) q))
;;; (run* q (assoco 'x '((x . 6) . ,q) '(x . 6)))

;;; reverseo
(define (reverseo ls ret)
    (conde
        [(== '() ls) (== '() ret)]
        [(fresh (a d)
            (== `(,a . ,d) ls)
            (fresh (res)
                (reverseo d res)
                (appendo res `(,a) ret)))]
    )
)
;;;  (run* x (reverseo `(a b c d) `(d . ,x)))

;;; stuttero
(define stutter
  (lambda (ls)
    (cond
      ((equal? '() ls) '())
      (else 
        (match-let* ((`(,a . ,d) ls)
		     (res (stutter d)))
          `(,a ,a . ,res))))))
(define (stuttero ls ret)
    (conde
        [(== '() ls) (== '() ret)]
        [(fresh (a d res)
            (== `(,a . ,d) ls)
            (stuttero d res)
            (== `(,a ,a . ,res) ret)
        )]
    )
)
(run 1 q (stuttero q '(1 1 2 2 3 3)))