#lang racket

(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))

;;; 1.
(define binary-to-decimal-cps
  (lambda (n k)
    (cond
      [(null? n) (k 0)]
      [else (binary-to-decimal-cps (cdr n)
                                   (lambda (v)
                                    (k (+ (car n) (* 2 v)))
                                   ))]
    )
  )
)

; (binary-to-decimal-cps '(1 1 0 1) (empty-k))
; (binary-to-decimal-cps '(0 1) (empty-k))
; (binary-to-decimal-cps '(1) (empty-k))
; (binary-to-decimal-cps '() (empty-k))

;;; 2.
(define times-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [else (times-cps (cdr ls) (lambda (v)
                                  (k (* (car ls) v))
                                ))]
    )
  )
)
; (times-cps '(1 2 3 4 5) (empty-k))
; (times-cps '(1 2 3 0 5) (empty-k))

;;; 3.
(define times-cps-shortcut 
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) 0]
      [else (times-cps (cdr ls) (lambda (v)
                                  (k (* (car ls) v))
                                ))]
    )
  )
)
; (times-cps-shortcut  '(1 2 3 4 5) (empty-k))
; (times-cps-shortcut  '(1 2 3 0 5) (empty-k))

; 4. 
(define plus-cps
  (lambda (m k)
    (k (lambda (n k^)
      (k^ (+ m n))
    ))
  )
)
; ((plus-cps 2 (empty-k)) 3 (empty-k)) 
; ((plus-cps ((plus-cps 2 (empty-k)) 3 (empty-k)) (empty-k)) 5 (empty-k))

;;; 5.
(define remv-first-9*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls))
        (cond
          [(remv-first-9*-cps (car ls) (lambda (v)
                                          (equal? (car ls) v)
                                        ))
            (remv-first-9*-cps (cdr ls) (lambda (v)
                                          (k (cons (car ls) v))
                                        ))
          ]
          [else
            (remv-first-9*-cps (car ls) (lambda (v)
                                          (k (cons v (cdr ls)))
                                        ))
          ]
        )
      ]
      [(eqv? (car ls) '9) (k (cdr ls))]
      [else (remv-first-9*-cps (cdr ls) (lambda (v)
                                          (k (cons (car ls) v))
                                        ))]
    )
  )
)
; (remv-first-9*-cps '((1 2 (3) 9)) (empty-k))
; (remv-first-9*-cps '(((((9) 9) 9) 9) 9) (empty-k))
; (remv-first-9*-cps '(9 (9 (9 (9)))) (empty-k))

;;; 6.
(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls)
        (cons-cell-count-cps (car ls) (lambda (v)
                                        (cons-cell-count-cps (cdr ls) (lambda (v^)
                                                                        (k (add1 (+ v v^)))
                                                                      ))
                                      ))]
      [else (k 0)]
    )
  )
)
; (cons-cell-count-cps '(1 2 3 4 (5)) (empty-k))

;;; 7.
(define find-cps
  (lambda (u s k)
    (let ([pr (assv u s)])
      (if pr
      (find-cps (cdr pr) s k)
      (k u))
    )
  )
)
; (find-cps 7 '((5 . a) (6 . 5) (7 . 6)) (empty-k))
; (find-cps 5 '((5 . 6) (9 . 6) (2 . 9))(empty-k))

;;; 8. 
(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 k)]
      [else (ack-cps m (sub1 n) (lambda (v)
                                  (ack-cps (sub1 m) v k)
                                ))]
    )
  )
)
; (ack-cps 2 2 (empty-k))
; (ack-cps 3 4 (empty-k))

;;; 9. 
(define fib-cps
  (lambda (n k)
    ((lambda (f)
      (f f n k))
    (lambda (f n k)
      (cond
        [(zero? n) (k 0)]
        [(zero? (sub1 n)) (k 1)]
        [else (f f (sub1 n) (lambda (v)
                              (f f (sub1 (sub1 n)) (lambda (v^)
                                                    (k (+ v v^))
                                                   ))
                            ))])
    ))
  )
)

; (fib-cps 6 (empty-k))

;;; 10. 
(define unfold-cps
  (lambda (p f g seed k)
    ((lambda (h)
      ((h h) seed '() k))
    (lambda (h)
      (lambda (seed ans k)
        (p seed (lambda (vp)
                  (if vp
                      (k ans)
                      (g seed (lambda (vg)
                                (f seed (lambda (vf)
                                          ((h h) vg (cons vf ans) k)
                                        ))
                      ))
                  )
                )
        )
      )
    ))
  )
)
; (define null?-cps
;     (lambda (ls k)
;       (k (null? ls))))
; (define car-cps
;     (lambda (pr k)
;       (k (car pr))))
; (define cdr-cps
;     (lambda (pr k)
;       (k (cdr pr))))
; (unfold-cps null?-cps car-cps cdr-cps '(a b c d e) (empty-k))

;;; 11. 
(define empty-s
  (lambda ()
    '()))
 
(define unify-cps
  (lambda (u v s k)
    (cond 
      [(eqv? u v) (k s)]
      [(number? u) (k (cons (cons u v) s))]
      [(number? v) (unify-cps v u s k)]
      [(pair? u)
        (if (pair? v)
          (find-cps (car u) s (lambda (v-find-u)
            (find-cps (car v) s (lambda (v-find-v)
              (unify-cps v-find-u v-find-v s (lambda (s^)
                (if s^
                  (find-cps (cdr u) s^ (lambda (v-find-cdr-u)
                    (find-cps (cdr v) s^ (lambda (v-find-cdr-v)
                      (unify-cps v-find-cdr-u v-find-cdr-v s^ k)
                    ))
                  ))
                (k #f))
              ))
            ))
          ))
          (k #f))]
      [else (k #f)]
    )
  )
)
; (unify-cps 'x 5 (empty-s) (empty-k))
; (unify-cps 'x 5 (unify-cps 'y 6 (empty-s) (empty-k)) (empty-k))
; (unify-cps '(x y) '(5 6) (empty-s) (empty-k))
; (unify-cps 'x 5 (unify-cps 'x 6 (empty-s) (empty-k)) (empty-k))
; (unify-cps '(x x) '(5 6) (empty-s) (empty-k))
; (unify-cps '(1 2 3) '(x 1 2) (empty-s) (empty-k))
; (unify-cps 'x 'y (empty-s) (empty-k))

;;; 12.
(define M-cps
  (lambda (f)
    (lambda (ls k)
      (cond
        [(null? ls) (k '())]
        [else ((M-cps f) (cdr ls) (lambda (v)
                                        (k (cons (f (car ls) (empty-k)) v))
                                      ))]
      )
    )
  )
)
; ((M-cps fib-cps) '(1 2 3 4 5 6) (empty-k))

;;; 13.
(define use-of-M-cps
  ((M-cps (lambda (n k) (k (add1 n)))) '(1 2 3 4 5) (empty-k))
)
; use-of-M-cps

;;; 14.
(define strange-cps
  (lambda (x k)
    ((lambda (g k)
      (k (lambda (x k) (g g k)))
    )
    (lambda (g k)
      (k (lambda (x k) (g g k)))
    )
    k)
  )
)

;;; 15.
(define use-of-strange-cps
  (let ([strange^ (((strange-cps 5 (empty-k)) 6 (empty-k)) 7 (empty-k))])
    (((strange^ 8 (empty-k)) 9 (empty-k)) 10 (empty-k))))
; use-of-strange-cps

;;; 16.
(define why-cps
  (lambda (f k)
    (
      (lambda (g k)
        (f (lambda (x k) (g g (lambda (v)(v x k)))) k)
      )
      (lambda (g k)
        (f (lambda (x k) (g g (lambda (v)(v x k)))) k)
      )
      k
    )
  )
)

(define almost-length-cps
    (lambda (f k)
      (k (lambda (ls k)
        (if (null? ls)
            (k 0)
            (f (cdr ls) (lambda (v) (k (add1 v)))))))))

; ((why-cps almost-length-cps (empty-k)) '(a b c d e) (empty-k))