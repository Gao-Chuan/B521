#lang racket
(require "mk.rkt")

(defrel (apply-Go G e t)
  (fresh (a G^)
    (== `(,a . ,G^) G)
    (fresh (aa da)
      (== `(,aa . ,da) a)
      (conde
       ((== aa e) (== da t))
       ((=/= aa e) (apply-Go G^ e t))))))

; G is env, e is expression, t is type
(defrel (!- G e t)
  (conde
    [(numbero e) (== 'Nat t)]
    [(== t 'Bool)
      (conde
        [(== #t e)]
        [(== #f e)])]
    [(fresh (ne)
      (== `(sub1 ,ne) e)
      (== 'Nat t)
      (!- G ne 'Nat))]
    [(fresh (teste)
      (== `(not ,teste) e)
      (== t 'Bool)
      (!- G teste 'Bool))]
    [(fresh (f l tf tl)
      (== `(cons ,f ,l) e)
      (== `(pairof ,tf ,tl) t)
      (!- G f tf)
      (!- G l tl))]
    [(fresh (ls tls_f tls_l)
      (== `(car ,ls) e)
      (== tls_f t)
      (!- G ls `(pairof ,tls_f ,tls_l)))]
    [(fresh (ls tls_f tls_l)
      (== `(cdr ,ls) e)
      (== tls_l t)
      (!- G ls `(pairof ,tls_f ,tls_l)))]
    [(fresh (ne1 ne2)
      (== `(+ ,ne1 ,ne2) e)
      (== 'Nat t)
      (!- G ne1 'Nat)
      (!- G ne2 'Nat))]
    [(fresh (ne1 ne2)
      (== `(* ,ne1 ,ne2) e)
      (== 'Nat t)
      (!- G ne1 'Nat)
      (!- G ne2 'Nat))]
    [(fresh (ne)
      (== `(zero? ,ne) e)
      (== 'Bool t)
      (!- G ne 'Nat))]
    [(fresh (teste anse elsee)
      (== `(if ,teste ,anse ,elsee) e)
      (!- G teste 'Bool)
      (!- G anse t)
      (!- G elsee t))]
    [(fresh (vn vt v exp)
      (== `(let ([,vn ,v]) ,exp) e)
      (symbolo vn)
      (!- `((,vn . ,vt) . ,G) exp t)
      (!- G v vt))]
    [(fresh (exp)
      (== `(fix ,exp) e)
      (!- G exp `(,t -> ,t)))]
    [(symbolo e) (apply-Go G e t)]
    ((fresh (x b)
        (== `(lambda (,x) ,b) e)
        (symbolo x)
        (fresh (tx tb)          
          (== `(,tx -> ,tb) t)
          (!- `((,x . ,tx) . ,G) b tb))))
    [(fresh (e1 arg)
      (== `(,e1 ,arg) e)
      (fresh (targ)
        ;; wrap arg's typw into t
        (!- G e1 `(,targ -> ,t))
        (!- G arg targ)))]))

;; test
; (run* q (!- '() #t q))
; (run* q (!- '() 17 q))
; (run* q (!- '() '(zero? 24) q))
; (run* q (!- '() '(zero? (sub1 24)) q))
; (run* q (!- '() '(not (zero? (sub1 24))) q))
; (run* q
;     (!- '() '(zero? (sub1 (sub1 18))) q))
; (run* q
;     (!- '()  '(lambda (n) (if (zero? n) n n)) q))
; (run* q
;     (!- '()  '(lambda (n) (if (zero? n) n #f)) q))
; (run* q
;     (!- '() '((lambda (n) (zero? n)) 5) q))
; (run* q
;     (!- '() '(if (zero? 24) 3 4) q))
; (run* q
;     (!- '() '(if (zero? 24) (zero? 3) (zero? 4)) q))
; (length (run 500 q (!- '() q '(Nat -> Nat))))
; (run* t (!- '() '(lambda (x) (zero? (car x))) t))
; (run* t (!- '() '((lambda (x) (zero? (car x))) (cons 0 1)) t))
; (run* t (!- '() '((lambda (x) (zero? (car x))) (cons 0 #f)) t))
; (run* t (!- '() '((lambda (x) (car x)) (cons (cons 0 0) #f)) t)) 
; (run* t (!- '() '((lambda (x) (zero? (car x))) (cons #f 0)) t))
; (run* t (!- '() '(lambda (x) (zero? (cdr x))) t))
; (run* t (!- '() '((lambda (x) (zero? (cdr x))) (cons 0 1)) t))
; (run* t (!- '() '((lambda (x) (zero? (cdr x))) (cons 0 #f)) t))
; (run* t (!- '() '((lambda (x) (zero? (cdr x))) (cons #f 0)) t))
;; Why does not work??
; (run* q
;     (!- '() '(let ([f (lambda (x) #t)])
;                (if #t (f (f 5)) (f #t)))
;         q))
; (run* q
;     (!- '() '(let ([f (lambda (x) x)])
;                        (if (f #t) (f (cons (f 4) 5)) (f (cons 5 (f 6)))))
;          q))