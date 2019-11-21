#lang racket
(require "./a10/mk.rkt")

(defrel (⊢ Γ e τ)
  #|
    how to pronounce it:
    from gamma, we can derive e has type tau
  |#
  (condᵉ
    [(numbero e) (== τ 'Nat)]
    [(fresh (nexp)
      (== `(zero? ,nexp) e)
      (⊢ Γ nexp 'Nat)
      (== τ 'Bool))]
    [(fresh (nexp₁ nexp₂ )
      (== `(+ ,nexp₁ ,nexp₂))
      (⊢ Γ nexp₁ 'Nat)
      (⊢ Γ nexp₂ 'Nat)
      (== τ 'Nat)
      )]
    [(fresh (conseq alter ) 
      (== (if ,test ,conseq ,alter) e))
      (⊢ Γ test 'Bool)
      (⊢ Γ conseq τ )
      (⊢ Γ alter τ )]
    [(sumbolᵒ e)
      (lookupᵒ Γ e τ )]
    [(fresh (x body)
      (== `(λ (,x) ,body) e))
      (fresh (x body)
        (== `(λ (,x) ,body) e)
          (fresh (τx τbody)
            (⊢ `((,x . ,τx ) . ,Γ ) body τbody)
            (== `(→ ,τx ,τbody) τ )))]
    [(fresh (rator rand)
      (== `(,rator ,rand) e)
      (fresh (τ x τ body)
        (⊢ Γ rator `(→ ,τx ,τ )
        (⊢ Γ rand τx))))]
    ))

#;
(define (lookup env var)
  (cond
    [(null? env) (error ...)]
    [else (match-let ([`((,x . ,t) . ,d) env])
            (if (eqv? var x)
              t
              (lookup d var)))]))

(defrel (lookupᵒ env var val)
  (condᵉ
    [(fresh (x t d)
      (== `((,x . ,t) . ,d) env)
      (condᵉ
        [(== var x) (== val )]
        [((=\= var x) (lookupᵒ d var val))]))]))  

(run 1 τ
  ⊢ '() '(zero? (+1 5)) τ )
(run 1 τ
  ⊢ '() '(if (zero? (+1 5)) 5 6) τ )
(run 1 τ
  ⊢ '() '(if (zero? (+1 5)) 5 (zero? 6)) τ )
(run 1 τ
  (⊢ '((x . Nat) (y . Bool) (z . Nat)) 'x τ ))
(run 1 τ
  (⊢ '((x . Nat) (y . Bool) (z . Nat)) 'x τ )) ;; return ('nat 'Bool). But we only want a 'nat
(run* τ
  (⊢ '() `(λ (x) (+ x x)) `(→ Nat Nat)))
(run* τ
  (⊢ '() `(λ (x) (+ x x)) τ ))
(run* τ
  (⊢ '() `((λ (x) (+ x x)) 5) τ ))
(run 10 e
  (⊢ '() e `(→ Nat Nat)))

|; 2017 ICFP paper for this with title "xxx tutorial"
#|STLC
Pro|#

;;; #|
;;;   Γ ⊢ a : A Γ ⊢ d : D 
;;;   ---------------------
;;;   Γ ⊢ (cons a d) : (Pair A D)
;;; |#
;;; [(fresh (a d A D)
;;;   (== `(cons ,a ,d) ,e)
;;;   (== `(pair ,A ,D) τ )
;;;   (⊢ Γ a A)
;;;   (⊢ Γ d D))]

;;; #|
;;;   Γ ⊢ pr : (Pair A D)
;;;   ----------------------
;;;   Γ ⊢ (car pr) :A
;;; |#
;;; [(fresh (pr D)
;;;   (== `(car pr) e)
;;;   (⊢ Γ pr `(Pair ,τ ,D)))]
;;; #|
;;;   Γ ⊢ pr : (Pair A D)
;;;   ----------------------
;;;   Γ ⊢ (car pr) :D
;;; |#
;;; [(fresh (pr A)
;;;   (== `(cdr ,pr)e)
;;;   (⊢ Γ pr `(Pair ,A ,τ )))]

;;; #|
;;;   ----------------------
;;;   Γ ⊢ (match  e  [p₁ r₁ ] [p₂ r₂]) : R
;;; |#

