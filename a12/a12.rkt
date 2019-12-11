#lang racket


(require "monads.rkt")

;; Maybe Monad
;;; 1.
(define (findf-maybe p ls)
    (cond
        [(null? ls) (Nothing)]
        [(p (car ls)) (Just (car ls))]
        [else (findf-maybe p (cdr ls))]
    )
)

;;; (findf-maybe symbol? '(1 2 c))
;;; (findf-maybe boolean? '(#f 1 2 c))
;;; (findf-maybe number? '(a b c))

;; Writer Monad
;;; 2.
(define (partition-writer p ls)
    (cond
        [(null? ls) (inj-writer '())]
        [(p (car ls)) (bind-writer (partition-writer p (cdr ls))
                                    (lambda (v)
                                        (inj-writer (cons (car ls) v))))]
        [else (bind-writer (tell (car ls))
                            (lambda (v)
                                (partition-writer p (cdr ls))))]
    )
)
;; (run-writer (partition-writer odd? '(1 2 3 4 5 6 7 8 9 10)))

;;; 3.
(define preVal
    (lambda (r)
        (match r
            [(Writer la a) a]
        )
    )
)
(define switch
    (lambda (r nr)
        (bind-writer r
                    (lambda (v)
                        (bind-writer (tell (preVal r))
                                     (lambda (v)
                                        (inj-writer nr)))))
    )
)
(define (powerXpartials x n) 
    (cond
      [(zero? n) (inj-writer 1)]
      [(zero? (sub1 n)) (inj-writer x)]
      [(odd? n) (let ([r (powerXpartials x (sub1 n))])
                    (switch r (* x (preVal r))))]
      [(even? n) (let ((nhalf (/ n 2)))
                    (let ([r (powerXpartials x nhalf)])
                        (switch r (* (preVal r) (preVal r)))))]))
;;; (run-writer (powerXpartials 2 6))
;;; (run-writer (powerXpartials 5 7))
;;; (run-writer (powerXpartials 3 5))

;; State Monad
;;; 4.
(define (replace-with-count x tr)
    (cond
        [(symbol? tr) (if (eqv? x tr)
                          (bind-state (get)
                                      (λ (s)
                                        (bind-state (put (add1 s))
                                                    (λ (t)
                                                    (inj-state s)))))
                            (inj-state tr))]
    [(pair? (car tr))
        (bind-state (replace-with-count x (car tr))
                    (λ (a)
                        (bind-state (replace-with-count x (cdr tr))
                                    (λ (d)
                                    (inj-state (cons a d))))))]
    [(eqv? x (car tr))
        (bind-state (get)
                (λ (s)
                    (bind-state (put (add1 s))
                               (λ (t)
                                 (bind-state (replace-with-count x (cdr tr))
                                             (λ (almost)
                                               (inj-state (cons s almost))))))))]
    [else
     (bind-state (replace-with-count x (cdr tr))
                 (λ (almost)
                   (inj-state (cons (car tr) almost))))]))
 ;;;((run-state (replace-with-count 'o '(a o (t o (e o t ((n . m) . o) . f) . t) . r))) 0)

;;; 5.
(define traverse
    (lambda (inj bind f)
        (letrec
            ((trav
                (lambda (tree)
                    (cond
                        [(pair? tree)
                            (go-on ([a (trav (car tree))]
                                    [d (trav (cdr tree))])
                                (inj (cons a d)))]
                        [else (f tree)]))))
        trav)))

(define reciprocal
    (λ (n)
        (cond
            [(zero? n) (Nothing)]
            [else (Just (/ 1 n))])))

(define traverse-reciprocal
    (traverse Just bind-maybe reciprocal))
;;; (reciprocal 2)
;;; (reciprocal 0)

;;; 6.
(define halve
    (lambda (n)
        (cond
            [(even? n) (inj-writer (/ n 2))]
            [else (bind-writer (tell n)
                                (lambda (_)
                                    (inj-writer n)
                                ))]
        )
    )
)

(define traverse-halve
    (traverse inj-writer bind-writer halve))


;;; (run-writer (traverse-halve '((1 . 2) . (3 . (4 . 5)))))

;;; 7.
(define state/sum
    (lambda (n)
        (bind-state (get)
                    (lambda (s)
                        (bind-state (put (+ s n))
                                    (lambda (_)
                                        (inj-state s)
                                    ))))
    )
)

(define traverse-state/sum
    (traverse inj-state bind-state state/sum))
;;; ((run-state (traverse-state/sum '((1 . 2) . (3 . (4 . 5))))) 0)

;;; Brainteaser
;;; talked with haoran
(define value-of-cps
  (lambda (expr env)
    (match expr
        [`,n #:when (number? n) (inj-cont n)]
        [`,b #:when (boolean? b) (inj-cont b)]      
        [`,y #:when (symbol? y) (inj-cont (apply-env env y))]
        [`(* ,x1 ,x2)
            (go-on ([vx1 (value-of-cps x1 env)]
                    [vx2 (value-of-cps x2 env)])
                (inj-cont (* vx1 vx2)))]
        [`(sub1 ,x)
            (go-on ([valx (value-of-cps x env)])
                (inj-cont (sub1 valx)))]
        [`(zero? ,x)
            (go-on ([valx (value-of-cps x env)])
                (inj-cont (zero? valx)))]
        [`(if ,test ,conseq ,alt)
            (go-on ([pred (value-of-cps test env)])
                (if pred
                    (value-of-cps conseq env)
                    (value-of-cps alt env)))]
        [`(capture ,k-id ,body) (callcc (lambda (k)
                                            (value-of-cps body (extend-env k-id k env))))]
        [`(return ,k-exp ,v-exp)
            (go-on  ([vk (value-of-cps k-exp env)]
                    [vv (value-of-cps v-exp env)])
                (vk vv))]
        [`(lambda (,id) ,body)
            (inj-cont (closure id body env))]
        [`(,rator ,rand)
            (go-on ([vrator (value-of-cps rator env)]
                    [vrand (value-of-cps rand env)])
                    (apply-proc vrator vrand))])))

(define extend-env
  (λ (x a env)
    (λ (y)
      (if (eqv? y x) a
          (apply-env env y)))))

(define empty-env
  (λ ()
    (λ (y) (error "Unknown!"))))


(define closure
  (λ (x body env)
    (λ (y)
      (value-of-cps body (extend-env x y env)))))

(define apply-proc
  (λ (clos a)
    (clos a)))

(define apply-env
  (λ (env y)
    (env y)))

(define fact-5
  '((lambda (f)
      ((f f) 5))
    (lambda (f)
      (lambda (n)
        (if (zero? n)
            1
            (* n ((f f) (sub1 n))))))))

(define capture-fun
    '(* 3 (capture q (* 2 (return q 4)))))
((run-cont (value-of-cps fact-5 (empty-env))) (lambda (v) v))
((run-cont (value-of-cps capture-fun (empty-env))) (lambda (v) v))