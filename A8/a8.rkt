#lang racket

(define apply-rk
    (lambda ()
        (match rk
            ['(empty-k) rv]
            [`(ack-ck ,m ,k) (begin (set! rm (sub1 m))
                                    (set! rn rv)
                                    (set! rk k)
                                    (ack-reg))]
            [`(depth-ck ,ls ,k) (begin (set! rls (cdr ls))
                                       (set! rk k)
                                       (set! rk (depth-ck-in))
                                       (depth-reg))]
            [`(depth-ck-in ,l ,k) (let [(l (add1 l))]
                                    (if (< l rv)
                                        (begin (set! rk k)
                                            (apply-rk))
                                        (begin (set! rv l)
                                            (set! rk k)
                                            (apply-rk))))]
            [`(fact-ck ,n ,k) (begin (set! rv (* n rv))
                                     (set! rk k)
                                     (apply-rk))]
            [`(pascal-init ,k) (begin (set! rm 1)
                                      (set! ra 0)
                                      (set! rk k)
                                      (rv))]
            [`(pascal-ck ,m ,k) (begin (set! rm (add1 rm))
                                          (set! rk k)
                                          (set! rk (pascal-ck-in))
                                          (rv))]
            [`(pascal-ck-in ,k ,a) (begin (set! ra a)
                                        (set! rv (cons ra rv))
                                        (set! rk k)
                                        (apply-rk))]
        )
    )
)

; 1. ack

(define empty-k
    (lambda()
        '(empty-k)
    )
)

(define rm #f)
(define rn #f)
(define rk #f)
(define rv #f)

(define ack-ck
    (lambda ()
        `(ack-ck ,rm ,rk)
    )
)

(define ack-reg
    (lambda ()
        (cond 
            [(zero? rm) (begin (set! rv (add1 rn)) (apply-rk))]
            [(zero? rn) (begin (set! rm (sub1 rm))
                              (set! rn 1)
                              (ack-reg))]
            [else (begin (set! rn (sub1 rn))
                         (set! rk (ack-ck))
                         (ack-reg))]
        )
    )
)

(define ack-reg-driver
    (lambda (m n)
        (begin
            (set! rm m)
            (set! rn n)
            (set! rk (empty-k))
            (ack-reg))
    )
)

; (ack-reg-driver 2 2)

; 2. depth
(define rls #f)

(define depth-ck
    (lambda ()
        `(depth-ck ,rls ,rk)
    )
)

(define depth-ck-in
    (lambda ()
        `(depth-ck-in ,rv ,rk)
    )
)

(define depth-reg
    (lambda ()
        (cond
            [(null? rls) (begin (set! rv 1)
                            (apply-rk))]
            [(pair? (car rls)) 
                (begin  (set! rk (depth-ck))
                        (set! rls (car rls))
                        (depth-reg))]
            [else (begin (set! rls (cdr rls)) 
                         (depth-reg))]
        )
    )
)

(define depth-reg-driver
    (lambda (ls)
        (begin (set! rls ls)
               (set! rk (empty-k))
               (depth-reg))
    )
)

; (depth-reg-driver '(1 (2 (3 (4)))))

; 3. fact
(define rfact #f)

(define fact-ck
    (lambda ()
        `(fact-ck ,rn ,rk)
    )
)

(define fact-reg
    (lambda ()
        (begin (set! rfact (lambda ()
                                (cond
                                    [(zero? rn) (begin (set! rv 1)
                                                        (apply-rk))]
                                    [else (begin (set! rk (fact-ck))
                                                 (set! rn (sub1 rn))
                                                 (rfact))])
                            ))
                (rfact))
    )
)

(define fact-reg-driver
    (lambda (n)
        (begin (set! rn n)
               (set! rk (empty-k))
               (fact-reg))
    )
)

; (fact-reg-driver 5)

; pascal
(define rpascal #f)
(define ra #f)

(define pascal-ck-in
    (lambda()
        `(pascal-ck-in ,rk ,ra)
    )
)

(define pascal-ck
    (lambda()
        `(pascal-ck ,rm ,rk)
    )
)

(define pascal-init
    (lambda()
        `(pascal-init ,rk)
    )
)

(define pascal-reg
    (lambda ()
        (begin 
            (set! rpascal (lambda ()
                            (begin
                                (set! rv (lambda ()
                                            (cond
                                                [(> rm rn) (begin
                                                                (set! rv '())
                                                                (apply-rk))]
                                                [else (begin
                                                        (set! ra (+ ra rm))
                                                        (set! rk (pascal-ck))
                                                        (rpascal)
                                                      )]
                                            )
                                        ))
                                (apply-rk)
                            )
                          ))
            (set! rk (pascal-init))
            (rpascal)
        )
    )
)

(define pascal-reg-driver
    (lambda (n)
        (begin (set! rn n)
                (set! rk (empty-k))
                (pascal-reg))
    )
)
; (pascal-reg-driver 10)

; Brainteaser
(define fib-cps
  (lambda (n k)
  (lambda()
    (cond
      [(and (not (negative? n)) (< n 2)) (k n)]
      [else (fib-cps (sub1 n) (lambda (v) 
                                (fib-cps (sub1 (sub1 n)) (lambda (t)
                                                            (k (+ v t))))))]))))

(define ramp-empty-k
    (lambda (k) k)
)

(define rampoline
    (lambda(t1 t2 t3)
        (let ([r (random 3)])
            (match r
                [0 (rampoline (t1) t2 t3)]
                [1 (rampoline t1 (t2) t3)]
                [2 (rampoline t1 t2 (t3))]
            )
        )
    )
)

(define fib-ramp-driver
  (lambda (n1 n2 n3)
    (let/cc jumpout
      (rampoline
        (lambda ()
          (fib-cps n1 (ramp-empty-k jumpout)))
        (lambda ()
          (fib-cps n2 (ramp-empty-k jumpout)))
        (lambda ()
          (fib-cps n3 (ramp-empty-k jumpout)))))))
; (require racket/trace)
; (trace fib-cps)

; (fib-ramp-driver 6 -1 -1)

; Just Dessert

