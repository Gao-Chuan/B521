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
