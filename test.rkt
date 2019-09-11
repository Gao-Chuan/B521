#lang racket

;;; (define + (lambda (n m) (
;;;     cond
;;;     [(zero? m) n]
;;;     [else (add 1 (+ n (sub1 m)))]
;;; )))

;;; (define *
;;;     (lambda (n m)
;;;         (cond
;;;             [(zero? m) 0]
;;;             [else (* n (sub1 m))]
;;;         )    
;;;     )
;;; )

;;; Environment
(define table
    (lambda (var)
        (match var
            [`a 'ant]
            [`b 'boar]
            [`c 'cat]
            [`d 'elephant])))

;;; (define apply-env
;;;     (lambda (env var)
;;;         (env var)))

;;; (define ext-env
;;;   (lambda (env var val)
;;;     (lambda (y)
;;;       (if (eqv? y var) val (env y)))))

;;;  (define table2
;;;     (ext-env table 'e 'dog))

;;; (apply-env table2 'e)
;;; (apply-env table2 'a)

(define init-env
    (lambda ()
        '()))

(define ext-env
    (lambda (env x val)
        `((,x . , val) . ,env)))

(define apply-env
    (lambda (env var)
        (match env
            ['() (error "whatever")]
            [`((,x . , val) . ,env)
                (if (eqv? x var)
                    val
                    (apply-env env var))])))

(define table1 
    (ext-env (init-env) 'd 'elephent))

(define table2
    (ext-env table1 'a 'test))


(apply-env table2 'a)
(apply-env table2 'd)

;;; to improve is to change, so to be perfect is to have changed often? is it true?
