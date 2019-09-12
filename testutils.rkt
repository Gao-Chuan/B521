#lang racket

(require rackunit
         (for-syntax syntax/parse))

(provide check-answers*
         test-suite)

(define-syntax (test-suite stx)
  (syntax-parse stx #:datum-literals (test-equal-if-defined)
    [(_ id
        (test-equal-if-defined f
                               [program expect] ...))
     ;;
     (if (identifier-binding #'f)
         #'(begin (println (~a "testing " id))
                  (check-tests [program expect]) ...)
         #'(begin (println (~a "not testing " id " because it is not defined"))
                  (void)))]))


(define-syntax (check-tests stx)
  (syntax-parse stx
    [(_) #'(println "passed")]
    [(_ [program expect] [p2 e2] ...)
     #'(begin (check-equal? program expect)
              (check-tests [p2 e2] ...))]))


(define-syntax (check-answers* stx)
  (syntax-parse stx
    [(_ testsuite) #'testsuite]
    [(_ testsuite testsuite2 ...)
     #'(begin testsuite
              (check-answers* testsuite2 ...))]))