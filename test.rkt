#lang racket

; (define fact/k
;     (lambda (n k)
;         (cond
;             [(zero? n) (let/cc f (k f))]
;             [else (* n (fact/k (sub1 n) k))]
;         )
;     )
; )

; (define five (let/cc k (fact/k 5 k)))

; (five 1)

; five


(define mult-inner-k
  (λ(v^ k^)
    (λ(v)
      (apply-k k^ (* v^ v)))))

(define mult-outer-k
  (λ(x2^ env^ k^)
    (λ(v)
      (value-of-cps x2^ env^ (mult-inner-k v k^)))))

(define sub1-inner-k
  (λ(k^)
    (λ(v)
      (apply-k k^ (sub1 v)))))

(define zeor-inner-k
  (λ(k^)
    (λ(v)
      (apply-k k^ (zero? v)))))

(define if-inner-k
  (λ(conseq^ alt^ env^ k^)
    (λ(predictResult)
      (if predictResult
          (value-of-cps conseq^ env^ k^)
          (value-of-cps alt^    env^ k^)))))

(define throw-inner-k
  (λ(v-exp^ env^)
    (λ(old-k)
      (value-of-cps v-exp^ env^ old-k))))

(define let-inner-k
  (λ(body^ env^ k^)
    (λ(valueAddToEnv)
      (value-of-cps body^
                    (extend-env env^ valueAddToEnv )
                    k^))))


(define app-inner-k
  (λ(v1^ k^)
    (λ(v2)
      (apply-closure v1^ v2 k^))))

(define app-outer-k
  (λ(rand^ env^ k^)
    (λ(v1)
      (value-of-cps rand^ env^ (app-inner-k v1 k^)))))
(define value-of-cps
  (λ(expr env k)
    (pmatch expr
            [`(const ,v) (apply-k k v)]

            [`(mult ,x1 ,x2) (value-of-cps x1 env (mult-outer-k x2 env k)
                                           #|(λ(v1)
                                                    (value-of-cps x2 env (λ(v2)
                                                                           (apply-k k (* v1 v2)))))|#)]

            [`(sub1 ,x) (value-of-cps x env (sub1-inner-k k)
                                      #|(λ(v)
                                              (apply-k k (sub1 v)))|#)]

            [`(zero ,x) (value-of-cps x env (zeor-inner-k k)
                                      #|(λ(v)
                                              (apply-k k (zero? v)))|#)]

            [`(if ,test ,conseq ,alt) (value-of-cps test env  (if-inner-k conseq alt env k)
                                                    #|(λ(v)
                                                               (if v
                                                                   (value-of-cps conseq env (λ(trueValue)
                                                                                              (apply-k k trueValue)))
                                                                   (value-of-cps alt    env (λ(altValue)
                                                                                              (apply-k k altValue)))))|#)]
 
            [`(letcc ,body) (value-of-cps body
                                          (extend-env env k )  ;這裏第二個參數是要保存的k~ 
                                          k          ;如果这里返回了，就表示 接下来的body中包含throw， 没有调用这里k。
                                          #|这个lambda可以直接写成 k |#)]

            [`(throw ,k-exp ,v-exp) (value-of-cps k-exp
                                                  env
                                                  (throw-inner-k v-exp env)
                                                  #| (λ(k^)  ; 這裏 k-exp 是個k變量， 是一個以前保存的continuation
                                                              (value-of-cps v-exp env (λ(v)
                                                                                        (k^ v))))|#)] ;这里最重要，使用保存下来的k，而不是使用当前的k。  這裏應該用 apply-k ??

            [`(let ,e ,body)  (value-of-cps e
                                            env
                                            (let-inner-k body env k)
                                            #|(λ(v)
                                              (value-of-cps body
                                                            (extend-env env v )
                                                            k))|#)]
   

            [`(var ,expr)   (apply-env env expr k)]

            [`(lambda ,body) (apply-k k (make-closure body env))]
 
            [`(app ,rator ,rand) (value-of-cps rator env (app-outer-k rand env k) #|(λ(v1)
                                                           (value-of-cps rand env (λ(v2)
                                                                                    (apply-closure v1 v2 k))))|#)])))


#|
(define make-closure
  (λ(body env)
    (λ(valueWillBeApplied k^)
      (value-of-cps body
                    (extend-env env valueWillBeApplied )
                    k^))))
|#


(define make-closure
  (λ(body env)
    `(closure-place-holder ,body ,env)))


#|
(define apply-closure
  (λ (v1 v2 k)
    (v1 v2 k)))
|#

(define apply-closure
  (λ (closure valueWillBeApplied k)
    (match closure
      [`(closure-place-holder ,body ,env)  (value-of-cps body (extend-env env valueWillBeApplied ) k)])))

#|
(define empty-env
  (lambda ()
    (lambda (y k^)
      (error 'value-of "unbound identifier"))))
|#
                                                                                    
(define empty-env
  (λ()
    `(empty-env-holder)))



#|
(define extend-env
  (λ(oldEnv valueAddToEnv )
    (λ(varNumberToQuery k^)
      (if (zero? varNumberToQuery)
          (apply-k k^ valueAddToEnv)
          (oldEnv (sub1 varNumberToQuery) k^)))))
|#

(define extend-env
  (λ(oldEnv valueAddToEnv)
    `(extend-env-holder ,oldEnv ,valueAddToEnv)))


#|
(define apply-env
  (λ(env varNumberToQuery k)
    (env varNumberToQuery k)))
|#


(define apply-env
  (λ(env varNumberToQuery k)
    (match env
      [`(empty-env-holder) (error `value-of-cps "unbound identifier")]
      [`(extend-env-holder ,oldEnv ,valueAddToEnv) (if (zero? varNumberToQuery)
                                                       (apply-k k valueAddToEnv)
                                                       (apply-env oldEnv (sub1 varNumberToQuery) k))])))




(define apply-k
  (λ(k v)
    (k v)))

  
(define empty-k
  (lambda ()
    (lambda (v)
      v)))

