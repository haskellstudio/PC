;;Wendy Kwok
;;A9
;;Anormalize

#lang racket

(require "parenthec.rkt")

(define-union exp
  (const n)
  (var v)
  (if test conseq alt)
  (mult rand1 rand2)
  (sub1 rand)
  (zero rand)
  (capture body)
  (return vexp kexp)
  (let vexp body)
  (lambda body)
  (app rator rand))

(define value-of
  (lambda (expr env k)
    (union-case expr exp
                [(const n) (let* ([k k]
                                  [v n]) (app-k k v))]
                [(var v) (let* ([env env]
                                [num v]
                                [k k]) (apply-env env num k))]
                [(if test conseq alt) (let* ([expr test]
                                             [env env]
                                             [k (kt_if-k conseq alt env k)])
                                        [value-of expr env k])]
                [(mult rand1 rand2) (let* ([expr rand1]
                                           [env env]
                                           [k (kt_mult-outer-k rand2 env k)])
                                      (value-of expr env k))]
                [(sub1 rand) (let* ([expr rand]
                                    [env env]
                                    [k (kt_sub1-k k)])
                               (value-of expr env k))]
                [(zero rand) (let* ([expr rand]
                                    [env env]
                                    [k (kt_zero-k k)])
                               (value-of expr env k))]
                [(capture body)
                 (let* ([expr body]
                        [env (envr_extend k env)]
                        [k k])
                   (value-of expr env k))]
                [(return vexp kexp)
                 (let* ([expr kexp]
                        [env env]
                        [k (kt_ret-k vexp env)])
                   (value-of expr env k))]
                [(let vexp body) (let* ([expr vexp]
                                        [env env]
                                        [k (kt_let-k body env k)])
                                   (value-of expr env k))]
                [(lambda body) (let* ([k k]
                                      [v (clos_closure body env)])
                                 (app-k k v))]
                [(app rator rand)
                 (let* ([expr rator]
                        [env env]
                        [k (kt_rator-k rand env k)])
                   (value-of expr env k))])))

(define-union kt
  (rator-k rand env k^)
  (rand-k clos k^)
  (let-k body env k^)
  (ret-k vexp env)
  (zero-k k^)
  (sub1-k k^)
  (mult-inner-k v^ k^)
  (mult-outer-k rand2 env k^)
  (if-k conseq alt env k^)
  (empty-k))

;;apply k
(define app-k
  (lambda (k v)
    (union-case k kt
                [(mult-outer-k rand2 env k^) (let* ([expr rand2]
                                                    [env env]
                                                    [k (kt_mult-inner-k v k^)])
                                               (value-of expr env k))]
            [(mult-inner-k v^ k^) (let* ([k k^]
                                         [v (* v^ v)])
                                    (app-k k v))]
            [(sub1-k k^) (let* ([k k^]
                                [v (- v 1)])
                           (app-k k v))]
            [(zero-k k^) (let* ([k k^]
                                [v (zero? v)])
                           (app-k k v))]
            [(ret-k vexp env) (let* ([expr vexp]
                                     [env env]
                                     [k v])
                                (value-of expr env k))]
            [(let-k body env k^)  (let* ([expr body]
                                         [env (envr_extend v env)]
                                         [k k^])
                                    (value-of expr env k))]
            [(rand-k clos k^) (let* ([c clos]
                                     [a v]
                                     [k k^])
                                (apply-closure c a k))]
            [(rator-k rand env k^) (let* ([expr rand]
                                          [env env]
                                          [k (kt_rand-k v k^)])
                                     (value-of expr env k))]
            [(if-k conseq alt env k^) (if v
                                          (let* ([expr conseq]
                                                 [env env]
                                                 [k k^])
                                            (value-of expr env k))
                                          (let* ([expr alt]
                                                 [env env]
                                                 [k k^])
                                            (value-of expr env k)))]
            [(empty-k) v])))

(define-union envr
  (empty)
  (extend arg env))

(define apply-env
  (lambda (env num k)
    (union-case env envr
                [(empty) (let* ([k k]
                                [v (error 'env "unbound variable")])
                           (app-k k v))]
                [(extend arg env)
                 (if (zero? num)
                     (let* ([k k]
                            [v arg])
                       (app-k k v))
                     (let* ([env env]
                            [num (sub1 num)]
                            [k k])
                       (apply-env env num k)))])))

(define-union clos
  (closure code env))

(define apply-closure
  (lambda (c a k)
    (union-case c clos
                [(closure code env)
                 (let* ([expr code]
                        [env (envr_extend a env)]
                        [k k])
                   (value-of expr env k))])))

;;should print 5
(pretty-print
 (value-of (exp_app
            (exp_app
             (exp_lambda (exp_lambda (exp_var 1)))
             (exp_const 5))
            (exp_const 6))
           (envr_empty) (kt_empty-k)))

; Factorial of 5...should be 120.
(pretty-print
 (value-of (exp_app
            (exp_lambda
             (exp_app
              (exp_app (exp_var 0) (exp_var 0))
              (exp_const 5)))
            (exp_lambda
             (exp_lambda
              (exp_if (exp_zero (exp_var 0))
                      (exp_const 1)
                      (exp_mult (exp_var 0)
                                (exp_app
                                 (exp_app (exp_var 1) (exp_var 1))
                                 (exp_sub1 (exp_var 0))))))))
           (envr_empty) (kt_empty-k)))
; Test of capture and return...should evaluate to 24.
(pretty-print
 (value-of
  (exp_mult (exp_const 2)
            (exp_capture
             (exp_mult (exp_const 5)
                       (exp_return (exp_mult (exp_const 2) (exp_const 6))
                                   (exp_var 0)))))
  (envr_empty) (kt_empty-k)))


(pretty-print
 (value-of (exp_let
            (exp_lambda
             (exp_lambda
              (exp_if
               (exp_zero (exp_var 0))
               (exp_const 1)
               (exp_mult
                (exp_var 0)
                (exp_app
                 (exp_app (exp_var 1) (exp_var 1))
                 (exp_sub1 (exp_var 0)))))))
            (exp_app (exp_app (exp_var 0) (exp_var 0)) (exp_const 5)))
           (envr_empty) (kt_empty-k)))
