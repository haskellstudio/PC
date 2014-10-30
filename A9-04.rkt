;;Wendy Kwok
;;A9
;;continuation to unions

#lang racket

(require C311/pmatch)
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
                [(const n) (app-k k n)]
                [(var v) (apply-env env v k)]
                [(if test conseq alt)
                 (value-of test env (kt_if-k conseq alt env k))]
                [(mult rand1 rand2) (value-of rand1 env (kt_mult-outer-k rand2 env k))]
                [(sub1 rand) (value-of rand env (kt_sub1-k k))]
                [(zero rand) (value-of rand env (kt_zero-k k))]
                [(capture body)
                 (value-of body (envr_extend k env) k)]
                [(return vexp kexp)
                 (value-of kexp env (kt_ret-k vexp env))]
                [(let vexp body) (value-of vexp env (kt_let-k body env k))]
                [(lambda body) (app-k k (clos_closure body env))]
                [(app rator rand)
                 (value-of rator env (kt_rator-k rand env k))])))

(define-union kt
  (rator-k rand env k)
  (rand-k clos k)
  (let-k body env k)
  (ret-k vexp env)
  (zero-k k)
  (sub1-k k)
  (mult-inner-k v^ k)
  (mult-outer-k rand2 env k)
  (if-k conseq alt env k)
  (empty-k))

;;apply k
(define app-k
  (lambda (k v)
    (union-case k kt
            [(mult-outer-k rand2 env k) (value-of rand2 env (kt_mult-inner-k v k))]
            [(mult-inner-k v^ k) (app-k k (* v^ v))]
            [(sub1-k k) (app-k k (- v 1))]
            [(zero-k k) (app-k k (zero? v))]
            [(ret-k vexp env) (value-of vexp env v)]
            [(let-k body env k) (value-of body (envr_extend v env) k)]
            [(rand-k clos k) (apply-closure clos v k)]
            [(rator-k rand env k) (value-of rand env (kt_rand-k v k))]
            [(if-k conseq alt env k) (if v
                                         (value-of conseq env k)
                                         (value-of alt env k))]
            [(empty-k) v])))

(define-union envr
  (empty)
  (extend arg env))

(define apply-env
  (lambda (env num k)
    (union-case env envr
                [(empty) (app-k k (error 'env "unbound variable"))]
                [(extend arg env)
                 (if (zero? num)
                     (app-k k arg)
                     (apply-env env (sub1 num) k))])))

(define-union clos
  (closure code env))

(define apply-closure
  (lambda (c a k)
    (union-case c clos
                [(closure code env)
                 (value-of code (envr_extend a env) k)])))

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
