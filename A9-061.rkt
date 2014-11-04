;;Wendy Kwok
;;A9
;;Registerize

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

;;define registers
(define-registers env num expr v c a k)

;;define pc
(define-program-counter pc)

(define value-of
  (lambda ()
    (union-case expr exp
                [(const n) (begin (set! k k)
                                  (set! v n)
                                  (app-k))]
                [(var v) (begin (set! env env)
                                (set! num v)
                                (set! k k)
                                (apply-env))]
                [(if test conseq alt) (begin (set! expr test)
                                             (set! env env)
                                             (set! k (kt_if-k conseq alt env k))
                                             (value-of))]
                [(mult rand1 rand2) (begin (set! expr rand1)
                                           (set! env env)
                                           (set! k (kt_mult-outer-k rand2 env k))
                                           (value-of))]
                [(sub1 rand) (begin (set! expr rand)
                                    (set! env env)
                                    (set! k (kt_sub1-k k))
                                    (value-of))]
                [(zero rand) (begin (set! expr rand)
                                    (set! env env)
                                    (set! k (kt_zero-k k))
                                    (value-of))]
                [(capture body)
                 (begin (set! expr body)
                        (set! env (envr_extend k env))
                        (set! k k)
                        (value-of))]
                [(return vexp kexp)
                 (begin (set! expr kexp)
                        (set! env env)
                        (set! k (kt_ret-k vexp env))
                        (value-of))]
                [(let vexp body) (begin (set! expr vexp)
                                        (set! env env)
                                        (set! k (kt_let-k body env k))
                                        (value-of))]
                [(lambda body) (begin (set! k k)
                                      (set! v (clos_closure body env))
                                      (app-k))]
                [(app rator rand)
                 (begin (set! expr rator)
                        (set! env env)
                        (set! k (kt_rator-k rand env k))
                        (value-of))])))

(define-union kt
  (rator-k rand env^ k^)
  (rand-k clos k^)
  (let-k body env^ k^)
  (ret-k vexp env^)
  (zero-k k^)
  (sub1-k k^)
  (mult-inner-k v^ k^)
  (mult-outer-k rand2 env^ k^)
  (if-k conseq alt env^ k^)
  (empty-k))


;;apply k
(define app-k
  (lambda ()
    (union-case k kt
                [(mult-outer-k rand2 env^ k^) (begin (set! expr rand2)
                                                    (set! env env^)
                                                    (set! k (kt_mult-inner-k v k^))
                                                    (value-of))]
            [(mult-inner-k v^ k^) (begin (set! k k^)
                                         (set! v (* v^ v))
                                         (app-k))]
            [(sub1-k k^) (begin (set! k k^)
                                (set! v (- v 1))
                                (app-k))]
            [(zero-k k^) (begin (set! k k^)
                                (set! v (zero? v))
                                (app-k))]
            [(ret-k vexp env^) (begin (set! expr vexp)
                                     (set! env env^)
                                     (set! k v)
                                     (value-of))]
            [(let-k body env^ k^)  (begin (set! expr body)
                                         (set! env (envr_extend v env^))
                                         (set! k k^)
                                         (value-of))]
            [(rand-k clos k^) (begin (set! c clos)
                                     (set! a v)
                                     (set! k k^)
                                     (apply-closure))]
            [(rator-k rand env^ k^) (begin (set! expr rand)
                                          (set! env env^)
                                          (set! k (kt_rand-k v k^))
                                          (value-of))]
            [(if-k conseq alt env^ k^) (if v
                                          (begin (set! expr conseq)
                                                 (set! env env^)
                                                 (set! k k^)
                                                 (value-of))
                                          (begin (set! expr alt)
                                                 (set! env env^)
                                                 (set! k k^)
                                                 (value-of)))]
            [(empty-k) v])))

(define-union envr
  (empty)
  (extend arg env^))

(define apply-env
  (lambda ()
    (union-case env^ envr
                [(empty) (begin (set! k k)
                           (set! v (error 'env "unbound variable"))
                           (app-k))]
                [(extend arg env^)
                 (if (zero? num)
                     (begin (set! k k)
                            (set! v arg)
                            (app-k))
                     (begin (set! env env^)
                            (set! num (sub1 num))
                            (set! k k)
                            (apply-env)))])))

(define-union clos
  (closure code env^))

(define apply-closure
  (lambda ()
    (union-case c clos
                [(closure code env^)
                 (begin (set! expr code)
                        (set! env (envr_extend a env^))
                        (set! k k)
                        (value-of))])))


;;should print 5
(pretty-print
 (begin  (set! expr (exp_app
                     (exp_app
                      (exp_lambda (exp_lambda (exp_var 1)))
                      (exp_const 5))
                     (exp_const 6)))
         (set! env (envr_empty))
         (set! k (kt_empty-k))
         (value-of)))

#|
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
|#