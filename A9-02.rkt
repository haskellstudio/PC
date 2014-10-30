;;Wendy Kwok
;;A9
;;Representation Independent with continuations

;;nothing done yet
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
                [(const n) (app-k k n)]
                [(var v) (apply-env env v k)]
                [(if test conseq alt)
                 (value-of test env (lambda (t)
                                      (if t
                                          (value-of conseq env k)
                                          (value-of alt env k))))]
                [(mult rand1 rand2) (value-of rand1 env (mult-outer-k rand2 env k))]
                [(sub1 rand) (value-of rand env (sub1-k k))]
                [(zero rand) (value-of rand env (zero-k k))]
                [(capture body)
                 (value-of body (envr_extend k env) k)]
                [(return vexp kexp)
                 (value-of kexp env (ret-k vexp env))]
                [(let vexp body) (value-of vexp env (let-k body env k))]
                [(lambda body) (app-k k (clos_closure body env))]
                [(app rator rand)
                 (value-of rator env (rator-k rand env k))])))

;;rator-k
(define rator-k
  (lambda (rand env k)
    (lambda (clos)
      (value-of rand env (rand-k clos k)))))


;;rand-k
(define rand-k
  (lambda (clos k)
    (lambda (a)
      (apply-closure clos a k))))


;;does let?
(define let-k
  (lambda (body env k)
    (lambda (v)
      (value-of body (envr_extend v env) k))))

;;does return
(define ret-k
  (lambda (vexp env)
    (lambda (k^)
      (value-of vexp env k^))))

;;zero l
(define zero-k
  (lambda (k)
    (lambda (n)
      (app-k k (zero? n)))))

;;sub1 k
(define sub1-k
  (lambda (k)
    (lambda (n)
      (app-k k (- n 1)))))

;;mult inner and outer k
(define mult-inner-k
  (lambda (n1 k)
    (lambda (n2)
      (app-k k (* n1 n2)))))

(define mult-outer-k
  (lambda (rand2 env k)
    (lambda (n1)
      (value-of rand2 env (mult-inner-k n1 k))) ))

;;apply k
(define app-k
  (lambda (k^ v^)
    (k^ v^)))

(define-union envr
  (empty)
  (extend arg env))

(define apply-env
  (lambda (env num k)
    (union-case env envr
                [(empty) (k (error 'env "unbound variable"))]
                [(extend arg env)
                 (if (zero? num)
                     (k arg)
                     (apply-env env (sub1 num) k))])))

(define-union clos
  (closure code env))

(define apply-closure
  (lambda (c a k)
    (union-case c clos
                [(closure code env)
                 (value-of code (envr_extend a env) k)])))


(pretty-print
 (value-of (exp_app
            (exp_app
             (exp_lambda (exp_lambda (exp_var 1)))
             (exp_const 5))
            (exp_const 6))
           (envr_empty) (lambda (v)
                          v)))

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
           (envr_empty) (lambda (v)
                          v)))

                                        ; Test of capture and return...should evaluate to 24.
(pretty-print
 (value-of
  (exp_mult (exp_const 2)
            (exp_capture
             (exp_mult (exp_const 5)
                       (exp_return (exp_mult (exp_const 2) (exp_const 6))
                                   (exp_var 0)))))
  (envr_empty) (lambda (v)
                 v)))


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
           (envr_empty) (lambda (v)
                          v)))