;;Wendy Kwok
;;A9
;;step 01
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


;; got certain to cps, dont know a few
(define value-of
  (lambda (expr env k)
    (union-case expr exp
                [(const n) (k n)] ;;
                [(var v) (k (apply-env env v))] ;;
                [(if test conseq alt)
                 (if (value-of test env (lambda (m)
                                          (value-of conseq env k)
                                          (value-of alt env k) )))] ;;                                                       
                [(mult rand1 rand2) (value-of rand1 env (lambda (m)
                                                          (value-of rand env (lambda (e)
                                                                               (k (* m e))))))] ;;
                [(sub1 rand) (value-of rand env (lambda (m)
                                                  (k (- m 1))))] ;;
                [(zero rand) (value-of rand env (lambda (m)
                                                  (k (zero? m))))] ;;
                [(capture body)
                 (call/cc
                  (lambda (k)
                    (value-of body (envr_extend k env))))] ;;idk
                [(return vexp kexp) (value-of kexp env (lambda (kexp)
                                                         (value-of vexp env kexp)))] ;;
                [(let vexp body)
                 (let ((v (value-of vexp env)))
                   (value-of body (envr_extend v env)))] ;;idk
                [(lambda body) (clos_closure body env)]
                [(app rator rand) (value-of rator env (lambda (m)
                                                        (value-of rand env (lambda (p)
                                                                             (k (apply-closure m p))))))] ;;
;;dont need to cps this, obviously simple. and like another tagg list for apply-env
(define-union envr
  (empty)
  (extend arg env))

;;no cps
(define apply-env
  (lambda (env num)
    (union-case env envr
                [(empty) (error 'env "unbound variable")]
                [(extend arg env)
                 (if (zero? num)
                     arg
                     (apply-env env (sub1 num)))])))

;;like a tagged list, like in apply env?
(define-union clos
  (closure code env))

(define apply-closure
  (lambda (c a)
    (union-case c clos
                [(closure code env)
                 (value-of code (envr_extend a env))])))

; Basic test...should be 5.
#|(pretty-print
 (value-of (exp_app
            (exp_app
             (exp_lambda (exp_lambda (exp_var 1)))
             (exp_const 5))
            (exp_const 6))
           (envr_empty)))

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
           (envr_empty)))

; Test of capture and return...should evaluate to 24.
(pretty-print
 (value-of
  (exp_mult (exp_const 2)
            (exp_capture
             (exp_mult (exp_const 5)
                       (exp_return (exp_mult (exp_const 2) (exp_const 6))
                                   (exp_var 0)))))
  (envr_empty)))

;; (let ([fact (lambda (f)
;;               (lambda (n)
;;                 (if (zero? n)
;;                     1
;;                     (* n ((f f) (sub1 n))))))])
;;   ((fact fact) 5))

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
           (envr_empty)))
 

|#