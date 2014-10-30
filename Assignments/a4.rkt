;;a4
;;Wendy Kwok
#lang racket
(require C311/pmatch)

;;PART I
;;value-of-fn

;;empty fn, but fn changed to normal named
(define empty-env
  (lambda ()
    (lambda (y) (error 'value-of "unbound variable ~s" y))))


;;extend fn
(define extend-env
  (lambda (x z env)
    (lambda (y)
      (if (eqv? y x) z (env y)))))

;;apply fn
(define apply-env
  (lambda (env y)
    (env y)))


;;apply-closure-fn
(define apply-closure-fn
  (lambda (p a)
    (p a)))

;;closure-fn
(define closure-fn
  (lambda (x body env)
    (lambda (a)
      (value-of-fn body (extend-env x a env)))))

;;value-of
(define value-of-fn
  (lambda (expr env)
    (pmatch expr
            [`,n (guard (number? n)) n]
            [`,y (guard (symbol? y)) (apply-env env y)]
            [`(lambda (,x) ,body) (closure-fn x body env)]
            [`(let ((,a ,b)) ,body) (let ((c (value-of-fn b env))) (value-of-fn body (extend-env a c env)))]
            [`(if ,cond ,true ,false) (if (value-of-fn cond env) (value-of-fn true env) (value-of-fn false env))]
            [`(zero? ,arg) (zero? (value-of-fn arg env))]
            [`(sub1 ,num) (sub1 (value-of-fn num env))]
            [`(* ,n1 ,n2) (* (value-of-fn n1 env) (value-of-fn n2 env))]
            [`(,rator ,rand) (apply-closure-fn (value-of-fn rator env) (value-of-fn rand env))])))




;;value-of-ds
;;apply-closure-ds
(define apply-closure-ds
  (lambda (p a)
    (pmatch p
            [`(closure ,x ,body ,env) (value-of-ds body (extend-env x a env)) ])))

;;closure-ds
(define closure-ds
  (lambda (x body env)
    `(closure ,x ,body ,env)))

;;valof-ds
(define value-of-ds
  (lambda (expr env)
    (pmatch expr
            [`,n (guard (number? n)) n]
            [`,y (guard (symbol? y)) (apply-env env y)]
            [`(lambda (,x) ,body) (closure-ds x body env)]
            [`(let ((,a ,b)) ,body) (let ((c (value-of-ds b env))) (value-of-ds body (extend-env a c env)))]
            [`(if ,cond ,true ,false) (if (value-of-ds cond env) (value-of-ds true env) (value-of-ds false env))]
            [`(zero? ,arg) (zero? (value-of-ds arg env))]
            [`(sub1 ,num) (sub1 (value-of-ds num env))]
            [`(* ,n1 ,n2) (* (value-of-ds n1 env) (value-of-ds n2 env))]
            [`(,rator ,rand) (apply-closure-ds (value-of-ds rator env) (value-of-ds rand env))])))



;;PART II
;;value-of-scopes

;;make something for null?, cons, cdr, and car. change the lambda line to be dynamic
;;so far modified closure-scope, added d-lambda.
(define value-of-scopes
  (lambda (expr env)
    (pmatch expr
            [`,n (guard (number? n)) n]
            [`,y (guard (symbol? y)) (apply-env env y)]
            [`(quote ()) '()]
            [`(null? ,ls) (null? (value-of-scopes ls env))]
            [`(cons ,a ,b) (cons (value-of-scopes a env) (value-of-scopes b env))]
            [`(cdr ,ls) (cdr (value-of-scopes ls env))]
            [`(car ,ls) (car (value-of-scopes ls env))]
            [`(lambda (,x) ,body) (closure-scopes 'lambda x body env)]
            [`(d-lambda (,x) ,body) (closure-scopes 'd-lambda x body env) ]
            [`(d-lambda (,x) ,body) (lambda (a env^) (value-of-scopes body (extend-env x a env^)))]
            [`(let ((,a ,b)) ,body) (let ((c (value-of-scopes b env))) (value-of-scopes body (extend-env a c env)))]
            [`(if ,cond ,true ,false) (if (value-of-scopes cond env) (value-of-scopes true env) (value-of-scopes false env))]
            [`(zero? ,arg) (zero? (value-of-scopes arg env))]
            [`(sub1 ,num) (sub1 (value-of-scopes num env))]
            [`(* ,n1 ,n2) (* (value-of-scopes n1 env) (value-of-scopes n2 env))]
            [`(,rator ,rand) (apply-closure-scopes (value-of-scopes rator env) (value-of-scopes rand env) env)])))


;;modify closures to fit the dynamic

;;apply-closure-scopes
(define apply-closure-scopes
  (lambda (rat rand env)
    (pmatch rat
            ( `(closure-scopes ,type ,x ,body ,cenv) (guard (eqv? type 'd-lambda)) ( (lambda (a env^) (value-of-scopes body (extend-env x a env^))) rand env))
            ( `(closure-scopes ,type ,x ,body ,cenv) (guard (eqv? type 'lambda)) ( (lambda (a env^) (value-of-scopes body (extend-env x a cenv))) rand env)))))

;;closure-scopes
(define closure-scopes
    (lambda (type x body env)
          `(closure-scopes ,type ,x ,body ,env)))