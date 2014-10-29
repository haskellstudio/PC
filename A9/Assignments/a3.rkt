;;a3
#lang racket
(require C311/pmatch)

;;1.
(define value-of
  (lambda (expr env)
    (pmatch expr
            [`,n (guard (number? n)) n ]
            [`,y (guard (symbol? y)) (env y)]
            [`(lambda (,x) ,body) (lambda (z) (value-of body (lambda (y)
							       (if (eqv? y x) z (env y))))) ]
            [`(lambda (,x ,y)  ,body) (lambda ( z a) (value-of body (lambda (w)
                                                                      (cond
                                                                       [(eqv? w x) z ]
                                                                       [(eqv? w y) a ]
                                                                       [else (env w)]))))]
	    [`(let ((,a ,b)) ,body) (let ((c (value-of b env))) (value-of body (lambda (y)
                                                                                 (if (eqv? y a) c (env y))))) ]
	    [`(if ,cond ,true ,false) (if (value-of cond env) (value-of true env) (value-of false env))  ]
	    [`(zero? ,arg) (zero? (value-of arg env)) ]
	    [`(sub1 ,num) (sub1 (value-of num env))]
	    [`(* ,n1 ,n2) (* (value-of n1 env) (value-of n2 env))]
	    [`(,rator ,rand) ((value-of rator env) (value-of rand env)) ]
            [`(,rator ,rand1 ,rand2) ((value-of rator env) (value-of rand1 env) (value-of rand2 env)) ])))

;;value-of-fn


;;empty fn
(define empty-env-fn
  (lambda ()
    (lambda (y) (error 'value-of "unbound variable ~s" y))))


;;extend fn
(define extend-env-fn
  (lambda (x z env)
    (lambda (y)
      (if (eqv? y x) z (env y)))))

;;apply fn
(define apply-env-fn
  (lambda (env y)
    (env y)))


;;value-of-fn

(define value-of-fn
  (lambda (expr env)
    (pmatch expr
            [`,n (guard (number? n)) n ]
            [`,y (guard (symbol? y)) (apply-env-fn env y)]
            [`(lambda (,x) ,body) (lambda (z) (value-of-fn body (extend-env-fn x z env))) ]
	    [`(let ((,a ,b)) ,body) (let ((c (value-of-fn b env))) (value-of-fn body (extend-env-fn a c env))) ]
	    [`(if ,cond ,true ,false) (if (value-of-fn cond env) (value-of-fn true env) (value-of-fn false env))  ]
	    [`(zero? ,arg) (zero? (value-of-fn arg env)) ]
	    [`(sub1 ,num) (sub1 (value-of-fn num env))]
	    [`(* ,n1 ,n2) (* (value-of-fn n1 env) (value-of-fn n2 env))]
	    [`(,rator ,rand) ((value-of-fn rator env) (value-of-fn rand env)) ])))


;;empty ds
(define empty-env-ds
  (lambda ()
    `(empty-env-ds)))


(define extend-env-ds
  (lambda (x z env)
    `(extend-env-ds ,x ,z ,env)))


(define apply-env-ds
  (lambda (env y)
    (pmatch env
            ( `(empty-env-ds) (lambda (y) (error 'value-of "unbound variable ~s" y)))
            ( `(extend-env-ds ,x ,z ,env) (if (eqv? x y) z (apply-env-ds env y)) ))))





;;value-of-ds


(define value-of-ds
  (lambda (expr env)
    (pmatch expr
            [`,n (guard (number? n)) n ]
            [`,y (guard (symbol? y)) (apply-env-ds env y)]
            [`(lambda (,x) ,body) (lambda (z) (value-of-ds body (extend-env-ds x z env))) ]
	    [`(let ((,a ,b)) ,body) (let ((c (value-of-ds b env))) (value-of-ds body (extend-env-ds a c env))) ]
	    [`(if ,cond ,true ,false) (if (value-of-ds cond env) (value-of-ds true env) (value-of-ds false env))  ]
	    [`(zero? ,arg) (zero? (value-of-ds arg env)) ]
	    [`(sub1 ,num) (sub1 (value-of-ds num env))]
	    [`(* ,n1 ,n2) (* (value-of-ds n1 env) (value-of-ds n2 env))]
	    [`(,rator ,rand) ((value-of-ds rator env) (value-of-ds rand env)) ])))




;;fo-eulav

(define empty-env
  (lambda ()
    (lambda (y) (error 'value-of "unbound variable ~s" y))))

(define fo-eulav
  (lambda (expr env)
    (pmatch expr
            [`,n (guard (number? n)) n ]
            [`,y (guard (symbol? y)) (env y)]
            [`(,body (,x) adbmal) (lambda (z) (fo-eulav body (lambda (y)
                                                               (if (eqv? y x) z (env y)))))  ]
            [`(,body ((,b ,a)) tel) (let ((c (fo-eulav b env))) (fo-eulav body (lambda (y)
                                                                                 (if (eqv? y a) c (env y))))) ]
            [`(,false ,true ,cond fi) (if (fo-eulav cond env) (fo-eulav true env) (fo-eulav false env))  ]
	    [`(,arg ?orez) (zero? (fo-eulav arg env)) ]
	    [`(,num 1bus) (sub1 (fo-eulav num env))]
	    [`(,n2 ,n1 *) (* (fo-eulav n1 env) (fo-eulav n2 env))]
	    [`(,rand ,rator) ((fo-eulav rator env) (fo-eulav rand env)) ])))