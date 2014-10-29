;;Wendy Kwok 
;;a5
#lang racket
(require C311/pmatch)

;;val-of-cbv
(define val-of-cbv
  (lambda (expr env)
    (pmatch expr
            (`,n (guard (number? n)) n)
            (`,b (guard (boolean? b)) b)
            (`,y (guard (symbol? y)) (unbox (apply-env env y)))  
            (`(lambda (,x) ,body) (closure-cbv x body env))
            (`(if ,test ,conseq ,false) (if (val-of-cbv test env)
                                           (val-of-cbv conseq env)
                                           (val-of-cbv false env)))
            (`(zero? ,n) (zero? (val-of-cbv n env)))
            (`(random ,n) (random (val-of-cbv n env)))
            (`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env)))
            (`(set! ,x ,rhs) (guard (symbol? x)) (set-box! (apply-env env x) (val-of-cbv rhs env)))
            (`(sub1 ,x) (sub1 (val-of-cbv x env)))
            (`(* ,x ,y) (* (val-of-cbv x env) (val-of-cbv y env)))
            (`(,rator ,rand) (apply-closure (val-of-cbv rator env) (box (val-of-cbv rand env)) env)))))


;;val-of-cbv
(define closure-cbv
  (lambda (x body env)
    (lambda (a env^)
      (val-of-cbv body (extend-env x a env)))))

;;val-of-cbr
(define val-of-cbr
  (lambda (expr env)
    (pmatch expr
            (`,n (guard (number? n)) n)
            (`,b (guard (boolean? b)) b)
            (`,y (guard (symbol? y)) (unbox (apply-env env y)))
            (`(sub1 ,x) (sub1 (val-of-cbr x env)))
            (`(* ,x ,y) (* (val-of-cbr x env) (val-of-cbr y env)))
            (`(lambda (,x) ,body) (closure-cbr x body env))
            (`(if ,test ,conseq ,false) (if (val-of-cbr test env)
                                           (val-of-cbr conseq env)
                                           (val-of-cbr false env)))
            (`(zero? ,n) (zero? (val-of-cbr n env)))
            (`(random ,n) (random (val-of-cbr n env)))
            (`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env)))
            (`(set! ,x ,rhs) (guard (symbol? x)) (set-box! (apply-env env x) (val-of-cbr rhs env)))
            (`(,rator ,x) (guard (symbol? x)) (apply-closure (val-of-cbr rator env) (apply-env env x) env))
            (`(,rator ,rand) (guard (not (symbol? rand))) (apply-closure (val-of-cbr rator env) (box (val-of-cbr rand env)) env)))))



;;val-of-cbr
(define closure-cbr
  (lambda (x body env)
    (lambda (a env^)
      (val-of-cbr body (extend-env x a env)))))

;;val-of-cbname
(define val-of-cbname
  (lambda (expr env)
    (pmatch expr
            (`,n (guard (number? n)) n)
            (`,b (guard (boolean? b)) b)
            (`,y (guard (symbol? y)) ((unbox (apply-env env y)))) 
            (`(sub1 ,x) (sub1 (val-of-cbname x env)))
            (`(* ,x ,y) (* (val-of-cbname x env) (val-of-cbname y env)))
            (`(lambda (,x) ,body) (closure-cbname x body env))
            (`(if ,test ,conseq ,false) (if (val-of-cbname test env)
                                           (val-of-cbname conseq env)
                                           (val-of-cbname false env)))
            (`(zero? ,n) (zero? (val-of-cbname n env)))
            (`(random ,n) (random (val-of-cbname n env)))
            (`(begin2 ,e1 ,e2) (begin (val-of-cbname e1 env) (val-of-cbname e2 env)))
            (`(set! ,x ,rhs) (guard (symbol? x)) (set-box! (apply-env env x) (val-of-cbname rhs env)))
            (`(,rator ,x) (guard (symbol? x)) (apply-closure (val-of-cbname rator env) (apply-env env x) env))
            (`(,rator ,rand) (guard (not (symbol? rand))) (apply-closure (val-of-cbname rator env) (box (lambda () (val-of-cbname rand env))) env)))))


;;val-of-cbname
(define closure-cbname
  (lambda (x body env)
    (lambda (a env^)
      (val-of-cbname body (extend-env x a env)))))

;;val-of-cbneed
(define val-of-cbneed
  (lambda (expr env)
    (pmatch expr
            (`,n (guard (number? n)) n)
            (`,b (guard (boolean? b)) b)
            (`,y (guard (symbol? y)) (unbox/need (apply-env env y)))
            (`(sub1 ,x) (sub1 (val-of-cbneed x env)))
            (`(* ,x ,y) (* (val-of-cbneed x env) (val-of-cbneed y env)))
            (`(lambda (,x) ,body) (closure-cbneed x body env))
            (`(if ,test ,conseq ,false) (if (val-of-cbneed test env)
                                           (val-of-cbneed conseq env)
                                           (val-of-cbneed false env)))
            (`(zero? ,n) (zero? (val-of-cbneed n env)))
            (`(random ,n) (random (val-of-cbneed n env)))
            (`(begin2 ,e1 ,e2) (begin (val-of-cbneed e1 env) (val-of-cbneed e2 env)))
            (`(set! ,x ,rhs) (guard (symbol? x)) (set-box! (apply-env env x) (val-of-cbneed rhs env)))
            (`(,rator ,x) (guard (symbol? x)) (apply-closure (val-of-cbneed rator env) (apply-env env x) env))
            (`(,rator ,rand) (guard (not (symbol? rand))) (apply-closure (val-of-cbneed rator env) (box (lambda () (val-of-cbneed rand env))) env)))))


;;val-of-cbneed
(define closure-cbneed
  (lambda (x body env)
    (lambda (a env^)
      (val-of-cbneed body (extend-env x a env)))))


;;helpers


(define unbox/need
  (lambda (b)
    (let ( (val ((unbox b))))
      (set-box! b (lambda () val))
      val)))


(define (empty-env)
  (lambda (y) (error "unbound variable ~s" y)))

(define extend-env
  (lambda (x a env)
    (lambda (y)
      (if (eqv? x y)
          a
          (env y)))))

(define apply-env
  (lambda (env y)
    (env y)))

(define apply-closure
  (lambda ( rat rand env)
    (rat rand env)))
