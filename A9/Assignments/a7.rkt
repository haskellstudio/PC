;;Wendy Kwok
;;a7

;;1.
(define binary-to-decimal-cps
  (lambda (n k)
    (cond
     [(null? n) (k 0)]
     [else (binary-to-decimal-cps
            (cdr n)
            (lambda (m)
              (k (+ (car n) (* 2 m)))))])))

;;2.
(define rember*1-cps
  (lambda (ls k)
    (cond
     [(null? ls) (k '())]
     [(pair? (car ls))
      (rember*1-cps (car ls) (lambda (p)
                               (cond
                                [(equal? (car ls) p) (rember*1-cps (cdr ls)
                                                                   (lambda (n)
                                                                     (k (cons (car ls) n))))]
                                [else (k (cons p (cdr ls)))])))]
     [(eqv? (car ls) '?) (k (cdr ls))]
     [else (rember*1-cps (cdr ls)
                         (lambda (m)
                           (k (cons (car ls) m))))])))


(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
            (error 'empty-k "You can only invoke the empty continuation once")
            (begin (set! once-only #t) v))))))

;;fn helpers
(define (empty-env)
  (lambda (y) (error "unbound variable ~s" y)))

;;fn extend-env
(define extend-env
  (lambda (x a env)
    (lambda (y)
      (if (eqv? x y)
          a
          (env y)))))

;;fn apply env
(define apply-env
  (lambda (env y)
    (env y)))

;;fn apply closure
(define apply-closure
  (lambda (rat rand k)
    (rat rand k)))

;;value-of-cps closure
(define closure
  (lambda (x body env)
    (lambda (y k)
      (value-of-cps body (extend-env x y env) k))))

;;value-of-cps
(define value-of-cps
  (lambda (expr env k)
    (pmatch expr
            [`,n (guard (or (number? n) (boolean? n))) (k n)]
            [`(+ ,x1 ,x2) (value-of-cps x1 env (lambda (m)
                                                 (value-of-cps x2 env (lambda (p)
                                                                        (k (+ m p))))))]
            [`(* ,x1 ,x2) (value-of-cps x1 env (lambda (m)
                                                 (value-of-cps x2 env (lambda (p)
                                                                        (k (* m p))))))]
            [`(sub1 ,x) (value-of-cps x env (lambda (m)
                                              (k (sub1 m))))]
            [`(zero? ,x) (value-of-cps x env (lambda (m)
                                               (k (zero? m))))]
            [`(if ,test ,conseq ,alt)
             (value-of-cps test env (lambda (m)
                                      (if m
                                          (value-of-cps conseq env k)
                                          (value-of-cps alt env k))))]
            [`(capture ,k-id ,body) (value-of-cps body (entend-env k-id k env) k)] 
            [`(return ,v-exp ,k-exp) (value-of-cps k-exp env (lambda (kexp) (value-of-cps v-exp env kexp)))]
            [`,x (guard (symbol? x)) (k (apply-env env x))]
            [`(lambda (,id) ,body) (closure id body env)]
            [`(,rator ,rand) (value-of-cps rator env (lambda (m)
                                                       (value-of-cps rand env (lambda (p)
                                                                                (k (apply-closure m p))))))]
)))


;;valof cps fn
(define value-of-cps-fn
  (lambda (expr env k)
    (pmatch expr
            [`,n (guard (or (number? n) (boolean? n))) (app-k-fn k n)]
            [`(+ ,x1 ,x2) (value-of-cps-fn x1 env (+-outer-k-fn x2 env k))]
            [`(* ,x1 ,x2) (value-of-cps-fn x1 env (*-outer-k-fn x2 env k))]
            [`(sub1 ,x) (value-of-cps-fn x env (sub1-fn k))]
            [`(zero? ,x) (value-of-cps-fn x env (zero?-fn k))]
            [`(if ,test ,conseq ,alt) (value-of-cps-fn test env (if-fn conseq alt env k))]
            [`(capture ,k-id ,body) (value-of-cps-fn body (entend-env k-id k env) k)] 
            [(`return ,v-exp ,k-exp) (value-of-cps-fn k-exp env (return-fn v-exp env))]
            [`,x (guard (symbol? x)) (app-k-fn k (apply-env env x))]
            [`(lambda (,id) ,body) (closure-fn id body env)]
            [`(,rator ,rand) (value-of-cps-fn rator env (rator-fn rand env k))] )))

;;fn-closure

;;value-of-cps-fn closure
(define closure-fn
  (lambda (x body env)
    (lambda (y k)
      (value-of-cps-fn-fn body (extend-env x y env) k))))


;;app k fn
(define app-k-fn
  (lambda (k v)
    (k v)))

;;*-inner-k-fn
(define *-inner-k-fn
  (lambda (v^ k)
    (lambda (v)
      (app-k-fn k (* v^ v))) ))

;;*-outer-k-fn
(define *-outer-k-fn
  (lambda (x2 env k)
    (lambda (v)
      (value-of-cps-fn x2 env (*-inner-k-fn v k)))))

;;+-inner-k-fn
(define +-inner-k-fn
  (lambda (v^ k)
    (lambda (v)
      (app-k-fn k (+ v^ v)))))

;;+-outer-k-fn
(define +-outer-k-fn
  (lambda (x2 env k)
    (lambda (v)
      (value-of-cps-fn x2 env (+-inner-k-fn v k)))))

;;sub1-fn
(define sub1-fn
  (lambda (k)
    (lambda (m)
      (app-k-fn k (sub1 m)))))

;;zero?-fn
(define zero?-fn
  (lambda (k)
    (lambda (m)
      (app-k-fn k (zero? m)))))
{
;;if-fn
(define if-fn
  (lambda (conseq alt env k)
    (lambda (m)
      (if m
          (value-of-cps-fn conseq env k)
          (value-of-cps-fn alt env k)))))

;;return-fn
(define return-fn
  (lambda (v-exp env)
    (lambda (kexp)
      (value-of-cps-fn v-exp env kexp))))


;;rand-fn
(define rand-fn
  (lambda (m k)
    (lambda (p)
      (app-k-fn k (apply-closure m p)))))

;;rator-fn
(define rator-fn
  (lambda (rand env k)
    (lambda (m)
      (value-of-cps-fn rand env (rand-fn m k)))))




;;Valof cps ds
(define value-of-cps-ds
  (lambda (exp env k)
    (pmatch exp
            (`,n (guard (or (number? n) (boolean? n))) (app-k-ds k n))
            (`(+ ,x1 ,x2) (value-of-cps-ds x1 env (+-outer-k-ds x2 env k)))
            (`(* ,x1 ,x2) (value-of-cps-ds x1 env (*-outer-k-ds x2 env k)))
            (`(sub1 ,x) (value-of-cps-ds x env (sub1-ds k)))
            (`(zero? ,x) (value-of-cps-ds x env (zero?-ds k)))
            (`(if ,test ,conseq ,alt) (value-of-cps-ds test env (if-ds conseq alt env k)))
            (`(capture ,k-id ,body) (value-of-cps-ds body (extend-env k-id k env) k))
            (`(return ,v-exp ,k-exp) (value-of-cps-ds k-exp env (ret-ds v-exp env)))
            (`,x (guard (symbol? x)) (app-k-ds k (apply-env env x)))
            (`(lambda (,x) ,body) (app-k-ds k (closure-ds x body env)))
            (`(,rator ,rand) (value-of-cps-ds rator env (rat-ds rand env k))))))

;;app-k-ds
(define app-k
  (lambda (k v)
    (pmatch k
	    [`(empty-k-ds) v]
            [`(*-inner-k-ds ,v^ ,k) (app-k-fn k (* v^ v))]
            [`(*-outer-k-ds x2 env k)  (value-of-cps-fn x2 env (*-inner-k-fn v k))]
            [`(+-inner-k-ds ,v^ ,k) (app-k-fn k (+ v^ v))]
            [`(+-outer-k-ds ,x2 ,env ,k) (value-of-cps-fn x2 env (+-inner-k-fn v k))]
            [`(sub1-ds ,k) (app-k-fn k (sub1 m))]
            [`(zero?-ds ,k) (app-k-fn k (zero? m))]
            [`(if ,conseq ,alt ,env ,k) 
	     (if m
		 (value-of-cps-fn conseq env k)
		 (value-of-cps-fn alt env k))]
            [`(return ,v-exp ,env) (value-of-cps-fn v-exp env kexp)]
            [`(rand-ds ,m ,k) (app-k-fn k (apply-closure m p))]
            [`(rator-ds ,rand ,env ,k) (value-of-cps-fn rand env (rand-fn m k))]
            )))

;;*-inner-k-ds
(define *-inner-k-ds
  (lambda (v^ k)
    `(*-inner-k-ds ,v^ ,k) ))

;;*-outer-k
(define *-outer-k
  (lambda (x2 env k)
    `(*-outer-k-ds ,x2 ,env ,k)))

;;+-inner-k-ds
(define +-inner-k-ds
  (lambda (v^ k)
    `(+-inner-k ,v^ ,k)))

;;+-outer-k-ds
(define +-outer-k-ds
  (lambda (x2 env k)
    `(+-outer-k-ds ,x2 ,env ,k)))

;;sub1-ds
(define sub1-ds
  (lambda (k)
    `(sub1-ds ,k)))

;;zero?-ds
(define zero?-ds
  (lambda (k)
    `(zero?-ds ,k)))

;;if-ds
(define if-ds
  (lambda (conseq alt env k)
    (lambda (m)
      `(if-ds ,conseq ,alt ,env ,k))))

;;return-ds
(define return-ds
  (lambda (v-exp env)
    `(return-ds ,v-exp ,env)))

;;
;;rand-ds
(define rand-ds
  (lambda (m k)
    `(rand-ds ,m ,k)))

;;rator-fn
(define rator-fn
  (lambda (rand env k)
    `(rator-fn ,rand ,env ,k)))

;;empty-k-ds
(define empty-k-ds
  (lambda ()
    `(empty-k-ds)))

