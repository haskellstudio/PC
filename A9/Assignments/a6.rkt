;;Wendy Kwok
;;a6

;;1.
(require C311/pmatch)


;;empty-k
(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
            (error 'empty-k "You can only invoke the empty continuation once")
            (begin (set! once-only #t) v))))))

;;1
(define last-non-zero
  (lambda (ls)
    (call/cc
     (lambda (k)
       (letrec
           ((lnz
             (lambda (ls)
               (cond
                [(null? ls) '()]
                [(zero? (car ls)) (k (lnz (cdr ls)))]
                [else (cons (car ls) (lnz (cdr ls)))]))))
         (lnz ls))))))

;;2.
(define mult/cc
  (lambda (n*)
    (call/cc
     (lambda (k)
       (letrec
           ((m/cc
             (lambda (n*)
               (cond
                [(null? n*) 1]
                [(zero? (car n*)) (k 0)]
                [else (m/cc (car n*) (mult/cc (cdr n*)))]))))
         (m/cc n*))))))

;;my-*
(define my-*
  (lambda (m n)
    (* m n)))

;;3.
(define times-cps
  (lambda (ls k)
    (cond
     [(null? ls) 1]
     [(zero? (car ls)) (k 0)]
     [else (times-cps (cdr ls) (lambda (m)
                                 (k (* (car ls) m))))])))

;;4.
(define times-cps-shortcut
  (lambda (ls k)
    (cond
     [(null? ls) 1]
     [(zero? (car ls)) 0]
     [else (time-cps-shortcut (cdr ls) (lambda (m)
                                         (k ( * (car ls) m))))])))

;;5.
(define plus-cps
  (lambda (m k)
    (k (lambda (n k)
         (k (+ m n))))))

;;6.
(define count-syms*-cps
  (lambda (ls k)
    (cond
     [(null? ls) (k 0)]
     [(pair? (car ls)) (count-syms*-cps (cdr ls) (lambda (m)
                                                   (count-syms*-cps (car ls) (lambda (n)
                                                                               (k (+ n m))))))]
     [(symbol? (car ls)) (count-sym*-cps (cdr ls) (lambda (m)
                                                    (k (add1 m))))]
     [else (count-syms*-cps (cdr ls) k)])))

;;7.
(define cons-cell-count-cps
  (lambda (ls k)
    (cond
     [(pair? ls) (cond-cell-count (car ls) (lambda (m)
                                             (cons-cell-count-cps (cde ls) (lambda (n)
                                                                             (k (+ m n))))))]
     [else (k 0)])))

;;8.
(define walk-cps
  (lambda (v ls k)
    (cond
     [(symbol? v) (let ([p (assq v ls)])
                    (cond
                     [p (walk-cps (cdr p) ls k)]
                     [else (k v)]))]
     [else (k v)])))

;;9.
(define ack-cps
  (lambda m n k)
  (cond
   [(zero? m) (k (add1 n))]
   [(zero? n) (ack-cps (sub1 m) 1 k)]
   [else (ack-cps m (sub1 n) (lambda (p)
                           (ack-cps (sub1 m) p k)))]))

;;10.
(define fib-cps
  (lambda (n k)
    ((lambda (fib-cps k)
       (fib-cps fib-cps n k))
     (lambda (fib-cps n k)
       (cond
        [(zero? n) (k 0)]
        [(= 1 n) (k 1)]
        [else (fib-cps fib-cps (sub1 n) (lambda (r)
                                          (fib-cps fib-cps (sub1 (sub1 n)) (lambda (s)
                                                                             (k (+ r s))))))])) k)))

;;11.
(define unfold-cps
  (lambda (p-cps f-cps g-cps seed k)
    ((lambda (h-cps k)
       (h-cps h-cps (lambda (e)
                      (e seed '() k))))
     (lambda (h-cps k)
       (k (lambda (seed ans k)
            (p-cps seed (lambda (p)
                          (cond
                           (p (k ans))
                           (else (f-cps seed (lambda (f)
                                               (g-cps seed (lambda (g)
                                                             (h-cps h-cps
                                                                    (lambda (h)
                                                                      (h g (cons f ans) k)))))))))))))) k)))

;;12.
(define empty-s
  (lambda ()
    '()))

(define extend-s
  (lambda (x v s)
    (cons `(,x . ,v) s)))

(define unify-cps
  (lambda (v w s k)
    (walk-cps v s (lambda (m) (walk-cps w s (lambda (n)
                                               (let ([v m])
                                                 (let ([w n])
                                                   (cond
                                                    [(eq? v w) (k s)]
                                                    [(symbol? v) (k (extend-s v w s))]
                                                    [(symbol? w) (k (extend-s w v s))]
                                                    [(and (pair? v) (pair? w))
                                                     (unify-cps (car v) (car w) s (lambda (uv)
                                                                                    (let ([s uv])
                                                                                      (cond
                                                                                       [s (unify-cps (cdr v) (cdr w) s k)]
                                                                                       [else (k #f)]))))]
                                                    [(equal? v w) (k s)]
                                                    [else (k #f)])))))))))

;;13.
(define M-cps
  (lambda (f k)
    (k (lambda (ls k)
         (M-cps f (lambda (m)
                    (cond
                     ((null? ls) (k '()))
                     (else (m (cdr ls) (lambda (n)
                                         (f (car ls) (lambda (p)
                                                       (k (cons p n))))))))))))))

;;14.
(define use-of-M-cps
  ((M-cps (lambda (n k)
            (k (add1 n)))
          (lambda (x)
            (x '(1 2 3 4 5) (empty-k))))))



