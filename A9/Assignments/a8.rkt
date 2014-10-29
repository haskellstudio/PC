;;Wendi Kwok
;;a8
#lang racket
;;trampoline
(define trampoline
  (lambda (th)
    (trampoline (th))))

(define m 0)
(define n 0)
(define v 0)
(define k 0)
(define ls 0)

;;ack tramp
(define ack-tramp-driver
  (lambda (m n)
    (call/cc (lambda (jumpout) (trampoline (ack-tramp m n (empty-k-ack-tramp jumpout)))))))

(define empty-k-ack-tramp
  (lambda (jumpout)
    `(empty-k-ack-tramp ,jumpout)))

(define k-ack-tramp
  (lambda (m k)
    `(k-ack-tramp ,m ,k)))

(define app-k-ack-tramp
  (lambda (k v)
    (pmatch k
            [`(k-ack-tramp ,m ,k) (lambda () (ack-tramp (sub1 m) v k))]
            [`(empty-k-ack-tramp ,jumpout) (jumpout v)])))

(define ack-tramp
  (lambda (m n k)
    (cond
     [(zero? m) (app-k-ack-tramp k (add1 n))]
     [(zero? n) (lambda () (ack-tramp (sub1 m) 1 k))]
     [else (lambda () (ack-tramp m (sub1 n) (k-ack-tramp m k)))])))

;;depth tramp
(define depth-tramp-driver
  (lambda (ls)
    (call/cc (lambda (jumpout) (trampoline (depth-tramp ls (empty-k-depth-tramp jumpout))))))) 

(define empty-k-depth-tramp
  (lambda (jumpout)
    `(empty-k-depth-tramp ,jumpout)))

(define inner-k-depth-tramp
  (lambda (v^ k^)
    `(inner-k-depth-tramp ,v^ ,k^)))

(define outer-k-depth-tramp
  (lambda (v^ k^)
    `(outer-k-depth-tramp ,v^ ,k^)))

(define app-k-depth-tramp
  (lambda (k v)
    (pmatch k
            [`(empty-k-depth-tramp ,jumpout) (jumpout v)]
            [`(inner-k-depth-tramp ,v^ ,k^) (let ((v^ (add1 v^)))
                                            (app-k-depth-tramp k^ (if (< v^ v) v v^)))]
            [`(outer-k-depth-tramp ,v^ ,k^) (lambda () (depth-tramp (cdr v^) (inner-k-depth-tramp v k^)))])))

(define depth-tramp
  (lambda (ls k)
    (cond
     [(null? ls) (app-k-depth-tramp k 1)]
     [(pair? (car ls)) (lambda () (depth-tramp (car ls) (outer-k-depth-tramp ls k)))]
     [else (depth-tramp (cdr ls) k)])))

;;fact tramp
(define fact-tramp-driver
  (lambda (n)
    (call/cc (lambda (jumpout) (trampoline (fact-tramp n (empty-k-fact-tramp jumpout))))))) 

(define empty-k-fact-tramp
  (lambda (jumpout)
    `(empty-k-fact-tramp ,jumpout)))

(define fact-k-tramp
  (lambda (n^ k^)
    `(fact-k-tramp ,n^ ,k^)))

(define app-k-fact-tramp
  (lambda (k v)
    (pmatch k
            [`(fact-k-tramp ,n^ ,k^) (app-k-fact-tramp k^ (* n^ v))]
            [`(empty-k-fact-tramp ,jumpout) (jumpout v)])))

(define fact-tramp
  (lambda (n k)
    ((lambda (fact-tramp k)
       (lambda () (fact-tramp fact-tramp n k)))
     (lambda (fact-tramp n k)
       (cond
        [(zero? n) (app-k-fact-tramp k 1)]
        [else (lambda () (fact-tramp fact-tramp (sub1 n) (fact-k-tramp n k)))])) k)))

;;ack registerized
(define ack-reg-driver
  (lambda (m^ n^)
    (begin
      (set! m m^)
      (set! n n^)
      (set! k (empty-k-ack-reg))
      (ack-reg))))

(define empty-k-ack-reg
  (lambda ()
    '(empty-k-ack-reg)))

(define app-k-ack-reg
  (lambda ()
    (pmatch k
            [`(inner-k-ack-reg ,m^ ,k^)
             (begin
               (set! m (sub1 m^))
               (set! k k^)
               (set! n v)
               (ack-reg))]
            [`(empty-k-ack-reg) v])))

(define inner-k-ack-reg
  (lambda (m k)
    `(inner-k-ack-reg ,m ,k)))

(define ack-reg
  (lambda ()
    (cond
     [(zero? m) (begin
                  (set! k k)
                  (set! v (add1 n))
                  (app-k-ack-reg))]
     [(zero? n) (begin
                  (set! m (sub1 m))
                  (set! n 1)
                  (ack-reg))]
     [else (begin
             (set! n (sub1 n))
             (set! k (inner-k-ack-reg m k))
             (ack-reg))])))


;;depth reg
(define depth-reg-driver
  (lambda (ls^)
    (begin
      (set! ls ls^)
      (set! k (empty-k-reg-depth))
      (depth-reg))))


(define empty-k-reg-depth
    (lambda ()
          `(empty-k-reg-depth)))

(define inner-k-reg-depth
    (lambda (v^ k^)
          `(inner-k-reg-depth ,v^ ,k^)))

(define outer-k-reg-depth
    (lambda (v^ k^)
          `(outer-k-reg-depth ,v^ ,k^)))

(define app-k-reg-depth
  (lambda () 
    (pmatch k
            [`(empty-k-reg-depth) v]
            [`(inner-k-reg-depth ,v^ ,k^)
             (let* ((v^ (add1 v^))
                    (setv (max v^ v)))
               (begin
                 (set! k k^)
                 (set! v setv)
                 (app-k-reg-depth)))]
            [`(outer-k-reg-depth ,v^ ,k^)
             (begin
               (set! ls (cdr v^))
               (set! k (inner-k-reg-depth v k^))
               (depth-reg))])))

(define depth-reg
  (lambda () 
    (cond
     [(null? ls)
      (begin
        (set! v 1)
        (app-k-reg-depth))]
     [(pair? (car ls))
      (begin
        (set! k (outer-k-reg-depth ls k))
        (set! ls (car ls))
        (depth-reg))]
     [else
      (begin
        (set! ls (cdr ls))
        (depth-reg))])))


;;fact reg
(define fact-reg-driver
  (lambda (n^)
    (begin
      (set! n n^)
      (set! k (empty-k-reg-fact))
      (fact-reg))))

(define empty-k-reg-fact
  (lambda ()
    `(empty-k-reg-fact)))

(define fact-k-reg
  (lambda (n^ k^)
    `(fact-k-reg ,n^ ,k^)))

(define app-k-reg-fact
  (lambda ()
    (pmatch k
            [`(fact-k-reg ,n^ ,k^)
             (begin
               (set! k k^)
               (set! v (* n^ v))
               (app-k-reg-fact))]
            [`(empty-k-reg-fact) v])))


(define fact-reg
  (lambda ()
    (cond
     [(zero? n) (begin
                  (set! v 1)
                  (app-k-fact-reg))]
     [(else (begin
              (set! k (fact-k-reg n k))
              (set! n (sub1 n))
              (fact-reg)))])))


