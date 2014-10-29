(require C311/pmatch)

;;3

(define extend
  (lambda ( x pred )
    (lambda ( e )
      (or (pred e) (eqv? x e)))))

;;4. walk symbol
(define walk-symbol
  (lambda (x assl)
    (cond
     [(assv x assl) (walk-symbol (cdr (assv x assl)) assl) ]
     [else x] )))

;;5. 
(define lambda->lumbda
  (lambda (expr)
    (pmatch expr
	    [`,y (guard (symbol? y)) y ]
	    [`(lambda (,x) ,body) `(lumbda (,x) ,(lambda->lumbda body))] 
	    [`(,rator ,rand) (list (lambda->lumbda rator) (lambda->lumbda rand)) ])))

;;6.
(define vars
  (lambda (expr)
    (pmatch expr
	    [`,y (guard (symbol? y)) `(,y)]
	    [`(lambda (,x) ,body) (vars body)]
	    [`(,rator ,rand) (append (vars rator) (vars rand)) ] )))

;;7.
(define unique-vars
  (lambda (expr)
    (pmatch expr
            [`,y (guard (symbol? y)) `(,y)]
            [`(lambda (,x) ,body) (vars body)]
            [`(,rator ,rand) (union (vars rator) (vars rand)) ] )))

;;8.
(define var-occurs-free?
  (lambda (x expr)
    (pmatch expr
	    [`,y (guard (symbol? y)) (eqv? x y) ]
	    [`(lambda (,y) ,body) (and (eqv? x y) (var-occurs-free? x body)) ]
	    [`(,rator ,rand) (or (var-occurs-free? x rator) (var-occurs-free? x rand))] )))

;;9. 
(define var-occurs-bound?
  (lambda (x expr)
    (pmatch expr
	    [`,y (guard (symbol? y)) (not (eqv? x y)) ]
	    [`(lambda (,y) ,body) (or (eqv? x y) (var-occurs-free? x body))]
	    [`(,rator ,rand) (or (var-occurs-bound? x rator) (var-occurs-bound? x rand)) ])))

;;10.
(define unique-free-vars
  (lambda (expr)
    (pmatch expr
	    [`,y (guard (symbol? y)) `(,y) ]
	    [`(lambda (,x) ,body) (remv x (unique-free-vars body)) ]
	    [`(,rator ,rand) (union (unique-free-vars rator) (unique-free-vars rand))] )))

;;11.
(define unique-bound-vars
  (lambda (expr)
    (pmatch expr
	    [`,y (guard (symbol? y)) `() ]
	    [`(lambda (,y) ,body) (if (var-occurs-free? y body) 
				      (cons y (unique-bound-vars body))
				      (unique-bound-vars body))]
	    [`(,rator ,rand) (union (unique-bound-vars rator)
				    (unique-bound-vars rand))])))

;;12.
(define lex
  (lambda (expr acc)
    (letrec (
	     (index
	      (lambda (y acc)
		(cond
		 [(eqv? (car acc) y) 0 ]
		 [else (add1 (index y (cdr acc)))]
		 ) )) )
      (pmatch expr
	      [`,y (guard (symbol? y)) (if (memv y acc)
					   `(var ,(index y acc))
					   `(free-var ,y))]
	      [`(lambda (,y) ,body) `(lambda ,(lex body (cons y acc))) ]
	      [`(,rator ,rand) (list (lex rator acc) (lex rand acc)) ] ))))