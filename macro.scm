(define-macro (%unit-test name expr expected)
  (let ((x (gensym)) (y (gensym)))
	`((lambda (,x ,y)
		(if (equal? ,x ,y)
			#t
			(begin
			  (prn 'ERROR ,name ,x ,y)
			  (error "%unit-test failed"))))
	  ,expr ,expected)))

(define-macro (cond . cs)
  (if (null? cs)
	  #f
	  (let ((c (car cs)))
		(if (pair? c)
			(if (eq? (car c) 'else)
				`(begin ,@(cdr c))
				(if (pair? (cdr c))
					`(if ,(car c)
						 (begin ,@(cdr c))
						 (cond ,@(cdr cs)))
					(let ((x (gensym)))
					  `(let ((,x ,(car c)))
						 (if ,x
							 ,x
							 (cond ,@(cdr cs)))))))))))

(define-macro (and . xs)
  (cond ((null? xs) #t)
		((null? (cdr xs)) (car xs))
		(else
		 `(if ,(car xs)
			  (and ,@(cdr xs))
			  #f))))

(define-macro (or . xs)
  (cond ((null? xs) #f)
		((null? (cdr xs)) (car xs))
		(else
		 (let ((x (gensym)))
		   `((lambda (,x)
			   (if ,x
				   ,x
				   (or ,@(cdr xs))))
			 ,(car xs))))))

(define-macro (when test . xs)
  `(if ,test (begin ,@xs)))

(define-macro (unless test . xs)
  `(if ,test #f (begin ,@xs)))

(define-macro (letrec* bs . xs)
  `((lambda ,(map car bs)
	  ,@(map (lambda (b) `(set! ,(car b) ,(cadr b))) bs)
	  ((lambda () (begin ,@xs))))
	,@(map (lambda (b) #f) bs)))

(define-macro (letrec . x)
  `(letrec* ,@x))

(define-macro (let . xs)
  (if (symbol? (car xs))
	  (let ((name (car xs))
			(bs (cadr xs))
			(body (cddr xs)))
		`((letrec* ((,name (lambda ,(map car bs) ,@body)))
			,name)
		  ,@(map cadr bs)))
	  (let ((bs (car xs))
			(body (cdr xs)))
		`((lambda ,(map car bs) (begin ,@body))
		  ,@(map cadr bs)))))

(define-macro (let* bs . xs)
  (if (null? bs)
	  `((lambda () ,@xs))
	  `(let (,(car bs))
		 (let* ,(cdr bs) ,@xs))))

;(define-macro (let-values