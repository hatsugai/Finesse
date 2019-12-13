(define macro-definitions (make-hash-table))

(define (find-transformer name)
  (hash-table-get macro-definitions name #f))

(define (register-transformer name transformer)
  (hash-table-put! macro-definitions name transformer))

(define (expand expr)
  (if (pair? expr)
      (let ((tag (car expr)))
        (if (symbol? tag)
            (cond ((eq? tag 'quote) expr)
                  ((eq? tag 'lambda)
                   (expand-lambda (cadr expr) (cddr expr)))
                  ((eq? tag 'if)
                   (cons 'if (expand-list (cdr expr))))
                  ((eq? tag 'set!)
                   (list 'set! (cadr expr) (expand (caddr expr))))
                  ((eq? tag 'begin)
                   (cons 'begin (expand-list (cdr expr))))
                  (else
                   (let ((transformer (find-transformer tag)))
                     (if transformer
                         (expand (apply transformer (cdr expr)))
                         (expand-list expr)))))
            (expand-list expr)))
      expr))

(define (expand-list expr)
  (map expand expr))

(define (expand-lambda formals body)
  ;; find def/expr boundary
  (let loop ((xs body) (ds '()))
    (if (null? xs)
        (error "no expr")
        (let ((x (car xs)))
          (if (pair? x)
              (let ((tag (car x)))
                (if (symbol? tag)
                    (cond ((eq? tag 'define)
                           (loop (cdr xs) (cons (cdr x) ds)))
                          ((eq? tag 'begin)
                           (loop (append (cdr x) (cdr xs)) ds))
                          (else
                           (let ((transformer (find-transformer tag)))
                             (if transformer
                                 (let ((y (apply transformer (cdr x))))
                                   (loop (cons y (cdr xs)) ds))
                                 (expand-lambda2 formals (reverse ds) xs)))))
                    (expand-lambda2 formals (reverse ds) xs)))
              (expand-lambda2 formals (reverse ds) xs))))))

(define (expand-lambda2 formals defs exprs)
  (if (null? defs)
      (list 'lambda formals (cons 'begin (expand-list exprs)))
      (list 'lambda formals
            (cons
             (list 'lambda (map car defs)
                   (cons 'begin
                         (append
                          (map (lambda (p)
                                 (list 'set! (car p) (expand (cadr p))))
                               defs)
                          (expand-list exprs))))
             (map (lambda (p) #f) defs)))))
