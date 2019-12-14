(define-record-type compile-time-environment
  make-cenv
  cenv?
  (params cenv-params)
  (closed cenv-closed)
  (boxed cenv-boxed))

(define (compile expr cenv next)
  (match expr
    ((('lambda () . body))
     (compile `(begin ,@body) cenv next))
    (_
     (compile2 expr cenv next))))

(define (compile2 expr cenv next)
  (cond ((symbol? expr)
         (compile-variable expr cenv next))
        ((pair? expr)
         (let ((tag (car expr)))
           (cond ((eq? tag 'quote)
                  (compile-quote (cadr expr) cenv next))
                 ((eq? tag 'lambda)
                  (compile-lambda (cadr expr) (caddr expr) cenv next))
                 ((eq? tag 'if)
                  (if (pair? (cdddr expr))
                      (compile-if (cadr expr) (caddr expr) (cadddr expr)
                                  cenv next)
                      (compile-if (cadr expr) (caddr expr) #f cenv next)))
                 ((eq? tag 'set!)
                  (compile-set! (cadr expr) (caddr expr) cenv next))
                 ((eq? tag 'begin)
                  (compile-begin (cdr expr) cenv next))
                 (else
                  (compile-application (car expr) (cdr expr) cenv next)))))
        (else
         (vector VMI_CONSTANT next expr))))

(define (compile-variable var cenv next)
  (compile-refer
   var cenv
   (if (memq var (cenv-boxed cenv))
       (vector VMI_INDIRECT next)
       next)))

(define (compile-quote const cenv next)
  (vector VMI_CONSTANT next const))

(define (compile-application fun args cenv next)
  (let ((n (length args)))
    (let loop ((xs (reverse args))
               (c (compile fun cenv
                           (if (tail? next)
                               (vector VMI_SHIFTJUMP n (length (cenv-params cenv)))
                               (vector VMI_APPLY next n)))))
      (if (null? xs)
          c
          (loop (cdr xs)
                (compile (car xs) cenv
                         (vector VMI_PUSH c)))))))

(define (compile-begin xs cenv next)
  (let loop ((xs (reverse xs)) (c next))
    (if (pair? xs)
        (loop (cdr xs)
              (compile (car xs) cenv c))
        c)))

(define (compile-set! var expr cenv next)
  (compile-lookup
   var (cenv-params cenv) (cenv-closed cenv)
   (lambda (k)
     (compile expr cenv
              (vector VMI_ASSIGN_PARAM next k)))
   (lambda (k)
     (compile expr cenv
              (vector VMI_ASSIGN_CLOSED next k)))
   (lambda (v)
     (compile expr cenv
              (vector VMI_ASSIGN_GLOBAL_SYMBOL next v)))))

(define (compile-if test then else cenv next)
  (let ((thenc (compile then cenv next))
        (elsec (compile else cenv next)))
    (compile test cenv
             (vector VMI_BRANCH thenc elsec))))

(define (compile-lambda formals expr cenv next)
  (let ((params
         (formals->params formals))
        (vars-in-scope
         (append (cenv-params cenv) (cenv-closed cenv))))
    (let ((closed
           (collect-vars-free (lset-difference eq? vars-in-scope params) expr))
          (params-boxed
           (collect-vars-set params expr)))
      (let ((boxed
             (lset-union eq?
               params-boxed
               (lset-intersection eq? (cenv-boxed cenv) closed))))
        (let ((cenv-x (make-cenv params closed boxed)))
          (push-vars
           closed cenv                  ; push closed onto stack
           (vector VMI_CLOSE
                   next
                   (length closed)      ; num of closed on stack
                   (vector
                    (if (formals-rest? formals) VMI_ENTER_REST VMI_ENTER)
                    (make-boxes params params-boxed
                                (compile expr cenv-x
                                         (vector VMI_RETURN (length params))))
                    (length params))))))))) ; num of params, including rest

(define (formals->params formals)
  (let loop ((x formals) (acc '()))
    (cond ((null? x) acc)
          ((pair? x)
           (loop (cdr x) (cons (car x) acc)))
          (else
           (cons x acc)))))

(define (formals-rest? formals)
  (cond ((null? formals) #f)
        ((pair? formals)
         (formals-rest? (cdr formals)))
        (else #t)))

(define (compile-refer x cenv next)
  (compile-lookup
   x (cenv-params cenv) (cenv-closed cenv)
   (lambda (k) (vector VMI_REF_PARAM next k))
   (lambda (k) (vector VMI_REF_CLOSED next k))
   (lambda (v) (vector VMI_REF_GLOBAL_SYMBOL next v))))

(define (compile-lookup x params closed return-params return-closed return-global)
  (let ((k (find-index x params)))
    (if k
        (return-params k)
        (let ((k (find-index x closed)))
          (if k
              (return-closed k)
              (return-global x) )))))

(define (find-index x xs)
  (find-index-loop x xs 0))

(define (find-index-loop x xs k)
  (cond ((null? xs) #f)
        ((eq? x (car xs)) k)
        (else
         (find-index-loop x (cdr xs) (+ k 1)))))

(define (collect-vars-set candidates expr)
  (cond ((symbol? expr) '())
        ((pair? expr)
         (let ((tag (car expr)))
           (cond ((eq? tag 'quote) '())
                 ((eq? tag 'lambda)
                  (let ((candidates-x
                         (lset-difference eq?
                           candidates (formals->params (cadr expr)))))
                    (collect-vars-set candidates-x (caddr expr))))
                 ((eq? tag 'set!)
                  (let ((v (cadr expr))
                        (s (collect-vars-set candidates (caddr expr))))
                    (if (and (memq v candidates)
                             (not (memq v s)))
                        (cons v s)
                        s)))
                 ((eq? tag 'if)
                  (collect-vars-set-in-list candidates (cdr expr)))
                 ((eq? tag 'begin)
                  (collect-vars-set-in-list candidates (cdr expr)))
                 (else
                  (collect-vars-set-in-list candidates expr)))))
        (else '())))

(define (collect-vars-set-in-list candidates xs)
  (fold-left
    (lambda (s x)
      (lset-union eq?
        s
        (collect-vars-set candidates x)))
    '() xs))

(define (make-boxes params boxes next)
  (let ((n (length params)))
    (let loop ((xs params) (k 0) (c next))
      (if (null? xs)
          c
          (loop (cdr xs) (+ k 1)
                (if (memq (car xs) boxes)
                    (vector VMI_BOX c (- n k 1))
                    c))))))

(define (collect-vars-free candidates expr)
  (cond
   ((symbol? expr)
    (if (memq expr candidates)
        (list expr)
        '()))
   ((pair? expr)
    (let ((tag (car expr)))
      (cond
       ((eq? tag 'quote) '())
       ((eq? tag 'lambda)
        (let ((candidates-x
               (lset-difference eq? candidates (formals->params (cadr expr)))))
          (collect-vars-free candidates-x (caddr expr))))
       ((eq? tag 'set!)
        (let ((v (cadr expr))
              (s (collect-vars-free candidates (caddr expr))))
          (if (and (memq v candidates)
                   (not (memq v s)))
              (cons v s)
              s)))
       ((eq? tag 'if)
        (collect-vars-free-in-list candidates (cdr expr)))
       ((eq? tag 'begin)
        (collect-vars-free-in-list candidates (cdr expr)))
       (else
        (collect-vars-free-in-list candidates expr)))))
   (else '())))

(define (collect-vars-free-in-list candidates xs)
  (fold-left
    (lambda (s x)
      (lset-union eq? s (collect-vars-free candidates x)))
    '() xs))

(define (push-vars vars cenv next)
  (if (null? vars)
      next
      (push-vars (cdr vars) cenv
        (compile-refer (car vars) cenv
          (vector VMI_PUSH next)))))

(define (tail? next)
  (eqv? (vector-ref next 0) VMI_RETURN))
