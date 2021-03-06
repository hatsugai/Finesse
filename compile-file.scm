(define cenv0 (make-cenv '()' () '()))
(define next0 (vector #f))              ; for 'tail?'

(define (compile-toplevel form)
  (if (pair? form)
      (let ((tag (car form)))
        (cond ((eq? tag 'define)
               (let ((x (compile-define form)))
                 (format #t "~S\n" x)))
              ((eq? tag 'define-macro)
               (compile-define-macro form))
              ((eq? tag 'begin)
               (for-each compile-toplevel (cdr form)))
              (else
               (let ((transformer (find-transformer tag)))
                 (if transformer
                     (let ((x (apply transformer (cdr form))))
                       (compile-toplevel x))
                     (error "unknown form" form))))))
      (error "unknown form" form)))

(define (compile-define-macro form)
  (let ((name (car (cadr form)))
        (formals (cdr (cadr form)))
        (body (cddr form)))
    (let ((expr (list 'lambda formals (cons 'begin body))))
      (let ((transformer (eval expr interaction-environment)))
        (register-transformer name transformer)))))

(define (compile-define form)
  (let ((x (cadr form)))
    (if (symbol? x)
        (list x (caddr form))
        (let ((name (car x))
              (formals (cdr x))
              (body (cddr form)))
          (let ((expr (expand `(lambda ,formals ,@body))))
            (let ((code (compile expr cenv0 next0)))
              ;; (close next num-closed code)
              (list name (vector-ref code 3))))))))

(define (compile-file filename)
  (let ((port (open-input-file filename)))
    (let loop ((x (read port)))
      (unless (eof-object? x)
        (compile-toplevel x)
        (loop (read port))))
    (close-input-port port)))
