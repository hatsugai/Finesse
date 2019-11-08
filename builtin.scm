;;; (name num-params rest?)

(define char-rename-map
  '((#\! . "_EX_")
	(#\$ . "_DOL_")
	(#\% . "_PER_")
	(#\& . "_AMP_")
	(#\* . "_AST_")
	(#\+ . "_PLUS_")
	(#\- . "_MINUS_")
	(#\. . "_DOT_")
	(#\/ . "_SL_")
	(#\: . "_COL_")
	(#\< . "_LESS_")
	(#\= . "_EQ_")
	(#\> . "_GRE_")
	(#\? . "_QUES_")
	(#\@ . "_AT_")
	(#\^ . "_CIR_")
	(#\~ . "_TIL_")))

(define (rename sym)
  (let loop ((cs (string->list (symbol->string sym))) (rs '()))
	(if (null? cs)
		(list->string (reverse rs))
		(let ((c (car cs)))
		  (let ((p (assv c char-rename-map)))
			(if p
				(loop (cdr cs)
					  (append (reverse (string->list (cdr p))) rs))
				(loop (cdr cs) (cons c rs))))))))

(define (for-each/index proc xs)
  (let loop ((xs xs) (k 0))
	(when (pair? xs)
	  (proc k (car xs))
	  (loop (cdr xs) (+ k 1)))))

(define (gen-scheme-defs basename defs)
  (with-output-to-file
	  (format #f "~A.init" basename)
	(lambda ()
	  (for-each/index
		(lambda (k x)
		  (format #t "(~A #(0 ~A ~A ~A))\n" (car x) k (cadr x) (caddr x)))
		defs))))

(define (gen-builtin-header basename defs)
  (with-output-to-file
	  (format #f "~A.h" basename)
	(lambda ()
	  (for-each/index
		(lambda (k x)
		  (format #t "#define PRIMITIVE_NO_~A ~A\n" (rename (car x)) k))
		defs)
      (format #t "\n#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n")
	  (for-each
		(lambda (x)
		  (format #t "opt builtin_~A(vm_context *vm, unsigned argc, const opt *argv);\n" (rename (car x))))
		defs)
      (format #t "\n#ifdef __cplusplus\n}\n#endif\n"))))

(define (gen-builtin-table basename defs)
  (with-output-to-file
	  (format #f "~A-table.c" basename)
	(lambda ()
	  (format #t "#include \"object.h\"\n")
	  (format #t "#include \"~A.h\"\n\n" basename)
	  (format #t "pf_builtin_t builtin_procedure[] = {\n")
	  (for-each
		(lambda (x)
		  (format #t "    &builtin_~A,\n" (rename (car x))))
		defs)
	  (format #t "};\n"))))

(define (load-defs-file path acc)
  (let ((port (open-input-file path)))
    (let loop ((x (read port)) (acc acc))
      (if (eof-object? x)
          (begin
            (close-input-port port)
            acc)
          (loop (read port) (cons x acc))))))

(define (load-defs path-list)
  (let loop ((ps path-list) (acc '()))
    (if (null? ps)
        (reverse acc)
        (loop (cdr ps) (load-defs-file (car ps) acc)))))

;;; args = (basename defs-filename ...)
(define (main args)
  (let ((basename (cadr args))
        (defs (load-defs (cddr args))))
    (gen-scheme-defs basename defs)
    (gen-builtin-header basename defs)
    (gen-builtin-table basename defs)
    0))
