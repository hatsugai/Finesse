(define vm-instructions
  '(BUILTIN                             ; fixed 0
	CONSTANT
	PUSH
	INDIRECT
	REF_PARAM
	REF_CLOSED
	REF_GLOBAL_SYMBOL
	REF_GLOBAL
	ASSIGN_PARAM
	ASSIGN_CLOSED
	ASSIGN_GLOBAL_SYMBOL
	ASSIGN_GLOBAL
	CLOSE
	BRANCH
	BOX
	APPLY
	SHIFTJUMP
	ENTER
	ENTER_REST
	RETURN
    AMB))

(define (for-each/index proc xs)
  (let loop ((xs xs) (k 0))
	(when (pair? xs)
	  (proc k (car xs))
	  (loop (cdr xs) (+ k 1)))))

(define (gen-scheme-defs)
  (with-output-to-file
	  "vminst-defs.scm"
	(lambda ()
	  (for-each/index
		(lambda (k inst)
		  (format #t "(define VMI_~A ~A)\n" inst k))
		vm-instructions))))

(define (gen-c-defs)
  (with-output-to-file
	  "vminst-defs.h"
	(lambda ()
	  (format #t "#define NUM_INSTRUCTIONS ~A\n" (length vm-instructions))
	  (for-each/index
		(lambda (k inst)
		  (format #t "#define VMI_~A ~A\n" inst k))
		vm-instructions))))

(define (gen-c-inst-names)
  (with-output-to-file
	  "vminst-defs.c"
	(lambda ()
	  (format #t "const char *instruction_name[] = {\n")
	  (for-each/index
		(lambda (k inst)
		  (format #t " /* ~A */ \"~A\",\n" k inst))
		vm-instructions)
	  (format #t "};\n"))))


(define (main args)
  (gen-scheme-defs)
  (gen-c-defs)
  (gen-c-inst-names)
  0)
