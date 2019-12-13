;; debug
(%abort 0 #f)
(%prn 1 #t)

;; 6.1
(eq? 2 #f)

;; 6.2
(fixnum? 1 #f)
(flonum? 1 #f)
(< 3 #t)
(> 3 #t)
(<= 3 #t)
(>= 3 #t)
(* 1 #t)
(+ 1 #t)
(- 2 #t)
(/ 2 #t)
(quotient 2 #f)
(modulo 2 #f)
(exp 1 #f)
(log 2 #t)
(sin 1 #f)
(cos 1 #f)
(tan 1 #f)
(asin 1 #f)
(acos 1 #f)
(atan 2 #t)
(sqrt 1 #f)
(expt 2 #f)

;; 6.3
(boolean? 1 #f)

;; 6.4
(pair? 1 #f)
(cons 2 #f)
(car 1 #f)
(cdr 1 #f)
(set-car! 2 #f)
(set-cdr! 2 #f)

;; 6.5 Symbols
(symbol? 1 #f)
(symbol->string 1 #f)
(string->symbol 1 #f)

;; 6.6 Characters
(char? 1 #f)
(char->integer 1 #f)
(integer->char 1 #f)

;; 6.7 Strings
(string? 1 #f)
(make-string 2 #t)
(string-length 1 #f)
(string-ref 2 #f)
(string-set! 3 #f)

;; 6.8 Vectors
(vector? 1 #f)
(make-vector 2 #t)
(vector-length 1 #f)
(vector-ref 2 #f)
(vector-set! 3 #f)

;; 6.9 Bytevectors
(bytevector? 1 #f)
(make-bytevector 2 #t)
(bytevector-length 1 #f)
(bytevector-u8-ref 2 #f)
(bytevector-u8-set! 3 #f)

;; 6.10 Control features
(procedure? 1 #f)
(apply 3 #t)
(call/cc 1 #f)
(sys_resume_cont 1 #f)
