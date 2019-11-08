(use gauche.record)
(use srfi-1)
(use file.util)
(use util.match)

(define cur-script-path (current-load-path))

(add-load-path "." :relative)

(load "vminst-defs.scm")
(load "expand.scm")
(load "compiler.scm")
(load "compile-file.scm")

(define (main args)
  (let-values
      (((dir fn ext)
        (decompose-path cur-script-path)))
    (compile-file (format #f "~A/macro.scm" dir))
    (for-each compile-file (cdr args))
    0))
