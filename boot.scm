(define (rev xs)
  (if (null? xs)
      '()
      (app (rev (cdr xs)) (cons (car xs) '()))))

(define (app xs ys)
  (if (null? xs)
      ys
      (app (rev (cdr (rev xs)))
           (cons (car (rev xs)) ys))))

(define (main)
  (%prn (apply rev '((0 1 2 3 4 5 6 7 8 9))))
  0)
