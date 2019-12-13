(define (req p)
  (if (not p) (amb)))

(define (an-element-of items)
  (req (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (conflict? m k qs)
  (if (memv k qs)
      #t
      (let loop ((a (+ k 1)) (b (- k 1)) (qs qs))
        (if (null? qs)
            #f
            (let ((q (car qs)))
              (if (or (eqv? a q)
                      (eqv? b q))
                  #t
                  (loop (+ a 1) (- b 1) (cdr qs))))))))

(define (interval a b)
  (if (>= a b)
      '()
      (cons a (interval (+ a 1) b))))

(define (iota n)
  (interval 0 n))

(define (q m n qs)
  (if (= n m)
      qs
      (let ((k (an-element-of (iota m))))
        (if (conflict? m k qs)
            (amb)
            (q m (+ n 1) (cons k qs))))))

(define (main)
  (let ((qs (q 8 0 '())))
    (%prn qs)
    (amb)
    0))
