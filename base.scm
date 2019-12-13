;;;; 6.1

(define (eqv? x y) (eq? x y))

(define (%vector=? x y)
  (let ((n (vector-length x)))
    (if (eqv? n (vector-length y))
        (let loop ((k 0))
          (if (eqv? k n)
              #t
              (if (equal? (vector-ref x k) (vector-ref y k))
                  (loop (+ k 1))
                  #f)))
        #f)))

(define (%bytevector=? x y)
  (let ((n (bytevector-length x)))
    (if (eqv? n (bytevector-length y))
        (let loop ((k 0))
          (if (eqv? k n)
              #t
              (if (eqv? (bytevector-u8-ref x k) (bytevector-u8-ref y k))
                  (loop (+ k 1))
                  #f)))
        #f)))

(define (string=? x y)
  (let ((n (string-length x)))
    (if (eqv? n (string-length y))
        (let loop ((k 0))
          (if (eqv? k n)
              #t
              (if (eqv? (string-ref x k) (string-ref y k))
                  (loop (+ k 1))
                  #f)))
        #f)))

(define (equal? x y)
  (cond ((eqv? x y) #t)
        ((pair? x)
         (and (pair? y)
              (equal? (car x) (car y))
              (equal? (cdr x) (cdr y))))
        ((string? x)
         (and (string? y)
              (string=? x y)))
        ((vector? x)
         (and (vector? y)
              (%vector=? x y)))
        ((bytevector? x)
         (and (bytevector? y)
              (%bytevector=? x y)))
        (else #f)))

(define (null? x) (eq? x '()))

(define (error msg . r)
  (apply %prn "error: " msg r)
  (%abort))
