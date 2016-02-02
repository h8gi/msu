;;; "hello ${(+ 3 5)}" => "hello 5"
(define (expand-string str)
  (irregex-replace/all
   '(: "$" "{" ($ (*? any)) "}") str
   (lambda (m)
     (with-input-from-string (irregex-match-substring m 1)
       (lambda ()
         (let ([val (eval (read))])
           (if (eof-object? val) "" (->string val))))))))

;;; read and process
(define (call-with-lines proc #!optional (port (current-input-port)))
  (let loop ([line (read-line port)])
    (unless (eof-object? line)
      (proc line)
      (loop (read-line port)))))

;;; statistics
(define (sum-n-mean lst)
  (define (inner lst sum n)
    (cond ((null? lst) (values sum n (/ sum n)))
          (else (inner (cdr lst) (+ (car lst) sum) (add1 n)))))
  (inner lst 0 0))
(define (sum lst)
  (define (inner lst s)
    (cond ((null? lst) s)
          (else (inner (cdr lst) (+ (car lst) s)))))
  (inner lst 0))

(define (mean lst)
  (let-values ([(s n m) (sum-n-mean lst)])
    m))
