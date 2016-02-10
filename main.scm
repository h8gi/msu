(use utf8 srfi-1 socket)
(define (zero-fill str limit #!key (fill #\0) (right? #f))
  (let ([dif (- limit (string-length str))])
    (if (>= dif 0)
        (if right?
            (string-append str (make-string dif fill))
            (string-append (make-string dif fill) str))
        (if right?
            (string-take str limit)
            (string-drop str (abs dif))))))


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
;;; python's range
(define range
  (let ([range-inner
         (lambda (start stop step)
           (define (inner count acc)
             (cond [(< count stop)
                    (inner (+ step count) (cons count acc))]
                   [else (reverse! acc)]))
           (inner start '()))])
    (case-lambda
      [(stop) (range-inner 0 stop 1)]
      [(start stop) (range-inner start stop 1)]
      [(start stop step) (range-inner start stop step)])))

;;; hash table
(define (ht-ref ht key)
  (hash-table-ref/default ht key #f))
(define (set-default! ht key val)
  (unless (hash-table-exists? ht key)
    (hash-table-set! ht key val )))
(define (ht-inc! ht key val)
  (hash-table-set! ht key (+ val (ht-ref ht key))))
(define (ht-set! ht key val)
  (hash-table-set! ht key val))


;;; string util
(define (es-read #!optional (port (current-input-port)))
  (define (inner acc exp)
    (let ([ch (read-char)])
      (cond
       [(char=? ch #\")        
        `(conc ,@(reverse! (cons exp acc)))]
       [(char=? ch #\\)
        (let ([next (read-char)])
          (case next
            [(#\n) (inner acc (conc exp #\newline))]
            [(#\\) (inner acc (conc exp #\\))]
            [(#\a) (inner acc (conc exp #\alarm))]
            [(#\t) (inner acc (conc exp #\tab))]
            [(#\r) (inner acc (conc exp #\return))]
            [(#\b) (inner acc (conc exp #\backspace))]
            [(#\v) (inner acc (conc exp #\vtab))]
            [(#\f) (inner acc (conc exp #\page))]
            [else  (inner acc (conc exp next))]))]
       [(char=? ch #\$)                 ; substitute expression in ${exp} or $exp
        (if (char=? (peek-char) #\{)
            (begin (read-char)
                   (let* ([n-exp (read)])
                     (read-char)
                     (inner (cons n-exp (cons exp acc)) "")))
            (inner (cons (read) (cons exp acc)) ""))]
       [else (inner acc (conc exp ch))])))
  (with-input-from-port port
    (lambda () (inner '() ""))))


;;; syntax
;;; "hello ${(+ 3 5)}" => "hello 5"
(set-sharp-read-syntax!
 #\"
 es-read)

;;; macro

(define-syntax while/cc
  (syntax-rules ()
    [(_ return test body ...)
     (call/cc
      (lambda (return)
        (let loop ()
          (when test
            body ...
            (loop)))))]))

;;; socket send 
(define (socket-send-all-to so buf saddr)
  (when (= (fold (lambda (msg len)
                   (socket-send-to so msg saddr))
                 0
                 (string-chop (if (blob? buf) (blob->string buf) buf) (socket-send-size)))
           (socket-send-size))
    (socket-send-to so "" saddr)))
