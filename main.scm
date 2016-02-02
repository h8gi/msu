(use irregex)

(define (expand-string str)
  (irregex-replace/all
   '(: "$" "{" ($ (*? any)) "}") str
   (lambda (m)
     (with-input-from-string (irregex-match-substring m 1)
       (lambda ()
         (let ([val (eval (read))])
           (if (eof-object? val) "" (->string val))))))))
