# msu

my scheme utility

## #" read-syntax

~~~~~{.scheme}

(let ([name "yagi"])
  (display #"hello \$${name}\$\n")) ; => hello $yagi$

~~~~~

to compile this, `csc -X msu source.scm`
