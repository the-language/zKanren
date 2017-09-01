zKanren
========
```racket
(require "zkanren.rkt")
(sizedstream->list (runzk (fresh (x y z) (conde [(== x y)] [(=/= x y)]))))
;; => '(((=/= ((<var:1> . <var:2>)))) ((== (<var:1> . <var:2>))))
(define-relation (r) (all succeed (r)))
(sizedstream->list (runzk (r))) ;; => '(())
(define-relation (r2) (conde (succeed) ((r))))
(sizedstream->list (runzk (all (r2) fail))) ;; => '()
```
