zKanren
========
```racket
(require "zkanren.rkt")
(sizedstream->list (runzk (fresh (x y z) (conde [(== x y)] [(=/= x y)]))))
;; => '(((=/= ((<var:1> . <var:2>)))) ((== (<var:1> . <var:2>))))
```