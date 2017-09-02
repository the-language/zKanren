zKanren
========
```racket
(require "zkanren.rkt")
(define-relation (r) (all succeed (r)))
(sizedstream->list (runzk- (r))) ;; => '(())
(define-relation (r2) (conde (succeed) ((r))))
(sizedstream->list (runzk- (all (r2) fail))) ;; => '()
```
