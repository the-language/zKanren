zKanren
========
```racket
(require "zkanren.rkt")
(runzk* () fail) ;; => '(())
(define-relation (r) (all succeed (r)))
(runzk* () (r)) ;; => '(() ())
(define-relation (r2) (conde (succeed) ((r2))))
(runzk* () (all (r2) fail)) ;; => '(())
```
