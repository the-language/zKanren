zKanren
========
警告:zKanren有bug,不再维护,之后将被重写
```racket
(require "zkanren.rkt")
(runzk* () fail) ;; => '(())
(define-relation (r) (all succeed (r)))
(runzk* () (r)) ;; => '(() ())
(define-relation (r2) (conde (succeed) ((r2))))
(runzk* () (all (r2) fail)) ;; => '(())
```
