# flet vs labels for mutual references

`flet` does not allow local functions to reference each other. If `flush-section` calls `flush-pending-tool-call`, use `labels`:

```lisp
;; CORRECT:
(labels ((flush-text () ...)
         (flush-section () (flush-text))) ; OK
  ...)

;; WRONG:
(flet ((flush-text () ...)
       (flush-section () (flush-text))) ; UNDEFINED-FUNCTION
  ...)
```
