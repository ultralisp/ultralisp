# Rove: `signals` syntax

```lisp
;; CORRECT — body as first argument:
(signals (some-expression-that-should-signal))
(signals (some-expression) 'my-error-type)

;; WRONG:
(signals (error)
  (some-expression))
```
