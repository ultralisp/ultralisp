# Use log:info instead of format for output

Use `log:info` instead of `(format t ...)`. This gives structured output, log levels, and the ability to redirect/disable output without code changes.

```lisp
;; CORRECT — fully qualified symbol:
(log:info "tokens: ~A in / ~A out | cost: $~,3F" turn-in turn-out turn-cost)

;; WRONG — format:
(format t "~&[tokens: ~A in / ~A out | cost: $~,3F]~%" turn-in turn-out turn-cost)

;; WRONG — importing from LOG:
(:import-from #:log #:info)
(info "...")
```

Write `log:info` directly, without importing. The `log4cl` dependency should be registered in the `.asd` via `asdf:register-system-packages`.
