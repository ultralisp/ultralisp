# event-emitter: argument order in `on`

The `event-emitter` library has inconsistent argument ordering:
- `on` and `emit`: `(event object ...)` — **event first**
- `remove-listener`, `add-listener`, `listeners`: `(object event ...)` — **object first**

```lisp
;; CORRECT:
(event-emitter:on :tool-call provider handler)
(event-emitter:emit :tool-call provider call-id tool-name args)
(event-emitter:remove-listener provider :tool-call handler)

;; WRONG (object first in on):
(event-emitter:on provider :tool-call handler)
```

Wrong order gives `NO-APPLICABLE-METHOD-ERROR` on `SILO` because `on` tries to call `silo` on a keyword.
