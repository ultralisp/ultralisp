# JSON null representation


`serapeum:dict` with `:null` value: `(gethash "content" entry)` → `:NULL` (keyword). `(null ...)` returns NIL for `:NULL`.

Correct check:
```lisp
(eq :null (gethash "content" entry))
```
