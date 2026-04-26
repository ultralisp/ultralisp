# serapeum:dict returns hash-table, not alist

`serapeum:dict` creates a hash-table. Tests using `assoc`/`cdr` for access are broken — use `gethash`:

```lisp
;; CORRECT:
(gethash "role" entry)

;; WRONG:
(cdr (assoc :role entry))
```

**JSON data rule:** All JSON objects = hash-tables with string keys. Create with `serapeum:dict`, not `make-hash-table`. No alists for JSON data.

```lisp
;; CORRECT:
(serapeum:dict "a" 1 "b" (serapeum:dict "nested" 42))

;; WRONG:
'((:a . 1) (:b . 2))
```
