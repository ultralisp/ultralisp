# yason: with-output-to-string* and nil

`yason:with-output-to-string*` sets the output stream. Use `yason:encode` without an explicit stream argument:

```lisp
;; CORRECT:
(yason:with-output-to-string* ()
  (yason:encode object))

;; WRONG — nil suppresses output, returns empty string:
(yason:with-output-to-string* ()
  (yason:encode object nil))
```

Yason flags for round-trip: `:json-arrays-as-vectors t`, `:json-booleans-as-symbols t`, `:json-nulls-as-keyword t`. Without `:json-nulls-as-keyword`, both `nil` and `[]` become an empty JSON array.
