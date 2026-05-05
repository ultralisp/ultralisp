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

Thus use these functions, defined in src/utils.lisp module:

```
(-> to-json (t &key (:indent (or null integer)))
    (values string &optional))


(defun to-json (obj &key indent)
  (with-output-to-string* (:indent indent)
    (yason:encode obj)))


(-> from-json (string)
    (values t &optional))

(defun from-json (string)
  (yason:parse string
               :json-arrays-as-vectors t
               :json-booleans-as-symbols t
               :json-nulls-as-keyword t))
```

Or define them if they are missing.

Note, that with these settings booleans and nulls are returned as Lisp symbols:

```
CL-USER> (yason:parse "[1, 2, null, true, false]" :json-arrays-as-vectors t :json-booleans-as-symbols t :json-nulls-as-keyword t)
#(1 2 :NULL YASON:TRUE YASON:FALSE)
```

And when preparing object for serialization, you need to transform it to hash-tables and arrays having such symbols as values.
