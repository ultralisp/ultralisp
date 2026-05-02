# String literals: "\n" is NOT a newline

`"\n"` in Common Lisp is the literal characters backslash + `n`, NOT a newline.

Correct ways to write a newline:

```lisp
(string #\Newline)           ; character constant
(format nil "line1~%line2")  ; via ~%
```

In tests, `"a\nb"` ≠ `"a" + #\Newline + "b"`. Tests with `"\n"` may pass for the wrong reason (both sides equally "wrong").
