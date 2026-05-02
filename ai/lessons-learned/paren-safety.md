# Paren safety in Lisp files

### Don't write large files at once

Split large functions into helpers. A 100-line function with 10 nesting levels is a constant source of paren errors.

### Check paren balance before compilation

```lisp
(let* ((content (uiop:read-file-string "path/to/file.lisp"))
       (lines (cl-ppcre:split "\\n" content))
       (depth 0))
  (loop for line in lines
        for i from 1
        for d = (let ((dd 0) (in-str nil) (in-com nil))
                  (loop for ch across line
                        do (cond (in-com)
                                 (in-str (when (char= ch #\") (setf in-str nil)))
                                 ((char= ch #\;) (setf in-com t))
                                 ((char= ch #\") (setf in-str t))
                                 ((char= ch #\() (incf dd) (incf depth))
                                 ((char= ch #\)) (decf dd) (decf depth))))
                  dd)
        when (< depth 0)
          do (format t "*** NEGATIVE at line ~A~%" i))
  (format t "Final depth: ~A~%" depth))
```

If `Final depth` ≠ 0 — the file has an error. Negative depth = extra `)` on the indicated line.

### Count levels before `))))`

The most common error: one too many or too few closing parens. Before `))))`, mentally name each level being closed.

### When rewriting a large block, match closing parens to the NEW structure

When replacing a `table`-based layout with a `div`-based layout (or any similar structural rewrite), the nesting levels change because HTML tags have different nesting. The most reliable approach:

1. **Write the new block top-to-bottom, explicitly tracking each open level.**
2. **At the end, count open parens and write exactly that many close parens.**
3. **Run the paren-balance check IMMEDIATELY after editing** — don't batch multiple large edits before checking.

**Root cause of the bug (ultralisp source.lisp refactor):** When converting `<table><thead><tr><th>...</th></tr></thead><tbody><tr><td>...</td></tr></tbody></table>` to `<div><div>header</div><div class="divide-y"><div>row</div></div></div>`, the nesting structure changed (e.g. `table>tbody` became a single `div`, no separate `thead/tbody`). The closing paren count at the end of each method was carried over by analogy with the old structure, resulting in one extra `)` per method (4 methods = 4 extra parens).

**Lesson:** After any large structural rewrite, always verify paren balance before moving to the next edit. A paren-balance script takes seconds to run and catches these errors immediately.
