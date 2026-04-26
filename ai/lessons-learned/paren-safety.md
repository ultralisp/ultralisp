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
