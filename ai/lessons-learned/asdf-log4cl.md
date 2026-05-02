# ASDF package-inferred systems and log4cl

If a file uses package `LOG` via `log:info` without `:import-from`, ASDF doesn't know to load `log4cl`, and compilation fails with "Package LOG does not exist".

Fix: add explicit import in `uiop:define-package`:

```lisp
(:import-from #:log #:info)
```

This lets ASDF inference find the dependency through `(asdf:register-system-packages "log4cl" '("LOG"))` in the main `.asd`.

Then use the imported symbol directly: `(info "...")` instead of `(log:info "...")`.
