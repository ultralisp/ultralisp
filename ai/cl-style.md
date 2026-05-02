# Common Lisp Project Style Guide

Common coding conventions shared across CL projects (Ultralisp, Codabrus, etc.).

## Packages and files

- Use ASDF `:package-inferred-system` — each file under `src/` defines its own package matching the path. E.g. `src/models/project.lisp` → package `ultralisp/models/project`.
- Define packages with `uiop:define-package`, explicit `:use #:cl`, and `#:` for package names.
- Prefer `:local-nicknames` instead of `:use` or `:import-from`.
- For external symbols used very often, add `:import-from` to import them.
- List `:export` explicitly. Do not export internal symbols.
- Do not edit files inside `.qlot/` — they are third-party dependencies.

## Types and declarations

- Use `serapeum/types` `->` for function type declarations when available.
- Prefer `(declare (ignore var))` for unused parameters in methods/lambdas.

## Control flow and iteration

- Use `loop` without keyword style (`while`, `do`, `finally`, `repeat`, `for`) for iteration.
- Use `ecase` / `etypecase` for exhaustive dispatch; signal clear errors with `format`.

## Abstractions and macros

- Prefer defining macros for repetitive or boilerplate-heavy patterns.
- Keep macros small and readable; generate code that matches the project's existing style.
- Use `defgeneric` / `defmethod` for polymorphic behavior; document with `:documentation`.

## Error handling and conditions

- Use `define-condition` with slots and `:report` for project-specific errors.
- Use `error` with a format string and arguments rather than raw strings.

## CLOS style

- Use `defclass`, not `defstruct`.
- Write a function-constructor for each CLOS class to hide `make-instance`.
  - Required slots → required positional parameters.
  - Optional slots → `&key` parameters declared via `&rest restargs` and passed to `make-instance` via `apply`.
  - List all `&key` in signature, then `(declare (ignore ...))` for documentation.

```lisp
(defun make-session (project-dir &rest restargs &key state)
  (declare (ignore state))
  (apply #'make-instance 'session :project-dir project-dir restargs))
```

## General style

- Prefer `defun`/`defgeneric`/`defmethod` with type declarations where used in the codebase.
- Use `let`/`let*` for locals; use `shiftf` and `mod` where they clarify numeric logic.
- Prefer explicit slot accessors with `:initarg` keywords; use `defclass` with `:reader`/`:accessor`.

## Interactive development

- Work with running lisp image via `eval_lisp_form` MCP tool, if available.
- To avoid waiting forever, wrap forms in `(sb-sys:with-deadline (:seconds 10) ..body..)`.
- When redefining a class (e.g. `defstruct` → `defclass`), delete the package first:
  ```lisp
  (delete-package :my-package)
  (asdf:load-system "my-system" :force '("my-system/my-file"))
  ```

## Paren safety

- Don't write large functions (100+ lines). Split into helpers.
- Before `asdf:load-system`, check paren balance.
- Count closing parens carefully — `))))` errors are the most common.
