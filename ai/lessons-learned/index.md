# Lessons Learned — Index

Each file in this directory covers one concrete lesson. Files are named descriptively.

## General Common Lisp

| File | Topic |
|------|-------|
| [string-newline.md](string-newline.md) | `"\n"` is NOT a newline in CL |
| [log-info-not-format.md](log-info-not-format.md) | Use `log:info` instead of `format t` |
| [asdf-log4cl.md](asdf-log4cl.md) | ASDF package-inference + log4cl dependency |
| [flet-vs-labels.md](flet-vs-labels.md) | `labels` for mutual references, not `flet` |
| [clos-late-binding.md](clos-late-binding.md) | CLOS method late binding / shadowing |
| [testing-private-functions.md](testing-private-functions.md) | Exporting `%private` symbols for testing |
| [paren-safety.md](paren-safety.md) | Avoiding paren mismatch errors |
| [json-null.md](json-null.md) | JSON `:null` is a keyword, not CL `nil` |
| [serapeum-dict-hashtable.md](serapeum-dict-hashtable.md) | `serapeum:dict` → hash-table, not alist |
| [yason-output.md](yason-output.md) | `yason:with-output-to-string*` and nil pitfall |

## Rove Testing

| File | Topic |
|------|-------|
| [rove-signals-syntax.md](rove-signals-syntax.md) | Rove `signals` macro syntax |

## Library-Specific

| File | Topic |
|------|-------|
| [event-emitter-arg-order.md](event-emitter-arg-order.md) | `event-emitter` inconsistent arg order |
| [defmain-subcommands.md](defmain-subcommands.md) | `defmain` subcommand gotchas |

## Ultralisp-Specific

| File | Topic |
|------|-------|
| [mito-migrations-schema.md](mito-migrations-schema.md) | Keep `db/schema.sql` in sync with migrations |

---

### How to add a new lesson

1. Create a new `.md` file in `ai/lessons-learned/` with a descriptive name.
2. Include: a short title, which project it came from, the problem, and the solution with code examples.
3. Add an entry to this index in the appropriate section.
