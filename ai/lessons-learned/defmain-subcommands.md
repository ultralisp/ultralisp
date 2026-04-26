# defmain: subcommands gotchas

### Call subcommand via qualified symbol

`defmain` with `&subcommand` generates a local function `DEFMAIN:SUBCOMMAND`. Call it qualified:

```lisp
;; CORRECT:
(defmain:subcommand)

;; WRONG — creates unbound symbol in current package:
(subcommand)
```

### Import parent arguments into subcommands

`defcommand` passes parent args as parameters with symbols from the parent package. Without import, the subcommand sees its own unbound symbols:

```lisp
(:import-from #:codabrus/cli/main
              #:main
              #:model
              #:output)
```

### CL symbol names need shadowing

SBCL locks the COMMON-LISP package. `defcommand (main list)` fails with `SYMBOL-PACKAGE-LOCKED-ERROR`. Fix:

```lisp
(:shadowing-import-from #:defmain #:defcommand)
;; or:
(:shadow #:list)
```

Or just use different names: `ls` instead of `list`, `serve` instead of `do`.
