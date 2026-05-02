# Shadow CL symbols conflicting with route macros

**Source project:** ultralisp
**Date:** 2026-04-26

## Problem

`40ants-routes/defroutes` exports `get` and `post` symbols for defining HTTP route handlers. Importing `get` into a package that `(:use #:cl)` causes a name conflict with `common-lisp:get`.

```lisp
;; ❌ Error: name-conflict between COMMON-LISP:GET and 40ANTS-ROUTES/DEFROUTES:GET
(:import-from #:40ants-routes/defroutes
              #:post
              #:get)
```

## Solution

Use `:shadowing-import-from` to replace the CL symbol. This both shadows the inherited `cl:get` and imports the new one in a single step:

```lisp
(defpackage #:my-app
  (:use #:cl)
  (:shadowing-import-from #:40ants-routes/defroutes
                           #:get)
  (:import-from #:40ants-routes/defroutes
                #:post)
  ...)
```

**Do not** use `:shadow` + `:import-from` for the same symbol — ASDF rejects that with "Parameters :IMPORT-FROM and :SHADOW must be disjoint".

If you still need `cl:get` in the package, use the qualified name `cl:get`.
