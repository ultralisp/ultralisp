# Ultralisp

You are a professional Common Lisp programmer. Use best practices, build clear abstractions (including with macros), and write idiomatic code consistent with this project.

## What is Ultralisp

Ultralisp is a Quicklisp-compatible Common Lisp distribution server. Users add GitHub-hosted Lisp projects; the system automatically builds a distribution every few minutes. Runs as a web app + background workers, stores artifacts on S3.

## Development Commands

### Starting the environment

```bash
lake  # starts PostgreSQL, ElasticSearch, Gearman in Docker
```

In a Lisp REPL (`qlot exec ros emacs`):

```lisp
;; Web server
(ql:quickload :ultralisp/server)
(ultralisp/server:start-outside-docker)

;; Background worker (separate REPL)
(ql:quickload :ultralisp/worker)
(ultralisp/worker:start-outside-docker)
```

### Running tests

```lisp
(asdf:test-system :ultralisp-test)                              ; all tests
(rove:run :ultralisp-test/models/dist)                          ; module
(rove:run-test 'ultralisp-test/models/dist::test-adding-new-dist) ; single test
```

Tests use **Rove** + **Hamcrest**. DB tests use `with-test-db` (isolated schema).

### Running project checks in the REPL

```lisp
(40ants-logging:setup-for-repl :level :info)
(ultralisp/db:connect-toplevel)
(ql:quickload :ultralisp/dev)
(ultralisp/dev:run-check "guicho271828/type-i")
```

### Dependency management

`qlot` manages deps. After changing `qlfile`, run `qlot install`.

## Coding Style

See [`ai/cl-style.md`](ai/cl-style.md) for full CL coding conventions (packages, types, control flow, macros, CLOS, etc.).

Key points:
- ASDF `:package-inferred-system` — each file = its own package
- Define packages with `uiop:define-package`, `:use #:cl`
- Prefer `:local-nicknames` over `:use`
- Use `defclass` (not `defstruct`), with constructor functions

## Architecture

See [`ai/architecture.md`](ai/architecture.md) for detailed subsystem descriptions.

```
Browser ──► Reblocks/Clack web server (ultralisp/server)
               │
               ├── PostgreSQL (Mito ORM)
               ├── ElasticSearch (search)
               └── Gearman (task queue)
                        │
               Background workers (ultralisp/worker)
                        │
               ├── Project checker  (ultralisp/pipeline/)
               ├── Downloader       (ultralisp/downloader/)
               └── Uploader → S3    (ultralisp/uploader/)
```

## Lessons Learned

Lessons are organized as individual files in [`ai/lessons-learned/`](ai/lessons-learned/) — one lesson per file. The index is at [`ai/lessons-learned/index.md`](ai/lessons-learned/index.md).

As you work on this project, add new lessons there:
1. Create a new `.md` file with a descriptive name (e.g. `my-lesson.md`).
2. Include: title, source project, problem, solution with code.
3. Add an entry to `ai/lessons-learned/index.md` in the appropriate section.
