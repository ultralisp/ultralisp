# Ultralisp — Architecture Details

## Architecture Diagram

```
Browser ──► Reblocks/Clack web server (ultralisp/server)
               │
               ├── PostgreSQL (Mito ORM)
               ├── ElasticSearch (symbol/project search)
               └── Gearman (task queue)
                        │
               Background workers (ultralisp/worker)
                        │
               ├── Project checker (ultralisp/pipeline/)
               ├── Downloader (ultralisp/downloader/)
               └── Uploader → S3 (ultralisp/uploader/)
```

## Key Subsystems

| Subsystem | Entry point | Purpose |
|-----------|------------|---------|
| Web server | `ultralisp/server:start` | Reblocks-based web UI + REST API |
| Worker | `ultralisp/worker:process-jobs` | Gearman job consumer |
| Builder | `ultralisp/main:main` | CLI that builds dist metadata |
| Pipeline | `ultralisp/pipeline/` | Orchestrates check → build → upload |
| Models | `ultralisp/models/` | Mito ORM models (project, dist, source, version, etc.) |
| Widgets | `ultralisp/widgets/` | Reblocks UI components |
| API | `ultralisp/api/` | REST/OpenRPC endpoints |
| GitHub | `ultralisp/github/` | OAuth, webhooks, API calls |
| Downloader | `ultralisp/downloader/` | Fetches project source from GitHub/Git |
| Uploader | `ultralisp/uploader/` | Stores artifacts (S3 or fake local backend) |
| RPC | `ultralisp/rpc/` | Serializes commands sent through Gearman |

## Infrastructure (docker-compose.yml)

- **Postgres** — Primary database for projects, versions, checks, sources, dists
- **ElasticSearch** — Full-text search index for symbols, packages, and projects
- **Gearman** — Job queue system for distributing tasks between app and workers
- **App Server** — Reblocks-based web server on Hunchentoot (port 8080)
- **Worker** — Task processor (SBCL or LispWorks), receives tasks from Gearman
- **S3 / Fake Uploader** — Storage for distribution archives

## Core Entry Points

- `src/main.lisp:main` — CLI for building distributions locally
- `src/server.lisp:start` — Web server entry point, initializes Reblocks app, cron jobs
- `src/server.lisp:start-outside-docker` — Development mode for local REPL
- `src/worker.lisp:main` — Worker entry point, processes Gearman tasks
- `src/worker.lisp:start-outside-docker` — Development mode for worker

## Database Layer (src/db.lisp)

- Connection management with `cl-dbi` and `mito:*connection*`
- Transaction handling: `with-connection`, `with-transaction`
- Advisory locks: `with-lock`, `get-lock`, `try-to-get-lock` for distributed locking
- Cached connections mode for performance in worker threads
- Export: `with-connection`, `with-transaction`, `with-lock`, `execute`, `sql-fetch-all`
- PostgreSQL via **Mito** ORM. Migrations live in `src/migrations/` as numbered SQL files, applied at startup. `ultralisp/db:connect-toplevel` connects the REPL.

## Models (src/models/)

All models use Mito ORM and inherit from `dao-table-class`:

- **project.lisp** — `project` class (`project-name`, `enabled`, `disable-reason`); `update-and-enable-project`, `get-all-projects`, `get-project2`
- **check.lisp** — `check2` class (`:pending`, `:success`, `:error`); types: `:added-project`, `:changed-project`, `:via-cron`, `:via-webhook`, `:manual`
- **version.lisp** — `version` class (`:pending`, `:built`, `:error`); `make-version`, `get-prepared-versions`
- **dist.lisp** — `dist` class (name, base-url, state); `common-dist`, `get-prepared-dists`
- **source.lisp** — `source` class (URL, type, systems-info JSONB); `source-systems-info`, `get-all-sources`
- **index.lisp** — `index` class for ElasticSearch indexing; `get-index-status`, `reschedule-indexing`
- **moderator.lisp** — Moderation and permissions
- **dist-source.lisp** — Many-to-many between dists and sources

## RPC System (src/rpc/)

Gearman-based task distribution with command pattern:

- **core.lisp** — `serialize`/`deserialize` (cl-store + base64), `submit-task`, `gearman-call`
- **command.lisp** — `defcommand` for immediate/deferred execution, `task-with-commands`, `with-commands-processor`. Worker (read-only DB) sends write commands to master (write DB).

## Build Pipeline (src/builder.lisp, src/pipeline/)

- **builder.lisp** — `build` (download, check deps, create quickdist, upload to S3), `build-pending-dists`
- **pipeline/checking.lisp** — `perform-pending-checks` via worker

## Download/Upload (src/downloader/, src/uploader/)

- **downloader/** — base.lisp, github.lisp, git.lisp, version.lisp
- **uploader/** — base.lisp (`upload` GF), s3.lisp, fake.lisp (local dev)

## Search (src/search.lisp)

ElasticSearch integration: `index-objects`, `search-objects`. Uses `ultralisp/packages-extractor-api` to extract packages from ASDF systems. Timeout: `*indexing-timeout*` (default 5 min/project).

## Cron Jobs (src/cron.lisp)

Scheduled tasks via `cl-cron`: `deftask` defines tasks with connection and error handling. Tasks: periodic checks, dist builds, ES indexing. Functions: `setup`, `start`, `stop`, `simulate-cron`.

## Web Application (src/app.lisp, src/server.lisp)

- **app.lisp** — Reblocks routes: `/dist/*`, `/clpi/*`, `/projects/<name>.svg`, `/metrics`, `/webhook/github`, `/webhook/gitlab`, `/api/*`
- **server.lisp** — `start` (init Reblocks, cron, GitHub auth, S3), `ultralisp-server` class, `make-middlewares`, DB connection per request

## Other Subsystems

- **Authentication** — GitHub OAuth via `reblocks-auth`; `ultralisp/github/` handles webhooks + API
- **Sources** (`src/sources/`) — base, github, git, guesser, setup
- **Widgets** (`src/widgets/`) — main, landing, project(s), search, login-menu, version, dist(s), tags, maintenance
- **API** (`src/api/`) — OpenRPC: api, search, projects, server, tags
- **Protocols** (`src/protocols/`) — moderation, external-url, url, enabled, render-changes
- **Utils** (`src/utils/`) — db, github, http, git, lisp, retries, source, text, time, timing, trace
- **Other** — variables.lisp, logging.lisp, mail.lisp, routes.lisp, metrics.lisp, analytics.lisp, badges.lisp, import.lisp
