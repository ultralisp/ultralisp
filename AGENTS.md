# Professional Common Lisp Style

You are a professional Common Lisp programmer. Use best practices, build clear abstractions (including with macros), and write idiomatic code consistent with this project.

## Architecture Overview

Ultralisp is a Quicklisp-compatible Common Lisp software distribution. It consists of the following components:

### Infrastructure (from `docker-compose.yml`)

- **Postgres** - Primary database for projects, versions, checks, sources, dists
- **ElasticSearch** - Full-text search index for symbols, packages, and projects
- **Gearman** - Job queue system for distributing tasks between app and workers
- **App Server** - Reblocks-based web server on Hunchentoot (port 8080)
- **Worker** - Task processor that can run on SBCL or LispWorks, receives tasks from Gearman
- **S3 / Fake Uploader** - Storage for distribution archives

### Core Entry Points

- `src/main.lisp:main` - CLI for building distributions locally
- `src/server.lisp:start` - Web server entry point, initializes Reblocks app, cron jobs
- `src/server.lisp:start-outside-docker` - Development mode for local REPL
- `src/worker.lisp:main` - Worker entry point, processes Gearman tasks
- `src/worker.lisp:start-outside-docker` - Development mode for worker

### Database Layer (`src/db.lisp`)

- Connection management with `cl-dbi` and `mito:*connection*`
- Transaction handling: `with-connection`, `with-transaction`
- Advisory locks: `with-lock`, `get-lock`, `try-to-get-lock` for distributed locking
- Cached connections mode for performance in worker threads
- Export: `with-connection`, `with-transaction`, `with-lock`, `execute`, `sql-fetch-all`

### Models (`src/models/`)

All models use Mito ORM and inherit from `dao-table-class`:

- **project.lisp** - Projects, sources association, GitHub integration
  - `project` class with `project-name`, `enabled`, `disable-reason`
  - Functions: `update-and-enable-project`, `get-all-projects`, `get-project2`
  
- **check.lisp** - Project and source check records for build pipeline
  - `check2` class with `:pending`, `:success`, `:error` status
  - Types: `:added-project`, `:changed-project`, `:via-cron`, `:via-webhook`, `:manual`
  - Functions: `get-pending-checks`, `get-last-project-checks`, `make-check`
  
- **version.lisp** - Distribution versions
  - `version` class with `:pending`, `:built`, `:error` status
  - Functions: `make-version`, `get-prepared-versions`
  
- **dist.lisp** - Distribution configurations
  - `dist` class with name, base-url, state
  - Functions: `common-dist`, `get-prepared-dists`
  
- **source.lisp** - Source repositories (GitHub, Git)
  - `source` class with URL, type, systems-info JSONB
  - Functions: `source-systems-info`, `get-all-sources`
  
- **index.lisp** - Index status tracking
  - `index` class for tracking ElasticSearch indexing
  - Functions: `get-index-status`, `reschedule-indexing`
  
- **moderator.lisp** - Moderation and permissions
- **dist-source.lisp** - Many-to-many between dists and sources

### RPC System (`src/rpc/`)

Gearman-based task distribution with command pattern:

- **core.lisp** - Serialization and task submission
  - `serialize` / `deserialize` - cl-store + base64 encoding
  - `submit-task` - Send task to Gearman with timeout
  - `gearman-call` - Execute task synchronously
  
- **command.lisp** - Command pattern for worker to master communication
  - `defcommand` - Define functions that execute immediately or get caught for delayed execution
  - `task-with-commands` - Worker task wrapper that catches commands to execute in master
  - `with-commands-processor` - Execute caught commands in master process
  - Purpose: Worker (read-only DB) can send write commands to master (write DB)

### Build Pipeline (`src/builder.lisp`, `src/pipeline/`)

- **builder.lisp** - Main build orchestration
  - `build` - Download projects, check dependencies, create quickdist files, upload to S3
  - `build-pending-dists` - Build all pending distributions
  
- **pipeline/checking.lisp** - Check processing
  - `perform-pending-checks` - Process pending checks via worker

### Download/Upload (`src/downloader/`, `src/uploader/`)

- **downloader/base.lisp** - Base downloader protocol and implementations
- **downloader/github.lisp** - GitHub repository downloader
- **downloader/git.lisp** - Git repository downloader
- **downloader/version.lisp** - Version-specific downloading
- **uploader/base.lisp** - Upload protocol with `upload` generic function
- **uploader/s3.lisp** - Amazon S3 uploader
- **uploader/fake.lisp** - Local filesystem uploader for development

### Search (`src/search.lisp`)

ElasticSearch integration for symbol/package/project search:
- `index-objects` - Index projects and their symbols
- `search-objects` - Query ElasticSearch
- Uses `ultralisp/packages-extractor-api` to extract packages from ASDF systems
- Timeout: `*indexing-timeout*` (default 5 minutes per project)

### Cron Jobs (`src/cron.lisp`)

Scheduled tasks using `cl-cron`:
- `deftask` - Define cron tasks with connection and error handling
- Tasks include: periodic checks, dist builds, ElasticSearch indexing
- Functions: `setup`, `start`, `stop`, `simulate-cron`

### Web Application (`src/app.lisp`, `src/server.lisp`)

- **app.lisp** - Reblocks app definition with routes:
  - `/dist/*` - Static dist files
  - `/clpi/*` - CLPI documentation
  - `/projects/<name>.svg` - Badge SVG
  - `/metrics` - Prometheus metrics
  - `/webhook/github` - GitHub webhook endpoint
  - `/webhook/gitlab` - GitLab webhook endpoint
  - `/api/*` - OpenRPC API

- **server.lisp** - Server startup and middleware:
  - `start` - Initialize Reblocks, cron, GitHub auth, S3 uploader
  - `ultralisp-server` class extending `reblocks/server:server`
  - `make-middlewares` - API middleware at `/api`
  - Database connection per request via `with-db-connection-and-request-id`

### Sources Integration (`src/sources/`)

- **base.lisp** - Base source protocol
- **github.lisp** - GitHub source implementation
- **git.lisp** - Git source implementation
- **guesser.lisp** - Auto-detect source type from URL
- **setup.lisp** - Initialize sources on server start

### Widgets (`src/widgets/`)

Reblocks widgets for web UI:
- **main.lisp** - Main routes and navigation
- **landing.lisp** - Landing page
- **project.lisp** - Project detail page
- **projects.lisp** - Projects listing
- **search.lisp** - Search page
- **login-menu.lisp** - Authentication UI
- **version.lisp** - Version detail page
- **dist.lisp**, **dists.lisp** - Distribution pages
- **tags.lisp**, **all-tags.lisp** - Tag browsing
- **maintenance.lisp** - Maintenance mode widget

### API (`src/api/`)

OpenRPC API for external access:
- **api.lisp** - Main API definition
- **search.lisp** - Search endpoints
- **projects.lisp** - Project endpoints
- **server.lisp** - Clack app for `/api` routes
- **tags.lisp** - Tag endpoints

### Protocols (`src/protocols/`)

Generic protocols for extensibility:
- **moderation.lisp** - Moderation protocol
- **external-url.lisp** - External URL generation
- **url.lisp** - URL protocol
- **enabled.lisp** - Enable/disable protocol
- **render-changes.lisp** - Change rendering protocol

### Utils (`src/utils/`)

Utility functions organized by domain:
- **db.lisp** - Database helpers (JSON deflate/inflate, keyword conversion)
- **github.lisp** - GitHub API helpers
- **http.lisp** - HTTP request helpers
- **git.lisp** - Git operations
- **lisp.lisp** - Lisp implementation helpers
- **retries.lisp** - Retry logic
- **source.lisp** - Source helpers
- **text.lisp** - Text manipulation
- **time.lisp** - Time formatting
- **timing.lisp** - Timing utilities
- **trace.lisp** - Tracing helpers

### GitHub Integration (`src/github/`)

- **webhook.lisp** - GitHub webhook handling, creates checks on push events

### Other Key Files

- **variables.lisp** - Environment variable accessors
- **logging.lisp** - Logging configuration
- **mail.lisp** - Email sending via Resend
- **routes.lisp** - Route definitions
- **metrics.lisp** - Prometheus metrics
- **analytics.lisp** - Google/Yandex analytics
- **badges.lisp** - Badge SVG generation
- **import.lisp** - Data import utilities

## Packages and files

- Use package-inferred ASDF system style to specify dependencies between files - each file should have its own package.
- Define packages with `uiop:define-package`, explicit `:use #:cl`, and `#:` for package names.
- If there are many symbols used.
- Prefer to use `:local-nicknames` instead of `:use` or `:import-from`.
- For external symbols used very often, add `:import-from` to import them.
- List `:export` explicitly.
- Do not export symbols which should not be used by end-users of the library.
- Do not try to edit files inside `.qlot` directory - they are third-party dependencies.


## Types and declarations

- Use `serapeum/types` `->` for function type declarations when the package is available.
- Prefer `(declare (ignore var))` for unused parameters in methods/lambdas.

## Control flow and iteration

- Use `loop` without keyword style (`while`, `do`, `finally`, `repeat`, `for`) for iteration.
- Use `ecase` / `etypecase` for exhaustive dispatch; signal clear errors with `format` (e.g. `"Unknown TL constructor ID: #x~8,'0X"`).

## Abstractions and macros

- Prefer defining macros for repetitive or boilerplate-heavy patterns (e.g. `define-tl-type`, `define-tl-method`).
- Keep macros small and readable; generate code that matches the project's existing style.
- Use `defgeneric` / `defmethod` for polymorphic behavior; document with `:documentation`.

## Error handling and conditions

- Use `define-condition` with slots and `:report` for project-specific errors (e.g. `rpc-error`, `security-error`).
- Use `error` with a format string and arguments rather than raw strings.

## Style alignment with this project

- Prefer `defun`/`defgeneric`/`defmethod` with type declarations where used in the codebase.
- Use `let`/`let*` for locals; use `shiftf` and `mod` where they clarify numeric logic.
- Prefer explicit slot accessors with `:initarg` keywords; use `defclass` with `:reader`/`:accessor` as in the project.
- Write a function-constructor for each CLOS class to hide `make-instance` and make required arguments positional and optional arguments keyword. Name these functions the same as a class name.

## Interactive development

- Work with running lisp image via `eval_lisp_form` mcp tool, if it is available.
- To not wait for lisp form eternally, wrap it into `(sb-sys:with-deadline (:seconds 10) ..body..)` form.
