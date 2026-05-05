# Multi-entity Search

## Context

Currently, search only indexes **symbols** (functions, macros, classes, variables) in ElasticSearch index `symbols`. Projects and ASDF systems live only in PostgreSQL and are not searchable.

Goal: add Projects and Systems to ES, show search results in a tabbed UI grouped by entity type.

---

## New ES Indices

### `projects` index

| Field | Type | Description |
|-------|------|-------------|
| `name` | text | Project name, e.g. `"40ants/cl-websocket"` |
| `description` | text | Project description from GitHub |
| `tags` | array of keyword | GitHub topics (tags) |

Document ID: `"project:<name>"`

### `systems` index

| Field | Type | Description |
|-------|------|-------------|
| `name` | text | ASDF system name, e.g. `"cl-websocket"` |
| `description` | text | Short description from .asd file |
| `long-description` | text | Full description from .asd file |
| `license` | keyword | License name |
| `author` | text | Author string |
| `dependencies` | array of keyword | List of ASDF system names |
| `project-name` | keyword | Parent project name |

Document ID: `"system:<project-name>:<system-name>"`

### `symbols` index (existing, unchanged)

Already indexed with fields: `symbol`, `package`, `original-package`, `type`, `documentation`, `arguments`, `methods`, `slots`, `system`, `system-path`, `project`, `source`.

---

## Step 1. New ES Indices — Backend

### File: `src/search.lisp` (modifications)

Add functions:

- `ensure-indices` — creates `projects` and `systems` indices with explicit mappings if they don't exist. Called at server/worker startup.
- `index-project-doc (project-name description tags)` — indexes a single project document into `projects`.
- `index-system-docs (project-name systems-info)` — batch-indexes all systems from `systems-info` into `systems` using ES `_bulk` API.
- `delete-from-index-by-collection (collection project-name)` — deletes all documents from a given collection matching `project:"<name>"`.
- `search-collection (collection term &key fields from limit)` — generic search across a single ES collection.

Modify existing functions:

- `index-project` — after `delete-project-documents`, add calls to index project doc and system docs before indexing symbols.
- `delete-project-documents` — also delete from `projects` and `systems` collections.
- `index-source` — return system info from each source so that `index-project` can index system documents.

### File: `src/search2.lisp` (new, package `ultralisp/search2`)

New search API with multi-entity support:

```lisp
(defun search-all (query &key (per-type-limit 5))
  ;; Returns a plist:
  ;; (:projects ((:total N :results (...)))
  ;;  :systems  ((:total N :results (...)))
  ;;  :symbols  ((:total N :results (...))))

(defun search-by-type (query type &key (from 0) (limit 20))
  ;; TYPE is one of :projects, :systems, :symbols
  ;; Returns (values results total next-closure)
```

`search-all` calls `search-collection` for each of the three indices in sequence (or using `/_msearch` for efficiency).

`search-by-type` wraps `search-collection` for a single type with pagination.

---

## Step 2. Integrate into `index-project`

### Flow in `index-project` (src/search.lisp)

Current flow:
1. `delete-project-documents` (symbols only)
2. For each source: `index-source` → downloads code, loads systems, indexes symbols
3. Update index status

New flow:
1. `delete-project-documents` (all three collections: projects, systems, symbols)
2. Index project doc: `(index-project-doc name description tags)`
   - `name` from `(project-name project)`
   - `description` from `(project-description project)`
   - `tags` from `(ultralisp/models/tag:get-project-tags project)`
3. For each source: `index-source` → same as before, but also collect `systems-info`
4. After all sources processed: `(index-system-docs project-name all-systems-info)`
5. Index symbols (same as before, happens inside `index-source`)
6. Update index status

---

## Step 3. API v2

### File: `src/api/v2/search.lisp` (new, package `ultralisp/api/v2/search`)

Separate from existing `ultralisp/api/search`. New package, new RPC methods on the same `api` server.

```lisp
(define-rpc-method (api search-all) (term)
  (:summary "Search across projects, systems, and symbols.")
  (:param term string "A search term.")
  (:result (list-of search-result-group))
  ;; Returns a list of groups, each with :type, :total, :results
  ...)

(define-rpc-method (api search-by-type) (term type &key page-key limit)
  (:summary "Search entities of a specific type.")
  (:param term string "A search term.")
  (:param type string "One of: projects, systems, symbols.")
  (:param page-key integer "Offset for pagination.")
  (:param limit integer "Max items per page.")
  (:result (paginated-list-of search-result-item))
  ...)
```

Result classes:

- `search-result-group` — `type` (string), `total` (integer), `results` (list of `search-result-item`)
- `search-result-item` — polymorphic, with slots depending on entity type:
  - Project: `name`, `description`, `tags`
  - System: `name`, `description`, `project-name`, `dependencies`
  - Symbol: `type`, `symbol`, `package`, `system`, `documentation`

The existing `search-symbols` method in `ultralisp/api/search` remains unchanged for backward compatibility.

---

## Step 4. UI Widgets

### New directory: `src/widgets/search/`

All search-related widgets split into separate files under `src/widgets/search/`. Each file = its own package (package-inferred-system style).

### `src/widgets/search/page.lisp` — `ultralisp/widgets/search/page`

Main search page widget. Contains:

- **Search form** at top (duplicated from header, with current query pre-filled)
- **Tabs widget** from `reblocks-ui2/containers/tabs:tabs`

Logic:
1. On page load, read `query` and optional `tab` GET parameters
2. Call `ultralisp/search2:search-all` with the query
3. Build tab list dynamically:
   - "All" tab — always present
   - "Projects" tab — only if `total-projects > 0`
   - "Systems" tab — only if `total-systems > 0`
   - "Symbols" tab — only if `total-symbols > 0`
4. Determine initial tab from `tab` parameter or default to "All"
5. Create `tabs` widget with titles and content widgets

```lisp
(defwidget search-page (ui-widget)
  ((query :initform "" :type string :reader get-query)
   (results :initform nil :type list :accessor search-results)
   (error :initform nil :accessor get-error)))
```

### `src/widgets/search/all-tab.lisp` — `ultralisp/widgets/search/all-tab`

The "All" tab content. Renders vertical sections for each entity type (up to 5 each):

1. Projects section (if any) — shows up to 5 `project-card` widgets
2. Systems section (if any) — shows up to 5 `system-card` widgets
3. Symbols section (if any) — shows up to 5 `symbol-card` widgets

Each section has:
- Header with entity type name and count
- List of card widgets
- "All N results" button (if total > 5) that switches to the corresponding tab

### `src/widgets/search/project-card.lisp` — `ultralisp/widgets/search/project-card`

Renders a single project result:

```
┌─────────────────────────────────────────┐
│ 40ants/cl-websocket          [project]  │
│ WebSocket server for Reblocks apps       │
│ tags: web, websocket, reblocks           │
└─────────────────────────────────────────┘
```

- Project name (bold, link to `/projects/<name>`)
- Description
- Tags (as small badges)

### `src/widgets/search/system-card.lisp` — `ultralisp/widgets/search/system-card`

Renders a single ASDF system result:

```
┌─────────────────────────────────────────┐
│ cl-websocket                    [system] │
│ A WebSocket server for CL apps           │
│ from 40ants/cl-websocket                 │
└─────────────────────────────────────────┘
```

- System name (bold)
- Description
- Project name (link to `/projects/<name>`)

### `src/widgets/search/symbol-card.lisp` — `ultralisp/widgets/search/symbol-card`

Extracted from current `render-item` in `src/widgets/search.lisp`. Renders:

- `PACKAGE:SYMBOL` (bold, uppercased)
- Arguments (gray)
- Type (gray)
- Documentation (Markdown → HTML)
- Project link, system name, original package

### `src/widgets/search/projects-tab.lisp` — `ultralisp/widgets/search/projects-tab`

Full projects results tab with pagination. Contains:
- List of `project-card` widgets
- "Load more" link for pagination (calls `search-by-type :projects`)

### `src/widgets/search/systems-tab.lisp` — `ultralisp/widgets/search/systems-tab`

Full systems results tab with pagination. Same structure as projects-tab.

### `src/widgets/search/symbols-tab.lisp` — `ultralisp/widgets/search/symbols-tab`

Full symbols results tab with pagination. Refactored from current `search-results` widget behavior. Contains:
- List of `symbol-card` widgets
- "Load more" link (same logic as current `fetch-next-results`)

### `src/widgets/search/results-section.lisp` — `ultralisp/widgets/search/results-section`

Reusable section component used in `all-tab`. Renders:
- Section header (entity type + count)
- Up to N card widgets
- "All N results" button if total exceeds displayed count

---

## Step 5. Route

### File: `src/app.lisp` (modification)

Change import and route:

```lisp
;; Before:
(:import-from #:ultralisp/widgets/search #:make-search-page)
...
(page ("/search/" :name "search") (make-search-page))

;; After:
(:import-from #:ultralisp/widgets/search/page #:make-search-page)
...
(page ("/search/" :name "search") (make-search-page))
```

Support `?tab=projects|systems|symbols` parameter — `make-search-page` reads it and passes to `search-page` widget for initial tab selection.

---

## Step 6. ASDF System Registration

### File: `ultralisp.asd` (modification)

Add new component paths:

```lisp
(:module "src"
  :components
  (...
   ;; New search backend
   (:file "search2")

   ;; New API v2
   (:module "api"
     :components
     (...
      (:module "v2"
        :components
        ((:file "search")))))

   ;; New search widgets
   (:module "widgets"
     :components
     (...
      (:module "search"
        :components
        ((:file "page")
         (:file "all-tab")
         (:file "project-card")
         (:file "system-card")
         (:file "symbol-card")
         (:file "projects-tab")
         (:file "systems-tab")
         (:file "symbols-tab")
         (:file "results-section"))))))
```

---

## Step 7. Documentation

### File: `ai/search-architecture.md` (new, keep up to date)

Architecture documentation covering:
- ES indices and their mappings
- Indexing flow (cron → worker → ES)
- Search flow (UI → search2 → ES → render)
- Widget hierarchy
- API v2 endpoints

---

## Files Summary

| Action | File | Package |
|--------|------|---------|
| Modify | `src/search.lisp` | `ultralisp/search` |
| New | `src/search2.lisp` | `ultralisp/search2` |
| New | `src/api/v2/search.lisp` | `ultralisp/api/v2/search` |
| New | `src/widgets/search/page.lisp` | `ultralisp/widgets/search/page` |
| New | `src/widgets/search/all-tab.lisp` | `ultralisp/widgets/search/all-tab` |
| New | `src/widgets/search/project-card.lisp` | `ultralisp/widgets/search/project-card` |
| New | `src/widgets/search/system-card.lisp` | `ultralisp/widgets/search/system-card` |
| New | `src/widgets/search/symbol-card.lisp` | `ultralisp/widgets/search/symbol-card` |
| New | `src/widgets/search/projects-tab.lisp` | `ultralisp/widgets/search/projects-tab` |
| New | `src/widgets/search/systems-tab.lisp` | `ultralisp/widgets/search/systems-tab` |
| New | `src/widgets/search/symbols-tab.lisp` | `ultralisp/widgets/search/symbols-tab` |
| New | `src/widgets/search/results-section.lisp` | `ultralisp/widgets/search/results-section` |
| Modify | `src/app.lisp` | `ultralisp/app` |
| Modify | `ultralisp.asd` | — |
| New | `ai/search-architecture.md` | — |

Old `src/widgets/search.lisp` will be kept temporarily for reference, removed after migration is complete.

---

## Implementation Order

1. **Backend: ES indices + indexing** (Steps 1-2) — `src/search.lisp` modifications
2. **Backend: search2** (Step 1 cont.) — `src/search2.lisp`
3. **API v2** (Step 3) — `src/api/v2/search.lisp`
4. **UI widgets** (Step 4) — `src/widgets/search/` directory
5. **Route + ASDF** (Steps 5-6) — `src/app.lisp`, `ultralisp.asd`
6. **Documentation** (Step 7) — `ai/search-architecture.md`

Steps 1-2 must be done first (backend). Steps 3 and 4 can be done in parallel. Steps 5-6 wire everything together. Step 7 is ongoing.
