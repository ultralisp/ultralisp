# Search Architecture

## Overview

Ultralisp search indexes three entity types in ElasticSearch and provides a tabbed UI for browsing results.

---

## ElasticSearch Indices

All indices live on ES 7.x at `http://<elastic-host>:9200/`. Host is configured via `ELASTIC_SEARCH_HOST` env var (default: `"elastic"`).

### `symbols` index

Indexed since initial implementation. One document per exported symbol per package.

| Field | Type | Description |
|-------|------|-------------|
| `symbol` | text | Symbol name (lowercase) |
| `package` | text | Package where symbol was found during indexing |
| `original-package` | text | Symbol's actual home package |
| `type` | keyword | `function`, `macro`, `generic-function`, `type`, `variable`, etc. |
| `documentation` | text | Docstring (cleaned) |
| `arguments` | text | Lambda list string |
| `methods` | nested | For generic functions: `[specializers, doc]` |
| `slots` | nested | For class types: slot metadata |
| `system` | keyword | ASDF system name |
| `system-path` | keyword | Path to system definition |
| `project` | keyword | Project name (e.g. `40ants/cl-websocket`) |
| `source` | keyword | Source URI string |

Document ID: `src:<source-uri>:<package>:<symbol>` or `prj:<project-name>:<package>:<symbol>`

Search fields: `documentation`, `symbol`, `package`

### `projects` index

One document per project.

| Field | Type | Description |
|-------|------|-------------|
| `name` | text | Project name (e.g. `40ants/cl-websocket`) |
| `description` | text | Project description |
| `tags` | keyword[] | GitHub topics |

Document ID: `project:<name>`

### `systems` index

One document per ASDF system per project.

| Field | Type | Description |
|-------|------|-------------|
| `name` | text | ASDF system name |
| `description` | text | Short description from .asd |
| `long-description` | text | Full description from .asd |
| `license` | keyword | License name |
| `author` | text | Author string |
| `dependencies` | keyword[] | ASDF system names |
| `project-name` | keyword | Parent project name |

Document ID: `system:<project-name>:<system-name>`

---

## Indexing Flow

```
Cron (every 1 min)
  └── index-projects (src/search.lisp)
       └── get-projects-to-index (PostgreSQL)
            └── submit-task → Gearman queue
                 └── Worker: index-project (src/search.lisp)
                      ├── delete-project-documents (all collections)
                      ├── index-project-doc (→ projects index)
                      ├── For each source:
                      │    └── index-source
                      │         ├── download project code
                      │         ├── discover packages (packages-extractor-api)
                      │         ├── quickload each system
                      │         └── index-symbols (→ symbols index)
                      ├── index-system-docs (→ systems index, _bulk API)
                      └── update-index-status (PostgreSQL: project_index table)
```

Index status tracked in PostgreSQL `project_index` table:
- `status`: `:ok`, `:failed`, `:timeout`
- `next_update_at`: scheduled next run (exponential backoff on failure)
- `total_time`, `num_tries`

---

## Search Flow

### Backend (`src/search2.lisp`, package `ultralisp/search2`)

```
search-all(query, per-type-limit=5)
  ├── search-collection("projects", query, limit=5)
  ├── search-collection("systems", query, limit=5)
  └── search-collection("symbols", query, limit=5)
  → Returns: ((:projects :total N :results (...)) (:systems ...) (:symbols ...))

search-by-type(query, type, from, limit)
  └── search-collection(type, query, from, limit)
  → Returns: (values results total next-closure)
```

Uses ES `query_string` query, same as current `search-objects`.

### API v2 (`src/api/v2/search.lisp`, package `ultralisp/api/v2/search`)

OpenRPC methods on the `api` server:

| Method | Params | Returns |
|--------|--------|---------|
| `search-all` | `term` | List of `search-result-group` (one per entity type) |
| `search-by-type` | `term`, `type`, `page-key`, `limit` | Paginated `search-result-item` list |

Existing `search-symbols` in `ultralisp/api/search` is unchanged for backward compatibility.

---

## UI Widget Hierarchy

```
search-page (src/widgets/search/page.lisp)
├── search form (HTML <form>)
└── tabs (reblocks-ui2/containers/tabs:tabs)
    ├── "All" tab → all-tab widget
    │   ├── results-section (projects)
    │   │   ├── project-card × ≤5
    │   │   └── "All N results" button (if total > 5)
    │   ├── results-section (systems)
    │   │   ├── system-card × ≤5
    │   │   └── "All N results" button
    │   └── results-section (symbols)
    │       ├── symbol-card × ≤5
    │       └── "All N results" button
    ├── "Projects" tab → projects-tab (only if results exist)
    │   ├── project-card × N
    │   └── "Load more" pagination
    ├── "Systems" tab → systems-tab (only if results exist)
    │   ├── system-card × N
    │   └── "Load more" pagination
    └── "Symbols" tab → symbols-tab (only if results exist)
        ├── symbol-card × N
        └── "Load more" pagination
```

Tab visibility:
- **All**: always shown
- **Projects / Systems / Symbols**: only shown when `total > 0` for that type

URL: `/search/?query=<term>&tab=projects|systems|symbols`

### Widget Files

| File | Package | Widget |
|------|---------|--------|
| `src/widgets/search/page.lisp` | `ultralisp/widgets/search/page` | `search-page` — top-level page |
| `src/widgets/search/all-tab.lisp` | `ultralisp/widgets/search/all-tab` | Content for "All" tab |
| `src/widgets/search/project-card.lisp` | `ultralisp/widgets/search/project-card` | Single project result card |
| `src/widgets/search/system-card.lisp` | `ultralisp/widgets/search/system-card` | Single system result card |
| `src/widgets/search/symbol-card.lisp` | `ultralisp/widgets/search/symbol-card` | Single symbol result card |
| `src/widgets/search/projects-tab.lisp` | `ultralisp/widgets/search/projects-tab` | Full projects results + pagination |
| `src/widgets/search/systems-tab.lisp` | `ultralisp/widgets/search/systems-tab` | Full systems results + pagination |
| `src/widgets/search/symbols-tab.lisp` | `ultralisp/widgets/search/symbols-tab` | Full symbols results + pagination |
| `src/widgets/search/results-section.lisp` | `ultralisp/widgets/search/results-section` | Reusable section (header + cards + "All" button) |

---

## Key Files

| File | Purpose |
|------|---------|
| `src/search.lisp` | ES indexing (symbols, projects, systems), low-level `search-collection`, `index-project` |
| `src/search2.lisp` | High-level multi-entity search (`search-all`, `search-by-type`) |
| `src/api/search.lisp` | Legacy API — `search-symbols` (v1, unchanged) |
| `src/api/v2/search.lisp` | New API — `search-all`, `search-by-type` |
| `src/widgets/search/` | Search UI widgets |
| `src/widgets/frame.lisp` | Global header with search input |
| `src/app.lisp` | Route `/search/` |
| `src/cron.lisp` | Cron job `index-projects` |
| `src/models/index.lisp` | PostgreSQL `project_index` status tracking |
| `src/models/tag.lisp` | Tags for projects |
| `src/models/asdf-system.lisp` | ASDF system records in PostgreSQL |
| `src/models/system-info.lisp` | In-memory system metadata from .asd files |
