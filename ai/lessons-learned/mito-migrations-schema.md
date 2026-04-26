# Mito Migrations and db/schema.sql

**Project:** Ultralisp

When adding a new Mito migration (a new `.sql` file in `db/migrations/`), you **must also update `db/schema.sql`** with the same changes.

**Why:** Mito uses two different paths to initialize a database:
- **Existing DB:** applies pending migration files from `db/migrations/` in order.
- **Fresh/empty DB (e.g., the `unittest` schema created by `with-test-db` in tests):** executes `db/schema.sql` directly and skips the individual migration files entirely.

If `db/schema.sql` is out of sync, tests that use `with-test-db` will fail because the fresh schema won't have the new columns/tables that the migration files would have added.
