ALTER TABLE "project" DROP COLUMN "disable_description", DROP COLUMN "disable_reason";

ALTER TABLE "version" ADD COLUMN "type" text;
ALTER TABLE "version" ALTER COLUMN "number" TYPE text, ALTER COLUMN "number" DROP NOT NULL;

CREATE TABLE "action" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "project_id" BIGINT NOT NULL,
    "version_id" BIGINT NOT NULL,
    "type" TEXT NOT NULL,
    "params" JSONB NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);

ALTER TABLE "check" DROP COLUMN "description", DROP COLUMN "project_has_changes", DROP COLUMN "version_id";
ALTER TABLE "check" ADD COLUMN "error" text, ADD COLUMN "type" text;

CREATE TABLE "project_version" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "version_id" BIGINT NOT NULL,
    "source" TEXT NOT NULL,
    "name" TEXT NOT NULL,
    "description" TEXT NOT NULL,
    "params" JSONB NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);
