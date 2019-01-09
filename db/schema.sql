CREATE TABLE "user" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "email" VARCHAR(255),
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);
CREATE UNIQUE INDEX "unique_user_email" ON "user" ("email");

CREATE TABLE "project" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "source" TEXT NOT NULL,
    "name" TEXT NOT NULL,
    "description" TEXT NOT NULL,
    "params" JSONB NOT NULL,
    "enabled" BOOLEAN NOT NULL,
    "disable_reason" TEXT,
    "disable_description" TEXT,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);
CREATE UNIQUE INDEX "unique_project_name" ON "project" ("name");

CREATE TABLE "moderator" (
    "user_id" BIGINT NOT NULL,
    "project_id" BIGINT NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);

CREATE TABLE "user_shmuzer" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "email" VARCHAR(255),
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);
CREATE UNIQUE INDEX "unique_user_shmuzer_email" ON "user_shmuzer" ("email");

CREATE TABLE "version" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "number" TEXT NOT NULL,
    "built_at" TIMESTAMPTZ,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);
CREATE UNIQUE INDEX "unique_version_number" ON "version" ("number");

CREATE TABLE "check" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "project_id" BIGINT NOT NULL,
    "processed_at" TIMESTAMPTZ,
    "project_has_changes" BOOLEAN NOT NULL,
    "description" TEXT,
    "version_id" BIGINT,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);

CREATE TABLE "check_trigger" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "type" TEXT NOT NULL,
    "check_id" BIGINT NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);

CREATE TABLE "registration_code" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "email" VARCHAR(255) NOT NULL,
    "code" VARCHAR(255) NOT NULL,
    "valid_until" TIMESTAMP NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);

CREATE TABLE IF NOT EXISTS "schema_migrations" (
    "version" VARCHAR(255) PRIMARY KEY,
    "applied_at" TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
INSERT INTO schema_migrations (version) VALUES ('20181230102317');
