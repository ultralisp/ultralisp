CREATE TABLE "user" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "nickname" TEXT NOT NULL,
    "email" VARCHAR(255),
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);
CREATE UNIQUE INDEX "unique_user_email" ON "user" ("email");
CREATE UNIQUE INDEX "unique_user_nickname" ON "user" ("nickname");

CREATE TABLE "project" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "source" TEXT NOT NULL,
    "name" TEXT NOT NULL,
    "description" TEXT NOT NULL,
    "params" JSONB NOT NULL,
    "enabled" BOOLEAN NOT NULL,
    "systems_info" JSONB,
    "release_info" JSONB,
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

CREATE TABLE "version" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "number" TEXT,
    "type" TEXT,
    "built_at" TIMESTAMPTZ,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);
CREATE UNIQUE INDEX "unique_version_number" ON "version" ("number");

CREATE TABLE "action" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "project_id" BIGINT NOT NULL,
    "version_id" BIGINT NOT NULL,
    "type" TEXT NOT NULL,
    "params" JSONB NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);

CREATE TABLE "check" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "type" TEXT,
    "project_id" BIGINT NOT NULL,
    "processed_at" TIMESTAMPTZ,
    "processed_in" FLOAT,
    "error" TEXT,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);

CREATE TABLE "project_version" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "version_id" BIGINT NOT NULL,
    "source" TEXT NOT NULL,
    "name" TEXT NOT NULL,
    "description" TEXT NOT NULL,
    "params" JSONB NOT NULL,
    "systems_info" JSONB,
    "release_info" JSONB,
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

CREATE TABLE "social_profile" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "user_id" BIGINT NOT NULL,
    "service" TEXT NOT NULL,
    "service_user_id" TEXT NOT NULL,
    "metadata" JSONB NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);
CREATE UNIQUE INDEX "unique_social_profile_user_id_service_service_user_id" ON "social_profile" ("user_id", "service", "service_user_id");

CREATE TABLE IF NOT EXISTS "schema_migrations" (
    "version" VARCHAR(255) PRIMARY KEY,
    "applied_at" TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

create type index_status as enum ('ok', 'failed');

create table "project_index" (
       "id" bigserial not null primary key,
       "project_id" bigint not null references project (id),
       "total_time" bigint not null default 0,
       "last_update_at" timestamptz,
       "next_update_at" timestamptz,
       "status" index_status
);

create unique index "unique_project_index_project_id" ON "project_index" ("project_id");

-- INSERT INTO schema_migrations (version) VALUES ('20200102180000');
