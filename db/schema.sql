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

create type index_status as enum ('ok', 'failed', 'timeout');

CREATE OR REPLACE FUNCTION project_exists(id BIGINT)
RETURNS BOOLEAN AS
$$
BEGIN
IF id IN (SELECT project2.id FROM project2)
THEN RETURN true;
ELSE RETURN false;
END IF;
END
$$ LANGUAGE PLpgSQL;

create table "project_index" (
       "id" bigserial not null primary key,
       "project_id" bigint not null,
       "total_time" bigint not null default 0,
       "last_update_at" timestamptz,
       "next_update_at" timestamptz,
       "num_tries" integer default 0,
       "status" index_status
);

create unique index "unique_project_index_project_id" ON "project_index" ("project_id");

ALTER TABLE "project_index" ADD  CONSTRAINT "project_index_project_id_fkey" CHECK (project_exists(project_id));


CREATE TABLE "project2" (
    "name" TEXT NOT NULL,
    "description" TEXT NOT NULL,
    "id" BIGSERIAL NOT NULL,
    "version" BIGINT NOT NULL,
    "latest" BOOLEAN NOT NULL,
    "deleted" BOOLEAN NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ,
    PRIMARY KEY ("id", "version")
);
CREATE UNIQUE INDEX "unique_project2_name" ON project2 (lower(name));



CREATE TABLE "source" (
    "id" BIGSERIAL NOT NULL,
    "version" BIGINT NOT NULL,
    "latest" BOOLEAN NOT NULL,
    "deleted" BOOLEAN NOT NULL,
    "project_id" BIGINT NOT NULL,
    "project_version" BIGINT NOT NULL,
    "type" TEXT NOT NULL,
    "params" JSONB NOT NULL,
    "systems_info" JSONB,
    "release_info" JSONB,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ,
    PRIMARY KEY ("id", "version"),
    FOREIGN KEY ("project_id", "project_version")
    REFERENCES "project2" ("id", "version")
);


CREATE TYPE dist_state AS ENUM ('pending', 'prepared', 'ready');

CREATE TABLE "dist" (
    "id" BIGSERIAL NOT NULL,
    "version" BIGINT NOT NULL,
    "latest" BOOLEAN NOT NULL,
    "deleted" BOOLEAN NOT NULL,
    "name" TEXT NOT NULL,
    "quicklisp_version" TEXT NOT NULL DEFAULT 'old-build',
    "state" dist_state NOT NULL DEFAULT 'pending',
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ,
    "built_at" TIMESTAMPTZ,
    "lisp_implementation" TEXT,
    PRIMARY KEY (id, version)
);


CREATE TABLE "dist_source" (
    "dist_id" BIGINT NOT NULL,
    "dist_version" BIGINT NOT NULL,
    "source_id" BIGINT NOT NULL,
    "source_version" BIGINT NOT NULL,
    "include_reason" TEXT NOT NULL,
    "enabled" BOOLEAN NOT NULL,
    "disable_reason" JSONB NOT NULL,
    "deleted" BOOLEAN NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ,
    PRIMARY KEY (
        "dist_id", "dist_version",
        "source_id"
    ),
    FOREIGN KEY ("dist_id", "dist_version")
    REFERENCES "dist" ("id", "version"),
    FOREIGN KEY ("source_id", "source_version")
    REFERENCES "source" ("id", "version")
);

CREATE TABLE "dist_moderator" (
    -- dist_id references one or more dist records by id
    "dist_id" BIGINT NOT NULL,
    "user_id" BIGINT NOT NULL REFERENCES "user" ("id") ON DELETE CASCADE,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ,
    PRIMARY KEY ("dist_id", "user_id")
);


CREATE TABLE "project_moderator" (
    -- project_id references one or more dist records by id
    "project_id" BIGINT NOT NULL,
    "user_id" BIGINT NOT NULL REFERENCES "user" ("id") ON DELETE CASCADE,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ,
    PRIMARY KEY ("project_id", "user_id")
);


CREATE TABLE "super_moderator" (
    "user_id" BIGINT NOT NULL REFERENCES "user" ("id") ON DELETE CASCADE,
    "created_at" TIMESTAMPTZ DEFAULT NOW(),
    "updated_at" TIMESTAMPTZ DEFAULT NOW(),
    PRIMARY KEY ("user_id")
);


CREATE TABLE "check2" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "type" TEXT,
    "source_id" BIGINT NOT NULL,
    "source_version" BIGINT NOT NULL,
    "processed_at" TIMESTAMPTZ,
    "processed_in" FLOAT,
    "error" TEXT,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ,
    "lisp_implementation" TEXT,
    FOREIGN KEY ("source_id", "source_version")
    REFERENCES "source" ("id", "version")  ON DELETE CASCADE
);


CREATE TABLE "asdf_system" (
    "dist_id" BIGINT NOT NULL,
    "name" TEXT NOT NULL,
    "source_id" BIGINT NOT NULL,
    "dependencies" JSONB NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ,
    PRIMARY KEY ("dist_id", "name")
);

-- how to reset project2 tables
-- delete from project2; delete from source; delete from dist; delete from dist_source; delete from dist_moderator; delete from project_moderator; delete from check2;

INSERT INTO "dist" (
    version,
    latest,
    deleted,
    name,
    state,
    created_at,
    updated_at,
    lisp_implementation,
    quicklisp_version)
VALUES (
    0,
    True,
    False,
    'ultralisp',
    'ready',
    now(),
    now(),
    'SBCL',
    'initial'
);


CREATE TABLE IF NOT EXISTS "schema_migrations" (
    "version" VARCHAR(255) PRIMARY KEY,
    "applied_at" TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
