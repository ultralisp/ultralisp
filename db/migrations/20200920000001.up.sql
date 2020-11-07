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
CREATE UNIQUE INDEX "unique_project2_name" ON "project2" ("name");


CREATE SEQUENCE IF NOT EXISTS source_id_seq;

CREATE TABLE "source" (
    "project_id" BIGINT NOT NULL,
    "project_version" BIGINT NOT NULL,
    "id" BIGINT NOT NULL,
    "version" BIGINT NOT NULL,
    "latest" BOOLEAN NOT NULL,
    "deleted" BOOLEAN NOT NULL,
    "type" TEXT NOT NULL,
    "params" JSONB NOT NULL,
    "systems_info" JSONB,
    "release_info" JSONB,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ,
    PRIMARY KEY ("project_id", "project_version", "version"),
    FOREIGN KEY ("project_id", "project_version")
    REFERENCES "project2" ("id", "version")
);


CREATE TYPE dist_state AS ENUM ('pending', 'ready');

CREATE TABLE "dist" (
    "id" BIGSERIAL NOT NULL,
    "version" BIGINT NOT NULL,
    "latest" BOOLEAN NOT NULL,
    "deleted" BOOLEAN NOT NULL,
    "name" TEXT NOT NULL,
    "quicklisp_version" TEXT NOT NULL DEFAULT '',
    "state" dist_state NOT NULL DEFAULT 'pending',
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ,
    "built_at" TIMESTAMPTZ,
    PRIMARY KEY (id, version)
);

CREATE UNIQUE INDEX "unique_dist_name" ON "dist" ("name");

INSERT INTO "dist" (version, latest, deleted, name, state, created_at, updated_at)
     VALUES (0, True, False, 'ultralisp', 'ready', now(), now());


CREATE TABLE "dist_source" (
    "dist_id" BIGINT NOT NULL,
    "dist_version" BIGINT NOT NULL,
    "project_id" BIGINT NOT NULL,
    "project_version" BIGINT NOT NULL,
    "source_version" BIGINT NOT NULL,
    "include_reason" TEXT NOT NULL,
    "enabled" BOOLEAN NOT NULL,
    "disable_reason" JSONB NOT NULL,
    "deleted" BOOLEAN NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ,
    PRIMARY KEY (
        "dist_id", "dist_version",
        "project_id", "project_version", "source_version"
    ),
    FOREIGN KEY ("dist_id", "dist_version")
    REFERENCES "dist" ("id", "version"),
    FOREIGN KEY ("project_id", "project_version", "source_version")
    REFERENCES "source" ("project_id", "project_version", "version")
);

CREATE TABLE "dist_moderator" (
    -- dist_id references one or more dist records by id
    "dist_id" BIGINT NOT NULL,
    "user_id" BIGINT NOT NULL REFERENCES "user" ("id"),
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
