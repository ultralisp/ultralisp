CREATE TABLE "project_tag" (
    "project_id" BIGINT NOT NULL,
    "tag_id" BIGINT NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ,
    PRIMARY KEY ("project_id", "tag_id")
);
CREATE TABLE "tag" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "name" TEXT NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);
CREATE UNIQUE INDEX "unique_tag_name" ON "tag" ("name");
