CREATE TABLE "asdf_system" (
    "dist_id" BIGINT NOT NULL,
    "name" TEXT NOT NULL,
    "source_id" BIGINT NOT NULL,
    "dependencies" JSONB NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ,
    PRIMARY KEY ("dist_id", "name")
);
