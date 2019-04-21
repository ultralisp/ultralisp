ALTER TABLE "user" ADD COLUMN "nickname" text;
UPDATE "user" SET nickname = email;
ALTER TABLE "user" ALTER COLUMN nickname SET NOT NULL;

CREATE UNIQUE INDEX "unique_user_nickname" ON "user" ("nickname");

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
