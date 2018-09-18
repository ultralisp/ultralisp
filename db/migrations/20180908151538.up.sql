ALTER TABLE "user" ADD COLUMN "email" character varying(255);
CREATE UNIQUE INDEX "unique_user_email" ON "user" ("email");
