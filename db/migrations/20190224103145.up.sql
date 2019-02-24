ALTER TABLE "project"
        ADD COLUMN "release_info" jsonb DEFAULT NULL,
        ADD COLUMN "systems_info" jsonb DEFAULT NULL;
