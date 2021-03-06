DROP INDEX "unique_project_index_project_id";

DELETE FROM "project_index";

CREATE OR REPLACE FUNCTION project_exists(id BIGINT)
RETURNS BOOLEAN AS
$$
BEGIN
IF id IN (SELECT project2.id FROM project2)
THEN RETURN true;
ELSE RETURN false;
END IF;
END;
$$ LANGUAGE PLpgSQL;

ALTER TABLE "project_index" DROP CONSTRAINT "project_index_project_id_fkey";

ALTER TABLE "project_index" ADD  CONSTRAINT "project_index_project_id_fkey" CHECK (project_exists(project_id));
