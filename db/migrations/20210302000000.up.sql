DROP INDEX "unique_project2_name";

UPDATE project2 SET name = 'hackertheory/pngload-duplicate' WHERE id = 424 and name = 'hackertheory/pngload';

CREATE UNIQUE INDEX "unique_project2_name" ON project2 (lower(name));
