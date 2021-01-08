insert into project2 (id, version, name, description, latest, deleted, created_at, updated_at)
     select id, 0, name, description, true, false, created_at, updated_at from project;





