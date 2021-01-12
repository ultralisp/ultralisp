insert into source (id, version, latest, deleted, project_id, project_version, type, params, systems_info, release_info, created_at, updated_at)
     select id, 0, true, false, id, 0, 'GITHUB', params, systems_info, release_info, created_at, updated_at from project;
