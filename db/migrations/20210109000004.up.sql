insert into moderator (user_id, project_id, created_at, updated_at)
     select user_id, project_id, created_at, updated_at from project_moderator;
