delete from moderator where not exists (select 1 from "user" where moderator.user_id = "user".id);

insert into project_moderator (user_id, project_id, created_at, updated_at)
     select user_id, project_id, created_at, updated_at from moderator;
