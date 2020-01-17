create type index_status as enum ('ok', 'failed');

create table "project_index" (
       "id" bigserial not null primary key,
       "project_id" bigint not null references project (id),
       "total_time" bigint not null default 0,
       "last_update_at" timestamptz,
       "next_update_at" timestamptz,
       "status" index_status
);

create unique index "unique_project_index_project_id" ON "project_index" ("project_id");
