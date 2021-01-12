insert into dist_source (dist_id, dist_version, source_id, source_version, include_reason, enabled, disable_reason, deleted, created_at, updated_at)
     select (select id from dist where name = 'ultralisp'), 0, id, 0, 'DIRECT', enabled,
            CASE WHEN enabled THEN '[]'::jsonb
                 ELSE '{"TYPE": "JUST-ADDED", "COMMENT": "This source waits for the check."}'::jsonb
            END,
            false, created_at, updated_at
       from project;
