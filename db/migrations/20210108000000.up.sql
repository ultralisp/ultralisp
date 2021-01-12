-- This statement breaks Mito migrations and was moved into a separate file.
-- Related issue is: https://github.com/fukamachi/mito/issues/74

INSERT INTO "dist" (version, latest, deleted, name, state, created_at, updated_at, built_at, quicklisp_version)
     VALUES (0, True, False, 'ultralisp', 'ready', now(), now(),
             (select built_at from version where type = 'READY' order by built_at desc limit 1),
             (select number from version where type = 'READY' order by built_at desc limit 1));
