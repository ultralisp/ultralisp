select setval('source_id_seq', (select max(id) from source));
