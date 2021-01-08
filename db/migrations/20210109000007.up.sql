-- We need these statements to fix nicknames created from 2019-01-26 to 2019-03-30

UPDATE "user" SET "nickname" = "social_profile"."service_user_id"
  FROM "social_profile"
 WHERE "user"."id" = "social_profile"."user_id"
   AND "user"."nickname" LIKE '%@%';


DELETE FROM "user" as u
      WHERE NOT EXISTS (SELECT 1 FROM "social_profile"
                         WHERE u.id = "social_profile"."user_id") AND "nickname" LIKE '%@%';
