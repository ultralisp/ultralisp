update "source"
   set "type" = lower("type");

update "check2"
   set "type" = lower("type");

update "dist_source"
   set "include_reason" = lower("include_reason");
