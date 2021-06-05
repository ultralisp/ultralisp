alter table dist add column lisp_implementation text;
alter table check2 add column lisp_implementation text;

update dist set lisp_implementation = 'SBCL';
update check2 set lisp_implementation = 'SBCL';
