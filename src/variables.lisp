(defpackage #:ultralisp/variables
  (:use #:cl))
(in-package ultralisp/variables)


(defmacro def-env-var (getter var-name &optional default)
  `(progn
     (defun ,getter ()
       "If `value' is not given, then tries to extract it from env variables or fall back to default."
       (or (uiop:getenv ,var-name)
           ,default))
     (export ',getter)))

(def-env-var get-projects-dir
  "PROJECTS_DIR"
  "./build/sources/")

(def-env-var get-dist-dir
  "DIST_DIR"
  "./build/dist/")

(def-env-var get-base-url
  "BASE_URL"
  "http://dist.ultralisp.org/")

(def-env-var get-dist-name
  "DIST_NAME"
  "ultralisp")

(def-env-var get-yandex-counter-id
  "YANDEX_COUNTER_ID")

(def-env-var get-google-counter-id
  "GOOGLE_COUNTER_ID")

(def-env-var get-postgres-host
  "POSTGRES_HOST"
  "localhost")

(def-env-var get-postgres-dbname
  "POSTGRES_DBNAME"
  "ultralisp")

(def-env-var get-postgres-user
  "POSTGRES_USER"
  "ultralisp")

(def-env-var get-postgres-ro-user
  "POSTGRES_RO_USER"
  "ultralisp_ro")

(def-env-var get-postgres-pass
  "POSTGRES_PASS"
  "ultralisp")

(def-env-var get-postgres-ro-pass
  "POSTGRES_RO_PASS"
  "ultralisp_ro")

(def-env-var get-lfarm-workers
  "LFARM_WORKERS")

(def-env-var get-github-client-id
  "GITHUB_CLIENT_ID")

(def-env-var get-github-secret
  "GITHUB_SECRET")
