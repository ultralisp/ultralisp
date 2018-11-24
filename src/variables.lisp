(defpackage #:ultralisp/variables
  (:use #:cl))
(in-package ultralisp/variables)


(defmacro def-env-var (getter var-name &optional default)
  `(progn
     (defun ,getter ()
       (or (uiop:getenv ,var-name)
           ,default))
     (export ',getter)))

(def-env-var get-projects-dir
  "PROJECTS_DIR"
  "~/tmp/ultralisp/build/sources/")

(def-env-var get-projects-dir
  "PROJECTS_DIR"
  "~/tmp/ultralisp/build/sources/")

(def-env-var get-dist-dir
  "DIST_DIR"
  "~/tmp/ultralisp/build/dist/")

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
