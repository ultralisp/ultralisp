(defpackage #:ultralisp/github/core
  (:use #:cl)
  (:import-from #:dexador)
  (:import-from #:jonathan
                #:to-json)
  (:export
   #:*client-id*
   #:*secret*
   #:get-oauth-token-by
   #:make-authentication-url))
(in-package ultralisp/github/core)


(defvar *client-id* nil
  "OAuth client id")


(defvar *secret* nil
  "OAuth secret")


(defun make-authentication-url ()
  (format nil
          "https://github.com/login/oauth/authorize?client_id=~A&scope=write:repo_hook,repo"
          *client-id*))


(defun get-oauth-token-by (code)
  (let* ((response (dex:post "https://github.com/login/oauth/access_token"
                             :content (to-json (list :|code| code
                                                     :|client_id| *client-id*
                                                     :|client_secret| *secret*))
                             :headers '(("Accept" . "application/json")
                                        ("Content-Type" . "application/json"))))
         (data (jonathan:parse response)))
    (values (getf data :|access_token|)
            data)))
