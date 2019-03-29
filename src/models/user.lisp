(defpackage #:ultralisp/models/user
  (:use #:cl)
  (:import-from #:mito)
  (:import-from #:mito-email-auth/weblocks
                #:user-with-email)
  (:export #:user
           #:get-all-users))
(in-package ultralisp/models/user)


