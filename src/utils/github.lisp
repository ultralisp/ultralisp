(defpackage #:ultralisp/utils/github
  (:use #:cl)
  (:import-from #:cl-ppcre)
  (:import-from #:rutils
                #:fmt)
  (:import-from #:ultralisp/utils/http
                #:get-json)
  (:export
   #:get-branches
   #:extract-github-name
   #:*token*))
(in-package ultralisp/utils/github)


(defvar *token* nil
  "If given, will be used to make authenticated requests.")


(defun extract-github-name (url)
  "It should extract \"cbaggers/livesupport\" from urls like:

   http://github.com/cbaggers/livesupport
   https://github.com/cbaggers/livesupport
   https://github.com/cbaggers/livesupport/
   https://github.com/cbaggers/livesupport.git
   https://github.com/cbaggers/livesupport/issues"
  
  (cl-ppcre:register-groups-bind (name)
      ("https?://github.com/(.*?/.*?)($|/|\\.git)" url)
    name))


(defun get-branches (url)
  "Returns two values: a list of branches and default branch.

   If repository not found, returns NIL."
  (handler-case
      (let ((name (extract-github-name url))
            (headers (when *token*
                       (list (cons "Authorization"
                                   (fmt "token ~A" *token*))))))
        (when name
          (values
           (loop for item in (get-json (fmt "https://api.github.com/repos/~A/branches" name)
                                       :headers headers)
                 collect (getf item :|name|) into results
                 finally (return (sort results #'string<)))
           (getf (get-json (fmt "https://api.github.com/repos/~A" name)
                           :headers headers)
                 :|default_branch|))))
    (dexador:http-request-not-found ()
      nil)))
