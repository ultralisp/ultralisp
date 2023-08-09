(uiop:define-package #:ultralisp/api/tags
  (:use #:cl)
  (:import-from #:ultralisp/db)
  (:import-from #:ultralisp/models/tag
                #:get-all-tags-with-counters)
  (:import-from #:ultralisp/api/api
                #:api
                #:*default-page-size*)
  (:import-from #:openrpc-server
                #:define-rpc-method))
(in-package #:ultralisp/api/tags)


(defclass tag ()
  ((name :initarg :name)
   (project-count :initarg :project-count)))


(define-rpc-method (api get-all-tags) ()
  (:summary "Returns known tags and a number of projects associated with each tag. Tags are sorted alphabetically.")
  (:result (list-of tag))
  
  (ultralisp/db:with-connection ()
    (loop for row in (get-all-tags-with-counters)
          for name = (getf row :name)
          for count = (getf row :count)
          unless (zerop count)
          collect (make-instance 'tag
                                 :name name
                                 :project-count count))))
