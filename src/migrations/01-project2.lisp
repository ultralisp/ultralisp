(defpackage #:ultralisp/migrations/01-project
  (:use #:cl)
  (:import-from #:ultralisp/models/project
                #:project2)
  (:import-from #:ultralisp/models/versioned
                #:object-version)
  (:import-from #:mito
                #:object-id)
  (:import-from #:ultralisp/models/dist
                #:find-dist)
  (:import-from #:ultralisp/db
                #:with-transaction)
  (:import-from #:ultralisp/models/source
                #:source)
  (:import-from #:ultralisp/models/dist-source
                #:dist-source)
  (:import-from #:ultralisp/models/moderator
                #:get-moderators)
  (:export #:create-new-projects))
(in-package #:ultralisp/migrations/01-project)


(defun create-new-project (old-project)
  (with-transaction
    (let* ((new-project
             (mito:create-dao 'project2
                              :name (ultralisp/models/project:get-name old-project)
                              :description (ultralisp/models/project:get-description old-project)
                              :latest t))
           (source
             (mito:create-dao 'source
                              :project-id (object-id new-project)
                              :project-version (object-version new-project)
                              :latest t
                              :type (ultralisp/models/project:get-source old-project)
                              :params (ultralisp/models/project:get-params old-project)
                              :systems-info (ultralisp/models/project:get-systems-info old-project)
                              :release-info (ultralisp/models/project:get-release-info old-project)))
           (dist (find-dist "common"))
           (dist-source
             (mito:create-dao 'dist-source
                              :dist-id (object-id dist)
                              :dist-version (object-version dist)
                              :source-id (object-id source)
                              :source-version (object-version source)
                              :include-reason :direct))
           (moderators
             (loop for old-moderator in (get-moderators old-project)
                   collect
                      (mito:create-dao 'ultralisp/models/project-moderator:project-moderator
                                       :project-id (object-id new-project)
                                       :user-id (object-id old-moderator)))))
      (values new-project
              source
              dist-source
              dist
              moderators))))


(defun create-new-projects ()
  (with-transaction
    (mapcar #'create-new-project
            (ultralisp/models/project:get-all-projects))))
