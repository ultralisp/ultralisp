(defpackage #:ultralisp/models/action
  (:use #:cl)
  (:import-from #:sxql
                #:order-by
                #:limit
                #:where)
  (:import-from #:ultralisp/models/project
                #:project)
  (:import-from #:jonathan)
  (:import-from #:mito
                #:save-dao
                #:select-dao
                #:dao-table-class)
  (:import-from #:ultralisp/models/version
                #:get-or-create-pending-version
                #:make-version
                #:version)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:ultralisp/utils/db
                #:deflate-json
                #:inflate-json
                #:deflate-keyword
                #:inflate-keyword)
  (:export #:get-project-actions
           #:get-version-actions
           #:get-all-actions
           #:base-action
           #:project-added
           #:project-removed
           #:project-updated
           #:make-project-added-action
           #:make-project-removed-action
           #:make-project-updated-action
           #:get-project
           #:get-version
           #:get-params))
(in-package #:ultralisp/models/action)


(defmacro defaction (name)
  (let ((make-func-name (intern
                         (string-upcase
                          (format nil "make-~A-action"
                                  name)))))
    `(progn
       (defclass ,name ()
         ((project :col-type project
                   :initarg :project
                   :reader get-project)
          (version :col-type version
                   :initform nil
                   :initarg :version
                   :documentation "Stores a link to a Ultralisp version triggered by this check."
                   :accessor get-version)
          (type :col-type :text
                :initarg :type
                :reader get-type
                :documentation "Should be one of :project-added :project-disabled :project-enabled"
                :inflate #'inflate-keyword
                :deflate #'deflate-keyword)
          (params :col-type (:jsonb)
                  :initarg :params
                  :initform nil
                  :accessor get-params
                  :deflate #'deflate-json
                  :inflate #'inflate-json))
         (:table-name "action")
         (:metaclass mito:dao-table-class))


       (defun ,make-func-name (project &rest params &key version &allow-other-keys)
         (remf params :version)
         (mito:save-dao (make-instance ',name
                                       :type (make-keyword ',name)
                                       :project project
                                       :version (or version
                                                    (get-or-create-pending-version))
                                       :params params))))))


(defaction base-action)
(defaction project-added)
(defaction project-removed)
(defaction project-updated)


(defun upgrade-types (actions)
  (loop for instance in actions
        for type = (get-type instance)
        for real-type = (case type
                          (:project-added 'project-added)
                          (:project-removed 'project-removed)
                          (:project-updated 'project-updated))
        collect (change-class instance real-type)))


(defun get-all-actions ()
  (upgrade-types (mito:select-dao 'base-action)))


(defun get-project-actions (project)
  (check-type project project)
  (upgrade-types
   (mito:select-dao 'base-action
     (where (:= :project project))
     (order-by (:desc :created-at)))))


(defun get-version-actions (version &key limit)
  (upgrade-types
   (select-dao 'base-action
     (where (:= :version version))
     (when limit
       (limit limit)))))
