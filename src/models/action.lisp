(defpackage #:ultralisp/models/action
  (:use #:cl)
  (:import-from #:ultralisp/models/project
                #:project)
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
  (:export #:get-project-actions
           #:get-version-actions
           #:get-all-actions
           #:base-action
           #:project-added
           #:project-removed
           #:get-project
           #:get-version))
(in-package ultralisp/models/action)


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
                :inflate (lambda (text)
                           (make-keyword (string-upcase text)))
                :deflate #'symbol-name))
         (:table-name "action")
         (:metaclass mito:dao-table-class))


       (defun ,make-func-name (project &key version)
         (mito:save-dao (make-instance ',name
                                       :type :project-added
                                       :project project
                                       :version (get-or-create-pending-version)))))))


(defaction base-action)
(defaction project-added)
(defaction project-removed)


(defun upgrade-types (actions)
  (loop for instance in actions
        for type = (get-type instance)
        for real-type = (case type
                          (:project-added 'project-added)
                          (:project-removed 'project-removed))
        collect (change-class instance real-type)))


(defun get-all-actions ()
  (upgrade-types (mito:select-dao 'base-action)))


(defun get-project-actions (project)
  (upgrade-types
   (mito:retrieve-dao 'base-action
                      :project project)))


(defun get-version-actions (version)
  (upgrade-types
   (mito:retrieve-dao 'base-action
                      :version version)))
