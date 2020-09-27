(defpackage #:ultralisp/models/dist-source
  (:use #:cl)
  (:import-from #:jonathan)
  (:import-from #:alexandria
                #:make-keyword)
  (:export
   #:dist-source
   #:dist-id
   #:dist-version
   #:project-id
   #:project-version
   #:source-version
   #:include-reason
   #:enabled-p
   #:disable-reason
   #:deleted-p))
(in-package ultralisp/models/dist-source)


(defclass dist-source ()
  ((dist-id :col-type :bigint
            :initarg :dist-id
            :reader dist-id)
   (dist-version :col-type :bigint
                 :initarg :dist-version
                 :reader dist-version)
   (project-id :col-type :bigint
               :initarg :project-id
               :reader project-id)
   (project-version :col-type :bigint
                    :initarg :project-version
                    :reader project-version)
   (source-version :col-type :bigint
                   :initarg :source-version
                   :reader source-version)
   (include-reason :col-type :text
                   :initarg :include-reason
                   :reader include-reason
                   :deflate #'symbol-name
                   :inflate (lambda (text)
                              (make-keyword (string-upcase text))))
   (enabled :col-type :boolean
            :initarg :enabled
            :initform t
            :reader enabled-p)
   (disable-reason :col-type :jsonb
                   :initarg :disable-reason
                   :initform nil
                   :reader disable-reason
                   :deflate #'jonathan:to-json
                   :inflate (lambda (text)
                              (jonathan:parse
                               ;; Jonathan for some reason is unable to work with
                               ;; `base-string' type, returned by database
                               (coerce text 'simple-base-string))))
   (deleted :col-type :boolean
            :initarg :deleted
            :initform nil
            :reader deleted-p))
  (:primary-key
   dist-id dist-version
   project-id project-version source-version)
  (:metaclass mito:dao-table-class))



(defun %project-dist-source (project-id project-version source-version)
  (mito:retrieve-dao 'dist-source
                     :project-id project-id
                     :project-version project-version
                     :source-version source-version))
