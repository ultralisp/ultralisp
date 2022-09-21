(defpackage #:ultralisp/models/asdf-system
  (:use #:cl)
  (:import-from #:mito
                #:object-id)
  (:import-from #:quickdist)
  (:import-from #:ultralisp/utils/db
                #:inflate-json
                #:deflate-json)
  (:import-from #:ultralisp/models/dist-source
                #:dist-name
                #:source->dists)
  (:import-from #:ultralisp/models/source
                #:get-all-sources
                #:source-systems-info)
  (:import-from #:ultralisp/models/project
                #:project-name
                #:source->project)
  (:import-from #:rutils
                #:fmt)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:local-time
                #:timestamp<)
  (:export
   #:dist-id
   #:asdf-system-name
   #:source-id
   #:asdf-system-dependencies
   #:make-asdf-system
   #:fill-database
   #:get-conflicting-systems
   #:asdf-system-project
   #:remove-source-systems
   #:asdf-systems-conflict))
(in-package #:ultralisp/models/asdf-system)


(defclass asdf-system ()
  ((dist-id :col-type :bigint
            :initarg :dist-id
            :reader dist-id)
   (name :col-type :text
         :initarg :name
         :reader asdf-system-name)
   (source-id :col-type :bigint
              :initarg :source-id
              :reader source-id)
   (dependencies :col-type :jsonb
                 :initarg :dependencies
                 :initform nil
                 :accessor asdf-system-dependencies
                 :deflate #'deflate-json
                 :inflate #'inflate-json
                 :documentation "A list of strings with ASDF system names."))
  (:primary-key dist-id name)
  (:metaclass mito:dao-table-class))


(defun make-asdf-system (dist-id name source-id dependencies)
  (mito:create-dao 'asdf-system
                   :dist-id dist-id
                   :name name
                   :source-id source-id
                   :dependencies (sort (copy-list dependencies)
                                       #'string<)))


(defparameter *cond* nil)

(defparameter *original-cond* nil)

(define-condition asdf-systems-conflict (error)
  ((conflicts :initarg :conflicts
              :reader asdf-systems-conflicts)
   (dist :initarg :dist
         :reader asdf-systems-conflicting-dist))
  (:report (lambda (condition stream)
             (format stream "Distribution ~S already has these ASDF systems:~%~{~A~^~%~}"
                     (dist-name (asdf-systems-conflicting-dist condition))
                     (loop for conflict in (asdf-systems-conflicts condition)
                           for system-name = (asdf-system-name conflict)
                           for project = (asdf-system-project conflict)
                           for project-name = (project-name project)
                           collect (fmt " - ~S in project ~S"
                                        system-name
                                        project-name))))))


(defun add-source-systems (dist source)
  (ultralisp/db:with-transaction
    (let ((conflicts (get-conflicting-systems source :dists dist)))
      (when conflicts
        (error 'asdf-systems-conflict
               :dist dist
               :conflicts conflicts)))
    
    (remove-source-systems dist source)
  
    (loop for system-info in (source-systems-info source)
          for system-name = (quickdist:get-name system-info)
          for dependencies = (quickdist:get-dependencies system-info)
          do (make-asdf-system (object-id dist)
                               system-name
                               (object-id source)
                               dependencies))))


(defun remove-source-systems (dist source)
  (mito:execute-sql "DELETE FROM asdf_system WHERE dist_id = ? AND source_id = ?"
                    (list (mito:object-id dist)
                          (mito:object-id source))))


(defmethod print-object ((obj asdf-system) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream
            "~12I~S~:@_source=~A~:@_dependencies=~S"
            (asdf-system-name obj)
            (or (ignore-errors (asdf-system-source obj))
                "<error:unable to fetch source>")
            (asdf-system-dependencies obj))))


(defun asdf-system-source (system)
  (ultralisp/models/source:get-latest-version-by-id
   (source-id system)))

(defun asdf-system-project (system)
  (source->project
   (asdf-system-source system)))


(defun fill-database ()
  "Builds a hash dist-id->system-name->source. Then fills \"asdf_system\"
   table with this data.

   Should be used manually, to fill database after migration."
  
  (let ((sources (get-all-sources))
        (dist-id->dist (make-hash-table :test 'equal))
        (dist-id->system-name->sources (make-hash-table :test 'equal)))
    
    (labels ((register (dists systems source)
               (loop for dist in dists
                     do (add-systems-to-dist dist systems source)))
             
             (get-dist-hash (dist)
               (let ((dist-id (mito:object-id dist)))
                 (unless (gethash dist-id dist-id->dist)
                   (setf (gethash dist-id dist-id->dist)
                         dist)
                   (setf (gethash dist-id dist-id->system-name->sources)
                         (make-hash-table :test 'equal)))
                 (gethash dist-id dist-id->system-name->sources)))
             
             (add-systems-to-dist (dist systems source)
               (loop with hash = (get-dist-hash dist)
                     for system-info in systems
                     for system-name = (quickdist:get-name system-info)
                     do (push source
                              (gethash system-name hash)))))
      (loop for source in sources
            for dists = (source->dists source :enabled t)
            for systems = (source-systems-info source)
            do (register dists systems source)
            finally (progn (save-hash-to-db dist-id->system-name->sources)
                           (return dist-id->system-name->sources))))))


(defun save-hash-to-db (dist-id->system-name->sources)
  (loop initially (mito:execute-sql
                   "TRUNCATE TABLE asdf_system" )
        for dist-id being the hash-keys of dist-id->system-name->sources
          using (hash-value system-name->sources)
        do (loop for system-name being the hash-keys of system-name->sources
                   using (hash-value sources)
                 for sorted-sources = (sort (copy-list sources)
                                            #'timestamp<
                                            :key #'mito:object-created-at)
                 for source = (first sorted-sources)
                 for source-id = (mito:object-id source)
                 ;; Now we need to extract system's dependencies
                 ;; from the source
                 for source-systems = (source-systems-info source)
                 for system-info = (find system-name source-systems
                                         :key #'quickdist:get-name
                                         :test #'string=)
                 for dependencies = (quickdist:get-dependencies system-info)
                 do (make-asdf-system dist-id
                                      system-name
                                      source-id
                                      dependencies))))


(defun get-conflicting-systems (source &key dists)
  "Checks if this source includes some systems which already provided
   by other sources in any of the dists to which the source is connected."
  (let* ((source-id (mito:object-id source))
         ;; It is important here to fetch all dists
         ;; even if source is disabled there. Otherwise
         ;; source will be disabled during the one check
         ;; and enabled during the next check.
         (dists (or (uiop:ensure-list dists)
                    (source->dists source)))
         (dist-ids (mapcar #'mito:object-id
                           dists))
         (systems (source-systems-info source))
         (system-names (mapcar #'quickdist:get-name
                               systems)))
    (when (and dist-ids
               system-names)
      (mito:select-dao 'asdf-system
        (sxql:where (:and (:in 'dist-id
                               dist-ids)
                          (:in 'name
                               system-names)
                          (:!= 'source-id
                               source-id)))))))


(defun show-conflicts ()
  "Returns a list of lists.
   Each item is a plist like:

   (:source <source> :conflicts (<conflicting-asdf-system1> ...))
  "
  (loop for source in (get-all-sources)
        for conflicts = (get-conflicting-systems source)
        when conflicts
          collect (list :source source
                        :conflicts conflicts)))
