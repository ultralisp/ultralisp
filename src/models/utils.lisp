(defpackage #:ultralisp/models/utils
  (:use #:cl)
  (:import-from #:jonathan)
  (:import-from #:quickdist
                #:get-path
                #:get-filename
                #:get-dependencies
                #:get-system-files
                #:get-project-prefix
                #:get-content-sha1
                #:get-md5sum
                #:get-file-size
                #:get-archive-path
                #:get-project-url
                #:get-project-name)
  (:import-from #:ultralisp/models/system-info
                #:system-info)
  (:export #:systems-info-to-json
           #:release-info-to-json
           #:systems-info-from-json
           #:release-info-from-json))
(in-package #:ultralisp/models/utils)


(defun %system-info-to-json (system-info)
  (check-type system-info system-info)
  
  (list :path (uiop:native-namestring (get-path system-info))
        :project-name (get-project-name system-info)
        :filename (get-filename system-info)
        :name (quickdist:get-name system-info)
        :dependencies (get-dependencies system-info)
        :license (ultralisp/models/system-info::system-info-license system-info)
        :author (ultralisp/models/system-info::system-info-author system-info)
        :maintainer (ultralisp/models/system-info::system-info-maintainer system-info)
        :description (ultralisp/models/system-info::system-info-description system-info)
        :long-description (ultralisp/models/system-info::system-info-long-description system-info)))

(defun systems-info-to-json (systems-info)
  "Prepares a list of systems info objects to be serialized to json."
  (jonathan:to-json
     (mapcar #'%system-info-to-json
           systems-info)))


(defun release-info-to-json (release-info)
  (jonathan:to-json
   (if release-info
       (list :project-name (get-project-name release-info)
             :project-url (get-project-url release-info)
             :archive-path (uiop:native-namestring (get-archive-path release-info))
             :file-size (get-file-size release-info)
             :md5sum (get-md5sum release-info)
             :content-sha1 (get-content-sha1 release-info)
             :project-prefix (get-project-prefix release-info)
             :system-files (get-system-files release-info))
       :null)))


(defun %system-info-from-json (data)
  "Prepares a list of systems info objects to be deserialized from json."
  (when data
    (make-instance 'system-info
                   :path (getf data :path)
                   :project-name (getf data :project-name)
                   :filename (getf data :filename)
                   :name (getf data :name)
                   :dependencies (getf data :dependencies)
                   :license (getf data :license)
                   :author (getf data :author)
                   :maintainer (getf data :maintainer)
                   :description (getf data :description)
                   :long-description (getf data :long-description))))


(defun release-info-from-json (json)
  (let ((data (jonathan:parse (coerce json 'simple-base-string))))
    (when data
      (make-instance 'quickdist:release-info
                     :project-name (getf data :project-name)
                     :project-url (getf data :project-url)
                     :archive-path (getf data :archive-path)
                     :file-size (getf data :file-size)
                     :md5sum (getf data :md5sum)
                     :content-sha1 (getf data :content-sha1)
                     :project-prefix (getf data :project-prefix)
                     :system-files (getf data :system-files)))))


(defun systems-info-from-json (json)
  "Prepares a list of systems info objects to be serialized to json."
  (let ((data (jonathan:parse (coerce json 'simple-base-string))))
    (mapcar #'%system-info-from-json
            data)))
