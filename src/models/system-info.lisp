(uiop:define-package #:ultralisp/models/system-info
  (:use #:cl)
  (:import-from #:quickdist))
(in-package #:ultralisp/models/system-info)


;; TODO: Think if this class duplicates
;; models/asdf-system::asdf-system class in some sense.
;; Seems, I've used asdf-system class just to one-shot task
;; just to see if there are any system conflicts in the database.
;; Probably it should be removed?
(defclass system-info (quickdist:system-info)
  ;; Here we add additional fields to the base system info
  ;; to show these fields in API.
  ((license :initarg :license
            :initform nil
            :reader system-info-license)
   (author :initarg :author
           :initform nil
           :reader system-info-author)
   (maintainer :initarg :maintainer
               :initform nil
               :reader system-info-maintainer)
   (description :initarg :description
                :initform nil
                :reader system-info-description)
   (long-description :initarg :long-description
                     :initform nil
                     :reader system-info-long-description)))


(defun transform-to-ultralisp-systems-info (ql-system)
  (check-type ql-system quickdist:system-info)

  ;; We have to load asd file first
  ;; and known to ASDF. So we can safely call registered-system
  (let* ((systems-before '("asdf" "uiop"))
         (asdf/system-registry:*registered-systems*
           (quickdist::copy-hash-table-partially
            asdf/system-registry:*registered-systems*
            :keys systems-before))
         (system (progn
                   (asdf:load-asd
                    (quickdist:get-path ql-system))
                   (asdf:registered-system
                    (quickdist:get-name ql-system))))
         ;; For package-inferred systems, subsystems will not
         ;; be found because they are not defined in ASD file explicitly.
         ;; Thus for such systems we don't fill additional fields.
         (author (when system
                   (asdf:system-author system)))
         (maintainer (when system
                       (asdf:system-maintainer system)))
         (license (when system
                    (asdf:system-license system)))
         (description (when system
                        (asdf:system-description system)))
         (long-description (when system
                             (asdf:system-long-description system))))
    
    (change-class ql-system
                  'system-info
                  :author author
                  :maintainer maintainer
                  :license license
                  :description description
                  :long-description long-description)))
