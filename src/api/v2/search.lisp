(uiop:define-package #:ultralisp/api/v2/search
  (:use #:cl)
  (:import-from #:ultralisp/api/api
                #:api
                #:*default-page-size*)
  (:import-from #:ultralisp/search2)
  (:import-from #:ultralisp/search
                #:bad-query)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:openrpc-server
                #:define-rpc-method))
(in-package #:ultralisp/api/v2/search)


(defclass search-result-group ()
  ((type :initarg :type
         :type string
         :reader search-result-group-type)
   (total :initarg :total
          :type integer
          :reader search-result-group-total)
   (results :initarg :results
            :type list
            :reader search-result-group-results)))


(defclass search-result-item ()
  ((name :initarg :name
         :type string
         :reader search-result-item-name)
   (description :initarg :description
                :type string
                :reader search-result-item-description)))


(defclass project-search-result (search-result-item)
  ((tags :initarg :tags
         :type list
         :reader search-result-tags)))


(defclass system-search-result (search-result-item)
  ((project-name :initarg :project-name
                 :type string
                 :reader search-result-project-name)
   (dependencies :initarg :dependencies
                  :type list
                  :reader search-result-dependencies)))


(defclass symbol-search-result (search-result-item)
  ((type :initarg :type
         :type string
         :reader search-result-type)
   (symbol :initarg :symbol
           :type string
           :reader search-result-symbol)
   (package :initarg :package
            :type string
            :reader search-result-package)
   (system :initarg :system
           :type string
           :reader search-result-system)
   (documentation :initarg :documentation
                  :type string
                  :reader search-result-documentation)))


(defun make-project-result (source)
  (make-instance 'project-search-result
                 :name (getf source :|name|)
                 :description (or (getf source :|description|) "")
                 :tags (getf source :|tags|)))


(defun make-system-result (source)
  (make-instance 'system-search-result
                 :name (getf source :|name|)
                 :description (or (getf source :|description|) "")
                 :project-name (or (getf source :|project-name|) "")
                 :dependencies (getf source :|dependencies|)))


(defun make-symbol-result (source)
  (make-instance 'symbol-search-result
                 :type (or (getf source :|type|) "")
                 :symbol (or (getf source :|symbol|) "")
                 :package (or (getf source :|package|) "")
                 :system (or (getf source :|system|) "")
                 :documentation (or (getf source :|documentation|) "")))


(define-rpc-method (api search-all) (term &key (dist "default"))
  (:summary "Search across projects, systems, and symbols.")
  (:param term string "A search term.")
  (:param dist string "Distribution name. Default: \"default\" for main Ultralisp.")
  (:result (list-of search-result-group))

  (let ((data (ultralisp/search2:search-all term :dist dist)))
    (when data
      (let ((project-data (getf data :projects))
            (system-data (getf data :systems))
            (symbol-data (getf data :symbols)))
        (list
         (make-instance 'search-result-group
                        :type "projects"
                        :total (getf project-data :total)
                        :results (mapcar #'make-project-result
                                         (getf project-data :results)))
         (make-instance 'search-result-group
                        :type "systems"
                        :total (getf system-data :total)
                        :results (mapcar #'make-system-result
                                         (getf system-data :results)))
         (make-instance 'search-result-group
                        :type "symbols"
                        :total (getf symbol-data :total)
                        :results (mapcar #'make-symbol-result
                                         (getf symbol-data :results))))))))


(define-rpc-method (api search-by-type) (term type &key (page-key 0) (limit *default-page-size*) (dist "default"))
  (:summary "Search entities of a specific type.")
  (:param term string "A search term.")
  (:param type string "One of: projects, systems, symbols.")
  (:param page-key integer "Offset for pagination.")
  (:param limit integer "Max items per page.")
  (:param dist string "Distribution name. Default: \"default\" for main Ultralisp.")
  (:result (paginated-list-of search-result-item))

  (let* ((keyword-type (make-keyword (string-upcase type)))
         (make-result-fn (ecase keyword-type
                           (:projects #'make-project-result)
                           (:systems #'make-system-result)
                           (:symbols #'make-symbol-result)))
         (results (ultralisp/search2:search-by-type term keyword-type
                                                    :from page-key
                                                    :limit limit
                                                    :dist dist)))
    (values (mapcar make-result-fn results)
            (+ page-key (length results)))))
