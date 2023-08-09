(uiop:define-package #:ultralisp/api/search
  (:use #:cl)
  (:import-from #:ultralisp/db)
  (:import-from #:ultralisp/models/tag)
  (:import-from #:ultralisp/api/api
                #:api
                #:*default-page-size*)
  (:import-from #:alexandria
                #:plist-hash-table)
  (:import-from #:serapeum
                #:dict)
  (:import-from #:ultralisp/models/project
                #:project2)
  (:import-from #:mito
                #:object-id)
  (:import-from #:openrpc-server
                #:define-rpc-method))
(in-package #:ultralisp/api/search)


(defclass search-result ()
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


(define-rpc-method (api search-symbols) (term &key (page-key 0) (limit *default-page-size*))
  (:summary "Search a symbol by it's name or docstring.")
  (:param term string "A search term. Syntax is the same as on the site.")
  (:param page-key integer "Next page key.")
  (:param limit integer "Maximum number of items per page.")
  (:result (paginated-list-of search-result))
  
  (ultralisp/search:search-objects term)
  (loop for item in (ultralisp/search:search-objects term
                                                     :from page-key
                                                     :limit limit)
        for type = (string-downcase
                    (symbol-name (first item)))
        for symbol = (second item)
        for documentation = (third item)
        for other-params = (cdddr item)
        collect (make-instance 'search-result
                               :type type
                               :symbol symbol
                               :package (getf other-params :package)
                               :system (getf other-params :system)
                               :documentation documentation)
        into results
        finally (return (values results
                                (+ page-key
                                    (length results))))))
