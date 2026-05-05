(uiop:define-package #:ultralisp/search2
  (:use #:cl)
  (:import-from #:ultralisp/search
                #:search-collection
                #:bad-query)
  (:export
   #:search-all
   #:search-by-type))
(in-package #:ultralisp/search2)


(defun search-all (query &key (per-type-limit 5) (dist "default"))
  "Search across projects, systems, and symbols.
Returns a plist:
 (:projects (:total N :results (...))
  :systems  (:total N :results (...))
  :symbols  (:total N :results (...)))"
  (flet ((do-search (collection fields)
           (multiple-value-bind (results total next-closure)
               (search-collection collection query
                                   :fields fields
                                   :limit per-type-limit
                                   :dist dist)
             (declare (ignore next-closure))
             (list :total total :results results))))
    (handler-case
        (list :projects (do-search "projects" '("name" "description"))
              :systems (do-search "systems" '("name" "description" "long-description" "author"))
              :symbols (do-search "symbols" '("documentation" "symbol" "package")))
      (bad-query ()
        nil))))


(defun search-by-type (query type &key (from 0) (limit 20) (dist "default"))
  "Search entities of a specific type.
TYPE is one of :projects, :systems, :symbols.
Returns (values results total next-closure)."
  (let ((collection (ecase type
                      (:projects "projects")
                      (:systems "systems")
                      (:symbols "symbols")))
        (fields (ecase type
                  (:projects '("name" "description"))
                  (:systems '("name" "description" "long-description" "author"))
                  (:symbols '("documentation" "symbol" "package")))))
    (search-collection collection query
                       :fields fields
                       :from from
                       :limit limit
                       :dist dist)))
