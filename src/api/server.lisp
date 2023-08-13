(uiop:define-package #:ultralisp/api/server
  (:use #:cl)
  (:import-from #:jsonrpc)
  (:import-from #:openrpc-server
                #:type-to-schema)
  (:import-from #:ultralisp/api/projects)
  (:import-from #:ultralisp/api/search)
  (:import-from #:ultralisp/api/tags)
  (:import-from #:ultralisp/models/source)
  (:import-from #:alexandria
                #:plist-hash-table))
(in-package #:ultralisp/api/server)


(defmethod openrpc-server/interface:slots-to-exclude ((obj mito.dao:dao-table-class))
  (list* "synced"
         (call-next-method)))


(defmethod openrpc-server/interface:slots-to-exclude ((type (eql (find-class 'quickdist:system-info))))
  (list* "path"
         "project-name"
         (call-next-method)))


(defmethod openrpc-server/interface:transform-result ((obj symbol))
  (when obj
    (symbol-name obj)))


(defmethod openrpc-server/interface:transform-result ((obj ultralisp/models/source:source))
  (let ((result (call-next-method))
        (params (ultralisp/models/source::source-params obj)))
    (loop with params-hash = (make-hash-table :test 'equal)
          for (key value) on params by #'cddr
          do (setf (gethash (string-downcase
                             (symbol-name key))
                            params-hash)
                   value)
          finally (setf (gethash "params" result)
                        params-hash))
    (values result)))
