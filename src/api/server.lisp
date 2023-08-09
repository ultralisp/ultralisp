(uiop:define-package #:ultralisp/api/server
  (:use #:cl)
  (:import-from #:jsonrpc)
  (:import-from #:openrpc-server
                #:type-to-schema)
  (:import-from #:ultralisp/api/projects)
  (:import-from #:ultralisp/api/search)
  (:import-from #:ultralisp/api/tags))
(in-package #:ultralisp/api/server)


(defmethod openrpc-server/interface:slots-to-exclude ((obj mito.dao:dao-table-class))
  (list* "synced"
         (call-next-method)))


(defmethod openrpc-server/interface:slots-to-exclude ((type (eql (find-class 'quickdist:system-info))))
  (list* "path"
         "project-name"
         (call-next-method)))
