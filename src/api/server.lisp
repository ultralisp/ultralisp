(uiop:define-package #:ultralisp/api/server
  (:use #:cl)
  (:import-from #:jsonrpc)
  (:import-from #:openrpc-server
                #:type-to-schema)
  (:import-from #:ultralisp/api/projects)
  (:import-from #:ultralisp/api/search))
(in-package #:ultralisp/api/server)


(defmethod openrpc-server/interface:slots-to-exclude ((type mito.dao.table:dao-table-class))
  (list* "synced"
         (call-next-method)))
