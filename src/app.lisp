(defpackage #:ultralisp/app
  (:use #:cl)
  ;; (:import-from #:flamegraph)
  (:import-from #:reblocks/app
                #:defapp)
  (:import-from #:reblocks/request-handler))
(in-package #:ultralisp/app)


(defapp app
  :prefix "/"
  :description "The UltraLisp.org server."
  :autostart nil
  :debug t)


;; Flamegraph does not work on SBCL 2.1.2 yet.
;; 
;; (defmethod reblocks/request-handler:handle-request :around ((app app))
;;   (let ((path (reblocks/request:get-parameter "flame")))
;;     (if path
;;         (flamegraph:save-flame-graph (path)
;;           (call-next-method))
;;         (call-next-method))))
