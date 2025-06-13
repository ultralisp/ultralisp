(defpackage #:ultralisp/app
  (:use #:cl)
  (:import-from #:ultralisp/widgets/maintenance
                #:make-maintenance-widget)
  (:import-from #:ultralisp/widgets/main
                #:make-main-routes)
  
  (:import-from #:reblocks/app
                #:get-current
                #:defapp)
  (:import-from #:reblocks/routes
                #:static-file)
  (:import-from #:reblocks/request-handler)
  (:import-from #:40ants-routes/defroutes
                #:post)
  (:import-from #:ultralisp/routes
                #:process-webhook-route)
  (:import-from #:reblocks-prometheus
                #:metrics)
  (:import-from #:ultralisp/stats
                #:make-collector))
(in-package #:ultralisp/app)


(defapp app
  :prefix "/"
  :description "The UltraLisp.org server."
  :autostart nil
  :debug t
  :routes ((reblocks/routes:page ("/")
             (make-maintenance-widget
              (make-main-routes)))
           (static-file "/robots.txt"
                        (asdf:system-relative-pathname "ultralisp"
                                                       "static/robots.txt"))
           (static-file "/static/gear.gif"
                        (asdf:system-relative-pathname :ultralisp #P"src/widgets/gear.gif")
                        :content-type "image/gif")
           (metrics ("/metrics" :user-metrics (list (make-collector))))
           (post ("/webhook/github")
             (process-webhook-route (get-current)))))


;; Flamegraph does not work on SBCL 2.1.2 yet.
;; 
;; (defmethod reblocks/request-handler:handle-request :around ((app app))
;;   (let ((path (reblocks/request:get-parameter "flame")))
;;     (if path
;;         (flamegraph:save-flame-graph (path)
;;           (call-next-method))
;;         (call-next-method))))

