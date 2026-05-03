(uiop:define-package #:ultralisp/widgets/search/page
  (:use #:cl)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/request
                #:get-parameter)
  (:import-from #:reblocks/page
                #:get-title)
  (:import-from #:reblocks/widget
                #:defwidget
                #:update)
  (:import-from #:reblocks-ui2/widget
                #:render
                #:ui-widget)
  (:import-from #:reblocks-ui2/themes/tailwind
                #:tailwind-theme)
  (:import-from #:reblocks-ui2/containers/tabs
                #:tabs)
  (:import-from #:ultralisp/search2
                #:search-all)
  (:import-from #:ultralisp/search
                #:bad-query)
  (:import-from #:ultralisp/widgets/search/all-tab
                #:make-all-tab)
  (:import-from #:ultralisp/widgets/search/projects-tab
                #:make-projects-tab)
  (:import-from #:ultralisp/widgets/search/systems-tab
                #:make-systems-tab)
  (:import-from #:ultralisp/widgets/search/symbols-tab
                #:make-symbols-tab)
  (:import-from #:rutils
                #:fmt)
  (:export
   #:make-search-page))
(in-package #:ultralisp/widgets/search/page)


(defwidget search-page (ui-widget)
  ((query :initform "" :type string :reader get-query)
   (tab-param :initform ""
              :type string
              :reader get-tab-param)
   (dist :initform "default" :type string :reader get-dist)
   (tabs-widget :initform nil :accessor get-tabs-widget)
   (error :initform nil :accessor get-error)))


(defun make-search-page ()
  (make-instance 'search-page))


(defun build-tabs (widget query tab-param dist)
  (handler-case
      (let* ((data (search-all query :dist dist))
             (projects-data (getf data :projects))
             (systems-data (getf data :systems))
             (symbols-data (getf data :symbols))
             (project-total (or (getf projects-data :total) 0))
             (system-total (or (getf systems-data :total) 0))
             (symbol-total (or (getf symbols-data :total) 0))
             (titles (list "All"))
             (subwidgets (list (make-all-tab
                                :query query
                                :dist dist
                                :projects projects-data
                                :systems systems-data
                                :symbols symbols-data))))
        (when (> project-total 0)
          (push "Projects" titles)
          (push (make-projects-tab :query query :dist dist) subwidgets))
        (when (> system-total 0)
          (push "Systems" titles)
          (push (make-systems-tab :query query :dist dist) subwidgets))
        (when (> symbol-total 0)
          (push "Symbols" titles)
          (push (make-symbols-tab :query query :dist dist) subwidgets))
        (let* ((titles (nreverse titles))
               (subwidgets (nreverse subwidgets))
               (initial-idx (or (position (cond
                                            ((string= tab-param "projects") "Projects")
                                            ((string= tab-param "systems") "Systems")
                                            ((string= tab-param "symbols") "Symbols")
                                            (t "All"))
                                         titles
                                         :test #'string=)
                                0)))
          (setf (get-tabs-widget widget)
                (tabs titles subwidgets :idx initial-idx))))
    (bad-query ()
      (setf (get-error widget) t))))


(defmethod render ((widget search-page) (theme tailwind-theme))
  (let ((query (get-parameter "query"))
        (tab-param (get-parameter "tab"))
        (dist (or (get-parameter "dist") "default")))
    (when query
      (setf (get-title)
            (fmt "Search results for \"~A\"" query))
      
      (unless (and (string= (get-query widget)
                            query)
                   (string= (get-tab-param widget)
                            tab-param)
                   (string= (get-dist widget)
                            dist))
        (setf (slot-value widget 'query) query
              (slot-value widget 'tab-param) tab-param
              (slot-value widget 'dist) dist
              (get-error widget) nil
              (get-tabs-widget widget) nil)
        (build-tabs widget query tab-param dist)))
    
    (with-html ()
      (cond
        ((get-error widget)
         (:p ("Unable to parse \"~A\"" (get-query widget))))
        ((get-tabs-widget widget)
         (render (get-tabs-widget widget) theme))
        (t (:p "Enter a search query."))))))
