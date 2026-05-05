(uiop:define-package #:ultralisp/widgets/search/all-tab
  (:use #:cl)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks-ui2/widget
                #:render
                #:ui-widget)
  (:import-from #:reblocks-ui2/themes/tailwind
                #:tailwind-theme)
  (:import-from #:ultralisp/widgets/search/results-section
                #:make-results-section)
  (:import-from #:ultralisp/widgets/search/project-card
                #:make-project-card)
  (:import-from #:ultralisp/widgets/search/system-card
                #:make-system-card)
  (:import-from #:ultralisp/widgets/search/symbol-card
                #:make-symbol-card)
  (:export
   #:make-all-tab))
(in-package #:ultralisp/widgets/search/all-tab)


(defwidget all-tab (ui-widget)
  ((query :initarg :query :reader tab-query)
   (dist :initarg :dist :reader tab-dist)
   (projects :initarg :projects :reader tab-projects)
   (systems :initarg :systems :reader tab-systems)
   (symbols :initarg :symbols :reader tab-symbols)))


(defun make-all-tab (&key query (dist "default") projects systems symbols)
  (make-instance 'all-tab
                 :query query :dist dist
                 :projects projects
                 :systems systems
                 :symbols symbols))


(defun make-project-cards (results)
  (loop for item in results
        collect (make-project-card
                 :name (getf item :|name|)
                 :description (getf item :|description|)
                 :tags (getf item :|tags|))))


(defun make-system-cards (results)
  (loop for item in results
        collect (make-system-card
                 :name (getf item :|name|)
                 :description (getf item :|description|)
                 :project-name (getf item :|project-name|)
                 :dependencies (getf item :|dependencies|))))


(defun make-symbol-cards (results)
  (loop for item in results
        collect (make-symbol-card
                 :type (getf item :|type|)
                 :symbol (getf item :|symbol|)
                 :doc (or (getf item :|documentation|) "")
                 :arguments (getf item :|arguments|)
                 :project (getf item :|project|)
                 :system (getf item :|system|)
                 :package (getf item :|package|)
                 :original-package (getf item :|original-package|))))


(defmethod render ((widget all-tab) (theme tailwind-theme))
  (let* ((query (tab-query widget))
         (dist (tab-dist widget))
         (projects-data (tab-projects widget))
         (systems-data (tab-systems widget))
         (symbols-data (tab-symbols widget))
         (project-total (or (getf projects-data :total) 0))
         (system-total (or (getf systems-data :total) 0))
         (symbol-total (or (getf symbols-data :total) 0)))
    (with-html ()
      (when (> project-total 0)
        (render (make-results-section
                 :title "Projects"
                 :total project-total
                 :cards (make-project-cards (getf projects-data :results))
                 :tab-name "projects"
                 :query query
                 :dist dist)
                theme))
      (when (> system-total 0)
        (render (make-results-section
                 :title "Systems"
                 :total system-total
                 :cards (make-system-cards (getf systems-data :results))
                 :tab-name "systems"
                 :query query
                 :dist dist)
                theme))
      (when (> symbol-total 0)
        (render (make-results-section
                 :title "Symbols"
                 :total symbol-total
                 :cards (make-symbol-cards (getf symbols-data :results))
                 :tab-name "symbols"
                 :query query
                 :dist dist)
                theme))
      (when (and (zerop project-total)
                 (zerop system-total)
                 (zerop symbol-total))
        (:p ("No results for \"~A\"" query))))))
