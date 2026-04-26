(defpackage #:ultralisp/widgets/search
  (:use #:cl)
  (:import-from #:3bmd)
  (:import-from #:reblocks/request)
  (:import-from #:reblocks/page)
  (:import-from #:reblocks/widget
                #:defwidget
                #:update)
  (:import-from #:ultralisp/search
                #:search-objects)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:cl-ppcre
                #:regex-replace-all)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:log)
  (:import-from #:rutils
                #:fmt
                #:take)
  (:import-from #:reblocks-ui/form
                #:render-link)
  (:import-from #:reblocks-ui2/widget
                #:render
                #:ui-widget)
  (:import-from #:reblocks-ui2/themes/tailwind
                 #:tailwind-theme)
  (:import-from #:ultralisp/variables
                #:*link-color-classes*)
  (:export
   #:make-search-page))
(in-package #:ultralisp/widgets/search)


(defwidget search-results (ui-widget)
  ((query :initform ""
          :type string
          :reader get-query)
   (results :initform nil
            :accessor get-results
            :type list)
   (total :initform 0
          :accessor get-total
          :type integer)
   (next-results :initform nil
                 :accessor get-next-results-closure
                 :documentation "A closure to get next results.")
   (error :initform nil
          :accessor get-error
          :documentation "If elastic search will return error,
                          the condition will be stored in this slot.")))


(defun make-search-page ()
  (make-instance 'search-results))


(defun fetch-next-results (widget)
  (let ((getter (get-next-results-closure widget)))
    (when getter
      (log:info "Fetching next batch of results")
      (handler-case
          (multiple-value-bind (results total next-closure)
              (funcall getter)
            (setf (get-results widget)
                  (append (get-results widget)
                          results)
                  (get-total widget)
                  total
                  (get-next-results-closure widget)
                  next-closure))
        (ultralisp/search:bad-query (condition)
          (setf (get-error widget)
                condition))))))


(defmethod (setf get-query) (new-query (widget search-results))
  (when new-query
    (unless (string= (get-query widget)
                     new-query)
      (log:info "Changing the query" new-query)
      (setf (slot-value widget 'query)
            new-query
            (get-results widget)
            nil
            (get-error widget)
            nil
            (get-next-results-closure widget)
            (lambda ()
              (search-objects new-query)))
      (fetch-next-results widget))))


(defun to-html (doc)
  (let ((replaced (regex-replace-all
                   "`([^`]+?)'"
                   doc
                   "`\\1`")))
    (with-output-to-string (s)
      (3bmd:parse-string-and-print-to-stream replaced s))))


(defgeneric render-item (type name doc &rest rest)
  (:method (type name doc &key arguments system project package original-package)
    (with-html ()
      (:li :class "list-none mb-4"
           (:span :class "font-bold"
                  ("~:@(~A:~A~)" package name))
           (when arguments
             (:span :class "ml-2 text-gray-500" arguments))
           (:span :class "ml-2 text-gray-500" type)
           (:div :class "mt-1"
                 (:raw (to-html doc)))
           (when project
             (:div :class "text-xs inline-block"
                   (:label "project:")
                    (:a :href (fmt "/projects/~A" project)
                        :class *link-color-classes*
                        project)))
           (when system
             (:div :class "text-xs inline-block ml-2"
                   (:label "system:")
                   (:span system)))
           (when original-package
             (:div :class "text-xs inline-block ml-2"
                   (:label "original-package:")
                   (:span original-package)))))))


(defun to-uppercased-symbols (item)
  (append (take 3 item)
         (loop for (key value) on (cdddr item) by #'cddr
               appending (list (make-keyword (string-upcase key))
                               value))))


(defmethod render ((widget search-results) (theme tailwind-theme))
  (let ((query (reblocks/request:get-parameter "query")))
    (when query
      (setf (reblocks/page:get-title)
            (fmt "Search results for \"~A\"" query))
      (setf (get-query widget)
            query))
    (with-html ()
      (cond
        ((get-error widget)
         (:p ("Unable to parse \"~A\"" query)))
        ((get-results widget)
         (let ((results (get-results widget))
               (total (get-total widget)))
           (:ul :class "pl-0"
                (loop for item in results
                      for uppercased = (to-uppercased-symbols item)
                      do (apply #'render-item
                                uppercased)))
           (when (> total (length results))
             (render-link
              (lambda (&rest args)
                (declare (ignorable args))
                (fetch-next-results widget)
                (update widget))
              "Load more"))))
        (t (:p ("No results for \"~A\"" query)))))))
