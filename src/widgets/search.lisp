(defpackage #:ultralisp/widgets/search
  (:use #:cl)
  (:import-from #:3bmd)
  (:import-from #:weblocks-lass)
  (:import-from #:weblocks/request)
  (:import-from #:weblocks/page)
  (:import-from #:weblocks/widget
                #:render
                #:defwidget)
  (:import-from #:ultralisp/search
                #:search-objects)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:cl-ppcre
                #:regex-replace-all)
  (:import-from #:weblocks/dependencies
                #:get-dependencies)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:rutils
                #:fmt
                #:take)
  (:export
   #:make-search-page))
(in-package ultralisp/widgets/search)


(defwidget search-results ()
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
  "In given text replaces `items' with `items` and
   render it as Markdown to HTML.

   From text:

   \"Test `with` foo `bar' and 'baz` `like' blah\"

   it first will make:

   \"Test `with` foo `bar` and 'baz` `like` blah\"

   Returns a string with HTML."
  (let ((replaced (regex-replace-all
                   "`([^`]+?)'"
                   doc
                   "`\\1`")))
    (with-output-to-string (s)
      (3bmd:parse-string-and-print-to-stream replaced s))))


(defgeneric render-item (type name doc &rest rest)
  (:method (type name doc &key arguments system project package original-package)
    (with-html
      (:li (:span :class "name"
                  ("~:@(~A:~A~)" package name))
           (when arguments
             (:span :class "args" arguments))
           (:span :class "type" type)
           (:div :class "doc"
                 (:raw (to-html doc)))
           (when project
             (:div :class "item-footer project"
                   (:label "project:")
                   (:a :href (fmt "/projects/~A" project)
                       project)))
           (when system
             (:div :class "item-footer system"
                   (:label "system:")
                   (:span system)))
           (when original-package
             (:div :class "item-footer package"
                   (:label "original-package:")
                   (:span original-package)))))))


(defun to-uppercased-symbols (item)
  (append (take 3 item) ;; First 3 items are type name doc
          ;; Rest items are keyword arguments
          (loop for (key value) on (cdddr item) by #'cddr
                appending (list (make-keyword (string-upcase key))
                                value))))


(defmethod render ((widget search-results))
  (let ((query (weblocks/request:get-parameter "query")))
    (when query
      (setf (weblocks/page:get-title)
            (fmt "Search results for \"~A\"" query))
      (setf (get-query widget)
            query))
    (with-html
      (cond
        ((get-error widget)
         (:p ("Unable to parse \"~A\"" query)))
        ((get-results widget)
         (let ((results (get-results widget))
               (total (get-total widget)))
           (:ul :class "search-results"
                (loop for item in results
                      for uppercased = (to-uppercased-symbols item)
                      do (apply #'render-item
                                uppercased)))
           (when (> total (length results))
             (weblocks-ui/form:render-link
              (lambda (&rest args)
                (declare (ignorable args))
                (fetch-next-results widget)
                (weblocks/widget:update widget))
              "Load more"))))
        (t (:p ("No results for \"~A\"" query)))))))


(defmethod get-dependencies ((widget search-results))
  (append
   (list
    (weblocks-lass:make-dependency
      `(.search-results
        :margin 0
        (li :list-style-type none
            :margin-bottom 1em
            (.name :font-weight bold)
            (.type :margin-left 0.7em
                   :color gray)
            (.args :margin-left 0.7em
                   :color gray)
            (.doc
             ((:and p :last-child) :margin-bottom 0))
            (.item-footer
             :display inline-block
             :font-size 0.75em
             (label :display inline-block))))))
   (call-next-method)))
