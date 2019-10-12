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
  (:export
   #:make-search-page))
(in-package ultralisp/widgets/search)


(defwidget search-results ()
  ())


(defun make-search-page ()
  (make-instance 'search-results))


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


(defgeneric render-item (type name doc)
  (:method (type name doc)
    (with-html
      (:li (:span :class "name" name)
           (:span :class "type" type)
           (:div :class "doc"
                 (:raw (to-html doc)))))))

(defmethod render ((widget search-results))
  (let* ((query (weblocks/request:get-parameter "query"))
         (results (search-objects query)))
    (with-html
      (cond
        (results
         (:p ("Results for \"~A\"" query))
         (:ul :class "search-results"
          (loop for item in results
                do (apply #'render-item item))))
        (t
         (:p ("No results for \"~A\"" query)))))))


(defmethod get-dependencies ((widget search-results))
  (append
   (list
    (weblocks-lass:make-dependency
      `(.search-results
        :margin 0
        (li :list-style-type none
            (.name :font-weight bold)
            (.type :margin-left 0.7em
                   :color gray)))))
   (call-next-method)))
