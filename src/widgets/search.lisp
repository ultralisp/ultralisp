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


(defgeneric render-item (type name doc &rest rest)
  (:method (type name doc &key arguments system project)
    (with-html
      (:li (:span :class "name" name)
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
                   (:span system)))))))


(defun to-uppercased-symbols (item)
  (append (take 3 item) ;; First 3 items are type name doc
          ;; Rest items are keyword arguments
          (loop for (key value) on (cdddr item) by #'cddr
                appending (list (make-keyword (string-upcase key))
                                value))))


(defmethod render ((widget search-results))
  (let ((query (weblocks/request:get-parameter "query")))
    (setf (weblocks/page:get-title)
          (fmt "Search results for \"~A\"" query))
    (handler-case
        (let ((results (search-objects query)))
          (with-html
            (cond
              (results
               (:p ("Results for \"~A\"" query))
               (:ul :class "search-results"
                    (loop for item in results
                          for uppercased = (to-uppercased-symbols item)
                          do (apply #'render-item
                                    uppercased))))
              (t
               (:p ("No results for \"~A\"" query))))))
      (ultralisp/search:bad-query ()
        (with-html
          (:p ("Unable to parse \"~A\"" query)))))))


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
