(defpackage #:ultralisp/widgets/frame
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks-ui2/widget
                #:render
                #:ui-widget)
  (:import-from #:reblocks-ui2/themes/tailwind
                #:tailwind-theme)
  (:import-from #:reblocks/request
                #:get-parameter)
  (:import-from #:reblocks/page
                #:get-title)
  (:import-from #:reblocks-auth/models
                #:get-current-user
                #:get-nickname
                #:anonymous-p)
  (:import-from #:reblocks/response
                #:add-retpath-to)
  (:import-from #:alexandria
                #:random-elt)
  (:import-from #:ultralisp/analytics
                #:render-google-counter
                #:render-yandex-counter)
  (:import-from #:ultralisp/widgets/login-menu
                #:make-login-menu)
  (:export #:wrap-with-page-frame))
(in-package #:ultralisp/widgets/frame)


(defparameter +search-help+
  (list "signal - this will search in symbol's name and documentation."
        "project:\"40ants/reblocks\" AND symbol:\"request\""
        "package:\"reblocks/actions\" to search all symbols exported from a package."))


(defwidget page-frame-widget (ui-widget)
  ((content-widget :initarg :content
                   :reader content)))


(defun wrap-with-page-frame (widget)
  (make-instance 'page-frame-widget
                 :content widget))


(defun render-header ()
  (let ((query (get-parameter "query"))
        (show-search (null (uiop:getenv "HIDE_SEARCH"))))
    (with-html ()
      (:header :class "border-b border-sky-200 pb-2 mb-4"
               (:h1 :class "text-2xl font-bold"
                    (:a :href "/" :class "text-sky-600 hover:text-sky-700"
                        "Ultralisp.org"))
               (when show-search
                 (:form :method "GET"
                        :action "/search/"
                        :class "mt-2 flex gap-2"
                        (:input :type "text"
                                :name "query"
                                :value query
                                :class "border rounded px-2 py-1 w-full max-w-md"
                                :placeholder "search a symbol"))
                 (:p :class "text-xs text-gray-500 mt-1"
                     ("Try: ~A" (random-elt +search-help+))))))))


(defun render-footer ()
  (with-html ()
    (:footer :class "text-gray-400 mt-12"
             (:p ("Ultralisp v~A proudly served by [Common Lisp](https://common-lisp.net) and [Reblocks](http://40ants.com/reblocks/)!"
                  (asdf:component-version
                   (asdf:find-system :ultralisp)))))))


(defmethod render ((widget page-frame-widget) (theme tailwind-theme))
  (render-yandex-counter)
  (render-google-counter)
  (let ((login-menu (make-login-menu)))
    (render login-menu theme))
  (with-html ()
    (:div :class "max-w-4xl mx-auto px-4"
          (render-header)
          (:div :class "page-content"
                (render (content widget) theme))
          (render-footer))))
