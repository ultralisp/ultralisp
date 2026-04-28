(defpackage #:ultralisp/widgets/frame
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:40ants-routes/route-url
                #:route-url)
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
  (:import-from #:ultralisp/metrics
                #:get-number-of-projects)
  (:import-from #:ultralisp/widgets/login-menu
                #:make-login-menu)
  (:import-from #:ultralisp/variables
                #:*link-color-classes*
                #:+cl-info+
                #:*started-at*)
  (:import-from #:ultralisp/utils/time
                #:humanize-duration)
  (:import-from #:local-time
                #:now)
  (:import-from #:local-time-duration
                #:timestamp-difference)
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
        (show-search (null (uiop:getenv "HIDE_SEARCH")))
        (num-projects (or (ignore-errors
                            (get-number-of-projects))
                          0)))
    (with-html ()
      (:header :class "border-b border-sky-200 pb-2 mb-4"
               (:h1 :class "text-4xl font-bold"
                     (:a :href "/" :class *link-color-classes*
                         "Ultralisp.org")
                     (unless (zerop num-projects)
                       (:sup :class "text-sm font-normal text-gray-500 ml-1"
                             (format nil "includes ~R project~P"
                                     num-projects
                                     num-projects))))
               (when show-search
                 (:form :method "GET"
                        :action (route-url "search")
                        :class "mt-2 flex gap-2"
                        (:input :type "text"
                                :name "query"
                                :value query
                                :class "border rounded px-2 py-1 w-full max-w-md"
                                :placeholder "search a symbol"))
                 (:p :class "text-xs text-gray-500 mt-1"
                     ("Try: ~A" (random-elt +search-help+))))))))


(defun make-version-info ()
  (format nil "~A~@[~2%Uptime: ~A~]"
          +cl-info+
          (when *started-at*
            (humanize-duration
             (timestamp-difference (now)
                                   *started-at*)))))


(defun render-footer ()
  (with-html ()
    (:footer :class "text-gray-400 mt-12"
             (:p "Ultralisp v"
                 (:span :title (make-version-info)
                        (asdf:component-version
                         (asdf:find-system :ultralisp)))
                 (" proudly served by [Common Lisp](https://common-lisp.net) and [Reblocks](http://40ants.com/reblocks/)!")))))


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


(defmethod reblocks-ui2/themes/styling:css-classes ((widget page-frame-widget) (theme tailwind-theme) &key)
  (list "mt-4 mb-4"))
