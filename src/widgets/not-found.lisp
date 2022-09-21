(defpackage #:ultralisp/widgets/not-found
  (:use #:cl)
  (:import-from #:reblocks/page
                #:render
                #:get-title)
  (:import-from #:reblocks/app)
  (:import-from #:reblocks/html
                #:with-html-string)
  (:import-from #:reblocks/response
                #:immediate-response)
  (:export #:page-not-found))
(in-package #:ultralisp/widgets/not-found)


(defun page-not-found (&key (title "Outer space"))
  "This is not a widget, but a function to interrupt
   any processing or rendering and to show 404 page instead."
  (setf (get-title)
        title)

  (immediate-response
   ;; TODO: replace with reblocks/response:return-page
   (with-html-string
     (render (reblocks/app:get-current)
             (with-html-string
               (:h1 "404")
               (:h2 "Page not found"))))
   :content-type "text/html"
   :code 404))


