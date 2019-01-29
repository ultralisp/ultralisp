(defpackage #:ultralisp/widgets/not-found
  (:use #:cl)
  (:import-from #:weblocks/page
                #:render
                #:get-title)
  (:import-from #:weblocks/app)
  (:import-from #:weblocks/html
                #:with-html-string)
  (:import-from #:weblocks/response
                #:immediate-response)
  (:export #:page-not-found))
(in-package ultralisp/widgets/not-found)


(defun page-not-found (&key (title "Outer space"))
  "This is not a widget, but a function to interrupt
   any processing or rendering and to show 404 page instead."
  (setf (get-title)
        title)

  (immediate-response
   ;; TODO: replace with weblocks/response:return-page
   (with-html-string
     (render (weblocks/app:get-current)
             (with-html-string
               (:h1 "404")
               (:h2 "Page not found"))))
   :content-type "text/html"
   :code 404))


