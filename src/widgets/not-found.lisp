(defpackage #:ultralisp/widgets/not-found
  (:use #:cl)
  (:import-from #:weblocks/page)
  (:import-from #:weblocks/app)
  (:import-from #:weblocks/html
                #:with-html-string)
  (:import-from #:weblocks/response
                #:abort-processing)
  (:export #:page-not-found))
(in-package ultralisp/widgets/not-found)


(defun page-not-found (&key (title "Outer space"))
  "This is not a widget, but a function to interrupt
   any processing or rendering and to show 404 page instead."
  (setf (weblocks/page:get-title)
        title)

  (weblocks/response:abort-processing
   ;; TODO: replace with weblocks/response:return-page
   (with-html-string
     (weblocks/page:render (weblocks/app:get-current)
                           (with-html-string
                             (:h1 "404")
                             (:h2 "Page not found"))))
   :content-type "text/html"
   :code 404))


