(defpackage #:ultralisp/widgets/sponsors
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks/page
                #:get-title)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks-ui2/widget
                #:render
                #:ui-widget)
  (:import-from #:reblocks-ui2/themes/tailwind
                #:tailwind-theme)
  (:export
   #:make-sponsors-widget))
(in-package #:ultralisp/widgets/sponsors)


(defwidget sponsors-widget (ui-widget)
  ())


(defun make-sponsors-widget ()
  (make-instance 'sponsors-widget))


(defmethod render ((widget sponsors-widget) (theme tailwind-theme))
  (setf (get-title)
        "Ultralisp - Sponsors")

  (with-html ()
    (:h2 :class "text-2xl font-bold" "Gold Sponsors")
    (:p "This place is vacant. Links to Gold Supporters will also be placed on the front page of the site.")

    (:h2 :class "text-2xl font-bold mt-6" "Grand Supporters")
    (:p "This place is vacant. Links to Gold Supporters will also be placed on the front page of the site.")

    (:p ("To get listed on this page, become a \"gold\" or \"grand\" sponsor on [Patreon](https://www.patreon.com/ultralisp) and donate money to support further development of the Ultralisp.org:"))
    (:div :class "mt-2"
          (:a :class "inline-block px-4 py-2 bg-green-600 text-white rounded hover:bg-green-700"
              :href "https://www.patreon.com/join/ultralisp"
              "Donate $$$ at Patreon"))))
