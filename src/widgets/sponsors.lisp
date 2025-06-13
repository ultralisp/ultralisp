(defpackage #:ultralisp/widgets/sponsors
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:defwidget
                #:render)
  (:import-from #:reblocks/page
                #:get-title)
  (:import-from #:reblocks/html
                #:with-html)
  (:export
   #:make-sponsors-widget))
(in-package #:ultralisp/widgets/sponsors)


(defwidget sponsors-widget ()
  ())


(defun make-sponsors-widget ()
  (make-instance 'sponsors-widget))


;; Without this method widget had "sponsours-widget" class
;; and was removed by ADBlock:
(defmethod reblocks/widget:get-css-classes ((widget sponsors-widget))
  (list "widget sp-widget"))


(defmethod render ((widget sponsors-widget))
  (setf (get-title)
        "Ultralisp - Sponsors")
  
  (with-html ()
    (:h2 "Gold Sponsors")
    (:p "This place is vacant. Links to Gold Supporters will also be placed on the front page of the site.")
    
    (:h2 "Grand Supporters")
    (:p "This place is vacant. Links to Gold Supporters will also be placed on the front page of the site.")
    ;; (:ul
    ;;  (:li (:a :href "https://some"
    ;;           "name") ". Thank you!"))
    
    (:p ("To get listed on this page, become a \"gold\" or \"grand\" sponsor on [Patreon](https://www.patreon.com/ultralisp) and donate money to support further development of the Ultralisp.org:"))
    (:div :class "donate"
          (:a :class "button success"
              :href "https://www.patreon.com/join/ultralisp"
              "Donate $$$ at Patreon"))))
