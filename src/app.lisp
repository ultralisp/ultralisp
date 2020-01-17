(defpackage #:ultralisp/app
  (:use #:cl)
  (:import-from #:weblocks/app
                #:defapp))
(in-package ultralisp/app)


(defapp app
  :prefix "/"
  :description "The UltraLisp.org server."
  :autostart nil
  :debug t)
