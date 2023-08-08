(uiop:define-package #:ultralisp/client
  (:use #:cl)
  (:shadow #:apropos))
(in-package #:ultralisp/client)

;; TODO: придумать как генерить closure для запроса следующей страницы
(openrpc:generate-client ultralisp-client
                         "http://localhost:8081/api/openrpc.json")


(defvar *client* nil)


(defun ensure-client-connected ()
  (unless *client*
    (let ((cl (make-ultralisp-client)))
      (jsonrpc:client-connect cl :url "http://localhost:8081/api/" :mode :http)
      (setf *client* cl))))


(defun apropos (term)
  (ensure-client-connected)
  (let ((results (search-symbol *client* term)))
    (cond
      (results
       (loop for result in results
             do (format t "~2&### ~A:~A (~A)~2&~A"
                        (search-result-package result)
                        (search-result-symbol result)
                        (search-result-type result)
                        (search-result-documentation result))))
      (t
       (format t "No results. Try another search term."))))
  (values))
