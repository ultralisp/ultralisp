(defpackage #:ultralisp/mail
  (:use #:cl)
  (:import-from #:mailgun)
  (:import-from #:reblocks/response
                #:make-uri)
  ;; (:import-from #:mito-email-auth/models
  ;;               #:get-code
  ;;               #:get-email)
  (:export
   #:send-login-code))
(in-package #:ultralisp/mail)


;; TODO: remove this code, or move it to reblocks-auth
;; (defun send-login-code (code &key retpath)
;;   (let* ((retpath (when retpath
;;                     (quri:url-encode retpath)))
;;          (url (make-uri
;;                (format nil
;;                        "/login?code=~A~@[&retpath=~A~]"
;;                        (get-code code)
;;                        retpath)))
;;          (email (get-email code)))

;;     (cond
;;       ((and mailgun:*domain*
;;             mailgun:*api-key*)
;;        (log:debug "Sending login code to" email)
       
;;        (mailgun:send ("Ultralisp <noreply@ultralisp.org>"
;;                       email
;;                       "The code to log into Ultralisp.org")
;;          (:p ("To log into [Ultralisp.org](~A), follow [this link](~A)."
;;               url
;;               url))
;;          (:p "Hurry up! This link will expire in one hour.")))

;;       (t (log:warn "You didn't set MAILGUN_DOMAIN and MAILGUN_API_KEY env variables. So I am unable to send auth code."
;;                    url)
;;          (reblocks/response:redirect url)))))
