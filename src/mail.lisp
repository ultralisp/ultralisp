(defpackage #:ultralisp/mail
  (:use #:cl)
  (:import-from #:mailgun)
  (:import-from #:weblocks/response
                #:make-uri)
  (:import-from #:mito-email-auth/models
                #:get-code
                #:get-email)
  (:export
   #:send-login-code))
(in-package ultralisp/mail)


(defun send-login-code (code &key retpath)
  (let* ((retpath (when retpath
                    (quri:url-encode retpath)))
         (url (make-uri
               (format nil
                       "/login?code=~A~@[&retpath=~A~]"
                       (get-code code)
                       retpath)))
         (email (get-email code)))

    (cond
      ((and mailgun:*domain*
            mailgun:*api-key*)
       (log:debug "Sending login code to" email)
       
       (mailgun:send ("Ultralisp <noreply@ultralisp.org>"
                      email
                      "The code to log into the Ultralisp.org")
         (:p ("To log into [Ultralisp.org](~A), follow [this link](~A)"
              url
              url))
         (:p "Hurry up! Link is will expire in one hour.")))
      
      (t (log:warn "You didn't set MAILGUN_DOMAIN and MAILGUN_API_KEY env variables. So I unable to send auth code."
                   url)
         (weblocks/response:redirect url)))))
