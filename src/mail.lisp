(uiop:define-package #:ultralisp/mail
  (:use #:cl)
  (:import-from #:reblocks-auth/providers/email/resend))
(in-package #:ultralisp/mail)


(reblocks-auth/providers/email/resend:define-code-sender send-code ("Ultralisp.org <noreply@ultralisp.org>" url)
  (:p ("To log into [Ultralisp.org](~A), follow [this link](~A)."
       url
       url))
  (:p "Hurry up! This link will expire in one hour."))
