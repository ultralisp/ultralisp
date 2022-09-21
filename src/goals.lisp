(defpackage #:ultralisp/goals
  (:use #:cl)
  (:import-from #:reblocks/session)
  (:import-from #:log4cl)
  
  (:import-from #:reblocks/response
                #:send-script)
  (:import-from #:reblocks/hooks
                #:on-application-hook-render)
  (:export
   #:reach-goal))
(in-package #:ultralisp/goals)


(defun reach-goal (name &key survive-redirect-p)
  (if survive-redirect-p
      ;; Если отметить достижение цели надо будет лишь после редиректа,
      ;; то добавим её в специальный список внутри сессии
      (push name
            (reblocks/session:get-value :skazorama-goals))
      
      (send-script
       `(reach-goal ,name))))


(on-application-hook-render
  send-delayed-goals ()

  "Отправим события, которые были отложены до следующего рендеринга страницы."
  (loop for name in (reblocks/session:get-value :skazorama-goals)
        do (send-script
            `(reach-goal ,name)))

  (reblocks/session:delete-value :skazorama-goals))
