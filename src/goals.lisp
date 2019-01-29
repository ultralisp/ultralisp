(defpackage #:ultralisp/goals
  (:use #:cl)
  (:import-from #:weblocks/session)
  (:import-from #:log4cl)
  
  (:import-from #:weblocks/response
                #:send-script)
  (:import-from #:weblocks/hooks
                #:on-application-hook-render)
  (:export
   #:reach-goal))
(in-package ultralisp/goals)


(defun reach-goal (name &key survive-redirect-p)
  (if survive-redirect-p
      ;; Если отметить достижение цели надо будет лишь после редиректа,
      ;; то добавим её в специальный список внутри сессии
      (push name
            (weblocks/session:get-value :skazorama-goals))
      
      (send-script
       `(reach-goal ,name))))


(on-application-hook-render
  send-delayed-goals ()

  "Отправим события, которые были отложены до следующего рендеринга страницы."
  (loop for name in (weblocks/session:get-value :skazorama-goals)
        do (send-script
            `(reach-goal ,name)))

  (weblocks/session:delete-value :skazorama-goals))
