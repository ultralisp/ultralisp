(defpackage #:ultralisp/widgets/project
  (:use #:cl)
  (:import-from #:ultralisp/models/project
                #:get-external-url
                #:get-description
                #:get-versions
                #:enable-project
                #:disable-project
                #:get-github-project
                #:is-enabled-p)
  (:import-from #:cl-ppcre
                #:register-groups-bind)
  (:import-from #:weblocks/widget
                #:defwidget
                #:render)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:weblocks/page
                #:get-title)
  (:import-from #:ultralisp/widgets/not-found
                #:page-not-found)
  (:import-from #:ultralisp/widgets/utils
                #:render-switch)
  (:import-from #:weblocks-auth/models
                #:get-current-user)
  (:import-from #:ultralisp/models/moderator
                #:is-moderator-p)
  (:import-from #:ultralisp/models/action
                #:get-project-actions)
  (:import-from #:ultralisp/models/check
                #:get-project-checks)
  (:import-from #:ultralisp/cron
                #:get-time-of-the-next-check)
  (:export
   #:make-project-widget))
(in-package ultralisp/widgets/project)


(defwidget project ()
  ())


(defun make-project-widget ()
  (make-instance 'project))


(defun render-action (action)
  (with-html
    (:li "FOO")))


(defun toggle (widget project)
  (check-type widget project)
  (check-type project ultralisp/models/project:project)
  (if (is-enabled-p project)
      (disable-project project :reason :manual)
      (ultralisp/models/check:make-added-project-check project))
  (weblocks/widget:update widget))


(defclass next-check ()
  ((at :initarg :at
       :reader get-time)))


(defmethod ultralisp/widgets/changelog:render-object ((obj next-check) &key timestamp)
  (with-html
    (:li ("~@[~A - ~]Planned check"
          (when timestamp
            (ultralisp/utils:format-timestamp (get-time obj)))))))


(defun render-project (widget project author-name project-name)
  (check-type project ultralisp/models/project:project)
  
  (let* (;; (actions (get-project-actions project))
         ;; TODO: Optimize get-version
         ;; seconds  |     gc     |   consed  | calls |  sec/call  |  name
         ;; -------------------------------------------------------
         ;;      0.190 |      0.000 | 3,454,016 |     1 |   0.189999 | GET-VERSIONS
         ;;      0.080 |      0.000 |   776,720 |     1 |   0.079999 | GET-PROJECT-ACTIONS
         ;;      0.040 |      0.000 |   410,864 |     3 |   0.013327 | RENDER
         ;;      0.020 |      0.000 |   275,248 |     1 |   0.019994 | RENDER-PROJECT
         ;;      0.000 |      0.000 |         0 |     1 |   0.000000 | GET-DESCRIPTION
         ;;      0.000 |      0.000 |         0 |     1 |   0.000000 | GET-EXTERNAL-URL
         ;;      0.000 |      0.000 |   163,824 |     1 |   0.000000 | GET-PROJECT-CHECKS
         ;; -------------------------------------------------------
         ;;      0.330 |      0.000 | 5,080,672 |     9 |            | Total
         ;; (versions (get-versions project))
         (description (get-description project))
         (url (get-external-url project))
         (next-check-at (get-time-of-the-next-check project))
         ;; (checks (get-project-checks project :pending t))
         (current-user-is-moderator
           (is-moderator-p project
                           (get-current-user)))
         (not-moderator
           (not current-user-is-moderator))
         (changelog (sort nil
                          ;; (append actions
                          ;;         versions
                          ;;         checks)
                          #'local-time:timestamp>
                          ;; We want last actions came first
                          :key #'mito:object-updated-at))
         (is-enabled (is-enabled-p project))
         (reason (unless is-enabled
                   (ultralisp/models/project:get-disable-reason project)))
         (reason-description
           (when reason
             (case reason
               (:manual "Turned off manually")
               (:system-conflict "System conflict")
               (t (format nil "~A" reason))))))
    (setf (get-title)
          (format nil "~A/~A – ~A"
                  author-name
                  project-name
                  description))


    (with-html
      ;; Show a list of versions where it was included
      (:h1 :class "full-name"
           (:a :class "author-name"
               :href (format nil "/projects/~A" author-name)
               author-name)
           "/"
           (:span :class "project-name"
                  project-name)
           (render-switch is-enabled
                          (lambda (&rest args)
                            (declare (ignorable args))
                            (toggle widget project))
                          :disabled not-moderator
                          :title (when not-moderator
                                   "You are not a moderator of this project"))
           (when reason-description
             (:span :class "disable-reason"
                    reason-description)))
      (:h2 :class "project-description"
           description)
      (:p (:a :href url
              "View project on GitHub"))
       
      (ultralisp/widgets/changelog:render (cons (make-instance 'next-check
                                                               :at next-check-at)
                                                changelog)
                                          :timestamps t))))


(defmethod render ((widget project))
  (register-groups-bind (user-or-org project-name)
      ("^/projects/(.*)/(.*)$" (weblocks/request:get-path))
    ;; This is not an idiomatic Weblocks code because we should
    ;; make a database query only when widget gets created, not
    ;; during the render.
    (let ((project (get-github-project user-or-org
                                       project-name)))
      (cond
        (project (render-project widget project user-or-org project-name))
        (t (page-not-found))))))


(defmethod weblocks/dependencies:get-dependencies ((widget project))
  (append
   (list
    (weblocks-lass:make-dependency
      `((:and .widget .project)
        (.full-name
         :margin-bottom 0
         (.author-name
          :color "black")
         (.project-name :margin-right 0.5em))
        (.project-description
         :font-size 1.5em)
        (.disable-reason
         :position relative
         :font-size 0.3em
         :top -0.4em))))
   (call-next-method)))
