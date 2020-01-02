(defpackage #:ultralisp/widgets/search
  (:use #:cl)
  (:import-from #:3bmd)
  (:import-from #:weblocks-lass)
  (:import-from #:weblocks/request)
  (:import-from #:weblocks/page)
  (:import-from #:weblocks/widget
                #:render
                #:defwidget)
  (:import-from #:ultralisp/search
                #:search-objects)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:cl-ppcre
                #:regex-replace-all)
  (:import-from #:weblocks/dependencies
                #:get-dependencies)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:rutils
                #:fmt
                #:take)
  (:export
   #:make-search-page))
(in-package ultralisp/widgets/search)


(defwidget search-results ()
  ())


(defun make-search-page ()
  (make-instance 'search-results))


(defun to-html (doc)
  "In given text replaces `items' with `items` and
   render it as Markdown to HTML.

   From text:

   \"Test `with` foo `bar' and 'baz` `like' blah\"

   it first will make:

   \"Test `with` foo `bar` and 'baz` `like` blah\"

   Returns a string with HTML."
  (let ((replaced (regex-replace-all
                   "`([^`]+?)'"
                   doc
                   "`\\1`")))
    (with-output-to-string (s)
      (3bmd:parse-string-and-print-to-stream replaced s))))


(defgeneric render-item (type name doc &rest rest)
  (:method (type name doc &key arguments system project package original-package)
    (with-html
      (:li (:span :class "name"
                  ("~:@(~A:~A~)" package name))
           (when arguments
             (:span :class "args" arguments))
           (:span :class "type" type)
           (:div :class "doc"
                 (:raw (to-html doc)))
           (when project
             (:div :class "item-footer project"
                   (:label "project:")
                   (:a :href (fmt "/projects/~A" project)
                       project)))
           (when system
             (:div :class "item-footer system"
                   (:label "system:")
                   (:span system)))
           (when original-package
             (:div :class "item-footer package"
                   (:label "original-package:")
                   (:span original-package)))))))


(defun to-uppercased-symbols (item)
  (append (take 3 item) ;; First 3 items are type name doc
          ;; Rest items are keyword arguments
          (loop for (key value) on (cdddr item) by #'cddr
                appending (list (make-keyword (string-upcase key))
                                value))))


(defmethod render ((widget search-results))
  (let ((query (weblocks/request:get-parameter "query")))
    (setf (weblocks/page:get-title)
          (fmt "Search results for \"~A\"" query))
    (handler-case
        (multiple-value-bind (results total)
            (search-objects query)
          (with-html
            (cond
              (results
               (:ul :class "search-results"
                    (loop for item in results
                          for uppercased = (to-uppercased-symbols item)
                          do (apply #'render-item
                                    uppercased)))
               (when (> total (length results))
                 (:p ("And ~A more"
                      (- total (length results))))))
              (t
               (:p ("No results for \"~A\"" query))))))
      (ultralisp/search:bad-query ()
        (with-html
          (:p ("Unable to parse \"~A\"" query)))))))


(defmethod get-dependencies ((widget search-results))
  (append
   (list
    (weblocks-lass:make-dependency
      `(.search-results
        :margin 0
        (li :list-style-type none
            :margin-bottom 1em
            (.name :font-weight bold)
            (.type :margin-left 0.7em
                   :color gray)
            (.args :margin-left 0.7em
                   :color gray)
            (.doc
             ((:and p :last-child) :margin-bottom 0))
            (.item-footer
             :display inline-block
             :font-size 0.75em
             (label :display inline-block))))))
   (call-next-method)))


(defun load-systems-dependencies ()
  (let* ((systems-before '("asdf" "uiop"))
         (loaded-system-name (pathname-name asd-path))
         (asdf/system-registry:*registered-systems*
           (copy-hash-table-partially
            asdf/system-registry:*registered-systems*
            :keys systems-before)))
    (tagbody
     asd-loader
       (handler-case (asdf:load-asd asd-path)
         (asdf:missing-dependency (condition)
           (let ((missing-system-name (asdf/find-component:missing-requires condition)))
             #+quicklisp
             (progn
               ;; We need this to process dependencies from :defsystem-depends-on
               ;; argument of the `defsystem', like
               (log:info "Loading a missing dependency" missing-system-name)
               (ql:quickload missing-system-name)
               (log:info "Restarting to load asd file again" asd-path)
               (go asd-loader))
             #-quicklisp
             (log:error "Unable to a missing dependency because quicklisp is unavailable" missing-system-name)))))
     
    (flet ((was-loaded-before (system-name)
             (member system-name
                     systems-before
                     :test #'string-equal)))
      (log:debug "Collecting dependencies" asd-path)
      (let ((dependencies (loop for system-name in (remove-if #'was-loaded-before (asdf:registered-systems))
                                for primary-name = (asdf:primary-system-name system-name)
                                when (string-equal primary-name
                                                   loaded-system-name)
                                  do (log:info "Dependencies for" system-name "are collected")
                                  and appending (get-external-dependencies system-name))))
        (log:debug "Dependencies are collected")
        (sort (remove-duplicates dependencies
                                 :test #'equal)
              #'string-lessp
              :key #'first)))))


(defun index-project (project)
  (let* ((tmp-dir "/tmp/checker")
         (downloaded (download project tmp-dir :latest t))
         (path (downloaded-project-path downloaded)))
    
    (unwind-protect
         (prog1
             (let ((systems (ultralisp/models/project:get-systems-info project)))
               (load-systems-dependencies systems)
               (let ((packages (load-systems-and-collect-packages systems)))
                 (loop for package in packages
                       do (index)))))
      ;; Here we need to make a clean up to not clutter the file system
      (log:info "Deleting checked out" path)
      (delete-directory-tree path
                             :validate t))))
