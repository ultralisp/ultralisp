(defpackage #:ultralisp/search
  (:use #:cl)
  (:import-from #:log)
  (:import-from #:mito)
  (:import-from #:str)
  (:import-from #:quri)
  (:import-from #:quickdist)
  (:import-from #:quicklisp-client)
  (:import-from #:dexador)
  (:import-from #:jonathan)
  (:import-from #:closer-mop)
  (:import-from #:qlot)
  (:import-from #:qlot/install)
  (:import-from #:ultralisp/db
                #:with-transaction)
  (:import-from #:rutils
                #:fmt)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:ultralisp/downloader/base
                #:downloaded-project-path
                #:download)
  (:import-from #:ultralisp/models/project
                #:ensure-project
                #:project-name
                #:get-projects-with-sources
                #:get-project2
                #:project-sources
                #:project2
                #:get-recently-updated-projects
                #:get-all-projects
                #:get-github-project)
  (:import-from #:ultralisp/packages-extractor-api
                #:with-saved-ultralisp-root
                #:get-packages)
  (:import-from #:ultralisp/rpc/core
                #:submit-task)
  (:import-from #:log4cl-extras/context
                #:with-fields)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:ultralisp/models/index
                #:get-index-status
                #:set-index-status
                #:get-projects-to-index
                #:reschedule-indexing)
  (:import-from #:ultralisp/variables
                #:get-elastic-host)
  (:import-from #:local-time
                #:timestamp-difference
                #:now)
  (:import-from #:ultralisp/models/source
                #:source-systems-info)
  (:import-from #:ultralisp/rpc/command
                #:defcommand)
  (:import-from #:trivial-timeout
                #:timeout-error
                #:with-timeout)
  (:import-from #:global-vars
                #:define-global-parameter)
  (:export
   #:search-objects
   #:bad-query
   #:index-projects
   #:delete-project-documents
   #:delete-documents-which-should-not-be-in-the-index))
(in-package #:ultralisp/search)


(defvar *current-system-name* nil)
(defvar *current-package-name* nil)
(defvar *current-project-name* nil)
(defvar *current-source-uri* nil)
(defvar *current-system-path* nil)

(define-global-parameter *indexing-timeout* (* 5 60))


(defun index (collection id data)
  (let ((content (jonathan:to-json data))
        (url (fmt "http://~A:9200/~A/_doc/~A"
                  (get-elastic-host)
                  collection
                  (quri:url-encode id))))
    (log:info "Sending data to Elastic Search" collection id)
    (jonathan:parse
     (dex:put url
              :content content
              :headers '(("Content-Type" . "application/json"))))))


(defun delete-index ()
  (let ((url (fmt "http://~A:9200/symbols"
                  (get-elastic-host))))
    (log:info "Deleting index")
    (jonathan:parse
     (dex:delete url :headers '(("Content-Type" . "application/json"))))))


(defun delete-from-index (doc-id)
  (with-fields (:document-id doc-id)
    (let ((url (fmt "http://~A:9200/symbols/_doc/~A"
                    (get-elastic-host)
                    (quri:url-encode doc-id))))
      (log:info "Deleting document from index")
      (jonathan:parse
       (dex:delete url :headers '(("Content-Type" . "application/json")))))))


(define-condition bad-query (error)
  ((original-error :initarg :original-error
                   :reader get-original-error)))


(defun search-objects (term &key (from 0))
  ;; TODO: научиться обрабатывать 400 ответы от Elastic
  ;; например на запрос: TYPE:macro AND storage NAME:FLEXI-STREAMS:WITH-OUTPUT-TO-SEQUENCE
  (handler-case
      (loop with url = (fmt "http://~A:9200/symbols/_search"
                            (get-elastic-host))
            with query = (list
                          :|query| (list
                                    :|query_string|
                                    (list :|fields|
                                          (list "documentation" "symbol" "package")
                                          :|query| term))
                          :|from| from)
            with content = (jonathan:to-json query)
            with body = (dex:post url
                                  :content content
                                  :headers '(("Content-Type" . "application/json")))
            with response = (jonathan:parse body)
            with total = (getf (getf (getf response
                                           :|hits|)
                                     :|total|)
                               :|value|)
            for hit in (getf (getf response
                                   :|hits|)
                             :|hits|)
            for id = (getf hit :|_id|)
            for source = (getf hit :|_source|)
            for doc = (getf source :|documentation|)
            for type = (getf source :|type|)
            for symbol = (getf source :|symbol|)
            for system = (getf source :|system|)
            for system-path = (getf source :|system-path|)
            for project = (getf source :|project|)
            for project-source = (getf source :|source|)
            ;; This is a symbol's package:
            for package = (getf source :|package|)
            ;; And this is a package where symbol
            ;; was exported from:
            for original-package = (getf source :|original-package|)
            collect (list (make-keyword type)
                          symbol
                          doc
                          :id id
                          :arguments (getf source :|arguments|)
                          :project project
                          :source project-source
                          :package package
                          :original-package original-package
                          :system-path system-path
                          :system system) into results
            finally (return (values results
                                    total
                                    (when (< (+ from (length results))
                                             total)
                                      (let ((new-from (+ from (length results))))
                                        (lambda ()
                                          (search-objects term
                                                          :from new-from)))))))
    (dexador.error:http-request-not-found ()
      (values nil 0 nil))
    (dexador.error:http-request-bad-request (condition)
      (error 'bad-query :original-error condition))))


(defun symbol-function-type (symbol)
  (cond
    ((macro-function symbol)
     'macro)
    ((fboundp symbol)
     (type-of (fdefinition symbol)))))


(defun encode-symbol (symbol &optional (ftm-string "~a"))
  "Encode the symbol as a string, including the package.  If the value
is a string then just return the string."
  (let ((sym-string
          (etypecase symbol
            (string symbol)
            (symbol
             (let ((symbol-package (symbol-package symbol)))
               (concatenate
                'string
                (if symbol-package
                    (package-name symbol-package)
                    "")
                (cond
                  ((null symbol-package)
                   "#:")
                  ((multiple-value-bind
                         (sym status)
                       (find-symbol (symbol-name symbol)
                                    (package-name symbol-package))
                     (declare (ignore sym))
                     (member status '(:inherited :external)))
                   ":")
                  (t "::"))
                (symbol-name symbol))))
            ;; TODO: придумать что-то с type-specifier которые не являются
            ;;       символами а являются объектами
            (t symbol))))
    (format nil ftm-string sym-string)))


(defun is-external (symbol package)
  (multiple-value-bind
        (sym status)
      (find-symbol (symbol-name symbol) package)
    (declare (ignore sym))
    (eql status :external)))


(defun simplify-arglist (arglist)
  "return the first element of each list item this is to remove the
default values from the arglist."
  (mapcar (lambda (e)
            (cond
              ((listp e)
               (car e))
              (t e)))
          arglist))


(defun encode-xref (symbol)
  (encode-symbol symbol ":cl:symbol:`~~~a`"))


(defun encode-literal (symbol)
  (if (equal (symbol-package symbol) (find-package 'keyword))
      (format nil "``~s``" symbol)
      (format nil "``~a``" symbol)))


(defun prepare-text (text)
  "Removes spaces from left and right of each line, then joins
   lines not separated by an empty line into single paragraphs."
  (with-output-to-string (output)
    (loop with add-space = nil
          with some-lines-written = nil
          for line in (rutils:split #\Newline text)
          for stripped = (string-trim '(#\Space)
                                      line)
          when add-space
            do (write-char #\Space output)
          when (> (length stripped) 0)
            do (write-string stripped output)
               (setf add-space t
                     some-lines-written t)
          else
            when some-lines-written
              do (write-char #\Newline output)
                 (write-char #\Newline output)
                 (setf add-space nil
                       some-lines-written nil))))

(defun arglist-as-string (symbol)
  (let ((*package* (symbol-package symbol))
        (*print-case* :downcase))
    (format nil "~:S"
            (arglist symbol))))

(defun get-common-documentation* (symbol)
  (append
   (when *current-system-name*
     (list :|system| (string-downcase *current-system-name*)))

   (when *current-system-path*
     (list :|system-path| (format nil "~A" *current-system-path*)))
   
   (when *current-project-name*
     (list :|project| (string-downcase *current-project-name*)))

   (when *current-source-uri*
     (list :|source| (string-downcase *current-source-uri*)))
   
   (when *current-package-name*
     (list :|package| (string-downcase *current-package-name*)))
   
   (list
    :|original-package| (string-downcase
                         (package-name (symbol-package symbol)))
    :|symbol| (string-downcase (symbol-name symbol)))))


(defun get-function-documentation* (symbol type doc)
  (append
   (get-common-documentation* symbol)
   
   (list :|type| type
         :|documentation| (prepare-text doc)
         :|arguments| (arglist-as-string symbol))))


(defun encode-class (symbol)
  "Encode a class as a string including the package."
  (encode-symbol (class-name symbol)))


(defun encode-specializer (atom)
  "encode a single specializer lambda list"
  (cond ((eq (type-of atom) 'closer-mop:eql-specializer)
         (concatenate 'string
                      "(EQ " (encode-symbol
                              (closer-mop:eql-specializer-object atom)) ")"))
        ((closer-mop:classp atom)
         (encode-class atom))
        (t (encode-symbol atom))))


(defun get-methods (generic-function)
  (loop with methods = (closer-mop:generic-function-methods generic-function)
        for method in methods
        for specializers = (closer-mop:method-specializers method)
        for lambda-list = (closer-mop:method-lambda-list method)
        collect (list
                 (fmt "~A"
                      (mapcar #'encode-specializer specializers))
                 (or (documentation method t) ""))))


(defgeneric get-function-documentation (symbol type)
  (:documentation
   "Encode documentation for a function or macro as a JSON object member."))

(defmethod get-function-documentation (symbol type)
  (log:error "Unsupported function type" symbol type)
  (get-function-documentation*
   symbol type (or (documentation symbol 'function) "")))

(defmethod get-function-documentation (symbol (type (eql 'function)))
  (get-function-documentation*
   symbol type (or (documentation symbol type) "")))

;; CLISP-ism (might be other CL's as well): compiled functions are still
;; functions:
(defmethod get-function-documentation (symbol (type (eql 'compiled-function)))
  (get-function-documentation*
   symbol 'function (or (documentation symbol 'function) "")))

(defmethod get-function-documentation (symbol (type (eql 'macro)))
  (get-function-documentation*
   symbol type (or (documentation symbol 'function) "")))

(defmethod get-function-documentation (symbol (type (eql 'generic-function)))
  (append
   (get-function-documentation*
    symbol type (or (documentation symbol 'function) ""))
   (list :methods
         (get-methods (symbol-function symbol)))))

(defmethod get-function-documentation (symbol (type (eql 'cl:standard-generic-function)))
  (get-function-documentation symbol 'generic-function))

#+continuations
(defmethod get-function-documentation (symbol (type (eql 'cl-cont::funcallable/cc)))
  (get-function-documentation symbol 'function))


(defun class-p (symbol)
  "Return T if the symbol is a class."
  #+sbcl (eql (sb-int:info :type :kind symbol) :instance)
  #-sbcl (find-class symbol nil))


(defun variable-p (symbol)
  "Return T if the symbol is a bound variable."
  (and #+sbcl (sb-int:info :variable :kind symbol)
       (boundp symbol)))


(defun get-variable-documentation* (symbol type)
  (append
   (get-common-documentation* symbol)
   (list
    :|type| type
    :|documentation| (prepare-text (or (documentation symbol type) "")))))


(defun arglist (symbol)
  #+sbcl  (sb-introspect:function-lambda-list symbol)
  #+clisp (sys::arglist symbol)
  #+ccl (ccl:arglist symbol)
  #+ecl   (ext:function-lambda-list symbol)
  #-(or sbcl clisp ccl ecl) (error "arglist not available for this Lisp."))


(defmethod get-value-documentation (symbol (type (eql 'variable)))
  (get-variable-documentation* symbol type))

(defmethod get-value-documentation (symbol (type (eql 'setf)))
  (get-function-documentation* symbol type (or (documentation symbol 'setf) "")))

(defun get-class-slots (sym)
  (loop for slot in (closer-mop:class-direct-slots (find-class sym))
        collect (list
                 :name (closer-mop:slot-definition-name slot)
                 :initarg (write-to-string (closer-mop:slot-definition-initargs slot))
                 :readers (write-to-string (closer-mop:slot-definition-readers slot))
                 :writers (car (closer-mop:slot-definition-writers slot))
                 :type (class-name (class-of slot))
                 :documentation (or (documentation slot t) ""))))

(defmethod get-value-documentation (symbol (type (eql 'type)))
  (list*
   :|slots| (get-class-slots symbol)
   (get-variable-documentation* symbol type)))


(defun index-symbols (package)
  "
  Returns number of indexed symbols or 0.
  "
  ;; TODO: remove current-system-path (don't remember why :()
  (let* ((*current-system-path* (ql:where-is-system *current-system-name*))
         (current-package (etypecase package
                            (string (find-package (string-upcase package)))
                            (keyword (find-package package))
                            (package package)))
         (*current-package-name* (when current-package
                                   (package-name current-package)))
         (indexed-symbols 0))
    (cond
      (current-package
       (do-external-symbols (symbol current-package)
         (let ((external (is-external symbol current-package)))
           (when external
             (let* ((full-symbol-name (encode-symbol symbol))
                    (object-id (cond (*current-source-uri*
                                      (fmt "src:~A:~A" *current-source-uri* full-symbol-name))
                                     (*current-project-name*
                                      (fmt "prj:~A:~A" *current-project-name* full-symbol-name))
                                     (t
                                      full-symbol-name)))
                    (indexed nil))
               (when (symbol-function-type symbol)
                 (index "symbols"
                        object-id
                        (get-function-documentation symbol
                                                    (symbol-function-type symbol)))
                 (setf indexed t))
             
               (when (class-p symbol)
                 (index "symbols"
                        object-id
                        (get-value-documentation symbol 'type))
                 (setf indexed t))
             
               (when (variable-p symbol)
                 (index "symbols"
                        object-id
                        (get-value-documentation symbol 'variable))
                 (setf indexed t))

               (when indexed
                 (incf indexed-symbols)))))))
      (t (log:error "Unable to find package" package)))

    (values indexed-symbols)))


(defun index-all-packages ()
  "Временная функция для тестирования индесатора"
  (mapc #'index-symbols (list-all-packages)))


(defun safe-quickload (system)
  (flet ((abort-if-possible (c)
           (let ((restart (find-restart 'abort)))
             (when restart
               (log:warn "Aborting condition" c)
               (invoke-restart restart))))
         (continue-if-possible (c)
           (let ((restart (find-restart 'continue)))
             (when restart
               (log:warn "Continuing on condition" c)
               (invoke-restart restart))))
         (shadow-import-if-possible (c)
           #+sbcl
           (let ((restart (find-restart 'sb-impl::shadowing-import-it)))
             (when restart
               (log:warn "Shadowing import on condition" c)
               (invoke-restart restart)))))
    ;; In case of errors for which we don't have a special workaround,
    ;; we want this function to return nil. Such system will not be indexed,
    ;; but also it will not stop processing of other systems.
    (ignore-errors
     (with-log-unhandled ()
       (handler-bind ((simple-error
                        (lambda (c)
                          (let ((message (simple-condition-format-control c)))
                            (when (str:starts-with-p
                                   "Dependency looping"
                                   message)
                              (abort-if-possible c))
                            (when (search "overwriting old FUN-INFO" message)
                              (continue-if-possible c)))))
                      #+sbcl
                      (sb-ext:name-conflict #'shadow-import-if-possible)
                      #+sbcl
                      (sb-ext:package-locked-error
                        #'continue-if-possible)
                      (quicklisp-client:system-not-found
                        #'abort-if-possible))
         (ql:quickload system))))))


(defun index-source (project source &key (clear-dir t))
  "
  This function should be called inside the worker.

  Returns a number of indexed symbols.
  "
  (check-type project project2)
  (check-type source ultralisp/models/source:source)
  
  (let* ((path (downloaded-project-path
                (download source "/tmp/indexer" :latest t)))
         (qlfile-path (merge-pathnames #P"qlfile"
                                       path))
         (indexed-symbols 0))

    (unwind-protect
         (uiop:with-current-directory (path)
           (log:info "Working in" path)
           ;; Prepare qlfile and install the dependencies
           (cond
             ;; If qlfile already exists, we'll use it
             ((probe-file qlfile-path)
              nil)
             ;; Otherwise we'll create a new one
             (t
              (alexandria:with-output-to-file (s qlfile-path
                                                 :if-exists :supersede)
                (log:info "Creating a new qlfile")
                (format s "dist ultralisp http://dist.ultralisp.org/"))

              ;; Just in case, we'll remove lock file:
              (let ((lock-file (probe-file (merge-pathnames #P"qlfile.lock"
                                                            path))))
                (log:info "Deleting old qlfile.lock")
                (when lock-file
                  (delete-file lock-file)))))

           ;; This will create .qlot/ folder out of qlfile
           (qlot/install:install-project path :install-deps nil)

           (log:info "Entering local quicklisp")
           (qlot:with-local-quicklisp (path)
             (loop with systems = (source-systems-info source)
                   with *current-project-name* = (ultralisp/models/project:project-name project)
                   with *current-source-uri* = (ultralisp/models/source::params-to-string
                                                source
                                                ;; We'll use this string as a document ID and
                                                ;; don't want it to change on every commit.
                                                :last-seen nil)
                   with asdf:*central-registry* = (cons path asdf:*central-registry*)
                   for system in systems
                   do (log:info "Checking system" system)
                      (let ((data (get-packages (quickdist:get-name system)
                                                :work-dir path)))
                        (log:info "Indexing system" system data)
                        (loop for item in data
                              for *current-system-name* = (getf item :system)
                              for packages = (getf item :packages)
                              do (when (safe-quickload *current-system-name*)
                                   (let ((*current-system-path* (ql:where-is-system *current-system-name*)))
                                     (loop for package in packages
                                           for num-symbols-in-package = (index-symbols package)
                                           do (log:info "Indexing package" package)
                                              (incf indexed-symbols
                                                    num-symbols-in-package)))))))))
      ;; Here we need to make a clean up to not clutter the file system
      (when clear-dir
        (log:info "Deleting checked out" path)
        (uiop:delete-directory-tree path
                                    :validate t)))

    (values indexed-symbols)))


(defcommand update-index-status (project status processed-in)
  (with-fields (:project (typecase project
                           (string project)
                           (t
                            (project-name project)))
                :status status
                :processed-in processed-in)
    (log:info "Updating project index status")
    (set-index-status (ensure-project project)
                      status
                      :total-time processed-in)))


(defun index-project (project &key
                                (clear-dir t)
                                (debug nil)
                      &aux (started-at (now)))
  "
  This function should be called inside the worker.

  Returns a number of indexed symbols.
  "
  (flet ((get-total-time ()
           (floor (timestamp-difference
                   (now)
                   started-at))))
    (handler-case
        (handler-bind ((error (lambda (c)
                                (when debug
                                  (invoke-debugger c)))))
            (with-log-unhandled ()
              (with-transaction
                (with-saved-ultralisp-root
                  (let ((project (ensure-project project)))

                    (delete-project-documents project)

                    (log:info "Indexing sources")
                    (loop for source in (project-sources project)
                          summing (index-source project source
                                                :clear-dir clear-dir))

                    (update-index-status project
                                         :ok
                                         (get-total-time))
                    
                    (log:info "Indexing sources DONE"))))))
      (error (condition)
        (log:error "Project was not indexed because of" condition)
        (update-index-status project
                             :failed
                             (get-total-time))))))


(defun sources-changed-since-last-index-p (project)
  (multiple-value-bind (status index-updated-at)
      (get-index-status project)
    (case status
      (:ok
       (let* ((sources (project-sources project)))
         (loop for source in sources
               for source-updated-at = (mito:object-updated-at source)
                 thereis (local-time:timestamp> source-updated-at
                                                index-updated-at))))
      (otherwise
       ;; For all other statuses we want to retry indexing again and again.
       ;; Probably at some moment the problem will be noticed by
       ;; Ultralisp maintainer and fixed.
       t))))


(defun index-projects (&key names force (limit 10))
  (let ((projects (cond
                    (names
                     (mapcar #'get-project2 names))
                    ;; Reindexing all projects
                    (force (let ((all (get-projects-with-sources)))
                             (if limit
                                 (rutils:take limit
                                              all)
                                 all)))
                    ;; 
                    (t (get-projects-to-index :limit limit)))))
    (loop for project in projects
          for project-name = (project-name project)
          do (with-fields (:project-name project-name)
               (with-log-unhandled ()
                 (handler-case
                     (cond
                       ((sources-changed-since-last-index-p project)
                        (log:info "Sending task to index project ~A" project-name)
                        (with-timeout (*indexing-timeout*)
                          (submit-task
                           'index-project
                           :args (list project))))
                       (t
                        (log:info "We don't need to reindex this project because shources not changed")
                        (reschedule-indexing project)))
                   (timeout-error ()
                     (log:error "Project was not indexed because of timeout")
                     (update-index-status project
                                          :timeout
                                          *indexing-timeout*))))
               (log:info "Done with indexing ~A"
                         project-name)))
    (log:info "All projects were indexed")))


(defmacro do-all-docs ((doc-symbol query) &body body)
  `(loop for (documents total get-next) = (multiple-value-list
                                           (search-objects ,query))
           then (multiple-value-list
                 (funcall get-next))
         do (dolist (,doc-symbol documents)
              ,@body)
         while get-next))


(defun delete-project-documents (project)
  (let* ((project (ensure-project project))
         (project-name (project-name project)))
    (do-all-docs (document (fmt "project:\"~A\""
                                project-name))
      (let* ((doc-plist (cdddr document))
             (doc-id (getf doc-plist :id)))
        (delete-from-index doc-id)))))


(defun delete-documents-which-should-not-be-in-the-index ()
  (let* ((projects-to-index (get-projects-with-sources))
         (project-names-to-index (mapcar #'project-name
                                         projects-to-index)))
    (do-all-docs (document "*")
      (let* ((doc-plist (cdddr document))
             (doc-id (getf doc-plist :id))
             (project-name (getf doc-plist :project)))
        (assert project-name)
        (unless (member project-name
                        project-names-to-index
                        :test #'string-equal)
          (with-fields (:project project-name
                        :doc-id doc-id)
            (log:info "Deleting document from index")
            (delete-from-index doc-id)))))))
