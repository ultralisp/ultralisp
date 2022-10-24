(uiop:define-package #:ultralisp/api/server
  (:use #:cl)
  (:import-from #:jsonrpc)
  (:import-from #:ultralisp/search)
  (:import-from #:reblocks/session)
  (:import-from #:jsonrpc/transport/websocket
                #:websocket-transport)
  (:import-from #:jsonrpc/class
                #:bind-server-to-transport)
  (:import-from #:alexandria
                #:length=
                #:copy-hash-table
                #:make-keyword
                #:symbolicate)
  (:import-from #:lambda-fiddle
                #:with-destructured-lambda-list)
  (:import-from #:jsonrpc/transport/http
                #:http-transport)
  (:import-from #:serapeum
                #:dict))
(in-package #:ultralisp/api/server)


(defvar *default-page-size* 2)

(defvar *server* nil)


(defvar *methods* (make-hash-table :test 'equal)
  "This hash keeping all methods.")

(defvar *method-info* (make-hash-table :test 'equal)
  "Hash to keep additional information about RPC method signatures.")


(defclass parameter ()
  ((name :initarg :name
         :initform nil
         :reader parameter-name)
   (type :initarg :type
         :initform nil
         :reader parameter-type)
   (required :initarg :required
             :initform nil
             :accessor parameter-required)))


(defmethod print-object ((obj parameter) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~S ~S~A"
            (parameter-name obj)
            (parameter-type obj)
            (if (parameter-required obj)
                " required"
                ""))))


(defclass result ()
  ((type :initarg :type
         :reader result-type)))

(defmethod print-object ((obj result) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~S"
            (result-type obj))))


(defclass method-info ()
  ((params :initarg :params
           :reader method-params)
   (result :initarg :result
           :reader method-result)))

(defmethod print-object ((obj method-info) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "(~{~S~^, ~}) -> ~S"
            (method-params obj)
            (method-result obj))))


(defun make-lambda-list (required-args optional-args keyword-args)
  (append required-args
          (when optional-args
            (list* '&optional
                   optional-args))
          (when keyword-args
            (list* '&key
                   keyword-args))))


(defun sort-params (params required-args optional-args keyword-args)
  "Accepts PARAMS list and reorders it such that first go required args, then optional and finally keyword."

  (let* ((simplified-optional-args (mapcar #'simplify-arg optional-args))
         (simplified-keyword-args (mapcar #'simplify-arg keyword-args))
         (sorted-params
           (sort (copy-list params) #'<
                 :key (lambda (param)
                        (let ((name (parameter-name param)))
                          (cond
                            ((member name required-args)
                             1)
                            ((member name simplified-optional-args)
                             2)
                            ((member name simplified-keyword-args)
                             3)
                            (t
                             (error "Parameter ~S not found among method arguments: ~S"
                                    name
                                    (make-lambda-list required-args optional-args keyword-args)))))))))
    (let ((not-documented
            (set-difference (append required-args
                                    simplified-optional-args
                                    simplified-keyword-args)
                            (mapcar #'parameter-name sorted-params))))
      (when not-documented
        (error "Some parameters of function are not documented: ~{~S~^, ~}"
               not-documented)))
    sorted-params))


(defun separate-method-info-forms (body)
  (loop with params = nil
        with result = nil
        with rest-body = nil
        for form in body
        for info-form-p = (and (consp form)
                               (keywordp (first form))
                               (member (first form)
                                       '(:param :result)))
        if info-form-p
        collect form into method-info-forms
        else
        collect form into new-body
        finally (return (values method-info-forms
                                new-body))))

(defun make-method-info (info-forms required-args optional-args keyword-args)
  (loop with params = nil
        with result = nil
        for form in info-forms
        for form-type = (first form)
        do (ecase form-type
             (:param (push (make-instance 'parameter
                                          :name (second form)
                                          :type (third form))
                           params))
             (:result (setf result
                            (make-instance 'result
                                           :type (second form)))))
        finally (return (let ((sorted-params
                                (sort-params params
                                             required-args
                                             optional-args
                                             keyword-args)))
                          (loop for param in sorted-params
                                when (member (parameter-name param)
                                             required-args)
                                do (setf (parameter-required param)
                                         t))
                          (make-instance 'method-info
                                         :params sorted-params
                                         :result result)))))


(defun simplify-arg (arg)
  (etypecase arg
    (symbol arg)
    (cons (car arg))))


(defun sym-to-string (sym)
  (string-downcase (symbol-name sym)))


(defgeneric transform-result (object)
  (:documentation "Prepares object for serialization before responding to RPC call.

Result should be list, hash-map or a value of primitive type.")
  (:method ((object list))
    (mapcar #'transform-result object)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-call-form (name keyword-arg-keys keyword-arg-names optional-arg-names required-args-getters)
    `(etypecase args
       (hash-table
        ;; If arguments were given in a hash-table, then we need
        ;; to figure out which are positional and which are keywords
        ;; and to transform key names to symbols:
        (let ((new-args
                (append (list ,@required-args-getters)
                        (loop for optional-name in (list ,@optional-arg-names)
                              for (value present-p) = (multiple-value-list
                                                       (gethash optional-name args))
                              if present-p
                              collect value into values
                              else
                              do (return values))
                        (loop for key-name in (list ,@keyword-arg-names)
                              for key-symbol in (list ,@keyword-arg-keys)
                              for (value present-p) = (multiple-value-list
                                                       (gethash key-name args))
                              when present-p
                              append (list key-symbol value)))))
          (apply ',name new-args))
        )
       (list
        (apply ',name args))))

  (defun make-wrapper-form (call-form paginated-result)
    (cond
      (paginated-result
       `(multiple-value-bind (result next-page-key)
            ,call-form
          (let ((response (dict "items" (transform-result result))))
            (when next-page-key
              (setf (gethash "next-page-key" response)
                    next-page-key))
            response)))
      (t
       `(transform-result ,call-form)))))


(defmacro define-rpc-method (name args &body body)
  (with-destructured-lambda-list (:required required-args
                                  :optional optional-args
                                  :key keyword-args)
                                 args
    (multiple-value-bind (info-forms body)
        (separate-method-info-forms body)
      (let* ((paginated-result
               (loop for form in info-forms
                     when (and (eql (first form)
                                    :result)
                               (consp (second form))
                               (string-equal (symbol-name (car (second form)))
                                             "paginated-list-of"))
                     do (return t)))
             (optional-args (mapcar #'simplify-arg optional-args))
             (keyword-args (mapcar #'simplify-arg keyword-args))
             (name-as-string (string-downcase (symbol-name name)))
             (wrapper-name (symbolicate "%RPC-" name))
             (required-args-getters (loop for arg in required-args
                                          for name = (sym-to-string arg)
                                          collect `(multiple-value-bind (value present-p)
                                                       (gethash ,name args)
                                                     (unless present-p
                                                       (error "Argument ~A is required but not supplied by a client."
                                                              ',arg))
                                                     value)))
             (optional-arg-names (mapcar #'sym-to-string optional-args))
             (keyword-arg-names (mapcar #'sym-to-string keyword-args))
             (keyword-arg-keys (mapcar #'make-keyword keyword-args))
             (call-form (make-call-form name keyword-arg-keys keyword-arg-names optional-arg-names required-args-getters))
             (wrapper-form (make-wrapper-form call-form paginated-result)))

        `(flet ((,wrapper-name (args)
                  ,wrapper-form))
           
           (defun ,name ,args
             ,@body)

           (setf (gethash ,name-as-string *methods*)
                 #',wrapper-name)
           
           (setf (gethash ,name-as-string *method-info*)
                 (collect-method-info ',info-forms
                                      ',required-args
                                      ',optional-args
                                      ',keyword-args))
           
           (when *server*
             (jsonrpc:expose *server* ,name-as-string
                             #',wrapper-name)))))))


(defun make-content-descriptor (name &key type reference required (schema nil schema-given-p))
  (let ((result (make-hash-table :test 'equal))
        (schema (if schema
                    (copy-hash-table schema)
                    (make-hash-table :test 'equal))))
    (unless (or type reference schema-given-p)
      (error "Reference or type or schema should be given."))
    
    (cond
      (type
       (setf (gethash "type" schema)
             type))
      (reference
       (setf (gethash "$ref" schema)
             reference)))

    (setf (gethash "name" result)
          name
          (gethash "schema" result)
          schema)

    (when required
      (setf (gethash "required" result)
            t))

    result))


(defun get-params-as-content-descriptors (name)
  (let ((method-info (gethash name *method-info*)))
    (loop for param in (method-params method-info)
          collect (make-content-descriptor (sym-to-string (parameter-name param))
                                           :type (sym-to-string (parameter-type param))
                                           :required (parameter-required param)))))


(defgeneric primitive-type-p (type)
  (:method ((type t))
    nil)
  (:method ((type (eql 'integer)))
    t)
  (:method ((type (eql 'string)))
    t))


(defgeneric type-to-schema (type)
  (:method ((type t))
    (cond
      ((primitive-type-p type)
       (dict "type"
             (string-downcase (symbol-name type))))
      ;; Non paginated results:
      ((and (listp type)
            (symbolp (car type))
            (string-equal (car type)
                          "list-of"))
       (unless (length= 2 type)
         (error "Type definition ~S should have this form (LIST-OF ~A)."
                type
                (or (second type)
                    "SOME-TYPE")))
       (dict "type" "array"
             "items" (type-to-schema (second type))))
      ;; Paginated results:
      ((and (listp type)
            (symbolp (car type))
            (string-equal (car type)
                          "paginated-list-of"))
       (unless (length= 2 type)
         (error "Type definition ~S should have this form (PAGINATED-LIST-OF ~A)."
                type
                (or (second type)
                    "SOME-TYPE")))
       (dict "type" "object"
             "properties" (dict "items" (dict "type" "array"
                                              "items" (type-to-schema (second type)))
                                "next-page-key" (dict "type" "string"))
             "required" (list "items")
             "x-paginated-list" t))
      (t
       (error "Type ~S is not supported. Please, define ~S method for it."
              type
              'type-to-schema)))))


(defmethod type-to-schema ((type (eql 'ultralisp/models/project:project2)))
  (dict "type" "object"
        "properties" (dict "name" (type-to-schema 'string)
                           "description" (type-to-schema 'string))
        "required" (list "name")
        "x-cl-class" (symbol-name type)
        "x-cl-package" (package-name (symbol-package type))))


(defmethod transform-result ((obj ultralisp/models/project:project2))
  (dict "name" (ultralisp/models/project::project-name obj)
        "description" (ultralisp/models/project::project-description obj)))


(defun get-method-result-as-content-descriptor (name)
  (let* ((method-info (gethash name *method-info*))
         (result (method-result method-info))
         (type (result-type result)))
    (make-content-descriptor (concatenate 'string name "-result")
                             :schema (type-to-schema type)
                             ;; :type (sym-to-string (result-type result))
                             )))


(defun generate-methods (mapper)
  (loop for name being the hash-key of mapper
        using (hash-value func)
        collect (let ((method (make-hash-table :test #'equal)))
                  (setf (gethash "name" method)
                        name)
                  (cond
                    ((string= name "rpc.discover")
                     (setf (gethash "params" method)
                           #())
                     (setf (gethash "result" method)
                           (make-content-descriptor "OpenRPC Schema"
                                                    :reference "https://raw.githubusercontent.com/open-rpc/meta-schema/master/schema.json")))
                    (t
                     (setf (gethash "params" method)
                           (get-params-as-content-descriptors name))
                     (setf (gethash "result" method)
                           (get-method-result-as-content-descriptor name))
                     ;; Require to pass arguments as a dictionary.
                     ;; This way we'll be able to process keyword
                     ;; arguments of our methods.
                     (setf (gethash "paramStructure" method)
                           "by-name")))
                  method)))


(defgeneric make-info (server)
  (:method ((server jsonrpc:server))
    (let ((info (make-hash-table :test 'equal)))
      (setf (gethash "title" info)
            "Experimental API")
      (setf (gethash "version" info)
            "0.1.0")
      info)))


(defun rpc-discover (server args)
  (declare (ignore args))
  (let ((response (make-hash-table :test 'equal)))
    (setf (gethash "methods" response)
          (generate-methods (jsonrpc/mapper::exposable-mapper *server*)))
    (setf (gethash "openrpc" response)
          "1.0.0")
    (setf (gethash "info" response)
          (make-info server))
    response))


(define-rpc-method sum-elements (x y &key (multiplier 1))
  (:param x integer)
  (:param y integer)
  (:param multiplier integer)
  (:result integer)
  (* (+ x y)
     multiplier))


(define-rpc-method get-project (name)
  (:param name string)
  (:result (list-of ultralisp/models/project:project2))
  (ultralisp/db:with-connection ()
    (list (ultralisp/models/project:get-project2 name)
          (ultralisp/models/project:get-project2 "40ants/40ants-critic"))))

(define-rpc-method get-projects-by-tag (tag)
  (:param tag string)
  (:result (list-of ultralisp/models/project:project2))
  (ultralisp/db:with-connection ()
    (ultralisp/models/tag::get-projects-by-tag tag)))


(define-rpc-method get-projects-by-tag-paginated (tag &key limit page-key)
  (:param tag string)
  (:param limit integer)
  (:param page-key integer)
  (:result (paginated-list-of ultralisp/models/project:project2))
  (ultralisp/db:with-connection ()
    (ultralisp/models/tag::get-projects-by-tag-paginated tag
                                                         :limit (or limit 2)
                                                         :page-key page-key)))


(defclass search-result ()
  ((type :initarg :type
         :type string
         :reader search-result-type)
   (symbol :initarg :symbol
           :type string
           :reader search-result-symbol)
   (package :initarg :package
            :type string
            :reader search-result-package)
   (system :initarg :system
           :type string
           :reader search-result-system)
   (documentation :initarg :documentation
                  :type string
                  :reader search-result-documentation)))


(defmethod type-to-schema ((type (eql 'search-result)))
  (dict "type" "object"
        "properties" (dict "type" (type-to-schema 'string)
                           "symbol" (type-to-schema 'string)
                           "package" (type-to-schema 'string)
                           "system" (type-to-schema 'string)
                           "documentation" (type-to-schema 'string))
        "required" (list "type" "symbol" "package" "system" "documentation")
        "x-cl-class" (symbol-name type)
        "x-cl-package" (package-name (symbol-package type))))


(defmethod transform-result ((obj search-result))
  (dict "type" (search-result-type obj)
        "symbol" (search-result-symbol obj)
        "package" (search-result-package obj)
        "system" (search-result-system obj)
        "documentation" (search-result-documentation obj)))


(define-rpc-method search-symbol (term)
  (:param term string)
  (:result (list-of search-result))
  (ultralisp/search:search-objects term)
  (loop for item in (ultralisp/search:search-objects term)
        for type = (string-downcase
                    (symbol-name (first item)))
        for symbol = (second item)
        for documentation = (third item)
        for other-params = (cdddr item)
        collect (make-instance 'search-result
                               :type type
                               :symbol symbol
                               :package (getf other-params :package)
                               :system (getf other-params :system)
                               :documentation documentation)))


(defun make-api-app ()
  (let ((server (jsonrpc:make-server))
        (http-transport (make-instance 'http-transport))
        (websocket-transport (make-instance 'websocket-transport)))
    ;; Only one transport can be used for message processing loop,
    ;; but we need to call both because this function sets a callback
    ;; for message dispatching inside the transport.
    (bind-server-to-transport server http-transport)
    ;; This transport will be used for message processing loop.
    (bind-server-to-transport server websocket-transport)

    (jsonrpc:expose server "rpc.discover"
                    (lambda (args)
                      (rpc-discover server args)))

    (loop for name being the hash-key of *methods*
          using (hash-value func)
          do (jsonrpc:expose server name func))

    (setf *server* server)
    
    (let ((websocket-app (jsonrpc/transport/websocket:make-clack-app websocket-transport))
          (http-app (jsonrpc/transport/http:make-clack-app http-transport)))
      (lambda (env)
        (reblocks/session:with-session (env)
          (cond
            ((wsd:websocket-p env)
             (funcall websocket-app env))
            ((and (string-equal (getf env :path-info)
                                "/openrpc.json")
                  (eql (getf env :request-method)
                       :get))
             (list 200
                   (list :content-type "application/json")
                   (list (yason:with-output-to-string* ()
                           (yason:encode (rpc-discover server nil))))))
            (t
             (funcall http-app env))))))))
