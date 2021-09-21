(defpackage #:ultralisp/package-variance-fix
  (:use #:cl)
  (:import-from #:log4cl)
(in-package ultralisp/package-variance-fix)


;; For some reasons, some libraries start to raise ASDF compile errors
;; because of package variance. This is the only hack I could imagine
;; to suppress warnings from SBCL during compilation.

#+sbcl
(defmethod asdf/component:around-compile-hook :around ((component t))
  (let ((previous-hook (call-next-method)))
    (lambda (compile-function)
      (handler-bind ((sb-int:package-at-variance-error
                       (lambda (c)
                         (log:warn "Suppressing package-at-varience-error: ~S"
                                   (apply #'format nil
                                          (simple-condition-format-control c)
                                          (simple-condition-format-arguments c)))
                         (invoke-restart 'sb-impl::drop-them))))
        ;; We need this binding to make sbcl signal a restartable error
        (let ((sb-ext:*on-package-variance* '(:error t)))
          (if previous-hook
              (funcall previous-hook compile-function)
              (funcall compile-function)))))))
