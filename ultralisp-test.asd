(defsystem "ultralisp-test"
  :class :package-inferred-system
  :pathname "t"
  :depends-on ("hamcrest"
               ;; These two systems here only to
               ;; check if they are able to compile
               ;; without errors:
               "ultralisp/worker"
               ;; Server must go after the worker
               ;; to catch most not imported
               ;; packages:
               "ultralisp/server"
               ;; Test suites:
               "ultralisp-test/models/dist"
               "ultralisp-test/models/project"
               "ultralisp-test/models/source"
               "ultralisp-test/models/dist-source"
               "ultralisp-test/github/webhook"
               "ultralisp-test/pipeline"
               "ultralisp-test/db")
  :perform (test-op (op c)
                    ;; This code changes log level when
                    ;; Ultralisp checks it's own code
                    ;; we need to learn how to make these changes
                    ;; and redo them on exit from the rove:run
                    ;; 
                    ;; (symbol-call :log :config
                    ;;              :sane2 :warn)
                    (if (symbol-call :rove :run c)
                        (format t "Tests are OK~%")
                        (error "Tests failed"))))
