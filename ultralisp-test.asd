(defsystem "ultralisp-test"
  :class :package-inferred-system
  :pathname "t"
  :depends-on ("hamcrest"
               ;; These two systems here only to
               ;; check if they are able to compile
               ;; without errors:
               "ultralisp/worker"
               "ultralisp/server"
               ;; Test suites:
               "ultralisp-test/models/dist"
               "ultralisp-test/models/project"
               "ultralisp-test/models/source"
               "ultralisp-test/models/dist-source"
               "ultralisp-test/github/webhook"
               "ultralisp-test/pipeline"
               "ultralisp-test/db")
  :perform (test-op :after (op c)
                    (symbol-call :log :config
                                 :sane2 :warn)
                    (symbol-call :rove :run c)))
