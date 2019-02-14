(defsystem "ultralisp-test"
  :class :package-inferred-system
  :pathname "t"
  :depends-on ("hamcrest"
               "ultralisp"
               "ultralisp-test/models/project"
               "ultralisp-test/github/webhook"
               "ultralisp-test/pipeline")
  :perform (test-op :after (op c)
                    (symbol-call :log :config
                                 :sane2 :warn)
                    (symbol-call :rove :run c)))
