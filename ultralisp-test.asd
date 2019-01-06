(defsystem ultralisp-test
  :class :package-inferred-system
  :pathname "t"
  :depends-on (:hamcrest
               "ultralisp-test/project"
               "ultralisp-test/pipeline")
  :perform (test-op :after (op c)
                    (symbol-call :rove :run c)))
