(defsystem ultralisp-test
  :class :package-inferred-system
  :pathname "t"
  :depends-on ("ultralisp-test/project")
  :perform (test-op :after (op c)
                    (symbol-call :rove :run c)))
