(defun search-version-in-changelog (lines)
  (let* ((line (nth 4 lines))
         (space-pos (position #\Space line)))
    (when space-pos
      (subseq line 0 space-pos))))


(defsystem ultralisp
  :description "A fast-moving Common Lisp software distribution for those who want to publish his/her software today."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :licence "BSD"
  :class :package-inferred-system
  :version (:read-file-line "ChangeLog.rst" :at search-version-in-changelog)
  :pathname "src"
  :depends-on ("cl-interpol"
               "log4sly"
	       ;; To not load it when worker is starting
	       ;; This should fix issue with bordeaux-threads recompilation:
	       ;; https://github.com/ultralisp/ultralisp/issues/84
	       "dbd-postgres"
               ;; We need this while will not support package inferred systems:
               ;; https://github.com/ultralisp/ultralisp/issues/3
               "weblocks-ui"
               ;; To make inplace links work in the HTML
               "ultralisp/main"
               "ultralisp/server")
  :in-order-to ((test-op (test-op ultralisp-test)))
  :perform (compile-op :before (o c)
                       #+ros.installing
                       (roswell:roswell '("install" "40ants/defmain"))))

(register-system-packages "prometheus.collectors.sbcl" '(#:prometheus.sbcl))
(register-system-packages "prometheus.collectors.process" '(#:prometheus.process))
