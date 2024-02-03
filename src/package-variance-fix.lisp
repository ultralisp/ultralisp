(defpackage #:ultralisp/package-variance-fix
  (:use #:cl)
  (:import-from #:log4cl))
(in-package #:ultralisp/package-variance-fix)


;; For some reasons, some libraries start to raise ASDF compile errors
;; because of package variance. This is the only hack I could imagine
;; to suppress warnings from SBCL during compilation.

;; Disabled because tests are failing to load now. They fail with trace:
;; An unhandled error condition has been signalled:
;;      The value
;;        (LAMBDA (ASDF/INTERFACE::THUNK)
;;          (HANDLER-BIND (((AND WARNING (NOT STYLE-WARNING))
;;                          (LAMBDA (ASDF/INTERFACE::C)
;;                            (FORMAT *ERROR-OUTPUT* "~&~@<~S: ~3i~:_~A~:>~%"
;;                                    (CLASS-NAME (CLASS-OF ASDF/INTERFACE::C))
;;                                    ASDF/INTERFACE::C)
;;                            (MUFFLE-WARNING ASDF/INTERFACE::C))))
;;            (LET ((SB-EXT:*ON-PACKAGE-VARIANCE* '(:WARN T)))
;;              (FUNCALL ASDF/INTERFACE::THUNK))))
;;      is not of type
;;        (OR FUNCTION SYMBOL)
;;   Backtrace for: #<SB-THREAD:THREAD "main thread" RUNNING {1001550393}>
;;   0: (SB-VM::CALL-SYMBOL)
;;   1: ((LAMBDA (ULTRALISP/PACKAGE-VARIANCE-FIX::COMPILE-FUNCTION) :IN ASDF/COMPONENT:AROUND-COMPILE-HOOK) #<FUNCTION (LAMBDA (&REST ASDF/LISP-ACTION::FLAGS) :IN ASDF/LISP-ACTION:PERFORM-LISP-COMPILATION) {1006A879FB}>)
;;   2: ((LAMBDA (ULTRALISP/PACKAGE-VARIANCE-FIX::COMPILE-FUNCTION) :IN ASDF/COMPONENT:AROUND-COMPILE-HOOK) #<FUNCTION (LAMBDA (&REST ASDF/LISP-ACTION::FLAGS) :IN ASDF/LISP-ACTION:PERFORM-LISP-COMPILATION) {1006A879FB}>)
;;   3: (ASDF/LISP-ACTION:PERFORM-LISP-COMPILATION #<ASDF/LISP-ACTION:COMPILE-OP > #<ASDF/LISP-ACTION:CL-SOURCE-FILE "slynk" "slynk-backend">)
;;   4: ((SB-PCL::EMF ASDF/ACTION:PERFORM) #<unused argument> #<unused argument> #<ASDF/LISP-ACTION:COMPILE-OP > #<ASDF/LISP-ACTION:CL-SOURCE-FILE "slynk" "slynk-backend">)
;;   5: ((LAMBDA NIL :IN ASDF/ACTION:CALL-WHILE-VISITING-ACTION))
;;   6: ((:METHOD ASDF/ACTION:PERFORM-WITH-RESTARTS :AROUND (T T)) #<ASDF/LISP-ACTION:COMPILE-OP > #<ASDF/LISP-ACTION:CL-SOURCE-FILE "slynk" "slynk-backend">) [fast-method]
;;   7: ((:METHOD ASDF/PLAN:PERFORM-PLAN (T)) #<ASDF/PLAN:SEQUENTIAL-PLAN {100322FAC3}>) [fast-method]
;;   8: ((FLET SB-C::WITH-IT :IN SB-C::%WITH-COMPILATION-UNIT))
;;   9: ((:METHOD ASDF/PLAN:PERFORM-PLAN :AROUND (T)) #<ASDF/PLAN:SEQUENTIAL-PLAN {100322FAC3}>) [fast-method]
;;   10: ((:METHOD ASDF/OPERATE:OPERATE (ASDF/OPERATION:OPERATION ASDF/COMPONENT:COMPONENT)) #<ASDF/LISP-ACTION:LOAD-OP > #<ASDF/PACKAGE-INFERRED-SYSTEM:PACKAGE-INFERRED-SYSTEM "ultralisp-test"> :PLAN-CLASS NIL :PLAN-OPTIONS NIL) [fast-method]
;;   11: ((SB-PCL::EMF ASDF/OPERATE:OPERATE) #<unused argument> #<unused argument> #<ASDF/LISP-ACTION:LOAD-OP > #<ASDF/PACKAGE-INFERRED-SYSTEM:PACKAGE-INFERRED-SYSTEM "ultralisp-test"> :VERBOSE NIL)
;;   12: ((LAMBDA NIL :IN ASDF/OPERATE:OPERATE))
;;   13: ((:METHOD ASDF/OPERATE:OPERATE :AROUND (T T)) #<ASDF/LISP-ACTION:LOAD-OP > #<ASDF/PACKAGE-INFERRED-SYSTEM:PACKAGE-INFERRED-SYSTEM "ultralisp-test"> :VERBOSE NIL) [fast-method]
;;   14: ((SB-PCL::EMF ASDF/OPERATE:OPERATE) #<unused argument> #<unused argument> ASDF/LISP-ACTION:LOAD-OP "ultralisp-test" :VERBOSE NIL)

;; #+sbcl
;; (defmethod asdf/component:around-compile-hook :around ((component t))
;;   (let ((previous-hook (call-next-method)))
;;     #'(lambda (compile-function)
;;         (handler-bind ((sb-int:package-at-variance-error
;;                          (lambda (c)
;;                            (log:warn "Suppressing package-at-varience-error: ~S"
;;                                      (apply #'format nil
;;                                             (simple-condition-format-control c)
;;                                             (simple-condition-format-arguments c)))
;;                            (invoke-restart 'sb-impl::drop-them))))
;;           ;; We need this binding to make sbcl signal a restartable error
;;           (let ((sb-ext:*on-package-variance* '(:error t)))
;;             (cond
;;               (previous-hook
;;                (format t "TRACE: Calling previous hook ~S~%"
;;                        previous-hook)
;;                (funcall previous-hook compile-function))
;;               (t
;;                (format t "TRACE: Calling compile-function ~S~%"
;;                        compile-function)
;;                (funcall compile-function))))))))
