(defpackage #:ultralisp/utils
  (:use #:cl)
  (:export
   #:getenv))
(in-package ultralisp/utils)


(defun getenv (name &optional (default nil))
  "Возвращает значение из переменной окружения или дефолт, если переменная не задана"
  (let ((value (uiop:getenv name)))
    (if value
        (cond 
          ((or (integerp default)
               (floatp default))
           (read-from-string value))
          ((stringp default)
           value)
          (t value))
        default)))
