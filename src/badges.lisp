(uiop:define-package #:ultralisp/badges
  (:use #:cl)
  (:import-from #:alexandria
                #:last-elt
                #:when-let*)
  (:import-from #:ultralisp/models/project
                #:get-project2)
  (:import-from #:ultralisp/models/dist
                #:common-dist
                #:dist-name)
  (:import-from #:ultralisp/clpi
                #:project-versions)
  (:export #:badge-svg))

(in-package #:ultralisp/badges)

(defun normalize-version (version)
  (cond
    ((null version) nil)
    ((stringp version)
     (unless (zerop (length version))
       version))
    (t (princ-to-string version))))

(defparameter +badge-side-padding+ 7)
(defparameter +badge-min-section-width+ 32)

(defun badge-char-width (char)
  (cond
    ((find char "il.:/'`" :test #'char=) 3.5)
    ((find char "fjrt" :test #'char=) 4.5)
    ((find char "MW" :test #'char=) 9.0)
    ((find char "IJ" :test #'char=) 4.0)
    ((upper-case-p char) 7.0)
    ((digit-char-p char) 6.0)
    ((find char "mw" :test #'char=) 7.0)
    ((find char "-_/" :test #'char=) 4.5)
    ((lower-case-p char) 5.5)
    (t 6.0)))

(defun badge-text-width (text)
  (loop for char across text
        sum (badge-char-width char)))

(defun badge-section-width (text)
  (let* ((content-width (badge-text-width text))
         (total-width (+ content-width (* 2 +badge-side-padding+))))
    (round (max total-width +badge-min-section-width+))))

(defun format-position (value)
  (let* ((number (float value 1.0d0))
         (string (format nil "~,1F" number))
         (length (length string)))
    (if (and (>= length 3)
             (char= (char string (- length 2)) #\.)
             (char= (char string (1- length)) #\0))
        (subseq string 0 (- length 2))
        string)))

(defun compose-badge (label-text value-text value-color)
  (let* ((label-width (badge-section-width label-text))
         (value-width (badge-section-width value-text))
         (total-width (+ label-width value-width))
         (label-center (format-position (/ label-width 2)))
         (value-center (format-position (+ label-width (/ value-width 2)))))
    (format nil
            "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"~D\" height=\"20\"><linearGradient id=\"b\" x2=\"0\" y2=\"100%\"><stop offset=\"0\" stop-color=\"#bbb\" stop-opacity=\".1\"/><stop offset=\"1\" stop-opacity=\".1\"/></linearGradient><mask id=\"a\"><rect width=\"~D\" height=\"20\" rx=\"3\" fill=\"#fff\"/></mask><g mask=\"url(#a)\"><path fill=\"#555\" d=\"M0 0h~Dv20H0z\"/><path fill=\"~A\" d=\"M~D 0h~Dv20H~Dz\"/><path fill=\"url(#b)\" d=\"M0 0h~Dv20H0z\"/></g><g fill=\"#fff\" text-anchor=\"middle\" font-family=\"DejaVu Sans,Verdana,Geneva,sans-serif\" font-size=\"11\"><text x=\"~A\" y=\"15\" fill=\"#010101\" fill-opacity=\".3\">~A</text><text x=\"~A\" y=\"14\">~A</text><text x=\"~A\" y=\"15\" fill=\"#010101\" fill-opacity=\".3\">~A</text><text x=\"~A\" y=\"14\">~A</text></g></svg>"
            total-width
            total-width
            label-width
            value-color
            label-width
            value-width
            label-width
            total-width
            label-center
            label-text
            label-center
            label-text
            value-center
            value-text
            value-center
            value-text)))


(defun make-versioned-badge (dist-name version)
  (compose-badge (format nil "~@(~A~)" dist-name)
                 (format nil "~A" version)
                 "#007ec6"))


(defun make-missing-badge (dist-name)
  (compose-badge (format nil "~@(~A~)" dist-name)
                 "not available"
                 "#9f9f9f"))

(defun badge-svg (project-name)
  (let* ((dist (common-dist))
         (version (when-let* ((project (get-project2 project-name))
                              (versions (project-versions dist project)))
                    (normalize-version (last-elt versions)))))
    (if version
        (make-versioned-badge (dist-name dist) version)
        (make-missing-badge (dist-name dist)))))
