(uiop:define-package #:ultralisp/badges
  (:use #:cl)
  (:import-from #:alexandria
                #:last-elt
                #:when-let)
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


(defun make-versioned-badge (dist-name version)
  (format nil
          "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"137\" height=\"20\"><linearGradient id=\"b\" x2=\"0\" y2=\"100%\"><stop offset=\"0\" stop-color=\"#bbb\" stop-opacity=\".1\"/><stop offset=\"1\" stop-opacity=\".1\"/></linearGradient><mask id=\"a\"><rect width=\"137\" height=\"20\" rx=\"3\" fill=\"#fff\"/></mask><g mask=\"url(#a)\"><path fill=\"#555\" d=\"M0 0h61v20H0z\"/><path fill=\"#007ec6\" d=\"M61 0h76v20H61z\"/><path fill=\"url(#b)\" d=\"M0 0h137v20H0z\"/></g><g fill=\"#fff\" text-anchor=\"middle\" font-family=\"DejaVu Sans,Verdana,Geneva,sans-serif\" font-size=\"11\"><text x=\"30.5\" y=\"15\" fill=\"#010101\" fill-opacity=\".3\">~@(~A~)</text><text x=\"30.5\" y=\"14\">~:*~@(~A~)</text><text x=\"98\" y=\"15\" fill=\"#010101\" fill-opacity=\".3\">~A</text><text x=\"98\" y=\"14\">~:*~A</text></g></svg>"
          dist-name
          version))


(defun make-missing-badge (dist-name)
  (format nil
          "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"144\" height=\"20\"><linearGradient id=\"b\" x2=\"0\" y2=\"100%\"><stop offset=\"0\" stop-color=\"#bbb\" stop-opacity=\".1\"/><stop offset=\"1\" stop-opacity=\".1\"/></linearGradient><mask id=\"a\"><rect width=\"144\" height=\"20\" rx=\"3\" fill=\"#fff\"/></mask><g mask=\"url(#a)\"><path fill=\"#555\" d=\"M0 0h61v20H0z\"/><path fill=\"#9f9f9f\" d=\"M61 0h83v20H61z\"/><path fill=\"url(#b)\" d=\"M0 0h144v20H0z\"/></g><g fill=\"#fff\" text-anchor=\"middle\" font-family=\"DejaVu Sans,Verdana,Geneva,sans-serif\" font-size=\"11\"><text x=\"30.5\" y=\"15\" fill=\"#010101\" fill-opacity=\".3\">~@(~A~)</text><text x=\"30.5\" y=\"14\">~:*~@(~A~)</text><text x=\"101.5\" y=\"15\" fill=\"#010101\" fill-opacity=\".3\">not available</text><text x=\"101.5\" y=\"14\">not available</text></g></svg>"
          dist-name))

(defun badge-svg (project-name)
  (let* ((dist (common-dist))
         (version (when-let (project (get-project2 project-name))
                            (let ((versions (project-versions dist project)))
                              (when versions
                                (normalize-version (last-elt versions)))))))
    (if version
        (make-versioned-badge (dist-name dist) version)
        (make-missing-badge (dist-name dist)))))
