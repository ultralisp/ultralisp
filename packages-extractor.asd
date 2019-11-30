(defsystem "packages-extractor"
  :class :package-inferred-system
  :build-operation "program-op"
  :build-pathname "packages-extractor"
  :pathname "src"
  :depends-on ("packages-extractor/packages-extractor")
  :entry-point "ultralisp/packages-extractor::main")


(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
