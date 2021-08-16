(defsystem :cl-wave-file-writer
  :serial t
  :version "1.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-wave-writer"
  :description "A wave file writer"
  :long-description "A wave file writer"
  :components ((:module "src"
			:serial t
			:components ((:file "packages")
				     (:file "wave-file-writer")))))

(defsystem :cl-wave-file-writer/doc
  :serial t
  :version "1.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-wave-file-writer"
  :description "Documentation sources of cl-wave-file-writer"
  :long-description
  "Documentation sources of cl-wave-file-writer. Documentation is created via (cl-wave-file-writer-make-doc::make-doc)"
  :depends-on (:cl-wave-file-writer :cl-html-readme :docparser)
  :components ((:module "make-readme"
			:serial t
			:components ((:file "packages")
				     (:file "make-doc")))))
