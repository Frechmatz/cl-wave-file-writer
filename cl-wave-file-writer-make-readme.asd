(defsystem :cl-wave-file-writer-make-readme
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-wave-file-writer"
  :description "A wave file writer"
  :long-description "A wave file writer"
  :depends-on (:cl-wave-file-writer :cl-html-readme :docparser)
  :components ((:module "make-readme"
			:serial t
			:components ((:file "packages")
				     (:file "make-readme")))))

