(defsystem :cl-wave-file-writer-make-readme
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-synthesizer"
  :description "A wave file writer"
  :long-description "A wave file writer"
  :depends-on (:cl-wave-file-writer :cl-readme)
  :components ((:module "make-readme"
			:serial t
			:components ((:file "packages")
				     (:file "make-readme")))))

