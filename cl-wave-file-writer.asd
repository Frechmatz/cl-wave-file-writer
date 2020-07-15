(defsystem :cl-wave-file-writer
  :serial t
  :version "1.0.0"
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

