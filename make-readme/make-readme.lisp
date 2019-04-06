(in-package :cl-wave-file-writer-make-readme)

(defun write-html ()
  (let ((CL-README:*HOME-DIRECTORY* "/Users/olli/src/lisp/cl-wave-file-writer/"))
    (let ((docstr (concatenate
		   'string
		   "<html><body>"
		   "<h1>cl-wave-file-writer</h1>"
		   "A Common Lisp library for writing wave (.wav) files. The writer does not buffer data, so files of arbitrary 
                    size can be written."
		   (example-code "examples/example-1.lisp")
		   ;;"<h2>Installation</h2>"
		   ;;(read-text-file "makedoc/installation.html")
		   "<h2>API</h2>"
		   (make-function-string 'cl-wave-file-writer:make-writer :append-separator nil)
		   (read-text-file "make-readme/acknowledge.html")
		   "<hr/><p><small>Generated " (current-date) "</small></p>"
		   "</body></html>"
		   )))
      (with-open-file (fh (make-path "make-readme/generated/readme.html")
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create
			  :external-format :utf-8)
	(format fh "~a" docstr)))))

;;(write-html)

