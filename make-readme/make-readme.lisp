(in-package :cl-wave-file-writer-make-readme)

(defun make-function-string (f)
  (concatenate
   'string
   "<p>"
   (cl-readme:sbcl-make-function-decl f)
   "</p><p>"
   (documentation f 'function)
   "</p>"))


(defun get-readme ()
  `("<html><body>"
    (semantic (:name "header")
	      (heading (:name "cl-wave-file-writer")
		       ,(cl-readme:read-verbatim "make-readme/introduction.html")))
    (semantic (:name "nav")
	      (heading (:name "Table of contents") (toc)))
    (semantic (:name "section")
	      (heading (:name "Installation" :toc t)
		       ,(cl-readme:read-verbatim "make-readme/installation.html"))
	      (heading (:name "Example" :toc t)
		       ,(cl-readme:read-code "examples/example-1.lisp"))
	      (heading (:name "API" :toc t)
		       ,(make-function-string 'cl-wave-file-writer:make-writer))
	      (heading (:name "Acknowledgements" :toc t)
		       ,(cl-readme:read-verbatim "make-readme/acknowledge.html")))
    (semantic (:name "footer")
	      "<p><small>Generated " ,(cl-readme:current-date) "</small></p>")
    "</body></html>"))

(defun make-readme ()
  (let ((cl-readme:*home-directory* "/Users/olli/src/lisp/cl-wave-file-writer/")
	(cl-readme:*tab-width* 8))
    (with-open-file (fh (cl-readme:make-path "docs/index.html")
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create
			:external-format :utf-8)
      (cl-readme:doc-to-html fh (get-readme))))
  "DONE")

;;(make-readme)

