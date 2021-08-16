(in-package :cl-wave-file-writer-make-doc)

(defun make-index (system)
  (docparser:parse system))

(defun get-index-node (index package-name symbol-name)
  (aref (docparser:query
	 index
	 :package-name (string-upcase package-name)
	 :symbol-name (string-upcase symbol-name))
	0))

(defun make-function-string (index package-name symbol-name)
  "Returns HTML representation of a function"
  (let* ((node (get-index-node index package-name symbol-name))
	 (lambda-list (docparser:operator-lambda-list node)))
    (concatenate
     'string
     "<b>" (string-downcase symbol-name) "</b>&nbsp;"
     (string-downcase (format nil "~a" (if lambda-list lambda-list "()")))
     "<p>" (docparser:node-docstring node) "</p>")))

(defun make-code-string (path)
  "Returns HTML representation of a source code file"
  (concatenate
   'string
   "<p><pre><code>"
   (cl-html-readme:read-file path :replace-tabs t :escape t)
   "</code></pre></p>"))

(defun now ()
  "Returns a string representing the current date and time."
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
      (get-decoded-time)
    (declare (ignore dow dst-p tz))
    ;; 2018-09-19 21:28:16
    (let ((str (format nil "~4,'0d-~2,'0d-~2,'0d  ~2,'0d:~2,'0d:~2,'0d" yr mon day hr min sec)))
      str)))

(defun get-readme (index)
  `("<html><body>"
    (semantic (:name "header")
	      (heading (:name "cl-wave-file-writer")
		       ,(cl-html-readme:read-file "make-readme/introduction.html")))
    (semantic (:name "nav")
	      (heading (:name "Table of contents") (toc)))
    (semantic (:name "section")
	      (heading (:name "Installation" :toc t)
		       ,(cl-html-readme:read-file "make-readme/installation.html"))
	      (heading (:name "Example" :toc t)
		       ,(make-code-string "examples/example-1.lisp"))
	      (heading (:name "Change-Log" :toc t)
		       (heading (:name "Version 1.0.0")
				"<p>Initial release of cl-wave-file-writer.</p>"
				"<p><b>This version is the current quicklisp release.</b></p>")
		       (heading (:name "Version 1.0.1")
				"<p>Removed redundant clipping.</p>"))
	      (heading (:name "API" :toc t)
		       ,(make-function-string index "cl-wave-file-writer" "make-writer"))
	      (heading (:name "Run tests" :toc t)
		       ,(cl-html-readme:read-file "make-readme/run-tests.html"))
	      (heading (:name "Generate documentation" :toc t)
		       ,(make-code-string "make-readme/generate-doc.lisp"))
	      (heading (:name "Acknowledgements" :toc t)
		       ,(cl-html-readme:read-file "make-readme/acknowledge.html")))
    (semantic (:name "footer")
	      "<hr/><p><small>Generated " ,(now) "</small></p>")
    "</body></html>"))

(defun make-doc ()
  (let ((cl-html-readme:*home-directory* (asdf:system-source-directory :cl-wave-file-writer))
	(cl-html-readme:*tab-width* 8)
	(index (make-index :cl-wave-file-writer)))
    (with-open-file (fh (cl-html-readme:make-path "docs/index.html")
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create
			:external-format :utf-8)
      (cl-html-readme:doc-to-html fh (get-readme index))))
  "DONE")

;;(make-doc)

