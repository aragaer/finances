#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :alexandria :silent t)
(ql:quickload :cl-org-mode :silent t)
(ql:quickload :split-sequence :silent t)
(ql:quickload :unix-options :silent t)

(load "main.lisp")
(load "parse.lisp")
(load "write.lisp")

(defvar *logdir* "~/Dropbox/finances/")

(defun record-to-plist (record)
  (let ((result (list :title (cl-org-mode:title-of record)
		      :tags (cl-org-mode:tags-of record))))
    (loop for keyword in (cl-org-mode:children-of (cl-org-mode:section-of record))
       do (setf (getf result (cl-org-mode:name-of keyword)) (cl-org-mode:value-of keyword)))
    result))

(defun currency-main (argv)
  (unix-options:with-cli-options ((cdr argv) t)
    (summary debug unix-options:&parameters infile outfile)
    (setf *debug* debug)
    (setf *owned* nil)
    (with-open-file (html outfile
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
      (format html "<html><body>")
      (write-currency-log-as-html html
				  (process-currency-log
				   (read-currency-log infile)))
      (format html "</body></html>"))
    (if summary
      (format t "~{~a~^~%~}~%" (format-owned)))))

(defun log-main (argv)
  (let ((filename (second argv)))
    (format t "Reading ~a~%" filename)
    (let ((doc (cl-org-mode:org-parse (pathname filename))))
      (loop for record in (cl-org-mode:node.out doc)
	 do (format t "~a~%" (record-to-plist record))))))

(defun get-file-name ()
  (multiple-value-bind (s m h d month year)
      (get-decoded-time)
    (declare (ignore s m h d))
    (merge-pathnames (format nil "~d/~2,'0d.org" year month) *logdir*)))

(defvar *tag-regular* "регулярное")
(defvar *doc-template* "")

(defun get-date ()
  (multiple-value-bind (s m h date month year)
      (get-decoded-time)
    (declare (ignore s m h))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month date)))

(defun make-keyword (name value)
  (make-instance 'cl-org-mode:org-keyword
		 :optional nil
		 :name name
		 :value value))

(defun make-record (title tags &rest keywords)
  (cl-org-mode:make-node
   title
   (make-instance 'cl-org-mode:org-section
		  :children (loop while keywords
			       collecting (make-keyword (pop keywords)
							(pop keywords))))
   nil :tags tags))

(defun record-main (argv)
  (unix-options:with-cli-options ((cdr argv) t)
    ((regular "regular")
     unix-options:&parameters
     (date "date")
     (tags "tags")
     (sum "sum"))
    (let ((filename (get-file-name)))
      (format t "Writing to ~a~%" filename)
      (let ((doc (cl-org-mode:org-parse (if (probe-file filename)
					    (pathname filename)
					    *doc-template*)))
	    (tag-list (split-sequence:split-sequence #\, tags))
	    (title (format nil "~{~a~^ ~}" unix-options:free)))
	(if regular (push *tag-regular* tag-list))
	(let ((nodes (cl-org-mode:node.out doc))
	      (new-node (make-record title tag-list
				     :when (or date (get-date))
				     :sum sum)))
	  (setf (cl-org-mode:node.out doc) (append nodes (list new-node)))
	  (with-open-file (out (get-file-name)
			       :direction :output
			       :if-exists :supersede
			       :if-does-not-exist :create)
	    (cl-org-mode:org-present :normal doc out)))))))

(defun main (argv)
  (alexandria:switch ((second argv) :test #'equal)
    ("log" (log-main (cdr argv)))
    ("record" (record-main (cdr argv)))
    ("currency" (currency-main (cdr argv)))))
