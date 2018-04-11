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

(defun record-main (argv)
  (unix-options:with-cli-options ((cdr argv) t)
    ((regular "regular")
     unix-options:&parameters
     (date "date")
     (tags "tags")
     (sum "sum"))
    (format t "Writing to ~a~%" (get-file-name))
    (with-open-file (out (get-file-name)
			 :direction :output
			 :if-exists :append
			 :if-does-not-exist :create)
      (format out "* ~{~a~^ ~}" unix-options:free)
      (let ((tag-list (split-sequence:split-sequence #\, tags)))
	(if regular (push *tag-regular* tag-list))
	(if tag-list
	    (format out " :~{~a~^:~}:" tag-list)))
      (format out "~%")
      (format out "#+WHEN: ")
      (if date
	  (format out "~a" date)
	  (multiple-value-bind (s m h date month year)
	      (get-decoded-time)
	    (declare (ignore s m h))
	    (format out "~4,'0d-~2,'0d-~2,'0d" year month date)))
      (format out "~%")
      (format out "#+SUM: ~a~%" sum))))

(defun main (argv)
  (alexandria:switch ((second argv) :test #'equal)
    ("log" (log-main (cdr argv)))
    ("record" (record-main (cdr argv)))
    ("currency" (currency-main (cdr argv)))))
