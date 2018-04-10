#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :cl-org-mode :silent t)
(ql:quickload :split-sequence :silent t)
(ql:quickload :unix-options :silent t)

(load "main.lisp")
(load "parse.lisp")
(load "write.lisp")

(defvar *logdir* "~/Dropbox/finances/")
(defvar *currency-log* (merge-pathnames "currency.log" *logdir*))
(defvar *currency-table* "currency.html")

(defun write-my-html (filename log)
  (with-open-file (html filename
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
    (format html "<html><body>")
    (write-currency-log-as-html html log)
    (format html "</body></html>")))

(defun read-my-currency-log ()
  (setf *owned* nil)
  (write-my-html *currency-table*
		 (process-currency-log
		  (read-currency-log *currency-log*)))
  (format t "~{~a~^~%~}~%" (format-owned)))

(defun currency-main (argv)
  (declare (ignore argv))
  (read-my-currency-log))

(defun log-main (argv)
  (let ((filename (elt argv 1)))
    (format t "Reading ~a~%" filename)
    (format t "~a~%" (cl-org-mode::read-org-file filename))))

(defun get-file-name ()
  (multiple-value-bind (s m h d month year)
      (get-decoded-time)
    (declare (ignore s m h d))
    (merge-pathnames (format nil "~d/~2,'0d.org" year month) *logdir*)))

(defun record-main (argv)
  (unix-options:with-cli-options ((subseq argv 1) t)
    (unix-options:&parameters
     (date "date")
     (tags "tags")
     (sum "sum"))
    (format t "Writing to ~a~%" (get-file-name))
    (with-open-file (out (get-file-name)
			 :direction :output
			 :if-exists :append
			 :if-does-not-exist :create)
      (format out "* ~{~a~^ ~}" unix-options:free)
      (if tags
	  (format out " :~{~a~^:~}:" (split-sequence:split-sequence #\, tags)))
      (format out "~%")
      (if date
	  (format out "  [~a]~%" date)
	  (multiple-value-bind (s m h date month year)
	      (get-decoded-time)
	    (declare (ignore s m h))
	    (format out "  [~4,'0d-~2,'0d-~2,'0d]~%" year month date)))
      (format out "  ~a~%" sum))))
