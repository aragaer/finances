(load "main.lisp")

(define-condition malformed-entry-error (error)
  ())

(defun stream-at-event-start-p (stream)
  (eq #\* (peek-char nil stream nil nil)))

(defun read-stars (stream)
  (loop while (eq #\* (peek-char nil stream))
	do (read-char stream))
  (unless (eq #\Space (read-char stream))
    (error 'malformed-entry-error)))

(defun process-tags (tagstring)
  (setq tagstring (string-trim " :" tagstring))
  (loop for beg = 0 then (1+ end)
	for end = (and beg (position #\: tagstring :start beg))
	when beg
	  collect (subseq tagstring beg end)
	while end))

(defun read-title-line (stream)
  (let ((result nil)
	(title nil))
    (read-stars stream)
    (setq title (string-trim " " (read-line stream)))
    (let ((tagstart (search " :" title)))
      (when (and tagstart
	     (eq #\: (char title (- (length title) 1))))
	(setf (getf result :tags) (process-tags (subseq title tagstart)))
	(setq title (string-trim " " (subseq title 0 tagstart)))))
    (setf (getf result :title) title)
    result))

(defun read-event (stream)
  (unless (stream-at-event-start-p stream)
    (return-from read-event nil))
  (let ((data (read-title-line stream)))
    (format t "title: ~a~%" (getf data :title))
    (handler-case
	(loop until (stream-at-event-start-p stream)
	      do (format t "~a~%" (read-line stream)))
      (end-of-file () nil))
    data))

(defun read-event-log (filename)
  (with-open-file (in filename)
    (loop for event = (read-event in)
       while event
       collect event)))
