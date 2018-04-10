(load "main.lisp")

(defun parse-currency-change-event (event-name
				    currency
				    amount
				    bank)
  (list :event :currency-change
	:currency currency
	:data (list event-name currency amount bank)))

(defun parse-currency-exchange-event (event-name
				      currency
				      amount
				      base
				      bank)
  (list :event :currency-exchange
	:currency currency
	:data (list event-name currency amount base bank)))

(defun parse-line (line)
  (with-input-from-string (in line)
    (let ((tokens (loop for word = (read in nil nil)
		     while word
		     collecting word)))
      (if (eq (length tokens) 4)
	  (apply 'parse-currency-change-event tokens)
	  (apply 'parse-currency-exchange-event tokens)))))

(defun read-currency-log (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
       while line
       collect (parse-line line))))
