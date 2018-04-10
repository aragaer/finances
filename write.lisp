(load "main.lisp")

(defun write-change-event (out event)
  (destructuring-bind (event-name currency amount bank)
      (getf event :data)
    (let ((exchange-rate (getf event :rate)))
      (format out "<td>~a</td><td>~a</td><td>~a</td>~%"
	      bank event-name currency)
      (format out "<td>~a</td><td>~a</td><td>~a</td><td></td>~%"
	      amount (* amount exchange-rate)
	      exchange-rate))))

(defun write-exchange-event (out event)
  (destructuring-bind (event-name currency amount base bank)
      (getf event :data)
    (format out "<td>~a</td><td>~a</td><td>~a</td>~%"
	    bank event-name currency)
    (format out "<td>~a</td><td>~a</td><td>~a</td>~%"
	    amount base (getf event :new-rate))
    (format out "<td style=\"background:")
    (let ((old-rate (getf event :rate))
	  (new-rate (getf event :new-rate)))
      (if (eq 0 old-rate)
	  (format out "#ffc\">=~,2f</td>~%"
		  new-rate)
	  (format out "~a\">~,2@f</td>~%"
		  (if (> new-rate old-rate) "#fcc" "#cfc")
		  (- new-rate old-rate))))))

(defun write-currency-log-as-html (html log)
  (format html "<table>~%")
  (format html "<tr style=\"background:#ccc\">")
  (dolist (header (list "Банк" "Событие" "Валюта" "Сумма"
			"В рублях" "Курс" "Изменение"))
    (format html "<th>~a</th>" header))
  (format html "</tr>~%")
  (let ((white t))
    (dolist (record log)
      (if white
	  (format html "<tr>")
	  (format html "<tr style=\"background:#d0e8ff\">"))
      (setf white (not white))
      (ecase (getf record :event)
	(:currency-change
	 (write-change-event html record))
	(:currency-exchange
	 (write-exchange-event html record)))
      (format html "</tr>")))
  (format html "</table>~%"))

