(defvar *base-currency* 'RUB)

(defclass money-amount ()
  ((currency
    :initarg :currency
    :initform *base-currency*)
   (amount
    :initarg :amount)))

(defclass wallet ()
  ((name :initarg :name)
   (currency :initarg :currency)
   (amount :initform 0)))

(defclass bank ()
  ((name :initarg :name)
   (wallets :initform nil)))

(defvar *banks* nil)

(defclass owned-currency (money-amount)
  ((exchange-rate :initarg :exchange-rate)
   (amount :initform 0)))

(defvar *owned* nil)

(define-condition currency-mismatch-error (error)
  ((currency1 :initarg :currency1 :reader currency1)
   (currency2 :initarg :currency2 :reader currency2)))

(defgeneric add (wallet amount))
(defgeneric take (wallet amount))

(defmethod add ((wallet wallet) amount)
  (with-slots ((w-amount amount) currency) wallet
    (incf (slot-value (getf *owned* currency) 'amount) amount)
    (incf w-amount amount)))

(defmethod take ((wallet wallet) amount)
  (with-slots ((w-amount amount) currency) wallet
    (decf (slot-value (getf *owned* currency) 'amount) amount)
    (decf w-amount amount)))

(defun transfer (from-wallet to-wallet amount)
  (with-slots ((from-currency currency) (from-amount amount))
      from-wallet
    (with-slots ((to-currency currency) (to-amount amount))
	to-wallet
      (unless (eq from-currency to-currency)
	(error 'currency-mismatch-error
	       :currency1 from-currency
	       :currency2 to-currency))
      (decf from-amount amount)
      (incf to-amount amount))))

(defun format-owned ()
  (loop
     for (nil owned) on *owned* by #'cddr
     collecting
       (with-slots (currency exchange-rate amount) owned
	 (if (eq currency *base-currency*)
	     (format nil "~a ~a" currency amount)
	     (format nil "~a ~ax~a"
		     currency amount exchange-rate)))))

(defun update-exchange-rate (owned-currency
			     change-amount
			     base-currency)
  (with-slots ((exchange-rate exchange-rate)
	       (owned-amount amount)) owned-currency
    (let* ((owned-value (* exchange-rate owned-amount))
	   (new-value (+ owned-value base-currency))
	   (new-amount (+ owned-amount change-amount))
	   (new-rate (/ new-value new-amount)))
      (setf owned-amount new-amount)
      (setf exchange-rate new-rate))))

(defmacro with-owned-currency (currency &rest body)
  `(let ((owned (getf *owned* ,currency)))
     (unless owned
       (setf owned (make-instance 'owned-currency
				  :currency ,currency
				  :exchange-rate 0))
       (setf (getf *owned* ,currency) owned))
     ,@body))

(defun currency-change-amount (event)
  (destructuring-bind (event-name currency amount bank)
      (getf event :data)
    (with-owned-currency currency
      (with-slots ((owned-amount amount)) owned
	(format t "Change: ~a ~a ~,2@f ~a->~a ~a~%"
		event-name currency
		amount owned-amount
		(+ amount owned-amount) bank)
	(incf owned-amount amount))))
  event)

(defun currency-exchange (event)
  (destructuring-bind (event-name currency amount base bank)
      (getf event :data)
    (with-owned-currency currency
      (let ((old-rate 0) (old-amount 0))
	(with-slots (exchange-rate (owned-amount amount)) owned
	  (setf old-rate exchange-rate)
	  (setf old-amount owned-amount))
	(update-exchange-rate owned amount base)
	(with-slots ((new-rate exchange-rate)
		     (new-amount amount)) owned
	  (setf (getf event :new-rate) new-rate)
	  (format t "Exhange: ~a ~a ~,2@f ~a ~a->~a ~a->~a (~,2@f) ~a~%"
		  event-name currency
		  amount base
		  old-amount new-amount
		  old-rate new-rate
		  (- new-rate old-rate)
		  bank)))))
  event)

(defun process-currency-log (log)
  (loop
     for record in log
     for event-type = (getf record :event)
     do (with-owned-currency (getf record :currency)
	  (setf (getf record :rate)
		(slot-value owned 'exchange-rate)))
     when (eq event-type :currency-change)
     collect (currency-change-amount record)
     when (eq event-type :currency-exchange)
     collect (currency-exchange record)))
