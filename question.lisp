
(defgeneric ask (question)
  (:documentation "Ask the question to the user and return their
  answer."))

(defgeneric get-answer (question)
  (:documentation "Retrieve answer given by user and return it."))



(defclass question ()
  ((text :accessor question-text :initarg :text)
   (prompt :accessor question-prompt :initarg :prompt))
  (:default-initargs :prompt "")
  (:documentation "This class is used as root class and also for
  simple one-line text answers."))

(defmethod ask (question)
  (format *query-io* "~a~%~a" (question-text question) (question-prompt question))
  (get-answer question))

(defmethod get-answer (question)
  (read-line *query-io*))


(defclass converting-question (question)
  ((converter :accessor question-converter :initarg :conv))
  (:documentation "This class is for questions where the line entered
  by the user needs to be converted to something else."))

(defmethod get-answer ((question converting-question))
  (multiple-value-bind (answer problem? text)
      (funcall (question-converter question) (read-line *query-io*))
    (if problem?
	(progn
	  (format *query-io* "~a~%~a" text (question-prompt question))
	  (get-answer question))
	answer)))

(defun make-read-converter (type warning)
  "Return a converter that uses Lisp's reader and check the result's type"
  (lambda (answer)
    (handler-case 
	(let ((object (read-from-string answer)))
	  (if (typep object type)
	      object
	      (values object t warning)))
      (end-of-file () (values nil t)))))

(defun make-conversion-chain (converters)
  (labels ((convert (answer converters)
	     (multiple-value-bind (object problem? text)
		 (funcall (first converters) answer)
	       (if (or problem? (null (rest converters)))
		   (values object problem? text)
		   (convert object (rest converters))))))
    (if (null converters)
	#'identity
	(lambda (answer) (convert answer converters)))))


(defmacro make-check (var-list text &body body)
  "Convenience macro to create functions that obey the checker
protocol"
  (destructuring-bind (var) var-list
    (once-only (text)
      `(lambda (,var)
	 (handler-case
	     (if (progn ,@body)
		 ,var
		 (values ,var t ,text))
	   (error () (values ,var t "Erreur interne. ")))))))

(defun positive? (&key strict)
  (make-check (answer) (format nil "Pas ~apositif. "
			       (if strict "strictement " ""))
    (if strict (> answer 0) (>= answer 0))))

(defun in-range? (lower upper &key lstrict ustrict)
  (make-check (answer) (format nil "Pas entre ~a~a et ~a~a. "
			       lower (if lstrict "" " (inclus)")
			       upper (if ustrict "" " (inclus)"))
    (and (if lstrict (> answer lower) (>= answer lower))
	 (if ustrict (< answer upper) (<= answer upper)))))

(defun divisible? (divisor)
  (make-check (answer) (format nil "Pas divisible par ~a" divisor)
    (zerop (rem answer divisor))))
