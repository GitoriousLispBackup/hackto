(defpackage :trendspottr
  (:use :common-lisp :drakma)
  (:export :login :query))

(in-package :trendspottr)

(defvar *url* "http://trendspottr.com/api/v1.1/search.php")
(defvar *username* nil)
(defvar *password* nil)

(defun login (username password)
  (setf *username* username
		*password* password))

(defun query (query &optional (number-of-results 10))
  (let ((*text-content-types* '(("text" . nil)
								("application" . "json"))))
	(json:decode-json-from-string
	 (http-request (format nil "~a?q=~a&n=~a&expand=true" *url*
						   (url-encode query :latin-1)
						   number-of-results)
				   :basic-authorization (list *username* *password*)))))
