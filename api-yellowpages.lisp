(defpackage :yellowpages
  (:use :common-lisp :drakma)
  (:export :find-business :find-business-listings :business-details-list :get-business-details))

(in-package :yellowpages)

(defvar *url* "http://api.sandbox.yellowapi.com")
(defvar *api-key* nil)

(defvar *provinces*
  '(("ON" "Ontario")
	("QUE" "Quebec")
	("PEI" "Prince Edward Island")))

(defun find-business (what where)
  (let ((*text-content-types* '(("text" . nil)
								("application" . "json"))))
	(json:decode-json-from-string
	 (http-request (format nil "~a/FindBusiness/?what=~a&where=~a&fmt=JSON&pgLen=10&apikey=~a&UID=20320320932039203920392aaaa"
						   *url*
						   (url-encode what :latin-1)
						   (url-encode where :latin-1)
						   *api-key*)))))

(defun find-business-listings (what where)
  (rest (assoc :listings (find-business what where))))

(defun business-details-list (results)
  "``results'' is the result of calling FIND-BUSINESS."
  (loop for item in results
	 collect (list (assoc :name item)
				   (assoc :id item)
				   (assoc :prov (rest (assoc :address item)))))
)

(defun get-business-details (province business-name listing-id)
  (let ((*text-content-types* '(("text" . nil)
								("application" . "json"))))
;	(json:decode-json-from-string
	 (http-request (format nil "~a/GetBusinessDetails/?prov=~a&bus-name=~a&listingId=~a&fmt=JSON&apikey=~a&UID=20320320932039203920392aaaa"
						   *url*
						   (url-encode (second (find province *provinces* :test #'string= :key #'first)) :latin-1)
						   (url-encode business-name :latin-1)
						   listing-id
						   *api-key*))))
;)

(defun business-thumbnail-photo-urls (item)
  "``item'' is a business detail listing that comes from
GET-BUSINESS-DETAILS."
;;  (loop for photo in (rest (assoc :photos (rest (assoc :products item))))
  )