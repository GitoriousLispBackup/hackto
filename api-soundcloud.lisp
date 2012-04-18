(defpackage :soundcloud
  (:use :common-lisp :cl-web-utils :oauth2 :drakma)
  (:export :login
		   :upload-mp3))

(in-package :soundcloud)

(defvar *api-key* nil)
(defvar *token* nil)
(defvar *code* nil)

(defvar *redirect-uri* "http://fffffffoauth.com/")
(defvar *redirect* nil)

(defun login (client-id client-secret username password)
  (setf *token* (request-token
				 "https://api.soundcloud.com/oauth2/token"
				 ""
				 :redirect-uri *redirect-uri*
				 :method :post
				 :other `(("client_id" . ,client-id)
						  ("client_secret" . ,client-secret)
						  ("grant_type" . "password")
						  ("username" . ,username)
						  ("password" . ,password)))))

;;(define-json-request upload-track (oauth-token track[asset-data] track[title] track[sharing] asset-data)
;;  "http://api.soundcloud.com/tracks.json" :method :POST :all-args-p t)

(defun upload-mp3 (title path &optional (sharing "public"))
  "Requires user to have authorized uploading."
  (http-request "http://api.soundcloud.com/tracks.json"
				:method :post
				:content-length t
				:parameters `(("oauth_token" . ,*token*)
							  ("track[asset_data]" . ,path)
							  ("track[title]" . ,title)
							  ("track[sharing]" . ,sharing))))