(defsystem :hackto
  :name "hackto"
  :description "Web application for band managers"
  :version "0.1"
  :author "Rudolf Olah"
  :license "AGPL"
  :depends-on (:hunchentoot :alexandria :parenscript :cl-who :split-sequence :cl-web-utils)
  :serial t
  :components ((:file "api-soundcloud")
			   (:file "api-freshbooks")
			   (:file "api-wordpress")
			   (:file "api-trendspottr")
			   (:file "api-yellowpages")
			   (:file "api-twilio")
			   (:file "hackto")))