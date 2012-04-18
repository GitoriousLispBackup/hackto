(defpackage :hackto
  (:use :common-lisp :hunchentoot :cl-who :parenscript)
  (:export :start-server))

(in-package :hackto)

(setf *js-string-delimiter* #\")

(defvar *band* nil)
(defvar *manager-phone-number* nil)
(defvar *albums* (list))
(defvar *expenses* (list))

(defun band-name (band) (first band))
(defun band-albums (band) (second band))
(defun band-expenses (band) (third band))

(defun album-title (album) (first album))
(defun album-songs (album) (second album))
(defun album-download-count (album)
  (loop for song in (album-songs album) sum (song-download-count song)))

(defun song-title (song) (first song))
(defun song-download-url (song) (second song))
(defun song-download-count (song) (third song))

(defun make-band (band-name)
  (list band-name (list) (list)))

(defun make-album (title songs)
  (list title songs))

(defun add-album (album)
  (push album *albums*))

(defun make-song (title download-url download-count)
  (list title download-url download-count))

(defun make-expense (item cost date)
  (list item cost date))

(defun expense-item (expense) (first expense))
(defun expense-cost (expense) (second expense))
(defun expense-date (expense) (third expense))

(defun add-band-expense (item cost)
  (push (make-expense item cost "14 April 2012") *expenses*))

(defun api-login ()
  (trendspottr:login "hackto" "HT2012")
  #|(soundcloud:login nil
					nil
					nil
					nil)
  |#
  )

(defun make-review (title text &optional (user "anonymous"))
  (list title text user))
(defun review-title (review) (first review))
(defun review-text (review) (second review))
(defun review-user (review) (third review))

(defun trends-for (query)
  (assoc :results (trendspottr:query query 10)))

(defun sentiment-of-text (text)
  "Returns a positive integer if the text contains a lot of positive words, returns a negative integer if the text contains negative words."
  (length text))

(defun css ()
  "
body { background: #333; color: #aaa; font-family: sans-serif; font-size: 9pt; }
h1,h2,h3,h4,h5,h6 { color: #aaa; }
h1 { font-size: 16pt }
h2 { font-size: 13pt }
a, a:visited { color: #eee; }
table { width: 100% }
.cost, .download-count { text-align: center }
.date { text-align: right }
li { padding: 5px; font-size: 11pt; }
li.light { color: #888 }
li.heavy { color: #eee }
")

(defmacro page (title &body body)
  `(with-html-output-to-string (stream nil :prologue t :indent t)
	 (:html
	  (:head
	   (:title ,title)
	   (:script :src "https://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js")
	   (:style (str (css))))
	  (:body
	   (:div :id "header"
			 (:h1 (:a :href "/" "Band Manager")))
	   (:div :id "content"
			 ,@body)))))

(define-easy-handler (index :uri "/") ()
  (page "Band Manager"
	(:ul
	 (:li (:a :href "/stats" "Band Stats"))
	 (:li (:a :href "/venues?what=concert+venues&where=Toronto" "Venues"))
	 (fmt "<li><a href=\"/trends?query=~a\">Trends</a></li>" (url-encode (band-name *band*)))
	 (:li (:a :href "/expenses" "Expenses")))
	(:h2 "Add Expense")
	(:form :action "/expenses/add" :method :post
		   (:input :type :text :name "item" :placeholder "Item name")
		   (:input :type :text :name "cost" :placeholder "Item cost")
		   (:br)
		   (:input :type :submit))
	(:h2 "Upload Music")
	(:form :action "/upload" :method :post
		   (:input :type :text :name "song-title" :placeholder "Song Title")
		   (:br)
		   (:input :type :text :id "location" :name "location" :placeholder "Location")
		   (:a :href "#" :onclick "go_gps()" "GPS")
		   (:br)
		   (:input :type :file :name "file" :placeholder "MP3")
		   (:input :type :submit))
	(:script
	 "
function go_gps() {
  navigator.geolocation.getCurrentPosition(gps, function() {});
}

function gps(position) {
  $('#location').val('' + position.coords.latitude + ', ' + position.coords.longitude);
}
")))

(define-easy-handler (stats :uri "/stats") ()
  (page "Stats - Band Manager"
	(:h1 "Stats")
	(loop for album in *albums*
		 do (htm (:div :class "album"
					   (:h2 (str (album-title album)))
					   (:p (fmt "Downloaded ~a times." (album-download-count album)))
					   (:table :class "songs"
							   (:tr (:th "Song")
									(:th "# of Downloads")
									(:th ""))
							   (loop for song in (album-songs album)
								  do (htm (:tr (fmt "<td class=\"song-title\"><a href=\"/trends?query=~a\">~a</a></td>" (song-title song) (song-title song))
											   (:td :class "download-count" (str (song-download-count song)))
											   (:td :class "play" (:button :disabled "disabled" "Play")))))))))))

(define-easy-handler (venues :uri "/venues") (what where)
  (let ((results (yellowpages:business-details-list (yellowpages:find-business-listings what where))))
	(page "Venues - Band Manager"
	  (:h1 "Find Venues")
	  (:ul :class "links"
		   (fmt "<li><a href=\"/venues?what=bars&where=~a\">Bars</a></li>" where)
		   (fmt "<li><a href=\"/venues?what=concert+venues&where=~a\">Concert Venues</a></li>" where)
		   (fmt "<li><a href=\"/venues?what=stadiums&where=~a\">Stadiums</a></li>" where))
	  (:h1 "Set Location")
	  (:form :action "/venues"
			 :method :get
			 (:p (:label "Location")
				 (:input :type :hidden :name "what" :value what)
				 (:input :type :text :name "where" :id "location" :placeholder "City or Province")
				 ;(:a :href "#" :onclick "go_gps()" "GPS")
				 )
			 (:p (:input :type :submit)))
	  (:h1 "Venue Search Results")
	  (:p "Click to see trends about the venue")
	  (:ul :class "venues"
		   (loop for venue in results
			  for name = (rest (assoc :name venue))
			  do (fmt "<li><a href=\"/trends?query=~a\">~a</a> <small><a href=\"http://maps.google.com/maps?q=~a\">map</a></small></li>" (url-encode name) name (url-encode name))))
	  (:script
	 "
function go_gps() {
  navigator.geolocation.getCurrentPosition(gps, function() {});
}

function gps(position) {
  $('#location').val('cZ' + position.coords.latitude + ',' + position.coords.longitude);
}
")
)))

(define-easy-handler (trends :uri "/trends") (query)
  (let* ((results (trends-for query))
		 (links (rest (second results)))
		 (hashtags (rest (third results)))
		 (phrases (rest (fourth results)))
		 (sources (rest (fifth results))))
	(page "Trends"
	  (:h1 "Trends")
	  (:h2 "Headlines")
	  (:ul :class "headlines"
		   (loop for headline in links
			  for link = (rest (assoc :value headline))
			  for info = (rest (assoc :expanded headline))
			  do (htm (:li (:a :href link (str (rest (assoc :title info))))))))
	  (:h2 "Hashtags")
	  (:ul :class "hashtags"
		   (loop for tag in hashtags
			  for title = (rest (assoc :value tag))
			  for weight = (rest (assoc :weight tag))
			  do (if (> weight 1.2)
					 (htm (:li :class "heavy" (str title)))
					 (htm (:li :class "light" (str title))))))
	  (:h2 "Phrases")
	  (:ul :class "phrases"
		   (loop for phrase in phrases
			  for title = (rest (assoc :value phrase))
			  do (htm (:li (str title)))))
	  (:h2 "Sources")
	  (:ul :class "sources"
		   (loop for source in sources
			  for title = (rest (assoc :value source))
			  do (htm (:li (str title))))))))

(define-easy-handler (expenses :uri "/expenses") ()
  (page "Expenses - Band Manager"
	(:h1 "Expenses")
	(:table :class "expenses"
			(:tr (:th "Item")
				 (:th "Cost")
				 (:th "Date"))
			(loop for expense in *expenses*
				 do (htm (:tr (:td :class "item" (str (expense-item expense)))
							  (:td :class "cost" "$" (str (expense-cost expense)))
							  (:td :class "date" (str (expense-date expense)))))))
	(:h2 "Add Expense")
	(:form :action "/expenses/add" :method :post
		   (:input :type :text :name "item" :placeholder "Item name")
		   (:input :type :text :name "cost" :placeholder "Item cost")
		   (:br)
		   (:input :type :submit))))

(define-easy-handler (add-expense :uri "/expenses/add") (item cost)
  (add-band-expense item cost)
  (redirect "/expenses"))

(define-easy-handler (upload-live-music :uri "/upload") (song-title location file)
  (page "Uploaded"
	(:h1 "Upload complete")
	(:p (fmt "Uploaded ~a from ~a" song-title location))))

(defun add-sample-albums ()
  (loop for i from 1 to 3
	 do (add-album (make-album (format nil "Best Album Ever #~a" (random 100))
							   (loop for i from 1 to (random 12) collect (make-song (format nil "Title #~a" i) "http://google.ca" (random 100)))))))

(defun start-server (&optional (port 4242))
  (setf *band* (make-band "Best Coast"))
  (setf *manager-phone-number* "1231232")
  (setf *expenses* (list))
  (start (make-instance 'easy-acceptor :port port)))